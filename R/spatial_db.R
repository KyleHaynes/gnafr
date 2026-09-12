#' Store polygon attributes alongside addresses in a GNAF database
#'
#' Looks up all distinct address coordinates in one call and saves the requested
#' polygon attributes in a separate table keyed by `address_detail_pid`.
#' All source addresses are retained, including addresses without valid
#' coordinates or a matching polygon, whose attributes are missing.
#'
#' @param con Writable DuckDB connection from [gnaf_connect()].
#' @param shapes An `sf` polygon object with a known CRS.
#' @param output_table Name of the new table to create in the same database.
#'   Must not already exist. Remove the old enrichment table explicitly before
#'   rebuilding it.
#' @param return_cols Character vector of polygon attributes to store. Defaults
#'   to all non-geometry attributes. At least one column is required.
#' @param address_table Source address table: `"gnaf_addresses"` (default) or
#'   `"custom_addresses"`. To enrich both, call this function separately with
#'   different output table names.
#' @param points_crs CRS of the stored coordinates. Default 4326 (WGS84).
#'   Set this to the datum of your source data; see [spatial_lookup()].
#' @param verbose Print progress if `TRUE`.
#' @param geography Optional registered name for use with `gnaf_match()`.
#'   Default `NULL` creates an unregistered table for backwards compatibility.
#'   Prefer [gnaf_add_geography()] for a complete add/list/coverage/remove workflow.
#' @return Invisibly, the number of address rows in the new table.
#' @details The output is a snapshot. Rebuild it after replacing address data,
#'   changing coordinates or changing boundaries. Join it to the returned
#'   `address_detail_pid` from [gnaf_match()] to respect principal/primary
#'   resolution. Register the table with [gnaf_register_geography()] (or supply
#'   `geography`) to request it in `gnaf_match(..., geographies = name)`.
#'
#'   SQL deduplicates coordinates before they enter R. All distinct coordinates
#'   and their polygon attributes are processed together, without batching.
#'   Allow enough memory for the coordinates and their point geometries.
#'   The operation is atomic:
#'   a failed lookup or write rolls back the new table and temporary staging
#'   tables. Existing address tables and the match cache are not modified.
#'   Overlaps use the first polygon in the order supplied to `shapes`.
#'
#'   Remove the enrichment with `DBI::dbRemoveTable(con, output_table)` when it is
#'   no longer needed. This leaves the GNAF address data intact.
#' @export
gnaf_add_spatial <- function(con, shapes, output_table, return_cols = NULL,
                             address_table = c("gnaf_addresses", "custom_addresses"),
                             points_crs = 4326, verbose = TRUE, geography = NULL) {
  address_table <- match.arg(address_table)
  if (!is.null(geography)) {
    geography <- .geography_name(geography)
    if (geography %in% gnaf_list_geographies(con)$name)
      stop("Geography already registered: ", geography,
           ". Remove it explicitly before rebuilding.", call. = FALSE)
  }
  if (!is.character(output_table) || length(output_table) != 1L ||
      is.na(output_table) || !nzchar(trimws(output_table)))
    stop("'output_table' must be one non-empty table name", call. = FALSE)
  if (DBI::dbExistsTable(con, output_table))
    stop("Output table already exists: ", output_table,
         ". Remove it explicitly before rebuilding.", call. = FALSE)
  if (!DBI::dbExistsTable(con, address_table))
    stop("Address table not found: ", address_table, call. = FALSE)
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'verbose' must be TRUE or FALSE", call. = FALSE)

  # Validate CRS/columns and infer the staging schema before starting any writes.
  empty_points <- data.table::data.table(longitude = numeric(), latitude = numeric())
  prototype <- spatial_lookup(empty_points, shapes, return_cols = return_cols,
                              points_crs = points_crs, verbose = FALSE)
  return_cols <- setdiff(names(prototype), names(empty_points))
  if (length(return_cols) == 0L || "address_detail_pid" %in% return_cols)
    stop("Select at least one polygon attribute other than 'address_detail_pid'",
         call. = FALSE)

  points_crs <- sf::st_crs(points_crs)
  valid_sql <- "isfinite(longitude) AND isfinite(latitude)"
  if (isTRUE(sf::st_is_longlat(points_crs)))
    valid_sql <- paste(valid_sql,
      "AND longitude BETWEEN -180 AND 180 AND latitude BETWEEN -90 AND 90")
  source_sql <- as.character(DBI::dbQuoteIdentifier(con, address_table))
  output_sql <- as.character(DBI::dbQuoteIdentifier(con, output_table))
  attribute_sql <- paste0("v.", DBI::dbQuoteIdentifier(con, return_cols), collapse = ", ")
  values_table <- basename(tempfile("gnafr_spatial_values_"))
  values_sql <- as.character(DBI::dbQuoteIdentifier(con, values_table))

  rows <- DBI::dbWithTransaction(con, {
    points <- data.table::as.data.table(DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT longitude, latitude FROM %s WHERE %s", source_sql, valid_sql)))
    if (verbose) message(format(nrow(points), big.mark = ","), " distinct coordinates to look up.")
    values <- spatial_lookup(points, shapes, return_cols = return_cols,
                              chunk_size = NULL, points_crs = points_crs,
                              verbose = FALSE)
    DBI::dbWriteTable(con, values_table, as.data.frame(values), temporary = TRUE)
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE %s AS
       SELECT g.address_detail_pid, %s
       FROM %s g LEFT JOIN %s v
         ON g.longitude = v.longitude AND g.latitude = v.latitude",
      output_sql, attribute_sql, source_sql, values_sql))
    DBI::dbRemoveTable(con, values_table)
    if (!is.null(geography))
      .register_geography(con, geography, output_table, address_table, points_crs,
                          validate = FALSE)
    as.numeric(DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", output_sql))$n)
  })
  if (verbose) message("Created ", output_table, " with ",
                       format(rows, big.mark = ","), " address rows.")
  invisible(rows)
}
