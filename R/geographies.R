#' Add a named geography to a GNAF database
#'
#' Calculate and store polygon attributes, then register them for use with
#' `gnaf_match(..., geographies = name)`. Processes all distinct coordinates
#' together, without batching. Existing geographies are never overwritten.
#' @param con Writable connection from [gnaf_connect()].
#' @param name Short geography name, e.g. `"sa2_2021"`. Names use letters,
#'   digits and underscores, start with a letter, and are stored lowercase.
#' @param shapes An `sf` polygon object with a known CRS.
#' @param return_cols Polygon columns to store. A named character vector renames
#'   them, e.g. `c(sa2_code = "SA2_CODE21", sa2_name = "SA2_NAME21")`.
#'   Default: all non-geometry columns. Choose distinct output names across layers.
#' @param address_table `"gnaf_addresses"` (default) or `"custom_addresses"`.
#' @param points_crs CRS of the stored coordinates. Default 4326; use 7844 for
#'   GDA2020 or 4283 for GDA94.
#' @param verbose Print progress.
#' @return Invisibly, the number of stored address rows.
#' @details Each geography is a snapshot in its own table, `gnaf_geo_<name>`.
#'   Rebuild after changing address coordinates, loading new GNAF data or changing
#'   boundaries. Creation and registration are atomic. Missing assignments remain
#'   missing; no nearest-polygon substitution is performed.
#' @seealso [gnaf_list_geographies()], [gnaf_geography_coverage()],
#'   [gnaf_join_geographies()], [gnaf_remove_geography()]
#' @md
#' @export
gnaf_add_geography <- function(con, name, shapes, return_cols = NULL,
                                address_table = c("gnaf_addresses", "custom_addresses"),
                                points_crs = 4326, verbose = TRUE) {
  name <- .geography_name(name)
  address_table <- match.arg(address_table)
  if (!is.null(names(return_cols))) {
    if (!is.character(return_cols) || anyNA(return_cols) || anyDuplicated(return_cols) ||
        anyNA(names(return_cols)) || any(!nzchar(names(return_cols))) ||
        anyDuplicated(tolower(names(return_cols))) ||
        !inherits(shapes, "sf") ||
        !all(return_cols %in% names(sf::st_drop_geometry(shapes))))
      stop("'return_cols' must map distinct output names to polygon columns", call. = FALSE)
    shapes <- shapes[, unname(return_cols), drop = FALSE]
    names(shapes)[match(unname(return_cols), names(shapes))] <- names(return_cols)
    return_cols <- names(return_cols)
  }
  gnaf_add_spatial(con, shapes, paste0("gnaf_geo_", name), return_cols,
                   address_table, points_crs, verbose, geography = name)
}

#' Register an existing geography table without repeating the spatial lookup
#'
#' Use this for tables previously created by [gnaf_add_spatial()], such as
#' `gnaf_sa2_2021`. Checks that PIDs are non-missing and unique and that at least
#' one attribute exists. A table can belong to only one geography.
#' @inheritParams gnaf_add_geography
#' @param table Existing enrichment table in the main database schema.
#' @param points_crs Optional source coordinate CRS. Default `NA` records an
#'   unknown CRS for a legacy table; use 7844 when the coordinates were GDA2020.
#' @return Invisibly, the registered geography name. Re-registering the same
#'   name, table and source is allowed.
#' @md
#' @export
gnaf_register_geography <- function(con, name, table,
                                     address_table = c("gnaf_addresses", "custom_addresses"),
                                     points_crs = NA) {
  name <- .geography_name(name)
  address_table <- match.arg(address_table)
  DBI::dbWithTransaction(con, {
    .register_geography(con, name, table, address_table, points_crs, validate = TRUE)
  })
  invisible(name)
}

#' List registered geographies and their available columns
#' @md
#' @param con Connection from [gnaf_connect()]; read-only is supported.
#' @return A `data.table` with `name`, `table_name`, `address_table`,
#'   `points_crs`, `created_at`, `available`, and a list column `columns`.
#'   Listing only reads metadata; use [gnaf_geography_coverage()] for counts.
#'   Missing tables remain listed with `available = FALSE`.
#' @export
gnaf_list_geographies <- function(con) {
  if (!DBI::dbExistsTable(con, "gnaf_geographies")) {
    return(data.table::data.table(
      name = character(), table_name = character(), address_table = character(),
      points_crs = character(), created_at = as.POSIXct(character(), tz = "UTC"),
      available = logical(), columns = list()))
  }
  out <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT name, table_name, address_table, points_crs, created_at
     FROM gnaf_geographies ORDER BY name"))
  available <- vapply(out$table_name, function(x) DBI::dbExistsTable(con, x), logical(1))
  columns <- lapply(seq_len(nrow(out)), function(i) {
    if (!available[i]) return(character())
    setdiff(DBI::dbListFields(con, out$table_name[i]), "address_detail_pid")
  })
  data.table::set(out, j = "available", value = available)
  data.table::set(out, j = "columns", value = columns)
  out[]
}

#' Join saved geography attributes onto existing match results
#' @md
#' @param results A data.frame or data.table containing `address_detail_pid`.
#'   If present, `matched` excludes unmatched rows and `source` restricts joins
#'   to the registered address source. Without these fields, PIDs alone are used.
#' @param con Connection from [gnaf_connect()]; read-only is supported.
#' @param geographies Registered names, or `TRUE` for all available geographies.
#'   `NULL` or `FALSE` adds none.
#' @return A new `data.table` with attributes appended, preserving input order,
#'   repeated matches, unmatched rows and original columns. The input is not
#'   modified. Duplicate lookup PIDs and column-name collisions are errors.
#' @details Only distinct matched PIDs are sent to DuckDB. Geography joins run
#'   after principal/primary resolution and do not change match scores or cache
#'   entries. Each join uses the final returned address PID and source.
#' @export
gnaf_join_geographies <- function(results, con, geographies = TRUE) {
  if (!is.data.frame(results) || !"address_detail_pid" %in% names(results) ||
      !is.character(results$address_detail_pid))
    stop("'results' must contain a character 'address_detail_pid' column", call. = FALSE)
  specs <- .geography_specs(con, geographies)
  .join_geography_specs(results, con, specs)
}

#' Check geography coverage against current addresses
#' @md
#' @inheritParams gnaf_join_geographies
#' @return A `data.table`, one row per geography attribute, with `geography`,
#'   `column`, `address_table`, `address_rows`, `stored_rows`, `non_missing`,
#'   `missing`, `coverage_pct` and `orphaned_rows`. Coverage uses current source
#'   addresses as the denominator, including missing coordinates. `stored_rows`
#'   counts current PIDs present in the enrichment; orphaned rows no longer have
#'   a source address. Zero source addresses give `NA_real_` coverage.
#' @details Counts are calculated on demand, not cached. They can reveal newly
#'   added or removed PIDs, but cannot detect coordinate edits to an existing PID.
#'   Rebuild the geography after such edits.
#' @export
gnaf_geography_coverage <- function(con, geographies = TRUE) {
  specs <- .geography_specs(con, geographies)
  empty <- data.table::data.table(geography = character(), column = character(),
    address_table = character(), address_rows = numeric(), stored_rows = numeric(),
    non_missing = numeric(), missing = numeric(), coverage_pct = numeric(),
    orphaned_rows = numeric())
  if (!nrow(specs)) return(empty)
  data.table::rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    .validate_geography_table(con, specs$table_name[i])
    attrs <- specs$columns[[i]]
    table <- DBI::dbQuoteIdentifier(con, specs$table_name[i])
    source <- DBI::dbQuoteIdentifier(con, specs$address_table[i])
    counts <- paste0("count(s.", DBI::dbQuoteIdentifier(con, attrs), ")", collapse = ", ")
    totals <- DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) AS address_rows, count(s.address_detail_pid) AS stored_rows, %s
       FROM %s g LEFT JOIN %s s USING (address_detail_pid)", counts, source, table))
    total <- as.numeric(totals[[1L]])
    present <- as.numeric(totals[1L, -(1:2), drop = TRUE])
    orphaned <- as.numeric(DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) AS n FROM %s s ANTI JOIN %s g USING (address_detail_pid)",
      table, source))$n)
    data.table::data.table(geography = specs$name[i], column = attrs,
      address_table = specs$address_table[i], address_rows = total,
      stored_rows = as.numeric(totals[[2L]]), non_missing = present,
      missing = total - present,
      coverage_pct = if (total == 0) NA_real_ else 100 * present / total,
      orphaned_rows = orphaned)
  }))
}

#' Remove a saved geography and its registration
#' @md
#' @param con Writable connection from [gnaf_connect()].
#' @param name Registered geography name.
#' @return Invisibly, the removed geography name.
#' @details Drops only the registered enrichment table and its metadata, in one
#'   transaction. Source address tables and match cache entries are retained.
#'   If the table was already removed manually, its registration is removed.
#' @export
gnaf_remove_geography <- function(con, name) {
  name <- .geography_name(name)
  specs <- gnaf_list_geographies(con)
  row <- match(name, specs$name)
  if (is.na(row)) stop("Unknown geography: ", name, call. = FALSE)
  table <- specs$table_name[row]
  .geography_table_name(table)
  DBI::dbWithTransaction(con, {
    if (DBI::dbExistsTable(con, table)) DBI::dbRemoveTable(con, table)
    DBI::dbExecute(con, "DELETE FROM gnaf_geographies WHERE name = ?", params = list(name))
  })
  invisible(name)
}

.geography_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", name))
    stop("Geography names must start with a letter and contain only letters, digits and underscores",
         call. = FALSE)
  tolower(name)
}

.geography_table_name <- function(table) {
  protected <- c("gnaf_addresses", "custom_addresses", "gnaf_match_cache",
                 "gnaf_locality_index", "gnaf_geographies")
  if (!is.character(table) || length(table) != 1L || is.na(table) || !nzchar(table) ||
      tolower(table) %in% protected)
    stop("Choose an enrichment table, not a GNAF source or system table", call. = FALSE)
  table
}

.validate_geography_table <- function(con, table) {
  .geography_table_name(table)
  fields <- DBI::dbListFields(con, table)
  if (!"address_detail_pid" %in% fields || length(fields) < 2L)
    stop("Geography table must contain 'address_detail_pid' and at least one attribute",
         call. = FALSE)
  prototype <- DBI::dbGetQuery(con, sprintf("SELECT address_detail_pid FROM %s LIMIT 0",
                                           DBI::dbQuoteIdentifier(con, table)))
  if (!is.character(prototype$address_detail_pid))
    stop("Geography address_detail_pid must be a character column", call. = FALSE)
  invalid <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) != count(DISTINCT address_detail_pid) AS invalid FROM %s",
    DBI::dbQuoteIdentifier(con, table)))$invalid
  if (isTRUE(invalid)) stop("Geography table has missing or duplicate address_detail_pid values: ",
                             table, call. = FALSE)
  invisible(fields)
}

.register_geography <- function(con, name, table, address_table, points_crs, validate) {
  .geography_table_name(table)
  stored <- DBI::dbGetQuery(con,
    "SELECT table_name, table_type FROM information_schema.tables
     WHERE table_catalog = current_database() AND table_schema = 'main'
       AND lower(table_name) = lower(?)", params = list(table))
  if (nrow(stored) != 1L || stored$table_type != "BASE TABLE")
    stop("Geography must be a persistent enrichment table in the main database schema",
         call. = FALSE)
  table <- stored$table_name
  if (validate) .validate_geography_table(con, table)
  if (!DBI::dbExistsTable(con, address_table)) stop("Address source table is missing", call. = FALSE)
  crs <- sf::st_crs(points_crs)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS gnaf_geographies (
    name VARCHAR PRIMARY KEY, table_name VARCHAR UNIQUE NOT NULL,
    address_table VARCHAR NOT NULL, points_crs VARCHAR,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  existing <- DBI::dbGetQuery(con,
    "SELECT name, table_name, address_table FROM gnaf_geographies WHERE name = ? OR table_name = ?",
    params = list(name, table))
  if (nrow(existing)) {
    if (nrow(existing) == 1L && existing$name == name && existing$table_name == table &&
        existing$address_table == address_table) return(invisible(name))
    stop("Geography name or table already registered; remove the old geography first", call. = FALSE)
  }
  DBI::dbExecute(con,
    "INSERT INTO gnaf_geographies (name, table_name, address_table, points_crs) VALUES (?, ?, ?, ?)",
    params = list(name, table, address_table, if (is.na(crs)) NA_character_
      else if (!is.na(crs$epsg)) paste0("EPSG:", crs$epsg) else crs$input))
  invisible(name)
}

.geography_specs <- function(con, geographies) {
  if (is.null(geographies) || identical(geographies, FALSE) ||
      identical(geographies, character())) return(data.table::data.table())
  if (!identical(geographies, TRUE) &&
      (!is.character(geographies) || anyNA(geographies)))
    stop("'geographies' must be geography names, TRUE, FALSE or NULL", call. = FALSE)
  catalog <- gnaf_list_geographies(con)
  if (identical(geographies, TRUE)) return(catalog[which(catalog$available)])
  requested <- unique(vapply(geographies, .geography_name, character(1)))
  rows <- match(requested, catalog$name)
  if (anyNA(rows)) stop("Unknown geography: ", paste(requested[is.na(rows)], collapse = ", "),
    ". Use gnaf_list_geographies() or register an existing table with gnaf_register_geography().",
    call. = FALSE)
  out <- catalog[rows]
  if (any(!out$available)) stop("Geography table is missing: ",
    paste(out$table_name[!out$available], collapse = ", "), call. = FALSE)
  out
}

.join_geography_specs <- function(results, con, specs) {
  out <- data.table::as.data.table(data.table::copy(results))
  if (!nrow(specs)) return(out)
  attrs <- unlist(specs$columns, use.names = FALSE)
  if (anyDuplicated(attrs) || any(attrs %in% names(out)))
    stop("Geography columns overlap each other or existing result columns. ",
         "Choose distinct output names with return_cols when adding geographies.", call. = FALSE)
  for (i in seq_len(nrow(specs))) {
    eligible <- !is.na(out$address_detail_pid)
    if ("matched" %in% names(out)) eligible <- eligible & out$matched %in% TRUE
    if ("source" %in% names(out)) eligible <- eligible &
      out$source %in% if (specs$address_table[i] == "custom_addresses") "custom" else "gnaf"
    rows <- which(eligible)
    ids <- data.table::data.table(address_detail_pid = unique(out$address_detail_pid[rows]))
    lookup <- .fetch_geography(con, specs$table_name[i], ids)
    if (anyDuplicated(lookup$address_detail_pid))
      stop("Geography table has duplicate address_detail_pid values: ", specs$table_name[i], call. = FALSE)
    target <- rep(NA_integer_, nrow(out))
    target[rows] <- match(out$address_detail_pid[rows], lookup$address_detail_pid)
    for (column in specs$columns[[i]])
      data.table::set(out, j = column, value = lookup[[column]][target])
  }
  out[]
}

.fetch_geography <- function(con, table, ids) {
  temporary <- basename(tempfile("gnafr_geography_ids_"))
  duckdb::duckdb_register(con, temporary, ids)
  on.exit(duckdb::duckdb_unregister(con, temporary), add = TRUE)
  data.table::as.data.table(DBI::dbGetQuery(con, sprintf(
    "SELECT s.* FROM %s s JOIN %s i USING (address_detail_pid)",
    DBI::dbQuoteIdentifier(con, table), DBI::dbQuoteIdentifier(con, temporary))))
}
