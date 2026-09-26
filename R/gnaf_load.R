#' Load GNAF CSV data into the database
#'
#' Uses DuckDB's native CSV reader for maximum speed. The GNAF CSV must contain
#' at minimum the columns produced by the standard GNAF Core download. All
#' GNAF Core columns are captured, including \code{DATE_CREATED},
#' \code{LEGAL_PARCEL_ID}, \code{MB_CODE}, \code{ALIAS_PRINCIPAL},
#' \code{PRINCIPAL_PID} (alias-to-principal address mapping), and
#' \code{PRIMARY_SECONDARY} / \code{PRIMARY_PID} (main-dwelling vs.
#' sub-dwelling mapping), stored as \code{alias_principal}, \code{principal_pid},
#' \code{primary_secondary} and \code{primary_pid}.
#'
#' The complete CSV batch, locality index and match cache are updated in one
#' transaction. If a file fails to load, the previous database contents remain.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param path Character vector of one or more paths to GNAF CSV files.
#' @param overwrite If \code{TRUE}, deletes existing GNAF rows before loading.
#' @param collapse_same_coordinates If \code{TRUE}, remove GNAF secondary
#'   addresses whose linked primary is present and has exactly the same finite
#'   longitude and latitude, including aliases of those secondaries. Defaults
#'   to \code{FALSE}, preserving unit-level detail. Missing links or coordinates
#'   are retained; custom addresses are untouched. Applies to all GNAF rows
#'   after the CSV batch loads, including previously loaded rows. Reload with
#'   this option off to restore removed records. Matching then searches the
#'   remaining addresses, so unit-level match scores and coverage may change.
#' @return Invisibly, the total number of GNAF rows now in the database.
#' @export
gnaf_load <- function(con, path, overwrite = FALSE,
                      collapse_same_coordinates = FALSE) {
  .validate_collapse_same_coordinates(collapse_same_coordinates)
  if (!is.character(path) || length(path) == 0L)
    stop("'path' must be a non-empty character vector")

  missing <- path[!file.exists(path)]
  if (length(missing) > 0L)
    stop("File(s) not found:\n  ", paste(missing, collapse = "\n  "))

  restore_order <- .disable_insertion_order(con)

  DBI::dbBegin(con)
  committed <- FALSE
  on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
  # Registered after the rollback handler so the SET runs outside the
  # transaction on the error path.
  on.exit(restore_order(), add = TRUE)

  if (overwrite) {
    DBI::dbExecute(con, "DELETE FROM gnaf_addresses WHERE source = 'gnaf'")
    message("Cleared existing GNAF rows.")
  }

  st_case_sql <- .street_type_case_sql("STREET_TYPE")
  for (p in path) {
    # Normalise to forward slashes (DuckDB accepts them on Windows)
    p_fwd <- gsub("\\\\", "/", p)
    message("Loading: ", p)

    DBI::dbExecute(con, sprintf("
      INSERT INTO gnaf_addresses (
        address_detail_pid, address_label, address_site_name, building_name,
        flat_type, flat_number, level_type, level_number,
        number_first, number_last, lot_number,
        street_name, street_type, street_suffix,
        locality_name, state, postcode,
        longitude, latitude, source, alias_type,
        date_created, legal_parcel_id, mb_code,
        alias_principal, principal_pid, primary_secondary, primary_pid,
        geocode_type
      )
      SELECT
        ADDRESS_DETAIL_PID                     AS address_detail_pid,
        ADDRESS_LABEL                          AS address_label,
        ADDRESS_SITE_NAME                      AS address_site_name,
        BUILDING_NAME                          AS building_name,
        FLAT_TYPE                              AS flat_type,
        CAST(FLAT_NUMBER   AS VARCHAR)         AS flat_number,
        LEVEL_TYPE                             AS level_type,
        CAST(LEVEL_NUMBER  AS VARCHAR)         AS level_number,
        TRY_CAST(NUMBER_FIRST AS INTEGER)      AS number_first,
        TRY_CAST(NUMBER_LAST  AS INTEGER)      AS number_last,
        LOT_NUMBER                             AS lot_number,
        STREET_NAME                            AS street_name,
        (%s)                                   AS street_type,
        STREET_SUFFIX                          AS street_suffix,
        LOCALITY_NAME                          AS locality_name,
        STATE                                  AS state,
        TRY_CAST(POSTCODE  AS INTEGER)         AS postcode,
        TRY_CAST(LONGITUDE AS DOUBLE)          AS longitude,
        TRY_CAST(LATITUDE  AS DOUBLE)          AS latitude,
        'gnaf'                                 AS source,
        NULL::VARCHAR                          AS alias_type,
        CAST(TRY_STRPTIME(DATE_CREATED, ['%%d-%%m-%%Y', '%%Y-%%m-%%d']) AS DATE) AS date_created,
        LEGAL_PARCEL_ID                        AS legal_parcel_id,
        CAST(MB_CODE AS VARCHAR)               AS mb_code,
        ALIAS_PRINCIPAL                        AS alias_principal,
        PRINCIPAL_PID                          AS principal_pid,
        PRIMARY_SECONDARY                      AS primary_secondary,
        PRIMARY_PID                            AS primary_pid,
        GEOCODE_TYPE                           AS geocode_type
      FROM read_csv(?, header = true, all_varchar = true, ignore_errors = true)
      ON CONFLICT DO NOTHING
    ", st_case_sql), params = list(p_fwd))

    message("Done: ", p)
  }

  if (collapse_same_coordinates) .collapse_gnaf_secondaries(con)

  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_addresses")$n
  message("Total GNAF addresses in database: ", format(n, big.mark = ","))
  message("Rebuilding locality index ...")
  gnaf_rebuild_locality_index(con)
  message("Rebuilding street-type index ...")
  gnaf_rebuild_street_type_index(con)
  DBI::dbExecute(con, "ANALYZE gnaf_addresses")
  DBI::dbExecute(con, "ANALYZE gnaf_locality_index")
  DBI::dbExecute(con, "ANALYZE gnaf_street_type_index")
  .invalidate_match_cache(con)
  DBI::dbCommit(con)
  committed <- TRUE
  invisible(n)
}

.validate_collapse_same_coordinates <- function(value) {
  if (!is.logical(value) || length(value) != 1L || is.na(value))
    stop("'collapse_same_coordinates' must be TRUE or FALSE", call. = FALSE)
}

# Called inside the loader transaction, before index rebuilds/cache invalidation.
# Use the explicit relationship, never coordinates alone: unrelated dwellings
# can share a geocode. UNION also terminates malformed cycles in alias links.
.collapse_gnaf_secondaries <- function(con, state = NULL) {
  state_filter <- if (is.null(state)) "" else "AND s.state = ?"
  sql <- sprintf("
    WITH RECURSIVE collapsed(address_detail_pid) AS (
      SELECT s.address_detail_pid
      FROM gnaf_addresses s
      JOIN gnaf_addresses p ON p.address_detail_pid = s.primary_pid
      WHERE s.source = 'gnaf' AND p.source = 'gnaf'
        AND s.primary_secondary IN ('S', 'SECONDARY')
        AND p.primary_secondary IN ('P', 'PRIMARY')
        AND s.address_detail_pid <> p.address_detail_pid
        AND isfinite(s.longitude) AND isfinite(s.latitude)
        AND s.longitude = p.longitude AND s.latitude = p.latitude
        %s
      UNION
      SELECT a.address_detail_pid
      FROM gnaf_addresses a
      JOIN collapsed c ON a.principal_pid = c.address_detail_pid
      WHERE a.source = 'gnaf'
        AND a.alias_principal IN ('A', 'ALIAS')
    )
    DELETE FROM gnaf_addresses
    WHERE address_detail_pid IN (SELECT address_detail_pid FROM collapsed)
  ", state_filter)
  n <- if (is.null(state)) DBI::dbExecute(con, sql) else
    DBI::dbExecute(con, sql, params = list(state))
  message("Removed ", format(n, big.mark = ","),
          " same-coordinate GNAF secondary/alias rows.")
  invisible(n)
}
