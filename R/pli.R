# Queensland Property Location Index (PLI)
#
# The PLI is the State of Queensland's own address dataset (Lands Address
# Location Framework). It overlaps heavily with G-NAF but carries addresses
# that G-NAF has not yet picked up. gnaf_load_pli() adds the ones G-NAF does
# not already have to gnaf_addresses, wrangled as the G-NAF loaders wrangle
# their own data so that they match, rank and index like any other address.

# Columns of the pipe-delimited text file, in file order.
.PLI_COLUMNS <- c(
  "ADDRESS_PID", "PLAN", "LOT", "LOTPLAN_STATUS", "ADDRESS_STATUS",
  "ADDRESS_STANDARD", "UNIT_TYPE", "UNIT_NUMBER", "UNIT_SUFFIX",
  "PROPERTY_NAME", "STREET_NO_1", "STREET_NO_1_SUFFIX", "STREET_NO_2",
  "STREET_NO_2_SUFFIX", "STREET_NAME", "STREET_TYPE", "STREET_SUFFIX",
  "LOCALITY", "LOCAL_AUTHORITY", "LGA_CODE", "LATITUDE", "LONGITUDE",
  "GEOCODE_TYPE", "DATUM"
)

# Coordinate reference systems the PLI can be supplied in, and G-NAF's own.
.PLI_DATUMS <- c(GDA94 = "EPSG:4283", GDA2020 = "EPSG:7844")

# Lots that stand for "no lot": a lot-only address is only usable with a real one.
.PLI_NULL_LOTS <- c("0", "9999")

# The PLI spells street directions out; G-NAF stores (and prints) their codes.
.PLI_SUFFIX_CODES <- c(EAST = "E", WEST = "W", NORTH = "N", SOUTH = "S",
                       CENTRAL = "CN")

#' Add Queensland Property Location Index addresses to the database
#'
#' Adds the addresses in the Queensland Property Location Index (PLI) that
#' G-NAF does not already contain. Run it after the database is built (see
#' \code{\link{gnaf_build_db}}); the walkthrough is in the repository's
#' \file{Adding PLI.md}.
#'
#' The PLI ("Property address Queensland - Text data package") can be
#' downloaded from the Queensland Spatial Catalogue
#' (\url{https://qldspatial.information.qld.gov.au/catalogue/custom/detail.page?fid=\{F878C43D-3087-4102-8F28-1CFEA49B34F1\}}).
#' Pass either the downloaded \file{.zip} or the \file{.txt} inside it.
#'
#' Each PLI address is wrangled the way the G-NAF loaders wrangle theirs:
#' \itemize{
#'   \item The label is built by the same code as G-NAF's, so it reads the
#'     same: flat, number, street, then \code{LOCALITY QLD POSTCODE}. Flat,
#'     street-number and range suffixes are kept in the label as G-NAF keeps
#'     them, street directions become G-NAF's codes (\code{EAST} as \code{E}),
#'     street types are canonicalised, and the PLI's \code{UNIT_TYPE} \code{U}
#'     is G-NAF's \code{UNIT}. The PLI's property name is a site name, not a
#'     building name: as with G-NAF's own \code{address_site_name} it is stored
#'     in that column and left out of the label.
#'   \item A lot is used only when there is no street number:
#'     \code{LOT 24 SMITH ROAD, ...}, with \code{lot_number} set. The lot and
#'     plan are always kept in \code{legal_parcel_id} (\code{lot/plan}).
#'   \item The PLI has no state or postcode. The state is \code{QLD}; the
#'     postcode is taken from G-NAF: the locality's only postcode, else the
#'     postcode G-NAF gives that street in the locality, else the postcode of
#'     the nearest G-NAF address in the locality. The PLI's own \code{(LGA)}
#'     suffix on ambiguous locality names is dropped.
#'   \item Coordinates are converted to GDA2020, the datum G-NAF uses, when
#'     the file declares GDA94 (it does), using PROJ's standard GDA94 to GDA2020
#'     transformation.
#'   \item An address that appears on several parcels or with several geocodes
#'     is one address; its property-centroid geocode is kept.
#' }
#'
#' Only addresses whose label G-NAF does not already have (in any of its
#' principal or alias rows, compared with case, commas, full stops and spacing
#' ignored) are added, as principal rows with \code{source = "pli"} and PIDs
#' \code{PLI<address_pid>} (G-NAF PIDs begin \code{GA}, so they cannot collide;
#' this is checked, as is any clash with a custom address). Two sources often
#' describe one address with a different building name (or none), or call a
#' unit a \code{SHOP} or \code{SUITE} where the other says \code{UNIT}; such an
#' address is not added, and the summary counts it separately from an exact
#' label match. Addresses already loaded from an earlier PLI file count as
#' already present, and a label that occurs twice in the PLI is added once.
#'
#' A summary of what was in G-NAF already and what was added is printed and
#' returned.
#'
#' After loading, the street-type and locality indexes, the exact-label index
#' (when the database has one) and the match cache are refreshed. Reloading
#' G-NAF does not remove PLI rows; run this again with \code{overwrite = TRUE}
#' afterwards, so PLI addresses G-NAF now has are not added twice.
#'
#' @param con DBI connection from \code{gnaf_connect}, opened for writing, to a
#'   database that already holds G-NAF.
#' @param path Path to \file{DP_PROP_LOCATION_INDEX_QLD.zip} or the
#'   \file{.txt} file inside it.
#' @param overwrite If \code{TRUE}, removes PLI rows loaded earlier
#'   (\code{source = "pli"}) before adding. Default \code{FALSE}.
#' @param address_status PLI address statuses to consider: \code{"P"} (primary)
#'   and \code{"A"} (alternate) by default.
#' @param include_deleted_lotplans If \code{TRUE}, also consider addresses whose
#'   lot on plan is flagged \code{D}. Default \code{FALSE}.
#' @param verbose If \code{TRUE} (default), print progress and the summary.
#' @return Invisibly, a \code{data.table} with one row per stage
#'   (\code{stage}, \code{n}, \code{pct}) describing what happened to the PLI
#'   records, with \code{pct} taken over \code{n} of the unique addresses
#'   considered. Attributes \code{datum} and \code{coordinate_check} describe
#'   the coordinate conversion and its check against G-NAF, and
#'   \code{postcode_methods} counts the addresses given a postcode from the
#'   locality, the street or the nearest address.
#' @export
gnaf_load_pli <- function(con, path, overwrite = FALSE,
                          address_status = c("P", "A"),
                          include_deleted_lotplans = FALSE, verbose = TRUE) {
  for (arg in c("overwrite", "include_deleted_lotplans", "verbose")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop("'", arg, "' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.character(address_status) || length(address_status) == 0L ||
      anyNA(address_status))
    stop("'address_status' must be a character vector of PLI address status codes",
         call. = FALSE)
  address_status <- toupper(address_status)
  if (!DBI::dbExistsTable(con, "gnaf_addresses") ||
      !.table_has_rows(con, "gnaf_addresses"))
    stop("gnaf_addresses is empty: load G-NAF first (gnaf_build_db() or ",
         "gnaf_load_psv()), then add the PLI.", call. = FALSE)
  if (!"source" %in% DBI::dbListFields(con, "gnaf_addresses"))
    stop("gnaf_addresses has no 'source' column; run gnaf_init(con) to migrate it.",
         call. = FALSE)
  if (DBI::dbGetQuery(con, "
        SELECT count(*) AS n FROM gnaf_addresses
        WHERE state = 'QLD' AND coalesce(source, '') <> 'pli' AND postcode IS NOT NULL
      ")$n == 0L)
    stop("gnaf_addresses holds no Queensland G-NAF addresses to take postcodes ",
         "from: load QLD first (gnaf_build_db(states = \"QLD\")).", call. = FALSE)

  say <- function(...) if (isTRUE(verbose)) message(...)
  timer <- proc.time()[["elapsed"]]
  file <- .pli_resolve_path(path)
  on.exit(if (!is.null(file$cleanup)) unlink(file$cleanup, recursive = TRUE), add = TRUE)
  .pli_check_header(file$path)

  tables <- c("__gnafr_pli_raw__", "__gnafr_pli_clean__", "__gnafr_pli_gk__",
              "__gnafr_pli_pc__", "__gnafr_pli_todo__", "__gnafr_pli_near__",
              "__gnafr_pli_prev__", "__gnafr_pli_lab__",
              "__gnafr_pli_new__", "__gnafr_pli_xy__")
  on.exit(for (t in tables)
    try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", t)), silent = TRUE),
    add = TRUE)

  say("Reading ", basename(file$path), " ...")
  .pli_read_raw(con, file$path)
  n_read <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM __gnafr_pli_raw__")$n
  say("  ", format(n_read, big.mark = ","), " PLI records.")
  datums <- DBI::dbGetQuery(con, "
    SELECT upper(trim(datum)) AS datum, count(*) AS n
    FROM __gnafr_pli_raw__ GROUP BY 1")
  unknown <- setdiff(datums$datum, names(.PLI_DATUMS))
  if (length(unknown))
    stop("Unsupported DATUM in the PLI file: ", paste(unknown, collapse = ", "),
         ". Supported: ", paste(names(.PLI_DATUMS), collapse = ", "), ".",
         call. = FALSE)

  say("Cleaning and de-duplicating addresses ...")
  counts <- .pli_stage_clean(con, address_status, include_deleted_lotplans)
  say("Resolving postcodes from G-NAF ...")
  .pli_stage_postcodes(con)
  say("Building G-NAF style labels and comparing them with G-NAF ...")
  stages <- .pli_stage_classify(con, overwrite)

  n_new <- stages$added
  say("Converting coordinates to GDA2020 and checking them against G-NAF ...")
  xy <- .pli_convert_coordinates(con)
  check <- .pli_coordinate_check(con, xy$overlap)

  .pli_check_collisions(con)

  DBI::dbBegin(con)
  committed <- FALSE
  on.exit(if (!committed) try(DBI::dbRollback(con), silent = TRUE), add = TRUE,
          after = FALSE)
  if (isTRUE(overwrite))
    DBI::dbExecute(con, "DELETE FROM gnaf_addresses WHERE source = 'pli'")
  if (n_new > 0L) .pli_insert(con)
  .pli_refresh_indexes(con)
  DBI::dbCommit(con)
  committed <- TRUE

  summary <- .pli_summary(n_read, counts, stages)
  attr(summary, "datum") <- datums
  attr(summary, "postcode_methods") <- stages$postcode_methods
  attr(summary, "coordinate_check") <- check
  if (isTRUE(verbose)) .pli_report(summary, check, proc.time()[["elapsed"]] - timer)
  invisible(summary)
}

# A .zip is unpacked to a temporary directory; the .txt inside is returned.
.pli_resolve_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path))
    stop("'path' must be an existing PLI .zip or .txt file", call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!grepl("\\.zip$", path, ignore.case = TRUE))
    return(list(path = path, cleanup = NULL))
  members <- utils::unzip(path, list = TRUE)$Name
  txt <- members[grepl("\\.txt$", members, ignore.case = TRUE)]
  if (length(txt) != 1L)
    stop("Expected exactly one .txt file in the PLI zip; found ", length(txt),
         call. = FALSE)
  dir <- tempfile("gnafr_pli_")
  dir.create(dir)
  utils::unzip(path, files = txt, exdir = dir)
  list(path = normalizePath(file.path(dir, txt), winslash = "/", mustWork = TRUE),
       cleanup = dir)
}

# The file must be the 24-column PLI extract, in the documented order.
.pli_check_header <- function(file) {
  bytes <- readBin(file, "raw", 8192L)
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
    bytes <- bytes[-(1:3)]
  end <- which(bytes %in% as.raw(c(0x0a, 0x0d)))[1L]
  if (is.na(end))
    stop("This does not look like the PLI text file: no header line.", call. = FALSE)
  first <- rawToChar(bytes[seq_len(end - 1L)])
  cols <- toupper(trimws(strsplit(first, "|", fixed = TRUE)[[1L]]))
  if (length(cols) != length(.PLI_COLUMNS))
    stop("This does not look like the PLI text file: expected ",
         length(.PLI_COLUMNS), " pipe-delimited columns, found ", length(cols),
         ".", call. = FALSE)
  if (!identical(cols, .PLI_COLUMNS))
    stop("Unexpected PLI columns. Expected, in order: ",
         paste(.PLI_COLUMNS, collapse = ", "), ".", call. = FALSE)
  invisible(TRUE)
}

.pli_read_raw <- function(con, file) {
  names <- paste0("'", tolower(.PLI_COLUMNS), "'", collapse = ", ")
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE TEMP TABLE __gnafr_pli_raw__ AS
     SELECT * FROM read_csv('%s', delim = '|', header = true, all_varchar = true,
                            quote = '', escape = '', names = [%s])",
    gsub("'", "''", file, fixed = TRUE), names))
  invisible(NULL)
}

# One clean, G-NAF-shaped row per PLI address (PID).
.pli_stage_clean <- function(con, address_status, include_deleted_lotplans) {
  statuses <- paste0("'", gsub("'", "''", address_status), "'", collapse = ", ")
  suffix_case <- paste(sprintf("WHEN '%s' THEN '%s'", names(.PLI_SUFFIX_CODES),
                               .PLI_SUFFIX_CODES), collapse = " ")
  null_lots <- paste0("'", .PLI_NULL_LOTS, "'", collapse = ", ")
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE __gnafr_pli_clean__ AS
    WITH r AS (
      SELECT
        trim(address_pid) AS address_pid,
        NULLIF(trim(plan), '') AS plan,
        NULLIF(upper(trim(lot)), '') AS lot,
        coalesce(upper(NULLIF(trim(lotplan_status), '')), '') AS lotplan_status,
        upper(trim(address_status)) AS address_status,
        upper(NULLIF(trim(unit_type), '')) AS unit_type,
        upper(NULLIF(trim(unit_number), '')) AS unit_number,
        upper(NULLIF(trim(unit_suffix), '')) AS unit_suffix,
        NULLIF(regexp_replace(upper(trim(property_name)), '\\s+', ' ', 'g'), '')
          AS property_name,
        TRY_CAST(NULLIF(trim(street_no_1), '') AS INTEGER) AS number_first,
        upper(NULLIF(trim(street_no_1_suffix), '')) AS number_first_suffix,
        TRY_CAST(NULLIF(trim(street_no_2), '') AS INTEGER) AS number_last_raw,
        upper(NULLIF(trim(street_no_2_suffix), '')) AS number_last_suffix_raw,
        regexp_replace(upper(trim(street_name)), '\\s+', ' ', 'g') AS street_name,
        upper(NULLIF(NULLIF(trim(street_type), ''), 'XXX')) AS street_type,
        CASE upper(trim(street_suffix)) %s
             ELSE NULLIF(upper(trim(street_suffix)), '') END AS street_suffix,
        regexp_replace(regexp_replace(upper(trim(locality)),
                       '\\s*\\([^)]*\\)\\s*$', ''), '\\s+', ' ', 'g') AS locality_name,
        TRY_CAST(latitude AS DOUBLE) AS lat,
        TRY_CAST(longitude AS DOUBLE) AS lon,
        upper(NULLIF(trim(geocode_type), '')) AS geocode_type,
        upper(trim(datum)) AS datum
      FROM __gnafr_pli_raw__
    ),
    kept AS (
      SELECT * FROM r
      WHERE address_status IN (%s) AND street_name <> ''
        AND (%s OR lotplan_status <> 'D')
    ),
    one AS (
      SELECT *, row_number() OVER (
        PARTITION BY address_pid
        ORDER BY (geocode_type = 'PC') DESC NULLS LAST, (lotplan_status = 'C') DESC,
                 plan, lot, lat, lon) AS rn
      FROM kept
    )
    SELECT * EXCLUDE (rn),
           CASE WHEN number_first IS NOT NULL THEN number_last_raw END AS number_last,
           CASE WHEN number_first IS NOT NULL AND number_last_raw IS NOT NULL
                THEN number_last_suffix_raw END AS number_last_suffix,
           regexp_extract(unit_number, '^([A-Z]*)([0-9]+)([A-Z]*)$', 1) AS flat_prefix,
           regexp_extract(unit_number, '^([A-Z]*)([0-9]+)([A-Z]*)$', 2) AS flat_digits,
           regexp_extract(unit_number, '^([A-Z]*)([0-9]+)([A-Z]*)$', 3) AS flat_suffix_a,
           CASE WHEN number_first IS NULL AND lot IS NOT NULL
                     AND lot NOT IN (%s) THEN lot END AS lot_only
    FROM one WHERE rn = 1",
    suffix_case, statuses, if (isTRUE(include_deleted_lotplans)) "TRUE" else "FALSE",
    null_lots))
  one <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM __gnafr_pli_clean__")$n
  filtered <- DBI::dbGetQuery(con, sprintf("
    SELECT count(*) AS n FROM __gnafr_pli_raw__
    WHERE upper(trim(address_status)) IN (%s)
      AND (%s OR coalesce(upper(trim(lotplan_status)), '') <> 'D')",
    statuses, if (isTRUE(include_deleted_lotplans)) "TRUE" else "FALSE"))$n
  list(considered_rows = filtered, addresses = one)
}

# A postcode for every address: the locality's only G-NAF postcode, else the
# street's, else the nearest G-NAF address's.
.pli_stage_postcodes <- function(con) {
  # G-NAF's principal rows are authoritative for a locality's postcodes; alias
  # (synonym) rows only speak for names that no principal row uses.
  gnaf <- function(alias = "") {
    p <- if (nzchar(alias)) paste0(alias, ".") else ""
    sprintf(paste0("%1$sstate = 'QLD' AND %1$spostcode IS NOT NULL AND ",
                   "coalesce(%1$ssource, '') <> 'pli' AND (%1$salias_type IS NULL OR ",
                   "%1$slocality_name NOT IN (SELECT locality_name FROM gnaf_addresses ",
                   "WHERE alias_type IS NULL AND state = 'QLD' AND ",
                   "coalesce(source, '') <> 'pli'))"), p)
  }
  # Stage 1: a locality with one G-NAF postcode needs no more; a street that
  # lies in one postcode settles the rest of the several-postcode localities.
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE __gnafr_pli_pc__ AS
    WITH loc AS (
      SELECT locality_name, count(DISTINCT postcode) AS npc, min(postcode) AS only_pc
      FROM gnaf_addresses WHERE %1$s AND locality_name IS NOT NULL GROUP BY 1
    ),
    st AS (
      SELECT locality_name,
             trim(street_name || ' ' || coalesce(street_type, '')) AS full_street,
             min(postcode) AS only_pc
      FROM gnaf_addresses
      WHERE %1$s AND locality_name IN (SELECT locality_name FROM loc WHERE npc > 1)
      GROUP BY 1, 2 HAVING count(DISTINCT postcode) = 1
    ),
    c AS (
      SELECT p.address_pid, p.locality_name, p.lat, p.lon,
             trim(p.street_name || ' ' || coalesce(%2$s, '')) AS full_street
      FROM __gnafr_pli_clean__ p
    )
    SELECT c.address_pid, c.locality_name, c.lat, c.lon,
           CAST(coalesce(loc.only_pc, st.only_pc) AS INTEGER) AS postcode,
           CASE WHEN loc.only_pc IS NOT NULL THEN 'locality'
                WHEN st.only_pc IS NOT NULL THEN 'street' END AS method
    FROM c
    LEFT JOIN loc ON loc.locality_name = c.locality_name AND loc.npc = 1
    LEFT JOIN st ON st.locality_name = c.locality_name AND st.full_street = c.full_street",
    gnaf(), .street_type_case_sql("p.street_type")))

  # Stage 2: whatever is left in a several-postcode locality takes the postcode
  # of the nearest G-NAF address in it. (Materialised first, so the distance
  # join only ever sees the unresolved addresses.)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE __gnafr_pli_todo__ AS
    SELECT address_pid, locality_name, lat, lon FROM __gnafr_pli_pc__
    WHERE postcode IS NULL AND lat IS NOT NULL AND lon IS NOT NULL
      AND locality_name IN (SELECT DISTINCT locality_name FROM gnaf_addresses
                            WHERE state = 'QLD' AND alias_type IS NULL
                              AND coalesce(source, '') <> 'pli')")
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE __gnafr_pli_near__ AS
    SELECT t.address_pid,
           arg_min(g.postcode, (g.latitude - t.lat) * (g.latitude - t.lat) +
             ((g.longitude - t.lon) * cos(radians(t.lat))) *
             ((g.longitude - t.lon) * cos(radians(t.lat)))) AS postcode
    FROM __gnafr_pli_todo__ t
    JOIN gnaf_addresses g ON g.locality_name = t.locality_name
      AND g.alias_type IS NULL AND g.latitude IS NOT NULL AND g.postcode IS NOT NULL
      AND g.state = 'QLD' AND coalesce(g.source, '') <> 'pli'
    GROUP BY 1"))
  DBI::dbExecute(con, "
    UPDATE __gnafr_pli_pc__ SET postcode = n.postcode, method = 'nearest'
    FROM __gnafr_pli_near__ n
    WHERE __gnafr_pli_pc__.address_pid = n.address_pid
      AND __gnafr_pli_pc__.postcode IS NULL")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS __gnafr_pli_todo__")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS __gnafr_pli_near__")
  invisible(NULL)
}

# G-NAF's label (same builder as the PSV loader) for every usable address, then
# the funnel: unusable, already in G-NAF, already loaded, duplicate, new.
.pli_stage_classify <- function(con, overwrite) {
  label <- .psv_label_sql(
    "QLD", street_name = "d.STREET_NAME", street_type = "d.STREET_TYPE_CODE",
    street_suffix = "d.STREET_SUFFIX_CODE", locality_name = "d.LOCALITY_NAME",
    postcode = "CAST(d.POSTCODE AS VARCHAR)")
  key <- .exact_key_sql
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE __gnafr_pli_lab__ AS
    WITH d AS (
      SELECT
        c.address_pid,
        CAST(NULL AS VARCHAR) AS BUILDING_NAME, c.property_name,
        CASE WHEN c.flat_digits <> '' THEN
          CASE c.unit_type WHEN 'U' THEN 'UNIT' ELSE c.unit_type END END AS FLAT_TYPE_CODE,
        CASE WHEN c.flat_digits <> '' THEN c.flat_digits END AS FLAT_NUMBER,
        NULLIF(c.flat_prefix, '') AS FLAT_NUMBER_PREFIX,
        CASE WHEN c.flat_digits <> '' THEN
          NULLIF(coalesce(c.flat_suffix_a, '') || coalesce(c.unit_suffix, ''), '') END
          AS FLAT_NUMBER_SUFFIX,
        CAST(NULL AS VARCHAR) AS LEVEL_TYPE_CODE, CAST(NULL AS VARCHAR) AS LEVEL_NUMBER,
        CAST(NULL AS VARCHAR) AS LEVEL_NUMBER_PREFIX,
        CAST(NULL AS VARCHAR) AS LEVEL_NUMBER_SUFFIX,
        c.number_first AS NUMBER_FIRST, CAST(NULL AS VARCHAR) AS NUMBER_FIRST_PREFIX,
        c.number_first_suffix AS NUMBER_FIRST_SUFFIX,
        c.number_last AS NUMBER_LAST, CAST(NULL AS VARCHAR) AS NUMBER_LAST_PREFIX,
        c.number_last_suffix AS NUMBER_LAST_SUFFIX,
        c.lot_only AS LOT_NUMBER, CAST(NULL AS VARCHAR) AS LOT_NUMBER_PREFIX,
        CAST(NULL AS VARCHAR) AS LOT_NUMBER_SUFFIX,
        c.street_name AS STREET_NAME, c.street_type AS STREET_TYPE_CODE,
        c.street_suffix AS STREET_SUFFIX_CODE, c.locality_name AS LOCALITY_NAME,
        pc.postcode AS POSTCODE, pc.method AS postcode_method,
        c.plan, c.lot, c.unit_number, c.lot_only,
        c.lat, c.lon, c.geocode_type, c.datum
      FROM __gnafr_pli_clean__ c
      LEFT JOIN __gnafr_pli_pc__ pc USING (address_pid)
    ),
    lab AS (
      SELECT d.*,
        CASE
          WHEN d.unit_number IS NOT NULL AND d.FLAT_NUMBER IS NULL THEN 'unparseable_unit'
          WHEN d.NUMBER_FIRST IS NULL AND d.lot_only IS NULL THEN 'no_number_or_lot'
          WHEN d.POSTCODE IS NULL THEN 'no_postcode'
        END AS reason,
        %s AS address_label
      FROM d
    )
    SELECT lab.*,
      CASE WHEN reason IS NULL THEN %s END AS lookup_key,
      CASE WHEN reason IS NULL THEN %s END AS core_key
    FROM lab",
    label, key("address_label"),
    key(.pli_core_sql("address_label", "FLAT_TYPE_CODE", "FLAT_NUMBER"))))

  # G-NAF's labels, exactly and with building name and flat type set aside (the
  # same address is often written with a different building name, or as UNIT
  # where G-NAF says SHOP, SHED or SUITE).
  gnaf_keys <- function(where) sprintf("
    WITH g AS (
      SELECT address_detail_pid, address_label AS lbl, building_name, flat_type,
             flat_number, longitude, latitude, alias_type IS NULL AS principal
      FROM gnaf_addresses WHERE %s AND address_label IS NOT NULL
    ),
    b AS (
      SELECT *, CASE WHEN building_name IS NOT NULL AND
                          starts_with(lbl, building_name || ' ')
                     THEN substr(lbl, length(building_name) + 2) ELSE lbl END AS l1
      FROM g
    )
    SELECT address_detail_pid, %s AS k, %s AS core, longitude, latitude, principal
    FROM b", where, key("lbl"), key(.pli_core_sql("l1", "flat_type", "flat_number")))
  DBI::dbExecute(con, paste("CREATE OR REPLACE TEMP TABLE __gnafr_pli_gk__ AS",
    gnaf_keys("state = 'QLD' AND coalesce(source, '') <> 'pli'")))
  DBI::dbExecute(con, paste("CREATE OR REPLACE TEMP TABLE __gnafr_pli_prev__ AS",
    gnaf_keys(if (isTRUE(overwrite)) "FALSE" else "source = 'pli'")))

  DBI::dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE __gnafr_pli_new__ AS
    WITH s AS (
      SELECT l.*,
        CASE WHEN l.reason IS NOT NULL THEN l.reason
             WHEN l.lookup_key IN (SELECT k FROM __gnafr_pli_gk__) THEN 'in_gnaf'
             WHEN l.core_key IN (SELECT core FROM __gnafr_pli_gk__) THEN 'in_gnaf_relaxed'
             WHEN l.core_key IN (SELECT core FROM __gnafr_pli_prev__)
               OR ('PLI' || l.address_pid) IN
                  (SELECT address_detail_pid FROM __gnafr_pli_prev__)
               THEN 'already_loaded'
        END AS st0
      FROM __gnafr_pli_lab__ l
    ),
    cand AS (
      SELECT address_pid, row_number() OVER (
               PARTITION BY core_key ORDER BY address_pid) AS rn
      FROM s WHERE st0 IS NULL
    )
    SELECT s.* EXCLUDE (st0),
           coalesce(s.st0, CASE WHEN cand.rn > 1 THEN 'duplicate_label' ELSE 'added' END)
             AS status
    FROM s LEFT JOIN cand USING (address_pid)")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS __gnafr_pli_lab__")

  st <- DBI::dbGetQuery(con, "SELECT status, count(*) AS n FROM __gnafr_pli_new__ GROUP BY 1")
  get <- function(s) if (s %in% st$status) st$n[st$status == s] else 0L
  methods <- DBI::dbGetQuery(con, "
    SELECT postcode_method AS how, count(*) AS n FROM __gnafr_pli_new__
    WHERE postcode_method IS NOT NULL GROUP BY 1 ORDER BY 2 DESC")
  list(unparseable_unit = get("unparseable_unit"),
       no_number_or_lot = get("no_number_or_lot"), no_postcode = get("no_postcode"),
       in_gnaf = get("in_gnaf"), in_gnaf_relaxed = get("in_gnaf_relaxed"),
       already_loaded = get("already_loaded"),
       duplicate_label = get("duplicate_label"), added = get("added"),
       postcode_methods = methods)
}

# A label with its leading flat word made generic (UNIT), so a unit that one
# source calls a SHOP and the other a UNIT still compares equal.
.pli_core_sql <- function(label, flat_type, flat_number) {
  sprintf(paste0("CASE WHEN %2$s IS NOT NULL AND %3$s IS NOT NULL AND ",
                 "starts_with(%1$s, %2$s || ' ') THEN 'UNIT ' || substr(%1$s, ",
                 "length(%2$s) + 2) ELSE %1$s END"), label, flat_type, flat_number)
}

# GDA94 points become GDA2020 (only the addresses that will be added, plus a
# sample of those G-NAF already has, for the check).
.pli_convert_coordinates <- function(con) {
  pts <- data.table::setDT(DBI::dbGetQuery(con, "
    SELECT address_pid, lon, lat, datum FROM __gnafr_pli_new__ WHERE status = 'added'"))
  pts <- .pli_to_gda2020(pts)
  duckdb::duckdb_register(con, "__gnafr_pli_xy__", pts[, .(address_pid, lon2020, lat2020)],
                          overwrite = TRUE)
  overlap <- data.table::setDT(DBI::dbGetQuery(con, "
    SELECT * FROM (
      SELECT n.lon, n.lat, n.datum, g.longitude AS glon, g.latitude AS glat
      FROM __gnafr_pli_new__ n
      JOIN (SELECT k, any_value(longitude) AS longitude, any_value(latitude) AS latitude
            FROM __gnafr_pli_gk__ WHERE principal AND longitude IS NOT NULL GROUP BY k) g
        ON g.k = n.lookup_key
      WHERE n.status = 'in_gnaf') USING SAMPLE 20000 ROWS"))
  list(points = pts, overlap = overlap)
}

# Transform lon/lat to GDA2020 with PROJ, row by row's declared datum.
.pli_to_gda2020 <- function(pts) {
  pts[, `:=`(lon2020 = lon, lat2020 = lat)]
  for (datum in setdiff(unique(pts$datum), "GDA2020")) {
    rows <- which(pts$datum == datum & !is.na(pts$lon) & !is.na(pts$lat))
    if (!length(rows)) next
    out <- sf::sf_project(.PLI_DATUMS[[datum]], .PLI_DATUMS[["GDA2020"]],
                          cbind(pts$lon[rows], pts$lat[rows]))
    data.table::set(pts, rows, "lon2020", out[, 1L])
    data.table::set(pts, rows, "lat2020", out[, 2L])
  }
  pts
}

# Distance in metres between two lon/lat pairs (small distances).
.pli_metres <- function(lon1, lat1, lon2, lat2) {
  k <- 111320
  sqrt(((lat1 - lat2) * k)^2 + ((lon1 - lon2) * k * cos(lat1 * pi / 180))^2)
}

# How far the PLI's points sit from G-NAF's for addresses both have, as supplied
# and after conversion: the conversion is right when the second is the smaller.
.pli_coordinate_check <- function(con, overlap) {
  if (nrow(overlap) == 0L)
    return(list(n = 0L, median_m_supplied = NA_real_, median_m_gda2020 = NA_real_))
  conv <- .pli_to_gda2020(data.table::copy(overlap))
  list(n = nrow(overlap),
       median_m_supplied = stats::median(.pli_metres(overlap$lon, overlap$lat,
                                                      overlap$glon, overlap$glat)),
       median_m_gda2020 = stats::median(.pli_metres(conv$lon2020, conv$lat2020,
                                                     overlap$glon, overlap$glat)))
}

.pli_check_collisions <- function(con) {
  clash <- DBI::dbGetQuery(con, "
    SELECT count(*) AS n FROM __gnafr_pli_new__ n
    WHERE n.status = 'added' AND (
      ('PLI' || n.address_pid) IN (SELECT address_detail_pid FROM gnaf_addresses
                                   WHERE coalesce(source, '') <> 'pli')
      OR ('PLI' || n.address_pid) IN (SELECT address_detail_pid FROM custom_addresses))")$n
  if (clash > 0L)
    stop(clash, " PLI address(es) would reuse the PID of an existing address; ",
         "nothing was added.", call. = FALSE)
  invisible(TRUE)
}

.pli_insert <- function(con) {
  type_sql <- .street_type_case_sql("n.STREET_TYPE_CODE")
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (
      address_detail_pid, address_label, address_site_name, building_name,
      flat_type, flat_number, level_type, level_number, number_first, number_last,
      lot_number, street_name, street_type, street_suffix, locality_name, state,
      postcode, longitude, latitude, source, alias_type, date_created,
      legal_parcel_id, mb_code, alias_principal, principal_pid,
      primary_secondary, primary_pid, geocode_type)
    SELECT
      'PLI' || n.address_pid, n.address_label, n.property_name, NULL,
      n.FLAT_TYPE_CODE,
      CASE WHEN n.FLAT_NUMBER IS NOT NULL THEN
        coalesce(n.FLAT_NUMBER_PREFIX, '') ||
        CAST(TRY_CAST(n.FLAT_NUMBER AS INTEGER) AS VARCHAR) ||
        coalesce(n.FLAT_NUMBER_SUFFIX, '') END,
      NULL, NULL, n.NUMBER_FIRST, n.NUMBER_LAST,
      n.LOT_NUMBER, n.STREET_NAME, (%s), n.STREET_SUFFIX_CODE, n.LOCALITY_NAME, 'QLD',
      n.POSTCODE, x.lon2020, x.lat2020, 'pli', NULL, NULL,
      CASE WHEN n.lot IS NOT NULL AND n.plan IS NOT NULL THEN n.lot || '/' || n.plan END,
      NULL, 'PRINCIPAL', NULL, NULL, NULL, n.geocode_type
    FROM __gnafr_pli_new__ n
    JOIN __gnafr_pli_xy__ x USING (address_pid)
    WHERE n.status = 'added'", type_sql))
  invisible(NULL)
}

.pli_refresh_indexes <- function(con) {
  gnaf_rebuild_locality_index(con)
  if (DBI::dbExistsTable(con, "gnaf_street_type_index"))
    gnaf_rebuild_street_type_index(con)
  state <- tryCatch(.exact_index_state_any(con), error = function(e) NULL)
  if (!is.null(state)) gnaf_rebuild_exact_index(con, variants = state$variants)
  DBI::dbExecute(con, "ANALYZE gnaf_addresses")
  .invalidate_match_cache(con)
  invisible(NULL)
}

# The exact-label index's settings if the database has one, whether or not it
# is still current (the PLI load is what makes it stale).
.exact_index_state_any <- function(con) {
  if (!DBI::dbExistsTable(con, "gnaf_exact_index_meta")) return(NULL)
  meta <- DBI::dbGetQuery(con, "SELECT variants FROM gnaf_exact_index_meta")
  if (nrow(meta) != 1L) return(NULL)
  list(variants = isTRUE(meta$variants))
}

.pli_summary <- function(n_read, counts, stages) {
  n_addr <- counts$addresses
  usable <- n_addr - stages$unparseable_unit - stages$no_number_or_lot -
    stages$no_postcode
  out <- data.table::data.table(
    stage = c("PLI records read", "Records considered (status filters)",
              "Unique addresses", "Unusable: unit number not understood",
              "Unusable: no street number or lot", "Unusable: no postcode found in G-NAF",
              "Usable addresses", "Already in G-NAF (same label)",
              "Already in G-NAF (same label ignoring building name / flat type)",
              "Already loaded from a previous PLI file",
              "Duplicate label within the PLI", "Added"),
    n = as.numeric(c(n_read, counts$considered_rows, n_addr, stages$unparseable_unit,
                     stages$no_number_or_lot, stages$no_postcode, usable, stages$in_gnaf,
                     stages$in_gnaf_relaxed, stages$already_loaded,
                     stages$duplicate_label, stages$added)),
    pct_of = c("records read", "records read", "records considered", "unique addresses",
               "unique addresses", "unique addresses", "unique addresses",
               rep("usable addresses", 5L)))
  base <- c(n_read, n_read, counts$considered_rows, n_addr, n_addr, n_addr, n_addr,
            rep(usable, 5L))
  out[, pct := round(100 * n / base, 2)]
  data.table::setcolorder(out, c("stage", "n", "pct", "pct_of"))
  out[]
}

.pli_report <- function(summary, check, elapsed) {
  fmt <- function(n) format(n, big.mark = ",", scientific = FALSE)
  lines <- sprintf("  %-66s %11s  %6.2f%% of %s", summary$stage, fmt(summary$n),
                   summary$pct, summary$pct_of)
  message("\nPLI load summary\n", paste(lines, collapse = "\n"))
  if (!is.na(check$median_m_supplied))
    message(sprintf(paste0("\nCoordinates: for %s addresses in both, the PLI point was ",
                           "%.2f m from G-NAF's as supplied and %.2f m after conversion ",
                           "to GDA2020."),
                    fmt(check$n), check$median_m_supplied, check$median_m_gda2020))
  message(sprintf("Done in %.0f s.", elapsed))
}
