core_csv_row <- function() {
  fields <- c(
    "ADDRESS_DETAIL_PID", "ADDRESS_LABEL", "ADDRESS_SITE_NAME", "BUILDING_NAME",
    "FLAT_TYPE", "FLAT_NUMBER", "LEVEL_TYPE", "LEVEL_NUMBER", "NUMBER_FIRST",
    "NUMBER_LAST", "LOT_NUMBER", "STREET_NAME", "STREET_TYPE", "STREET_SUFFIX",
    "LOCALITY_NAME", "STATE", "POSTCODE", "LONGITUDE", "LATITUDE", "DATE_CREATED",
    "LEGAL_PARCEL_ID", "MB_CODE", "ALIAS_PRINCIPAL", "PRINCIPAL_PID",
    "PRIMARY_SECONDARY", "PRIMARY_PID", "GEOCODE_TYPE"
  )
  row <- data.table::as.data.table(stats::setNames(as.list(rep(NA_character_, length(fields))), fields))
  row[, `:=`(ADDRESS_DETAIL_PID = "NEW", ADDRESS_LABEL = "UNIT 01 10 SMITH ROAD, BRISBANE QLD 4000",
             NUMBER_FIRST = "10", STREET_NAME = "SMITH", STREET_TYPE = "RD",
             LOCALITY_NAME = "BRISBANE", STATE = "QLD", POSTCODE = "4000",
             FLAT_TYPE = "UNIT", FLAT_NUMBER = "01", DATE_CREATED = "2017-07-27")]
  row
}

test_that("CSV imports preserve string identifiers and ISO dates in quoted paths", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  path <- tempfile(pattern = "gnaf's-", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(core_csv_row(), path)
  expect_equal(suppressMessages(gnaf_load(con, path)), 1)
  row <- DBI::dbGetQuery(con, "SELECT flat_number, street_type, date_created FROM gnaf_addresses")
  expect_equal(row$flat_number, "01")
  expect_equal(row$street_type, "ROAD")
  expect_equal(row$date_created, as.Date("2017-07-27"))
})

test_that("gnaf_load backfills a street-type split for rows with a blank street_type", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  row <- core_csv_row()
  row[, `:=`(STREET_NAME = "THE POINT CIRCUIT", STREET_TYPE = NA_character_)]
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(row, path)
  expect_equal(suppressMessages(gnaf_load(con, path)), 1)
  idx <- DBI::dbGetQuery(con, "SELECT * FROM gnaf_street_type_index")
  expect_equal(nrow(idx), 1L)
  expect_equal(idx$street_name, "THE POINT CIRCUIT")
  expect_equal(idx$effective_name, "THE POINT")
  expect_equal(idx$effective_type, "CIRCUIT")
})

test_that("failed multi-file CSV replacement retains addresses, index and cache", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, source, locality_name, state, postcode)
    VALUES ('OLD', 'gnaf', 'BRISBANE', 'QLD', 4000)")
  gnaf_rebuild_locality_index(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score) VALUES ('OLD', 'OLD', 100)")
  paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  on.exit(unlink(paths), add = TRUE)
  data.table::fwrite(core_csv_row(), paths[1L])
  data.table::fwrite(data.table::data.table(wrong_column = "bad"), paths[2L])
  expect_error(suppressMessages(gnaf_load(con, paths, overwrite = TRUE)))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses")$address_detail_pid, "OLD")
  expect_equal(DBI::dbGetQuery(con,
    "SELECT locality_name FROM gnaf_locality_index")$locality_name, "BRISBANE")
  expect_equal(gnaf_cache_status(con)$rows, 1)
})

test_that("failed PSV replacement retains the previous state", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, source, state) VALUES ('OLD', 'gnaf', 'QLD')")
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  specs <- c("ADDRESS_DETAIL", "ADDRESS_DEFAULT_GEOCODE", "STREET_LOCALITY",
             "LOCALITY", "ADDRESS_ALIAS", "PRIMARY_SECONDARY", "ADDRESS_SITE",
             "ADDRESS_MESH_BLOCK_2021", "MB_2021")
  for (spec in specs) writeLines(c("wrong_column", "bad"),
    file.path(path, paste0("QLD_", spec, "_psv.psv")))
  expect_error(suppressMessages(gnaf_load_psv(con, path, overwrite = TRUE, load_aliases = FALSE)))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses")$address_detail_pid, "OLD")
})

collapse_csv_rows <- function() {
  rows <- core_csv_row()[rep(1L, 14L)]
  rows[, `:=`(
    ADDRESS_DETAIL_PID = c("P", "S", "DIFFERENT_LON", "DIFFERENT_LAT",
      "MISSING_LON", "MISSING_LAT", "ORPHAN", "UNRELATED", "ALIAS",
      "NESTED_ALIAS", "PRIMARY_ALIAS", "NO_GEO_P", "NO_GEO_S", "NONFINITE"),
    PRIMARY_SECONDARY = "SECONDARY", PRIMARY_PID = "P",
    LONGITUDE = "153.01", LATITUDE = "-27.41",
    ALIAS_PRINCIPAL = "PRINCIPAL"
  )]
  rows[ADDRESS_DETAIL_PID %in% c("P", "UNRELATED", "NO_GEO_P"), `:=`(
    PRIMARY_SECONDARY = "PRIMARY", PRIMARY_PID = NA_character_,
    FLAT_TYPE = NA_character_, FLAT_NUMBER = NA_character_,
    ADDRESS_LABEL = "10 SMITH ROAD, BRISBANE QLD 4000")]
  rows[ADDRESS_DETAIL_PID == "S", PRIMARY_SECONDARY := "S"]
  rows[ADDRESS_DETAIL_PID == "P", PRIMARY_SECONDARY := "P"]
  rows[ADDRESS_DETAIL_PID == "DIFFERENT_LON", LONGITUDE := "153.01000001"]
  rows[ADDRESS_DETAIL_PID == "DIFFERENT_LAT", LATITUDE := "-27.41000001"]
  rows[ADDRESS_DETAIL_PID == "MISSING_LON", LONGITUDE := NA_character_]
  rows[ADDRESS_DETAIL_PID == "MISSING_LAT", LATITUDE := NA_character_]
  rows[ADDRESS_DETAIL_PID == "ORPHAN", PRIMARY_PID := "ABSENT"]
  rows[ADDRESS_DETAIL_PID %in% c("NO_GEO_P", "NO_GEO_S"),
       `:=`(LONGITUDE = NA_character_, LATITUDE = NA_character_)]
  rows[ADDRESS_DETAIL_PID == "NO_GEO_S", PRIMARY_PID := "NO_GEO_P"]
  rows[ADDRESS_DETAIL_PID == "NONFINITE", LONGITUDE := "Inf"]
  rows[ADDRESS_DETAIL_PID %in% c("ALIAS", "NESTED_ALIAS", "PRIMARY_ALIAS"),
       `:=`(PRIMARY_SECONDARY = NA_character_, PRIMARY_PID = NA_character_,
            ALIAS_PRINCIPAL = "ALIAS",
            PRINCIPAL_PID = c("S", "ALIAS", "P"))]
  rows
}

test_that("CSV collapse is opt-in, exact, linked and restricted to GNAF", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  rows <- collapse_csv_rows()
  data.table::fwrite(rows, path)
  expect_equal(suppressMessages(gnaf_load(con, path)), nrow(rows))
  DBI::dbExecute(con, "INSERT INTO custom_addresses SELECT * FROM gnaf_addresses")
  DBI::dbExecute(con, "UPDATE custom_addresses SET source = 'custom'")
  custom_before <- DBI::dbGetQuery(con, "SELECT * FROM custom_addresses ORDER BY address_detail_pid")
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score) VALUES ('UNIT', 'S', 100)")

  expect_equal(suppressMessages(gnaf_load(con, path, collapse_same_coordinates = TRUE)), 11)
  expect_setequal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses")$address_detail_pid,
    setdiff(rows$ADDRESS_DETAIL_PID, c("S", "ALIAS", "NESTED_ALIAS")))
  expect_identical(DBI::dbGetQuery(con,
    "SELECT * FROM custom_addresses ORDER BY address_detail_pid"), custom_before)
  expect_equal(gnaf_cache_status(con)$rows, 0)
  expect_equal(suppressMessages(gnaf_load(con, path, collapse_same_coordinates = TRUE)), 11)
  expect_equal(suppressMessages(gnaf_load(con, path)), nrow(rows))
})

test_that("CSV collapse waits for the complete batch and matching returns the primary", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  on.exit(unlink(paths), add = TRUE)
  rows <- collapse_csv_rows()[ADDRESS_DETAIL_PID %in% c("P", "S")]
  data.table::fwrite(rows[ADDRESS_DETAIL_PID == "S"], paths[1L])
  data.table::fwrite(rows[ADDRESS_DETAIL_PID == "P"], paths[2L])
  expect_equal(suppressMessages(gnaf_load(con, paths, collapse_same_coordinates = TRUE)), 1)
  out <- gnaf_match("UNIT 01 10 SMITH ROAD, BRISBANE QLD 4000", con,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "P")
  expect_identical(out$address_label, rows[ADDRESS_DETAIL_PID == "P", ADDRESS_LABEL])
  expect_true(is.na(out$flat_number))
})

test_that("collapse and index changes roll back together on a failed load", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(collapse_csv_rows(), path)
  suppressMessages(gnaf_load(con, path))
  before <- DBI::dbGetQuery(con, "SELECT * FROM gnaf_addresses ORDER BY address_detail_pid")
  testthat::local_mocked_bindings(
    gnaf_rebuild_locality_index = function(con) stop("index failed"))
  expect_error(suppressMessages(gnaf_load(con, path,
    collapse_same_coordinates = TRUE)), "index failed")
  expect_identical(DBI::dbGetQuery(con,
    "SELECT * FROM gnaf_addresses ORDER BY address_detail_pid"), before)
})

test_that("collapse_same_coordinates requires a single non-missing logical", {
  for (value in list(NA, NULL, logical(), c(TRUE, FALSE), 1, "TRUE")) {
    expect_error(gnaf_load(NULL, "unused", collapse_same_coordinates = value),
                 "'collapse_same_coordinates' must be TRUE or FALSE", fixed = TRUE)
    expect_error(gnaf_load_psv(NULL, "unused", collapse_same_coordinates = value),
                 "'collapse_same_coordinates' must be TRUE or FALSE", fixed = TRUE)
    expect_error(gnaf_build_db(NULL, "unused", collapse_same_coordinates = value),
                 "'collapse_same_coordinates' must be TRUE or FALSE", fixed = TRUE)
  }
})

write_collapse_psv <- function(path, state) {
  pids <- paste0(state, c("_P", "_S", "_A"))
  detail <- data.table::data.table(
    ADDRESS_DETAIL_PID = pids, DATE_RETIRED = NA_character_,
    DATE_CREATED = "2020-01-01", ADDRESS_SITE_PID = "SITE",
    STREET_LOCALITY_PID = "STREET", LOCALITY_PID = "LOC", POSTCODE = "4000",
    NUMBER_FIRST = "10", FLAT_NUMBER = c(NA_character_, "1", "1"),
    ALIAS_PRINCIPAL = c("P", "P", "A"), PRIMARY_SECONDARY = c("P", "S", NA_character_)
  )
  for (column in c("BUILDING_NAME", "LOT_NUMBER", "LOT_NUMBER_PREFIX", "LOT_NUMBER_SUFFIX",
                   "FLAT_TYPE_CODE", "FLAT_NUMBER_PREFIX", "FLAT_NUMBER_SUFFIX",
                   "LEVEL_TYPE_CODE", "LEVEL_NUMBER", "LEVEL_NUMBER_PREFIX",
                   "LEVEL_NUMBER_SUFFIX", "NUMBER_FIRST_PREFIX", "NUMBER_FIRST_SUFFIX",
                   "NUMBER_LAST", "NUMBER_LAST_PREFIX", "NUMBER_LAST_SUFFIX", "LEGAL_PARCEL_ID"))
    data.table::set(detail, j = column, value = NA_character_)
  tables <- list(
    ADDRESS_DETAIL = detail,
    ADDRESS_DEFAULT_GEOCODE = data.table::data.table(
      ADDRESS_DETAIL_PID = pids, LONGITUDE = 153.01, LATITUDE = -27.41,
      GEOCODE_TYPE_CODE = "PC", DATE_RETIRED = NA_character_),
    STREET_LOCALITY = data.table::data.table(STREET_LOCALITY_PID = "STREET",
      STREET_NAME = "SMITH", STREET_TYPE_CODE = "RD", STREET_SUFFIX_CODE = NA_character_,
      DATE_RETIRED = NA_character_),
    LOCALITY = data.table::data.table(LOCALITY_PID = "LOC", LOCALITY_NAME = "BRISBANE",
      DATE_RETIRED = NA_character_),
    ADDRESS_ALIAS = data.table::data.table(ALIAS_PID = pids[3L], PRINCIPAL_PID = pids[2L],
      ALIAS_TYPE_CODE = "RA", DATE_RETIRED = NA_character_),
    PRIMARY_SECONDARY = data.table::data.table(SECONDARY_PID = pids[2L], PRIMARY_PID = pids[1L],
      DATE_RETIRED = NA_character_),
    ADDRESS_SITE = data.table::data.table(ADDRESS_SITE_PID = "SITE", ADDRESS_SITE_NAME = "SITE NAME",
      DATE_RETIRED = NA_character_),
    ADDRESS_MESH_BLOCK_2021 = data.table::data.table(ADDRESS_DETAIL_PID = pids, MB_2021_PID = "MB",
      DATE_RETIRED = NA_character_),
    MB_2021 = data.table::data.table(MB_2021_PID = "MB", MB_2021_CODE = "123"),
    LOCALITY_ALIAS = data.table::data.table(LOCALITY_ALIAS_PID = "LA", LOCALITY_PID = "LOC",
      NAME = "OTHER SUBURB", POSTCODE = "4001", ALIAS_TYPE_CODE = "SYN", DATE_RETIRED = NA_character_),
    STREET_LOCALITY_ALIAS = data.table::data.table(STREET_LOCALITY_ALIAS_PID = "SA",
      STREET_LOCALITY_PID = "STREET", STREET_NAME = "OLD SMITH", STREET_TYPE_CODE = "RD",
      STREET_SUFFIX_CODE = NA_character_, ALIAS_TYPE_CODE = "SYN", DATE_RETIRED = NA_character_)
  )
  for (name in names(tables))
    data.table::fwrite(tables[[name]], file.path(path, paste0(state, "_", name, "_psv.psv")), sep = "|")
}

test_that("PSV collapse removes secondary aliases and is scoped to loaded states", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  for (state in c("QLD", "NSW")) write_collapse_psv(path, state)
  expect_equal(suppressMessages(gnaf_load_psv(con, path, state = c("QLD", "NSW"))), 14)
  expect_equal(suppressMessages(gnaf_load_psv(con, path,
    state = "QLD", collapse_same_coordinates = TRUE)), 10)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM gnaf_addresses")
  expect_setequal(rows$address_detail_pid[rows$state == "QLD"],
                  c("QLD_P", "QLD_P_LALA", "QLD_P_SASA"))
  expect_equal(sum(rows$state == "NSW"), 7L)
  expect_equal(suppressMessages(gnaf_load_psv(con, path,
    state = c("QLD", "NSW"), collapse_same_coordinates = TRUE)), 6)

  status <- suppressMessages(gnaf_build_db(con, path, states = "all", overwrite = TRUE,
    load_aliases = FALSE, build_street_aliases = FALSE, collapse_same_coordinates = TRUE))
  expect_equal(status[table == "gnaf_addresses", rows], 2)
  expect_setequal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses")$address_detail_pid, c("QLD_P", "NSW_P"))
})
