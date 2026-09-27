pli_row <- function(...) {
  base <- list(
    ADDRESS_PID = "1", PLAN = "RP1", LOT = "1", LOTPLAN_STATUS = "C",
    ADDRESS_STATUS = "P", ADDRESS_STANDARD = "UK", UNIT_TYPE = "", UNIT_NUMBER = "",
    UNIT_SUFFIX = "", PROPERTY_NAME = "", STREET_NO_1 = "", STREET_NO_1_SUFFIX = "",
    STREET_NO_2 = "", STREET_NO_2_SUFFIX = "", STREET_NAME = "", STREET_TYPE = "",
    STREET_SUFFIX = "", LOCALITY = "KEPERRA", LOCAL_AUTHORITY = "BRISBANE CITY",
    LGA_CODE = "1000", LATITUDE = "-27.400000", LONGITUDE = "153.000000",
    GEOCODE_TYPE = "PC", DATUM = "GDA94")
  utils::modifyList(base, list(...))
}

# Writes rows as the QSpatial extract is written: pipe-delimited, a UTF-8 BOM,
# CRLF line endings.
pli_file <- function(rows, zip = FALSE, header = gnafr:::.PLI_COLUMNS) {
  lines <- c(paste(header, collapse = "|"),
             vapply(rows, function(r) paste(unlist(r), collapse = "|"), character(1L)))
  txt <- tempfile("DP_PROP_LOCATION_INDEX_QLD_", fileext = ".txt")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), txt)
  if (!zip) return(txt)
  z <- tempfile("DP_PROP_LOCATION_INDEX_QLD_", fileext = ".zip")
  old <- setwd(dirname(txt))
  on.exit(setwd(old), add = TRUE)
  utils::zip(z, basename(txt), flags = "-q")
  z
}

gnaf_fixture <- function(...) {
  # PLI point for the address G-NAF also has, in G-NAF's own datum.
  g1 <- gnafr:::.pli_to_gda2020(data.table::data.table(lon = 153, lat = -27.4, datum = "GDA94"))
  row <- function(pid, label, ..., street_name, street_type = "STREET", locality = "KEPERRA",
                  postcode = 4054L, lon = g1$lon2020, lat = g1$lat2020, alias_type = NA_character_) {
    data.table::data.table(
      address_detail_pid = pid, address_label = label, street_name = street_name,
      street_type = street_type, locality_name = locality, state = "QLD",
      postcode = postcode, longitude = lon, latitude = lat, source = "gnaf",
      alias_type = alias_type, ...)
  }
  rows <- data.table::rbindlist(list(
    row("G1", "11 ROLLESTON STREET, KEPERRA QLD 4054", number_first = 11L,
        street_name = "ROLLESTON"),
    row("G2", "UNIT 5 12 MAIN STREET, KEPERRA QLD 4054", number_first = 12L,
        flat_type = "UNIT", flat_number = "5", street_name = "MAIN"),
    row("G3", "SHOP 1 20 MAIN STREET, KEPERRA QLD 4054", number_first = 20L,
        flat_type = "SHOP", flat_number = "1", street_name = "MAIN"),
    row("G4", "SPHERE UNIT 7 30 MAIN STREET, KEPERRA QLD 4054", number_first = 30L,
        flat_type = "UNIT", flat_number = "7", building_name = "SPHERE", street_name = "MAIN"),
    row("G1_ALIAS", "3 OLD LANE, KEPERRA QLD 4054", number_first = 3L, street_name = "OLD",
        street_type = "LANE", alias_type = "LOCALITY:SYN", principal_pid = "G1"),
    row("AL1", "1 ZED STREET, ALIASVILLE QLD 4200", number_first = 1L, street_name = "ZED",
        locality = "ALIASVILLE", postcode = 4200L, alias_type = "LOCALITY:SYN",
        principal_pid = "G1"),
    row("S1", "1 ALPHA STREET, SPLITVILLE QLD 4100", number_first = 1L, street_name = "ALPHA",
        locality = "SPLITVILLE", postcode = 4100L, lon = 153.0, lat = -27.0),
    row("S2", "1 BETA STREET, SPLITVILLE QLD 4101", number_first = 1L, street_name = "BETA",
        locality = "SPLITVILLE", postcode = 4101L, lon = 153.1, lat = -27.1)
  ), fill = TRUE)
  con <- gnaf_connect(":memory:")
  gnaf_init(con)
  DBI::dbAppendTable(con, "gnaf_addresses", as.data.frame(rows))
  gnaf_rebuild_locality_index(con)
  con
}

pli_rows <- function() {
  r <- function(pid, ...) pli_row(ADDRESS_PID = pid, ...)
  list(
    # already in G-NAF: exact label, exact label despite a property name, an alias label
    r("1001", STREET_NO_1 = "11", STREET_NAME = "ROLLESTON", STREET_TYPE = "STREET"),
    r("1002", UNIT_TYPE = "U", UNIT_NUMBER = "5", STREET_NO_1 = "12", STREET_NAME = "MAIN",
      STREET_TYPE = "STREET", PROPERTY_NAME = "MAIN TOWERS"),
    r("1003", STREET_NO_1 = "3", STREET_NAME = "OLD", STREET_TYPE = "LANE"),
    # already in G-NAF once the building name / flat type is set aside
    r("1004", UNIT_TYPE = "U", UNIT_NUMBER = "7", STREET_NO_1 = "30", STREET_NAME = "MAIN",
      STREET_TYPE = "STREET"),
    r("1014", UNIT_TYPE = "U", UNIT_NUMBER = "1", STREET_NO_1 = "20", STREET_NAME = "MAIN",
      STREET_TYPE = "STREET"),
    # new: number suffix and range, unit number/suffix, lot-only, lot ignored, XXX type,
    # street direction, "(LGA)" locality suffix
    r("1005", STREET_NO_1 = "15", STREET_NO_1_SUFFIX = "A", STREET_NO_2 = "17",
      STREET_NAME = "NEWTON", STREET_TYPE = "STREET", PROPERTY_NAME = "THE  HOMESTEAD"),
    r("1006", UNIT_TYPE = "U", UNIT_NUMBER = "2C", STREET_NO_1 = "9", STREET_NAME = "NEWTON",
      STREET_TYPE = "STREET"),
    r("1007", UNIT_TYPE = "APT", UNIT_NUMBER = "3", UNIT_SUFFIX = "B", STREET_NO_1 = "9",
      STREET_NAME = "NEWTON", STREET_TYPE = "STREET"),
    r("1008", LOT = "24", PLAN = "RP99", STREET_NAME = "RURAL", STREET_TYPE = "ROAD"),
    r("1009", LOT = "5", PLAN = "SP7", STREET_NO_1 = "10", STREET_NAME = "OAK", STREET_TYPE = "STREET"),
    r("1010", STREET_NO_1 = "9", STREET_NAME = "THE ESPLANADE", STREET_TYPE = "XXX"),
    r("1011", STREET_NO_1 = "4", STREET_NAME = "SUFFIX", STREET_TYPE = "ROAD", STREET_SUFFIX = "EAST"),
    r("1012", STREET_NO_1 = "6", STREET_NAME = "PAREN", STREET_TYPE = "STREET",
      LOCALITY = "KEPERRA (BRISBANE CITY)"),
    # one address, two geocodes: the property centroid wins
    r("2000", STREET_NO_1 = "8", STREET_NAME = "GEO", STREET_TYPE = "STREET",
      GEOCODE_TYPE = "BC", LATITUDE = "-27.410000", LONGITUDE = "153.010000"),
    r("2000", STREET_NO_1 = "8", STREET_NAME = "GEO", STREET_TYPE = "STREET",
      GEOCODE_TYPE = "PC", LATITUDE = "-27.420000", LONGITUDE = "153.020000"),
    # two PIDs, one label: added once
    r("3001", STREET_NO_1 = "12", STREET_NAME = "TWIN", STREET_TYPE = "STREET"),
    r("3002", STREET_NO_1 = "12", STREET_NAME = "TWIN", STREET_TYPE = "STREET"),
    # a locality with two postcodes: the street settles one, the nearest address the other
    r("1015", STREET_NO_1 = "5", STREET_NAME = "ALPHA", STREET_TYPE = "STREET",
      LOCALITY = "SPLITVILLE", LATITUDE = "-27.5", LONGITUDE = "153.5"),
    r("1016", STREET_NO_1 = "5", STREET_NAME = "GAMMA", STREET_TYPE = "STREET",
      LOCALITY = "SPLITVILLE", LATITUDE = "-27.090000", LONGITUDE = "153.090000"),
    # alternate address (kept by default) and a deleted lot on plan (dropped by default)
    r("1017", ADDRESS_STATUS = "A", STREET_NO_1 = "20", STREET_NAME = "ALT", STREET_TYPE = "STREET"),
    r("1018", LOTPLAN_STATUS = "D", STREET_NO_1 = "21", STREET_NAME = "DEL", STREET_TYPE = "STREET"),
    # unusable: no number and a placeholder lot, unit text, a locality G-NAF lacks
    r("1019", LOT = "9999", STREET_NAME = "NONUM", STREET_TYPE = "ROAD"),
    r("1020", UNIT_TYPE = "U", UNIT_NUMBER = "SP", STREET_NO_1 = "2", STREET_NAME = "BADUNIT",
      STREET_TYPE = "STREET"),
    r("1021", STREET_NO_1 = "2", STREET_NAME = "LOST", STREET_TYPE = "STREET", LOCALITY = "NOWHERE"),
    # a locality G-NAF only knows as an alias name still yields its postcode
    r("1022", STREET_NO_1 = "7", STREET_NAME = "ZED", STREET_TYPE = "STREET", LOCALITY = "ALIASVILLE")
  )
}

pli_load <- function(con, rows = pli_rows(), ..., zip = FALSE) {
  suppressMessages(gnaf_load_pli(con, pli_file(rows, zip = zip), verbose = FALSE, ...))
}

pli_loaded <- function(con) {
  data.table::setDT(DBI::dbGetQuery(con,
    "SELECT * FROM gnaf_addresses WHERE source = 'pli' ORDER BY address_detail_pid"))
}

stat <- function(summary, stage) summary$n[summary$stage == stage]

test_that("the load reports what G-NAF already had and what was added", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  s <- pli_load(con)
  expect_identical(stat(s, "PLI records read"), 25)
  expect_identical(stat(s, "Records considered (status filters)"), 24)
  expect_identical(stat(s, "Unique addresses"), 23)
  expect_identical(stat(s, "Unusable: unit number not understood"), 1)
  expect_identical(stat(s, "Unusable: no street number or lot"), 1)
  expect_identical(stat(s, "Unusable: no postcode found in G-NAF"), 1)
  expect_identical(stat(s, "Usable addresses"), 20)
  expect_identical(stat(s, "Already in G-NAF (same label)"), 3)
  expect_identical(
    stat(s, "Already in G-NAF (same label ignoring building name / flat type)"), 2)
  expect_identical(stat(s, "Duplicate label within the PLI"), 1)
  expect_identical(stat(s, "Added"), 14)
  # the stages account for every usable address
  expect_identical(sum(s$n[s$stage %in% c("Already in G-NAF (same label)",
    "Already in G-NAF (same label ignoring building name / flat type)",
    "Already loaded from a previous PLI file", "Duplicate label within the PLI",
    "Added")]), stat(s, "Usable addresses"))
  expect_equal(s$pct[s$stage == "Added"], round(100 * 14 / 20, 2))
  expect_equal(nrow(pli_loaded(con)), 14L)
})

test_that("labels and components are wrangled as G-NAF's are", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  pli_load(con)
  d <- pli_loaded(con)
  get <- function(pid) d[address_detail_pid == paste0("PLI", pid)]
  # number suffix and range stay in the label; the number is stored without the suffix
  expect_identical(get("1005")$address_label, "15A-17 NEWTON STREET, KEPERRA QLD 4054")
  expect_identical(c(get("1005")$number_first, get("1005")$number_last), c(15L, 17L))
  # the property name is a site name: kept as such, out of the label
  expect_identical(get("1005")$address_site_name, "THE HOMESTEAD")
  expect_true(is.na(get("1005")$building_name))
  # U is UNIT; a lettered unit number and a unit suffix are both kept
  expect_identical(get("1006")$address_label, "UNIT 2C 9 NEWTON STREET, KEPERRA QLD 4054")
  expect_identical(get("1006")[, .(flat_type, flat_number)], data.table::data.table(flat_type = "UNIT", flat_number = "2C"))
  expect_identical(get("1007")$address_label, "APT 3B 9 NEWTON STREET, KEPERRA QLD 4054")
  expect_identical(get("1007")$flat_number, "3B")
  # a lot is used only when there is no street number
  expect_identical(get("1008")$address_label, "LOT 24 RURAL ROAD, KEPERRA QLD 4054")
  expect_identical(get("1008")$lot_number, "24")
  expect_identical(get("1009")$address_label, "10 OAK STREET, KEPERRA QLD 4054")
  expect_true(is.na(get("1009")$lot_number))
  # ... but the lot on plan is always kept, as G-NAF keeps it
  expect_identical(c(get("1008")$legal_parcel_id, get("1009")$legal_parcel_id), c("24/RP99", "5/SP7"))
  # the XXX street-type placeholder is no type; directions become codes
  expect_identical(get("1010")$address_label, "9 THE ESPLANADE, KEPERRA QLD 4054")
  expect_true(is.na(get("1010")$street_type))
  expect_identical(get("1011")$address_label, "4 SUFFIX ROAD E, KEPERRA QLD 4054")
  expect_identical(get("1011")$street_suffix, "E")
  # the PLI's "(LGA)" locality qualifier is dropped
  expect_identical(get("1012")$locality_name, "KEPERRA")
  # state, source, principal-ness and identifiers
  expect_true(all(d$state == "QLD"))
  expect_true(all(d$source == "pli"))
  expect_true(all(is.na(d$alias_type) & is.na(d$principal_pid)))
  expect_true(all(d$alias_principal == "PRINCIPAL"))
  expect_true(all(startsWith(d$address_detail_pid, "PLI")))
  expect_false(any(d$address_detail_pid %in% DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses WHERE source = 'gnaf'")$address_detail_pid))
})

test_that("postcodes come from G-NAF's locality, then the street, then the nearest address", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  pli_load(con)
  d <- pli_loaded(con)
  expect_true(all(d[locality_name == "KEPERRA", postcode] == 4054L))
  expect_identical(d[address_detail_pid == "PLI1015", postcode], 4100L)   # ALPHA STREET
  expect_identical(d[address_detail_pid == "PLI1016", postcode], 4101L)   # nearest is BETA
  expect_identical(d[address_detail_pid == "PLI1016", address_label],
                   "5 GAMMA STREET, SPLITVILLE QLD 4101")
  # a locality that G-NAF only has as an alias name still resolves ...
  expect_identical(d[address_detail_pid == "PLI1022", postcode], 4200L)
  # ... and loading the same file again cannot change any postcode (so add nothing)
  expect_identical(stat(pli_load(con), "Added"), 0)
})

test_that("one geocode per address is kept, the property centroid, converted to GDA2020", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  s <- pli_load(con)
  d <- pli_loaded(con)[address_detail_pid == "PLI2000"]
  expect_identical(d$geocode_type, "PC")
  supplied <- data.table::data.table(lon = 153.02, lat = -27.42, datum = "GDA94")
  converted <- gnafr:::.pli_to_gda2020(data.table::copy(supplied))
  expect_equal(c(d$longitude, d$latitude), c(converted$lon2020, converted$lat2020), tolerance = 1e-9)
  moved <- gnafr:::.pli_metres(153.02, -27.42, d$longitude, d$latitude)
  expect_gt(moved, 1)
  expect_lt(moved, 2.5)
  expect_gt(d$latitude, -27.42)   # GDA2020 sits north-east of GDA94 in Queensland
  # the check against G-NAF: shared addresses sit ~1.5 m apart as supplied, on top of
  # each other once converted
  check <- attr(s, "coordinate_check")
  expect_gt(check$median_m_supplied, 1)
  expect_lt(check$median_m_gda2020, 0.01)
  expect_identical(attr(s, "datum")$datum, "GDA94")
  # a file already in GDA2020 is left alone
  expect_equal(gnafr:::.pli_to_gda2020(data.table::data.table(lon = 153, lat = -27.4,
    datum = "GDA2020"))$lat2020, -27.4)
})

test_that("status filters and the unusable reasons behave", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  s <- pli_load(con, address_status = "P")
  expect_identical(stat(s, "Added"), 13)
  expect_false("PLI1017" %in% pli_loaded(con)$address_detail_pid)
  s <- pli_load(con, overwrite = TRUE, include_deleted_lotplans = TRUE)
  expect_true("PLI1018" %in% pli_loaded(con)$address_detail_pid)
  expect_identical(stat(s, "Added"), 15)
  expect_error(gnaf_load_pli(con, pli_file(pli_rows()), address_status = character()),
               "address_status")
  expect_error(gnaf_load_pli(con, pli_file(pli_rows()), overwrite = NA), "overwrite")
})

test_that("reloading adds nothing twice; overwrite replaces the earlier PLI rows", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  pli_load(con)
  first <- pli_loaded(con)
  again <- pli_load(con)
  expect_identical(stat(again, "Added"), 0)
  # the duplicate-label row now finds its label already loaded
  expect_identical(stat(again, "Already loaded from a previous PLI file"), 15)
  expect_identical(pli_loaded(con), first)
  replaced <- pli_load(con, overwrite = TRUE)
  expect_identical(stat(replaced, "Added"), 14)
  expect_identical(pli_loaded(con), first)
  # G-NAF's own rows are never touched
  expect_identical(DBI::dbGetQuery(con,
    "SELECT count(*) AS n FROM gnaf_addresses WHERE source = 'gnaf'")$n, 8)
})

test_that("a zipped extract loads the same as the text file", {
  skip_if(Sys.which("zip") == "", "no zip utility")
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  s <- pli_load(con, zip = TRUE)
  expect_identical(stat(s, "Added"), 14)
})

test_that("added addresses are matched like any G-NAF address", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  pli_load(con)
  out <- gnaf_match(c("Unit 2C, 9 Newton St, Keperra QLD 4054", "15a-17 newton street keperra",
                      "11 Rolleston Street, Keperra QLD 4054"),
                    con, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, c("PLI1006", "PLI1005", "G1"))
  expect_identical(out$source, c("pli", "pli", "gnaf"))
})

test_that("the exact-label index is refreshed, keeping its settings", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_rebuild_exact_index(con, variants = FALSE)
  expect_false(is.null(gnafr:::.exact_index_state(con)))
  pli_load(con)
  state <- gnafr:::.exact_index_state(con)
  expect_false(is.null(state))          # rebuilt, not left stale
  expect_false(state$variants)
  expect_true("PLI1006" %in% DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_exact_index")$address_detail_pid)
  # a database without the index does not gain one
  con2 <- gnaf_fixture()
  on.exit(gnaf_disconnect(con2), add = TRUE)
  pli_load(con2)
  expect_false(DBI::dbExistsTable(con2, "gnaf_exact_index"))
})

test_that("the match cache is cleared and locality/street-type indexes refreshed", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score, algorithm_version) VALUES ('X', 'G1', 100, 1)")
  pli_load(con)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM gnaf_match_cache")$n, 0)
  expect_true("THE ESPLANADE" %in% DBI::dbGetQuery(con,
    "SELECT street_name FROM gnaf_street_type_index")$street_name)
})

test_that("a PID that would collide with an existing address stops the load", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = "PLI1006", address_label = "1 CUSTOM STREET, KEPERRA QLD 4054",
    number_first = 1L, street_name = "CUSTOM", street_type = "STREET",
    locality_name = "KEPERRA", state = "QLD", postcode = 4054L)))
  expect_error(pli_load(con), "reuse the PID")
  expect_equal(nrow(pli_loaded(con)), 0L)
})

test_that("bad input is refused with a clear message", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_error(gnaf_load_pli(con, tempfile(fileext = ".txt")), "existing PLI")
  expect_error(gnaf_load_pli(con, pli_file(pli_rows(), header = rev(gnafr:::.PLI_COLUMNS))),
               "Unexpected PLI columns")
  expect_error(gnaf_load_pli(con, pli_file(pli_rows(), header = gnafr:::.PLI_COLUMNS[-1L])),
               "24 pipe-delimited columns")
  odd <- pli_rows()
  odd[[1L]]$DATUM <- "AGD66"
  expect_error(pli_load(con, odd), "Unsupported DATUM")
  empty <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(empty), add = TRUE)
  gnaf_init(empty)
  expect_error(gnaf_load_pli(empty, pli_file(pli_rows())), "load G-NAF first")
  nonqld <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(nonqld), add = TRUE)
  gnaf_init(nonqld)
  DBI::dbExecute(nonqld, "INSERT INTO gnaf_addresses (address_detail_pid, address_label, state, postcode, source)
    VALUES ('N1', '1 A STREET, SYDNEY NSW 2000', 'NSW', 2000, 'gnaf')")
  expect_error(gnaf_load_pli(nonqld, pli_file(pli_rows())), "no Queensland")
})

test_that("the CLI-style summary prints without error", {
  con <- gnaf_fixture()
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_message(gnaf_load_pli(con, pli_file(pli_rows()), verbose = TRUE), "PLI load summary")
})
