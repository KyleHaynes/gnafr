test_that("candidate pruning honours zero and small street weights", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = "TARGET", address_label = "10 XYZ ROAD, BRISBANE QLD 4000",
    number_first = 10L, street_name = "XYZ", street_type = "ROAD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L
  )))
  for (street_weight in c(0, 10)) {
    weights <- list(postcode = 100 - street_weight, suburb = 0,
                    street_name = street_weight, street_type = 0, number = 0, flat = 0)
    out <- gnaf_match("10 ABC Road, Brisbane QLD 4000", con,
                      weights = weights, min_score = 90L, cache = FALSE, verbose = FALSE)
    expect_identical(out$address_detail_pid, "TARGET")
    expect_equal(out$total_score, 100 - street_weight)
  }
})

test_that("locality fallback ranks distinct postcodes rather than locality rows", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = "TARGET", address_label = "10 MAIN ROAD, BRISBANE QLD 4001",
    number_first = 10L, street_name = "MAIN", street_type = "ROAD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4001L
  )))
  # Several closer synonyms for one irrelevant postcode must occupy one slot.
  DBI::dbExecute(con, "DELETE FROM gnaf_locality_index")
  DBI::dbWriteTable(con, "gnaf_locality_index", data.frame(
    locality_name = c(paste0("BRISBANX", LETTERS[1:6]), "BRISBANE"),
    postcode = c(rep(4000L, 6L), 4001L), state = "QLD"
  ), append = TRUE)
  out <- gnaf_match("10 Main Road, Brisbanx QLD 4999", con,
                    normalize = FALSE, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "TARGET")
})

test_that("a locality lost to a street-type/suburb word collision is recovered", {
  # "Point Lookout" is a real QLD suburb, but "LOOKOUT" is also a legitimate
  # street type - with no comma to mark the boundary, address_parse() (which
  # has no database access) picks "LOOKOUT" as the street type and loses the
  # suburb entirely. gnaf_match() should recover it against the real
  # gnaf_locality_index once a connection is available.
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = "TARGET", address_label = "15 CUMMING PARADE, POINT LOOKOUT QLD 4183",
    number_first = 15L, street_name = "CUMMING", street_type = "PARADE",
    locality_name = "POINT LOOKOUT", state = "QLD", postcode = 4183L
  )))

  before <- address_parse("15 Cumming Pde Point Lookout QLD 4183")
  expect_true(is.na(before$in_locality))

  out <- gnaf_match("15 Cumming Pde Point Lookout QLD 4183", con,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "TARGET")
  expect_identical(out$total_score, 100L)
  expect_identical(out$input_standardised,
                   "15 CUMMING PARADE, POINT LOOKOUT QLD 4183")
})

test_that(".recover_missing_locality() leaves already-resolved and unrecoverable rows untouched", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_locality_index VALUES ('POINT LOOKOUT', 4183, 'QLD')")

  parsed <- data.table::data.table(
    input_id = 1:3,
    in_postcode = c(4183L, 4183L, NA_integer_),
    in_state = c("QLD", "QLD", NA_character_),
    in_locality = c(NA_character_, "ALREADY SET", NA_character_),
    in_street_name = c("CUMMING PDE POINT", "SMITH", NA_character_),
    in_street_type = c("LOOKOUT", "STREET", NA_character_),
    in_street_suffix = NA_character_
  )
  before <- data.table::copy(parsed)
  gnafr:::.recover_missing_locality(con, parsed)

  expect_identical(parsed[1L, .(in_locality, in_street_name, in_street_type)],
                   data.table::data.table(in_locality = "POINT LOOKOUT",
                                          in_street_name = "CUMMING",
                                          in_street_type = "PARADE"))
  # Row 2 already had a locality; row 3 had no postcode to search with -
  # neither is this function's job, so both must be untouched.
  expect_identical(parsed[2:3], before[2:3])
})

test_that("database query errors remain errors when matching quietly", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  parsed <- address_parse("10 Main Road, Brisbane QLD 4000")
  DBI::dbRemoveTable(con, "gnaf_addresses")
  expect_error(.match_postcode_duckdb(con, parsed, 1L, 60L,
    .default_match_weights(), FALSE, verbose = FALSE), "query failed")
  expect_false("__gnafr_pc_inputs__" %in% DBI::dbListTables(con))
})

test_that("state matching does not repeat its search through locality fallback", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = "TARGET", address_label = "10 MAIN ROAD, BRISBANE QLD 4000",
    number_first = 10L, street_name = "MAIN", street_type = "ROAD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L
  )))
  testthat::local_mocked_bindings(.match_locality_duckdb = function(...) {
    stop("Redundant locality search")
  })
  out <- gnaf_match("10 Main Road, Brisbane QLD", con, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "TARGET")
  expect_identical(out$total_score, 80L)
})
