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
