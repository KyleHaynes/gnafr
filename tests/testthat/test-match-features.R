feature_results <- function() {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = c("A_WRONG", "Z_RIGHT", "LOT_ONLY"),
    address_label = c("LOT 7 10 MAIN ROAD, BRISBANE QLD 4000",
      "LOT 7 42 MAIN ROAD, BRISBANE QLD 4000", "LOT 7 MAIN ROAD, BRISBANE QLD 4000"),
    lot_number = "7", number_first = c(10L, 42L, NA_integer_),
    street_name = "MAIN", street_type = "ROAD", locality_name = "BRISBANE",
    state = "QLD", postcode = 4000L
  )))
  gnaf_match(c("Lot 7 42 Main Rd, Brisbane QLD 4000", "nowhere"), con,
    max_results = 5L, min_score = 0L, cache = FALSE, verbose = FALSE)
}

test_that("street numbers locate the correct property despite duplicate lots", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = c("A_WRONG", "Z_RIGHT"),
    address_label = c("LOT 7 10 MAIN ROAD, BRISBANE QLD 4000",
                      "42 MAIN ROAD, BRISBANE QLD 4000"),
    lot_number = c("7", NA_character_), number_first = c(10L, 42L),
    street_name = "MAIN", street_type = "ROAD", locality_name = "BRISBANE",
    state = "QLD", postcode = 4000L
  )))
  # Cover postcode and state paths, with fallback disabled so a wrong lot
  # cannot exclude the correct street number at retrieval time.
  for (input in c("Lot 7 42 Main Rd, Brisbane QLD 4000", "Lot 7 42 Main Rd, Brisbane QLD")) {
    out <- gnaf_match(input, con, locality_fallback = FALSE,
      street_number_fallback = FALSE, cache = FALSE, verbose = FALSE)
    expect_identical(out$address_detail_pid, "Z_RIGHT")
    expect_identical(out$score_number, 10L)
  }
  lot <- gnaf_match("Lot 7 Main Rd, Brisbane QLD 4000", con,
                    cache = FALSE, verbose = FALSE)
  expect_identical(lot$address_detail_pid, "A_WRONG")
  expect_identical(lot$score_number, 10L)
})

test_that("a lot-only candidate label cannot masquerade as a house number", {
  pairs <- data.table::data.table(
    address_label = c("LOT 7 MAIN ROAD", "LOT 7 42A MAIN ROAD", "7 LOT ROAD"),
    street_name = c("MAIN", "MAIN", "LOT"))
  expect_identical(.candidate_number_token(pairs), c(NA_character_, "42A", "7"))
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  duckdb::duckdb_register(con, "lot_labels", pairs)
  actual <- DBI::dbGetQuery(con, paste("SELECT", .candidate_number_token_sql(),
                                     "AS token FROM lot_labels g"))$token
  expect_identical(actual, c("", "42A", "7"))
})

test_that("evidence retains separate lot conflicts without mutating ranking results", {
  x <- feature_results()
  x[matched == TRUE, lot_number := "8"]
  before <- data.table::copy(x)
  out <- gnaf_match_features(x)
  expect_identical(x, before)
  expect_identical(out$total_score, before$total_score)
  expect_true(out[matched == TRUE, lot_number_conflict])
  expect_true(out[matched == TRUE, has_identifier_conflict])
  expect_false(out[matched == TRUE, number_conflict])
  expect_equal(out[matched == TRUE, agreement_number], 1)
  added <- setdiff(names(out), names(x))
  expect_true(all(is.na(unlist(out[matched == FALSE, ..added]))))
  empty <- gnaf_match_features(x[0L])
  expect_identical(names(empty), names(out))
  expect_identical(vapply(empty, typeof, ""), vapply(out, typeof, ""))
})

test_that("evidence distinguishes missing identifiers, conflicts and candidate ties", {
  x <- feature_results()[matched == TRUE]
  wrong <- data.table::copy(x)
  wrong[, `:=`(address_detail_pid = "OTHER", number_first = 11L,
    flat_number = "2", in_flat_number = "1", in_state = "NSW")]
  x[, `:=`(in_lot_number = "007", lot_number = NA_character_)]
  out <- gnaf_match_features(data.table::rbindlist(list(x, wrong), fill = TRUE))
  expect_true(out$lot_number_candidate_missing[1L])
  expect_false(out$lot_number_conflict[1L])
  expect_true(out$number_conflict[2L])
  expect_true(out$flat_number_conflict[2L])
  expect_true(out$state_conflict[2L])
  expect_true(all(out$tied_best))
  expect_equal(out$score_gap, c(0, 0))
  duplicate <- gnaf_match_features(data.table::rbindlist(list(x, x)))
  expect_true(all(is.na(duplicate$score_gap)))
  expect_false(any(duplicate$tied_best))
  single <- data.table::copy(x)
  single[, input_id := 9L]
  mixed <- gnaf_match_features(data.table::rbindlist(list(x, wrong, single), fill = TRUE))
  expect_equal(mixed$score_gap, c(0, 0, NA_real_))
  expect_identical(mixed$tied_best, c(TRUE, TRUE, FALSE))
})

test_that("linked-address evidence still refers to the candidate that was ranked", {
  x <- feature_results()
  expected <- gnaf_match_features(x)
  fields <- c("address_detail_pid", "address_label", "number_first", "lot_number")
  for (field in fields) data.table::set(x, j = paste0("matched_", field), value = x[[field]])
  x[matched == TRUE, `:=`(address_detail_pid = "PRIMARY", address_label = "OTHER",
    number_first = 99L, lot_number = "99")]
  actual <- gnaf_match_features(x)
  evidence <- setdiff(names(expected), names(x))
  expect_identical(actual[, ..evidence], expected[, ..evidence])
})
