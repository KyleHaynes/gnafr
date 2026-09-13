library(data.table)

fixture_rows <- function() {
  data.table(
    address_detail_pid = c("A", "A2", "LOT", "RANGE", "NSW", "QLD", "ALIAS"),
    address_label = c(
      "UNIT 2 LEVEL 3 10 SMITH STREET, ST LUCIA QLD 4067",
      "10 SMITH STREET, ST LUCIA QLD 4067",
      "LOT 7 KREIS ROAD, WESTBROOK QLD 4350",
      "10-20 RANGE ROAD, BRISBANE QLD 4000",
      "10 MAIN ROAD, SPRINGFIELD NSW 2000",
      "10 MAIN ROAD, SPRINGFIELD QLD 4000",
      "10 OLD ROAD, BRISBANE QLD 4000"
    ),
    address_site_name = c("SMITH CENTRE", rep(NA_character_, 6L)),
    number_first = c(10L, 10L, NA_integer_, 10L, 10L, 10L, 10L),
    number_last = c(NA_integer_, NA_integer_, NA_integer_, 20L,
                    NA_integer_, NA_integer_, NA_integer_),
    lot_number = c(NA, NA, "7", NA, NA, NA, NA),
    flat_type = c("UNIT", rep(NA_character_, 6L)),
    flat_number = c("2", rep(NA_character_, 6L)),
    level_type = c("LEVEL", rep(NA_character_, 6L)),
    level_number = c("3", rep(NA_character_, 6L)),
    street_name = c("SMITH", "SMITH", "KREIS", "RANGE", "MAIN", "MAIN", "OLD"),
    street_type = c("STREET", "STREET", rep("ROAD", 5L)),
    locality_name = c("ST LUCIA", "ST LUCIA", "WESTBROOK", "BRISBANE",
                      "SPRINGFIELD", "SPRINGFIELD", "BRISBANE"),
    state = c("QLD", "QLD", "QLD", "QLD", "NSW", "QLD", "QLD"),
    postcode = c(4067L, 4067L, 4350L, 4000L, 2000L, 4000L, 4000L),
    alias_type = c(rep(NA_character_, 6L), "STREET:SYN")
  )
}

new_fixture_connection <- function(path = ":memory:") {
  con <- gnaf_connect(path)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, fixture_rows()))
  con
}

make_parsed <- function(in_number_first = 25L,
                        in_number_last = NA_integer_,
                        in_flat_type = NA_character_,
                        in_flat_number = NA_character_,
                        in_level_type = NA_character_,
                        in_level_number = NA_character_,
                        in_lot_number = NA_character_,
                        in_building_name = NA_character_,
                        in_street_name = "SAINT JAMES",
                        in_street_type = "COURT",
                        in_street_suffix = NA_character_,
                        in_locality = "TAMBORINE MOUNTAIN",
                        in_state = "QLD",
                        in_postcode = 4272L) {
  data.table(
    in_number_first, in_number_last, in_number_suffix = NA_character_,
    in_flat_type, in_flat_number, in_level_type, in_level_number,
    in_lot_number, in_building_name, in_street_name, in_street_type,
    in_street_suffix, in_locality, in_state, in_postcode
  )
}

test_that("gnaf_match validates addresses and numeric controls", {
  expect_error(gnaf_match(123L, NULL), "character vector")
  expect_error(gnaf_match(character(), NULL), "non-empty")
  expect_error(gnaf_match("x", NULL, max_results = 0L), "positive integer")
  expect_error(gnaf_match("x", NULL, min_score = 101), "between 0 and 100")
})

test_that("standardised input includes flat, level, lot, and ranges", {
  p <- make_parsed(
    in_number_first = 10L, in_number_last = 20L,
    in_flat_type = "UNIT", in_flat_number = "2",
    in_level_type = "LEVEL", in_level_number = "3",
    in_lot_number = "7"
  )
  out <- gnafr:::.standardise_input(p)
  expect_match(out, "UNIT 2 LEVEL 3 LOT 7 10-20")
})

test_that("exact and fuzzy component paths return the expected wide fields", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)

  out <- gnaf_match(
    "Unit 2 Level 3 10 Smyth St, St Lucia QLD 4067",
    con, cache = FALSE, verbose = FALSE
  )
  expect_equal(out$address_detail_pid, "A")
  expect_equal(out$address_site_name, "SMITH CENTRE")
  expect_equal(out$level_number, "3")
  expect_equal(out$score_flat, 5L)
})

test_that("range and explicit lot blocking use the number score", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)

  out <- gnaf_match(c(
    "15 Range Rd, Brisbane QLD 4000",
    "Lot 7 Kreis Rd, Westbrook QLD 4350"
  ), con, cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid, c("RANGE", "LOT"))
  expect_equal(out$score_number, c(7L, 10L))
})

test_that("matching signatures are deduplicated then fanned out", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  x <- rep("15 Range Rd, Brisbane QLD 4000", 100L)
  out <- gnaf_match(x, con, cache = FALSE, verbose = FALSE)
  expect_equal(out$input_id, seq_along(x))
  expect_true(all(out$address_detail_pid == "RANGE"))
})

test_that("locality candidates retain state and aliases remain filterable", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)

  state_match <- gnaf_match(
    "10 Main Rd, Springfield NSW", con, cache = FALSE, verbose = FALSE
  )
  expect_equal(state_match$address_detail_pid, "NSW")

  alias_match <- gnaf_match(
    "10 Old Rd, Brisbane QLD 4000", con,
    alias_types = "STREET:SYN", cache = FALSE, verbose = FALSE
  )
  expect_equal(alias_match$address_detail_pid, "ALIAS")

  default_alias_match <- gnaf_match(
    "10 Old Rd, Brisbane QLD 4000", con,
    cache = FALSE, verbose = FALSE
  )
  expect_equal(default_alias_match$address_detail_pid, "ALIAS")
})

test_that("max_results greater than one bypasses the one-result cache", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  out <- gnaf_match(
    "10 Smith St, St Lucia QLD 4067", con,
    max_results = 2L, cache = TRUE, verbose = FALSE
  )
  expect_equal(nrow(out), 2L)
  expect_setequal(out$address_detail_pid, c("A", "A2"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM gnaf_match_cache")$n, 0)
})

test_that("legacy cache rows are ignored and replaced with the current version", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  input <- "15 Range Rd, Brisbane QLD 4000"
  key <- gnafr:::.standardise_input(address_parse(input))
  DBI::dbExecute(con, sprintf(
    "INSERT INTO gnaf_match_cache
       (input_standardised, address_detail_pid, total_score, algorithm_version)
     VALUES ('%s', 'A2', 80, 1)", key
  ))

  out <- gnaf_match(input, con, min_score = 90L, verbose = FALSE)
  expect_equal(out$address_detail_pid, "RANGE")
  cached <- DBI::dbGetQuery(con, "
    SELECT algorithm_version, total_score FROM gnaf_match_cache
  ")
  expect_equal(cached$algorithm_version, gnafr:::.CACHE_ALGORITHM_VERSION)
  expect_gte(cached$total_score, 90L)
})

test_that("address mutations invalidate cached matches", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  invisible(gnaf_match(
    "15 Range Rd, Brisbane QLD 4000", con, verbose = FALSE
  ))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM gnaf_match_cache")$n, 0)

  extra <- fixture_rows()[1L]
  extra[, `:=`(
    address_detail_pid = "NEW", address_label = "1 NEW ROAD, BRISBANE QLD 4000",
    number_first = 1L, flat_type = NA_character_, flat_number = NA_character_,
    level_type = NA_character_, level_number = NA_character_, street_name = "NEW",
    street_type = "ROAD", locality_name = "BRISBANE", postcode = 4000L
  )]
  suppressMessages(gnaf_add(con, extra))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM gnaf_match_cache")$n, 0)
})

test_that("read-only connections silently skip cache writes", {
  path <- tempfile(fileext = ".duckdb")
  con <- new_fixture_connection(path)
  gnaf_disconnect(con)
  on.exit(unlink(path), add = TRUE)

  read_only <- gnaf_connect(path, read_only = TRUE)
  on.exit(gnaf_disconnect(read_only), add = TRUE)
  expect_no_error(gnaf_match(
    "15 Range Rd, Brisbane QLD 4000", read_only,
    cache = TRUE, verbose = FALSE
  ))
})

test_that("con-first compatibility order warns for one cycle", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_warning(
    out <- gnaf_match(con, "15 Range Rd, Brisbane QLD 4000",
                      cache = FALSE, verbose = FALSE),
    "deprecated"
  )
  expect_equal(out$address_detail_pid, "RANGE")
})

test_that("exact labels still return requested alternative matches", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  out <- gnaf_match("10 SMITH STREET, ST LUCIA QLD 4067", con,
                    max_results = 2L, cache = FALSE, verbose = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$address_detail_pid, c("A2", "A"))
})

test_that("custom-excluded matches do not populate the shared default cache", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses SELECT * FROM custom_addresses WHERE address_detail_pid = 'A'")
  DBI::dbExecute(con, "UPDATE gnaf_addresses SET source = 'gnaf'")
  input <- "10 Smith St, St Lucia QLD 4067"
  out <- gnaf_match(input, con, include_custom = FALSE,
                    cache_threshold = 0, verbose = FALSE)
  expect_equal(out$address_detail_pid, "A")
  expect_equal(gnaf_cache_status(con)$rows, 0)
  cached <- gnaf_match(input, con, verbose = FALSE)
  uncached <- gnaf_match(input, con, cache = FALSE, verbose = FALSE)
  expect_equal(cached$address_detail_pid, uncached$address_detail_pid)
  expect_equal(cached$address_detail_pid, "A2")
})

test_that("street-only fallback respects alias exclusions", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, street_name, street_type, locality_name, state, postcode, source, alias_type)
    VALUES ('SO', 'SMITH', 'ROAD', 'BRISBANE', 'QLD', 4000, 'gnaf', 'street_only')")
  included <- gnaf_match("Smith Rd, Brisbane QLD 4000", con,
    street_only_fallback = TRUE, cache = FALSE, verbose = FALSE)
  excluded <- gnaf_match("Smith Rd, Brisbane QLD 4000", con,
    include_aliases = FALSE, street_only_fallback = TRUE, cache = FALSE, verbose = FALSE)
  expect_true(included$matched)
  expect_false(excluded$matched)
})

test_that("input ranges retrieve interior numbers in postcode, state and locality paths", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  rows <- fixture_rows()[4L][rep(1L, 3L)]
  rows[, `:=`(address_detail_pid = c("INTERIOR", "OVERLAP", "OUTSIDE"),
              address_label = c("12 RANGE ROAD, BRISBANE QLD 4000",
                                "18-25 RANGE ROAD, BRISBANE QLD 4000",
                                "30 RANGE ROAD, BRISBANE QLD 4000"),
              number_first = c(12L, 18L, 30L), number_last = c(NA_integer_, 25L, NA_integer_))]
  suppressMessages(gnaf_add(con, rows))
  out <- gnaf_match(c("10-20 Range Rd, Brisbane QLD 4000",
                      "10-20 Range Rd, Brisbane QLD",
                      "10-20 Range Rd, Brisbane QLD 4999"),
                    con, max_results = 5L, cache = FALSE, verbose = FALSE)
  for (id in 1:3) {
    matched <- out[input_id == id]
    # "QLD" (a same-postcode fixture row with an unrelated street and
    # suburb - MAIN/SPRINGFIELD vs the input's RANGE/BRISBANE) used to
    # squeak past min_score as a spare-slot filler; the reshaped
    # score_street_name/score_suburb (see .component_similarity_factor())
    # now correctly keeps a merely-coincidental postcode match out.
    expected_pids <- c("RANGE", "INTERIOR", "OVERLAP")
    expected_numbers <- c(10L, 5L, 3L)
    expect_equal(matched$address_detail_pid, expected_pids)
    expect_equal(matched$score_number, expected_numbers)
  }
})

test_that("direction and suffixed unit numbers determine the winning candidate", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table(
    address_detail_pid = c("A_WRONG_SUFFIX", "B_WRONG_DIRECTION", "Z_CORRECT"),
    address_label = c("UNIT 2 10B MAIN ROAD NORTH, BRISBANE QLD 4000",
                      "UNIT 2 10A MAIN ROAD SOUTH, BRISBANE QLD 4000",
                      "UNIT 2 10A MAIN ROAD NORTH, BRISBANE QLD 4000"),
    number_first = 10L, flat_type = "UNIT", flat_number = "2",
    street_name = "MAIN", street_type = "ROAD", street_suffix = c("N", "S", "N"),
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L
  )
  suppressMessages(gnaf_add(con, rows))
  out <- gnaf_match(c("Unit 2 10A Main Rd North, Brisbane QLD 4000",
                      rows$address_label[3L], "Unit 2 10A Main Rd N, Brisbane QLD 4000"),
                    con, cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid, rep("Z_CORRECT", 3L))
  expect_equal(out$total_score, rep(100L, 3L))

  DBI::dbExecute(con, "UPDATE custom_addresses SET number_first = NULL WHERE address_detail_pid = 'Z_CORRECT'")
  out <- gnaf_match("Unit 2 10A Main Rd North, Brisbane QLD", con,
                    cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid, "Z_CORRECT")
  expect_equal(out$score_number, 10L)
})

test_that("matching units retain credit when level information is incomplete", {
  con <- new_fixture_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  rows <- fixture_rows()[1L][rep(1L, 3L)]
  rows[, `:=`(address_detail_pid = c("Z_MISSING_LEVEL", "B_WRONG_LEVEL", "C_WRONG_UNIT"),
              address_label = c("UNIT 2 10 SMITH STREET, ST LUCIA QLD 4067",
                                "UNIT 2 LEVEL 4 10 SMITH STREET, ST LUCIA QLD 4067",
                                "UNIT 9 LEVEL 3 10 SMITH STREET, ST LUCIA QLD 4067"),
              flat_number = c("2", "2", "9"), level_number = c(NA, "4", "3"))]
  suppressMessages(gnaf_add(con, rows))
  out <- gnaf_match("Apartment 2 Level 3 10 Smith St, St Lucia QLD 4067",
                    con, max_results = 5L, cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid,
               c("A", "Z_MISSING_LEVEL", "B_WRONG_LEVEL", "A2", "C_WRONG_UNIT"))
  expect_equal(out$score_flat, c(5L, 4L, 3L, 2L, 2L))
})

test_that("locality fallback respects custom component weights", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table(address_detail_pid = c("WRONG", "CORRECT"),
                     number_first = 10L, street_name = "MAIN", street_type = "ROAD",
                     locality_name = c("BUNDABERG", "BRISBANE"), state = "QLD",
                     postcode = c(4000L, 4001L))
  suppressMessages(gnaf_add(con, rows))
  out <- gnaf_match("10 Main Rd, Brisbane QLD 4000", con,
                    weights = list(postcode = 20, suburb = 40, street_name = 15,
                                   street_type = 10, number = 10, flat = 5),
                    cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid, "CORRECT")
  expect_equal(out$total_score, 94L)
})
