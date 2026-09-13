test_that("Illawong unit typo never redirects to street number three", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table::data.table(
    address_detail_pid = c("A-WRONG-HOUSE", "B-PARENT", "C-WRONG-UNIT", "Z-CORRECT"),
    address_label = c("3 ILLAWONG STREET, CANNONVALE QLD 4802",
      "24 ILLAWONG STREET, CANNONVALE QLD 4802",
      "UNIT 2 24 ILLAWONG STREET, CANNONVALE QLD 4802",
      "UNIT 3 24 ILLAWONG STREET, CANNONVALE QLD 4802"),
    number_first = c(3L, 24L, 24L, 24L),
    flat_type = c(NA_character_, NA_character_, "UNIT", "UNIT"),
    flat_number = c(NA_character_, NA_character_, "2", "3"),
    street_name = "ILLAWONG", street_type = "STREET",
    locality_name = "CANNONVALE", state = "QLD", postcode = 4802L
  )
  suppressMessages(gnaf_add(con, rows))
  inputs <- paste(c("UNIT", "UNI", "UNTI", "UNITS", "FLTA", "APARTMNT",
    "MY BUILDING NAME", "BLOCK 7", "UNRECOGNISED PREFIX", ""),
    "3 24 ILLAWONG STREET, CANNONVALE QLD 4802")
  for (cache in c(FALSE, TRUE, TRUE)) {
    out <- gnaf_match(inputs, con, cache = cache, verbose = FALSE)
    expect_equal(out$address_detail_pid, rep("Z-CORRECT", length(inputs)))
    expect_equal(out$input_raw, inputs)
    expect_true(all(out$matched))
  }
  # Exercise geographic fallback as well as the usual postcode path.
  fallback <- gnaf_match(c(
    "MY BUILDING NAME 3 24 ILLAWONG STREET, CANNONVALE QLD",
    "MY BUILDING NAME 3 24 ILLAWONG STREET, CANNONVALE QLD 4803"
  ), con, cache = FALSE, verbose = FALSE, min_score = 80L)
  expect_equal(fallback$address_detail_pid, rep("Z-CORRECT", 2L))
  # Exercise fuzzy scoring and the larger-batch query, without exact-label hits.
  batch <- paste0("MY BUILDING NAME 3 24 ILLAWON STREET, CANNONVALE QLD 4802", strrep(" ", 0:105))
  parsed <- address_parse(batch)
  parsed[, input_standardised := gnafr:::.standardise_input(parsed)]
  result <- gnafr:::.match_postcode_duckdb(con, parsed, 1L, 80L,
    gnafr:::.default_match_weights(), TRUE)$matches
  expect_equal(nrow(result), length(batch))
  expect_true(all(result$address_detail_pid == "Z-CORRECT"))
  expect_true(all(result$total_score < 100L))
  out <- gnaf_match(batch[1L], con, cache = FALSE, verbose = FALSE, max_results = 4L)
  expect_equal(out$address_detail_pid[1L], "Z-CORRECT")
  expect_true(all(out$total_score < 100L))
  expect_false("A-WRONG-HOUSE" %in% out$address_detail_pid)
  # A previously cached false perfect match must not survive the algorithm
  # change. Use a fuzzy label so the exact-label shortcut cannot mask this.
  key <- parsed$input_standardised[1L]
  DBI::dbExecute(con, paste(
    "INSERT INTO gnaf_match_cache",
    "(input_standardised, address_detail_pid, total_score, algorithm_version)",
    "VALUES (?, 'A-WRONG-HOUSE', 100, ?)"
  ), params = list(key, gnafr:::.CACHE_ALGORITHM_VERSION - 1L))
  fresh <- gnaf_match(batch[1L], con, cache = TRUE, verbose = FALSE)
  expect_identical(fresh$address_detail_pid, "Z-CORRECT")
  expect_lt(fresh$total_score, 100L)
})
