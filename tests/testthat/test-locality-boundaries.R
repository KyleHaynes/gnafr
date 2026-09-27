test_that("RIVER belongs to the locality when an earlier street type is present", {
  out <- address_parse(c(
    "15 watermans way river heads qld 4655",
    "UNIT 2 15 WATERMANS WAY RIVER HEADS QLD 4655",
    "10 SMITH ROAD LITTLE RIVER VIC 3211",
    "10 SMITH RIVER, BRISBANE QLD 4000",
    "10 SMITH RIVER BRISBANE QLD 4000"
  ))
  expect_identical(out$in_street_name, c("WATERMANS", "WATERMANS", "SMITH", "SMITH", "SMITH"))
  expect_identical(out$in_street_type, c("WAY", "WAY", "ROAD", "RIVER", "RIVER"))
  expect_identical(out$in_locality, c("RIVER HEADS", "RIVER HEADS", "LITTLE RIVER", "BRISBANE", "BRISBANE"))
  expect_identical(out$in_number_first, c(15L, 15L, 10L, 10L, 10L))
  expect_identical(out$in_flat_number[2L], "2")
})

test_that("reported matches recover boundaries without erasing spelling differences", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = c("SAMS", "ESPLANADE", "WATERMANS"),
    address_label = c("1 SAMS WAY, MARSDEN QLD 4132",
                      "89 THE ESPLANADE, ST LUCIA QLD 4067",
                      "15 WATERMANS WAY, RIVER HEADS QLD 4655"),
    number_first = c(1L, 89L, 15L),
    street_name = c("SAMS", "THE ESPLANADE", "WATERMANS"),
    street_type = c("WAY", NA_character_, "WAY"),
    locality_name = c("MARSDEN", "ST LUCIA", "RIVER HEADS"),
    state = "QLD", postcode = c(4132L, 4067L, 4655L)
  )))
  inputs <- c("1 ams way, marsden qld 4132",
              "89 THE ESPLANADE S LUCIA QLD 4067",
              "15 watermans way river heads qld 4655")
  # These historical score expectations use the weights from that report.
  weights <- list(postcode = 20L, suburb = 15L, street_name = 40L,
                  street_type = 10L, number = 10L, flat = 5L)
  out <- gnaf_match(inputs, con, weights = weights, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, c("SAMS", "ESPLANADE", "WATERMANS"))
  expect_identical(out$input_raw, inputs)
  expect_identical(out$in_locality, c("MARSDEN", "S LUCIA", "RIVER HEADS"))
  expect_identical(out$in_street_name, c("AMS", "THE ESPLANADE", "WATERMANS"))
  expect_identical(out$in_street_type, c("WAY", NA_character_, "WAY"))
  # "AMS" for "SAMS" is a street typo, so number and flat credit are scaled down.
  expect_identical(out$total_score[c(1L, 3L)], c(78L, 100L))
  expect_gt(out$total_score[2L], 90L)
  expect_lt(out$total_score[2L], 100L)
  expect_lt(out$score_suburb[2L], out$score_suburb[3L])
  expect_identical(out$score_street_name[2L], 40L)

  # Reference-assisted recovery is optional and must respect the existing flag.
  disabled <- gnaf_match(inputs[2L], con, locality_fallback = FALSE,
                         weights = weights, cache = FALSE, verbose = FALSE)
  expect_true(is.na(disabled$in_locality))
})

test_that("fuzzy boundary recovery requires unique postcode and state evidence", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbWriteTable(con, "gnaf_locality_index", data.frame(
    locality_name = c("ST LUCIA", "ST LUCIA", "SO LUCIA",
                      "ST LUCIA", "S LUCIA", "ST LUCIA"),
    postcode = c(4067L, 4001L, 4001L, 4002L, 4003L, 4003L),
    state = c("QLD", "QLD", "QLD", "NSW", "QLD", "QLD")
  ), append = TRUE)
  parsed <- data.table::data.table(
    input_id = 1:8,
    in_postcode = c(4067L, 4001L, 4002L, 4999L, NA_integer_, 4067L, 4003L, 4067L),
    in_state = "QLD", in_locality = c(rep(NA_character_, 5L), "ALREADY SET", NA, NA),
    in_street_name = c(rep("THE ESPLANADE S LUCIA", 7L), "THE S LUCIA"),
    in_street_type = NA_character_, in_street_suffix = NA_character_
  )
  before <- data.table::copy(parsed)
  gnafr:::.recover_missing_locality(con, parsed)
  expect_identical(parsed$in_locality[c(1L, 7L)], c("S LUCIA", "S LUCIA"))
  expect_identical(parsed$in_street_name[c(1L, 7L)], rep("THE ESPLANADE", 2L))
  # Ambiguous matches, wrong state/postcode, missing postcode, an existing
  # locality, and an empty street after THE must not be reinterpreted.
  expect_identical(parsed[c(2:6, 8L)], before[c(2:6, 8L)])
  expect_false("__gnafr_locrecover__" %in% DBI::dbListTables(con))
})
