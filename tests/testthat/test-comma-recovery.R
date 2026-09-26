test_that("a compass word before ST belongs to the street name", {
  inputs <- c(
    "Riverside Caravan Park Site 72 14-20 Little West St Winston Qld 4825",
    "Riverside Caravan Park Site 72 14-20 Little West St, Winston Qld 4825",
    "Riverside Caravan Park, Site 72, 14-20 Little West St, Winston, Qld 4825",
    "10 Little East St Brisbane QLD 4000",
    "10 Little North St Brisbane QLD 4000",
    "10 Little South St Brisbane QLD 4000",
    "10 Main Rd St Lucia QLD 4067",
    "10 St James St St Lucia QLD 4067"
  )
  parsed <- address_parse(inputs)
  expect_identical(parsed$in_street_name, c(rep("LITTLE WEST", 3L),
    "LITTLE EAST", "LITTLE NORTH", "LITTLE SOUTH", "MAIN", "SAINT JAMES"))
  expect_identical(parsed$in_street_type, c(rep("STREET", 6L), "ROAD", "STREET"))
  expect_identical(parsed$in_locality, c(rep("WINSTON", 3L), rep("BRISBANE", 3L),
    "ST LUCIA", "ST LUCIA"))
  expect_identical(parsed$in_flat_number[1:3], rep("72", 3L))
  expect_identical(parsed$in_number_first[1:3], rep(14L, 3L))
  expect_identical(parsed$in_number_last[1:3], rep(20L, 3L))
  expect_identical(parsed$input_raw, inputs)
})

test_that("locality recovery accepts unique transpositions and preserves their spelling", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbWriteTable(con, "gnaf_locality_index", data.frame(
    locality_name = c("BURLEIGH HEADS", "SURFERS PARADISE", "BURLEIGH HEADS",
                      "UBRLEGIH HEADS", "BURLEIGH HEADS"),
    postcode = c(4220L, 4217L, 4001L, 4001L, 4002L),
    state = c("QLD", "QLD", "QLD", "QLD", "NSW")
  ), append = TRUE)
  inputs <- c(
    "UNIT 22 100 THE ESPLANADE UBRLEIGH HEADS QLD 4220",
    "UNIT 22 100 THE ESPLANADE, UBRLEIGH HEADS QLD 4220",
    "UNIT 22, 100 THE ESPLANADE UBRLEIGH HEADS, QLD 4220",
    "NEPEAN LODGE UNIT 7 11 OLD BURLEIGH RD SUFRERS PARADISE QLD 4217",
    "NEPEAN LODGE UNIT 7 11 OLD BURLEIGH RD, SUFRERS PARADISE, QLD 4217",
    "UNIT 22 100 THE ESPLANADE UBRLEIGH HEADS QLD 4001",
    "UNIT 22 100 THE ESPLANADE UBRLEIGH HEADS QLD 4002",
    "UNIT 22 100 THE ESPLANADE UBRLEIGH HEADS QLD 4999",
    "UNIT 22 100 THE ESPLANADE UBRLEIGH HEADS QLD"
  )
  parsed <- address_parse(inputs)
  before <- data.table::copy(parsed)
  gnafr:::.recover_missing_locality(con, parsed)
  expect_identical(parsed$in_street_name[1:5],
    c(rep("THE ESPLANADE", 3L), rep("OLD BURLEIGH", 2L)))
  expect_identical(parsed$in_street_type[1:5], c(rep(NA_character_, 3L), rep("ROAD", 2L)))
  expect_identical(parsed$in_locality[1:5], c(rep("UBRLEIGH HEADS", 3L), rep("SUFRERS PARADISE", 2L)))
  expect_identical(parsed[6:9], before[6:9])
  expect_identical(parsed$in_flat_number, before$in_flat_number)
  expect_identical(parsed$in_number_first, before$in_number_first)
  expect_identical(parsed$input_raw, inputs)
  expect_false("__gnafr_locrecover__" %in% DBI::dbListTables(con))
})

test_that("comma recovery improves component scores without repairing house numbers", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  reference <- data.table::data.table(
    address_detail_pid = paste0("CASE", 1:6),
    address_label = c(
      "UNIT 22 100 THE ESPLANADE, BURLEIGH HEADS QLD 4220",
      "AQUARIUS ON THE BEACH UNIT 406 75-77 THE STRAND, NORTH WARD QLD 4810",
      "RIVERSIDE CARAVAN PARK SITE 72 14-20 LITTLE WEST STREET, WINSTON QLD 4825",
      "NEPEAN LODGE UNIT 7 11 OLD BURLEIGH ROAD, SURFERS PARADISE QLD 4217",
      "TALL TREES UNIT 14 3745-3759 PACIFIC HIGHWAY, SLACKS CREEK QLD 4127",
      "67 COONOWRIN ROAD, GLASS HOUSE MOUNTAINS QLD 4518"
    ),
    street_name = c("THE ESPLANADE", "THE STRAND", "LITTLE WEST", "OLD BURLEIGH", "PACIFIC", "COONOWRIN"),
    street_type = c(NA, NA, "STREET", "ROAD", "HIGHWAY", "ROAD"),
    locality_name = c("BURLEIGH HEADS", "NORTH WARD", "WINSTON", "SURFERS PARADISE", "SLACKS CREEK", "GLASS HOUSE MOUNTAINS"),
    postcode = c(4220L, 4810L, 4825L, 4217L, 4127L, 4518L), state = "QLD",
    number_first = c(100L, 75L, 14L, 11L, 3745L, 67L),
    number_last = c(NA, 77L, 20L, NA, 3759L, NA),
    flat_number = c("22", "406", "72", "7", "14", NA),
    flat_type = c("UNIT", "UNIT", "SITE", "UNIT", "UNIT", NA)
  )
  suppressMessages(gnaf_add(con, reference))
  inputs <- c(
    "UNIT 22 100 THE ESPLANADE UBRLEIGH HEADS QLD 4220",
    "AQUARIUS ON THE BEACH UNIT 406 7-77 THE STRA, NORTH WARD QLD 4810",
    "Riverside Caravan Park Site 72 14-20 Little West St Winston Qld 4825",
    "NEPEAN LODGE UNIT 7 11 OLD BURLEIGH RD SUFRERS PARADISE QLD 4217",
    "Tall Trees Unit 14 3745-379 Pacific Hwy, Slacks Creek Qld 4127",
    "517 Coonowrin Rd, Glass House Mountains Qld 4518"
  )
  # Explicit weights reproduce the report independently of future defaults.
  weights <- list(postcode = 12L, suburb = 12L, street_name = 16L,
                  street_type = 10L, number = 30L, flat = 20L)
  out <- gnaf_match(inputs, con, weights = weights, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, reference$address_detail_pid)
  expect_identical(out$input_raw, inputs)
  expect_identical(out$in_number_first, c(100L, 7L, 14L, 11L, 3745L, 517L))
  expect_identical(out$in_number_last, c(NA_integer_, 77L, 20L, NA_integer_, 379L, NA_integer_))
  # Number credit is scaled by street agreement, so the truncated "THE STRA" keeps
  # only part of the credit its contained 7-77 range would otherwise earn.
  expect_identical(out$score_number, c(30L, 11L, 30L, 30L, 0L, 0L))
  expect_identical(out$total_score[c(2L, 3L, 5L, 6L)], c(67L, 100L, 70L, 70L))
  expect_true(all(out$total_score[c(1L, 4L)] > 90L & out$total_score[c(1L, 4L)] < 100L))
  expect_true(all(out$score_suburb[c(1L, 4L)] < weights$suburb))
  expect_identical(out$in_locality[c(1L, 4L)], c("UBRLEIGH HEADS", "SUFRERS PARADISE"))
})
