test_that("number token extraction preserves prefixes, boundaries and unusual streets", {
  pairs <- data.table::data.table(
    address_label = c(
      "10 MAIN ROAD", "UNIT 2/10A MAIN ROAD", "LEVEL 3 UNIT 2 10A-12B MAIN ROAD",
      "10   MAIN ROAD", "THE MAIN BUILDING 12B MAIN ROAD",
      "10 MAINLY HOUSE 12 MAIN ROAD", "10 MAIN ANNEX 12 MAIN ROAD",
      "10 A+B ROAD", "10 (OLD) MAIN ROAD", "3 3RD AVENUE",
      "10 MAIN, BRISBANE", "10 MAIN", "10 MAIN\tROAD", "10\tMAIN ROAD",
      "10 MAIN ROAD", NA, "NO NUMBER MAIN ROAD", "10  ", "10 MAIN ROAD",
      "10 MAIN ROAD\n", "10A-12B MAIN ROAD", "unit 2 10a main road"
    ),
    street_name = c(
      rep("MAIN", 7L), "A+B", "(OLD) MAIN", "3RD", rep("MAIN", 7L),
      "", NA, "MAIN", "MAIN", " main "
    )
  )
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  duckdb::duckdb_register(con, "number_tokens", pairs)
  # Keep the original expression as an oracle, including its NULL and empty
  # street behaviour. An early occurrence of a street in a building name can
  # require searching later in the label.
  original <- paste0(
    "REGEXP_EXTRACT(UPPER(g.address_label), ",
    "'(^|[ /])([0-9]+[A-Z]?(-[0-9]+[A-Z]?)?) +' || ",
    "REGEXP_ESCAPE(UPPER(TRIM(g.street_name))) || '( |,|$)', 2)"
  )
  out <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS actual, %s AS expected FROM number_tokens g",
    .candidate_number_token_sql(), original
  ))
  expect_identical(out$actual, out$expected)
  expect_identical(out$actual[1:12],
    c("10", "10A", "10A-12B", "10", "12B", "12", "10", "10", "10", "3", "10", "10"))
})

test_that("split number joins retain the same candidates and component scores", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table::data.table(
    address_detail_pid = sprintf("P%02d", 1:12),
    address_label = c(
      "10 MAIN ROAD", "10-20 MAIN ROAD", "12 MAIN ROAD", "18-25 MAIN ROAD",
      "30 MAIN ROAD", "UNIT 2 10A MAIN ROAD", "LOT 7 MAIN ROAD", "MAIN ROAD",
      "20-10 MAIN ROAD", "10-10 MAIN ROAD", "0 MAIN ROAD", "10 OTHER ROAD"
    ),
    number_first = c(10L, 10L, 12L, 18L, 30L, NA, NA, NA, 20L, 10L, 0L, 10L),
    number_last = c(NA, 20L, NA, 25L, NA, NA, NA, NA, 10L, 10L, NA, NA),
    lot_number = c(rep(NA_character_, 6L), "7", rep(NA_character_, 5L)),
    street_name = c(rep("MAIN", 11L), "OTHER"), street_type = "ROAD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L,
    alias_type = c(rep(NA_character_, 10L), "STREET:SYN", "LOCALITY:SYN")
  )
  suppressMessages(gnaf_add(con, rows))
  inputs <- address_parse(c(
    "10 Main Road, Brisbane QLD 4000", "10-20 Main Road, Brisbane QLD 4000",
    "15 Main Road, Brisbane QLD 4000", "20-10 Main Road, Brisbane QLD 4000",
    "10-10 Main Road, Brisbane QLD 4000", "10A Main Road, Brisbane QLD 4000",
    "Lot 7 Main Road, Brisbane QLD 4000", "Main Road, Brisbane QLD 4000",
    "0 Main Road, Brisbane QLD 4000", "10 Main Road, Brisbane QLD"
  ))
  duckdb::duckdb_register(con, "branch_inputs", inputs)
  for (join in c("g.postcode = i.in_postcode", "g.state = i.in_state")) {
    for (alias_types in list(NULL, NA_character_, "STREET:SYN")) {
      alias_sql <- .alias_type_sql(alias_types)
      filter <- if (is.null(alias_sql)) "TRUE" else alias_sql
      for (weights in list(.default_match_weights(),
        list(postcode = 20.5, suburb = 15, street_name = 0,
             street_type = 10, number = 49.5, flat = 5))) {
        expected <- .run_duckdb_score_query(
          con, "branch_inputs", "custom_addresses", join,
          paste(filter, "AND", .number_prefilter_sql()), weights, 20L, 0L
        )
        actual <- .run_duckdb_score_query(
          con, "branch_inputs", "custom_addresses", join, filter,
          weights, 20L, 0L, split_number = TRUE
        )
        for (part in c("matches", "diagnostics")) {
          if (!is.null(actual[[part]])) data.table::setorderv(actual[[part]], names(actual[[part]]))
          if (!is.null(expected[[part]])) data.table::setorderv(expected[[part]], names(expected[[part]]))
          expect_equal(actual[[part]], expected[[part]])
        }
      }
    }
  }
})

test_that("bulk locality fallback preserves numbered, range, lot and suffix matches", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table::data.table(
    address_detail_pid = c("NUMBER", "RANGE", "LOT", "SUFFIX"),
    address_label = c("10 MAIN ROAD", "10-20 RANGE ROAD", "LOT 7 LOT ROAD", "10A SUFFIX ROAD"),
    number_first = c(10L, 10L, NA, NA), number_last = c(NA, 20L, NA, NA),
    lot_number = c(NA, NA, "7", NA),
    street_name = c("MAIN", "RANGE", "LOT", "SUFFIX"), street_type = "ROAD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L
  )
  suppressMessages(gnaf_add(con, rows))
  inputs <- address_parse(rep(c(
    "10 Main Road, Brisbane QLD 4001", "15 Range Road, Brisbane QLD 4001",
    "Lot 7 Lot Road, Brisbane QLD 4001", "10A Suffix Road, Brisbane QLD 4001"
  ), 26L))
  before <- data.table::copy(inputs)
  out <- .match_locality_duckdb(con, inputs, 1L, 80L,
                              .default_match_weights(), TRUE)$matches
  data.table::setorder(out, input_id)
  expect_identical(out$address_detail_pid, rep(rows$address_detail_pid, 26L))
  expect_identical(out$score_number, rep(c(10L, 7L, 10L, 10L), 26L))
  expect_identical(inputs, before)
  expect_false(any(grepl("^__gnafr_", DBI::dbListTables(con))))
})

test_that("shared fuzzy localities expand to every input without mixing states", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = c("A_QLD", "B_NSW"), address_label = "10 MAIN ROAD",
    number_first = 10L, street_name = "MAIN", street_type = "ROAD",
    locality_name = "BRISBANE", state = c("QLD", "NSW"), postcode = c(4000L, 2000L)
  )))
  inputs <- address_parse(rep("10 Main Road, Brisbanx QLD 4999", 6L))
  inputs[, in_state := rep(c("QLD", "NSW", NA_character_), 2L)]
  before <- data.table::copy(inputs)
  out <- .match_locality_duckdb(con, inputs, 2L, 60L,
                              .default_match_weights(), TRUE)$matches
  data.table::setorder(out, input_id, address_detail_pid)
  expect_identical(out$input_id, c(1L, 2L, 3L, 3L, 4L, 5L, 6L, 6L))
  expect_identical(out$address_detail_pid,
                   rep(c("A_QLD", "B_NSW", "A_QLD", "B_NSW"), 2L))
  expect_identical(inputs, before)
})

test_that("postcode score pruning retains every candidate reaching the threshold", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table::CJ(postcode = c(4000L, 4001L, 4002L, 4003L, 4076L, 4999L),
                          street_name = c("MAIN", "MAINE", "OTHER", "WILLIAM",
                            "MOUNT GRAVATT EAST", "MOUNT GRAVATT WEST"))
  rows[, `:=`(address_detail_pid = paste0("P", .I),
               address_label = paste("10", street_name, "ROAD"),
               number_first = 10L, street_type = "ROAD", locality_name = "BRISBANE", state = "QLD")]
  suppressMessages(gnaf_add(con, rows))
  inputs <- address_parse(c("10 Main Road, Brisbane QLD 4000", "10 Main Road, Brisbane QLD",
    "10 Xilliam Road, Brisbane QLD 4000", "10 Mount Gravatt East Road, Brisbane QLD 4067"))
  duckdb::duckdb_register(con, "bound_inputs", inputs)
  for (weights in list(.default_match_weights(),
    list(postcode = 20.5, suburb = 15.5, street_name = 39.5, street_type = 9.5, number = 10, flat = 5),
    list(postcode = 50, suburb = 20, street_name = 0, street_type = 10, number = 15, flat = 5))) {
    all <- .run_duckdb_score_query(con, "bound_inputs", "custom_addresses",
      "g.state = i.in_state", .number_prefilter_sql(), weights, 100L, 0L)$matches
    for (threshold in c(60L, 80L, 86L, 95L)) {
      actual <- .run_duckdb_score_query(con, "bound_inputs", "custom_addresses",
        "g.state = i.in_state", .number_prefilter_sql(), weights, 100L, threshold)$matches
      expected <- all[total_score >= threshold]
      data.table::setorder(expected, input_id, match_rank)
      if (nrow(expected) == 0L) {
        expect_null(actual)
      } else {
        data.table::setorder(actual, input_id, match_rank)
        expect_equal(actual, expected)
      }
    }
  }
})

test_that("bulk ranking uses edit evidence and retains zero-padded lot candidates", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- data.table::data.table(
    address_detail_pid = c("Z_CORRECT", "A_PREFIX", "LOT"),
    address_label = c("10 WILLIAM ROAD", "10 XILLIAMSON ROAD", "LOT 7 MAIN ROAD"),
    number_first = c(10L, 10L, NA_integer_),
    lot_number = c(NA_character_, NA_character_, "7"),
    street_name = c("WILLIAM", "XILLIAMSON", "MAIN"), street_type = "ROAD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L)
  suppressMessages(gnaf_add(con, rows))
  parsed <- address_parse(c("10 Xilliam Road, Brisbane QLD 4000",
                             "Lot 007 Main Road, Brisbane QLD 4000"))
  duckdb::duckdb_register(con, "metric_inputs", parsed)
  for (split in c(FALSE, TRUE)) {
    out <- .run_duckdb_score_query(con, "metric_inputs", "custom_addresses",
      "g.postcode = i.in_postcode",
      if (split) "TRUE" else .number_prefilter_sql(),
      .default_match_weights(), 1L, 86L, split_number = split)$matches
    data.table::setorder(out, input_id)
    expect_identical(out$address_detail_pid, c("Z_CORRECT", "LOT"))
    expect_identical(out$score_number, c(10L, 10L))
  }
})
