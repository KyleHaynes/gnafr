postcode_identity_rows <- function() {
  data.table::data.table(
    address_detail_pid = c("MUSGRAVE190", "MUSGRAVE14", "PETRIE190"),
    address_label = c("190 MUSGRAVE ROAD, RED HILL QLD 4059",
      "14 MUSGRAVE ROAD, RED HILL QLD 4000", "190 PETRIE TERRACE, RED HILL QLD 4000"),
    number_first = c(190L, 14L, 190L), street_name = c("MUSGRAVE", "MUSGRAVE", "PETRIE"),
    street_type = c("ROAD", "ROAD", "TERRACE"), locality_name = "RED HILL",
    state = "QLD", postcode = c(4059L, 4000L, 4000L)
  )
}

postcode_identity_connection <- function(rows = postcode_identity_rows()) {
  con <- gnaf_connect(":memory:")
  gnaf_init(con)
  suppressMessages(gnaf_add(con, rows))
  con
}

test_that("a unique otherwise exact address outranks postcode agreement under both weights", {
  con <- postcode_identity_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  inputs <- c("190 MUSGRAVE RD, RED HILL QLD 4059", "190 MUSGRAVE RD, RED HILL QLD 4000")
  weights <- list(
    list(postcode = 20, suburb = 15, street_name = 40, street_type = 10, number = 10, flat = 5),
    list(postcode = 12, suburb = 12, street_name = 16, street_type = 10, number = 30, flat = 20)
  )
  for (w in weights) {
    out <- gnaf_match(inputs, con, weights = w, fallback_threshold = 0L,
                      cache = FALSE, verbose = FALSE)
    expect_equal(out$address_detail_pid, rep("MUSGRAVE190", 2L))
    expect_equal(out$match_basis, c("exact_components", "postcode_only"))
    expect_equal(out$total_score, c(100L, 100L - w$postcode))
    expect_equal(out$score_postcode, c(w$postcode, 0L))
    many <- gnaf_match(inputs, con, weights = w, max_results = 3L, min_score = 0L,
                      cache = FALSE, verbose = FALSE)
    expect_equal(many[match_rank == 1L]$address_detail_pid, out$address_detail_pid)
    expect_equal(many[input_id == 2L & address_detail_pid == "MUSGRAVE190"]$match_rank, 1L)
    expect_equal(anyDuplicated(many, by = c("input_id", "address_detail_pid")), 0L)
    evidence <- gnaf_match_features(many)
    expect_equal(evidence[input_id == 2L & match_rank == 1L]$score_gap,
                 if (w$postcode == 20) -10 else 9)
  }
  disabled <- gnaf_match(inputs[2L], con, locality_fallback = FALSE,
                         weights = weights[[1L]], cache = FALSE, verbose = FALSE)
  expect_equal(disabled$address_detail_pid, "MUSGRAVE14")
  expect_equal(disabled$match_basis, "weighted")
  excluded <- gnaf_match(inputs[2L], con, include_custom = FALSE, cache = FALSE, verbose = FALSE)
  expect_false(excluded$matched)
  expect_identical(excluded$match_basis, NA_character_)
})

test_that("postcode preference respects score cutoffs and does not inflate scores", {
  con <- postcode_identity_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  input <- "190 MUSGRAVE RD, RED HILL QLD 4000"
  w <- list(postcode = 80, suburb = 4, street_name = 4, street_type = 4, number = 4, flat = 4)
  out <- gnaf_match(input, con, weights = w, min_score = 0L, cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid, "MUSGRAVE190")
  expect_equal(out$total_score, 20L)
  filtered <- gnaf_match(input, con, weights = w, min_score = 21L, max_results = 5L,
                         cache = FALSE, verbose = FALSE)
  expect_false("MUSGRAVE190" %in% filtered$address_detail_pid)
  expect_true(all(filtered$total_score >= 21L))
})

test_that("uniqueness includes alternatives hidden by scores and top-N limits", {
  rows <- postcode_identity_rows()
  other <- data.table::copy(rows[1L])
  other[, `:=`(address_detail_pid = "SECONDARY", postcode = 4060L,
    address_label = "190 MUSGRAVE ROAD, RED HILL QLD 4060",
    primary_secondary = "S", primary_pid = "MUSGRAVE190")]
  rows <- data.table::rbindlist(list(rows, other), fill = TRUE)
  con <- postcode_identity_connection(rows)
  on.exit(gnaf_disconnect(con), add = TRUE)
  input <- "190 MUSGRAVE RD, RED HILL QLD 4058"
  # 4059 gets nearby-postcode credit; 4060 falls below this cutoff.
  out <- gnaf_match(input, con, min_score = 90L, cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid, "MUSGRAVE190")
  expect_equal(out$match_basis, "weighted")
  many <- gnaf_match(input, con, min_score = 0L, max_results = 10L, cache = FALSE, verbose = FALSE)
  expect_false(any(many$match_basis == "postcode_only"))
  # The exact-postcode address wins even with another distinct exact identity.
  exact <- gnaf_match("190 MUSGRAVE RD, RED HILL QLD 4059", con,
                      cache = FALSE, verbose = FALSE)
  expect_equal(exact$address_detail_pid, "MUSGRAVE190")
  expect_equal(exact$match_basis, "exact_components")
})

test_that("principal aliases share identity and alias filters remain effective", {
  rows <- postcode_identity_rows()
  alias <- data.table::copy(rows[1L])
  alias[, `:=`(address_detail_pid = "ALIAS", alias_type = "LOCALITY:SYN",
               principal_pid = "MUSGRAVE190")]
  con <- postcode_identity_connection(data.table::rbindlist(list(rows, alias), fill = TRUE))
  on.exit(gnaf_disconnect(con), add = TRUE)
  input <- "190 MUSGRAVE RD, RED HILL QLD 4000"
  out <- gnaf_match(input, con, max_results = 5L, cache = FALSE, verbose = FALSE)
  expect_equal(out[address_detail_pid %in% c("ALIAS", "MUSGRAVE190")]$match_basis,
               rep("postcode_only", 2L))
  linked <- gnaf_match(input, con, return_principal = TRUE, cache = FALSE, verbose = FALSE)
  expect_equal(linked$address_detail_pid, "MUSGRAVE190")
  expect_equal(linked$matched_address_detail_pid, "ALIAS")
  expect_equal(linked$match_basis, "postcode_only")
  core <- gnaf_match(input, con, include_aliases = FALSE, cache = FALSE, verbose = FALSE)
  expect_equal(core$address_detail_pid, "MUSGRAVE190")
  expect_equal(core$match_basis, "postcode_only")
})

test_that("zero-weight disagreements cannot claim exact identity or stop retrieval", {
  rows <- postcode_identity_rows()
  rows[1L, postcode := 4000L]
  rows[1L, address_label := "190 MUSGRAVE ROAD, RED HILL QLD 4000"]
  rows[1L, address_detail_pid := "Z_RIGHT"]
  rows[3L, address_detail_pid := "A_WRONG"]
  con <- postcode_identity_connection(rows)
  on.exit(gnaf_disconnect(con), add = TRUE)
  w <- list(postcode = 100, suburb = 0, street_name = 0, street_type = 0, number = 0, flat = 0)
  out <- gnaf_match("190 MUSGRAVE RD, RED HILL QLD 4000", con, weights = w,
                    max_results = 3L, cache = FALSE, verbose = FALSE)
  expect_equal(out$address_detail_pid[1L], "Z_RIGHT")
  expect_equal(out$match_basis, c("exact_components", rep("weighted", nrow(out) - 1L)))
  expect_true(all(out$total_score == 100L))
  # A misleading exact label with conflicting components must not short circuit.
  DBI::dbExecute(con, "UPDATE custom_addresses SET street_name = 'OTHER' WHERE address_detail_pid = 'Z_RIGHT'")
  DBI::dbExecute(con, "UPDATE custom_addresses SET street_name = 'MUSGRAVE', street_type = 'ROAD' WHERE address_detail_pid = 'A_WRONG'")
  recovered <- gnaf_match("190 MUSGRAVE RD, RED HILL QLD 4000", con, weights = w,
                          cache = FALSE, verbose = FALSE)
  expect_equal(recovered$address_detail_pid, "A_WRONG")
  expect_equal(recovered$match_basis, "exact_components")
})

test_that("exact identity rejects missing evidence and structural disagreements in R and SQL", {
  con <- postcode_identity_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  row <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT * FROM custom_addresses WHERE address_detail_pid = 'MUSGRAVE190'"))
  parsed <- address_parse("190 MUSGRAVE RD, RED HILL QLD 4000")
  pair <- cbind(parsed, row)
  variants <- list(
    list(), list(number_first = 14L), list(street_name = "PETRIE"),
    list(locality_name = "OTHER"), list(state = "NSW"), list(street_type = "STREET"),
    list(in_street_name = NA_character_), list(in_locality = NA_character_),
    list(in_number_first = NA_integer_), list(number_last = 192L),
    list(in_number_suffix = "A"), list(street_suffix = "N"),
    list(flat_type = "UNIT", flat_number = "2"), list(level_number = "1"),
    list(in_flat_number = "2"), list(in_level_number = "1"),
    list(in_lot_number = "7"), list(in_building_name = "CENTRE"),
    list(in_number_last = 192L, number_last = 192L),
    list(in_street_type = NA_character_, in_state = NA_character_),
    list(in_flat_type = "APARTMENT", flat_type = "UNIT", in_flat_number = "02", flat_number = "2"),
    list(in_level_type = "FLOOR", level_type = "L", in_level_number = "1", level_number = "1")
  )
  pairs <- data.table::rbindlist(lapply(variants, function(changes) {
    p <- data.table::copy(pair)
    for (field in names(changes)) data.table::set(p, j = field, value = changes[[field]])
    p
  }))
  expected <- seq_len(nrow(pairs)) %in% c(1L, 19L, 20L, 21L, 22L)
  original <- data.table::copy(pairs)
  expect_identical(.address_identity(pairs), expected)
  expect_identical(pairs, original)
  duckdb::duckdb_register(con, "identity_pairs", pairs)
  sql <- DBI::dbGetQuery(con, paste("SELECT", .address_identity_sql("p", "p"), "AS exact FROM identity_pairs p"))
  expect_identical(sql$exact, expected)
})

test_that("postcode corrections bypass storage and cached matches keep their basis", {
  con <- postcode_identity_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  # Lowering the cache threshold must not store a postcode correction.
  input <- "190 MUSGRAVE RD, RED HILL QLD 4058"
  first <- gnaf_match(input, con, cache_threshold = 0L, verbose = FALSE)
  second <- gnaf_match(input, con, cache_threshold = 0L, verbose = FALSE)
  expect_equal(first$match_basis, "postcode_only")
  expect_equal(second, first)
  expect_equal(gnaf_cache_status(con)$rows, 0)
  exact_input <- "190 MUSGRAVE RD, RED HILL QLD 4059"
  exact <- gnaf_match(exact_input, con, verbose = FALSE)
  expect_equal(gnaf_cache_status(con)$rows, 1)
  # Force the cache path rather than the exact-label shortcut.
  testthat::local_mocked_bindings(.exact_label_match = function(...) NULL)
  cached <- gnaf_match(exact_input, con, verbose = FALSE)
  expect_equal(cached$match_basis, exact$match_basis)
  expect_equal(cached$address_detail_pid, exact$address_detail_pid)
  DBI::dbExecute(con, "UPDATE gnaf_match_cache SET algorithm_version = 17, address_detail_pid = 'MUSGRAVE14'")
  renewed <- gnaf_match(exact_input, con, verbose = FALSE)
  expect_equal(renewed$address_detail_pid, "MUSGRAVE190")
  expect_equal(renewed$match_basis, "exact_components")
})
