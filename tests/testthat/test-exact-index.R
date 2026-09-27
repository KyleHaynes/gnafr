exact_index_rows <- function() {
  data.table::data.table(
    address_detail_pid = c("P1", "P2", "P3", "D1", "D2", "A1", "A2", "P2-ALIAS"),
    address_label = c(
      "UNIT 50 11 ROLLESTON STREET, KEPERRA QLD 4054",
      "11 ROLLESTON STREET, KEPERRA QLD 4054",
      "LOT 24 INGLEWOOD - TEXAS ROAD, INGLEWOOD QLD 4387",
      "5 TWIN STREET, KEPERRA QLD 4054",
      "5 TWIN STREET, KEPERRA QLD 4054",
      "U 3 12 MAIN STREET, KEPERRA QLD 4054",
      "UNIT 3 12 MAIN STREET, KEPERRA QLD 4054",
      "11 ROLLESTON STREET, KEPERRA QLD 4054"),
    flat_type = c("UNIT", NA, NA, NA, NA, "U", "UNIT", NA),
    flat_number = c("50", NA, NA, NA, NA, "3", "3", NA),
    number_first = c(11L, 11L, NA, 5L, 5L, 12L, 12L, 11L),
    lot_number = c(NA, NA, "24", NA, NA, NA, NA, NA),
    street_name = c("ROLLESTON", "ROLLESTON", "INGLEWOOD - TEXAS", "TWIN", "TWIN",
                    "MAIN", "MAIN", "ROLLESTON"),
    street_type = c("STREET", "STREET", "ROAD", "STREET", "STREET", "STREET",
                    "STREET", "STREET"),
    locality_name = c("KEPERRA", "KEPERRA", "INGLEWOOD", "KEPERRA", "KEPERRA",
                      "KEPERRA", "KEPERRA", "KEPERRA"),
    state = "QLD",
    postcode = c(4054L, 4054L, 4387L, 4054L, 4054L, 4054L, 4054L, 4054L),
    alias_type = c(rep(NA_character_, 7L), "STREET:SYN"),
    principal_pid = c(rep(NA_character_, 7L), "P3"),
    source = "gnaf"
  )
}

# A database with a few principal addresses (and a synonym alias that shares
# P2's label) loaded straight into gnaf_addresses, as gnaf_load() would.
exact_index_db <- function(rows = exact_index_rows(), build = TRUE) {
  con <- gnaf_connect(":memory:")
  gnaf_init(con)
  DBI::dbAppendTable(con, "gnaf_addresses", as.data.frame(rows))
  gnaf_rebuild_locality_index(con)
  if (build) gnaf_rebuild_exact_index(con)
  con
}

exact_index_keys <- function(con, pid) {
  DBI::dbGetQuery(con, "SELECT lookup_key FROM gnaf_exact_index
                        WHERE address_detail_pid = ? ORDER BY lookup_key",
                  params = list(pid))$lookup_key
}

test_that("the index holds each label and the requested written variants", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  keys <- exact_index_keys(con, "P1")
  expect_true(all(c(
    "UNIT 50 11 ROLLESTON STREET KEPERRA QLD 4054",
    "U 50 11 ROLLESTON STREET KEPERRA QLD 4054",
    "U 50 11 ROLLESTON ST KEPERRA QLD 4054",
    "U 50 11 ROLLESTON ST KEPERRA 4054",
    "UNIT 50 11 ROLLESTON ST KEPERRA QLD 4054",
    "50/11 ROLLESTON STREET KEPERRA QLD 4054",
    "50/11 ROLLESTON ST KEPERRA 4054"
  ) %in% keys))
  expect_length(keys, 12L)
  # A plain address has street-type and state variants only.
  expect_setequal(exact_index_keys(con, "P2"), c(
    "11 ROLLESTON STREET KEPERRA QLD 4054", "11 ROLLESTON ST KEPERRA QLD 4054",
    "11 ROLLESTON STREET KEPERRA 4054", "11 ROLLESTON ST KEPERRA 4054"))
  state <- gnafr:::.exact_index_state(con)
  expect_true(state$variants)
  expect_identical(gnaf_rebuild_exact_index(con, variants = FALSE),
                   DBI::dbGetQuery(con, "SELECT count(*) AS n FROM gnaf_exact_index")$n)
  expect_setequal(exact_index_keys(con, "P2"), "11 ROLLESTON STREET KEPERRA QLD 4054")
  expect_false(gnafr:::.exact_index_state(con)$variants)
  expect_error(gnaf_rebuild_exact_index(con, variants = NA), "TRUE or FALSE")
})

test_that("keys normalise case, punctuation and spacing the same way on both sides", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  # Spaces around a hyphen and full stops are removed from label and input alike.
  expect_true("LOT 24 INGLEWOOD-TEXAS ROAD INGLEWOOD QLD 4387" %in%
                exact_index_keys(con, "P3"))
})

test_that("ambiguous keys are omitted and a genuine label beats a variant", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  # Two addresses share a label: neither may be named by it.
  expect_length(exact_index_keys(con, "D1"), 0L)
  expect_length(exact_index_keys(con, "D2"), 0L)
  # "U 3 12 MAIN STREET" is A1's own label and also a variant of A2's; the
  # label wins, and A2 keeps its other keys.
  key <- "U 3 12 MAIN STREET KEPERRA QLD 4054"
  owner <- DBI::dbGetQuery(con, "SELECT address_detail_pid FROM gnaf_exact_index
                                 WHERE lookup_key = ?", params = list(key))
  expect_identical(owner$address_detail_pid, "A1")
  expect_true("UNIT 3 12 MAIN STREET KEPERRA QLD 4054" %in% exact_index_keys(con, "A2"))
})

test_that("aliases are not indexed, so a synonym cannot shadow the real address", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_length(exact_index_keys(con, "P2-ALIAS"), 0L)
  out <- gnaf_match("11 Rolleston Street, Keperra QLD 4054", con, cache = FALSE,
                    verbose = FALSE)
  expect_identical(out$address_detail_pid, "P2")
})

test_that("labels and their written variants resolve without parsing", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  inputs <- c("UNIT 50 11 ROLLESTON STREET, KEPERRA QLD 4054",
              "unit 50 11 rolleston street keperra qld 4054",
              "  Unit 50 11 Rolleston Street,  Keperra QLD 4054 ",
              "u 50 11 rolleston st keperra 4054",
              "50/11 Rolleston St, Keperra QLD 4054")
  out <- gnaf_match(inputs, con, cache = FALSE, verbose = FALSE)
  expect_identical(out$input_id, seq_along(inputs))
  expect_identical(out$input_raw, inputs)
  expect_true(all(out$address_detail_pid == "P1"))
  expect_true(all(out$total_score == 100L))
  expect_true(all(out$match_basis == "exact_components"))
  expect_true(all(out$match_rank == 1L))
  expect_true(all(out$match_status == "matched"))
  expect_identical(out$input_standardised, rep("UNIT 50 11 ROLLESTON STREET, KEPERRA QLD 4054", 5L))
  # The parsed-input columns describe the address.
  expect_identical(unique(out$in_street_name), "ROLLESTON")
  expect_identical(unique(out$in_street_type), "STREET")
  expect_identical(unique(out$in_locality), "KEPERRA")
  expect_identical(unique(out$in_flat_type), "UNIT")
  expect_identical(unique(out$in_flat_number), "50")
  expect_identical(unique(out$in_number_first), 11L)
  expect_identical(unique(out$in_postcode), 4054L)
})

test_that("the indexed path agrees with the full matcher on everything but its own label", {
  indexed <- exact_index_db()
  plain <- exact_index_db(build = FALSE)
  on.exit({ gnaf_disconnect(indexed); gnaf_disconnect(plain) }, add = TRUE)
  inputs <- c("UNIT 50 11 ROLLESTON STREET, KEPERRA QLD 4054",
              "11 ROLLESTON STREET, KEPERRA QLD 4054")
  a <- gnaf_match(inputs, indexed, cache = FALSE, verbose = FALSE)
  b <- gnaf_match(inputs, plain, cache = FALSE, verbose = FALSE)
  expect_identical(names(a), names(b))
  for (nm in setdiff(names(a), c("address_detail_pid", "principal_pid", "alias_type",
                                 "alias_principal", "date_created"))) {
    expect_identical(a[[nm]], b[[nm]], info = nm)
  }
})

test_that("lot-only addresses are matched exactly by their label", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  out <- gnaf_match("LOT 24 INGLEWOOD - TEXAS ROAD, INGLEWOOD QLD 4387", con,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "P3")
  expect_identical(out$match_basis, "exact_components")
  expect_identical(out$total_score, 100L)
  expect_identical(out$in_lot_number, "24")
  expect_true(is.na(out$in_number_first))
  # The parser's normalisation of the street name carries over.
  expect_identical(out$in_street_name, "INGLEWOOD-TEXAS")
})

test_that("a batch keeps input order and still resolves what the index cannot", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  inputs <- c("11 Rollesten Street, Keperra QLD 4054", NA,
              "50/11 Rolleston St, Keperra QLD 4054", "",
              "11 ROLLESTON STREET, KEPERRA QLD 4054")
  out <- gnaf_match(inputs, con, cache = FALSE, verbose = FALSE)
  expect_identical(out$input_id, seq_along(inputs))
  expect_identical(out$matched, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(out$address_detail_pid, c("P2", NA, "P1", NA, "P2"))
  # The misspelt street was scored, not looked up; the exact labels were not.
  expect_lt(out$total_score[1L], 100L)
  expect_identical(out$total_score[c(3L, 5L)], c(100L, 100L))
})

test_that("an ambiguous label is left to the full matcher", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  out <- gnaf_match("5 TWIN STREET, KEPERRA QLD 4054", con, cache = FALSE, verbose = FALSE)
  expect_true(out$matched)
  expect_true(out$address_detail_pid %in% c("D1", "D2"))
})

test_that("custom addresses with the same label keep competing", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  suppressMessages(gnaf_add(con, data.table::data.table(
    address_detail_pid = "CUSTOM-P2", address_label = "11 ROLLESTON STREET, KEPERRA QLD 4054",
    number_first = 11L, street_name = "ROLLESTON", street_type = "STREET",
    locality_name = "KEPERRA", state = "QLD", postcode = 4054L)))
  out <- gnaf_match("11 ROLLESTON STREET, KEPERRA QLD 4054", con, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "CUSTOM-P2")
  out <- gnaf_match("11 ROLLESTON STREET, KEPERRA QLD 4054", con, include_custom = FALSE,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "P2")
})

test_that("options that could change the answer bypass the index", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  label <- "11 ROLLESTON STREET, KEPERRA QLD 4054"
  # Only alias rows are eligible here, so the principal must not be returned.
  out <- gnaf_match(label, con, alias_types = "STREET:SYN", cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "P2-ALIAS")
  # More than one result per input needs the full candidate list.
  out <- gnaf_match(label, con, max_results = 2L, cache = FALSE, verbose = FALSE)
  expect_gte(nrow(out), 2L)
  # The written variants respect normalize = FALSE; the label itself still hits.
  out <- gnaf_match(c("11 rolleston st keperra 4054", label), con, normalize = FALSE,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$input_standardised[2L], label)
})

test_that("custom weights are imputed by the real scorer", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  weights <- list(postcode = 20, suburb = 15, street_name = 40, street_type = 10,
                  number = 10, flat = 5)
  out <- gnaf_match("11 ROLLESTON STREET, KEPERRA QLD 4054", con, weights = weights,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "P2")
  expect_identical(c(out$score_postcode, out$score_suburb, out$score_street_name,
                     out$score_street_type, out$score_number, out$score_flat),
                   c(20L, 15L, 40L, 10L, 10L, 5L))
  expect_identical(out$total_score, 100L)
  expect_identical(unlist(gnafr:::.perfect_scores(gnafr:::.default_match_weights())[
    c("score_postcode", "score_flat", "total_score")]),
    c(score_postcode = 12L, score_flat = 20L, total_score = 100L))
  # An unreachable min_score leaves the matcher to decide (nothing qualifies).
  out <- gnaf_match("11 ROLLESTON STREET, KEPERRA QLD 4054", con, min_score = 100,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$total_score, 100L)
})

test_that("a stale index is ignored rather than trusted", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_false(is.null(gnafr:::.exact_index_state(con)))
  DBI::dbAppendTable(con, "gnaf_addresses", as.data.frame(data.table::data.table(
    address_detail_pid = "P9", address_label = "9 NEWBUILD STREET, KEPERRA QLD 4054",
    number_first = 9L, street_name = "NEWBUILD", street_type = "STREET",
    locality_name = "KEPERRA", state = "QLD", postcode = 4054L, source = "gnaf")))
  expect_null(gnafr:::.exact_index_state(con))
  out <- gnaf_match("9 NEWBUILD STREET, KEPERRA QLD 4054", con, cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "P9")
  gnaf_rebuild_exact_index(con)
  expect_false(is.null(gnafr:::.exact_index_state(con)))
  # A database without the tables behaves exactly as before.
  DBI::dbExecute(con, "DROP TABLE gnaf_exact_index")
  expect_null(gnafr:::.exact_index_state(con))
})

test_that("inputs answered by the index are not written to the match cache", {
  con <- exact_index_db()
  on.exit(gnaf_disconnect(con), add = TRUE)
  out <- gnaf_match(c("11 ROLLESTON STREET, KEPERRA QLD 4054",
                      "UNIT 50 11 ROLLESTON STREET, KEPERRA QLD 4054"),
                    con, verbose = FALSE)
  expect_true(all(out$matched))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM gnaf_match_cache")$n, 0)
})

test_that("street and flat abbreviations agree with the parser dictionaries", {
  st <- gnafr:::.exact_street_abbreviations()
  expect_identical(nrow(st), nrow(gnafr:::.EXACT_STREET_ABBREV))
  map <- gnafr:::.get_street_type_map()
  expect_identical(unname(map[st$abbr]), st$street_type)
  ft <- gnafr:::.get_parser_resources()$ft_map
  expect_identical(unname(ft[gnafr:::.EXACT_FLAT_ABBREV]), names(gnafr:::.EXACT_FLAT_ABBREV))
  # Every abbreviation the index will emit is one the parser reads back to the
  # same canonical word, so a variant can only ever name the same street type.
  p <- address_parse(paste("10 MAIN", st$abbr, "KEPERRA QLD 4054"))
  expect_identical(p$in_street_type, st$street_type)
})
