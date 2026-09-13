threshold_results <- function() {
  data.table::data.table(
    input_id = c(1L, 2L, 3L, 3L, 4L),
    input_raw = c("10 Main Rd, Brisbane QLD 4000", "5 Old St, St Lucia QLD 4067",
                  "2 New Ave, Toowong QLD 4066", "2 New Ave, Toowong QLD 4066",
                  "no such place"),
    input_standardised = c("10 MAIN ROAD, BRISBANE QLD 4000", "5 OLD STREET, ST LUCIA QLD 4067",
                           "2 NEW AVENUE, TOOWONG QLD 4066", "2 NEW AVENUE, TOOWONG QLD 4066",
                           "NO SUCH PLACE"),
    match_rank = c(1L, 1L, 1L, 2L, NA_integer_),
    matched = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    address_label = c("10 MAIN ROAD, BRISBANE QLD 4000", "5 OLD STREET, ST LUCIA QLD 4067",
                      "2 NEW AVENUE, TOOWONG QLD 4066", "2 NEWER AVENUE, TOOWONG QLD 4066",
                      NA_character_),
    total_score = c(100L, 81L, 95L, 70L, NA_integer_),
    score_postcode = c(20L, 20L, 20L, 20L, NA_integer_),
    score_suburb = c(15L, 15L, 15L, 15L, NA_integer_),
    score_street_name = c(40L, 21L, 40L, 15L, NA_integer_),
    score_street_type = c(10L, 10L, 10L, 10L, NA_integer_),
    score_number = c(10L, 10L, 5L, 5L, NA_integer_),
    score_flat = c(5L, 5L, 5L, 5L, NA_integer_),
    in_postcode = c(4000L, 4067L, 4066L, 4066L, NA_integer_),
    postcode = c(4000L, 4067L, 4066L, 4066L, NA_integer_),
    in_street_name = c("MAIN", "OLD", "NEW", "NEW", NA_character_),
    street_name = c("MAIN", "OLD", "NEW", "NEWER", NA_character_),
    extra_col = letters[1:5]
  )
}

full_range <- function(vars, maxes) {
  stats::setNames(lapply(vars, function(v) c(0L, maxes[[v]])), vars)
}

test_that("gnaf_text_scores appends the four similarity columns", {
  x <- data.table::data.table(
    input_raw = c("10 Main Road, Brisbane QLD 4000", "ABC", "gone"),
    address_label = c("10 MAIN ROAD, BRISBANE QLD 4000", "ABD", NA_character_),
    matched = c(TRUE, TRUE, FALSE)
  )
  scored <- gnaf_text_scores(x)
  expect_identical(names(scored), c(names(x), "jarowinkler_score", "jaccard_score",
                                    "levenshtein_score", "text_similarity"))
  expect_identical(names(x), c("input_raw", "address_label", "matched"))
  expect_equal(unlist(scored[1L, .(jarowinkler_score, jaccard_score, levenshtein_score, text_similarity)]),
               c(jarowinkler_score = 100, jaccard_score = 100, levenshtein_score = 100, text_similarity = 100))
  expect_equal(scored$levenshtein_score[2L], round(100 * (1 - 1 / 3), 1))
  expect_true(all(is.na(unlist(scored[3L, .(jarowinkler_score, jaccard_score, levenshtein_score, text_similarity)]))))
  expect_error(gnaf_text_scores(data.frame(a = 1)), "must be a data.table")
  expect_error(gnaf_text_scores(data.table::data.table(a = 1)), "missing columns")
})

test_that("text scores compare the original match when a linked address is returned", {
  x <- threshold_results()
  original <- gnaf_text_scores(x)
  x[, matched_address_label := address_label]
  x[matched == TRUE, address_label := "A DIFFERENT PRIMARY ADDRESS"]
  before <- copy(x)
  resolved <- gnaf_text_scores(x)
  scores <- c("jarowinkler_score", "jaccard_score", "levenshtein_score", "text_similarity")
  expect_identical(resolved[, ..scores], original[, ..scores])
  expect_identical(x, before)
})

test_that("threshold vars include text scores and maxes come from data and weights", {
  x <- threshold_results()
  data <- .gnaf_threshold_prepare(x)
  vars <- .gnaf_threshold_vars(data)
  expect_identical(vars[1:2], c("total_score", "score_postcode"))
  expect_true(all(c("jarowinkler_score", "jaccard_score", "levenshtein_score", "text_similarity") %in% vars))
  expect_false("levenshtein_score" %in% .gnaf_threshold_vars(x))

  maxes <- .gnaf_threshold_maxes(data, vars)
  expect_identical(maxes$total_score, 100L)
  expect_identical(maxes$score_street_name, 40L)
  expect_identical(maxes$score_flat, 5L)
  expect_identical(maxes$levenshtein_score, 100L)

  boosted <- copy(x)[, score_flat := 12L]
  expect_identical(.gnaf_threshold_maxes(boosted, "score_flat")$score_flat, 12L)
  empty <- .gnaf_threshold_prepare(x[0L])
  expect_identical(.gnaf_threshold_maxes(empty, vars)$score_number, 10L)
  expect_identical(nrow(empty), 0L)
})

test_that("scope mirrors the generated data.table filter, including NA handling", {
  x <- threshold_results()
  vars <- .gnaf_threshold_vars(x)
  maxes <- .gnaf_threshold_maxes(x, vars)
  thresholds <- full_range(vars, maxes)
  thresholds$total_score <- c(80L, 100L)
  thresholds$score_street_name <- c(30L, 40L)

  keep <- .gnaf_threshold_scope(x, thresholds, maxes, matched_only = TRUE)
  expect_identical(keep, c(TRUE, FALSE, TRUE, FALSE, FALSE))

  conds <- .gnaf_threshold_conditions(thresholds, maxes, matched_only = TRUE)
  expect_identical(conds, c("matched == TRUE", "total_score >= 80", "score_street_name >= 30"))
  code <- .gnaf_threshold_code("x", conds)
  expect_identical(code$data.table[["in_scope"]],
                   "x[matched == TRUE & total_score >= 80 & score_street_name >= 30]")
  expect_identical(eval(parse(text = code$data.table[["in_scope"]])), x[keep])
  expect_identical(eval(parse(text = code$data.table[["out_of_scope"]])), x[!keep])
})

test_that("upper bounds, rank and flags are reflected in scope and code", {
  x <- threshold_results()
  vars <- .gnaf_threshold_vars(x)
  maxes <- .gnaf_threshold_maxes(x, vars)
  thresholds <- full_range(vars, maxes)
  thresholds$score_number <- c(0L, 5L)

  keep <- .gnaf_threshold_scope(x, thresholds, maxes, matched_only = FALSE,
                                top_rank_only = TRUE, flagged = 3L)
  expect_identical(keep, rep(FALSE, 5L))
  keep2 <- .gnaf_threshold_scope(x, thresholds, maxes, matched_only = FALSE,
                                 top_rank_only = TRUE)
  expect_identical(keep2, c(FALSE, FALSE, TRUE, FALSE, FALSE))

  conds <- .gnaf_threshold_conditions(thresholds, maxes, matched_only = FALSE,
                                      top_rank_only = TRUE, flagged = c(3L, 4L))
  expect_identical(conds, c("match_rank == 1", "score_number <= 5", "!input_id %in% c(3, 4)"))
  code <- .gnaf_threshold_code("res", conds)
  expect_identical(
    code$dplyr[["in_scope"]],
    "res |>\n  dplyr::filter(match_rank == 1, score_number <= 5, !input_id %in% c(3, 4))"
  )
  expect_identical(eval(parse(text = code$data.table[["in_scope"]]), list(res = x)), x[keep])
})

test_that("text-score thresholds wrap the source in gnaf_text_scores()", {
  x <- threshold_results()
  data <- .gnaf_threshold_prepare(x)
  vars <- .gnaf_threshold_vars(data)
  maxes <- .gnaf_threshold_maxes(data, vars)
  thresholds <- full_range(vars, maxes)
  expect_false(.gnaf_threshold_uses_text(thresholds, maxes))
  thresholds$jarowinkler_score <- c(95L, 100L)
  expect_true(.gnaf_threshold_uses_text(thresholds, maxes))

  conds <- .gnaf_threshold_conditions(thresholds, maxes)
  code <- .gnaf_threshold_code("x", conds, wrap_text = TRUE)
  expect_identical(code$data.table[["in_scope"]],
                   "gnaf_text_scores(x)[matched == TRUE & jarowinkler_score >= 95]")
  expect_identical(code$dplyr[["in_scope"]],
                   "x |>\n  gnaf_text_scores() |>\n  dplyr::filter(matched == TRUE, jarowinkler_score >= 95)")
  keep <- .gnaf_threshold_scope(data, thresholds, maxes)
  expect_identical(eval(parse(text = code$data.table[["in_scope"]])), data[keep])
  expect_identical(eval(parse(text = code$data.table[["out_of_scope"]])), data[!keep])
  expect_true(any(keep) && !all(keep[1:4]))

  plain <- .gnaf_threshold_code("x", character(), wrap_text = TRUE)
  expect_identical(plain$data.table[["in_scope"]], "gnaf_text_scores(x)")
})

test_that("no active conditions yields pass-through code", {
  code <- .gnaf_threshold_code("x", character())
  expect_identical(code$data.table, c(in_scope = "x", out_of_scope = "x[0L]"))
  expect_match(code$dplyr[["out_of_scope"]], "dplyr::filter(FALSE)", fixed = TRUE)
})

test_that("the app validates input and its server produces printable code", {
  expect_error(gnaf_threshold_filter(data.frame(a = 1), run = FALSE), "must be a data.table")
  expect_error(gnaf_threshold_filter(data.table::data.table(a = 1), run = FALSE), "missing columns")
  x0 <- threshold_results()
  expect_error(gnaf_threshold_filter(x0, text_scores = NA, run = FALSE), "must be TRUE or FALSE")
  expect_error(gnaf_threshold_filter(x0, max_rows = 0, run = FALSE), "positive")
  expect_error(gnaf_threshold_filter(x0, html = NA, run = FALSE), "'html' must be")
  expect_error(gnaf_threshold_filter(x0, html = c(TRUE, FALSE), run = FALSE), "'html' must be")
  expect_error(gnaf_threshold_filter(x0, html = "", run = FALSE), "'html' must be")

  x <- threshold_results()
  app <- gnaf_threshold_filter(x, run = FALSE)
  expect_s3_class(app, "shiny.appobj")

  shiny::testServer(app, {
    session$setInputs(
      obj_name = "x", matched_only = TRUE, top_rank_only = FALSE,
      diff_pair = "std_match", diff_granularity = "diff_chars",
      thr_total_score = c(80, 100), thr_score_street_name = c(30, 40)
    )
    # Data prep (gnaf_text_scores) runs once, synchronously, on session start.
    expect_false(is.null(data()))
    # Threshold changes are debounced so a slider drag only recomputes once
    # settled; advance the mocked clock past the debounce window to observe it.
    session$elapse(.GNAF_THRESHOLD_DEBOUNCE_MS + 50)

    expect_identical(scope(), c(TRUE, FALSE, TRUE, FALSE, FALSE))
    expect_identical(nrow(in_data()), 2L)
    expect_identical(nrow(out_data()), 3L)
    expect_identical(
      code()$data.table[["in_scope"]],
      "x[matched == TRUE & total_score >= 80 & score_street_name >= 30]"
    )
    expect_match(output$code_dt, "# in scope", fixed = TRUE)

    session$setInputs(thr_total_score = c(0, 100), thr_score_street_name = c(0, 40),
                      thr_levenshtein_score = c(90, 100))
    session$elapse(.GNAF_THRESHOLD_DEBOUNCE_MS + 50)
    expect_identical(
      code()$data.table[["in_scope"]],
      "gnaf_text_scores(x)[matched == TRUE & levenshtein_score >= 90]"
    )
    prepared <- .gnaf_threshold_prepare(x)
    expect_identical(scope(), .gnaf_threshold_scope(
      prepared, thresholds(),
      .gnaf_threshold_maxes(prepared, .gnaf_threshold_vars(prepared))
    ))

    session$setInputs(thr_levenshtein_score = c(0, 100))
    session$elapse(.GNAF_THRESHOLD_DEBOUNCE_MS + 50)
    flagged(3L)
    expect_identical(scope(), c(TRUE, TRUE, FALSE, FALSE, FALSE))
    expect_identical(out_data()[flagged == TRUE, unique(input_id)], 3L)

    res <- result()
    expect_s3_class(res, "gnaf_threshold_filter")
    expect_identical(res$flagged_input_ids, 3L)
    expect_identical(res$n_in_scope, 2L)
    expect_identical(res$n_out_of_scope, 3L)
    expect_identical(res$code$data.table[["in_scope"]], "x[matched == TRUE & !input_id %in% c(3)]")
    expect_false(any(c("in_scope", "out_of_scope") %in% names(res)))
    expect_output(print(res), "2 rows in scope, 3 out of scope, 1 flagged")
    expect_output(print(res), "x[matched == TRUE & !input_id %in% c(3)]", fixed = TRUE)
  })
})

test_that("text_scores = FALSE skips gnaf_text_scores() and drops text vars/sliders", {
  x <- threshold_results()
  app <- gnaf_threshold_filter(x, text_scores = FALSE, run = FALSE)
  expect_s3_class(app, "shiny.appobj")

  shiny::testServer(app, {
    session$setInputs(thr_total_score = c(80, 100))
    session$elapse(.GNAF_THRESHOLD_DEBOUNCE_MS + 50)
    expect_false(is.null(data()))
    expect_false("jarowinkler_score" %in% names(data()))
    expect_identical(names(data()), names(x))
    expect_identical(scope(), x$total_score %in% 80:100 & x$matched)
  })
})

test_that("score colouring scales to each component's maximum, not 100", {
  full_total <- .gnaf_score_col("Total", digits = 0, max = 100)$style(100)
  full_type <- .gnaf_score_col("Street type", digits = 0, max = 10)$style(10)
  expect_identical(full_type$background, full_total$background)
  expect_identical(full_type$background, .gnaf_score_fill(100))
  expect_identical(full_type$color, "#f8fafc")

  half_name <- .gnaf_score_col("Street name", digits = 0, max = 40)$style(20)
  expect_identical(half_name$background, .gnaf_score_fill(50))
  expect_identical(half_name$color, "#102a43")
  expect_identical(.gnaf_score_col("x", max = 0)$style(70)$background, .gnaf_score_fill(70))
  expect_identical(.gnaf_score_col("x")$style(NA_real_)$background, "#f3f4f6")
})

test_that("large results are downsampled for the histogram, not the tables/counts", {
  expect_identical(formals(gnaf_threshold_filter)$max_rows, 200L)

  big <- data.table::data.table(total_score = 1:2000)
  keep <- rep(c(TRUE, FALSE), 1000)
  untouched <- .gnaf_threshold_sample_rows(big, keep, sample_n = 5000L)
  expect_identical(untouched$data, big)
  expect_identical(untouched$keep, keep)

  set.seed(1)
  sampled <- .gnaf_threshold_sample_rows(big, keep, sample_n = 100L)
  expect_identical(nrow(sampled$data), 100L)
  expect_identical(length(sampled$keep), 100L)
  expect_true(all(sampled$data$total_score %in% big$total_score))
})

test_that("tables and plot render for the prepared data", {
  x <- threshold_results()
  data <- .gnaf_threshold_prepare(x)
  vars <- .gnaf_threshold_vars(data)
  maxes <- .gnaf_threshold_maxes(data, vars)

  table <- .gnaf_threshold_table(
    data, pair = "std_match", method_fn = jsdiffr::diff_chars,
    extra_cols = "extra_col", vars = vars, maxes = maxes, show_flagged = FALSE
  )
  expect_s3_class(table, "reactable")
  out <- copy(data)[, flagged := input_id == 3L]
  expect_s3_class(.gnaf_threshold_table(
    out, pair = "raw_match", method_fn = jsdiffr::diff_words, extra_cols = character(),
    vars = vars, maxes = maxes, show_flagged = TRUE
  ), "reactable")
  expect_s3_class(.gnaf_threshold_details(data[1L]), "shiny.tag")

  thresholds <- full_range(vars, maxes)
  thresholds$total_score <- c(80L, 100L)
  thresholds$jaccard_score <- c(0L, 90L)
  keep <- .gnaf_threshold_scope(data, thresholds, maxes)
  expect_s3_class(.gnaf_threshold_plot(data, vars, keep, thresholds, maxes), "ggplot")
  expect_null(.gnaf_threshold_plot(.gnaf_threshold_prepare(x[0L]), vars, logical(), thresholds, maxes))
})

test_that(".gnaf_threshold_html_page renders a self-contained diff table", {
  x <- threshold_results()
  page <- .gnaf_threshold_html_page(x, "res")

  expect_type(page, "character")
  expect_length(page, 1L)
  expect_match(page, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(page, "res &mdash; input vs matched diff", fixed = TRUE)
  # jsdiff-pre/jsdiff-added markup + colours come straight from jsdiffr, same
  # as the app's own diff cells - this is what "same look and feel" means.
  expect_match(page, "jsdiff-added", fixed = TRUE)
  expect_match(page, "diff-table", fixed = TRUE)
  # The unmatched row (input_id 4) has no address_label to diff against.
  expect_match(page, "No match to compare", fixed = TRUE)
  # Every row is included - no max_rows-style truncation in this mode.
  expect_identical(lengths(regmatches(page, gregexpr("<tr>", page))), nrow(x) + 1L)
  # Standardised input and matched address get their own plain-text columns,
  # in addition to (not instead of) the diff column.
  expect_match(
    page,
    '<th data-type="text">Standardised</th><th data-type="text">Matched address</th><th data-type="none">Diff</th>',
    fixed = TRUE
  )
  expect_match(page, "<td>5 OLD STREET, ST LUCIA QLD 4067</td><td>5 OLD STREET, ST LUCIA QLD 4067</td>", fixed = TRUE)
  # Sortable/resizable columns are plain JS/CSS against data-type + colgroup,
  # no table widget - keeps the file lightweight for very large results.
  expect_match(page, "<colgroup>", fixed = TRUE)
  expect_match(page, "col-resizer", fixed = TRUE)
  expect_match(page, "addEventListener('click'", fixed = TRUE)

  words_page <- .gnaf_threshold_html_page(x, "res", pair = "raw_match", method_fn = jsdiffr::diff_words)
  expect_match(
    words_page,
    '<th data-type="text">Input</th><th data-type="text">Matched address</th><th data-type="none">Diff</th>',
    fixed = TRUE
  )
})

test_that("html = TRUE/path bypasses the Shiny app and writes a static diff table", {
  x <- threshold_results()

  html_string <- gnaf_threshold_filter(x, html = TRUE, run = FALSE)
  expect_type(html_string, "character")
  expect_match(html_string, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(html_string, "jsdiff-pre", fixed = TRUE)

  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  result <- gnaf_threshold_filter(x, html = path, launch.browser = FALSE)
  expect_identical(result, path)
  expect_true(file.exists(path))
  written <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(written, "<!DOCTYPE html>", fixed = TRUE)
})
