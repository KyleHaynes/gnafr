# Run from the repository root:
# Rscript --vanilla dev/test_calibrate_match_scores.R
# Synthetic software checks, not evidence of real address-matching accuracy.
pkgload::load_all(".", quiet = TRUE)
source("dev/calibrate_match_scores.R")
library(testthat)

calibration_fixture <- function() {
  set.seed(812L)
  n <- 900L
  x <- address_parse(paste(seq_len(n), "Main Road, Brisbane QLD 4000"))
  wrong_number <- sample(c(FALSE, TRUE), n, replace = TRUE)
  x[, `:=`(input_standardised = gnafr:::.standardise_input(x),
    address_detail_pid = paste0("P", input_id), matched = TRUE, match_rank = 1L,
    number_first = in_number_first + as.integer(wrong_number),
    number_last = NA_integer_, flat_number = NA_character_, lot_number = NA_character_,
    street_name = "MAIN", street_type = "ROAD", locality_name = "BRISBANE",
    state = "QLD", postcode = 4000L,
    validation_group = paste0("G", input_id),
    correct_match = stats::rbinom(n, 1L, ifelse(wrong_number, 0.2, 0.8)))]
  x[, address_label := paste(number_first, "MAIN ROAD, BRISBANE QLD 4000")]
  gnafr:::.score_pairs(x)
}

test_that("logistic calibration fits observed labels and preserves source results", {
  x <- calibration_fixture()
  before <- data.table::copy(x)
  model <- fit_match_calibration(x[1:500], x[501:700])
  scored <- predict_match_calibration(model, x[701:900])
  expect_identical(x, before)
  expect_true(all(is.finite(scored$match_probability)))
  expect_true(all(scored$match_probability > 0 & scored$match_probability < 1))
  expect_gt(mean(scored[number_conflict == FALSE, match_probability]),
             mean(scored[number_conflict == TRUE, match_probability]))
  report <- evaluate_match_calibration(model, x[701:900], threshold = 0.5)
  expect_identical(report$summary$inputs, 200L)
  expect_gt(report$summary$brier, 0)
  expect_equal(sum(report$reliability$n), 200L)
  expect_equal(report$thresholds$accepted, sum(!scored$number_conflict))
  expect_true(length(model$dropped_predictors) > 0L)
  empty <- predict_match_calibration(model, x[0L])
  expect_identical(empty$match_probability, numeric())
})

test_that("calibration rejects leaked groups, repeated addresses and invalid labels", {
  x <- calibration_fixture()
  model <- fit_match_calibration(x[1:500], x[501:700])
  expect_error(fit_match_calibration(x[1:500], x[400:600]), "groups overlap")
  leaked <- data.table::copy(x[501:700])
  leaked[1L, input_standardised := x$input_standardised[1L]]
  expect_error(fit_match_calibration(x[1:500], leaked), "addresses overlap")
  expect_error(evaluate_match_calibration(model, x[501:700], 0.5), "groups overlap")
  expect_error(evaluate_match_calibration(model, x[701:900], NA_real_), "threshold")
  invalid <- data.table::copy(x[1:500])
  invalid[1L, correct_match := NA_integer_]
  expect_error(fit_match_calibration(invalid, x[501:700]), "no missing labels")
  invalid[, correct_match := 1L]
  expect_error(fit_match_calibration(invalid, x[501:700]), "both correct and incorrect")
})

test_that("unmatched inputs remain in coverage and alternatives get no top-match probability", {
  x <- calibration_fixture()
  model <- fit_match_calibration(x[1:500], x[501:700])
  test <- data.table::copy(x[701:900])
  test[1L, `:=`(matched = FALSE, address_detail_pid = NA_character_,
                match_rank = NA_integer_, total_score = NA_integer_, correct_match = 0L)]
  alternative <- data.table::copy(test[2L])
  alternative[, `:=`(match_rank = 2L, address_detail_pid = "ALTERNATIVE", total_score = 50L)]
  candidates <- data.table::rbindlist(list(test, alternative))
  scored <- predict_match_calibration(model, candidates)
  expect_true(all(is.na(scored[matched == FALSE | match_rank == 2L, match_probability])))
  report <- evaluate_match_calibration(model, candidates, 0.5)
  expect_identical(report$summary$inputs, 200L)
  expect_identical(report$summary$returned_matches, 199L)
})
