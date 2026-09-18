# Logistic baseline for the correctness of the existing top-ranked candidate.
# Source this file after loading gnafr. See reports/SCORING_OVERHAUL.md for the
# labelled-data contract and a train/validation/test example. No files are read
# or written, and no model is fitted merely by sourcing this script.

match_calibration_predictors <- function(evidence) {
  columns <- grep(paste0(
    "^agreement_|^(raw|standardised)_(jarowinkler|jaccard|levenshtein)_score$|",
    "_(conflict|input_missing|candidate_missing)$|^score_gap$|^tied_best$"
  ), names(evidence), value = TRUE)
  predictors <- data.table::copy(evidence[, columns, with = FALSE])
  # Total and mean text scores are deliberately excluded: they duplicate
  # the component/individual text predictors and make the design singular.
  for (name in columns) {
    value <- as.numeric(predictors[[name]])
    if (name == "score_gap" || grepl("_score$", name)) value <- value / 100
    data.table::set(predictors, j = paste0(name, "_unavailable"), value = as.numeric(is.na(value)))
    value[is.na(value)] <- 0
    data.table::set(predictors, j = name, value = value)
  }
  predictors
}

match_calibration_rows <- function(results, outcome, group) {
  required <- c(outcome, group, "match_rank")
  missing <- setdiff(required, names(results))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  evidence <- gnafr::gnaf_match_features(results)
  # Features must see all candidates before retaining the top result, otherwise
  # the competitor margin and ties would always be unavailable.
  rows <- which(!evidence$matched | evidence$match_rank == 1L)
  evidence <- evidence[rows]
  if (!nrow(evidence) || anyNA(evidence$input_id) || anyDuplicated(evidence$input_id)) {
    stop("Each split must contain exactly one top result (or unmatched row) per input ID")
  }
  y <- evidence[[outcome]]
  if (!(is.logical(y) || is.numeric(y)) || anyNA(y) || !all(y %in% c(0, 1))) {
    stop("Verified outcome must be logical or numeric 0/1, with no missing labels")
  }
  if (any(!evidence$matched & as.logical(y))) stop("An unmatched input cannot have a correct top match")
  if (anyNA(evidence[[group]]) || any(!nzchar(trimws(as.character(evidence[[group]]))))) {
    stop("Every input needs a non-missing validation group")
  }
  list(evidence = evidence, predictors = match_calibration_predictors(evidence), y = as.numeric(y))
}

predict_match_calibration <- function(model, results) {
  evidence <- gnafr::gnaf_match_features(results)
  predictors <- match_calibration_predictors(evidence)
  evidence[, match_probability := NA_real_]
  rows <- which(evidence$matched %in% TRUE & evidence$match_rank == 1L)
  if (length(rows)) {
    probability <- stats::predict(model$fit,
      newdata = as.data.frame(predictors[rows, model$predictors, with = FALSE]), type = "response")
    data.table::set(evidence, i = rows, j = "match_probability", value = as.numeric(probability))
  }
  # The model was trained on rank one. Do not misrepresent its probabilities
  # as suitable for lower-ranked candidates or for re-ranking.
  evidence[is.na(match_rank) | match_rank != 1L, match_probability := NA_real_]
  evidence
}

match_calibration_report <- function(scored, outcome, thresholds) {
  top <- scored[matched == FALSE | match_rank == 1L]
  y <- as.numeric(top[[outcome]])
  available <- is.finite(top$match_probability)
  probability <- top$match_probability[available]
  observed <- y[available]
  clipped <- pmin(1 - 1e-15, pmax(1e-15, probability))
  summary <- data.table::data.table(
    inputs = nrow(top), returned_matches = sum(available),
    top_match_accuracy = mean(y),
    brier = if (length(probability)) mean((probability - observed)^2) else NA_real_,
    log_loss = if (length(probability)) -mean(observed * log(clipped) +
      (1 - observed) * log1p(-clipped)) else NA_real_
  )
  policy <- data.table::rbindlist(lapply(thresholds, function(threshold) {
    # A model probability cannot override known structural conflicts or ties.
    accepted <- available & top$match_probability >= threshold &
      top$has_identifier_conflict %in% FALSE & top$tied_best %in% FALSE
    data.table::data.table(threshold = threshold, accepted = sum(accepted),
      false_accepts = sum(y[accepted] == 0),
      precision = if (any(accepted)) mean(y[accepted]) else NA_real_,
      coverage = mean(accepted))
  }))
  bins <- data.table::data.table(probability = probability, correct = observed)
  bins[, bin := pmin(9L, as.integer(probability * 10))]
  reliability <- bins[, .(n = .N, predicted = mean(probability), observed = mean(correct)), by = bin]
  data.table::setorder(reliability, bin)
  list(summary = summary, thresholds = policy, reliability = reliability)
}

fit_match_calibration <- function(train, validation, outcome = "correct_match",
                                  group = "validation_group") {
  parts <- lapply(list(train = train, validation = validation), match_calibration_rows,
                  outcome = outcome, group = group)
  overlap <- intersect(parts$train$evidence[[group]], parts$validation$evidence[[group]])
  if (length(overlap)) stop("Training and validation groups overlap; split groups before fitting")
  # Also catch accidentally duplicated address inputs even if callers assign
  # them distinct entity IDs. Group households/repeated addresses beforehand.
  address_key <- function(x) toupper(gsub("[[:space:]]+", " ", trimws(x$input_standardised)))
  overlap <- intersect(address_key(parts$train$evidence), address_key(parts$validation$evidence))
  overlap <- overlap[!is.na(overlap) & nzchar(overlap)]
  if (length(overlap)) stop("Training and validation addresses overlap; group repeated addresses together")
  keep <- parts$train$evidence$matched %in% TRUE
  y <- parts$train$y[keep]
  if (length(unique(y)) != 2L) stop("Training needs both correct and incorrect returned matches")
  design <- as.matrix(parts$train$predictors[keep])
  # Drop constant and exactly dependent columns using training data only.
  # Record every exclusion so the baseline remains inspectable.
  decomposition <- qr(cbind(`(Intercept)` = 1, design))
  independent <- decomposition$pivot[seq_len(decomposition$rank)]
  selected <- colnames(design)[independent[independent > 1L] - 1L]
  if (!length(selected)) stop("Training has no varying independent predictors")
  if (length(y) <= length(selected) + 1L) stop("Too few training matches for the model's independent predictors")
  training <- as.data.frame(parts$train$predictors[keep, selected, with = FALSE])
  training$.correct <- y
  formula <- stats::reformulate(if (length(selected)) selected else "1", response = ".correct")
  environment(formula) <- baseenv()
  warnings <- character()
  fit <- withCallingHandlers(stats::glm(formula, data = training,
    family = stats::binomial(), na.action = stats::na.fail, model = FALSE,
    control = stats::glm.control(maxit = 100L)), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (length(warnings) || !isTRUE(fit$converged) || isTRUE(fit$boundary) || any(!is.finite(stats::coef(fit)))) {
    stop("Unstable logistic fit; check sample size/separation or use penalised logistic regression. ",
      paste(unique(warnings), collapse = "; "))
  }
  model <- list(fit = fit, predictors = selected,
    dropped_predictors = setdiff(colnames(design), selected),
    training_groups = unique(parts$train$evidence[[group]]),
    validation_groups = unique(parts$validation$evidence[[group]]),
    training_addresses = unique(address_key(parts$train$evidence)),
    validation_addresses = unique(address_key(parts$validation$evidence)),
    outcome = outcome, group = group)
  scored <- predict_match_calibration(model, validation)
  model$validation <- match_calibration_report(scored, outcome,
    thresholds = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999))
  model
}

evaluate_match_calibration <- function(model, test, threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold) ||
      threshold < 0 || threshold > 1) stop("Choose one threshold between 0 and 1 using validation data")
  rows <- match_calibration_rows(test, model$outcome, model$group)
  seen <- c(model$training_groups, model$validation_groups)
  if (length(intersect(rows$evidence[[model$group]], seen))) stop("Test groups overlap training/validation")
  keys <- toupper(gsub("[[:space:]]+", " ", trimws(rows$evidence$input_standardised)))
  seen <- c(model$training_addresses, model$validation_addresses)
  overlap <- intersect(keys, seen)
  if (any(!is.na(overlap) & nzchar(overlap))) stop("Test addresses overlap training/validation")
  match_calibration_report(predict_match_calibration(model, test), model$outcome, threshold)
}
