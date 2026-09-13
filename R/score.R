# Weights must sum to 100.
.WEIGHTS <- list(postcode = 20L, suburb = 15L, street_name = 40L, street_type = 10L, number = 10L, flat = 5L)


.default_match_weights <- function() {
  as.list(.WEIGHTS)
}

.validate_match_weights <- function(weights) {
  required_names <- names(.WEIGHTS)

  if (!is.list(weights) || is.null(names(weights))) {
    stop("'weights' must be a named list")
  }
  if (anyDuplicated(names(weights)) || !setequal(names(weights), required_names)) {
    stop(
      "'weights' must be a named list with exactly these entries: ",
      paste(required_names, collapse = ", ")
    )
  }

  weights <- weights[required_names]
  if (!all(vapply(weights, function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x)
  }, logical(1L)))) {
    stop("'weights' values must each be one finite number", call. = FALSE)
  }
  weight_values <- unlist(weights, use.names = TRUE)
  if (!is.numeric(weight_values) || anyNA(weight_values)) {
    stop("'weights' values must all be numeric and non-missing")
  }
  if (any(weight_values < 0)) {
    stop("'weights' values must be non-negative")
  }
  if (!isTRUE(all.equal(sum(weight_values), 100, tolerance = 1e-8))) {
    stop("'weights' must sum to 100")
  }

  lapply(weights, as.numeric)
}

#' Score candidate pairs
#'
#' Operates on a data.table that has been produced by joining the parsed inputs
#' with GNAF candidates.  Adds score columns in-place and returns the table.
#'
#' Expected columns from the parsed side (prefixed \code{in_}):
#'   in_postcode, in_locality, in_street_name, in_street_type,
#'   in_number_first, in_flat_number
#'
#' Expected columns from the GNAF side (no prefix):
#'   postcode, locality_name, street_name, street_type,
#'   number_first, number_last, flat_number
#'

#' @noRd
# Generates DuckDB SQL CASE expressions for each score component.
# i / g are the table aliases for inputs and gnaf candidates respectively.
.score_sql_exprs <- function(weights, i = "i", g = "g",
                             suburb_similarity = NULL,
                             street_similarity = NULL) {
  w_pc  <- as.integer(round(weights$postcode))
  w_sub <- weights$suburb
  w_sn  <- weights$street_name
  w_st  <- weights$street_type
  w_num <- weights$number
  w_fl  <- weights$flat
  if (is.null(suburb_similarity)) {
    suburb_similarity <- sprintf(
      "jaro_winkler_similarity(%s.in_locality, %s.locality_name)", i, g
    )
  }
  if (is.null(street_similarity)) {
    street_similarity <- sprintf(
      "jaro_winkler_similarity(%s.in_street_name, %s.street_name)", i, g
    )
  }

  # Match R rounding, including ties to even and fractional postcode weights.
  list(
    score_postcode = sprintf(
      "CASE WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND %s.in_postcode = %s.postcode THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 1 THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 2 THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 3 THEN %d ELSE 0 END",
      i, g, i, g, w_pc,
      i, g, i, g, as.integer(round(weights$postcode * 0.7)),
      i, g, i, g, as.integer(round(weights$postcode * 0.4)),
      i, g, i, g, as.integer(round(weights$postcode * 0.2))
    ),
    score_suburb = .score_name_sql(
      paste0(i, ".in_locality"), paste0(g, ".locality_name"), w_sub, suburb_similarity
    ),
    score_street_name = .score_name_sql(
      paste0(i, ".in_street_name"), paste0(g, ".street_name"), w_sn, street_similarity
    ),
    score_street_type = .score_street_type_sql(w_st, i, g),
    score_number = .score_number_sql(w_num, i, g),
    score_flat = .score_flat_sql(w_fl, i, g)
  )
}

#' @param pairs data.table of candidate pairs (modified in-place).
#' @param weights Named list of scoring weights.
#' @return The same data.table with added columns \code{score_*} and
#'   \code{total_score}.
#' @noRd
.score_pairs <- function(pairs, weights = .WEIGHTS) {

  # --- Postcode (20 pts) ---------------------------------------------------
  pairs[, score_postcode := {
    both_present <- !is.na(in_postcode) & !is.na(postcode)
    diff <- abs(as.integer(in_postcode) - as.integer(postcode))
    fifelse(!both_present,  0L,
    fifelse(diff == 0L,     as.integer(round(weights$postcode)),
    fifelse(diff == 1L,     as.integer(round(weights$postcode * 0.7)),
    fifelse(diff == 2L,     as.integer(round(weights$postcode * 0.4)),
    fifelse(diff == 3L,     as.integer(round(weights$postcode * 0.2)), 0L)))))
  }]

  # --- Suburb / locality (15 pts) ------------------------------------------
  # Jaro-Winkler similarity; NA on either side → 0. Reshaped via
  # .component_similarity_factor() so two unrelated names of similar length
  # (raw JW noise floor ~0.5-0.6) don't bank meaningful partial credit -
  # only applied where both sides are present, so the reshaping's own floor
  # never leaks into the NA case (see .COMPONENT_SIM_FLOOR).
  pairs[, score_suburb := .score_name(in_locality, locality_name, weights$suburb)]

  # --- Street name (40 pts) ------------------------------------------------
  pairs[, score_street_name := .score_name(in_street_name, street_name, weights$street_name)]

  pairs[, score_street_type := .score_street_type(pairs, weights$street_type)]
  pairs[, score_number := .score_number(pairs, weights$number)]
  pairs[, score_flat := .score_flat(pairs, weights$flat)]

  # --- Total ---------------------------------------------------------------
  pairs[, total_score := score_postcode + score_suburb + score_street_name +
                         score_street_type + score_number + score_flat]

  pairs
}
