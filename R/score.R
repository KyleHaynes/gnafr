# Weights must sum to 100.
.WEIGHTS <- list(postcode = 12L, suburb = 12L, street_name = 16L, street_type = 10L, number = 30L, flat = 20L)


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
#
# street_gate is a SQL expression scaling the number and flat scores by street
# agreement (see .street_gate()). Bulk queries pass a column computed once per
# candidate; otherwise it is derived from the street names.
.score_sql_exprs <- function(weights, i = "i", g = "g",
                             suburb_similarity = NULL,
                             street_similarity = NULL,
                             street_gate = NULL) {
  w_sub <- weights$suburb
  w_sn  <- weights$street_name
  w_st  <- weights$street_type
  w_num <- weights$number
  w_fl  <- weights$flat

  if (is.null(street_gate)) {
    if (is.null(street_similarity))
      street_similarity <- .name_similarity_sql(
        paste0(i, ".in_street_name"), paste0(g, ".street_name")
      )
    street_gate <- .street_gate_sql(street_similarity)
  }

  # Match R rounding, including ties to even and fractional postcode weights.
  list(
    score_postcode = .score_postcode_sql(
      paste0(i, ".in_postcode"), paste0(g, ".postcode"), weights$postcode
    ),
    score_suburb = .score_name_sql(
      paste0(i, ".in_locality"), paste0(g, ".locality_name"), w_sub, suburb_similarity
    ),
    score_street_name = .score_name_sql(
      paste0(i, ".in_street_name"), paste0(g, ".street_name"), w_sn, street_similarity
    ),
    score_street_type = .score_street_type_sql(w_st, i, g),
    score_number = .score_number_sql(w_num, i, g, street_gate),
    score_flat = .score_flat_sql(w_fl, i, g, street_gate)
  )
}

#' @param pairs data.table of candidate pairs (modified in-place).
#' @param weights Named list of scoring weights.
#' @param street_gate If \code{TRUE} (default), number and flat credit is scaled
#'   by street-name agreement, as in the DuckDB scorer. \code{FALSE} returns the
#'   raw agreement on each component, for features that must not depend on it.
#' @return The same data.table with added columns \code{score_*} and
#'   \code{total_score}.
#' @noRd
.score_pairs <- function(pairs, weights = .WEIGHTS, street_gate = TRUE) {

  # --- Postcode (20 pts) ---------------------------------------------------
  pairs[, score_postcode := .score_postcode(in_postcode, postcode, weights$postcode)]

  # --- Suburb / locality (15 pts) ------------------------------------------
  # Blend prefix and edit evidence, with explicit direction conflicts.
  pairs[, score_suburb := .score_name(in_locality, locality_name, weights$suburb)]

  # --- Street name (40 pts) ------------------------------------------------
  pairs[, score_street_name := .score_name(in_street_name, street_name, weights$street_name)]

  pairs[, score_street_type := .score_street_type(pairs, weights$street_type)]
  # Number and unit only locate an address within the street that was asked for.
  gate <- if (isTRUE(street_gate)) .street_gate(pairs$in_street_name, pairs$street_name) else 1
  pairs[, score_number := .score_number(pairs, weights$number, gate)]
  pairs[, score_flat := .score_flat(pairs, weights$flat, gate)]

  # --- Total ---------------------------------------------------------------
  pairs[, total_score := score_postcode + score_suburb + score_street_name +
                         score_street_type + score_number + score_flat]

  pairs
}
