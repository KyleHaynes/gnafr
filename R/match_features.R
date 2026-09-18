#' Extract address agreement, conflict and text features for review or modelling
#'
#' Adds evidence to a copy of [gnaf_match()] results without changing scores or
#' ranks. The six `agreement_*` columns recompute component credit on a 0-1
#' scale using equal 100-point component weights, independently of the caller's
#' ranking weights. `raw_*` and `standardised_*` columns contain the three
#' whole-address text similarities on a 0-100 scale, plus their combined scores.
#'
#' Each identifier has separate `*_conflict`, `*_input_missing` and
#' `*_candidate_missing` columns. Conflicts require evidence on both sides;
#' absence is not agreement. Street-number conflicts mean disjoint or invalid
#' intervals; compatible ranges are not conflicts. Number suffixes use the
#' candidate label, as in matching. Lots are always inspected separately.
#'
#' `has_identifier_conflict` covers street number/suffix, lot, unit/level
#' numbers and types, street type/direction and state. It is a review flag, not
#' a probability or proof that a candidate is wrong. `score_gap` is the gap to
#' the best other returned PID for the same input; it is `NA` when no other
#' candidate was returned. It cannot detect alternatives excluded by retrieval,
#' `min_score` or `max_results`. `tied_best` also refers only to returned PIDs.
#' Use `max_results > 1` to expose alternatives.
#'
#' Linked returns use the original `matched_*` address fields. Unmatched rows
#' retain their input, but all added evidence fields are `NA`. A high agreement
#' score (including 100) is not a calibrated probability of correctness.
#'
#' @param x A `data.table` returned by [gnaf_match()]. Input IDs must identify
#'   a single matching call; make them unique before combining separate calls.
#' @return A copy of `x` with numeric agreement/text/gap features and logical
#'   conflict, missing-evidence and tie indicators appended.
#' @examples
#' \dontrun{
#' candidates <- gnaf_match(addresses, con, max_results = 5, cache = FALSE)
#' evidence <- gnaf_match_features(candidates)
#' evidence[has_identifier_conflict == TRUE | tied_best == TRUE]
#' }
#' @export
gnaf_match_features <- function(x) {
  if (!data.table::is.data.table(x)) stop("'x' must be a data.table returned by gnaf_match()")
  required <- c("input_id", "input_raw", "input_standardised", "matched",
    "address_detail_pid", "address_label", "total_score", "in_postcode",
    "in_locality", "in_street_name", "in_street_type", "in_number_first",
    "in_flat_number", "postcode", "locality_name", "street_name", "street_type",
    "number_first", "number_last", "flat_number")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("'x' is missing columns: ", paste(missing, collapse = ", "))
  out <- data.table::copy(x)
  pairs <- data.table::copy(x)
  candidate_fields <- sub("^matched_", "", grep("^matched_", names(pairs), value = TRUE))
  for (field in intersect(candidate_fields, names(pairs))) {
    data.table::set(pairs, j = field, value = pairs[[paste0("matched_", field)]])
  }
  matched <- pairs$matched %in% TRUE
  added <- character()
  add <- function(name, value) {
    value[!matched] <- NA
    data.table::set(out, j = name, value = value)
    added <<- c(added, name)
  }

  # Recompute fixed features so changing retrieval weights cannot silently
  # change a fitted model's predictor scale. Do not overwrite ranking scores.
  component_names <- paste0("score_", names(.WEIGHTS))
  scored <- .score_pairs(pairs, stats::setNames(rep(list(100), length(.WEIGHTS)), names(.WEIGHTS)))
  for (name in component_names) add(sub("^score_", "agreement_", name), scored[[name]] / 100)
  text_names <- c("jarowinkler_score", "jaccard_score", "levenshtein_score", "text_similarity")
  for (source in c("raw", "standardised")) {
    text <- gnaf_text_scores(x, input = source)
    for (name in text_names) add(paste(source, name, sep = "_"), text[[name]])
  }

  value <- function(name) .score_identifier_value(.pair_column(pairs, name))
  compare <- function(name, input, candidate, conflict = NULL) {
    input_missing <- is.na(input) | input == ""
    candidate_missing <- is.na(candidate) | candidate == ""
    if (is.null(conflict)) conflict <- input != candidate
    add(paste0(name, "input_missing"), input_missing)
    add(paste0(name, "candidate_missing"), candidate_missing)
    add(paste0(name, "conflict"), !input_missing & !candidate_missing & conflict)
  }
  token <- .candidate_number_token(pairs)
  first <- pairs$number_first
  first[is.na(first)] <- suppressWarnings(as.integer(sub("[^0-9].*$", "", token[is.na(first)])))
  last <- pairs$number_last
  last[is.na(last)] <- first[is.na(last)]
  input_first <- pairs$in_number_first
  input_last <- .pair_column(pairs, "in_number_last", NA_integer_)
  input_last[is.na(input_last)] <- input_first[is.na(input_last)]
  compare("number_", input_first, first,
    input_last < input_first | last < first | input_first > last | first > input_last)
  compare("number_suffix_", value("in_number_suffix"),
    .score_value(sub("^[0-9]+([A-Z]?).*$", "\\1", token)))
  for (field in c("lot_number", "flat_number", "level_number", "state")) {
    compare(paste0(field, "_"), value(paste0("in_", field)), value(field))
  }
  for (field in c("flat_type", "level_type", "street_type", "street_suffix")) {
    map <- switch(field, flat_type = .get_score_flat_type_map(),
      level_type = .get_score_level_type_map(), street_type = .get_street_type_map(),
      street_suffix = .SCORE_DIRECTIONS)
    compare(paste0(field, "_"),
      .score_mapped_value(.pair_column(pairs, paste0("in_", field)), map),
      .score_mapped_value(.pair_column(pairs, field), map))
  }
  conflict_cols <- grep("_conflict$", added, value = TRUE)
  add("has_identifier_conflict", rowSums(out[, conflict_cols, with = FALSE], na.rm = TRUE) > 0L)
  for (field in c("postcode", "locality", "street_name")) {
    candidate <- if (field == "locality") "locality_name" else field
    compare(paste0(field, "_"), value(paste0("in_", field)), value(candidate))
  }

  # Collapse linked/duplicate representations of a PID before measuring ties.
  candidates <- data.table::data.table(input_id = x$input_id,
    pid = pairs$address_detail_pid, score = x$total_score)
  candidates <- unique(candidates[matched & !is.na(score)], by = c("input_id", "pid"))
  summary <- candidates[, {
    ordered <- sort(as.numeric(score), decreasing = TRUE)
    list(best = ordered[1L], runner_up = if (.N > 1L) ordered[2L] else NA_real_,
         n_best = sum(score == ordered[1L]))
  }, by = input_id]
  summary <- summary[match(x$input_id, summary$input_id)]
  other <- ifelse(x$total_score == summary$best, summary$runner_up, summary$best)
  add("score_gap", as.numeric(x$total_score - other))
  add("tied_best", summary$n_best > 1L & x$total_score == summary$best)
  out[]
}
