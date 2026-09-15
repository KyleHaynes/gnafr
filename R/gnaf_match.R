#' Match a vector of address strings against the GNAF database
#'
#' Uses a five-path strategy:
#' \enumerate{
#'   \item \strong{Postcode path} - primary, blocks on the parsed postcode.
#'   \item \strong{State path} - for inputs with no parseable postcode.
#'   \item \strong{Locality fallback} - for inputs whose best postcode result is
#'         weak (score below \code{fallback_threshold}). Uses DuckDB's built-in
#'         \code{jaro_winkler_similarity} to find the correct postcode from the
#'         parsed suburb name, then re-scores. Handles wrong or missing postcodes.
#'   \item \strong{Street-number-relaxed fallback} - for inputs whose best result
#'         so far is weak specifically because of a poor or absent number match
#'         (score below \code{fallback_threshold} with a near-zero number score,
#'         or no result at all). Every path above blocks candidates on the
#'         parsed number before street name is ever scored, so a real row on the
#'         correct street at a *different* number is invisible to them no matter
#'         how strong the rest of the match would be. This path drops the number
#'         constraint and joins on street name instead, letting the correct
#'         street (with an honestly low number score) compete on its merits
#'         against whatever else happened to satisfy the number filter.
#'   \item \strong{Street-only fallback} - optional; fires for inputs that are
#'         still unmatched after all other paths. Matches against street-level
#'         aliases built by \code{gnaf_build_street_aliases}. Useful when a
#'         specific street number is absent from GNAF but the street itself
#'         exists and \code{street_number_fallback} was disabled or didn't find it
#'         (e.g. the street name itself needs fuzzy resolution).
#' }
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param addresses Character vector of address strings.
#' @param max_results Maximum number of matches to return per input.
#' @param min_score Minimum total score (0-100) to include in results.
#' @param include_custom Include custom addresses in matching.
#' @param include_aliases If \code{FALSE}, a friendly shortcut for excluding
#'   every alias/synonym row (locality, street, address and street-only
#'   aliases) from matching — equivalent to \code{alias_types = NA}. Default
#'   \code{TRUE}. Cannot be combined with an explicit \code{alias_types}
#'   (throws an error) — use \code{alias_types} directly for finer control.
#' @param alias_types Character vector of \code{alias_type} values to include
#'   in matching. Use \code{NA} to include core GNAF rows (where
#'   \code{alias_type} is \code{NULL}). Default \code{NULL} matches all rows
#'   regardless of alias type or match strength; aliases compete with core
#'   addresses before ranking. Example: \code{c(NA, "street_only")} restricts
#'   to core addresses and street-level aliases only.
#' @param resolve_principal If \code{TRUE}, results that are an alias
#'   (\code{alias_type} is not \code{NA} and \code{principal_pid} is set) get
#'   extra \code{principal_address_label}, \code{principal_longitude},
#'   \code{principal_latitude}, \code{principal_locality_name} and
#'   \code{principal_postcode} columns, resolved from the real/canonical GNAF
#'   record the alias was derived from. \code{NA} for non-alias rows and for
#'   aliases with no \code{principal_pid} (e.g. \code{street_only}). Default
#'   \code{FALSE}.
#' @param return_principal If \code{TRUE}, replace an alias match's address
#'   fields with the full non-alias record linked by \code{principal_pid}.
#'   Default \code{FALSE}. Unlike \code{resolve_principal}, this changes the
#'   returned \code{address_detail_pid}, label, components and coordinates.
#' @param return_primary If \code{TRUE}, replace a secondary match's address
#'   fields with the full primary record linked by \code{primary_pid}.
#'   Default \code{FALSE}. When both return options are enabled, resolve the
#'   principal first, then its primary address. Both options apply regardless
#'   of match strength or \code{fallback_threshold}.
#' @param geographies Registered geography names, e.g. \code{"sa2_2021"}, or \code{TRUE}
#'   for all available layers. Default \code{NULL} adds none. Attributes are appended
#'   for the final returned PID/source, after principal/primary resolution.
#'   Missing assignments remain \code{NA}; match order, ranks and scores are retained.
#'   See \code{\link{gnaf_list_geographies}}, \code{\link{gnaf_add_geography}} and
#'   \code{\link{gnaf_join_geographies}}. Column-name collisions are errors.
#' @param locality_fallback If \code{TRUE} (default), re-searches by locality
#'   name for unmatched inputs and results below \code{fallback_threshold}
#'   whose locality component is weak.
#' @param street_number_fallback If \code{TRUE} (default), re-searches by
#'   street name (dropping the number-based candidate filter every other path
#'   uses) for unmatched inputs and results below \code{fallback_threshold}
#'   whose number component is weak - i.e. the parsed street exists, just not
#'   at the requested number. See Details.
#' @param street_only_fallback If \code{TRUE}, inputs still unmatched after all
#'   other paths are re-matched against street-level aliases
#'   (\code{alias_type = "street_only"}) in \code{gnaf_addresses}. Requires
#'   \code{gnaf_build_street_aliases} to have been run first. Default
#'   \code{FALSE}.
#' @param fallback_threshold Total score at or below which a weak-locality
#'   result is eligible for locality fallback. Default 90: a clean correct
#'   match typically scores 95+, whereas a
#'   coincidental match - same postcode and house number, unrelated street -
#'   can still reach into the low-to-mid 80s (e.g. 20 postcode + 10 number +
#'   partial suburb/street-name credit). 90 leaves enough margin to catch those
#'   coincidences and let the locality scan (which discovers the right postcode
#'   from the suburb name regardless of how far off the stated one is) find the
#'   true candidate, without firing for genuinely good matches.
#' @param weights Named list of score weights. Defaults to postcode = 20,
#'   suburb = 15, street_name = 40, street_type = 10, number = 10, flat = 5.
#'   Weights must sum to 100.
#' @details Street numbers compare both range endpoints: exact intervals earn
#'   full number weight, a candidate containing the input earns 70%, a candidate
#'   contained by the input earns 50%, and partial overlap earns 30%. Number
#'   suffixes are checked immediately before the candidate street in its label,
#'   including labels with unit, level or building prefixes.
#'
#'   Street directions qualify the street-type score: agreement keeps full
#'   credit, a missing direction halves it, and conflicting directions score
#'   zero. Unit and level identifiers contribute independently to the flat
#'   score (60% unit, 40% level when both dimensions are present). Missing
#'   identifiers earn half credit; conflicting identifiers earn none. UNIT,
#'   APARTMENT and FLAT are equivalent designators, as are LEVEL and FLOOR.
#'   These scores measure agreement; they are not calibrated probabilities.
#'   Candidate pruning uses the requested weights and minimum score: a street
#'   comparison is discarded only if its rounded score plus an upper bound on
#'   the other components cannot reach \code{min_score}. Postcode/state and
#'   number/lot blocking still limit which addresses are considered. Database
#'   query failures raise an error, including when \code{verbose = FALSE}.
#'
#'   The return options apply after matching, ranking and cache storage. Scores,
#'   ranks and parsed input fields still describe the original match, recorded
#'   in a complete set of \code{matched_*} address columns whenever either
#'   option is enabled (e.g. \code{matched_address_label},
#'   \code{matched_longitude} and \code{matched_principal_pid}). All returned address fields come from
#'   the linked record, including missing values. Missing or unavailable links
#'   leave the matched address unchanged. Lookups include custom addresses when
#'   \code{include_custom = TRUE}, preferring GNAF if a PID exists in both tables.
#'   Multiple matches resolving to the same address retain their original rows
#'   and ranks. \code{resolve_principal} columns describe the original alias.
#' @param normalize Passed to \code{address_parse}; defaults to \code{TRUE}.
#' @param cache If \code{TRUE} (default), checks \code{gnaf_match_cache} for
#'   previously matched addresses and stores new high-confidence results. The
#'   one-result cache requires default weights, normalisation, candidate filters
#'   and fallback settings, and is bypassed when \code{max_results > 1}.
#' @param cache_threshold Minimum score for a new result to be cached.
#'   Default 95.
#' @param verbose If \code{TRUE}, prints colored progress, timings, and match
#'   summary information using the \pkg{cli} package.
#' @return A \code{data.table} ordered by \code{input_id} then descending
#'   \code{total_score}. Includes a standardised input string for every row and
#'   retains unmatched inputs with missing match columns.
#' @export
gnaf_match <- function(addresses, con, max_results = 1L, min_score = 60L,
                       include_custom = TRUE,
                       include_aliases = TRUE,
                       alias_types = NULL,
                       resolve_principal = FALSE,
                       locality_fallback = TRUE,
                       street_number_fallback = TRUE,
                       street_only_fallback = FALSE,
                       fallback_threshold = 90L,
                       weights = .default_match_weights(),
                       normalize = TRUE,
                       cache = TRUE,
                       cache_threshold = 95L,
                       verbose = TRUE,
                       return_principal = FALSE,
                       return_primary = FALSE,
                       geographies = NULL) {

  legacy_order <- tryCatch(DBI::dbIsValid(addresses), error = function(e) FALSE) &&
    is.character(con)
  if (legacy_order) {
    warning(
      "`gnaf_match(con, addresses)` is deprecated; use `gnaf_match(addresses, con)`.",
      call. = FALSE
    )
    tmp <- addresses
    addresses <- con
    con <- tmp
  }

  if (!is.character(addresses) || length(addresses) == 0L)
    stop("'addresses' must be a non-empty character vector")

  max_results <- .as_positive_integer(max_results, "max_results")
  if (length(min_score) != 1L || is.na(min_score) || !is.numeric(min_score) ||
      min_score < 0 || min_score > 100) {
    stop("'min_score' must be one number between 0 and 100", call. = FALSE)
  }
  min_score <- as.integer(min_score)

  for (arg in c("fallback_threshold", "cache_threshold")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 0 || value > 100)
      stop("'", arg, "' must be one number between 0 and 100", call. = FALSE)
  }
  for (arg in c("include_custom", "include_aliases", "resolve_principal",
                "return_principal", "return_primary",
                "locality_fallback", "street_number_fallback", "street_only_fallback",
                "normalize", "cache", "verbose")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop("'", arg, "' must be TRUE or FALSE", call. = FALSE)
  }

  weights <- .validate_match_weights(weights)
  geography_specs <- .geography_specs(con, geographies)

  if (!isTRUE(include_aliases)) {
    if (!is.null(alias_types))
      stop("'include_aliases = FALSE' cannot be combined with an explicit ",
           "'alias_types'; pass alias_types = NA directly instead, or ",
           "leave include_aliases at its default (TRUE).", call. = FALSE)
    alias_types <- NA_character_
  }

  # Cached scores were computed with the default weights; serving or storing
  # them under different weights would silently mis-score, so the cache is
  # bypassed for the whole call when non-default weights are supplied.
  cache_schema_current <- DBI::dbExistsTable(con, "gnaf_match_cache") &&
    "algorithm_version" %in% DBI::dbListFields(con, "gnaf_match_cache")
  cache_usable <- isTRUE(cache) && cache_schema_current &&
    max_results == 1L &&
    isTRUE(include_custom) && is.null(alias_types) && isTRUE(normalize) &&
    isTRUE(locality_fallback) && isTRUE(street_number_fallback) &&
    !isTRUE(street_only_fallback) &&
    identical(as.numeric(fallback_threshold), 90) &&
    identical(weights, .validate_match_weights(.default_match_weights()))

  total_timer <- proc.time()[["elapsed"]]
  address_count <- length(addresses)
  address_word <- if (address_count == 1L) "address" else "addresses"
  .cli_match_step(
    verbose,
    sprintf(
      "Parsing %s %s.",
      cli::col_blue(format(address_count, big.mark = ",")),
      address_word
    )
  )
  parse_timer <- proc.time()[["elapsed"]]
  parsed <- address_parse(addresses, normalize = normalize)
  parse_elapsed <- proc.time()[["elapsed"]] - parse_timer

  # address_parse() has no database access, so when a comma-less address's
  # trailing street-type word is also a real locality word (e.g. "Point
  # Lookout" - LOOKOUT is a legitimate street type), it can only guess from a
  # fixed word list and sometimes loses the locality entirely. Now that a
  # real connection exists, recover it against the actual locality index.
  # Gated behind locality_fallback since it's the same kind of DB-assisted
  # locality guessing that flag already controls.
  if (isTRUE(locality_fallback)) .recover_missing_locality(con, parsed)

  .cli_match_step(verbose, "Standardising parsed input addresses.")
  standardise_timer <- proc.time()[["elapsed"]]
  parsed[, input_standardised := .standardise_input(parsed)]
  standardise_elapsed <- proc.time()[["elapsed"]] - standardise_timer
  .cli_match_detail(verbose, sprintf(
    "Input standardisation completed in %s.",
    cli::col_cyan(sprintf("%.2fs", standardise_elapsed))
  ))

  verbose_stats <- list(
    parse_elapsed = parse_elapsed,
    standardise_elapsed = standardise_elapsed,
    exact_inputs = 0L,
    exact_elapsed = 0,
    cache_inputs = 0L,
    cache_elapsed = 0,
    slow_inputs = 0L,
    slow_elapsed = 0,
    wrangle_elapsed = 0
  )

  results    <- list()
  diagnostics <- list()
  skip_ids   <- integer(0L)

  # ------------------------------------------------------------------
  # Fast path 1: exact address_label match
  # Fires when input_raw (uppercased) equals a GNAF address_label exactly.
  # Ideal for re-processing previously matched/standardised output.
  # ------------------------------------------------------------------
  exact_timer <- proc.time()[["elapsed"]]
  raw_is_standard <- toupper(trimws(parsed$input_raw)) == parsed$input_standardised
  use_exact_label_path <- nrow(parsed) <= 100L ||
    mean(raw_is_standard, na.rm = TRUE) >= 0.05
  exact_path <- if (isTRUE(use_exact_label_path)) {
    .exact_label_match(
      con, parsed, include_custom, alias_types, weights, min_score
    )
  } else {
    NULL
  }
  verbose_stats$exact_elapsed <- proc.time()[["elapsed"]] - exact_timer
  if (!is.null(exact_path) && nrow(exact_path) > 0L) {
    results[["exact"]] <- exact_path
    # An exact label can still score poorly, or leave requested alternatives
    # unfilled. Those inputs must continue through component matching.
    skip_ids <- exact_path[, .(complete = sum(total_score == 100L) >= max_results),
                            by = input_id][complete == TRUE, input_id]
    verbose_stats$exact_inputs <- uniqueN(exact_path$input_id)
    .cli_match_detail(verbose, sprintf(
      "%s input(s) matched via exact label lookup in %s.",
      cli::col_green(format(length(skip_ids), big.mark = ",")),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$exact_elapsed))
    ))
  } else {
    .cli_match_detail(verbose, sprintf(
      "%s input(s) matched via exact label lookup in %s.",
      cli::col_green("0"),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$exact_elapsed))
    ))
  }

  # ------------------------------------------------------------------
  # Fast path 2: match cache
  # Previously matched addresses above cache_threshold skip the full pipeline.
  # ------------------------------------------------------------------
  cache_timer <- proc.time()[["elapsed"]]
  if (isTRUE(cache) && !cache_usable)
    .cli_match_detail(
      verbose,
      "Match cache bypassed: it requires a current schema, default matching settings and max_results = 1."
    )
  if (cache_usable && DBI::dbExistsTable(con, "gnaf_match_cache")) {
    remaining_stds <- unique(stats::na.omit(
      parsed[!input_id %in% skip_ids, input_standardised]
    ))
    if (length(remaining_stds) > 0L) {
      cache_raw <- .cache_lookup(
        con, remaining_stds, include_custom, alias_types, min_score
      )
      if (nrow(cache_raw) > 0L) {
        cache_hits <- cache_raw[
          parsed[!input_id %in% skip_ids, .(input_id, input_standardised)],
          on = "input_standardised", nomatch = 0L
        ]
        if (nrow(cache_hits) > 0L) {
          cache_hits[, match_rank := 1L]
          results[["cache"]] <- cache_hits
          verbose_stats$cache_inputs <- uniqueN(cache_hits$input_id)
          skip_ids <- unique(c(skip_ids, cache_hits$input_id))
        }
      }
    }
  }
  verbose_stats$cache_elapsed <- proc.time()[["elapsed"]] - cache_timer
  .cli_match_detail(verbose, sprintf(
    "%s input(s) served from match cache in %s.",
    cli::col_green(format(verbose_stats$cache_inputs, big.mark = ",")),
    cli::col_cyan(sprintf("%.2fs", verbose_stats$cache_elapsed))
  ))

  deduplicated <- .deduplicate_match_inputs(
    parsed[!input_id %in% skip_ids]
  )
  match_inputs <- deduplicated$inputs
  input_fanout <- deduplicated$fanout
  has_pc <- match_inputs[!is.na(in_postcode)]
  no_pc  <- match_inputs[is.na(in_postcode)]
  slow_timer <- proc.time()[["elapsed"]]

  # ------------------------------------------------------------------
  # Path 1: postcode path - scored entirely in DuckDB
  # ------------------------------------------------------------------
  if (nrow(has_pc) > 0L) {
    n_pc <- uniqueN(has_pc$in_postcode)
    .cli_match_step(verbose, sprintf(
      "Scoring %s input(s) across %s unique postcode(s) in DuckDB.",
      cli::col_blue(format(nrow(has_pc), big.mark = ",")),
      cli::col_cyan(format(n_pc, big.mark = ","))
    ))
    # A separate exact-component pass is faster for interactive-sized calls,
    # but becomes a second full table scan for large registered input batches.
    # Large batches use exact-number, range, lot, suffix, and missing-number
    # branches inside one postcode query instead.
    use_separate_exact_path <- nrow(has_pc) <= 100L
    exact_components <- if (use_separate_exact_path) {
      .match_exact_components_duckdb(
        con, has_pc, max_results, min_score, weights, include_custom,
        verbose, alias_types
      )
    } else {
      .empty_path_result()
    }
    strong_ids <- if (!is.null(exact_components$matches) &&
                      nrow(exact_components$matches) > 0L) {
      exact_components$matches[
        , .(perfect_matches = sum(total_score == 100L)), by = input_id
      ][perfect_matches >= max_results, input_id]
    } else {
      integer()
    }
    fuzzy_inputs <- if (use_separate_exact_path) {
      has_pc[!input_id %in% strong_ids]
    } else {
      has_pc
    }
    fuzzy_components <- .match_postcode_duckdb(
      con, fuzzy_inputs, max_results, min_score, weights, include_custom,
      verbose, alias_types
    )
    pc_path <- .combine_path_results(
      exact_components, fuzzy_components, max_results
    )
    results[["postcode"]]     <- pc_path$matches
    diagnostics[["postcode"]] <- pc_path$diagnostics
  }

  # ------------------------------------------------------------------
  # Path 2: no-postcode inputs - state-level fallback in DuckDB
  # ------------------------------------------------------------------
  if (nrow(no_pc) > 0L) {
    no_pc_state <- no_pc[!is.na(in_state)]
    if (nrow(no_pc_state) > 0L) {
      input_word <- if (nrow(no_pc_state) == 1L) "row" else "rows"
      .cli_match_step(verbose, sprintf(
        "Attempting state fallback for %s input %s without a postcode.",
        cli::col_yellow(format(nrow(no_pc_state), big.mark = ",")),
        input_word
      ))
      st_path <- .match_state_duckdb(con, no_pc_state, max_results, min_score,
                                      weights, include_custom, verbose,
                                      alias_types)
      results[["no_postcode"]]     <- st_path$matches
      diagnostics[["no_postcode"]] <- st_path$diagnostics
    } else {
      .cli_match_alert(verbose, "warning",
                       "No postcode or state found for some inputs; skipping.")
    }
  }

  # ------------------------------------------------------------------
  # Path 3: locality fallback for weak / wrong-postcode results
  # ------------------------------------------------------------------
  if (locality_fallback) {
    pc_res <- results[["postcode"]]

    best_by_input <- if (!is.null(pc_res) && nrow(pc_res) > 0L) {
      ordered_pc <- copy(pc_res)
      setorder(ordered_pc, input_id, -total_score, address_detail_pid)
      ordered_pc[, .SD[1L], by = input_id]
    } else {
      data.table(
        input_id = integer(), total_score = integer(),
        score_suburb = integer()
      )
    }

    weak_ids <- best_by_input[
      total_score <= fallback_threshold & score_suburb < round(weights$suburb * 0.85),
      input_id
    ]
    matched_ids   <- best_by_input$input_id
    unmatched_ids <- has_pc[!input_id %in% matched_ids, input_id]
    # The state path already scored every eligible address in that state.
    # A locality-to-postcode retry can only repeat a subset of those candidates.
    no_pc_loc_ids <- no_pc[is.na(in_state) & !is.na(in_locality), input_id]

    fallback_ids   <- unique(c(weak_ids, unmatched_ids, no_pc_loc_ids))
    fallback_parse <- match_inputs[
      input_id %in% fallback_ids & !is.na(in_locality)
    ]

    if (nrow(fallback_parse) > 0L) {
      fallback_word <- if (nrow(fallback_parse) == 1L) "row" else "rows"
      .cli_match_step(verbose, sprintf(
        "Running locality fallback for %s input %s.",
        cli::col_magenta(format(nrow(fallback_parse), big.mark = ",")),
        fallback_word
      ))
      loc_path <- .match_locality_duckdb(con, fallback_parse, max_results,
                                          min_score, weights, include_custom,
                                          verbose, alias_types)
      results[["locality"]]     <- loc_path$matches
      diagnostics[["locality"]] <- loc_path$diagnostics
    }
  }

  # ------------------------------------------------------------------
  # Path 4: street-number-relaxed fallback. The number pre-filter shared by
  # every path above (.number_prefilter_sql()) excludes a candidate from the
  # SQL join - before street name is ever scored - whenever its number
  # doesn't match or overlap the input's. A real row on the correct street at
  # a *different* number is therefore invisible to every path above, no
  # matter how strong the street/suburb/postcode match would otherwise be.
  # This path drops the number constraint and joins on street_name instead,
  # for inputs whose best result so far is weak - letting the already-correct
  # scoring formula decide whether the real street (right name, honestly-low
  # number score) beats whatever else happened to satisfy the number filter.
  #
  # Deliberately not conditioned on the current best candidate's own
  # score_number: the winning candidate under the old number-first filter
  # necessarily has a matching/overlapping number (that's how it got
  # through), so its score_number is often already full even when its street
  # is wrong - checking it would almost never catch the exact bug this path
  # exists to fix. A plain weak-total-score bar (mirroring locality
  # fallback's) lets the street-name search itself decide whether re-running
  # helps; if the parsed street has no better row anywhere, it simply finds
  # nothing and the existing result stands.
  # ------------------------------------------------------------------
  if (isTRUE(street_number_fallback)) {
    combined_so_far <- rbindlist(
      Filter(function(r) !is.null(r) && nrow(r) > 0L,
             results[c("postcode", "no_postcode", "locality")]),
      fill = TRUE, use.names = TRUE
    )
    best_overall <- if (nrow(combined_so_far) > 0L) {
      ordered <- copy(combined_so_far)
      setorder(ordered, input_id, -total_score, address_detail_pid)
      ordered[, .SD[1L], by = input_id]
    } else {
      data.table(input_id = integer(), total_score = integer())
    }
    weak_ids <- best_overall[total_score <= fallback_threshold, input_id]
    zero_ids <- match_inputs[!input_id %in% best_overall$input_id, input_id]
    snr_ids <- unique(c(weak_ids, zero_ids))
    snr_parse <- match_inputs[
      input_id %in% snr_ids & !is.na(in_street_name) &
      (!is.na(in_postcode) | !is.na(in_state))
    ]
    if (nrow(snr_parse) > 0L) {
      .cli_match_step(verbose, sprintf(
        "Running street-number-relaxed fallback for %s input(s).",
        cli::col_magenta(format(nrow(snr_parse), big.mark = ","))
      ))
      snr_path <- .match_street_number_relaxed_duckdb(
        con, snr_parse, max_results, min_score, weights, include_custom,
        verbose, alias_types
      )
      results[["street_number_relaxed"]]     <- snr_path$matches
      diagnostics[["street_number_relaxed"]] <- snr_path$diagnostics
    }
  }

  # ------------------------------------------------------------------
  # Path 5: street-only fallback for inputs still unmatched after all paths
  # ------------------------------------------------------------------
  if (isTRUE(street_only_fallback) &&
      (is.null(alias_types) || "street_only" %in% alias_types)) {
    matched_so_far <- unique(unlist(lapply(
      results,
      function(r) if (!is.null(r) && nrow(r) > 0L) r$input_id else integer(0L)
    )))
    so_parse <- match_inputs[
      !input_id %in% matched_so_far &
      !is.na(in_postcode) &
      !is.na(in_street_name)
    ]
    if (nrow(so_parse) > 0L) {
      .cli_match_step(verbose, sprintf(
        "Running street-only fallback for %s input(s).",
        cli::col_magenta(format(nrow(so_parse), big.mark = ","))
      ))
      so_path <- .match_street_only_duckdb(
        con, so_parse, max_results, min_score, weights, verbose
      )
      results[["street_only"]]     <- so_path$matches
      diagnostics[["street_only"]] <- so_path$diagnostics
    }
  }

  verbose_stats$slow_elapsed <- proc.time()[["elapsed"]] - slow_timer

  # ------------------------------------------------------------------
  # Combine paths, deduplicate, re-rank
  # ------------------------------------------------------------------
  .cli_match_step(verbose, "Wrangling final match output.")
  wrangle_timer <- proc.time()[["elapsed"]]
  slow_names <- setdiff(names(results), c("exact", "cache"))
  for (nm in slow_names) {
    results[[nm]] <- .fanout_match_rows(results[[nm]], input_fanout)
  }
  for (nm in names(diagnostics)) {
    diagnostics[[nm]] <- .fanout_match_rows(diagnostics[[nm]], input_fanout)
  }
  out <- rbindlist(results, fill = TRUE, use.names = TRUE)
  if (nrow(out) > 0L) {
    # Deduplicate: same GNAF record may appear from multiple paths; the order
    # puts the higher-scoring duplicate first so unique() keeps it.
    setorder(out, input_id, -total_score, address_detail_pid)
    out <- unique(out, by = c("input_id", "address_detail_pid"))

    # Re-apply max_results and assign final rank
    out <- out[out[, .I[seq_len(min(.N, max_results))], by = input_id]$V1]
    out[, match_rank := seq_len(.N), by = input_id]
  } else {
    out <- .empty_result()
  }

  common_cols <- setdiff(intersect(names(out), names(parsed)), "input_id")
  if (length(common_cols) > 0L) out[, (common_cols) := NULL]

  out <- merge(parsed, out, by = "input_id", all.x = TRUE, sort = FALSE)
  out[, matched := !is.na(address_detail_pid)]
  out <- .append_match_status(out, parsed, diagnostics)

  cols_first <- c("input_id", "input_raw", "input_standardised", "address_label", "match_rank",
                  "matched", "match_status", "total_score", "score_postcode", "score_suburb",
                  "score_street_name", "score_street_type", "score_number",
                  "score_flat")
  setcolorder(out, c(cols_first, setdiff(names(out), cols_first)))
  setorder(out, input_id, -matched, match_rank)
  # Store newly matched high-confidence results in the cache.
  if (cache_usable && is.null(alias_types) &&
      DBI::dbExistsTable(con, "gnaf_match_cache") && nrow(out) > 0L)
    .cache_store(con, out[matched == TRUE & !input_id %in% results[["cache"]]$input_id],
                 cache_threshold)

  verbose_stats$wrangle_elapsed <- proc.time()[["elapsed"]] - wrangle_timer
  slow_path_matches <- out[
    matched == TRUE & !input_id %in% unique(c(
      results[["exact"]]$input_id %||% integer(0L),
      results[["cache"]]$input_id %||% integer(0L)
    )),
    uniqueN(input_id)
  ]
  verbose_stats$slow_inputs <- slow_path_matches
  .cli_match_detail(verbose, sprintf(
    "Final output wrangling completed in %s.",
    cli::col_cyan(sprintf("%.2fs", verbose_stats$wrangle_elapsed))
  ))

  if (isTRUE(resolve_principal)) out <- .resolve_principal(con, out)
  if (return_principal || return_primary) {
    # Snapshot the complete candidate before following either relationship.
    # Scores/ranks still belong to this candidate, including on cache hits.
    address_fields <- trimws(strsplit(gsub("g\\.", "", .GNAF_SELECT_COLS),
                                     ",", fixed = TRUE)[[1L]])
    for (field in address_fields)
      set(out, j = paste0("matched_", field), value = out[[field]])
    if (return_principal)
      out <- .return_linked_address(con, out, "principal_pid", include_custom)
    if (return_primary)
      out <- .return_linked_address(con, out, "primary_pid", include_custom)
  }

  if (nrow(geography_specs)) out <- .join_geography_specs(out, con, geography_specs)

  .cli_match_summary(verbose, parsed, out, total_timer, verbose_stats)
  out[]
}

# Replace the complete address record so labels, components and geocodes never
# mix fields from the original match and its linked address. Cache entries retain
# the original candidate; returning linked records is specific to this call.
.return_linked_address <- function(con, out, link_column, include_custom) {
  pids <- out[[link_column]]
  rows <- which(out$matched & !is.na(pids) & nzchar(trimws(pids)))
  if (length(rows) == 0L) return(out)

  links <- data.table(address_detail_pid = unique(pids[rows]))
  duckdb::duckdb_register(con, "__gnafr_return_links__", links, overwrite = TRUE)
  on.exit(duckdb::duckdb_unregister(con, "__gnafr_return_links__"), add = TRUE)

  tables <- "gnaf_addresses"
  if (include_custom && DBI::dbExistsTable(con, "custom_addresses"))
    tables <- c(tables, "custom_addresses")
  lookup <- rbindlist(lapply(tables, function(table) {
    setDT(DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM %s g
       JOIN __gnafr_return_links__ l USING (address_detail_pid)",
      .GNAF_SELECT_COLS, table
    )))
  }), use.names = TRUE)
  lookup <- unique(lookup, by = "address_detail_pid")
  target <- match(pids[rows], lookup$address_detail_pid)
  found <- !is.na(target)
  if (!any(found)) return(out)

  for (col in names(lookup))
    set(out, i = rows[found], j = col, value = lookup[[col]][target[found]])
  out
}

# Adds principal_address_label / principal_longitude / principal_latitude /
# principal_locality_name / principal_postcode columns, resolved from
# gnaf_addresses via each alias row's principal_pid. NA for non-alias rows and
# for aliases with no principal_pid (e.g. street_only).
.resolve_principal <- function(con, out) {
  cols <- c("principal_address_label", "principal_longitude",
            "principal_latitude", "principal_locality_name",
            "principal_postcode")
  out[, (cols) := list(NA_character_, NA_real_, NA_real_, NA_character_, NA_integer_)]

  if (nrow(out) > 0L && "principal_pid" %in% names(out)) {
    pids <- unique(out$principal_pid[!is.na(out$principal_pid)])
    if (length(pids) > 0L) {
      quoted <- paste0("'", gsub("'", "''", pids), "'", collapse = ", ")
      lookup <- setDT(DBI::dbGetQuery(con, sprintf("
        SELECT address_detail_pid AS principal_pid,
               address_label      AS principal_address_label,
               longitude          AS principal_longitude,
               latitude           AS principal_latitude,
               locality_name      AS principal_locality_name,
               postcode           AS principal_postcode
        FROM gnaf_addresses
        WHERE address_detail_pid IN (%s)
      ", quoted)))
      out[lookup, on = "principal_pid", `:=`(
        principal_address_label = i.principal_address_label,
        principal_longitude     = i.principal_longitude,
        principal_latitude      = i.principal_latitude,
        principal_locality_name = i.principal_locality_name,
        principal_postcode      = i.principal_postcode
      )]
    }
  }
  out
}

# ---------------------------------------------------------------------------
# DuckDB-based path implementations
# All scoring, joining, filtering and top-N ranking happen inside DuckDB.
# Only the final (small) result set is transferred to R.
# ---------------------------------------------------------------------------

# Candidate columns selected from the GNAF/custom side in every path query.
.GNAF_SELECT_COLS <- "g.address_detail_pid, g.address_label, g.address_site_name,
    g.building_name, g.flat_type, g.flat_number, g.level_type, g.level_number,
    g.number_first, g.number_last, g.lot_number,
    g.street_name, g.street_type, g.street_suffix, g.locality_name,
    g.state, g.postcode, g.longitude, g.latitude, g.source, g.alias_type,
    g.alias_principal, g.principal_pid, g.primary_secondary, g.primary_pid,
    g.geocode_type, g.date_created, g.legal_parcel_id, g.mb_code"

.table_has_rows <- function(con, table) {
  if (!DBI::dbExistsTable(con, table)) return(FALSE)
  isTRUE(tryCatch(
    DBI::dbGetQuery(
      con,
      sprintf("SELECT EXISTS (SELECT 1 FROM %s LIMIT 1) AS has_rows", table)
    )$has_rows[[1L]],
    error = function(e) FALSE
  ))
}

# Coarse street-number pre-filter shared by the postcode, state and locality
# paths: keep intersecting numeric intervals, or recover a suffixed number from
# the candidate label when its numeric field is missing. Bounds are explicit
# so unrelated ranges do not inflate the candidate set.
.number_prefilter_sql <- function() {
  paste(
    "(",
    sprintf("  (i.in_lot_number IS NOT NULL AND %s = %s)",
      .score_identifier_value_sql("g.lot_number"),
      .score_identifier_value_sql("i.in_lot_number")),
    "  OR (i.in_lot_number IS NULL AND (",
    "    i.in_number_first IS NULL",
    "    OR g.number_first = i.in_number_first",
    "    OR (g.number_first <= COALESCE(i.in_number_last, i.in_number_first)",
    "        AND i.in_number_first <= COALESCE(g.number_last, g.number_first))",
    "    OR (g.number_first IS NULL AND i.in_number_suffix IS NOT NULL",
    sprintf("        AND %s = CAST(i.in_number_first AS VARCHAR) || i.in_number_suffix)",
            .candidate_number_token_sql()),
    "  ))",
    ")"
  )
}

# Core query runner shared by all paths.
# inputs_tbl  : name of a duckdb_register'd virtual table of parsed inputs
# gnaf_tbl    : "gnaf_addresses" or "custom_addresses"
# join_clause : SQL ON expression (uses aliases i = inputs, g = gnaf)
# pre_filter  : additional WHERE predicates (coarse, no JW)
#
# Design notes:
#   * No window-function aggregates (COUNT/MAX OVER) before the score filter -
#     those force full materialisation of the join which kills RAM at scale.
#   * A score upper bound prunes impossible candidates without imposing an
#     unrelated similarity floor on calls with custom weights or low min_score.
#   * candidate_count is set to NA; match_status "below_min_score" vs
#     "no_candidate" is not distinguishable, which is an acceptable trade-off.
.run_duckdb_score_query <- function(con, inputs_tbl, gnaf_tbl, join_clause,
                                    pre_filter, weights, max_results, min_score,
                                    verbose = FALSE, label = "",
                                    split_number = FALSE) {
  exprs <- .score_sql_exprs(
    weights, i = "c", g = "c",
    suburb_similarity = "c.suburb_similarity",
    street_similarity = "c.street_similarity"
  )
  sel_scores <- paste(
    mapply(function(nm, ex) sprintf("    %s AS %s", ex, nm), names(exprs), exprs),
    collapse = ",\n"
  )
  score_total <- paste(names(exprs), collapse = " + ")
  # Postcode agreement is cheap and known already. Use its actual score so a
  # distant fallback postcode cannot borrow points it will never receive.
  # Bound the remaining components conservatively, including fractional weights.
  # Use the actual name expression, including blended metrics, directions and
  # exact/rounding rules. A raw-JW bound could discard a valid improved score.
  other_max <- sum(ceiling(unlist(weights[
    !names(weights) %in% c("postcode", "street_name")
  ])))
  street_bound <- sprintf(
    "(%s) + (%s) + %g >= %d",
    exprs$score_postcode, exprs$score_street_name, other_max, min_score
  )
  raw_projection <- sprintf("SELECT
    g.address_detail_pid, g.address_label,
    g.postcode, g.locality_name, g.street_name, g.street_type, g.street_suffix,
    g.number_first, g.number_last, g.lot_number,
    g.flat_type, g.flat_number, g.level_type, g.level_number,
    i.input_id,
    i.in_postcode, i.in_locality, i.in_street_name, i.in_street_type, i.in_street_suffix,
    i.in_number_first, i.in_number_last, i.in_number_suffix, i.in_lot_number,
    i.in_flat_type, i.in_flat_number, i.in_level_type, i.in_level_number,
    %s AS suburb_similarity,
    %s AS street_similarity",
    .name_similarity_sql("i.in_locality", "g.locality_name"),
    .name_similarity_sql("i.in_street_name", "g.street_name"))
  candidate_sql <- if (split_number) {
    branch <- function(predicate) sprintf(
      "%s
       FROM %s g JOIN %s i ON (%s) AND (%s)
       WHERE %s",
      raw_projection, gnaf_tbl, inputs_tbl, join_clause, predicate, pre_filter
    )
    paste(c(
      branch(paste(
        "i.in_lot_number IS NOT NULL",
        sprintf("AND %s = %s", .score_identifier_value_sql("g.lot_number"),
          .score_identifier_value_sql("i.in_lot_number"))
      )),
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND g.number_first = i.in_number_first"
      )),
      # Ordinary inputs only overlap a different start number when the
      # candidate has a range. This lets DuckDB filter those rows before joining.
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND i.in_number_last IS NULL AND g.number_last IS NOT NULL",
        "AND g.number_first < i.in_number_first",
        "AND i.in_number_first <= g.number_last"
      )),
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND i.in_number_last IS NOT NULL",
        "AND g.number_first != i.in_number_first",
        "AND g.number_first <= COALESCE(i.in_number_last, i.in_number_first)",
        "AND i.in_number_first <= COALESCE(g.number_last, g.number_first)"
      )),
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND i.in_number_suffix IS NOT NULL AND g.number_first IS NULL",
        sprintf("AND %s = CAST(i.in_number_first AS VARCHAR) || i.in_number_suffix",
                .candidate_number_token_sql())
      )),
      branch(
        "i.in_lot_number IS NULL AND i.in_number_first IS NULL"
      )
    ), collapse = "\nUNION ALL\n")
  } else {
    sprintf(
      "%s
       FROM %s g JOIN %s i ON %s
       WHERE %s",
      raw_projection, gnaf_tbl, inputs_tbl, join_clause, pre_filter
    )
  }

  final_scores <- paste0("r.", names(exprs), collapse = ", ")
  sql <- sprintf("
WITH raw_candidates AS (
%s
),
candidates AS (
  SELECT * FROM raw_candidates c
  WHERE %s
),
components AS (
  SELECT address_detail_pid, input_id,
%s
  FROM candidates c
),
scored AS (
  SELECT *, %s AS total_score
  FROM components
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY input_id ORDER BY total_score DESC, address_detail_pid) AS match_rank
  FROM scored
  WHERE total_score >= %d
)
SELECT %s, r.input_id, %s, r.total_score, r.match_rank
FROM ranked r
JOIN %s g ON g.address_detail_pid = r.address_detail_pid
WHERE r.match_rank <= %d
",
    candidate_sql,
    street_bound,
    sel_scores,
    score_total,
    min_score,
    .GNAF_SELECT_COLS, final_scores, gnaf_tbl, max_results
  )

  t0 <- proc.time()[["elapsed"]]
  dt <- tryCatch(
    setDT(DBI::dbGetQuery(con, sql)),
    error = function(e) {
      stop(sprintf("%s query failed: %s", label, conditionMessage(e)),
           call. = FALSE)
    }
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  if (verbose && nzchar(label)) {
    .cli_match_detail(verbose, sprintf(
      "%s: %s row(s) returned in %s.",
      label,
      cli::col_green(format(nrow(dt), big.mark = ",")),
      cli::col_cyan(sprintf("%.2fs", elapsed))
    ))
  }

  if (nrow(dt) == 0L) return(.empty_path_result())

  diag_dt <- dt[, .(
    candidate_count = NA_integer_,
    retained_count  = .N,
    best_score      = max(total_score)
  ), by = input_id]

  list(matches = dt, diagnostics = diag_dt)
}

# Combine results from gnaf_addresses + custom_addresses, re-rank.
.combine_path_results <- function(r1, r2, max_results) {
  matches <- rbindlist(
    Filter(Negate(is.null), list(r1$matches, r2$matches)),
    fill = TRUE, use.names = TRUE
  )
  if (nrow(matches) > 0L) {
    setorder(matches, input_id, -total_score, address_detail_pid)
    # Exact and fuzzy paths can return the same candidate. Remove repeats
    # before applying the limit so they cannot displace distinct alternatives.
    matches <- unique(matches, by = c("input_id", "address_detail_pid"))
    matches <- matches[matches[, .I[seq_len(min(.N, max_results))], by = input_id]$V1]
    matches[, match_rank := seq_len(.N), by = input_id]
  } else {
    matches <- NULL
  }

  diags <- rbindlist(
    Filter(Negate(is.null), list(r1$diagnostics, r2$diagnostics)),
    fill = TRUE, use.names = TRUE
  )
  if (nrow(diags) > 0L) {
    diags <- diags[, .(
      candidate_count = sum(candidate_count, na.rm = TRUE),
      retained_count  = sum(retained_count,  na.rm = TRUE),
      best_score      = suppressWarnings(max(best_score, na.rm = TRUE))
    ), by = input_id]
    diags[!is.finite(best_score), best_score := NA_real_]
  } else {
    diags <- NULL
  }

  list(matches = matches, diagnostics = diags)
}

.match_exact_components_duckdb <- function(
    con, inputs_dt, max_results, min_score, weights, include_custom,
    verbose = FALSE, alias_types = NULL) {
  inputs_dt <- inputs_dt[!is.na(in_street_name)]
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(
    con, "__gnafr_exact_component_inputs__", inputs_dt, overwrite = TRUE
  )
  on.exit(try(
    duckdb::duckdb_unregister(con, "__gnafr_exact_component_inputs__"),
    silent = TRUE
  ))

  alias_sql <- .alias_type_sql(alias_types)
  split_number <- identical(alias_types, NA_character_)
  pre_filter <- paste(
    if (split_number) "TRUE" else .number_prefilter_sql(),
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )
  join_on <- paste(
    "g.postcode = i.in_postcode",
    "AND g.street_name = i.in_street_name"
  )
  out <- .run_duckdb_score_query(
    con, "__gnafr_exact_component_inputs__", "gnaf_addresses",
    join_on, pre_filter, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (exact components)", split_number = split_number
  )
  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    custom <- .run_duckdb_score_query(
      con, "__gnafr_exact_component_inputs__", "custom_addresses",
      join_on, pre_filter, weights, max_results, min_score, verbose,
      label = "custom_addresses (exact components)", split_number = split_number
    )
    out <- .combine_path_results(out, custom, max_results)
  }
  out
}

.match_postcode_duckdb <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE,
                                   alias_types = NULL) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  # Block on an exact postcode match - this is the hot path and must stay a
  # cheap hash join against the full multi-million-row gnaf_addresses table.
  # Near-miss postcodes (off by a digit, postal vs. delivery postcode, etc.)
  # are NOT retrieved here; instead .score_sql_exprs still gives partial
  # credit when in_postcode and postcode happen to both appear (e.g. via the
  # locality-fallback path below, which discovers candidates by suburb name
  # regardless of how far off the stated postcode is - the right tool for
  # "the postcode looks fine but is actually wrong", which broadening this
  # join to +/- N would only handle for small, fixed N at a steep cost: every
  # extra offset multiplies the join's candidate volume (and runtime) because
  # both the input side AND the matching gnaf rows per postcode multiply.
  duckdb::duckdb_register(con, "__gnafr_pc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_pc_inputs__"), silent = TRUE))

  alias_sql <- .alias_type_sql(alias_types)
  join_on  <- "g.postcode = i.in_postcode"
  split_number <- nrow(inputs_dt) > 100L
  pre_filt <- paste(
    "i.in_postcode IS NOT NULL",
    if (!split_number) paste("AND", .number_prefilter_sql()) else "",
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )

  res <- .run_duckdb_score_query(
    con, "__gnafr_pc_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (postcode)", split_number = split_number
  )

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    r2 <- .run_duckdb_score_query(
      con, "__gnafr_pc_inputs__", "custom_addresses",
      join_on, pre_filt, weights, max_results, min_score, verbose,
      label = "custom_addresses (postcode)", split_number = split_number
    )
    res <- .combine_path_results(res, r2, max_results)
  }

  res
}

.match_state_duckdb <- function(con, inputs_dt, max_results, min_score,
                                weights, include_custom, verbose = FALSE,
                                alias_types = NULL) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_st_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_st_inputs__"), silent = TRUE))

  alias_sql <- .alias_type_sql(alias_types)
  join_on  <- "g.state = i.in_state"
  # The same number pre-filter as the postcode path: without it this path
  # scores an entire state's rows per input, which dominates its runtime.
  pre_filt <- paste(
    "i.in_state IS NOT NULL",
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )

  res <- .run_duckdb_score_query(
    con, "__gnafr_st_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (state)", split_number = TRUE
  )

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    r2 <- .run_duckdb_score_query(
      con, "__gnafr_st_inputs__", "custom_addresses",
      join_on, pre_filt, weights, max_results, min_score, verbose,
      label = "custom_addresses (state)", split_number = TRUE
    )
    res <- .combine_path_results(res, r2, max_results)
  }

  res
}

# Recovers a locality that address_parse() lost because, with no comma to
# mark the street/suburb boundary, its rightmost apparent street-type word
# was actually part of the suburb name (e.g. "Point Lookout" - LOOKOUT is a
# legitimate street type; .LOCALITY_COLLISION_WORDS in R/parse.R already
# handles some of these, but it's a fixed word list checked without any
# database access, so it can't be complete, and its "search one word further
# left" strategy still fails when *both* words of a two-word suburb collide
# with the vocabulary, e.g. "River Heights" - RIVER is also a street type).
#
# Targets only the unambiguous signature of this failure: in_locality is NA
# but there's leftover street text and a parsed postcode. Reconstructs the
# original comma-less tail (in_street_name + in_street_type + in_street_suffix,
# in that left-to-right order - the same order the non-comma parser emits
# them in) and checks whether its last 1-3 words are an exact, known locality
# for that postcode (longest match wins, so "RIVER HEIGHTS" isn't shadowed by
# "HEIGHTS" alone). When one is found, the remaining prefix is re-resolved
# into street_name/street_type with .resolve_boundary_street_types() - the
# same function the comma-hint path already uses - so the fix is: treat the
# newly-found boundary exactly like a comma would have been treated. Modifies
# `parsed` in place; does nothing to rows that already have a locality.
.recover_missing_locality <- function(con, parsed) {
  candidates <- parsed[
    is.na(in_locality) & !is.na(in_street_name) & !is.na(in_postcode),
    .(input_id, in_postcode, in_state, in_street_name, in_street_type, in_street_suffix)
  ]
  if (nrow(candidates) == 0L) return(invisible(NULL))

  candidates[, remainder := trimws(paste(
    fifelse(is.na(in_street_name), "", in_street_name),
    fifelse(is.na(in_street_type), "", in_street_type),
    fifelse(is.na(in_street_suffix), "", in_street_suffix)
  ))]
  words <- strsplit(candidates$remainder, "\\s+")
  n_words <- lengths(words)

  best_locality <- rep(NA_character_, nrow(candidates))
  best_prefix <- rep(NA_character_, nrow(candidates))
  # Longest candidate suffix first, so a genuine two-word locality isn't
  # shadowed by a shorter partial match that also happens to be real
  # elsewhere (e.g. "HEIGHTS" alone is a real locality in other postcodes).
  for (k in 3:1) {
    open <- is.na(best_locality) & n_words > k
    if (!any(open)) next
    idx <- which(open)
    suffix <- vapply(words[idx], function(w) paste(utils::tail(w, k), collapse = " "), character(1L))
    prefix <- vapply(words[idx], function(w) paste(utils::head(w, length(w) - k), collapse = " "), character(1L))
    check <- data.table(row = idx, postcode = candidates$in_postcode[idx],
                        state = candidates$in_state[idx], suffix = suffix)
    duckdb::duckdb_register(con, "__gnafr_locrecover__", check, overwrite = TRUE)
    hits <- tryCatch(
      setDT(DBI::dbGetQuery(con, "
        SELECT DISTINCT c.row
        FROM __gnafr_locrecover__ c
        JOIN gnaf_locality_index l
          ON l.postcode = c.postcode AND l.locality_name = c.suffix
         AND (c.state IS NULL OR l.state = c.state)
      ")),
      finally = try(duckdb::duckdb_unregister(con, "__gnafr_locrecover__"), silent = TRUE)
    )
    if (nrow(hits) == 0L) next
    matched_rows <- match(hits$row, idx)
    best_locality[idx[matched_rows]] <- suffix[matched_rows]
    best_prefix[idx[matched_rows]] <- prefix[matched_rows]
  }

  recovered <- which(!is.na(best_locality))
  if (length(recovered) == 0L) return(invisible(NULL))

  resources <- .get_parser_resources()
  resolved <- .resolve_boundary_street_types(
    best_prefix[recovered], rep(TRUE, length(recovered)), resources
  )
  street_name <- ifelse(
    is.na(resolved$start), best_prefix[recovered],
    trimws(substr(best_prefix[recovered], 1L, resolved$start - 1L))
  )
  parsed[
    match(candidates$input_id[recovered], input_id),
    `:=`(
      in_locality = best_locality[recovered],
      in_street_name = street_name,
      in_street_type = resolved$canonical,
      in_street_suffix = NA_character_
    )
  ]
  invisible(NULL)
}

.match_locality_duckdb <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE,
                                   alias_types = NULL) {
  if (nrow(inputs_dt) == 0L || !any(!is.na(inputs_dt$in_locality)))
    return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_loc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_loc_inputs__"), silent = TRUE))

  # Resolve exact locality names with an equi-join. Only genuinely noisy
  # locality tokens pay for an all-locality Jaro-Winkler comparison.
  loc_map <- setDT(DBI::dbGetQuery(con, "
    SELECT DISTINCT i.input_id, l.postcode AS alt_postcode
    FROM __gnafr_loc_inputs__ i
    JOIN gnaf_locality_index l
      ON l.locality_name = i.in_locality
     AND (i.in_state IS NULL OR l.state = i.in_state)
    WHERE i.in_locality IS NOT NULL
  "))

  fuzzy_inputs <- unique(inputs_dt[
    !input_id %in% loc_map$input_id & !is.na(in_locality),
    .(input_id, in_locality, in_state)
  ])
  if (nrow(fuzzy_inputs) > 0L) {
    # Many inputs share the same misspelt locality. Score each locality/state
    # pair once, then expand its postcode choices back to the original inputs.
    fuzzy_inputs[, locality_id := .GRP, by = .(in_locality, in_state)]
    fuzzy_keys <- unique(fuzzy_inputs[, .(locality_id, in_locality, in_state)])
    duckdb::duckdb_register(
      con, "__gnafr_fuzzy_loc_inputs__", fuzzy_keys, overwrite = TRUE
    )
    on.exit(try(
      duckdb::duckdb_unregister(con, "__gnafr_fuzzy_loc_inputs__"),
      silent = TRUE
    ), add = TRUE)
    fuzzy_map <- setDT(DBI::dbGetQuery(con, "
      WITH similarities AS (
        SELECT i.locality_id, l.postcode AS alt_postcode,
               jaro_winkler_similarity(l.locality_name, i.in_locality) AS similarity
        FROM __gnafr_fuzzy_loc_inputs__ i
        JOIN gnaf_locality_index l
          ON i.in_state IS NULL OR l.state = i.in_state
      ), postcodes AS (
        SELECT locality_id, alt_postcode, MAX(similarity) AS similarity
        FROM similarities
        WHERE similarity >= 0.85
        GROUP BY locality_id, alt_postcode
      ), ranked AS (
        SELECT *, ROW_NUMBER() OVER (
          PARTITION BY locality_id ORDER BY similarity DESC, alt_postcode
        ) AS locality_rank
        FROM postcodes
      )
      SELECT DISTINCT locality_id, alt_postcode
      FROM ranked
      WHERE locality_rank <= 5
    "))
    fuzzy_map <- fuzzy_inputs[fuzzy_map, on = "locality_id",
                              nomatch = 0L, allow.cartesian = TRUE][
      , .(input_id, alt_postcode)
    ]
    loc_map <- unique(rbindlist(list(loc_map, fuzzy_map), use.names = TRUE))
  }

  offsets <- inputs_dt[
    !input_id %in% loc_map$input_id & !is.na(in_postcode),
    .(alt_postcode = in_postcode + c(-3L, -2L, -1L, 1L, 2L, 3L)),
    by = input_id
  ]
  alt_map <- unique(rbindlist(list(loc_map, offsets), use.names = TRUE))
  if (nrow(alt_map) == 0L) return(.empty_path_result())

  expanded <- inputs_dt[alt_map, on = "input_id", nomatch = 0L]
  expanded <- expanded[
    is.na(in_postcode) | is.na(alt_postcode) | alt_postcode != in_postcode
  ]
  if (nrow(expanded) == 0L) return(.empty_path_result())
  duckdb::duckdb_register(
    con, "__gnafr_loc_expanded__", expanded, overwrite = TRUE
  )
  on.exit(try(
    duckdb::duckdb_unregister(con, "__gnafr_loc_expanded__"), silent = TRUE
  ), add = TRUE)

  alias_sql <- .alias_type_sql(alias_types)
  split_number <- nrow(expanded) > 100L
  pre_filter <- paste(
    if (split_number) "TRUE" else .number_prefilter_sql(),
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )
  res <- .run_duckdb_score_query(
    con, "__gnafr_loc_expanded__", "gnaf_addresses",
    "g.postcode = i.alt_postcode", pre_filter,
    weights, max_results, min_score, verbose,
    label = "gnaf_addresses (locality)", split_number = split_number
  )
  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    custom <- .run_duckdb_score_query(
      con, "__gnafr_loc_expanded__", "custom_addresses",
      "g.postcode = i.alt_postcode", pre_filter,
      weights, max_results, min_score, verbose,
      label = "custom_addresses (locality)", split_number = split_number
    )
    res <- .combine_path_results(res, custom, max_results)
  }
  res
}

# Street-number-relaxed fallback: the number pre-filter shared by every path
# above (.number_prefilter_sql()) excludes a candidate from the SQL join -
# before street name is ever scored - whenever its number doesn't match or
# overlap the input's. A real row on the correct street at a *different*
# number is therefore invisible to every path above, no matter how strong the
# street/suburb/postcode match would otherwise be, so a coincidentally
# numbered but wrong street can win by default. Drops the number constraint
# entirely and joins on street_name instead, so .run_duckdb_score_query()'s
# existing, already-calibrated scoring can compare "right street, honestly
# low number score" against whatever else was found on its own merits -
# nothing about scoring changes here, only which candidates are visible to it.
.match_street_number_relaxed_duckdb <- function(con, inputs_dt, max_results, min_score,
                                                weights, include_custom,
                                                verbose = FALSE, alias_types = NULL) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  has_pc <- inputs_dt[!is.na(in_postcode)]
  no_pc  <- inputs_dt[is.na(in_postcode) & !is.na(in_state)]

  alias_sql <- .alias_type_sql(alias_types)
  pre_filter <- paste("TRUE", if (!is.null(alias_sql)) paste("AND", alias_sql) else "")

  res <- .empty_path_result()

  if (nrow(has_pc) > 0L) {
    duckdb::duckdb_register(con, "__gnafr_snr_pc__", has_pc, overwrite = TRUE)
    on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_snr_pc__"), silent = TRUE))
    join_on <- "g.postcode = i.in_postcode AND g.street_name = i.in_street_name"
    pc_res <- .run_duckdb_score_query(
      con, "__gnafr_snr_pc__", "gnaf_addresses",
      join_on, pre_filter, weights, max_results, min_score, verbose,
      label = "gnaf_addresses (street-number relaxed, postcode)"
    )
    res <- .combine_path_results(res, pc_res, max_results)
    if (include_custom && .table_has_rows(con, "custom_addresses")) {
      pc_custom <- .run_duckdb_score_query(
        con, "__gnafr_snr_pc__", "custom_addresses",
        join_on, pre_filter, weights, max_results, min_score, verbose,
        label = "custom_addresses (street-number relaxed, postcode)"
      )
      res <- .combine_path_results(res, pc_custom, max_results)
    }
  }

  if (nrow(no_pc) > 0L) {
    duckdb::duckdb_register(con, "__gnafr_snr_st__", no_pc, overwrite = TRUE)
    on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_snr_st__"), silent = TRUE), add = TRUE)
    join_on <- "g.state = i.in_state AND g.street_name = i.in_street_name"
    st_res <- .run_duckdb_score_query(
      con, "__gnafr_snr_st__", "gnaf_addresses",
      join_on, pre_filter, weights, max_results, min_score, verbose,
      label = "gnaf_addresses (street-number relaxed, state)"
    )
    res <- .combine_path_results(res, st_res, max_results)
    if (include_custom && .table_has_rows(con, "custom_addresses")) {
      st_custom <- .run_duckdb_score_query(
        con, "__gnafr_snr_st__", "custom_addresses",
        join_on, pre_filter, weights, max_results, min_score, verbose,
        label = "custom_addresses (street-number relaxed, state)"
      )
      res <- .combine_path_results(res, st_custom, max_results)
    }
  }

  res
}

# Street-only fallback: match against alias_type = 'street_only' records only.
# Fired for inputs that survived all other paths without a match.
# number_first on these records is NULL, so numbered inputs score 0 for number
# but can still reach min_score via postcode + suburb + street name/type.
.match_street_only_duckdb <- function(con, inputs_dt, max_results, min_score,
                                      weights, verbose = FALSE) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_so_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_so_inputs__"), silent = TRUE))

  join_on  <- "g.postcode = i.in_postcode AND g.alias_type = 'street_only'"
  pre_filt <- "i.in_postcode IS NOT NULL AND g.alias_type = 'street_only'"

  .run_duckdb_score_query(
    con, "__gnafr_so_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (street_only)"
  )
}

.empty_result <- function() {
  data.table(
    input_id = integer(), input_raw = character(), input_standardised = character(),
    match_rank = integer(), matched = logical(), match_status = character(),
    total_score = integer(),
    score_postcode = integer(), score_suburb = integer(),
    score_street_name = integer(), score_street_type = integer(),
    score_number = integer(), score_flat = integer(),
    address_detail_pid = character(), address_label = character(),
    address_site_name = character(), building_name = character(),
    flat_type = character(), flat_number = character(),
    level_type = character(), level_number = character(), lot_number = character(),
    number_first = integer(), number_last = integer(),
    street_name = character(), street_type = character(),
    street_suffix = character(),
    locality_name = character(), state = character(),
    postcode = integer(), longitude = numeric(), latitude = numeric(),
    source = character(), alias_type = character(),
    alias_principal = character(), principal_pid = character(),
    primary_secondary = character(), primary_pid = character(),
    geocode_type = character(), date_created = as.Date(character()),
    legal_parcel_id = character(), mb_code = character(),
    in_postcode = integer(), in_state = character(), in_locality = character(),
    in_street_name = character(), in_street_type = character(),
    in_street_suffix = character(), in_number_first = integer(),
    in_number_last = integer(), in_flat_type = character(),
    in_flat_number = character(), in_level_type = character(),
    in_level_number = character(), in_lot_number = character(),
    in_building_name = character()
  )
}

.empty_path_result <- function() {
  list(matches = NULL, diagnostics = NULL)
}

.deduplicate_match_inputs <- function(parsed) {
  if (nrow(parsed) == 0L) {
    return(list(inputs = parsed, fanout = data.table(
      input_id = integer(), match_input_id = integer()
    )))
  }
  signature_cols <- c(
    setdiff(
      grep("^in_", names(parsed), value = TRUE),
      c("input_id", "input_raw", "input_standardised")
    ),
    "input_standardised"
  )
  signature_cols <- unique(signature_cols[signature_cols %in% names(parsed)])
  inputs <- unique(parsed, by = signature_cols)
  lookup <- inputs[, c("input_id", signature_cols), with = FALSE]
  setnames(lookup, "input_id", "match_input_id")
  fanout <- lookup[parsed, on = signature_cols, nomatch = 0L, allow.cartesian = TRUE,
                   .(input_id = i.input_id, match_input_id)]
  list(inputs = inputs, fanout = fanout)
}

.fanout_match_rows <- function(rows, fanout) {
  if (is.null(rows) || nrow(rows) == 0L || nrow(fanout) == 0L) return(rows)
  out <- merge(
    fanout, rows,
    by.x = "match_input_id", by.y = "input_id",
    all = FALSE, sort = FALSE, allow.cartesian = TRUE
  )
  out[, match_input_id := NULL]
  setcolorder(out, c("input_id", setdiff(names(out), "input_id")))
  out
}

.standardise_input <- function(parsed) {
  missing_column <- function(name, mode = "character") {
    if (name %in% names(parsed)) return(parsed[[name]])
    switch(mode,
      integer = rep(NA_integer_, nrow(parsed)),
      rep(NA_character_, nrow(parsed))
    )
  }
  postcode_chr <- ifelse(is.na(parsed$in_postcode), NA_character_, as.character(parsed$in_postcode))
  number_suffix <- missing_column("in_number_suffix")
  suffix_chr <- ifelse(is.na(number_suffix), "", number_suffix)
  number_chr <- ifelse(
    is.na(parsed$in_number_first),
    NA_character_,
    ifelse(
      is.na(parsed$in_number_last),
      paste0(parsed$in_number_first, suffix_chr),
      paste0(parsed$in_number_first, suffix_chr, "-", parsed$in_number_last)
    )
  )
  flat_chr <- ifelse(
    !is.na(parsed$in_flat_type) & !is.na(parsed$in_flat_number),
    paste(parsed$in_flat_type, parsed$in_flat_number),
    ifelse(!is.na(parsed$in_flat_number), parsed$in_flat_number, NA_character_)
  )
  in_level_type <- missing_column("in_level_type")
  in_level_number <- missing_column("in_level_number")
  in_lot_number <- missing_column("in_lot_number")
  level_chr <- ifelse(
    !is.na(in_level_type) & !is.na(in_level_number),
    paste(in_level_type, in_level_number),
    ifelse(!is.na(in_level_number), in_level_number, NA_character_)
  )
  lot_chr <- ifelse(
    !is.na(in_lot_number), paste("LOT", in_lot_number),
    NA_character_
  )

  line_one <- .collapse_address_parts(
    parsed$in_building_name,
    flat_chr,
    level_chr,
    lot_chr,
    number_chr,
    parsed$in_street_name,
    parsed$in_street_type,
    parsed$in_street_suffix
  )
  line_two <- .collapse_address_parts(parsed$in_locality, parsed$in_state, postcode_chr)

  out <- ifelse(
    !is.na(line_one) & !is.na(line_two),
    paste(line_one, line_two, sep = ", "),
    ifelse(!is.na(line_one), line_one, line_two)
  )
  out[nzchar(out) == FALSE] <- NA_character_
  out
}

.collapse_address_parts <- function(...) {
  parts <- list(...)
  parts <- lapply(parts, function(x) ifelse(is.na(x), "", as.character(x)))
  out <- do.call(paste, c(parts, sep = " "))
  out <- trimws(gsub("\\s+", " ", out))
  out[out == ""] <- NA_character_
  out
}

.append_match_status <- function(out, parsed, diagnostics) {
  diagnostic_dt <- rbindlist(diagnostics, fill = TRUE, use.names = TRUE)
  if (nrow(diagnostic_dt) > 0L) {
    diagnostic_dt <- diagnostic_dt[, .(
      candidate_count = sum(candidate_count, na.rm = TRUE),
      retained_count = sum(retained_count, na.rm = TRUE),
      best_score = suppressWarnings(max(best_score, na.rm = TRUE))
    ), by = input_id]
    diagnostic_dt[!is.finite(best_score), best_score := NA_real_]
    out <- diagnostic_dt[out, on = "input_id"]
  } else {
    out[, `:=`(candidate_count = NA_integer_, retained_count = NA_integer_, best_score = NA_real_)]
  }

  out[, match_status := fifelse(
    matched,
    "matched",
    fifelse(
      !is.na(candidate_count) & candidate_count > 0L & (is.na(retained_count) | retained_count == 0L),
      "below_min_score",
      fifelse(
        is.na(in_street_name) | (is.na(in_postcode) & is.na(in_state) & is.na(in_locality)),
        "insufficient_parse",
        "no_candidate"
      )
    )
  )]

  out[, c("candidate_count", "retained_count", "best_score") := NULL]
  out
}

# Exact address_label pass: returns scored candidate pairs for any input whose
# standardised text (uppercased, abbreviations expanded) matches a GNAF
# address_label exactly. Keying on the standardised form rather than the raw
# input means common abbreviations ("St" vs "Street") still hit this fast path
# instead of falling through to component matching. Both paths respect the
# same alias filters; linked-address resolution happens after final ranking.
.exact_label_match <- function(con, parsed, include_custom, alias_types = NULL,
                                weights = .default_match_weights(),
                                min_score = 0L) {
  std_upper <- unique(toupper(trimws(parsed$input_standardised)))
  std_upper <- std_upper[nzchar(std_upper) & !is.na(std_upper)]
  if (length(std_upper) == 0L) return(NULL)

  # Register as a virtual table so DuckDB can hash-join instead of scanning
  # with a 50k-item IN() literal (which kills the query planner at scale).
  lkp <- data.table(lbl_key = std_upper)
  duckdb::duckdb_register(con, "__gnafr_exact_lkp__", lkp, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_exact_lkp__"), silent = TRUE))

  alias_sql   <- .alias_type_sql(alias_types)
  alias_where <- if (!is.null(alias_sql)) sprintf("\n     AND %s", alias_sql) else ""

  sql <- sprintf(
    "WITH exact_keys AS MATERIALIZED (
       SELECT g.address_detail_pid
       FROM gnaf_addresses g
       JOIN __gnafr_exact_lkp__ l ON g.address_label = l.lbl_key%s
     )
     SELECT %s FROM gnaf_addresses g
     JOIN exact_keys k USING (address_detail_pid)",
    alias_where,
    .GNAF_SELECT_COLS
  )
  cands <- setDT(DBI::dbGetQuery(con, sql))

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    sql2 <- sprintf(
      "WITH exact_keys AS MATERIALIZED (
         SELECT g.address_detail_pid
         FROM custom_addresses g
         JOIN __gnafr_exact_lkp__ l ON g.address_label = l.lbl_key%s
       )
       SELECT %s FROM custom_addresses g
       JOIN exact_keys k USING (address_detail_pid)",
      alias_where, .GNAF_SELECT_COLS
    )
    cands <- rbindlist(list(cands, setDT(DBI::dbGetQuery(con, sql2))), fill = TRUE)
  }

  if (nrow(cands) == 0L) return(NULL)

  cands[, lbl_key := toupper(trimws(address_label))]
  pi <- copy(parsed)
  pi[, lbl_key := toupper(trimws(input_standardised))]

  joined <- cands[pi, on = "lbl_key", nomatch = 0L, allow.cartesian = TRUE]
  joined[, lbl_key := NULL]
  if (nrow(joined) == 0L) return(NULL)

  joined <- .score_pairs(joined, weights = weights)
  joined <- joined[total_score >= min_score]
  if (nrow(joined) == 0L) return(NULL)
  joined[, match_rank := 1L]
  joined
}

.cli_match_step <- function(verbose, text) {
  if (isTRUE(verbose)) cli::cli_alert_info(text)
}

.cli_match_detail <- function(verbose, text) {
  if (isTRUE(verbose)) cli::cli_li(text)
}

.cli_match_alert <- function(verbose, level, text) {
  if (!isTRUE(verbose)) return(invisible(NULL))

  switch(
    level,
    warning = cli::cli_alert_warning(text),
    danger = cli::cli_alert_danger(text),
    success = cli::cli_alert_success(text),
    cli::cli_alert_info(text)
  )
}

.cli_match_summary <- function(verbose, parsed, out, total_timer,
                               verbose_stats = NULL) {
  if (!isTRUE(verbose)) return(invisible(NULL))

  total_elapsed <- proc.time()[["elapsed"]] - total_timer
  matched_rows <- out[matched %in% TRUE]
  matched_inputs <- if (nrow(matched_rows) > 0L) uniqueN(matched_rows$input_id) else 0L
  unmatched_inputs <- nrow(parsed) - matched_inputs
  matched_pct <- if (nrow(parsed) > 0L) 100 * matched_inputs / nrow(parsed) else 0
  input_word <- if (nrow(parsed) == 1L) "row" else "rows"
  candidate_word <- if (nrow(matched_rows) == 1L) "row" else "rows"
  avg_best <- if (nrow(matched_rows) > 0L) {
    round(mean(matched_rows[match_rank == 1L, total_score]), 1)
  } else {
    NA_real_
  }

  cli::cli_h1("gnaf_match summary")
  cli::cli_alert_success(
    sprintf(
      "Matched %s of %s input %s (%s).",
      cli::col_green(format(matched_inputs, big.mark = ",")),
      cli::col_blue(format(nrow(parsed), big.mark = ",")),
      input_word,
      cli::col_green(sprintf("%.1f%%", matched_pct))
    )
  )
  cli::cli_li(
    sprintf(
      "Returned %s candidate %s after ranking and filtering.",
      cli::col_cyan(format(nrow(matched_rows), big.mark = ",")),
      candidate_word
    )
  )
  cli::cli_li(
    sprintf(
      "Unmatched inputs above min_score: %s.",
      cli::col_yellow(format(unmatched_inputs, big.mark = ","))
    )
  )
  if (!is.null(verbose_stats)) {
    cli::cli_li(
      sprintf(
        "Exact label matches: %s in %s.",
        cli::col_green(format(verbose_stats$exact_inputs, big.mark = ",")),
        cli::col_cyan(sprintf("%.2fs", verbose_stats$exact_elapsed))
      )
    )
    cli::cli_li(
      sprintf(
        "Cache matches: %s in %s.",
        cli::col_green(format(verbose_stats$cache_inputs, big.mark = ",")),
        cli::col_cyan(sprintf("%.2fs", verbose_stats$cache_elapsed))
      )
    )
    cli::cli_li(
      sprintf(
        "Slow-path matches: %s in %s.",
        cli::col_green(format(verbose_stats$slow_inputs, big.mark = ",")),
        cli::col_cyan(sprintf("%.2fs", verbose_stats$slow_elapsed))
      )
    )
  }
  if (!is.na(avg_best)) {
    cli::cli_li(sprintf("Average top-match score: %s.", cli::col_magenta(sprintf("%.1f", avg_best))))
  }
  cli::cli_text(
    sprintf(
      "Timings: parse %s, standardise %s, slow path %s, wrangle %s, total %s.",
      cli::col_cyan(sprintf("%.2fs", verbose_stats$parse_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$standardise_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$slow_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$wrangle_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", total_elapsed))
    )
  )
}
