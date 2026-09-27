# Granular comparisons share the existing number, street_type and flat weights.
# Missing evidence receives less credit than agreement and more than conflict.
.score_value <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x
}

.score_value_sql <- function(x) {
  sprintf("UPPER(TRIM(COALESCE(CAST(%s AS VARCHAR), '')))", x)
}

.SCORE_DIRECTIONS <- c(N = "NORTH", NTH = "NORTH", S = "SOUTH", STH = "SOUTH",
                       E = "EAST", W = "WEST", NE = "NORTH EAST", NW = "NORTH WEST",
                       SE = "SOUTH EAST", SW = "SOUTH WEST")

# Both sides pass through this map before comparison: user input arrives
# already canonicalised by address_parse() via inst/extdata/flat_types.csv
# (full English words), while a real GNAF candidate row stores its official
# short FLAT_TYPE_CODE verbatim from the source PSV - confirmed against a
# real load: APT, HSE, STU, BLDG, WHSE, CTGE, TNHS, DUPL, FCTY, KSK, PTHS,
# MSNT, VLLA, OFFC and SE all appear, alongside UNIT/SHOP/SITE/ROOM/SHED/
# REAR/WARD/FLAT which already match their canonical word directly. Without
# mapping GNAF's own codes here too, a perfectly correct match (input
# "Apartment 210" vs a real GNAF row coded "APT 210") scored as a *type
# conflict* - halving the identifier's credit - purely because "APARTMENT"
# != "APT" as strings, even though they mean the same thing.
#
# Reuses .get_flat_type_map() (the same abbreviation table address_parse()
# itself uses) so the two vocabularies can't drift apart again, with one
# addition: APARTMENT/FLAT/UNIT are deliberately merged into a single "UNIT"
# bucket, since these three are used near-interchangeably for residential
# sub-addresses in Australian English - every other category keeps its own
# distinct target, so e.g. a "Warehouse" input still correctly conflicts
# with a real "Shop".
.get_score_flat_type_map <- function() {
  if (is.null(.gnafr_env$score_flat_type_map)) {
    m <- .get_flat_type_map()
    m[m %in% c("APARTMENT", "FLAT", "UNIT")] <- "UNIT"
    .gnafr_env$score_flat_type_map <- m
  }
  .gnafr_env$score_flat_type_map
}

# Same idea for level/floor identifiers, reused from .get_level_type_map()
# the same way, with LEVEL/FLOOR merged into one bucket (an existing,
# deliberate choice: the two words describe the same physical concept).
# GNAF's own LEVEL_TYPE_CODE for this is overwhelmingly "L" in a real load
# (by far the single most common value), plus "FL" and "LG".
#
# "B" (basement) is added only here, not to level_types.csv's parsing
# vocabulary - a bare "B" is too easy to collide with real address text when
# *parsing* free-form input (e.g. "Tower B 25 Smith Street"), but GNAF's own
# level_type column is already a trusted, structured value rather than free
# text, so there's no such risk in this comparison.
.get_score_level_type_map <- function() {
  if (is.null(.gnafr_env$score_level_type_map)) {
    m <- .get_level_type_map()
    m[m %in% c("LEVEL", "FLOOR")] <- "LEVEL"
    m["B"] <- "BASEMENT"
    .gnafr_env$score_level_type_map <- m
  }
  .gnafr_env$score_level_type_map
}

.score_mapped_value <- function(x, map) {
  x <- .score_value(x)
  matched <- x %in% names(map)
  x[matched] <- unname(map[x[matched]])
  x
}

.score_mapped_value_sql <- function(x, map) {
  value <- .score_value_sql(x)
  # Identity entries already fall through to ELSE; omitting them keeps the
  # repeated SQL expressions small without changing the dictionary.
  map <- map[names(map) != unname(map)]
  if (!length(map)) return(value)
  # Most stored and parsed types are already canonical. A constant IN set
  # avoids walking hundreds of abbreviation CASE arms for every candidate.
  unchanged <- setdiff(unique(unname(map)), names(map))
  mapped <- paste("CASE", value,
    paste(sprintf("WHEN '%s' THEN '%s'", names(map), map), collapse = " "),
    "ELSE", value, "END")
  if (!length(unchanged)) return(mapped)
  sprintf("CASE WHEN %s IN (%s) THEN %s ELSE %s END", value,
    paste(sprintf("'%s'", unchanged), collapse = ", "), value, mapped)
}

.pair_column <- function(pairs, name, default = NA_character_) {
  if (name %in% names(pairs)) pairs[[name]] else rep(default, nrow(pairs))
}

# Use the number immediately before the candidate's street, so a unit, level
# or building prefix cannot be mistaken for the house number. These databases
# do not store number suffixes separately. Anchor against the candidate's own
# street spelling, including when the input street has a typo.
.candidate_number_token_sql <- function(g = "g") {
  label <- sprintf("UPPER(%s.address_label)", g)
  # A lot-only label must never supply a fallback house number. Keep the
  # inexpensive path for the usual labels without LOT.
  label <- sprintf(paste0("CASE WHEN STRPOS(%1$s, 'LOT ') > 0 THEN ",
    "REGEXP_REPLACE(%1$s, '\\bLOT +[0-9]+[A-Z]?(-[0-9]+[A-Z]?)?\\b', '', 'g') ",
    "ELSE %1$s END"), label)
  fallback <- sprintf(paste0(
    "REGEXP_EXTRACT(%s, ",
    "'(^|[ /])([0-9]+[A-Z]?(-[0-9]+[A-Z]?)?) +' || ",
    "REGEXP_ESCAPE(UPPER(TRIM(%s.street_name))) || '( |,|$)', 2)"
  ), label, g)
  # A per-row regex containing the street name is expensive to compile for
  # every candidate pair. Locate its first occurrence literally, then extract
  # the preceding number with a constant pattern. Only accept a complete street
  # token and a valid number; repeated street names in building prefixes and
  # other unusual layouts still use the original search.
  street <- sprintf("UPPER(TRIM(%s.street_name))", g)
  position <- sprintf("STRPOS(%s, ' ' || %s)", label, street)
  token <- sprintf(paste0(
    "REGEXP_EXTRACT(SUBSTR(%s, 1, %s - 1), ",
    "'(^|[ /])([0-9]+[A-Z]?(-[0-9]+[A-Z]?)?) *$', 2)"
  ), label, position)
  boundary <- sprintf("SUBSTR(%s, %s + 1 + LENGTH(%s), 1)",
                      label, position, street)
  extracted <- sprintf(paste0(
    "CASE WHEN %s > 0 AND %s IN ('', ' ', ',') AND %s != '' ",
    "THEN %s ELSE %s END"
  ), position, boundary, token, token, fallback)
  # Street-only aliases dominate the missing-number branch. They cannot have
  # a number token if their label has no digit; avoid compiling a street-specific
  # fallback regex for each one. Preserve NULL-street behaviour.
  sprintf(paste0("CASE WHEN %s.street_name IS NOT NULL AND ",
    "NOT REGEXP_MATCHES(%s.address_label, '[0-9]') THEN '' ELSE %s END"), g, g, extracted)
}

.candidate_number_token <- function(pairs) {
  street <- .pair_column(pairs, "street_name")
  label <- toupper(.pair_column(pairs, "address_label"))
  lot <- which(grepl("LOT ", label, fixed = TRUE))
  label[lot] <- gsub("\\bLOT +[0-9]+[A-Z]?(-[0-9]+[A-Z]?)?\\b", "", label[lot], perl = TRUE)
  pattern <- paste0(
    "(^|[ /])([0-9]+[A-Z]?(-[0-9]+[A-Z]?)?) +\\Q",
    toupper(trimws(street)), "\\E( |,|$)"
  )
  found <- stringi::stri_match_first_regex(label, pattern)[, 3L]
  found[is.na(street) | !nzchar(trimws(street))] <- NA_character_
  found
}

.score_number_sql <- function(weight, i, g, gate = NULL) {
  # A lot is not a street number and may repeat along a street. Only use it
  # as the locating identifier when no street number was supplied.
  token <- .candidate_number_token_sql(g)
  g_first <- sprintf("COALESCE(%s.number_first, TRY_CAST(REGEXP_EXTRACT(%s, '^[0-9]+') AS INTEGER))", g, token)
  i_first <- paste0(i, ".in_number_first")
  i_end <- sprintf("COALESCE(%s.in_number_last, %s)", i, i_first)
  g_end <- sprintf("COALESCE(%s.number_last, %s)", g, g_first)
  i_suffix <- .score_value_sql(paste0(i, ".in_number_suffix"))
  # With a stored numeric number, most candidates only need suffix evidence.
  # No digit immediately followed by a letter means a suffix is impossible,
  # so full token extraction is unnecessary (including labels that are NULL).
  g_suffix <- sprintf(paste0("CASE WHEN REGEXP_MATCHES(UPPER(%s.address_label), '[0-9][A-Z]') ",
    "THEN COALESCE(REGEXP_EXTRACT(%s, '^[0-9]+([A-Z]?)', 1), '') ELSE '' END"), g, token)
  i_lot <- .score_identifier_value_sql(paste0(i, ".in_lot_number"))
  g_lot <- .score_identifier_value_sql(paste0(g, ".lot_number"))
  # Inclusive interval comparisons preserve exact ranges and also retrieve
  # individual addresses within an input range. Partial overlap is weakest.
  interval <- sprintf(paste0(
    "CASE WHEN %1$s IS NULL OR %2$s IS NULL OR %3$s < %1$s OR %4$s < %2$s THEN 0.0",
    " WHEN %1$s = %2$s AND %3$s = %4$s THEN 1.0",
    " WHEN %2$s <= %1$s AND %3$s <= %4$s THEN 0.7",
    " WHEN %1$s <= %2$s AND %4$s <= %3$s THEN 0.5",
    " WHEN %1$s <= %4$s AND %2$s <= %3$s THEN 0.3 ELSE 0.0 END"
  ), i_first, g_first, i_end, g_end)
  # Either side missing a suffix is "missing evidence" (50%), not a
  # mismatch - matching every other missing-evidence tier in this file (see
  # .score_street_type()). A candidate's number often has no separate suffix
  # field to compare against at all, which isn't the same as a real conflict.
  suffix <- sprintf(paste0(
    "CASE WHEN %1$s = %2$s THEN 1.0",
    " WHEN %1$s = '' OR %2$s = '' THEN 0.5 ELSE 0.0 END"
  ), i_suffix, g_suffix)
  sprintf(paste0(
    "CAST(ROUND_EVEN(%g%s * (CASE WHEN %s IS NULL AND %s != '' THEN ",
    "CASE WHEN %s = %s THEN 1.0 ELSE 0.0 END ",
    "ELSE (%s) * (%s) END), 0) AS INTEGER)"
  ), weight, .gate_factor_sql(gate), i_first, i_lot, i_lot, g_lot, interval, suffix)
}

.score_number <- function(pairs, weight, gate = 1) {
  token <- .candidate_number_token(pairs)
  token_first <- suppressWarnings(as.integer(sub("[^0-9].*$", "", token)))
  g_first <- pairs$number_first
  g_first[is.na(g_first)] <- token_first[is.na(g_first)]
  i_first <- pairs$in_number_first
  i_end <- .pair_column(pairs, "in_number_last", NA_integer_)
  i_end[is.na(i_end)] <- i_first[is.na(i_end)]
  g_end <- pairs$number_last
  g_end[is.na(g_end)] <- g_first[is.na(g_end)]
  valid <- !is.na(i_first) & !is.na(g_first) & i_end >= i_first & g_end >= g_first
  interval <- data.table::fcase(
    valid & i_first == g_first & i_end == g_end, 1,
    valid & g_first <= i_first & i_end <= g_end, 0.7,
    valid & i_first <= g_first & g_end <= i_end, 0.5,
    valid & i_first <= g_end & g_first <= i_end, 0.3,
    default = 0
  )
  i_suffix <- .score_value(.pair_column(pairs, "in_number_suffix"))
  g_suffix <- .score_value(sub("^[0-9]+([A-Z]?).*$", "\\1", token))
  # Either side missing a suffix is "missing evidence" (50%), not a mismatch
  # - see the SQL twin.
  suffix <- data.table::fcase(i_suffix == g_suffix, 1, i_suffix == "" | g_suffix == "", 0.5, default = 0)
  i_lot <- .score_identifier_value(.pair_column(pairs, "in_lot_number"))
  g_lot <- .score_identifier_value(.pair_column(pairs, "lot_number"))
  as.integer(round(weight * gate * data.table::fifelse(
    is.na(i_first) & i_lot != "", as.numeric(i_lot == g_lot), interval * suffix
  )))
}

# A direction qualifies the street identity without adding a seventh weight.
.score_street_type_sql <- function(weight, i, g) {
  i_type <- .score_mapped_value_sql(paste0(i, ".in_street_type"), .get_street_type_map())
  g_type <- .score_mapped_value_sql(paste0(g, ".street_type"), .get_street_type_map())
  i_suffix <- .score_mapped_value_sql(paste0(i, ".in_street_suffix"), .SCORE_DIRECTIONS)
  g_suffix <- .score_mapped_value_sql(paste0(g, ".street_suffix"), .SCORE_DIRECTIONS)
  # Check "missing" before "equal": both sides coerce a missing type to '',
  # so checking equality first would let two absent types masquerade as an
  # agreement instead of the intended missing-evidence tier.
  raw_input <- .score_value_sql(paste0(i, ".in_street_type"))
  raw_candidate <- .score_value_sql(paste0(g, ".street_type"))
  type <- sprintf(paste0("CASE WHEN %1$s = '' OR %2$s = '' THEN 0.5 ",
    "WHEN %1$s = %2$s THEN 1.0 WHEN %3$s = %4$s THEN 1.0 ELSE 0.4 END"),
    raw_input, raw_candidate, i_type, g_type)
  suffix <- sprintf("CASE WHEN %1$s = %2$s THEN 1.0 WHEN %1$s = '' OR %2$s = '' THEN 0.5 ELSE 0.0 END", i_suffix, g_suffix)
  sprintf("CAST(ROUND_EVEN(%g * (%s) * (%s), 0) AS INTEGER)", weight, type, suffix)
}

.score_street_type <- function(pairs, weight) {
  i_type <- .score_mapped_value(pairs$in_street_type, .get_street_type_map())
  g_type <- .score_mapped_value(pairs$street_type, .get_street_type_map())
  i_suffix <- .score_mapped_value(.pair_column(pairs, "in_street_suffix"), .SCORE_DIRECTIONS)
  g_suffix <- .score_mapped_value(.pair_column(pairs, "street_suffix"), .SCORE_DIRECTIONS)
  # Missing-evidence check must precede the equality check - see the SQL twin.
  type <- data.table::fcase(i_type == "" | g_type == "", 0.5, i_type == g_type, 1, default = 0.4)
  suffix <- data.table::fcase(i_suffix == g_suffix, 1, i_suffix == "" | g_suffix == "", 0.5, default = 0)
  as.integer(round(weight * type * suffix))
}

.score_identifier <- function(input, candidate, input_type, candidate_type) {
  type_conflict <- input_type != "" & candidate_type != "" & input_type != candidate_type
  data.table::fcase(
    input == candidate & !type_conflict, 1,
    input == candidate | input == "" | candidate == "", 0.5,
    default = 0
  )
}

.score_flat <- function(pairs, weight, gate = 1) {
  value <- function(name) .score_value(.pair_column(pairs, name))
  i_flat <- .score_identifier_value(value("in_flat_number"))
  g_flat <- .score_identifier_value(value("flat_number"))
  i_level <- .score_identifier_value(value("in_level_number"))
  g_level <- .score_identifier_value(value("level_number"))
  flat <- .score_identifier(i_flat, g_flat,
    .score_mapped_value(value("in_flat_type"), .get_score_flat_type_map()),
    .score_mapped_value(value("flat_type"), .get_score_flat_type_map()))
  level <- .score_identifier(i_level, g_level,
    .score_mapped_value(value("in_level_type"), .get_score_level_type_map()),
    .score_mapped_value(value("level_type"), .get_score_level_type_map()))
  flat_present <- i_flat != "" | g_flat != ""
  level_present <- i_level != "" | g_level != ""
  # Divide the existing flat weight only when both dimensions carry evidence.
  fraction <- data.table::fcase(
    flat_present & level_present, 0.6 * flat + 0.4 * level,
    flat_present, flat,
    level_present, level,
    default = 1
  )
  as.integer(round(weight * gate * fraction))
}

.score_flat_sql <- function(weight, i, g, gate = NULL) {
  value <- function(alias, name) .score_identifier_value_sql(paste0(alias, ".", name))
  i_flat <- value(i, "in_flat_number")
  g_flat <- value(g, "flat_number")
  i_level <- value(i, "in_level_number")
  g_level <- value(g, "level_number")
  identifier <- function(input, candidate, input_type, candidate_type) {
    sprintf(paste0(
      "CASE WHEN %1$s = %2$s AND NOT (%3$s != '' AND %4$s != '' AND %3$s != %4$s) THEN 1.0",
      " WHEN %1$s = %2$s OR %1$s = '' OR %2$s = '' THEN 0.5 ELSE 0.0 END"
    ), input, candidate, input_type, candidate_type)
  }
  mapped <- function(alias, name, map) .score_mapped_value_sql(paste0(alias, ".", name), map)
  flat <- identifier(i_flat, g_flat,
    mapped(i, "in_flat_type", .get_score_flat_type_map()), mapped(g, "flat_type", .get_score_flat_type_map()))
  level <- identifier(i_level, g_level,
    mapped(i, "in_level_type", .get_score_level_type_map()), mapped(g, "level_type", .get_score_level_type_map()))
  flat_present <- sprintf("(%s != '' OR %s != '')", i_flat, g_flat)
  level_present <- sprintf("(%s != '' OR %s != '')", i_level, g_level)
  sprintf(paste0(
    "CAST(ROUND_EVEN(%g%s * (CASE WHEN %s AND %s THEN 0.6 * (%s) + 0.4 * (%s)",
    " WHEN %s THEN (%s) WHEN %s THEN (%s) ELSE 1.0 END), 0) AS INTEGER)"
  ), weight, .gate_factor_sql(gate), flat_present, level_present, flat, level, flat_present, flat, level_present, level)
}

# Shared shape for every "raw Jaro-Winkler similarity -> credit multiplier"
# mapping below: full credit at/above `high`, clamped to `floor` at/below
# `low`, and a squared ramp in between that keeps the floor's edge gentle
# while still reaching full credit only for genuinely close matches.
.similarity_ramp <- function(sim, low, high, floor) {
  ramp <- pmin(1, pmax(0, (sim - low) / (high - low)))
  floor + (1 - floor) * ramp^2
}

.similarity_ramp_sql <- function(sim_expr, low, high, floor) {
  ramp <- sprintf("GREATEST(0.0, LEAST(1.0, (%s - %g) / %g))", sim_expr, low, high - low)
  sprintf("(%g + %g * POWER(%s, 2))", floor, 1 - floor, ramp)
}

# Maps a single street_name/suburb Jaro-Winkler similarity (isolated word(s),
# not diluted by surrounding address text) to a credit multiplier. Real
# unrelated street/suburb names of similar length land at 0.48-0.60
# (CERIUM/TUCKEROO 0.56, GOODWIN/NORMAN 0.54, MAPLE/OAK 0.51, KINGS/BURNS
# 0.60), while genuine near-matches (abbreviation/typo differences) sit at
# 0.84-1.00 (ST JAMES/SAINT JAMES 0.84, MARTHA/MARHTA 0.96) - the gap between
# these should receive progressively more credit, without a full-credit
# plateau at 0.85. A wrong street shouldn't retain much credit just because
# it happens to share a few letters with the right one.
.COMPONENT_SIM_LOW <- 0.60
.COMPONENT_SIM_HIGH <- 1.0
.COMPONENT_SIM_FLOOR <- 0.05

.component_similarity_factor <- function(sim) {
  .similarity_ramp(sim, .COMPONENT_SIM_LOW, .COMPONENT_SIM_HIGH, .COMPONENT_SIM_FLOOR)
}

.component_similarity_sql <- function(sim_expr) {
  .similarity_ramp_sql(sim_expr, .COMPONENT_SIM_LOW, .COMPONENT_SIM_HIGH, .COMPONENT_SIM_FLOOR)
}

# A house number or unit only locates an address *within* a street, so agreement
# on either says nothing when the street itself differs. Their credit is scaled
# by how well the street name agrees: 1 for the same street (or when either name
# is missing, so there is no evidence to gate on), falling with the same name
# similarity that drives score_street_name. Without this, a wrong street sharing
# the number and unit (30 + 20 points) outranks the right street whose number is
# simply absent from GNAF.
.street_gate <- function(input, candidate) {
  input <- .score_name_value(input)
  candidate <- .score_name_value(candidate)
  gate <- rep(1, length(input))
  fuzzy <- nzchar(input) & nzchar(candidate) & input != candidate
  if (any(fuzzy)) gate[fuzzy] <- .name_similarity_factor(input[fuzzy], candidate[fuzzy])
  gate
}

# `similarity` is .name_similarity_sql() for the street names, which is exactly 0
# only when a name is missing (a present but different name never falls below
# its floor), so it doubles as the "no evidence" test without re-normalising.
.street_gate_sql <- function(similarity) {
  sprintf("(CASE WHEN %1$s = 0.0 THEN 1.0 ELSE %1$s END)", similarity)
}

# The gate multiplies the component's weight *inside* its single rounding
# (ROUND_EVEN(weight * gate * fraction)). Rounding the score and then scaling
# and rounding again nests ROUND_EVEN around these already-large CASE trees,
# which makes DuckDB spend seconds optimising even a one-row query.
.gate_factor_sql <- function(gate) {
  if (is.null(gate)) "" else paste0(" * ", gate)
}

# Rounding must not promote an imperfect name to full agreement, even for
# long names with one small edit. Exact text is checked independently of JW:
# implementations can return 1 for distinct strings in edge cases.
.score_name <- function(input, candidate, weight) {
  input <- .score_name_value(input)
  candidate <- .score_name_value(candidate)
  present <- !is.na(input) & !is.na(candidate) & nzchar(input) & nzchar(candidate)
  result <- integer(length(input))
  exact <- present & input == candidate
  result[exact] <- as.integer(round(weight))
  fuzzy <- present & !exact
  if (any(fuzzy)) {
    similarity <- .name_similarity_factor(input[fuzzy], candidate[fuzzy])
    result[fuzzy] <- as.integer(pmin(
      max(0, round(weight) - 1),
      round(weight * similarity)
    ))
  }
  result
}

.score_name_sql <- function(input, candidate, weight, similarity = NULL) {
  # Optional similarity is an already-computed credit factor, shared with the
  # pruning bound in the bulk query; it must include every name metric.
  if (is.null(similarity)) similarity <- .name_similarity_sql(input, candidate)
  input <- .score_name_value_sql(input)
  candidate <- .score_name_value_sql(candidate)
  sprintf(paste0(
    "CASE WHEN %1$s IS NULL OR %2$s IS NULL OR %1$s = '' OR %2$s = '' THEN 0",
    " WHEN %1$s = %2$s THEN %3$d",
    " ELSE CAST(LEAST(%4$d, ROUND_EVEN(%5$g * %6$s, 0)) AS INTEGER) END"
  ), input, candidate, as.integer(round(weight)),
  as.integer(max(0, round(weight) - 1)), weight,
  similarity)
}
