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
.SCORE_FLAT_TYPES <- c(APARTMENT = "UNIT", FLAT = "UNIT")
.SCORE_LEVEL_TYPES <- c(FLOOR = "LEVEL")

.score_mapped_value <- function(x, map) {
  x <- .score_value(x)
  matched <- x %in% names(map)
  x[matched] <- unname(map[x[matched]])
  x
}

.score_mapped_value_sql <- function(x, map) {
  value <- .score_value_sql(x)
  paste("CASE", value, paste(sprintf("WHEN '%s' THEN '%s'", names(map), map), collapse = " "),
        "ELSE", value, "END")
}

.pair_column <- function(pairs, name, default = NA_character_) {
  if (name %in% names(pairs)) pairs[[name]] else rep(default, nrow(pairs))
}

# Use the number immediately before the candidate's street, so a unit, level
# or building prefix cannot be mistaken for the house number. These databases
# do not store number suffixes separately. Anchor against the candidate's own
# street spelling, including when the input street has a typo.
.candidate_number_token_sql <- function(g = "g") {
  sprintf(paste0(
    "REGEXP_EXTRACT(UPPER(%s.address_label), ",
    "'(^|[ /])([0-9]+[A-Z]?(-[0-9]+[A-Z]?)?) +' || ",
    "REGEXP_ESCAPE(UPPER(TRIM(%s.street_name))) || '( |,|$)', 2)"
  ), g, g)
}

.candidate_number_token <- function(pairs) {
  street <- .pair_column(pairs, "street_name")
  label <- .pair_column(pairs, "address_label")
  pattern <- paste0(
    "(^|[ /])([0-9]+[A-Z]?(-[0-9]+[A-Z]?)?) +\\Q",
    toupper(trimws(street)), "\\E( |,|$)"
  )
  found <- stringi::stri_match_first_regex(toupper(label), pattern)[, 3L]
  found[is.na(street) | !nzchar(trimws(street))] <- NA_character_
  found
}

.score_number_sql <- function(weight, i, g) {
  token <- .candidate_number_token_sql(g)
  g_first <- sprintf("COALESCE(%s.number_first, TRY_CAST(REGEXP_EXTRACT(%s, '^[0-9]+') AS INTEGER))", g, token)
  i_first <- paste0(i, ".in_number_first")
  i_end <- sprintf("COALESCE(%s.in_number_last, %s)", i, i_first)
  g_end <- sprintf("COALESCE(%s.number_last, %s)", g, g_first)
  i_suffix <- .score_value_sql(paste0(i, ".in_number_suffix"))
  g_suffix <- sprintf("COALESCE(REGEXP_EXTRACT(%s, '^[0-9]+([A-Z]?)', 1), '')", token)
  i_lot <- .score_value_sql(paste0(i, ".in_lot_number"))
  g_lot <- .score_value_sql(paste0(g, ".lot_number"))
  # Inclusive interval comparisons preserve exact ranges and also retrieve
  # individual addresses within an input range. Partial overlap is weakest.
  interval <- sprintf(paste0(
    "CASE WHEN %1$s IS NULL OR %2$s IS NULL OR %3$s < %1$s OR %4$s < %2$s THEN 0.0",
    " WHEN %1$s = %2$s AND %3$s = %4$s THEN 1.0",
    " WHEN %2$s <= %1$s AND %3$s <= %4$s THEN 0.7",
    " WHEN %1$s <= %2$s AND %4$s <= %3$s THEN 0.5",
    " WHEN %1$s <= %4$s AND %2$s <= %3$s THEN 0.3 ELSE 0.0 END"
  ), i_first, g_first, i_end, g_end)
  suffix <- sprintf(paste0(
    "CASE WHEN %1$s = %2$s THEN 1.0",
    " WHEN %1$s = '' THEN 0.5 ELSE 0.0 END"
  ), i_suffix, g_suffix)
  sprintf(paste0(
    "CAST(ROUND_EVEN(%g * (CASE WHEN %s != '' THEN ",
    "CASE WHEN %s = %s THEN 1.0 ELSE 0.0 END ",
    "ELSE (%s) * (%s) END), 0) AS INTEGER)"
  ), weight, i_lot, i_lot, g_lot, interval, suffix)
}

.score_number <- function(pairs, weight) {
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
  suffix <- data.table::fcase(i_suffix == g_suffix, 1, i_suffix == "", 0.5, default = 0)
  i_lot <- .score_value(.pair_column(pairs, "in_lot_number"))
  g_lot <- .score_value(.pair_column(pairs, "lot_number"))
  as.integer(round(weight * data.table::fifelse(
    i_lot != "", as.numeric(i_lot == g_lot), interval * suffix
  )))
}

# A direction qualifies the street identity without adding a seventh weight.
.score_street_type_sql <- function(weight, i, g) {
  i_type <- .score_value_sql(paste0(i, ".in_street_type"))
  g_type <- .score_value_sql(paste0(g, ".street_type"))
  i_suffix <- .score_mapped_value_sql(paste0(i, ".in_street_suffix"), .SCORE_DIRECTIONS)
  g_suffix <- .score_mapped_value_sql(paste0(g, ".street_suffix"), .SCORE_DIRECTIONS)
  type <- sprintf("CASE WHEN %1$s = %2$s THEN 1.0 WHEN %1$s = '' OR %2$s = '' THEN 0.5 ELSE 0.4 END", i_type, g_type)
  suffix <- sprintf("CASE WHEN %1$s = %2$s THEN 1.0 WHEN %1$s = '' OR %2$s = '' THEN 0.5 ELSE 0.0 END", i_suffix, g_suffix)
  sprintf("CAST(ROUND_EVEN(%g * (%s) * (%s), 0) AS INTEGER)", weight, type, suffix)
}

.score_street_type <- function(pairs, weight) {
  i_type <- .score_value(pairs$in_street_type)
  g_type <- .score_value(pairs$street_type)
  i_suffix <- .score_mapped_value(.pair_column(pairs, "in_street_suffix"), .SCORE_DIRECTIONS)
  g_suffix <- .score_mapped_value(.pair_column(pairs, "street_suffix"), .SCORE_DIRECTIONS)
  type <- data.table::fcase(i_type == g_type, 1, i_type == "" | g_type == "", 0.5, default = 0.4)
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

.score_flat <- function(pairs, weight) {
  value <- function(name) .score_value(.pair_column(pairs, name))
  i_flat <- value("in_flat_number")
  g_flat <- value("flat_number")
  i_level <- value("in_level_number")
  g_level <- value("level_number")
  flat <- .score_identifier(i_flat, g_flat,
    .score_mapped_value(value("in_flat_type"), .SCORE_FLAT_TYPES),
    .score_mapped_value(value("flat_type"), .SCORE_FLAT_TYPES))
  level <- .score_identifier(i_level, g_level,
    .score_mapped_value(value("in_level_type"), .SCORE_LEVEL_TYPES),
    .score_mapped_value(value("level_type"), .SCORE_LEVEL_TYPES))
  flat_present <- i_flat != "" | g_flat != ""
  level_present <- i_level != "" | g_level != ""
  # Divide the existing flat weight only when both dimensions carry evidence.
  fraction <- data.table::fcase(
    flat_present & level_present, 0.6 * flat + 0.4 * level,
    flat_present, flat,
    level_present, level,
    default = 1
  )
  as.integer(round(weight * fraction))
}

.score_flat_sql <- function(weight, i, g) {
  value <- function(alias, name) .score_value_sql(paste0(alias, ".", name))
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
    mapped(i, "in_flat_type", .SCORE_FLAT_TYPES), mapped(g, "flat_type", .SCORE_FLAT_TYPES))
  level <- identifier(i_level, g_level,
    mapped(i, "in_level_type", .SCORE_LEVEL_TYPES), mapped(g, "level_type", .SCORE_LEVEL_TYPES))
  flat_present <- sprintf("(%s != '' OR %s != '')", i_flat, g_flat)
  level_present <- sprintf("(%s != '' OR %s != '')", i_level, g_level)
  sprintf(paste0(
    "CAST(ROUND_EVEN(%g * (CASE WHEN %s AND %s THEN 0.6 * (%s) + 0.4 * (%s)",
    " WHEN %s THEN (%s) WHEN %s THEN (%s) ELSE 1.0 END), 0) AS INTEGER)"
  ), weight, flat_present, level_present, flat, level, flat_present, flat, level_present, level)
}
