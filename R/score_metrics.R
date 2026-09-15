# DuckDB's edit and Jaro-Winkler functions compare UTF-8 bytes. Use the same
# representation in R, including the denominator, to keep both paths identical.
.score_edit_similarity <- function(input, candidate) {
  1 - stringdist::stringdist(enc2utf8(input), enc2utf8(candidate),
    method = "dl", useBytes = TRUE) /
    pmax(1L, nchar(enc2utf8(input), type = "bytes"),
         nchar(enc2utf8(candidate), type = "bytes"))
}

.score_edit_similarity_sql <- function(input, candidate) {
  sprintf(paste0("(1.0 - damerau_levenshtein(%1$s, %2$s) / ",
    "GREATEST(1, OCTET_LENGTH(ENCODE(%1$s)), OCTET_LENGTH(ENCODE(%2$s))))"),
    input, candidate)
}

.score_name_value <- function(x) {
  trimws(gsub("[[:space:]]+", " ", .score_value(x)))
}

.score_name_value_sql <- function(x) {
  sprintf("TRIM(REGEXP_REPLACE(%s, '[[:space:]]+', ' ', 'g'))", .score_value_sql(x))
}

# A common prefix is evidence, but edit distance also accounts for the rest of
# the name. The squared edit similarity rewards small edits near the beginning
# without giving unrelated names the Jaro-Winkler noise floor as useful credit.
.NAME_JW_SHARE <- 0.5
.NAME_DIRECTION_CONFLICT <- 0.5

.name_similarity_factor <- function(input, candidate) {
  jw <- fast.string::jaro_winkler(enc2utf8(input), enc2utf8(candidate), p = 0.1)
  factor <- pmax(.COMPONENT_SIM_FLOOR,
    .NAME_JW_SHARE * .component_similarity_factor(jw) +
    (1 - .NAME_JW_SHARE) * .score_edit_similarity(input, candidate)^2)
  conflict <- rep(FALSE, length(input))
  for (axis in list(c("NORTH", "SOUTH"), c("EAST", "WEST"))) {
    direction <- function(x) {
      as.integer(grepl(paste0("(^| )", axis[1L], "( |$)"), x)) -
        as.integer(grepl(paste0("(^| )", axis[2L], "( |$)"), x))
    }
    conflict <- conflict | direction(input) * direction(candidate) < 0L
  }
  factor * ifelse(conflict, .NAME_DIRECTION_CONFLICT, 1)
}

.name_similarity_sql <- function(input, candidate) {
  input <- .score_name_value_sql(input)
  candidate <- .score_name_value_sql(candidate)
  jw <- sprintf("jaro_winkler_similarity(%s, %s)", input, candidate)
  conflicts <- vapply(list(c("NORTH", "SOUTH"), c("EAST", "WEST")), function(axis) {
    direction <- function(x) sprintf(paste0(
      "(CAST(REGEXP_MATCHES(%1$s, '(^| )%2$s( |$)') AS INTEGER) - ",
      "CAST(REGEXP_MATCHES(%1$s, '(^| )%3$s( |$)') AS INTEGER))"), x, axis[1L], axis[2L])
    sprintf("%s * %s < 0", direction(input), direction(candidate))
  }, character(1L))
  sprintf(paste0(
    "CASE WHEN %1$s = '' OR %2$s = '' THEN 0.0 WHEN %1$s = %2$s THEN 1.0 ELSE ",
    "GREATEST(%3$g, %4$g * %5$s + %6$g * POWER(%7$s, 2)) * ",
    "CASE WHEN %8$s THEN %9$g ELSE 1.0 END END"),
    input, candidate, .COMPONENT_SIM_FLOOR, .NAME_JW_SHARE,
    .component_similarity_sql(jw), 1 - .NAME_JW_SHARE,
    .score_edit_similarity_sql(input, candidate),
    paste(conflicts, collapse = " OR "), .NAME_DIRECTION_CONFLICT)
}

# Ignore zero padding on numeric identifiers, preserving alphabetic suffixes
# and compound identifiers. Similar-looking but different numbers stay distinct.
.score_identifier_value <- function(x) {
  sub("^0+([0-9]+[A-Z]?)$", "\\1", .score_value(x))
}

.score_identifier_value_sql <- function(x) {
  x <- .score_value_sql(x)
  sprintf("REGEXP_REPLACE(%s, '^0+([0-9]+[A-Z]?)$', '\\1')", x)
}

.score_postcode <- function(input, candidate, weight) {
  input <- suppressWarnings(as.integer(input))
  candidate <- suppressWarnings(as.integer(candidate))
  valid <- !is.na(input) & !is.na(candidate) & input >= 0L & input <= 9999L &
    candidate >= 0L & candidate <= 9999L
  difference <- abs(input - candidate)
  factor <- data.table::fcase(valid & difference == 0L, 1,
    valid & difference == 1L, 0.7, valid & difference == 2L, 0.4,
    valid & difference == 3L, 0.2, default = 0)
  fuzzy <- which(valid & difference > 3L & input %/% 1000L == candidate %/% 1000L)
  if (length(fuzzy)) {
    left <- sprintf("%04d", input[fuzzy])
    right <- sprintf("%04d", candidate[fuzzy])
    # Only a single adjacent digit swap, with the leading digit unchanged.
    swap <- stringdist::stringdist(left, right, method = "dl") == 1 &
      stringdist::stringdist(left, right, method = "lv") == 2
    factor[fuzzy[swap]] <- 0.4
  }
  as.integer(round(weight * factor))
}

.score_postcode_sql <- function(input, candidate, weight) {
  input <- sprintf("TRY_CAST(%s AS INTEGER)", input)
  candidate <- sprintf("TRY_CAST(%s AS INTEGER)", candidate)
  left <- sprintf("LPAD(CAST(%s AS VARCHAR), 4, '0')", input)
  right <- sprintf("LPAD(CAST(%s AS VARCHAR), 4, '0')", candidate)
  sprintf(paste0(
    "CASE WHEN %1$s IS NULL OR %2$s IS NULL OR %1$s NOT BETWEEN 0 AND 9999 ",
    "OR %2$s NOT BETWEEN 0 AND 9999 THEN 0 ",
    "WHEN %1$s = %2$s THEN %3$d WHEN ABS(%1$s - %2$s) = 1 THEN %4$d ",
    "WHEN ABS(%1$s - %2$s) = 2 THEN %5$d WHEN ABS(%1$s - %2$s) = 3 THEN %6$d ",
    "WHEN FLOOR(%1$s / 1000.0) = FLOOR(%2$s / 1000.0) ",
    "AND damerau_levenshtein(%7$s, %8$s) = 1 AND levenshtein(%7$s, %8$s) = 2 ",
    "THEN %5$d ELSE 0 END"), input, candidate,
    as.integer(round(weight)), as.integer(round(weight * 0.7)),
    as.integer(round(weight * 0.4)), as.integer(round(weight * 0.2)), left, right)
}
