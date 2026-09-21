# Exact identity is deliberately independent of score weights and rounding.
# These field rules are shared by the SQL and R comparisons below.
.identity_fields <- function() {
  list(
    street_name = list(candidate = "street_name", required = TRUE),
    locality = list(candidate = "locality_name", required = TRUE),
    state = list(candidate = "state", supplied = TRUE),
    street_type = list(candidate = "street_type", supplied = TRUE, map = .get_street_type_map()),
    street_suffix = list(candidate = "street_suffix", map = .SCORE_DIRECTIONS),
    flat_number = list(candidate = "flat_number", identifier = TRUE),
    level_number = list(candidate = "level_number", identifier = TRUE),
    flat_type = list(candidate = "flat_type", map = .get_score_flat_type_map()),
    level_type = list(candidate = "level_type", map = .get_score_level_type_map()),
    lot_number = list(candidate = "lot_number", supplied = TRUE, identifier = TRUE),
    building_name = list(candidate = "building_name", supplied = TRUE)
  )
}

.identity_mapped_value_sql <- function(x, map) {
  value <- .score_value_sql(x)
  map <- map[!duplicated(names(map)) & names(map) != unname(map)]
  literal <- function(x) paste0("[", paste0("'", gsub("'", "''", x), "'", collapse = ","), "]")
  # Constant map lookup avoids expanding hundreds of CASE branches each time
  # the identity predicate is used in both filtering and ranking.
  sprintf("COALESCE(list_extract(map_extract(MAP(%s, %s), %s), 1), %s)",
          literal(names(map)), literal(unname(map)), value, value)
}

.address_identity_sql <- function(i = "i", g = "g") {
  fields <- .identity_fields()
  comparisons <- vapply(names(fields), function(name) {
    rule <- fields[[name]]
    normalise <- function(x) {
      if (!is.null(rule$map)) return(.identity_mapped_value_sql(x, rule$map))
      if (isTRUE(rule$identifier)) return(.score_identifier_value_sql(x))
      .score_name_value_sql(x)
    }
    left <- normalise(paste0(i, ".in_", name))
    right <- normalise(paste0(g, ".", rule$candidate))
    equal <- sprintf("%s = %s", left, right)
    if (isTRUE(rule$required)) equal <- sprintf("%s != '' AND (%s)", left, equal)
    if (isTRUE(rule$supplied)) equal <- sprintf("%s = '' OR (%s)", left, equal)
    paste0("(", equal, ")")
  }, character(1L))
  token <- .candidate_number_token_sql(g)
  first <- sprintf("COALESCE(%s.number_first, TRY_CAST(REGEXP_EXTRACT(%s, '^[0-9]+') AS INTEGER))", g, token)
  suffix <- sprintf(paste0("CASE WHEN REGEXP_MATCHES(UPPER(%s.address_label), '[0-9][A-Z]') ",
    "THEN COALESCE(REGEXP_EXTRACT(%s, '^[0-9]+([A-Z]?)', 1), '') ELSE '' END"), g, token)
  number <- sprintf(paste0("%1$s.in_number_first IS NOT NULL AND ",
    "COALESCE(%1$s.in_number_last, %1$s.in_number_first) >= %1$s.in_number_first AND ",
    "%1$s.in_number_first = %2$s AND ",
    "COALESCE(%1$s.in_number_last, %1$s.in_number_first) = COALESCE(%3$s.number_last, %2$s) AND ",
    "%4$s = %5$s"), i, first, g, .score_value_sql(paste0(i, ".in_number_suffix")), suffix)
  paste(c(paste0("COALESCE((", number, "), FALSE)"), comparisons), collapse = " AND ")
}

.address_identity <- function(pairs) {
  exact <- !is.na(pairs$in_number_first) & .score_number(pairs, 100) == 100L
  fields <- .identity_fields()
  for (name in names(fields)) {
    rule <- fields[[name]]
    normalise <- function(x) {
      if (!is.null(rule$map)) return(.score_mapped_value(x, rule$map))
      if (isTRUE(rule$identifier)) return(.score_identifier_value(x))
      .score_name_value(x)
    }
    left <- normalise(.pair_column(pairs, paste0("in_", name)))
    right <- normalise(.pair_column(pairs, rule$candidate))
    equal <- left == right
    if (isTRUE(rule$required)) equal <- left != "" & equal
    if (isTRUE(rule$supplied)) equal <- left == "" | equal
    exact <- exact & equal
  }
  exact %in% TRUE
}

.set_exact_match_basis <- function(pairs, con) {
  effective <- data.table::copy(pairs)
  if (anyNA(effective$street_type) && DBI::dbExistsTable(con, "gnaf_street_type_index")) {
    sti <- data.table::as.data.table(DBI::dbGetQuery(con,
      "SELECT street_name, effective_name, effective_type FROM gnaf_street_type_index"))
    effective[sti, on = "street_name",
      `:=`(street_name = fifelse(is.na(street_type), i.effective_name, street_name),
           street_type = fifelse(is.na(street_type), i.effective_type, street_type))]
  }
  exact <- .address_identity(effective) & !is.na(pairs$in_postcode) &
    !is.na(pairs$postcode) & pairs$in_postcode == pairs$postcode
  pairs[, match_basis := fifelse(exact, "exact_components", "weighted")]
  pairs
}

.order_address_matches <- function(matches) {
  matches[, .identity_rank := match(match_basis, c("exact_components", "postcode_only", "weighted"))]
  setorder(matches, input_id, .identity_rank, -total_score, address_detail_pid)
  matches[, .identity_rank := NULL]
  matches
}

# Search every exact locality postcode before applying score/top-N filters.
# Otherwise a hidden second address could make a correction appear unique.
.match_postcode_identity_duckdb <- function(con, inputs, max_results, min_score,
                                            weights, include_custom, verbose,
                                            alias_types = NULL) {
  inputs <- inputs[!is.na(in_postcode) & !is.na(in_number_first) &
                     !is.na(in_street_name) & nzchar(in_street_name) &
                     !is.na(in_locality) & nzchar(in_locality)]
  if (!nrow(inputs)) return(.empty_path_result())
  duckdb::duckdb_register(con, "__gnafr_identity_inputs__", inputs, overwrite = TRUE)
  on.exit(duckdb::duckdb_unregister(con, "__gnafr_identity_inputs__"), add = TRUE)
  mapping <- data.table::as.data.table(DBI::dbGetQuery(con, "
    SELECT DISTINCT i.input_id, l.postcode AS alt_postcode
    FROM __gnafr_identity_inputs__ i JOIN gnaf_locality_index l
      ON l.locality_name = i.in_locality
     AND (i.in_state IS NULL OR l.state = i.in_state)"))
  if (!nrow(mapping)) return(.empty_path_result())
  expanded <- inputs[mapping, on = "input_id", nomatch = 0L, allow.cartesian = TRUE]
  # A locality confined to the supplied postcode was already searched. When
  # an alternative exists, retain the supplied postcode too for uniqueness.
  search_ids <- expanded[alt_postcode != in_postcode, input_id]
  expanded <- expanded[input_id %in% search_ids]
  if (!nrow(expanded)) return(.empty_path_result())
  duckdb::duckdb_register(con, "__gnafr_identity_expanded__", expanded, overwrite = TRUE)
  on.exit(duckdb::duckdb_unregister(con, "__gnafr_identity_expanded__"), add = TRUE)
  alias_sql <- .alias_type_sql(alias_types)
  pre_filter <- paste(.number_prefilter_sql(),
    if (!is.null(alias_sql)) paste("AND", alias_sql) else "")
  tables <- c("gnaf_addresses", if (include_custom && .table_has_rows(con, "custom_addresses")) "custom_addresses")
  paths <- lapply(tables, function(table) .run_duckdb_score_query(
    con, "__gnafr_identity_expanded__", table,
    "g.postcode = i.alt_postcode AND g.locality_name = i.in_locality AND (i.in_state IS NULL OR g.state = i.in_state)",
    pre_filter, weights, .Machine$integer.max, 0L, verbose,
    label = paste(table, "(exact identity across postcodes)"), identity_only = TRUE))
  rows <- rbindlist(lapply(paths, `[[`, "matches"), fill = TRUE)
  if (!nrow(rows)) return(.empty_path_result())
  # Principal aliases share an address; primary_pid is intentionally NOT used:
  # a secondary dwelling or unit remains a distinct address.
  rows[, .address_key := fifelse(!is.na(alias_type) & !is.na(principal_pid) & nzchar(principal_pid),
                               principal_pid, address_detail_pid)]
  rows[, .identity_count := uniqueN(.address_key), by = input_id]
  rows[match_basis != "exact_components" & .identity_count == 1L,
       match_basis := "postcode_only"]
  rows[, c(".address_key", ".identity_count") := NULL]
  diagnostics <- rows[, .(candidate_count = .N,
    retained_count = sum(total_score >= min_score), best_score = max(total_score)), by = input_id]
  rows <- rows[total_score >= min_score]
  .order_address_matches(rows)
  rows <- unique(rows, by = c("input_id", "address_detail_pid"))
  rows <- rows[rows[, .I[seq_len(min(.N, max_results))], by = input_id]$V1]
  rows[, match_rank := seq_len(.N), by = input_id]
  list(matches = rows, diagnostics = diagnostics)
}
