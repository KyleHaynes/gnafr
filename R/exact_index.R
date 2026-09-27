# Exact-label index
#
# Most production batches are mostly addresses that already read like GNAF
# labels. Parsing, standardising and scoring each of those only to confirm what
# a hash lookup already shows is the dominant cost of a large gnaf_match() call.
# gnaf_exact_index maps a normalised label (and a few common written variants of
# it) straight to the address it names, so such inputs skip the parser and the
# scorer entirely.
#
# The index is deliberately conservative:
#   * principal rows only (alias_type IS NULL);
#   * a key that points at more than one address is dropped, so anything
#     ambiguous still goes through the full matcher;
#   * a key that is a genuine label always beats a variant of another label.

.EXACT_INDEX_VERSION <- 1L

# One key per string: upper case, commas and full stops removed, whitespace
# collapsed, spaces around "-" and "/" removed. The SQL form is used on both
# sides (index build and input lookup) so the two can never drift apart.
.exact_key_sql <- function(x) {
  sprintf(paste0("upper(trim(regexp_replace(regexp_replace(%s, ",
                 "'[,.\\s\\pZ]+', ' ', 'g'), '\\s*([-/])\\s*', '\\1', 'g')))"), x)
}

# Written short forms worth indexing, by canonical GNAF word. Only forms that
# the parser dictionaries map back to the same canonical word are kept (see
# the tests), so a variant key can never name a different street type.
.EXACT_STREET_ABBREV <- data.table::data.table(
  street_type = c("STREET", "ROAD", "DRIVE", "COURT", "AVENUE", "AVENUE",
                  "CRESCENT", "CRESCENT", "PLACE", "CIRCUIT", "CLOSE", "PARADE",
                  "TERRACE", "HIGHWAY", "LANE", "BOULEVARD", "BOULEVARD",
                  "ESPLANADE", "GROVE", "CIRCLE", "PARKWAY", "PROMENADE",
                  "SQUARE", "TRACK"),
  abbr        = c("ST", "RD", "DR", "CT", "AVE", "AV",
                  "CRES", "CR", "PL", "CCT", "CL", "PDE",
                  "TCE", "HWY", "LN", "BLVD", "BVD",
                  "ESP", "GR", "CIR", "PKWY", "PROM",
                  "SQ", "TRK")
)

# Leading flat words and their short forms.
.EXACT_FLAT_ABBREV <- c(UNIT = "U", APARTMENT = "APT", SUITE = "STE")

.exact_street_abbreviations <- function() {
  map <- .get_street_type_map()
  same <- unname(map[.EXACT_STREET_ABBREV$abbr]) == .EXACT_STREET_ABBREV$street_type
  data.table::copy(.EXACT_STREET_ABBREV[which(same)])
}

#' Rebuild the exact-label lookup index
#'
#' Rebuilds \code{gnaf_exact_index}, a table that maps normalised address labels
#' straight to the GNAF address they name. \code{gnaf_match()} consults it
#' before parsing anything, so inputs that are already GNAF labels (in any
#' case, with or without commas) are matched without being parsed, standardised
#' or scored, which is much faster for large batches.
#'
#' Each principal address contributes its label with commas and full stops
#' removed, plus (when \code{variants = TRUE}) the common written forms of it:
#' the flat word shortened (\code{UNIT 5 12 MAIN STREET} as \code{U 5 12 MAIN
#' STREET} and \code{5/12 MAIN STREET}), the street type shortened
#' (\code{MAIN ST}, \code{MAIN AVE}), and the state left out
#' (\code{... KEPERRA 4054}). A key that would identify more than one address
#' is left out, so ambiguous inputs are still resolved by the full matcher.
#'
#' The index is rebuilt automatically by \code{gnaf_load} and
#' \code{gnaf_load_psv}. Call this manually after modifying
#' \code{gnaf_addresses} yourself, or to add the index to a database built
#' before it existed. \code{gnaf_match()} ignores an index whose address count
#' no longer matches \code{gnaf_addresses}.
#'
#' @param con DBI connection from \code{gnaf_connect}, opened for writing.
#' @param variants If \code{TRUE} (default), also index the written variants
#'   described above. \code{FALSE} indexes only the labels themselves, which is
#'   smaller and faster to build.
#' @return Invisibly, the number of keys now in the index.
#' @export
gnaf_rebuild_exact_index <- function(con, variants = TRUE) {
  if (!is.logical(variants) || length(variants) != 1L || is.na(variants))
    stop("'variants' must be TRUE or FALSE", call. = FALSE)

  abbr <- .exact_street_abbreviations()
  duckdb::duckdb_register(con, "__gnafr_ix_abbr__", abbr, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_ix_abbr__"), silent = TRUE),
          add = TRUE)

  key <- .exact_key_sql
  flat_words <- names(.EXACT_FLAT_ABBREV)

  # Each stage adds forms to the ones before it; `kind` is a bit set recording
  # which variations were applied (0 = the label itself), so that a genuine
  # label always outranks a variant of some other address.
  street_stage <- if (variants) sprintf("
    UNION ALL
    SELECT p.pid,
           left(p.k0, p.pos - 1) || ' ' || p.nm || ' ' || a.abbr ||
             substr(p.k0, p.pos + length(p.anchor)) AS k,
           p.st, p.pc, 1 AS kind
    FROM located p
    JOIN __gnafr_ix_abbr__ a ON a.street_type = p.street_type
    WHERE p.pos > 0 AND substr(p.k0, p.pos + length(p.anchor), 1) IN ('', ' ')") else ""

  flat_stage <- if (variants) {
    words <- vapply(flat_words, function(w) sprintf("
    UNION ALL
    SELECT pid, '%s ' || substr(k, %d), st, pc, kind + 2
    FROM streets WHERE starts_with(k, '%s ')",
      .EXACT_FLAT_ABBREV[[w]], nchar(w) + 2L, w), character(1L))
    paste0(paste(words, collapse = ""), "
    UNION ALL
    SELECT pid, regexp_replace(k, '^UNIT ([0-9A-Z]+) ([0-9])', '\\1/\\2'), st, pc, kind + 2
    FROM streets WHERE regexp_matches(k, '^UNIT [0-9A-Z]+ [0-9]')")
  } else ""

  state_stage <- if (variants) "
    UNION ALL
    SELECT pid, left(k, length(k) - length(st) - length(pc) - 2) || ' ' || pc, st, pc, kind + 4
    FROM flats WHERE ends_with(k, ' ' || st || ' ' || pc)" else ""

  sql <- sprintf("
    CREATE OR REPLACE TABLE gnaf_exact_index AS
    WITH src AS (
      SELECT address_detail_pid AS pid,
             %s AS k0, %s AS nm, street_type,
             upper(state) AS st, lpad(CAST(postcode AS VARCHAR), 4, '0') AS pc
      FROM gnaf_addresses
      WHERE alias_type IS NULL AND address_label IS NOT NULL
    ),
    located AS (
      SELECT *, ' ' || nm || ' ' || street_type AS anchor,
             instr(k0, ' ' || nm || ' ' || street_type) AS pos
      FROM src
    ),
    streets AS (
      SELECT pid, k0 AS k, st, pc, 0 AS kind FROM located %s
    ),
    flats AS (
      SELECT pid, k, st, pc, kind FROM streets %s
    ),
    forms AS (
      SELECT DISTINCT pid, k, kind FROM (
        SELECT pid, k, st, pc, kind FROM flats %s
      ) WHERE k IS NOT NULL AND k <> ''
    ),
    best AS (SELECT k, min(kind) AS kind FROM forms GROUP BY k)
    SELECT f.k AS lookup_key, min(f.pid) AS address_detail_pid,
           CAST(f.kind AS UTINYINT) AS kind
    FROM forms f JOIN best b ON b.k = f.k AND b.kind = f.kind
    GROUP BY f.k, f.kind
    HAVING count(DISTINCT f.pid) = 1
    ORDER BY f.k",
    key("address_label"), key("street_name"), street_stage, flat_stage, state_stage)

  DBI::dbExecute(con, sql)
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TABLE gnaf_exact_index_meta AS
    SELECT (SELECT count(*) FROM gnaf_addresses WHERE alias_type IS NULL) AS n_principal,
           (SELECT count(*) FROM gnaf_exact_index) AS n_keys,
           %s AS variants, %d AS version, now() AS built_at",
    if (variants) "TRUE" else "FALSE", .EXACT_INDEX_VERSION))
  n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM gnaf_exact_index")$n
  invisible(n)
}

# NULL when the index is absent, from another version, or built for a different
# set of principal addresses than gnaf_addresses now holds (a stale index could
# name the wrong address, so it is never used).
.exact_index_state <- function(con) {
  if (!DBI::dbExistsTable(con, "gnaf_exact_index") ||
      !DBI::dbExistsTable(con, "gnaf_exact_index_meta")) return(NULL)
  meta <- tryCatch(
    DBI::dbGetQuery(con, "SELECT n_principal, variants, version FROM gnaf_exact_index_meta"),
    error = function(e) NULL)
  if (is.null(meta) || nrow(meta) != 1L ||
      !identical(as.integer(meta$version), .EXACT_INDEX_VERSION)) return(NULL)
  n <- DBI::dbGetQuery(con,
    "SELECT count(*) AS n FROM gnaf_addresses WHERE alias_type IS NULL")$n
  if (!identical(as.numeric(n), as.numeric(meta$n_principal))) return(NULL)
  list(variants = isTRUE(meta$variants))
}

# The score every component earns when the input is the address itself: each
# weight in full, produced by the real scorer so custom weights, rounding and
# any future component keep working.
.perfect_scores <- function(weights) {
  chr <- NA_character_
  int <- NA_integer_
  pair <- data.table::data.table(
    in_postcode = 4000L, postcode = 4000L, in_state = "QLD", state = "QLD",
    in_locality = "X", locality_name = "X",
    in_street_name = "Y", street_name = "Y",
    in_street_type = "STREET", street_type = "STREET",
    in_street_suffix = chr, street_suffix = chr,
    in_number_first = 1L, number_first = 1L,
    in_number_last = int, number_last = int, in_number_suffix = chr,
    in_flat_type = chr, flat_type = chr, in_flat_number = chr, flat_number = chr,
    in_level_type = chr, level_type = chr, in_level_number = chr, level_number = chr,
    in_lot_number = chr, lot_number = chr, in_building_name = chr, building_name = chr,
    address_label = "1 Y STREET, X QLD 4000")
  scored <- .score_pairs(pair, weights)
  as.list(scored[, c("score_postcode", "score_suburb", "score_street_name",
                     "score_street_type", "score_number", "score_flat",
                     "total_score"), with = FALSE])
}

# Look every input up in the index. Returns NULL when nothing hits, otherwise
# list(rows, parsed, ids): `rows` are complete match rows for the hit inputs
# (candidate columns, perfect scores, match_basis, match_rank), `parsed` the
# parse-shaped input table for them, `ids` their positions in `addresses`.
.exact_index_match <- function(con, addresses, index_state, include_custom,
                               min_score, weights, normalize) {
  x <- .repair_address_encoding(addresses)
  usable <- !is.na(x) & nzchar(trimws(x))
  if (!any(usable)) return(NULL)
  unique_raw <- unique(x[usable])
  inputs <- data.table::data.table(uq_id = seq_along(unique_raw), raw = unique_raw)
  duckdb::duckdb_register(con, "__gnafr_ix_in__", inputs, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_ix_in__"), silent = TRUE))

  kind_where <- if (isTRUE(normalize) && isTRUE(index_state$variants)) "" else
    "AND l.kind = 0"
  # A custom address with the same label competes with the GNAF one; leave
  # those inputs to the full matcher, which ranks both.
  custom_where <- if (isTRUE(include_custom) && .table_has_rows(con, "custom_addresses"))
    sprintf("AND NOT EXISTS (SELECT 1 FROM custom_addresses c WHERE %s = k.lookup_key)",
            .exact_key_sql("c.address_label")) else ""

  hits <- data.table::setDT(DBI::dbGetQuery(con, sprintf("
    WITH k AS (SELECT uq_id, %s AS lookup_key FROM __gnafr_ix_in__)
    SELECT k.uq_id, %s
    FROM k
    JOIN gnaf_exact_index l ON l.lookup_key = k.lookup_key %s
    JOIN gnaf_addresses g ON g.address_detail_pid = l.address_detail_pid
    WHERE g.alias_type IS NULL %s",
    .exact_key_sql("raw"), .GNAF_SELECT_COLS, kind_where, custom_where)))
  if (nrow(hits) == 0L) return(NULL)

  ids <- which(usable)
  ids <- ids[!is.na(match(match(x[ids], unique_raw), hits$uq_id))]
  if (length(ids) == 0L) return(NULL)
  rows <- hits[match(match(x[ids], unique_raw), hits$uq_id)]
  rows[, uq_id := NULL]

  scores <- .perfect_scores(weights)
  if (scores$total_score < min_score) return(NULL)
  for (nm in names(scores)) data.table::set(rows, j = nm, value = scores[[nm]])
  rows[, `:=`(match_basis = "exact_components", match_rank = 1L, input_id = ids)]

  list(rows = rows, parsed = .exact_hit_parsed(rows, addresses[ids], con, normalize),
       ids = ids)
}

# The parse-shaped description of an input that is exactly a GNAF label: the
# address's own components, normalised the way address_parse() normalises them.
.exact_hit_parsed <- function(rows, raw, con, normalize) {
  resources <- .get_parser_resources()
  on_unique <- function(x, f) {
    u <- unique(x)
    f(u)[match(x, u)]
  }
  canon <- function(x, map) on_unique(x, function(u) {
    m <- unname(map[u])
    data.table::fifelse(is.na(u) | is.na(m), u, m)
  })
  tidy <- function(x) on_unique(x, function(u) {
    out <- rep(NA_character_, length(u))
    ok <- !is.na(u)
    out[ok] <- .normalize_addr_keep_commas(u[ok])
    out
  })
  expand <- function(x, field) on_unique(x, function(u) {
    dt <- data.table::data.table(in_locality = u, in_street_name = u)
    .expand_abbreviations(dt)[[field]]
  })

  street <- tidy(rows$street_name)
  locality <- tidy(rows$locality_name)
  if (isTRUE(normalize)) {
    street <- expand(street, "in_street_name")
    locality <- expand(locality, "in_locality")
  }

  # Street-type-less GNAF rows carry the type inside street_name; give the
  # input the same name/type split the scorer uses for them.
  street_type <- rows$street_type
  missing_type <- is.na(street_type) & !is.na(rows$street_name)
  if (any(missing_type) && DBI::dbExistsTable(con, "gnaf_street_type_index")) {
    sti <- data.table::setDT(DBI::dbGetQuery(con,
      "SELECT street_name, effective_name, effective_type FROM gnaf_street_type_index"))
    at <- match(rows$street_name[missing_type], sti$street_name)
    found <- !is.na(at)
    idx <- which(missing_type)[found]
    street[idx] <- sti$effective_name[at[found]]
    street_type[idx] <- sti$effective_type[at[found]]
  }

  number_suffix <- rep(NA_character_, nrow(rows))
  maybe <- which(!is.na(rows$number_first) &
                   stringi::stri_detect_regex(rows$address_label, "[0-9][A-Z]"))
  if (length(maybe) > 0L) {
    token <- .candidate_number_token(rows[maybe, c("address_label", "street_name"), with = FALSE])
    letter <- sub("^[0-9]+([A-Z]?).*$", "\\1", token)
    number_suffix[maybe] <- data.table::fifelse(nzchar(letter), letter, NA_character_)
  }

  has_lot_word <- stringi::stri_detect_regex(rows$address_label, "\\bLOT\\b")
  lot <- data.table::fifelse(has_lot_word, rows$lot_number, NA_character_)

  data.table::data.table(
    input_id = rows$input_id, input_raw = raw,
    in_postcode = rows$postcode, in_state = rows$state, in_locality = locality,
    in_street_name = street, in_street_type = street_type,
    in_street_suffix = rows$street_suffix,
    in_number_first = rows$number_first, in_number_last = rows$number_last,
    in_number_suffix = number_suffix,
    in_flat_type = canon(rows$flat_type, resources$ft_map),
    in_flat_number = rows$flat_number,
    in_level_type = canon(rows$level_type, resources$level_map),
    in_level_number = rows$level_number, in_lot_number = lot,
    in_building_name = tidy(rows$building_name),
    input_standardised = rows$address_label
  )
}
