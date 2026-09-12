# gnafr

[![R-CMD-check](https://github.com/KyleHaynes/graphfast/workflows/R-CMD-check/badge.svg)](https://github.com/KyleHaynes/graphfast/actions)
[![Status](https://img.shields.io/badge/status-development-orange)](https://github.com/KyleHaynes/graphfast)

Fast, fuzzy Australian address matching against the [Geocoded National Address File
(G-NAF)](https://geoscape.com.au/data/g-naf/), backed by an embedded DuckDB database.

- **Bulk matching** — 100k+ addresses in a single call
- **Fuzzy matching** — handles abbreviations, typos, missing fields, and messy real-world strings
- **Confidence scoring** — transparent 0–100 score with per-component breakdown
- **Custom addresses** — add your own records and match them alongside G-NAF

Full documentation: <https://kylehaynes.github.io/gnafr>

---

## Installation

```r
devtools::install_github("KyleHaynes/gnafr")
```

---

## Getting G-NAF data

G-NAF is published by [Geoscape](https://geoscape.com.au/data/g-naf/) in two forms:

- **G-NAF Standard** (recommended) — the full pipe-delimited product: `ADDRESS_DETAIL`, `ADDRESS_ALIAS`, `STREET_LOCALITY_ALIAS`, `LOCALITY_ALIAS`, and more. Use this to get official locality/street *alias* records and the complete set of published columns.
- **G-NAF Core** — a simplified, flattened CSV per state/national extract. Smaller and simpler if you don't need the official alias tables.

gnafr can load either: `gnaf_build_db()` / `gnaf_load_psv()` for Standard, `gnaf_load()` for Core.

---

## Building the database

One call goes from a fresh DuckDB file to a match-ready database. This loads
Queensland from the **G-NAF Standard** product:

```r
library(gnafr)

con <- gnaf_connect("C:/temp/gnafx23.duckdb")

gnaf_build_db(
  con,
  gnaf_dir = "C:/temp/gnaf/G-NAF/G-NAF MAY 2026/Standard",
  states = "QLD"              # or a vector c("QLD","NSW"), or "all"
)
```

`gnaf_build_db()` runs `gnaf_init()`, `gnaf_load_psv()` and
`gnaf_build_street_aliases()` in sequence and prints a `gnaf_status()` summary.
The DuckDB file persists between sessions — build once, then reconnect with
`gnaf_connect()`.

### Using G-NAF Core instead

With the G-NAF Core CSV rather than the Standard PSV files, use `gnaf_load()`:

```r
gnaf_load(con, "C:/temp/gnaf.qld.csv")
```

`gnaf_load()` reads the CSV straight into DuckDB — the file is never pulled into
R. Pass a vector of paths for multiple states; `overwrite = TRUE` wipes and
reloads. The two paths are alternatives: pick one per database.

---

## Matching addresses

```r
library(gnafr)
con <- gnaf_connect("C:/temp/gnafx23.duckdb")

addresses <- c(
  "unit 110 120 musgrave Road red hill 4000 QLD",
  "18-20 drift cl goldsborough QLD 4865",
  "77 broadwater rd mount gravatt east 4122"
)

results <- gnaf_match(addresses, con, max_results = 1, min_score = 60)
```

`gnaf_match()` returns a `data.table` with one row per match: the matched G-NAF
fields (`address_detail_pid`, `address_label`, `longitude`, `latitude`, …) plus a
0–100 `total_score` and per-component scores (`score_postcode`, `score_suburb`,
`score_street_name`, `score_street_type`, `score_number`, `score_flat`). Inputs
with no match above `min_score` are retained with `matched = FALSE`.

Key arguments:

| Argument | Default | Effect |
|---|---|---|
| `max_results` | `1` | Return up to N candidates per input, ranked best first. |
| `min_score` | `60` | Drop candidates scoring below this. |
| `include_aliases` | `TRUE` | Include locality/street synonyms and official G-NAF alias records. |
| `resolve_principal` | `FALSE` | Add `principal_*` columns resolving alias matches back to their canonical address. |
| `return_principal` | `FALSE` | Return the full non-alias address linked by `principal_pid`. |
| `return_primary` | `FALSE` | Return the full primary address linked by `primary_pid`, after principal resolution if enabled. |
| `geographies` | `NULL` | Append saved geography attributes by registered name, or use `TRUE` for all available layers. |
| `weights` | defaults | Named list of score weights summing to 100. |

Matching details and review findings are in [MATCHING_REVIEW.md](MATCHING_REVIEW.md).
Scores measure component agreement, rather than a probability of correctness.
Use `max_results > 1` to inspect alternatives. Principal/primary options follow
stored PID relationships after ranking and preserve the original address in
`matched_*` columns; they do not depend on the fallback threshold.

### Saved geographies

Register an existing enrichment table once, then request it directly in matches:

```r
gnaf_register_geography(con, "sa2_2021", "gnaf_sa2_2021", points_crs = 7844)
gnaf_list_geographies(con)
gnaf_geography_coverage(con, "sa2_2021")
results <- gnaf_match(addresses, con, geographies = "sa2_2021")
```

Use `gnaf_add_geography()` to calculate a new layer from polygons, or
`gnaf_join_geographies()` to enrich existing results. `gnaf_remove_geography()`
removes a layer and its registration. See [example_usage.MD](example_usage.MD)
for the complete SA2 add, match, coverage and removal demo.

### Shiny geocoder

Launch an interactive geocoding app against the same DuckDB database:

```r
gnaf_app(db_path = "C:/temp/gnafx23.duckdb")
# or reuse an existing connection:
gnaf_app(con = con)
```

### Threshold filter (find false positives)

`gnaf_threshold_filter()` opens a smaller app over an existing `gnaf_match()`
result. Drag a range on any score - the component scores, plus the
Jaro-Winkler, Jaccard and Levenshtein text similarities - to split the rows
into in-scope and out-of-scope tables (each showing the input/matched diff),
flag individual inputs as false positives, and copy the equivalent
`data.table` or `dplyr` filter. Pressing "Done" (or closing the window) prints
that filter to the console so you can paste it straight into your script:

```r
results <- gnaf_match(addresses, con)
gnaf_threshold_filter(results)
#> ## data.table
#> # in scope
#> results[matched == TRUE & total_score >= 80 & score_street_name >= 30]
#> ...
```

The text similarity columns come from `gnaf_text_scores()`; when a threshold on
one of them is active the printed code wraps the result in that call
(`gnaf_text_scores(results)[... & jarowinkler_score >= 85]`) so it runs as-is.

For large results (hundreds of thousands of rows), the window opens immediately
and a progress bar shows while `gnaf_text_scores()` runs in the background,
threshold sliders are debounced so dragging one only recomputes once you pause,
and the score-distribution histogram is drawn from a sample. Pass
`text_scores = FALSE` to skip the text-score computation entirely if you only
need the component scores, and lower `max_rows` (default `200`, applies to the
two tables only - counts and the generated filter always cover every row) for
snappier redraws on a slow machine.

---

## Custom addresses

Addresses not (yet) in G-NAF — new developments, PO boxes, corrections — can be
added and matched alongside G-NAF transparently:

```r
gnaf_add(con, data.table(
  number_first = 1L, street_name = "EXAMPLE", street_type = "STREET",
  locality_name = "SAMPLETON", state = "QLD", postcode = 4999L
))

gnaf_remove_custom(con, "CUSTOM_1")
```

### Bulk custom import

```r
custom_bulk <- fread("C:/temp/my_custom_addresses.csv")

# Ensure required columns exist and types are correct
custom_bulk[, number_first := as.integer(number_first)]
custom_bulk[, postcode     := as.integer(postcode)]

gnaf_add(con, custom_bulk)
#> Inserted 4,832 custom address(es). Total custom: 4,832.
```

Custom addresses are stored in the same DuckDB file as GNAF data and persist across sessions.

---

## Working with results

### Extracting the best match per input

```r
best <- results[match_rank == 1]
```

### Filtering by confidence

```r
# Only high-confidence matches for automated processing
high_conf <- results[match_rank == 1 & total_score >= 80]

# Flag low-confidence for manual review
results[, needs_review := total_score < 60]
```

### Identifying unmatched inputs

```r
matched_ids   <- unique(results[matched == TRUE, input_id])
unmatched_ids <- setdiff(seq_along(addresses), matched_ids)

cat(sprintf("%d of %d inputs had no match above min_score\n",
            length(unmatched_ids), length(addresses)))
```

### Joining coordinates back to your data

```r
dt_in[, input_id := .I]

geo <- results[match_rank == 1, .(input_id, total_score, longitude, latitude,
                                   address_label, address_detail_pid)]

dt_out <- geo[dt_in, on = "input_id"]
```

### Inspecting score breakdown for diagnostics

```r
# Addresses where postcode matched but street name didn't
suspect <- results[score_postcode == 20 & score_street_name < 20]

# All components for a specific input
results[input_id == 42, .(match_rank, total_score, score_postcode, score_suburb,
                           score_street_name, score_street_type, score_number,
                           score_flat, address_label)]
```

---

## Performance notes

### Typical throughput

| Input size | Estimated time |
|------------|---------------|
| 1,000 | < 1 second |
| 10,000 | 2–5 seconds |
| 100,000 | 15–45 seconds |
| 500,000 | 2–5 minutes (chunk recommended) |

Times assume a laptop with SSD and ~3M GNAF records for QLD. Results vary with CPU, postcode spread, and proportion of addresses without postcodes.

### What drives performance

**Parsing** — parsing is vectorised and repeated normalised inputs are parsed once. Keep repeated values in the same call so they share this work.

**Postcode spread** — if 100k addresses all share one postcode, the broad fallback join can be large (100k × 2000 GNAF records = 200M pairs). Prefer the tight join path by ensuring street numbers parse correctly.

**DB I/O** — exact number, range, lot, and missing-number branches reduce candidate cardinality before fuzzy scoring. Keep DuckDB statistics current with `ANALYZE` after out-of-band bulk loads.

### Keeping the connection open

Re-using a single connection across multiple `gnaf_match` calls is faster than reconnecting each time. For Shiny apps or API services, keep `con` in a global or module-level variable.

---

## Function reference

| Task | Functions |
|---|---|
| Connect & setup | `gnaf_connect()`, `gnaf_disconnect()`, `gnaf_init()`, `gnaf_status()`, `sample_gnaf()` |
| Load G-NAF | `gnaf_build_db()`, `gnaf_load_psv()` (Standard), `gnaf_load()` (Core CSV) |
| Match | `gnaf_match()`, `gnaf_text_scores()` |
| Parse | `address_parse()` |
| Custom addresses | `gnaf_add()`, `gnaf_remove_custom()` |
| Maintenance | `gnaf_canonicalize_street_types()`, `gnaf_build_street_aliases()`, `gnaf_rebuild_locality_index()` |
| Match cache | `gnaf_cache_status()`, `gnaf_cache_history()`, `gnaf_cache_sample()`, `gnaf_cache_rollback()`, `gnaf_cache_clear()` |
| Spatial & app | `gnaf_app()`, `gnaf_threshold_filter()`, `spatial_lookup()`, `plot_boundaries_heatmap()`, `read_shapefile()`, `subset_shapefile()` |
| Testing | `address_perturb_sample()` |

---

## Documentation

The full manual — how scoring works, the matching paths, a detailed database
build walkthrough, and troubleshooting — is at <https://kylehaynes.github.io/gnafr>.
