# gnafr

[![R-CMD-check](https://github.com/KyleHaynes/graphfast/workflows/R-CMD-check/badge.svg)](https://github.com/KyleHaynes/graphfast/actions)
[![Status](https://img.shields.io/badge/status-development-orange)](https://github.com/KyleHaynes/graphfast)

Fast, fuzzy Australian address matching against the [Geocoded National Address File
(G-NAF)](https://geoscape.com.au/data/g-naf/), backed by an embedded DuckDB database.

- **Bulk matching** — 100k+ addresses in a single call
- **Fuzzy matching** — handles abbreviations, typos, missing fields, and messy real-world strings
- **Agreement scoring** — transparent 0–100 score with per-component breakdown
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

### Smaller lookup tables for shared primary/secondary coordinates

Pass `collapse_same_coordinates = TRUE` when loading to remove GNAF secondaries
whose linked primary has exactly the same longitude and latitude:

```r
gnaf_build_db(con, "C:/temp/gnaf/Standard", states = "QLD",
              collapse_same_coordinates = TRUE)
# For Core CSV instead:
gnaf_load(con, "C:/temp/gnaf.qld.csv", collapse_same_coordinates = TRUE)
```

The option also works on `gnaf_load_psv()` and defaults to `FALSE`. It removes
aliases of collapsed secondaries, preserves the primary and its aliases, and
leaves custom addresses and records with missing links or coordinates untouched.
Coordinates must match exactly; unrelated addresses at the same point are retained.
CSV loading applies it to all loaded GNAF rows; PSV loading applies it to each
requested state. Indexes are rebuilt and the match cache is cleared automatically.

This reduces the rows searched by matching, potentially improving lookup speed,
but gives up unit-level detail and can change match scores or coverage. Reload the
source files with the option off to restore removed records. To keep all records
and only change the returned address, use `gnaf_match(return_primary = TRUE)`;
that existing option does not reduce the lookup table or require equal coordinates.

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

The six scoring buckets use evidence suited to each field:

| Bucket | Default weight | Comparison |
|---|---:|---|
| Postcode | 20 | Exact, existing numeric proximity tiers, limited adjacent-digit transposition credit |
| Suburb | 15 | Equal blend of reshaped Jaro–Winkler and squared normalised Damerau–Levenshtein similarity; opposing directions halve credit |
| Street name | 40 | Same name metrics; full credit requires equal normalised text |
| Street type | 10 | Canonical abbreviations, missing/conflicting type tiers, explicit direction checks |
| Number | 10 | Exact numbers, interval overlap, suffix checks; zero-padded lot identifiers normalised |
| Flat | 5 | Separate unit/level agreement, canonical types and zero-padding normalisation |

See [the scoring formulas](docs/03-theory.qmd#sec-scoring) for details. These are
agreement scores; the metric blend has not been calibrated as a probability.

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

Matching details and review findings are in [MATCHING_REVIEW.md](reports/MATCHING_REVIEW.md).
Scores measure component agreement, rather than a probability of correctness.
Even 100 is not proof of a correct or unique match. When both a lot and street
number are supplied, the street number controls candidate retrieval and number
scoring; the lot is used for these only when the street number is absent.
Use `max_results > 1` to inspect alternatives. Principal/primary options follow
stored PID relationships after ranking and preserve the original address in
`matched_*` columns; they do not depend on the fallback threshold.

Use `gnaf_match_features(results)` to inspect separate number, lot, unit, level,
state and street-direction conflicts, missing evidence, both text comparisons,
and ties among returned candidates. The [scoring overhaul workflow](reports/SCORING_OVERHAUL.md)
provides a logistic baseline and validation process for verified historical data.
No probability model is fitted or enabled by default.

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
removes a layer and its registration. See [example_usage.MD](reports/example_usage.MD)
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
The default compares **raw input after basic text normalisation** with the
matched label; it does not expand `RD` to `ROAD`. Use
`gnaf_text_scores(results, input = "standardised")` for reconstructed parsed
input. Whole-address text scores are diagnostics calculated after matching and
do not affect the current ranking.

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

### Reviewing agreement and conflicts

```r
# Inspect structural conflicts and ties, even when total_score is high.
evidence <- gnaf_match_features(results)
review <- evidence[has_identifier_conflict == TRUE | tied_best == TRUE]
```

This review filter is not an automatic acceptance rule. Determine acceptance
thresholds from independently verified data, with a held-out test set.

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

`address_parse()` preserves `input_raw` while repairing malformed encoding,
pasted whitespace, and spacing around ranges and slash numbers. For example,
`Unit 3 40/B Smith St` retains unit `3` and street number `40B`. These structural
repairs also run with `normalize = FALSE`; that option controls abbreviation
expansion. Commas between street and suburb help resolve ambiguous names and
directions. Locality spelling corrections such as `Rocky View` to `Rockyview`
are left to matching against reference addresses.

**Postcode spread** — if 100k addresses all share one postcode, the broad fallback join can be large (100k × 2000 GNAF records = 200M pairs). Prefer the tight join path by ensuring street numbers parse correctly.

**DB I/O** — exact number, range, lot, and missing-number branches reduce candidate cardinality before fuzzy scoring. Keep DuckDB statistics current with `ANALYZE` after out-of-band bulk loads.

### Keeping the connection open

Re-using a single connection across multiple `gnaf_match` calls is faster than reconnecting each time. For Shiny apps or API services, keep `con` in a global or module-level variable.

### If `gnaf_load()` / `gnaf_load_psv()` crashes the R session

A hard crash of R (RStudio's "R Session Aborted" bomb, not an R error) during a
bulk load is almost always DuckDB running out of memory — a DuckDB
out-of-memory abort takes the whole R process down rather than raising a
catchable error. Two DuckDB defaults make this machine-dependent:

- **`memory_limit`** defaults to 80% of *physical* RAM. On a locked-down work
  machine, endpoint security agents, mandated antivirus, and other corporate
  software can already hold enough memory that 80% of physical RAM simply
  isn't available — DuckDB over-allocates and the OS kills the process. More
  installed RAM does not help if less of it is free.
- **`threads`** defaults to every logical core. Each parallel pipeline holds
  its own buffers, so a many-core work machine has a much higher peak memory
  footprint for the *same* load than a smaller home machine.

Fix: cap both at connect time and give DuckDB somewhere to spill:

```r
con <- gnaf_connect(
  "C:/temp/gnaf.duckdb",
  memory_limit   = "4GB",      # well under the machine's *free* RAM
  threads        = 4,
  temp_directory = "C:/temp/duckdb_spill"  # local, unsynced, unscanned disk
)
```

The loaders also disable DuckDB's `preserve_insertion_order` for the duration
of the load, which removes the largest single memory spike of
`INSERT ... SELECT FROM read_csv(...)`.

Other things worth ruling out on a machine that crashes:

- **Stale crash artifacts** — after a crash, delete any leftover
  `gnaf.duckdb.wal` / `gnaf.duckdb.tmp` next to the database (or start from a
  fresh database file). Replaying a large WAL on the next open can itself
  re-trigger the crash.
- **Antivirus / sync tooling** — ensure the database, its `.wal`, and the temp
  directory are on a local disk excluded from real-time scanning and not under
  OneDrive/DFS folder redirection.
- **Package version skew** — compare `packageVersion("duckdb")` (and R itself)
  between the working and crashing machines; upgrade the crashing machine to
  match. Several older duckdb builds had Windows-specific OOM/crash bugs.

---

## Function reference

| Task | Functions |
|---|---|
| Connect & setup | `gnaf_connect()`, `gnaf_disconnect()`, `gnaf_init()`, `gnaf_status()`, `sample_gnaf()` |
| Load G-NAF | `gnaf_build_db()`, `gnaf_load_psv()` (Standard), `gnaf_load()` (Core CSV) |
| Match | `gnaf_match()`, `gnaf_text_scores()`, `gnaf_match_features()` |
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
