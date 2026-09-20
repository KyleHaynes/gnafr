# Matching performance investigation

## Follow-up: default 50,000-input workload (18 September 2026)

The seed-1 sample from `simulated_inputs.rds` was run against
`C:/temp/test.duckdb`, with default matching settings except `cache = FALSE`.
The controlled comparison in fresh R sessions took **393.30 seconds before and
314.95 seconds after** the scoring shortcuts: 19.9% less elapsed time (1.25x
throughput). All returned columns, values, types and row order were identical,
including 49,837 matched inputs and 163 unmatched inputs. The original supplied
575.43-second run was not used as the controlled baseline.

The shortcuts avoid repeated abbreviation CASE evaluation for canonical types,
number-token extraction for digitless labels, suffix extraction where no suffix
can occur, and zero-padding regexes for identifiers that do not start with zero.
Reproduce with `dev/benchmark_match_speed.R`; its environment variables are
documented at the top of the script. These measurements precede the intentional
parsing and reference-index changes made on 20 September, so they establish
performance and result preservation for the scoring optimisations alone.

## Earlier investigation

13 September 2026. Workload: `simulated_inputs.rds`, rows `200000:300000`,
`C:/temp/gnafx23.duckdb`, `max_results = 1L`, `min_score = 80L`, `cache = FALSE`.
DuckDB 1.5.5 uses 16 threads and a 25 GiB memory limit on this machine.

## Measured results

The isolated end-to-end comparison used 10,001 inputs (`200000:210000`) against
the one-million-row reference database built from the RDS:

| Stage | Before | After |
|---|---:|---:|
| GNAF postcode scoring | 7.59 s | 1.25 s |
| GNAF locality scoring | 3.02 s | 0.46 s |
| Entire `gnaf_match()` call | 12.78 s | 4.03 s |

That is **3.17x faster end to end**, with every returned column, column type and
row order identical. Both runs matched 9,051 inputs and retained 950 unmatched
rows. These are single-run wall times; they are not a national accuracy estimate.

For the full database, the original postcode query completed in 769.56 seconds
and returned the same 93,293 rows reported in the original example. The baseline
process ended unexpectedly during locality fallback, so it did not produce a
new full-call elapsed time or a complete saved output. The supplied original
full-call time was 886.12 seconds.

## Why this workload is expensive

The database has 13,869,745 GNAF rows: 3,333,280 core rows and 10,536,465 aliases.
Locality synonyms alone account for 9,872,831 rows. The 100,001 inputs reduce to
99,956 distinct inputs across 434 postcodes. Number blocking still leaves many
candidate pairs to score, including synonyms that must remain eligible to win.

The full postcode profile produced **262,351,032 candidate pairs**. The early
street bound left **97,804,801** for component scoring, and **10,392,666** reached
the score threshold before ranking down to 93,293 rows. The expensive component
projection accounted for 11,179 of 11,893 cumulative operator seconds (94%).
This explains why optimizing the R output wrangling would make little difference.

The initial SQL inspection and query profiles identified three expensive operations:

- Number scoring builds a regular expression containing each candidate's street
  name. That expression also appears in the branch recovering suffixed house
  numbers from labels when the numeric database field is missing.
- The range branch joins ordinary input numbers against all rows in a postcode
  before establishing whether a candidate actually has a range.
- Locality fallback uses a postcode join with a combined number/lot predicate,
  then scores candidates with an upper bound that assumes full postcode credit
  even when the fallback postcode is far from the input postcode.

The small reference database used during initial investigation contained the
one million core records in the RDS. Its results are not a substitute for the
full database with aliases.

## Changes

### Reuse a constant number pattern

`R/score_components.R` now finds the first literal occurrence of the candidate
street in its label and extracts the preceding number with a constant pattern.
The fast result is accepted only when both the house number and the street
boundary are valid. Building prefixes containing the street name, partial street
names and other unusual layouts fall back to the original expression.

On all one million reference labels in the RDS, extraction took **59.57 seconds
before and 1.43 seconds after**, with identical output. The fast path handled
996,765 labels (99.6765%). This is an isolated extraction measurement, not the
end-to-end matching speedup.

### Make number branches selective before joining

`R/match.R` keeps the exact-number hash join, explicit lot matching,
suffix recovery and missing-number branches. For ordinary inputs with no range
endpoint, the overlapping-range branch now requires a candidate range endpoint
and a start below the input number. Inputs with their own range retain the full
interval-overlap comparison. These branches are disjoint and preserve the
previous candidate set, including unusual or reversed intervals.

Locality fallback also uses the split number joins when its expanded input set
exceeds 100 rows, as the main bulk postcode path already does.

### Prune using the actual postcode score

The early score bound now uses:

```text
actual postcode score
+ rounded street similarity score
+ sum of ceilings of the remaining component weights
```

A candidate is removed only if this bound is below `min_score`. For example,
with default weights and a distant incorrect postcode, the maximum possible
score is 80. At `min_score = 80`, candidates with a street score below 40 cannot
qualify and no longer pay for all the remaining component calculations.

Ceilings preserve a conservative bound for fractional weights; zero street
weights and lower thresholds remain supported. The scoring formula is unchanged.

### Deduplicate fuzzy locality work

Fuzzy locality lookup now operates on distinct `(in_locality, in_state)` pairs
and expands the selected postcodes back to every input. Previously `input_id`
was part of the distinct key, so repeated locality spellings were compared
against the locality index repeatedly. State restrictions, missing states,
the five-postcode limit and postcode tie ordering are preserved.

## Validation

`tests/testthat/test-match-performance.R` covers number extraction boundaries,
building/unit prefixes, range and suffix tokens, punctuation in street names,
empty/missing values, split-versus-unsplit candidate and score equality, alias
filters, fractional and zero weights, bulk locality fallback, shared fuzzy
localities across states, input ownership and registration cleanup.

Pruning is checked against an unpruned result set at thresholds 60, 80 and 95,
including fractional weights and zero street weight. The full package suite
passes: **172 tests, 754 assertions, zero failures or errors**, including
matching, scoring, audit, linked-address, cache, application and geography tests.
There is one package-build-version warning for the installed `shiny`.

No database migration, index rebuild, cache enablement or alias exclusion is
required. Reload the package code before rerunning an existing R session:

```r
devtools::load_all(".")
```

## Reproduce and compare

`benchmark_match_performance.R` runs the exact slice above by default. It opens
the database read-only, disables match caching and leaves DuckDB settings alone.
Run each version with the same database and inputs, preferably with other heavy
work stopped. Close existing write connections with `gnaf_disconnect(con)`,
including connections in the current R session, before benchmarking.

Before applying a change:

```r
Sys.setenv(
  GNAFR_BENCH_DB = "C:/temp/gnafx23.duckdb",
  GNAFR_PERF_SAVE = "before.rds"
)
source("benchmark_match_performance.R")
```

After applying it, in a fresh R session:

```r
Sys.setenv(
  GNAFR_BENCH_DB = "C:/temp/gnafx23.duckdb",
  GNAFR_PERF_COMPARE = "before.rds",
  GNAFR_PERF_SAVE = "after.rds"
)
source("benchmark_match_performance.R")
```

The script refuses to overwrite an existing result file. It reports throughput,
matched input count, speedup and equality of every returned column and type,
including ranks, component scores and unmatched rows. A mismatch raises an error
after saving the output for inspection. Input loading and result serialization
are outside the match timing.

Optional settings: `GNAFR_PERF_INPUTS`, `GNAFR_PERF_START`, `GNAFR_PERF_N`,
`GNAFR_PERF_MIN_SCORE` and `GNAFR_PERF_MAX_RESULTS`. Defaults are the workload
above. The separate `benchmark_match.R` continues to provide seeded perturbation
benchmarks with source-PID recovery statistics.

## Further opportunities

Large synonym tables still repeat street/locality comparisons and other
candidate attributes. Computing scores at a smaller shared-address level could
reduce this work further, but would need to preserve alias-specific labels,
components and PID tie ordering. Persisting derived number tokens would also
require explicit invalidation when custom or GNAF rows change. Neither is needed
for the improvements here.

No new index or fixed fuzzy-similarity cutoff was introduced. Lower thresholds
and unusual custom weights can legitimately retain much larger candidate sets;
the runtime gain for this specific slice should not be treated as a guarantee
for those workloads.

DuckDB's [profiling documentation](https://duckdb.org/docs/current/dev/profiling)
describes the query profiles used for diagnosis. Its
[EXPLAIN ANALYZE guide](https://duckdb.org/docs/current/guides/meta/explain_analyze)
explains why summed operator times can exceed wall time when operators run in
parallel. The measured matching times should be compared as wall times.
