# Matching algorithm review

Reviewed 12 September 2026. This review covers candidate retrieval, scoring,
ranking, caching and linked-address returns, with regression tests in temporary
DuckDB databases. It is not a measured national accuracy assessment.

## Changes made

| Finding | Change | Why it matters |
|---|---|---|
| A fixed street similarity floor of 0.3 rejected candidates even with zero street weight. | Prune using the rounded street score plus a conservative upper bound on all other components. | Candidates that can reach `min_score` survive this stage, including custom-weight calls. |
| Several locality names for one postcode could occupy all five fuzzy fallback slots. | Group by postcode, retain its best similarity, then rank five distinct postcodes. | Synonyms cannot crowd out another relevant postcode. |
| A completed state search was followed by another search of postcode subsets from that same state. | Skip that redundant locality retry. | Avoids repeated lookup, scoring and ranking for inputs with a state but no postcode. |
| SQL errors could become empty candidate sets, silently when `verbose = FALSE`. | Raise the query error with path context; registered inputs are still cleaned up. | A database failure is distinguishable from a valid unmatched result. |
| Two obsolete search implementations remained alongside the active paths. | Remove the unreferenced locality implementations. | One active scoring pipeline is easier to maintain and test. |
| The benchmark ignored its documented database environment variable. | Honour `GNAFR_BENCH_DB`, validate sample settings, and report accuracy by perturbation. | Repeatable evaluation works with a chosen database and includes unmatched inputs. |

The cache algorithm version is now 6. Earlier cache entries are ignored rather
than reused under changed candidate rules. No database rebuild is required.

## How matching and address resolution differ

Matching parses the input, retrieves candidates within postcode/state and
number/lot restrictions, scores their components, and ranks the surviving PIDs.
Aliases participate under the same candidate filters as core addresses.

After ranking, `return_principal = TRUE` follows the stored `principal_pid`.
`return_primary = TRUE` follows the stored `primary_pid`. With both enabled,
principal resolution happens first. Neither lookup uses the match score or
`fallback_threshold`. All original address fields remain available as
`matched_*`; unprefixed fields describe the returned address. Geography attributes
are joined to the final returned PID. Scores and ranks still describe the original
candidate, and multiple candidates resolving to one PID retain their rows.

The Musgrave Road example was verified against the live database before this
review: matched `GAQLD158544274` (unit 20) maps directly to primary
`GAQLD162996148` (110 Musgrave Road). Primary, alias-to-principal and combined
returns passed at score 100 with locality fallback disabled.

## Interpretation and limits

- Scores measure weighted agreement, not a probability that an address is correct.
  A conflicting unit can still score highly because the flat component has only
  five points by default. Inspect component scores and alternatives for important
  decisions; changing the weights needs evaluation on labelled examples.
- Search is deliberately restricted. A typo in a street number can exclude the
  correct address before scoring; a high-confidence result can also prevent a
  cross-postcode locality retry. This review does not introduce unrestricted
  fuzzy house-number matching.
- Numeric postcode proximity earns partial credit but is not geographic distance.
  State constrains state and locality discovery paths; it is not an independent
  scored component in the postcode path.
- Low `min_score` or low street weight can now admit candidates previously removed
  by the undocumented similarity floor. This improves consistency with the
  requested weights but can increase work and weak alternatives.
- Equal scores are ordered by PID. `max_results > 1` exposes alternatives; a
  score-100 label fast path does not enumerate every possible score tie.
- Missing relationship targets leave the current address unchanged. The stored
  PID alone cannot supply fields if its target record was never loaded.

## Validation and reproducible evaluation

Regression tests cover custom weights, crowded locality synonyms, quiet SQL
failures and cleanup, and skipping redundant state retries. The full package suite
also exercises scoring parity, aliases, primary returns, cache reuse, ranges,
units, missing inputs and geographies.

From the repository root, run a single-call, read-only benchmark:

```r
Sys.setenv(
  GNAFR_BENCH_DB = "C:/temp/gnafx23.duckdb",
  GNAFR_MATCH_BENCH_N = "1000",
  GNAFR_MATCH_BENCH_SEED = "42"
)
source("benchmark_match.R")
```

The script reports elapsed time, throughput, match rate, exact source-PID recovery
and principal-PID recovery, including a breakdown by simulated perturbation.
Sampling and perturbation time are excluded from matching time. It opens the
database read-only and disables match caching. There is no address batching.

The live database was reopened by another R process during this review, preventing
a new live benchmark. No live throughput or accuracy improvement is claimed.
For meaningful before/after comparisons, use the same database snapshot, sample
seed and inputs. Synthetic perturbations complement a manually labelled holdout
set; they do not establish production precision or recall.
