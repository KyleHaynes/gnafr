# Scoring review and path to learned probabilities

The reported lot/street-number problem was real. Previously, any input lot
replaced the street-number comparison in both retrieval and scoring. With input
`LOT 7 42 MAIN ROAD`, a different property at `LOT 7 10 MAIN ROAD` could earn all
ten number points and score 100. The correct property could also be excluded
before scoring if its stored lot was absent or different.

The fix gives an explicit street number precedence. Lot-only inputs still use
lots. Lot tokens in candidate labels cannot be recovered as street numbers.
Cache version 14 prevents reuse of matches computed under the previous rules.

This fixes a concrete defect; it does not turn the existing total into a
probability. For example, the default five-point unit bucket still allows a
wrong unit to score 95, a supplied state is not a component of the postcode-path
score, and lot disagreement needs independent inspection when a street number
is present. Parser errors, incomplete reference data, and competing candidates
can also produce misleadingly high agreement. Do not treat 100 as automatic
acceptance. No new arbitrary score cap or weight adjustment establishes accuracy.

## Which text is compared?

`gnaf_text_scores()` compares `input_raw` with the matched label after basic
normalisation: encoding/spacing repairs, uppercase, and removal of commas and
full stops. It does **not** reconstruct parsed components or expand street-type
abbreviations. Thus raw `10 Main Rd` and label `10 MAIN ROAD` differ.

`gnaf_text_scores(x, input = "standardised")` now compares the reconstructed
`input_standardised` instead, against the same normalised candidate label.
The candidate is not reparsed. Linked returns use `matched_address_label`.
These whole-string scores run after matching; they do not influence the current
candidate search, score total, or ranking. Street/suburb component similarities
are a different calculation, using parsed component values during ranking.

Both comparisons are useful predictors: standardisation removes some formatting
differences, while the raw input retains evidence the parser may have misplaced
or discarded. A high whole-string similarity cannot by itself rescue a conflicting
house number or unit.

## Evidence available now

```r
candidates <- gnaf_match(addresses, con, max_results = 5L, min_score = 0L,
                         cache = FALSE)
evidence <- gnaf_match_features(candidates)
evidence[has_identifier_conflict == TRUE | tied_best == TRUE]
```

`gnaf_match_features()` keeps scores/ranks intact and appends:

- Six component agreements rescaled independently of the caller's ranking weights.
- Jaro-Winkler, character-bigram Jaccard and Levenshtein scores for both raw and
  standardised input, plus the existing combined text diagnostics.
- Separate conflict and missing-evidence flags for street numbers, suffixes,
  lots, units, levels, types, directions, state, postcode, suburb and street name.
- The score gap to another returned PID and whether the best returned score ties.

Missing values do not count as agreement. Ranges with overlapping intervals do
not count as number conflicts. `has_identifier_conflict` is a conservative review
flag for structural identifiers/types/state, not a statement that every such
candidate is wrong. Suburb/street spelling differences are separate flags.
The gap is unknown with only one returned candidate. Aliases resolving to the
same principal may still be separate candidate PIDs; define the intended match
resolution before interpreting those ties.

## Logistic baseline on historical data

Use verified original addresses and accepted reference IDs. Previously geocoded
coordinates alone are not ground truth for a unit-level PID: many units share
coordinates, and earlier geocoding may itself be wrong. Decide whether the target
is an exact address, principal address, or building/primary address, then label
consistently. Keep unverified cases out of the fitting and accuracy denominators.

Split repeated addresses, households and related records together before
fitting. Prefer a later-period test set where feasible. All alternatives for an
input belong in the same split. Keep the real frequency of incorrect matches;
balancing the classes changes the probability target unless sampling is accounted
for. Freeze the GNAF snapshot, parser, retrieval settings, weights and number of
returned candidates for comparable training and deployment features.

The executable baseline is [dev/calibrate_match_scores.R](../dev/calibrate_match_scores.R).
It estimates **whether the existing top candidate is correct**. It does not
replace retrieval or re-rank candidates. This is a first calibrated acceptance
layer; a later learned ranker needs candidate-level training and evaluation.

```r
library(gnafr) # or pkgload::load_all(".") when running this checkout
source("dev/calibrate_match_scores.R")

# history has raw_address, verified_pid, validation_group and split columns.
# split is assigned upstream to whole groups: "train", "validation", "test".
# validation_group connects related student/household/repeated-address records.
results <- gnaf_match(history$raw_address, con, max_results = 5L,
                      min_score = 0L, cache = FALSE)
results[, verified_pid := history$verified_pid[input_id]]
results[, validation_group := history$validation_group[input_id]]
results[, split := history$split[input_id]]
# This example evaluates exact candidate PIDs, without linked-return options.
# Use an appropriate equivalent-PID truth mapping for principal/building targets.
stopifnot(!anyNA(results$verified_pid))
results[, correct_match := matched & address_detail_pid == verified_pid]

model <- fit_match_calibration(
  results[split == "train"], results[split == "validation"]
)
model$validation$summary
model$validation$reliability
model$validation$thresholds

# Choose the acceptance threshold using validation evidence and the cost of
# false matches; 0.99 below is an example, not a pre-approved operating point.
test_report <- evaluate_match_calibration(model, results[split == "test"],
                                          threshold = 0.99)
test_report
scored <- predict_match_calibration(model, results)
```

The workflow includes all six component predictors and all six individual text
similarities, plus conflicts, missingness and the competitor gap. It omits the
total and combined text averages because those duplicate other predictors.
Constant or exactly dependent columns are removed using training data only and
recorded in `dropped_predictors`. No coefficients are invented. Unstable fits
(including reported separation) stop rather than supplying misleading probabilities.
If separation or limited sample size is an issue, use penalised logistic regression
and tune its penalty inside the training/validation process.

Labels must be logical or 0/1 and include both correct and incorrect returned
matches. Unmatched inputs are excluded from fitting candidate probabilities but
remain in end-to-end coverage and accuracy. Lower-ranked predictions remain
missing because this baseline was trained on top candidates. The policy reports
also exclude known structural conflicts and tied best candidates from acceptance.

The workflow checks group/address overlap between splits. It cannot infer all
household relationships or prevent repeated inspection of the test set; those
remain dataset-design responsibilities. It retains split group/address keys in
the local model object for leakage checks.

Assess precision and false accepts at the chosen threshold, coverage (including
unmatched inputs), Brier score, log loss, and observed correctness within predicted
probability bins. Inspect high-scoring errors separately for lots, units, missing
numbers, parser failures and aliases. Small bins or a handful of accepted cases
do not establish high precision; quantify uncertainty before setting an operating
threshold. The current script reports point estimates, not confidence bounds.

Logistic regression is a baseline, not an automatic guarantee of calibration.
Its binomial fit is described in the [R glm documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html).
See the [probability calibration documentation](https://scikit-learn.org/stable/modules/calibration.html)
for reliability diagrams and the limits of proper scoring rules as calibration
measures. Validation and test labels must remain separate from fitting.

## What would justify replacing the matcher?

Measure candidate recall first: how often is the verified correct PID present
among the retrieved alternatives? A classifier cannot recover a candidate that
was never retrieved. `min_score = 0` removes the score threshold but does not
remove postcode/state/number restrictions or the top-N limit.

If correct candidates are missing, improve parsing and retrieval or evaluate a
second reference/address-validation service on the same held-out cases. If the
correct candidate is present but ranks poorly, compare learned candidate ranking
with this baseline. If ranking is good but reported confidence is poor, validate
the acceptance model and its calibration. Compare alternatives by held-out false
accepts, coverage and runtime, rather than by how often they produce a high score.

No historical student data was supplied for this change. The workflow is ready
for that evaluation; no production accuracy gain or calibrated probability is
claimed, and the weighted ranker remains the default.

Run the workflow's isolated synthetic software checks from the repository root
with `Rscript --vanilla dev/test_calibrate_match_scores.R`. These exercise
prediction, split leakage checks, label validation and unmatched-input coverage;
they do not measure real geocoding accuracy.

## Reported locality boundaries (20 September 2026)

The three supplied examples were checked against `C:/temp/test3a.duckdb`, opened
read-only with match caching disabled. All returned the same PIDs as the supplied
output; the improvements below reflect recovered address components.

| Input | Supplied score | Updated score | Explanation |
|---|---:|---:|---|
| `1 ams way, marsden qld 4132` | 84 | 84 | Parsing was correct. `AMS` still differs from reference `SAMS`. |
| `89 THE ESPLANADE S LUCIA QLD 4067` | 61 | 92 | Separate `THE ESPLANADE` from the locality; retain the `S LUCIA` typo penalty. |
| `15 watermans way river heads qld 4655` | 65 | 100 | Parse street `WATERMANS`, type `WAY`, locality `RIVER HEADS`. |

`address_parse()` recognises `RIVER` as a possible locality word and looks for
an earlier street type. For the ambiguous Esplanade example, `gnaf_match()` uses
the reference locality index after parsing, under `locality_fallback = TRUE`.
Exact locality recovery takes precedence. Fuzzy recovery requires a unique
multi-word locality and split within one character edit in the supplied
postcode/state; an existing parsed locality is left alone. Recovery uses one
additional batched query for unresolved tails, not a query per input.

The recovered input locality remains `S LUCIA`, rather than being overwritten
with `ST LUCIA`. Street agreement therefore improves without fabricating exact
locality agreement. This change does not alter scoring weights or turn the
heuristic score into a calibrated probability. Standalone `address_parse()`
cannot make this reference-assisted recovery without a database connection.

Regression coverage includes the supplied examples, unit prefixes, real RIVER
street types, ambiguous locality candidates, postcode/state restrictions,
exact-match precedence, and disabling locality fallback. Cache algorithm version
16 prevents stale scores from being returned after upgrading.

On the saved 50,000-input benchmark sample, the complete locality-recovery step
took 0.16 and 0.22 seconds in two runs against the current database. The fuzzy
pass found 37 additional boundaries beyond exact recovery. These timings include
the existing exact recovery but exclude parsing and candidate scoring; they are
not a new end-to-end speed benchmark or an accuracy estimate.

Validation: the full `testthat::test_local()` suite completed without test
failures. It reported two dependency warnings because the installed `shiny` and
`data.table` packages were built under R 4.5.3 while the runner uses R 4.5.0.
