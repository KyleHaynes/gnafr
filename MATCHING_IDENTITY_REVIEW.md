# Matching identity review

## Reported failure

`UNIT 3 24 ILLAWONG STREET, CANNONVALE QLD 4802` identified the unit,
but deleting the final `T` redirected the match to street number 3, with
both results scoring 100.

Two independent defects combined:

1. The parser classified `UNI` as a building name, `3` as the street number,
   and `24 ILLAWONG` as the street name. Number-based candidate filtering
   therefore excluded the intended street number 24 before scoring.
2. The name-scoring curve awarded full credit at Jaro–Winkler similarities
   of 0.85 or above. `24 ILLAWONG` could therefore earn the same street-name
   credit as `ILLAWONG`. The remaining components supplied the other 60 points.

Changing weights alone would not repair the missing candidate.

## Changes

- Recover an unambiguous one-edit or adjacent-transposition spelling of
  `UNIT`, `FLAT`, `APARTMENT`, or `SUITE` only when followed by a unit number,
  street number and street name. Preserve `input_raw`, building prefixes,
  row order and duplicates. Official abbreviations take precedence.
- Extend the squared name-credit curve from similarity 0.6 to 1.0; remove
  the full-credit plateau starting at 0.85. Keep the existing low similarity
  floor so unrelated names receive little credit.
- Require equal, non-empty names for full component credit. Cap fuzzy name
  scores below the rounded component weight, including long strings where
  rounding would otherwise conceal a small difference.
- Apply identical rules in R and DuckDB. Candidate-pruning upper bounds
  continue to use the shared curve and remain conservative.
- Increment the matching-cache algorithm version to 8. Old cached results
  are ignored without requiring users to delete their database or cache.

Exact label lookup, small-call component matching, large-batch postcode
matching, locality fallback, ranking and cache lookup were inspected as part
of tracing this failure. The scoring change affects every name comparison,
not just Illawong Street.

## Validation against the supplied database

Database: `C:/temp/gnafx23.duckdb`, opened read-only; matching cache disabled.

| Input prefix / street | Selected address | Score |
|---|---|---:|
| `UNIT 3 24 ILLAWONG` | Unit 3, 24 Illawong Street | 100 |
| `UNI 3 24 ILLAWONG` | Unit 3, 24 Illawong Street | 100 |
| `UNTI 3 24 ILLAWONG` | Unit 3, 24 Illawong Street | 100 |
| `UNI 3 24 ILLAWON` | Unit 3, 24 Illawong Street | 95 |

A recognised marker correction is standardisation: its resulting components
can still agree exactly. A remaining street-name spelling difference receives
less than full credit.

## Labelled sample comparison

`audit_match_identity.R` selects 2,000 evenly spaced rows from indices
200,000–300,000 of `simulated_inputs.rds`. It compares the revised code with
the prior marker handling and name-credit formula reconstructed through local
test bindings. Database, retrieval rules, weights, `min_score = 80`, and
`max_results = 1` are held constant. This is a targeted before/after comparison,
not a comparison against an independently installed historical release.

| Result | Before | After |
|---|---:|---:|
| Expected original G-NAF PID | 1,569 | 1,637 |
| Another PID | 222 | 151 |
| Unmatched | 209 | 212 |
| Another PID with score 100 | 132 | 63 |
| Elapsed seconds, first run | 11.97 | 12.10 |
| Elapsed seconds, repeat | 11.81 | 12.14 |

The expected-PID rate improved from 78.45% to 81.85%. Coverage changed from
89.55% to 89.40%. Timings are observations from this sample, not a performance
guarantee for the full million rows.

PID equality is stricter than dwelling equivalence. Three of the remaining
63 different-PID score-100 results have identical database address labels.
Other inspected examples involve a perturbation changing one valid address
into another: `116 CASCADE` becomes `16 CASCADE`, or unit `25` becomes `52`.
The supplied text alone cannot establish which number was originally intended.
The sample also contains other structural shorthand, such as `LT` for `LOT`,
which this dwelling-marker repair does not address.

## Regression coverage and interpretation

Regression fixtures include competing street numbers, the parent property,
competing unit numbers, marker typos, building prefixes, missing inputs,
duplicate inputs, fuzzy street names, long-name rounding, fractional and zero
weights, R/SQL score parity, small and large query paths, repeated cache use,
and rejection of a stale cached false-perfect match.

The final full `testthat` run completed with 831 passing assertions, no failures
and no skips. Installed-package build-version warnings (`shiny` and
`data.table`) occurred during validation; the final run recorded one warning.

Existing alias tests now use a sufficiently close competitor under the stricter
curve. The cache/alias test uses a locality transposition that remains above
the cache threshold, preserving its purpose of testing an actual cache hit.

The score remains a weighted component-agreement measure. It is not a calibrated
probability, nor does it express whether several candidates share the top score.
Building names and state are not independent weighted components in the current
six-weight model; state constrains some retrieval paths. Number blocking also
limits recovery of mistyped street numbers. Those are broader model limitations,
not resolved by adjusting the name curve.

Before changing production acceptance thresholds, assess a labelled sample
for the actual population and review ambiguous results with multiple candidates.
The revised curve intentionally gives some previous fuzzy matches lower scores.

To reproduce: run `Rscript --vanilla audit_match_identity.R` from the package
root, or set `GNAFR_BENCH_DB` to another reference database first. The returned
`audit` object includes both result sets, expected PIDs and a mismatch review.
