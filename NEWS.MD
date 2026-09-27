# gnafr 0.0.5

## Data loading

- New `gnaf_load_pli()` adds the Queensland Property Location Index (PLI) to a built database, as
  an extra step after `gnaf_build_db()`. Only addresses whose label G-NAF does not already have are
  added (as principal rows with `source = "pli"` and PIDs `PLI<address_pid>`); the summary it
  prints and returns says how many were already in G-NAF and how many were added, as counts and
  percentages. The PLI is wrangled the way G-NAF is: labels are built by the G-NAF loader's own
  code, a lot is used only when there is no street number, unit and street-number suffixes are
  kept, street directions become G-NAF's codes, the state is `QLD`, postcodes are imputed from
  G-NAF, and coordinates are converted from GDA94 to GDA2020 (with a check against G-NAF's own
  points). Two addresses that differ only by building name, or by `UNIT` against `SHOP`/`SUITE`,
  count as the same address. On a Queensland database and the 26 September 2026 extract, 95.5% of
  the PLI was already in G-NAF and 124,039 addresses (4.4%) were added. See `Adding PLI.md`.


# gnafr 0.0.4

## Address parsing

- Suburbs that begin or end with a street-type word are no longer split across the
  street type. `19 EXCALIBUR CT PARADISE, POINT QLD 4216` (and the same input with no
  comma, or with `COURT` spelt out) parsed as street type `PARADISE`, locality `POINT`;
  it now gives street `EXCALIBUR`, type `COURT`, locality `PARADISE POINT`, the same as
  the comma-in-the-right-place form. A comma that lands inside the suburb name is
  repaired when the word before it is a suburb word following a street type.
- `PARADISE`, `LOOKOUT`, `DOWNS`, `RANGE`, `BRANCH`, `ESTATE`, `RIDGE`, `COVE`,
  `HARBOUR`, `GAP`, `RESERVE`, `GARDENS`, `POCKET`, `LANDING`, `GRANGE`, `NEST`, `FLAT`
  and `JUNCTION` are now treated as likely suburb words when a street type comes
  before them (`Surfers Paradise`, `Point Lookout`, `Bracken Ridge`, `The Gap`).
  Chosen against every distinct QLD street/suburb pair: of ~112,500 comma-less
  inputs rebuilt from GNAF components, fully correct parses rise from 97.1% to
  98.5% with none made worse (`CORNER` was tried and left out because it is a real
  street type, as in `Blue Hills Corner`). Across all 5,967 QLD suburbs, 150 more
  now parse correctly and none regress. Older match-cache entries are ignored.

## Matching

- New exact-label index. `gnaf_load()` and `gnaf_load_psv()` now build
  `gnaf_exact_index`, a table that maps every principal address's label - upper case,
  commas and full stops removed - and its common written variants (`UNIT` as `U`,
  `5/12` unit notation, a shortened street type such as `ST` or `AVE`, no state) to its
  PID. `gnaf_match()` looks each input up in it before parsing, and an input that names
  exactly one address is returned immediately as `exact_components` at full weight,
  without being parsed, standardised or scored. On 1.4 million labels from a QLD
  database a call fell from 668 s to 54 s. Only principal rows are indexed and any key
  that would name two addresses, or a custom address, is left to the full pipeline. The
  index is used for `max_results = 1` with default alias handling, and ignored if
  `gnaf_addresses` has changed since it was built. Add it to an existing database with
  `gnaf_rebuild_exact_index(con)`; both loaders take `exact_index = FALSE` to defer it.
- Exact labels now resolve to the address that owns them. The previous exact-label
  stage could return a street-synonym alias that happens to share a real address's
  label (correct PID on 97.7% of a 1.4 million label sample, now 99.9%), and reported
  lot-only labels such as `LOT 24 SMITH ROAD, ...` as `weighted` even at 100 points
  (they are now `exact_components`), which sent about 7% of exact labels through the
  slow path.
- Inputs answered by the exact-label index are not written to the match cache, which
  they never consult. Without this a batch of clean addresses would insert every one
  of its rows into `gnaf_match_cache`.
- Number and unit credit is now conditional on the street agreeing. A house number
  or unit only locates an address within a street, so `score_number` and
  `score_flat` are scaled by street-name agreement (unchanged for the same street
  or when either street name is missing; near zero for an unrelated street).
  Previously a different street sharing the input's number and unit could collect
  the whole number + flat weight and outrank the right street whose number is
  missing from GNAF (GNAF lag). On real QLD addresses re-typed with a house number
  that does not exist on their street, the wrong street was returned for 79% of
  inputs without a unit and 48% with one; it is now 1% and 3%. On the simulated
  typo benchmark, correct-PID moves 78.9% -> 78.3%, wrong-PID 21.1% -> 20.5%, and
  ~1% of inputs move from matched to unmatched (about half of them wrong matches).
  `total_score` is still the sum of its component columns; `gnaf_match_features()`
  still reports raw agreement. Older match-cache entries are ignored.
- Candidates with equal scores are now ordered by the house number nearest the one
  requested, then by PID, so a missing number resolves to the closest real house
  on the street rather than the lowest PID.
- The `weights` and `fallback_threshold` documentation now states the current
  defaults (12/12/16/10/30/20).

## Misc

- Startup message is more verbose, detailing functions by category.


# gnafr 0.0.3

## Address parsing

- Read a descending hyphenated pair after a flat marker as a unit and a street
  number, so `U 6019-6 Parkland Bvd` parses as unit `6019`, number `6` instead
  of a single `6019-6` unit range. Ascending or equal ranges (`Unit 1-19`) are
  still unit ranges, and the pair is left alone when a separate street number
  is also present.
- Recover missing locality boundaries for adjacent-letter swaps such as
  `UBRLEIGH HEADS` and `SUFRERS PARADISE`, using a unique postcode/state
  reference match and preserving the typed spelling for scoring.
- Keep `ST` as the street type after a compass word in the name (e.g.
  `LITTLE WEST ST WINSTON`), while continuing to recognise `ROAD ST LUCIA`.
  Invalidate cached scores from the previous parsing rules.
- Keep `RIVER HEADS` and `LITTLE RIVER` together as localities when an earlier
  street type establishes the boundary, while retaining genuine `RIVER` types.
- With locality fallback enabled, recover a missing locality from a unique
  multi-word tail within one edit of a reference locality in the supplied
  postcode/state. Preserve the input spelling for scoring and reject ambiguous
  recoveries. This fixes `89 THE ESPLANADE S LUCIA QLD 4067` without treating
  `S LUCIA` as an exact match to `ST LUCIA`. Older match-cache entries are ignored.
- Recognise a state/territory written as its full name (`QUEENSLAND`) or, next
  to a postcode, a bare initial (`Q`), instead of absorbing it into the
  locality. `NSW`/`NT` share the initial `N`, so neither gets a bare-letter form.
- Fix an inconsistency where a typo inside a flat/unit/building prefix (e.g.
  `"UNIT 3 221 The Avene"`) could split a whole-name street like "The Avenue"
  into a fake name + type, something the correctly-spelled address never did.
  Fix a related bug where the same kind of prefix, without a comma, could
  make the parser read the house number itself as the street name (e.g.
  `"UNIT 5 10 Esplanade"`).

## Matching and scoring

- Rank candidate addresses by exact component matching: fully exact matches first,
  then unique otherwise-exact matches with a different postcode, then remaining
  candidates by weighted score. Require matching state and street type when supplied,
  and exact street number, street name, and locality for special preference. Add
  `match_basis` field to indicate ranking category (`exact_components`, 
  `postcode_only`, `weighted`, or `NA`). With locality fallback enabled, run a
  targeted exact-locality search across postcodes for sufficiently specified
  inputs, independent of weighted scores. Bumps the match-cache algorithm version,
  so old cached entries are no longer served.
- Some real GNAF records have a blank street type, with any type word baked
  into the street name instead (e.g. `street_name = "THE POINT CIRCUIT"`,
  `street_type = NULL`) - about 0.9% of rows in a full GNAF load. Scoring now
  compares these against a backfilled name/type split (new
  `gnaf_street_type_index`, built by `gnaf_load`/`gnaf_load_psv`/`gnaf_add`
  and rebuildable via `gnaf_rebuild_street_type_index()`) instead of the raw,
  type-inclusive street name, fixing an otherwise-correct match scoring well
  below its true quality. Bumps the match-cache algorithm version, so old
  cached scores for these rows are no longer served. **Existing databases
  need one explicit `gnaf_init(con)` call after upgrading** to create and
  backfill the new index - `gnaf_match()` does not call it automatically.
 - Add a compact `gnaf_street_type_index` and an accompanying
   `gnaf_rebuild_street_type_index()` convenience function to derive a
   best-effort `(effective_name, effective_type)` split for `street_name`
   values whose `street_type` is missing. Both the SQL bulk path and the R
   scoring code overlay this backfilled split so blank-type rows compare
   like-for-like with parsed input. The index is created and backfilled by
   `gnaf_init()` when needed; `gnaf_add()` updates the index for new custom
   rows and `gnaf_remove_custom()` triggers a rebuild when rows are removed.
 - Bump the match-cache algorithm (now v16) to reflect the street-type and
   locality-recovery changes; previously cached matches for affected rows are
   invalidated.
- Prefer explicit street numbers over lots in retrieval and scoring, and prevent
  lot-only labels from supplying a fallback house number. Ignore older cache entries.
- Add `gnaf_match_features()` for separate identifier conflicts, missing evidence,
  unweighted component agreement, both text comparisons and returned-candidate ties.
- Allow `gnaf_text_scores(input = "standardised")`; the default remains normalised
  raw input. Neither text comparison participates in the current ranking.
- Add a development workflow for logistic calibration against verified labels,
  with separate validation and test groups. No learned model is enabled by default.
- Short-circuit the common DuckDB scoring cases so unchanged values skip
  expensive work: already-canonical street types use a constant `IN` set instead
  of walking every abbreviation CASE arm, digitless street-only alias labels skip
  the street-specific number-token regex, suffixes are only extracted when a
  digit-letter sequence is present, and zero-padding stripping only runs its
  regex on values starting with `0`. Returned candidates and scores are unchanged.

## Documentation and validation

- Add `dev/benchmark_match_speed.R` and `simple_benchmark.R` to reproduce
  end-to-end `gnaf_match()` timings, including optional DuckDB SQL profiles and
  baseline comparison for the scoring speed-ups above.

# gnafr 0.0.2

## Address parsing

- Recognise the bare `U` unit marker, including addresses such as `U 20, 25 Smith St`.
- Preserve unit ranges such as `Unit 1-19`, including spaced hyphens, attached
  markers (`U1-19`), slash notation, and building-name prefixes.
- Parse `40/B` as street number `40` with suffix `B`, including combinations
  such as `Unit 3 40/B` and `3/40/B`, without losing the unit.
- Improve street/locality boundaries for multi-word and hyphenated suburbs,
  including `Flinders View`, `Eight Mile Plains`, `Kippa-Ring`, and `West End`.
  Keep `Rocky View` intact as a locality for subsequent reference matching.
- Preserve directional suburbs such as `North Lakes` and genuine street types
  in addresses such as `10 High St` and `10 Mount View St`.
- Support leading postcode/state pairs such as `4012 QLD Nundah`, including
  locality sections after a street comma and extra commas around geographic fields.
- Preserve multi-word building names ending in plural dwelling words, such as
  `Park View Apartments`, while retaining contextual dwelling-marker typo repair.
- Normalise pasted whitespace, HTML space entities, Unicode hyphens, and spacing
  around ranges and slash numbers. Repair malformed encoding before parsing and
  matching while preserving the original `input_raw` value.

## Matching and scoring

- Blend reshaped Jaro-Winkler similarity with squared normalised
  Damerau-Levenshtein similarity for street and suburb names. Reserve full credit
  for equal normalised names and halve fuzzy name credit for opposing directions.
- Award limited postcode credit for a single adjacent-digit transposition when
  the leading digit is unchanged, alongside the existing numeric proximity tiers.
- Treat equivalent street-type abbreviations consistently and ignore leading
  zero padding on numeric lot, unit, and level identifiers while preserving suffixes.
- Keep R and DuckDB scoring consistent, including fractional weights and UTF-8
  comparisons. Update candidate pruning to use the revised scoring rules and
  recognise equivalent zero-padded lot numbers.
- Invalidate matches cached under earlier scoring or parsing rules, including
  cases where repaired components leave the standardised address text unchanged.

## Documentation and validation

- Document the scoring formulas and parser normalisation behaviour. Matching
  scores measure component agreement and are not calibrated probabilities.
- Add regression coverage for parser edge cases, malformed encoding, cache
  invalidation, scoring consistency, and candidate pruning, plus a scoring benchmark.
