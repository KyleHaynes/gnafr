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
