# Tests that pin absolute point values (e.g. "a flat match scores 5") were
# written against these weights. Pass them explicitly so changing the package
# defaults (.WEIGHTS) doesn't silently rewrite what those tests assert.
legacy_weights <- list(
  postcode = 20, suburb = 15, street_name = 40, street_type = 10, number = 10, flat = 5
)
