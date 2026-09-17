library(data.table)

test_that("building prefixes preserve implicit unit and street numbers", {
  before <- c("MY BUILDING NAME 3 24 ILLAWONG", "3 24 ILLAWONG",
    "MY BUILDING NAME 3A 24B ILLAWONG", "MY BUILDING NAME 3 24-26 ILLAWONG",
    "BLOCK 7 3 24 ILLAWONG", "MY BUILDING NAME FLAT 3 24 ILLAWONG",
    "MY BUILDING NAME 24 ILLAWONG")
  resources <- gnafr:::.get_parser_resources()
  for (separator in c(", ", " ")) {
    x <- paste0(before, " STREET", separator, "CANNONVALE QLD 4802")
    p <- address_parse(x, normalize = FALSE)
    expect_equal(p$in_flat_number, c("3", "3", "3A", "3", "3", "3", NA_character_))
    expect_equal(p$in_number_first, rep(24L, length(x)))
    expect_equal(p$in_number_last, c(rep(NA_integer_, 3L), 26L, rep(NA_integer_, 3L)))
    expect_equal(p$in_number_suffix, c(NA_character_, NA_character_, "B", rep(NA_character_, 4L)))
    expect_equal(p$in_street_name, rep("ILLAWONG", length(x)))
    expect_equal(p$in_building_name, c("MY BUILDING NAME", NA_character_,
      "MY BUILDING NAME", "MY BUILDING NAME", "BLOCK 7", "MY BUILDING NAME", "MY BUILDING NAME"))
    expect_identical(p$input_raw, x)
    for (j in seq_along(before)) {
      scalar <- gnafr:::.parse_before(before[j], resources$ft_re,
        resources$ft_map, resources$ft_alt)
      for (field in names(scalar)) {
        expect_equal(p[[paste0("in_", field)]][j], scalar[[field]], info = paste(j, field))
      }
    }
  }
})

test_that("misspelled dwelling markers retain unit and street-number identity", {
  markers <- c("UNIT", "UNI", "UNTI", "UNITS", "UN", "FLTA", "APARTMNT", "SUIET")
  x <- paste(markers, "3 24 ILLAWONG STREET, CANNONVALE QLD 4802")
  for (normalize in c(TRUE, FALSE)) {
    parsed <- address_parse(x, normalize = normalize)
    expect_equal(parsed$in_flat_number, rep("3", length(x)))
    expect_equal(parsed$in_number_first, rep(24L, length(x)))
    expect_equal(parsed$in_street_name, rep("ILLAWONG", length(x)))
    expect_true(all(is.na(parsed$in_building_name)))
    expect_identical(parsed$input_raw, x)
    expect_identical(parsed$input_id, seq_along(x))
  }
})

test_that("dwelling typo recovery is contextual and preserves building prefixes", {
  x <- c(NA_character_, "", "UNI 24 ILLAWONG STREET, CANNONVALE QLD 4802",
    "SUNRISE UNI 3 24 ILLAWONG STREET CANNONVALE QLD 4802",
    "SUNRISE UNIT 3 24 ILLAWONG STREET, CANNONVALE QLD 4802",
    "10 UNIT STREET, CANNONVALE QLD 4802")
  p <- address_parse(x)
  expect_true(all(is.na(p$in_flat_number[c(1L, 2L, 3L, 6L)])))
  expect_equal(p$in_number_first[3:6], c(24L, 24L, 24L, 10L))
  expect_equal(p$in_flat_number[4:5], c("3", "3"))
  expect_equal(p$in_building_name[3:5], c("UNI", "SUNRISE", "SUNRISE"))
  expect_identical(p$input_raw, x)
})

# ---- Basic well-formed addresses -------------------------------------------

test_that("standard address parses all fields", {
  r <- address_parse("25 SAINT JAMES CT, TAMBORINE MOUNTAIN QLD 4272")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_name,  "SAINT JAMES")
  expect_equal(r$in_street_type,  "COURT")
  expect_equal(r$in_locality,     "TAMBORINE MOUNTAIN")
  expect_equal(r$in_state,        "QLD")
  expect_equal(r$in_postcode,     4272L)
})

# ---- Common user errors: wrong / abbreviated street type -------------------

test_that("Rd instead of Ct still parses street name correctly", {
  r <- address_parse("25 St James Rd, Tamborine Mountain QLD 4272")
  expect_equal(r$in_street_name, "SAINT JAMES")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_number_first, 25L)
})

test_that("Street instead of Drive parses correctly", {
  r <- address_parse("12 Kings Street, Sydney NSW 2000")
  expect_equal(r$in_street_name, "KINGS")
  expect_equal(r$in_street_type, "STREET")
})

test_that("abbreviated Ave parses to AVENUE", {
  r <- address_parse("10 Smith Ave, Brisbane QLD 4000")
  expect_equal(r$in_street_type, "AVENUE")
  expect_equal(r$in_street_name, "SMITH")
})

test_that("abbreviated Cres parses to CRESCENT", {
  r <- address_parse("7 Rose Cres, Perth WA 6000")
  expect_equal(r$in_street_type, "CRESCENT")
  expect_equal(r$in_street_name, "ROSE")
})

test_that("abbreviated Dr parses to DRIVE", {
  r <- address_parse("3 Oak Dr, Melbourne VIC 3000")
  expect_equal(r$in_street_type, "DRIVE")
  expect_equal(r$in_street_name, "OAK")
})

test_that("abbreviated Tce parses to TERRACE", {
  r <- address_parse("50 Murray Tce, Adelaide SA 5000")
  expect_equal(r$in_street_type, "TERRACE")
  expect_equal(r$in_street_name, "MURRAY")
})

# ---- Missing comma ----------------------------------------------------------

test_that("no comma between street and suburb still parses", {
  r <- address_parse("25 Saint James Ct Tamborine Mountain QLD 4272")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_postcode,     4272L)
  expect_equal(r$in_state,        "QLD")
})

# ---- Mixed case input -------------------------------------------------------

test_that("lowercase input is normalised and parsed", {
  r <- address_parse("25 saint james ct, tamborine mountain qld 4272")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_type,  "COURT")
  expect_equal(r$in_state,        "QLD")
  expect_equal(r$in_postcode,     4272L)
})

test_that("title-case input parses correctly", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain Qld 4272")
  expect_equal(r$in_street_type, "COURT")
  expect_equal(r$in_state,       "QLD")
})

# ---- Unit / flat notation --------------------------------------------------

test_that("slash notation: 3/25 Saint James Ct", {
  r <- address_parse("3/25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_name,  "SAINT JAMES")
})

test_that("UNIT prefix notation parses flat and number", {
  r <- address_parse("UNIT 3 25 Smith St, Sydney NSW 2000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
})

test_that("APT prefix notation parses flat", {
  r <- address_parse("APT 4 10 Main Rd, Melbourne VIC 3000")
  expect_equal(r$in_flat_number, "4")
  expect_equal(r$in_number_first, 10L)
})

test_that("attached U prefix: U3 25 Smith St", {
  r <- address_parse("U3 25 Smith St, Sydney NSW 2000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
})

# ---- Street number ranges --------------------------------------------------

test_that("range number: 110-120 Musgrave Rd parses first and last", {
  r <- address_parse("110-120 Musgrave Rd, Red Hill QLD 4059")
  expect_equal(r$in_number_first, 110L)
  expect_equal(r$in_number_last,  120L)
  expect_equal(r$in_street_name,  "MUSGRAVE")
})

# ---- Multi-word street names -----------------------------------------------

test_that("multi-word street name like Saint James is preserved", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_street_name, "SAINT JAMES")
})

test_that("multi-word locality is preserved", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_locality, "TAMBORINE MOUNTAIN")
})

# ---- Missing postcode / state ----------------------------------------------

test_that("address without postcode has NA in_postcode", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain QLD")
  expect_true(is.na(r$in_postcode))
  expect_equal(r$in_state, "QLD")
})

test_that("address without state has NA in_state", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain 4272")
  expect_true(is.na(r$in_state))
  expect_equal(r$in_postcode, 4272L)
})

# ---- Periods in address (e.g. "St." abbreviation) -------------------------

test_that("period after street type abbreviation is stripped", {
  r <- address_parse("10 Oak St. Sydney NSW 2000")
  expect_equal(r$in_street_type, "STREET")
  expect_equal(r$in_number_first, 10L)
})

# ---- Missing street type ----------------------------------------------------

test_that("missing street type still extracts number and guesses street/locality split", {
  r <- address_parse("190 MUSGRAVE RED HILL QLD 4059")
  expect_equal(r$in_number_first, 190L)
  expect_true(is.na(r$in_street_type))
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_equal(r$in_locality,    "RED HILL")
  expect_equal(r$in_state,       "QLD")
  expect_equal(r$in_postcode,    4059L)
})

test_that("missing street type with single-word remainder is treated as street name", {
  r <- address_parse("190 MUSGRAVE QLD 4059")
  expect_equal(r$in_number_first, 190L)
  expect_true(is.na(r$in_street_type))
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_true(is.na(r$in_locality))
})

# ---- Street-type/locality-name collision words (HILL, PARK, VALLEY, ...) --
# When the rightmost apparent street-type match is also a common locality
# word, an earlier unambiguous street-type token (if present) should win, so
# the collision word is treated as (part of) the locality instead. A leading
# business-name prefix plus an attached unit designator ("U20") routes these
# addresses through the scalar fallback parser rather than the vectorized
# fast path, so both must apply the same disambiguation.

test_that("locality-collision word after street type resolves correctly with no prefix", {
  r <- address_parse("U20 110 MUSGRAVE RD RED HILL 4060")
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_locality,    "RED HILL")
  expect_equal(r$in_number_first, 110L)
  expect_equal(r$in_flat_number,  "20")
})

test_that("business-name prefix before an attached unit does not break locality-collision resolution", {
  r <- address_parse("Cambridge on the hill U20 110 musgrave rd red hill 4060")
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_locality,    "RED HILL")
  expect_equal(r$in_number_first, 110L)
  expect_equal(r$in_flat_number,  "20")
  expect_equal(r$in_postcode,     4060L)
})

test_that("a genuine locality-collision word is still preserved when it is the real locality", {
  r <- address_parse("U20 110 MUSGRAVE RD BUSHLAND HILL 4060")
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_locality,    "BUSHLAND HILL")
})

# ---- Vectorised input ------------------------------------------------------

test_that("multiple addresses returned as one row each", {
  addrs <- c(
    "25 Saint James Ct, Tamborine Mountain QLD 4272",
    "10 Smith Ave, Brisbane QLD 4000"
  )
  r <- address_parse(addrs)
  expect_equal(nrow(r), 2L)
  expect_equal(r$input_id, c(1L, 2L))
  expect_equal(r$in_number_first, c(25L, 10L))
  expect_equal(r$in_street_type,  c("COURT", "AVENUE"))
})

test_that("empty string gives all-NA row", {
  r <- address_parse("")
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$in_postcode))
  expect_true(is.na(r$in_street_name))
})

# ---- Alpha-suffixed street numbers (e.g. 190A, 10B) -----------------------

test_that("number with trailing alpha extracts integer and suffix", {
  r <- address_parse("190A MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_number_first,  190L)
  expect_equal(r$in_number_suffix, "A")
  expect_equal(r$in_street_name,   "MUSGRAVE")
  expect_equal(r$in_street_type,   "ROAD")
  expect_equal(r$in_locality,      "RED HILL")
  expect_equal(r$in_postcode,      4059L)
  expect_equal(r$in_state,         "QLD")
})

test_that("lowercase alpha suffix is normalised before extraction", {
  r <- address_parse("190a MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_number_first,  190L)
  expect_equal(r$in_number_suffix, "A")
  expect_equal(r$in_street_name,   "MUSGRAVE")
})

test_that("plain number has NA suffix", {
  r <- address_parse("190 MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_number_first, 190L)
  expect_true(is.na(r$in_number_suffix))
})

test_that("flat + alpha-suffixed street number parses all fields", {
  r <- address_parse("UNIT 3 190A MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_flat_type,     "UNIT")
  expect_equal(r$in_flat_number,   "3")
  expect_equal(r$in_number_first,  190L)
  expect_equal(r$in_number_suffix, "A")
  expect_equal(r$in_street_name,   "MUSGRAVE")
})

# ---- Unit/street-number ambiguity & noisy prefixes -------------------------
# All of the following describe the same address — "Unit 6019, 6 Parkland
# Boulevard" — written with varying noise, marker placement and ordering.
# They should all parse to identical flat/number/street fields.

test_that("implied-pair convention picks rightmost NUM NUM STREETNAME over a noisy leading flat marker", {
  r <- address_parse("U10 BLAH 6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
  expect_equal(r$in_street_type,  "BOULEVARD")
})

test_that("explicit UNIT marker mid-string after the street number is recognised", {
  r <- address_parse("6 UNIT 6019 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

test_that("explicit UNIT marker after a noisy leading flat marker resolves to the trailing pair", {
  r <- address_parse("U10 BLAH UNIT 6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

test_that("explicit UNIT marker preceded by a noisy leading number+street phrase resolves to the trailing pair", {
  r <- address_parse("5 BLIND ROAD UNIT 6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

test_that("attached U-prefix flat number with following street number parses correctly", {
  r <- address_parse("U6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

# ---- Comma hint: word before the (last) comma is structurally the type -----
# Most real-world addresses look like "1 SMITH ST, BRISBANE QLD 4000". When
# present, the comma-adjacent word disambiguates the street type globally —
# both for coincidental abbreviation collisions inside multi-word street
# names, and for misspelt types that the generic fuzzy scan would otherwise
# attribute to a street-name word that merely resembles a type.

test_that("misspelt street type before a comma resolves over a coincidental name collision", {
  r <- address_parse("UNIT 6019 6 parkland bvdz, brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
  expect_equal(r$in_street_type,  "BOULEVARD")
  expect_equal(r$in_locality,     "BRISBANE CITY")
})

test_that("comma hint resolves a misspelt type that collides with an abbreviation inside the street name", {
  r <- address_parse("25 St James Rode, Tamborine Mountain QLD 4272")
  expect_equal(r$in_street_name, "SAINT JAMES")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_locality,    "TAMBORINE MOUNTAIN")
})

test_that("comma hint resolves a near-miss abbreviation to its canonical type", {
  r <- address_parse("1 Smith STX, Brisbane QLD 4000")
  expect_equal(r$in_street_name, "SMITH")
  expect_equal(r$in_street_type, "STREET")
  expect_equal(r$in_locality,    "BRISBANE")
})

test_that("comma hint is ignored when the comma-adjacent word doesn't look like a street type", {
  r <- address_parse("3/25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_street_name,  "SAINT JAMES")
  expect_equal(r$in_street_type,  "COURT")
})

test_that("comma hint uses the last comma so a leading unit comma isn't mistaken for the type", {
  r <- address_parse("UNIT 5, 10 Smith St, Brisbane QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "5")
  expect_equal(r$in_number_first, 10L)
  expect_equal(r$in_street_name,  "SMITH")
  expect_equal(r$in_street_type,  "STREET")
  expect_equal(r$in_locality,     "BRISBANE")
})

# ---- Attached single-letter flat prefix (F8, A6, D2 etc.) ------------------
# User-supplied addresses often use an informal shorthand: a single capital
# letter immediately followed by the flat number, with no space — e.g. "F8"
# (Flat 8), "A6" (Apartment 6). The street number follows as a separate token.

test_that("F-prefix flat parses flat_type=FLAT, flat_number, and correct street number", {
  r <- address_parse("F8 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017")
  expect_equal(r$in_flat_type,    "FLAT")
  expect_equal(r$in_flat_number,  "8")
  expect_equal(r$in_number_first, 536L)
  expect_equal(r$in_street_name,  "BEACONSFIELD")
  expect_equal(r$in_street_type,  "TERRACE")
  expect_equal(r$in_locality,     "BRIGHTON")
})

test_that("A-prefix flat parses flat_type=APARTMENT and correct street number", {
  r <- address_parse("A6 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017")
  expect_equal(r$in_flat_type,    "APARTMENT")
  expect_equal(r$in_flat_number,  "6")
  expect_equal(r$in_number_first, 536L)
  expect_equal(r$in_street_name,  "BEACONSFIELD")
})

test_that("unknown-letter prefix defaults to UNIT flat_type", {
  r <- address_parse("D2 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "2")
  expect_equal(r$in_number_first, 536L)
  expect_equal(r$in_street_name,  "BEACONSFIELD")
})

test_that("letter+digit flat designator after street number is extracted from street name", {
  # "36 A1 TAVISTOCK ST" — "A1" is Apartment 1, not part of street name
  r <- address_parse("36 A1 TAVISTOCK ST, TORQUAY QLD 4655")
  expect_equal(r$in_flat_type,    "APARTMENT")
  expect_equal(r$in_flat_number,  "1")
  expect_equal(r$in_number_first, 36L)
  expect_equal(r$in_street_name,  "TAVISTOCK")
  expect_equal(r$in_street_type,  "STREET")
})

# ---- Abbreviation normalization (normalize = TRUE) -------------------------

test_that("MT and MNT expand to MOUNT in street name and locality", {
  r <- address_parse("5 MT GRAVATT RD, MT GRAVATT QLD 4122")
  expect_equal(r$in_street_name, "MOUNT GRAVATT")
  expect_equal(r$in_locality,    "MOUNT GRAVATT")

  r2 <- address_parse("5 MNT VIEW DR, MNT ISA QLD 4825")
  expect_equal(r2$in_street_name, "MOUNT VIEW")
  expect_equal(r2$in_locality,    "MOUNT ISA")
})

test_that("ST expands in street names but remains literal in GNAF localities", {
  r <- address_parse("10 ST JAMES CT, ST LUCIA QLD 4067")
  expect_equal(r$in_street_name, "SAINT JAMES")
  expect_equal(r$in_street_type, "COURT")
  expect_equal(r$in_locality,    "ST LUCIA")
})

test_that("NTH and STH expand to NORTH and SOUTH", {
  r <- address_parse("1 NTH QUAY ST, STH BRISBANE QLD 4101")
  expect_equal(r$in_street_name, "NORTH QUAY")
  expect_equal(r$in_locality,    "SOUTH BRISBANE")
})

test_that("leading single-letter compass expands only when field-initial", {
  r <- address_parse("1 N SHORE DR, W END QLD 4101")
  expect_equal(r$in_street_name, "NORTH SHORE")
  expect_equal(r$in_locality,    "WEST END")
})

test_that("CK expands to CREEK", {
  r <- address_parse("12 MOUNTAIN CK RD, MOUNTAIN CREEK QLD 4557")
  expect_equal(r$in_street_name, "MOUNTAIN CREEK")
  expect_equal(r$in_locality,    "MOUNTAIN CREEK")
})

test_that("ordinals in street name expand to words", {
  r <- address_parse("5 1ST AVE, BROADBEACH QLD 4218")
  expect_equal(r$in_street_name, "FIRST")
  expect_equal(r$in_street_type, "AVENUE")

  r2 <- address_parse("5 3RD AVE, BROADBEACH QLD 4218")
  expect_equal(r2$in_street_name, "THIRD")

  r3 <- address_parse("5 12TH ST, SOUTH BRISBANE QLD 4101")
  expect_equal(r3$in_street_name, "TWELFTH")
  expect_equal(r3$in_street_type, "STREET")
})

test_that("normalize = FALSE leaves abbreviations unchanged", {
  r <- address_parse("5 MT GRAVATT RD, MT GRAVATT QLD 4122", normalize = FALSE)
  expect_equal(r$in_street_name, "MT GRAVATT")
  expect_equal(r$in_locality,    "MT GRAVATT")

  r2 <- address_parse("10 ST JAMES CT, ST LUCIA QLD 4067", normalize = FALSE)
  expect_equal(r2$in_street_name, "ST JAMES")
  expect_equal(r2$in_locality,    "ST LUCIA")
})

test_that("street number matching the postcode digits is not deleted with it", {
  # The misspelt street type forces the .parse_single fallback, where the
  # postcode used to be removed by pattern (deleting BOTH "4000" tokens)
  # rather than by position.
  r <- address_parse("4000 SMITH STREEET BRISBANE QLD 4000")
  expect_equal(r$in_postcode,     4000L)
  expect_equal(r$in_number_first, 4000L)
  expect_equal(r$in_street_name,  "SMITH")
  expect_equal(r$in_street_type,  "STREET")
  expect_equal(r$in_locality,     "BRISBANE")
})

test_that("NA and empty inputs return all-NA rows with correct input_id", {
  r <- address_parse(c(NA, "", "   ", "10 SMITH ST BRISBANE QLD 4000"))
  expect_equal(nrow(r), 4L)
  expect_equal(r$input_id, 1:4)

  na_cols <- c("in_postcode", "in_state", "in_locality", "in_street_name",
               "in_street_type", "in_street_suffix", "in_number_first",
               "in_number_last", "in_number_suffix", "in_flat_type",
               "in_flat_number", "in_level_type", "in_level_number",
               "in_lot_number", "in_building_name")
  for (col in na_cols) {
    expect_true(all(is.na(r[[col]][1:3])), info = col)
  }

  expect_equal(r$in_street_name[4L], "SMITH")
  expect_equal(r$in_postcode[4L],    4000L)
})

test_that("flat, level, and lot designators remain distinct", {
  r <- address_parse(c(
    "Shop 14 Level 3 52 Davenport Rd, South Brisbane QLD 4101",
    "Lot 7 Kreis Rd, Westbrook QLD 4350"
  ), normalize = FALSE)
  expect_equal(r$in_flat_type[1L], "SHOP")
  expect_equal(r$in_flat_number[1L], "14")
  expect_equal(r$in_level_type[1L], "LEVEL")
  expect_equal(r$in_level_number[1L], "3")
  expect_equal(r$in_number_first[1L], 52L)
  expect_equal(r$in_lot_number[2L], "7")
  expect_true(is.na(r$in_number_first[2L]))
})

test_that("type-like sole street names and lot building prefixes stay structural", {
  r <- address_parse(c(
    "Golden Beach Resort Unit 810 75 Esplanade, Golden Beach QLD 4551",
    "Unit 2206 194 The Avenue, Peregian Springs QLD 4573",
    "Araby Lot 21 Armstrongs Lane, Moore QLD 4314"
  ), normalize = FALSE)
  expect_equal(r$in_street_name, c("ESPLANADE", "THE AVENUE", "ARMSTRONGS"))
  expect_equal(r$in_street_type, c(NA_character_, NA_character_, "LANE"))
  expect_equal(r$in_number_first, c(75L, 194L, NA_integer_))
  expect_equal(r$in_lot_number, c(NA_character_, NA_character_, "21"))
  expect_equal(r$in_building_name, c("GOLDEN BEACH RESORT", NA_character_, "ARABY"))
})

test_that("official GNAF street types are recognised exactly", {
  r <- address_parse(c(
    "1 Coral Cove, Red Hill QLD 4059",
    "2 Summit Outlook, Brisbane QLD 4000",
    "3 River Retreat, Westbrook QLD 4350",
    "4 Island Access, Hope Island QLD 4212"
  ), normalize = FALSE)
  expect_equal(r$in_street_type, c("COVE", "OUTLOOK", "RETREAT", "ACCESS"))
  expect_equal(r$in_street_name, c("CORAL", "SUMMIT", "RIVER", "ISLAND"))
})

test_that("structural duplicates preserve order and raw text", {
  x <- c("10 Smith St, St Lucia QLD 4067", "10 SMITH ST,ST LUCIA QLD 4067",
         "10 Smith St, St Lucia QLD 4067")
  r <- address_parse(x, normalize = FALSE)
  expect_equal(r$input_id, seq_along(x))
  expect_equal(r$input_raw, x)
  expect_true(all(r$in_street_name == "SMITH"))
  expect_true(all(r$in_locality == "ST LUCIA"))
})

test_that("parser validates inputs and keeps a typed zero-row result", {
  expect_error(address_parse(1:3), "character vector")
  expect_error(address_parse("x", normalize = NA), "TRUE or FALSE")
  empty <- address_parse(character())
  expect_s3_class(empty, "data.table")
  expect_equal(nrow(empty), 0L)
  expect_type(empty$in_postcode, "integer")
  expect_type(empty$in_level_number, "character")
})

test_that("abbreviated street directions preserve the street and locality boundary", {
  out <- address_parse(c("10 Main Rd N, Sydney NSW 2000",
                         "10 Main Rd Sth, Sydney NSW 2000",
                         "10 Main Rd, North Sydney NSW 2060"))
  expect_equal(out$in_street_name, rep("MAIN", 3L))
  expect_equal(out$in_street_type, rep("ROAD", 3L))
  expect_equal(out$in_street_suffix, c("N", "STH", NA_character_))
  expect_equal(out$in_locality, c("SYDNEY", "SYDNEY", "NORTH SYDNEY"))
})

# ---- Bare "U" unit marker + two-number convention --------------------------

test_that("bare U marker followed by unit and street numbers parses both", {
  r <- address_parse("U 20, 25 Smith Street, Cannonvale QLD 4802")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "20")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_name,  "SMITH")
})

# ---- Hyphen-range flat numbers ----------------------------------------------

test_that("hyphenated unit range with a bare marker parses as one flat_number", {
  r <- address_parse("UNIT 1-19 25 Illawong Street, Cannonvale QLD 4802")
  expect_equal(r$in_flat_number,  "1-19")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_name,  "ILLAWONG")
})

test_that("hyphenated unit range with a building name prefix is preserved", {
  r <- address_parse("My Building Name Unit 1-19 25 Illawong Street, Cannonvale QLD 4802")
  expect_equal(r$in_building_name, "MY BUILDING NAME")
  expect_equal(r$in_flat_number,   "1-19")
  expect_equal(r$in_number_first,  25L)
})

test_that("hyphenated unit range with no separate street number uses marker-only case", {
  r <- address_parse("Unit 1-19 Smith Street, Brisbane QLD 4000")
  expect_equal(r$in_flat_number,  "1-19")
  expect_equal(r$in_street_name,  "SMITH")
  expect_true(is.na(r$in_number_first))
})

test_that("hyphenated flat range and hyphenated street range both parse independently", {
  r <- address_parse("Unit 1-19 24-26 Illawong Street, Cannonvale QLD 4802")
  expect_equal(r$in_flat_number,  "1-19")
  expect_equal(r$in_number_first, 24L)
  expect_equal(r$in_number_last,  26L)
})

# ---- Number + letter slash suffix ("40/B") ----------------------------------

test_that("number/letter slash notation parses as a street-number suffix, not a unit", {
  r <- address_parse("40/B Smith Street, Brisbane QLD 4000")
  expect_true(is.na(r$in_flat_type))
  expect_equal(r$in_number_first,  40L)
  expect_equal(r$in_number_suffix, "B")
  expect_equal(r$in_street_name,   "SMITH")
})

test_that("digit/digit slash notation still takes priority over the number+letter pattern", {
  r <- address_parse("3/25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
})

# ---- VIEW/MILE/RING/END locality-collision words ----------------------------

test_that("VIEW as a locality word is not split off as a street type", {
  r <- address_parse("10 Station Street, Flinders View QLD 4305")
  expect_equal(r$in_street_type, "STREET")
  expect_equal(r$in_locality,    "FLINDERS VIEW")
})

test_that("MILE as a locality word is not split off as a street type", {
  r <- address_parse("5 Main Road, Eight Mile Plains QLD 4113")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_locality,    "EIGHT MILE PLAINS")
})

test_that("RING as a locality word is not split off as a street type", {
  r <- address_parse("20 Oxley Avenue, Kippa-Ring QLD 4021")
  expect_equal(r$in_street_type, "AVENUE")
  expect_equal(r$in_locality,    "KIPPA-RING")
})

test_that("END as a locality word is not split off as a street type", {
  r <- address_parse("15 Mollison Street, West End QLD 4101")
  expect_equal(r$in_street_name, "MOLLISON")
  expect_equal(r$in_street_type, "STREET")
  expect_equal(r$in_locality,    "WEST END")
})

test_that("a bare, house-number-less locality made of collision words is not split at all", {
  r <- address_parse("Flinders View QLD 4305")
  expect_true(is.na(r$in_street_type))
  expect_equal(r$in_locality, "FLINDERS VIEW")
})

# ---- Leading postcode/state order -------------------------------------------

test_that("leading postcode-then-state order resolves to locality with no street portion", {
  r <- address_parse("4012 QLD Nundah")
  expect_equal(r$in_postcode, 4012L)
  expect_equal(r$in_state,    "QLD")
  expect_equal(r$in_locality, "NUNDAH")
  expect_true(is.na(r$in_street_name))
})

test_that("leading postcode/state before a real street address still parses the street", {
  r <- address_parse("4000 QLD 12 Smith Street")
  expect_equal(r$in_postcode,     4000L)
  expect_equal(r$in_number_first, 12L)
  expect_equal(r$in_street_name,  "SMITH")
})

# ---- Multi-word building names ending in a plural marker --------------------

test_that("a plural dwelling word at the end of a multi-word building name is preserved", {
  r <- address_parse("Park View Apartments 3 12 Smith Street, Brisbane QLD 4000")
  expect_equal(r$in_building_name, "PARK VIEW APARTMENTS")
  expect_equal(r$in_flat_number,   "3")
  expect_equal(r$in_number_first,  12L)
})

test_that("a bare plural dwelling marker with nothing before it is still typo-corrected", {
  r <- address_parse("Units 3 24 Illawong Street, Cannonvale QLD 4802")
  expect_equal(r$in_flat_type,   "UNIT")
  expect_true(is.na(r$in_building_name))
})

# ---- Malformed multibyte input must not abort the batch ---------------------

test_that("invalid multibyte input in a locality-collision row does not crash the batch", {
  bad <- paste0("110 Musgrave Rd Red Hill 4059 ", rawToChar(as.raw(0xFF)))
  expect_error(address_parse(c(bad, "10 Smith St, Brisbane QLD 4000")), NA)
})
