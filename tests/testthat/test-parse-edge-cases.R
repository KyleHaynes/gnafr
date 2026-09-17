test_that("locality collisions resolve without comma hints", {
  localities <- c("WEST END", "FLINDERS VIEW", "EIGHT MILE PLAINS",
                  "KIPPA-RING", "ROCKY VIEW", "NORTH LAKES", "ST LUCIA")
  for (prefix in c("", "10 ", "River House U20 10 ")) {
    x <- paste0(prefix, "Mollison St ", localities, " QLD 4101")
    p <- address_parse(x)
    expect_equal(p$in_street_name, rep("MOLLISON", length(x)))
    expect_equal(p$in_street_type, rep("STREET", length(x)))
    expect_equal(p$in_locality, localities)
    expect_true(all(is.na(p$in_street_suffix)))
  }
  p <- address_parse("Mollison St West End 4101")
  expect_equal(gnafr:::.standardise_input(p), "MOLLISON STREET, WEST END 4101")
})

test_that("real street types and directions survive locality disambiguation", {
  p <- address_parse(c("10 High St", "Mollison St", "10 High View",
    "10 Park View Flinders View QLD 4305",
    "10 St James St St Lucia QLD 4067",
    "10 Main Rd North, Sydney NSW 2000", "10 Main Rd N Sydney NSW 2000"))
  expect_equal(p$in_street_type,
    c("STREET", "STREET", "VIEW", "VIEW", "STREET", "ROAD", "ROAD"))
  expect_equal(p$in_street_name,
    c("HIGH", "MOLLISON", "HIGH", "PARK", "SAINT JAMES", "MAIN", "MAIN"))
  expect_equal(p$in_locality,
    c(rep(NA_character_, 3L), "FLINDERS VIEW", "ST LUCIA", "SYDNEY", "SYDNEY"))
  expect_equal(p$in_street_suffix, c(rep(NA_character_, 5L), "NORTH", "N"))
})

test_that("terminal ST remains a type after type-like street-name words", {
  p <- address_parse(c("10 Mount View St", "10 Park Lane St", "Mount View St"))
  expect_equal(p$in_street_name, c("MOUNT VIEW", "PARK LANE", "MOUNT VIEW"))
  expect_equal(p$in_street_type, rep("STREET", 3L))
  expect_true(all(is.na(p$in_locality)))
})

test_that("range punctuation is normalized across unit notations", {
  x <- c("Unit 1 -19 25 Smith St", "Unit 1- 19 25 Smith St",
    "Unit 1 \u2013 19 25 Smith St", "Unit 1-19/25 Smith St",
    "U1-19 25 Smith St", "Park View Apartments U1-19 25 Smith St",
    "Park View Apartments 1-19 / 25 Smith St")
  p <- address_parse(paste0(x, ", Brisbane QLD 4000"))
  expect_equal(p$in_flat_number, rep("1-19", length(x)))
  expect_equal(p$in_flat_type, rep("UNIT", length(x)))
  expect_equal(p$in_number_first, rep(25L, length(x)))
  expect_equal(p$in_street_name, rep("SMITH", length(x)))
  expect_equal(p$in_building_name,
    c(rep(NA_character_, 5L), rep("PARK VIEW APARTMENTS", 2L)))
})

test_that("slash letter suffixes compose with units and building names", {
  x <- c("40 / B Smith St", "Unit 3 40/B Smith St", "3/40/B Smith St",
    "Park View Apartments Unit 3 40 / B Smith St",
    "Park View Apartments 3 / 40 / B Smith St")
  p <- address_parse(paste0(x, ", Brisbane QLD 4000"))
  expect_equal(p$in_number_first, rep(40L, length(x)))
  expect_equal(p$in_number_suffix, rep("B", length(x)))
  expect_equal(p$in_flat_number, c(NA_character_, rep("3", 4L)))
  expect_equal(p$in_flat_type, c(NA_character_, rep("UNIT", 4L)))
  expect_equal(p$in_street_name, rep("SMITH", length(x)))
  expect_equal(p$in_building_name,
    c(rep(NA_character_, 3L), rep("PARK VIEW APARTMENTS", 2L)))
})

test_that("geo fields can lead the locality after a street comma", {
  x <- c("10 Smith St, 4012 QLD Nundah", "10 Smith St, QLD 4012 Nundah",
    "10 Smith St, Nundah, QLD, 4012", "10 Smith St, Nundah, 4012, QLD",
    "4012 QLD 10 Smith St, Nundah", "QLD 4012 10 Smith St, Nundah")
  p <- address_parse(x)
  expect_equal(p$in_postcode, rep(4012L, length(x)))
  expect_equal(p$in_state, rep("QLD", length(x)))
  expect_equal(p$in_locality, rep("NUNDAH", length(x)))
  expect_equal(p$in_street_name, rep("SMITH", length(x)))
  expect_equal(p$in_number_first, rep(10L, length(x)))
  expect_true(all(is.na(p$in_building_name)))
})

test_that("extra geo commas preserve the street and locality boundary", {
  x <- c("15 Mollison St, West End, QLD, 4101",
    "15 Mollison St, West End, QLD 4101",
    "15 Mollison St West End, QLD 4101")
  p <- address_parse(x)
  expect_equal(p$in_street_name, rep("MOLLISON", length(x)))
  expect_equal(p$in_street_type, rep("STREET", length(x)))
  expect_equal(p$in_locality, rep("WEST END", length(x)))
  expect_equal(p$in_state, rep("QLD", length(x)))
  expect_equal(p$in_postcode, rep(4101L, length(x)))
})

test_that("pasted whitespace and hyphens preserve address structure", {
  x <- c("U 20, 25 Smith St, Kippa- Ring QLD 4021",
    "U\u00a020,\u00a025 Smith St, Kippa\u2011Ring QLD 4021",
    "U&#x20;20,&#32;25 Smith St, Kippa-Ring QLD 4021",
    "U&nbsp;20,&nbsp;25 Smith St, Kippa-Ring QLD 4021")
  p <- address_parse(x)
  expect_equal(p$in_flat_type, rep("UNIT", length(x)))
  expect_equal(p$in_flat_number, rep("20", length(x)))
  expect_equal(p$in_number_first, rep(25L, length(x)))
  expect_equal(p$in_street_name, rep("SMITH", length(x)))
  expect_equal(p$in_locality, rep("KIPPA-RING", length(x)))
  expect_identical(p$input_raw, x)
})

test_that("invalid UTF-8 bytes are repaired without losing valid address fields", {
  bad <- paste0("10 Smith St, Brisbane QLD 4000", rawToChar(as.raw(0xFF)))
  Encoding(bad) <- "UTF-8"
  latin <- iconv("Caf\u00e9 10 Smith St, Brisbane QLD 4000",
    from = "UTF-8", to = "latin1")
  Encoding(latin) <- "latin1"
  x <- c(bad, "10 Smith St, Brisbane QLD 4000", latin, NA_character_, "")
  expect_no_warning(p <- address_parse(x))
  expect_identical(p$input_raw, x)
  expect_equal(p$in_postcode, c(rep(4000L, 3L), NA_integer_, NA_integer_))
  expect_equal(p$in_locality, c(rep("BRISBANE", 3L), NA_character_, NA_character_))
  expect_equal(p$in_building_name[3L], "CAF\u00c9")
})
