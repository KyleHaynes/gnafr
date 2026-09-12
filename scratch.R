require(data.table)
# require(gnafr)
devtools::document()


## Benchmarks
# 100k
d = fread("C:\\temp\\gnaf.qld.csv")
simulated_inputs = address_perturb_sample(
    d,
    n = 1E6,
    seed = 3
)
saveRDS(simulated_inputs, "simulated_inputs.rds")
simulated_inputs <- readRDS("simulated_inputs.rds")

con <- gnaf_connect("C:/temp/gnafx23.duckdb")
# simulated_inputs <- readRDS("x.rds")
result_dt <- gnaf_match(
        c(simulated_inputs$simulated_address[200000:300000]),
        con,
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE,
        cache = FALSE # Turning off to benchmark bad addresses
)
# ℹ Parsing 100,001 addresses.
# ℹ Standardising parsed input addresses.
# • Input standardisation completed in 1.28s.
# • 0 input(s) matched via exact label lookup in 0.28s.
# • 0 input(s) served from match cache in 0.00s.
# ℹ Scoring 99,956 input(s) across 434 unique postcode(s) in DuckDB.
# • gnaf_addresses (postcode): 93,293 row(s) returned in 656.54s.
# • custom_addresses (postcode): 2 row(s) returned in 0.11s.
# ℹ Running locality fallback for 4,721 input rows.
# • gnaf_addresses (locality): 53 row(s) returned in 215.48s.
# • custom_addresses (locality): 0 row(s) returned in 0.09s.
# ℹ Wrangling final match output.
# • Final output wrangling completed in 1.54s.

# ── gnaf_match summary ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ✔ Matched 93,375 of 100,001 input rows (93.4%).
# • Returned 93,375 candidate rows after ranking and filtering.
# • Unmatched inputs above min_score: 6,626.
# • Exact label matches: 0 in 0.28s.
# • Cache matches: 0 in 0.00s.
# • Slow-path matches: 93,375 in 876.77s.
# • Average top-match score: 98.3.
# Timings: parse 5.70s, standardise 1.28s, slow path 876.77s, wrangle 1.54s, total 886.12s.


simulated_inputs[, no_match := fifelse(simulated_address %in% result_dt[(!matched)]$input_raw, T, F)]
simulated_inputs[(no_match), .(simulated_address, ADDRESS_LABEL, perturbations)]

gnaf_match(c("43 THE PINNACLE, WORONGARY QLD 4213", ""), con)



# Test "perfect" addresses
result_dt <- gnaf_match(
        d$ADDRESS_LABEL[1:20000],
        con,
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)


result_dt[]

require(data.table.ext)

result_dt[]            
Error in class_colors[[tok]] : subscript out of bounds

## App -----------------------------------
gnaf_app(db_path = "C:/temp/gnaf.duckdb")

# ---- 2026-06-06 verbose timing breakdown ----
# Checks that gnaf_match() now reports parse, input standardisation,
# exact/cache/slow-path, and final wrangling timings separately.
result_dt <- gnaf_match(
        c(simulated_inputs$simulated_address[1:25], d$ADDRESS_LABEL[1:25]),
        con,
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)


gnaf_match("25 ST JAMES CR EAGLE HEIGHTS QLD 4271", con = con, weights = list(postcode = 20L, suburb = 15L, street_name = 40L, street_type = 10L, number = 10L, flat = 5L))
gnaf_match("25ST JAMES CR EAGLE HEIGHTS QLD 4271", con = con, max_results = 1)$address_label
 input_id                             input_raw                           input_standardised address_label


test <- gnaf_match(c(
    "25 ST JAMES CR EAGLE HEIGHTS QLD 4271",
    "25 ST JAMES CR EAGLE HEIGHTS 4271 QLD",
    "110-120 MUSGRADE RD RED HILL QLD 4060",
    "112 MUSGRAVE RD RED HILLS QLD 4059",
    "112 MUSGRAVE RD PADDINGTON 4059",
    "PARLAND 6019/6 PARKLAND BVD BRISBANE QLD 4001",
    "PARLAND 6019 6 PARKLAND BVD BRISBANE QLD 4001",
    "U6019 6 PARKLAND BVD BRISBANE QLD 4001"
), con = con, max_results = 1)

# Fall back on street type. Kinda not ideal to pass both arguments
gnaf_match("190 MUSGRAVE RD RED HILL 4059 QLD", con, alias_types = c("street_only"), street_only_fallback = T)
gnaf_match("190a MUSGRAVE RD RED HILL 4059 QLD", con, alias_types = c(NA), street_only_fallback = T)


d[ADDRESS_LABEL %plike% "\\d\\D "]


# Following are parsing badly from work (2026-06-09)
vec <- c(
"F8 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017",
"10 ERITT COURT, CARBROOK QLD 4130",
"U 20 20-24 REDMAN ST, EMU PARK QLD 4710",
"U 5A 5A PRINCE ST, HARLAXTON QLD 4350",
"U 5 1 TOM MURPHY DR, LONGREACH QLD 4730",
"U 1 2-4 STAGHORN AVE, SURFERS PARADISE QLD 4217",
"U 12 12 BISMARCK ST, MACLAGAN QLD 4352",
"U 4 10 SPRINGFIELD PLACE, FOREST LAKE QLD 4078",
"U 11 17 WOOD ST, MACKAY QLD 4740",
"U 79 256-270 SPRING STREE, KEARNEYS SPRING QLD 4350",
"U 1 49 DAUAN LANE, THURSDAY ISLAND QLD 4875",
"U 1 1 MARRIOTT PLACE, COES CREEK QLD 4560",
"U 105 1-25 PARNELL BOULEVARD, ROBINA QLD 4226",
"U 14 1 TAYLA ST, PIMPAMA QLD 4209",
"D2 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017",
"U 1 66 POWER ST, COOKTOWN QLD 4895",
"DONGA 10 296 MUKAKIYA ST, WELLESLEY ISLANDS QLD 4892",
"U 5 11A STURGESS CRESCENT, DALBY QLD 4405",
"U 503 8 BUCKINGHAM ST, ALEXANDRA HILLS QLD 4161",
"U 3 14 COLLINS ST, ATHERTON QLD 4883",
"U 1 97 OAK STREEK, BARCALDINE QLD 4725",
"U 3 3 CORKWOOD COURT, ARANA HILLS QLD 4054",
"36 A1 TAVISTOCK ST, TORQUAY QLD 4655",
"U 2 273 BRETON ST, COOPERS PLAINS QLD 4108",
"A6 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017",
"U 701 65-67 TYRON ST, UPPER MOUNT GRAVATT QLD 4122",
"U 106 1 CLUNIES ROSS COURT, EIGHT MILE PLAINS QLD 4113",
"U 3A 3A MAYO COURT, REDLYNCH QLD 4870",
"U 1 5 MAXWELL COURT, WINTON QLD 4735",
"U 1 17 THE GROVE ST, ALEXANDRA HEADLAND QLD 4572",
"U 13 29 CHERIMOYA PLACE, SUNNYBANK HILLS QLD 4109",
"U 15 1 TAYLA ST, PIMPAMA QLD 4209",
"U 18 1 TAYLA ST, PIMPAMA QLD 4209",
"U 5E 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017",
"U 5 5 SUNVILLA COURT, NAMBOUR QLD 4560"
)

ee = gnaf_match(vec, con = con, max_results = 1)
dd[address_label != ee$address_label]
