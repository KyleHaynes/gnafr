require(data.table)
simulated_inputs <- readRDS("simulated_inputs.rds")


devtools::load_all()
con <- gnaf_connect("C:/temp/gnafx23.duckdb")

gnaf_match(c(
    "67 ANNIE DRIVE, CAWARRA QLD 4702",
    "67 ANNIE DRIVE, CAWARRA QLD 4702",
    "15 CUMMING PDE POINT LOOKOUT QLD 4183",
    "15 CUMMING PDE, POINT LOOKOUT QLD 4183",
    "15 CUMMING PDE, POINT LOOKOUT 4183",
    "assd 15 CUMMING PDE POINT LOOKOUT QLD 4183",
    "assd 15 CUMMING PARADE POINT LOOKOUT 4183",
    "assd 15 CUMMING PARADE POINT LOOKOUT QLD 4183",
    "assd 15 CUMMING PARADE, POINT LOOKOUT QLD 4183",
    "UNIT 20/15 CUMMING PDE POINT LOOKOUT QLD 4183"
), con = con, verbose = F)


gnaf_match(c(
    "67 ANNIE DRIVE, CAWARRA QLD 4702",
    "67 ANNIE DRIVE, CAWARRAL QLD 4702"
), con = con, verbose = F, street_only_fallback = T)[, 2:6]

gnaf_match(c(
    "61A WILLIAM STREET, PORTSMITH QLD 4870",
    "61 WILLIAM STREET, PORTSMITH QLD 4870",
    "5 BABIECA SAINT, BUSHLAND BEACH QLD 4818",
    "5 BABIECA ST, BUSHLAND BEACH QLD 4818",
    "91 CORNWALL STREET, GREENSLOPES QLD 4120",
    "91 CORNWALL STREET, Annerley QLD 4120"
), con = con, verbose = F)[, 2:12]






gnaf_match(c(
    "20 110 MUSGRAVE RD, RED HILL QLD 4059",
    "20 110 MUSGRAVE RD, PADDINGTON QLD 4060",
    "20 122 MUSGRAVE RD, RED HILL QLD 4061",
    "20 122 MUSGRAVE RD, RED HILL QLD 4059",
    "20 120 MUSGRAVE RD, RED HILL QLD 4059",
    "1222 MUSGRAVE RD, RED HILL QLD 4059",
    "130 MUSGRAVE RD, RED HILL QLD 4059"
), con = con, verbose = F, street_only_fallback = F, return_principal = T)[, 2:6]
#                             input_raw                   input_standardised                       address_label match_rank matched
#                                <char>                               <char>                              <char>      <num>  <lgcl>
# 1: 122 MUSGRAVE RD, RED HILL QLD 4059 122 MUSGRAVE ROAD, RED HILL QLD 4059 122 WINDSOR ROAD, RED HILL QLD 4059          1    TRUE


simulated_inputs <- readRDS("simulated_inputs.rds")
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
gnaf_threshold_filter(result_dt[1:10000], html = T)

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