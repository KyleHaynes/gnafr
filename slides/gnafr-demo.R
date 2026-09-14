# Generated from gnafr-overview.qmd by slides/extract-demo.R.
# Run one labelled section at a time during the presentation.
# Set GNAF_STANDARD_DIR to the extracted G-NAF Standard folder beforehand.
# To use a prepared demo database, set GNAF_DEMO_DB and skip demo-build.
# Skip demo-add-sa2 if that database already contains sa2_2021.
# Press Done in the Shiny app before continuing to the next section.

# demo-setup ----
library(gnafr)
library(data.table)

gnaf_dir <- Sys.getenv("GNAF_STANDARD_DIR", "C:/data/G-NAF/Standard")
db_path <- Sys.getenv("GNAF_DEMO_DB",
                      file.path(tempdir(), "gnafr-r-user-group.duckdb"))
con <- gnaf_connect(db_path, memory_limit = "4GB", threads = 4L)

# demo-build ----
system.time(gnaf_build_db(con, gnaf_dir, states = "QLD"))

# demo-parse-one ----
address_parse("U 2 16 wally st, nundah qld 4012")

# demo-parse-cases ----
parse_examples <- c(
  "10 St James Ct, St Lucia QLD 4067",
  "5 Mt Gravatt Rd, Mt Gravatt QLD 4122",
  "5 1st Ave, Broadbeach QLD 4218",
  "Shop 14 Level 3 52 Davenport Rd, South Brisbane QLD 4101",
  "Lot 7 Kreis Rd, Westbrook QLD 4350",
  "Unti 2 16 Wally St, Nundah QLD 4012"
)
parsed <- address_parse(parse_examples)
parsed[, .(in_flat_number, in_level_number, in_lot_number,
           in_number_first, in_street_name, in_locality)]

# demo-match ----
gnaf_status(con)
addresses <- c(
  "U 2 16 wally st, nundah qld 4012",
  "27 Ivo Stret, Nundah QLD 4012",
  "61A William Street, Portsmith QLD 4870",
  "15 Cumming Pde, Point Lookout QLD 4183",
  "Unknown address"
)
results <- gnaf_match(addresses, con, max_results = 3L,
                      min_score = 60L, cache = FALSE)
results[, .(input_id, address_label, matched, match_rank,
            total_score, score_number, score_flat, alias_type)]

# demo-alias-control ----
core_only <- gnaf_match(addresses, con, include_aliases = FALSE)

# demo-linked-return ----
resolved <- gnaf_match(addresses, con, return_principal = TRUE,
                       return_primary = TRUE, cache = FALSE)
resolved[, .(matched_address_label, address_label,
             longitude, latitude, geocode_type, score_number)]

# demo-static-review ----
review_file <- file.path(
  tempdir(), "gnafr-review.html"
)
gnaf_threshold_filter(
  results,
  html = review_file,
  launch.browser = TRUE
)

# demo-shiny-review ----
review <- gnaf_threshold_filter(
  results,
  html = FALSE,
  max_rows = 30L
)

# demo-acceptance-rule ----
# Illustrative rule: replace with the rule agreed from labelled examples.
accepted <- results[
  matched == TRUE & match_rank == 1L &
  total_score >= 90L & score_number == 10L & score_flat == 5L
]

source_rows <- data.table(input_id = seq_along(addresses),
                          original_address = addresses)
final <- accepted[source_rows, on = "input_id"]

# demo-custom-address ----
office_sites <- data.table(
  address_detail_pid = "DEMO_SITE_001",
  number_first = 42L, street_name = "EXAMPLE", street_type = "ROAD",
  locality_name = "BRISBANE", state = "QLD", postcode = 4000L
)
gnaf_add(con, office_sites)
custom_result <- gnaf_match("42 Example Rd, Brisbane QLD 4000", con)
custom_result[, .(address_label, source, total_score, longitude, latitude)]

# demo-add-sa2 ----
sa2_file <- system.file("extdata", "SA2", "SA2_2021_AUST_GDA2020.shp",
                         package = "gnafr")
sa2 <- sf::st_read(sa2_file, quiet = TRUE)
gnaf_add_geography(con, "sa2_2021", sa2,
  return_cols = c(sa2_code = "SA2_CODE21", sa2_name = "SA2_NAME21"),
  points_crs = 7844
)

# demo-use-sa2 ----
gnaf_list_geographies(con)
with_sa2 <- gnaf_match(addresses, con, return_principal = TRUE,
                       geographies = "sa2_2021")
with_sa2[, .(input_id, address_label, sa2_code, sa2_name)]

# demo-cleanup ----
gnaf_disconnect(con)

