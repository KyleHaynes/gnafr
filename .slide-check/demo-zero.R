library(gnafr)
library(data.table)

gnaf_dir <- Sys.getenv("GNAF_STANDARD_DIR", "C:/data/G-NAF/Standard")
db_path <- Sys.getenv("GNAF_DEMO_DB",
                      file.path(tempdir(), "gnafr-r-user-group.duckdb"))
con <- gnaf_connect(db_path, memory_limit = "4GB", threads = 4L)

system.time(gnaf_build_db(con, gnaf_dir, states = "QLD"))

## flowchart TB
##   R["R session"] --> D["DuckDB engine"]
##   D <--> F[("gnafr.duckdb")]
##   D --> T["Selected results in R"]
##   style D fill:#e3f2ef,stroke:#21705b,stroke-width:2px
##   style F fill:#e9f2fa,stroke:#236188,stroke-width:2px

## flowchart LR
##   P["G-NAF PSV files"] -->|read and join in SQL| D[("Reference tables")]
##   R["R: parse the input batch"] --> I["Register parsed rows"]
##   subgraph SQL["DuckDB: work on the batch"]
##     I --> C["Find candidates"] --> S["Score components"] --> K["Rank per input"]
##     D --> C
##   end
##   K --> O["R: data.table of results"]
##   style SQL fill:#f0f6fa,stroke:#236188
##   style O fill:#e3f2ef,stroke:#21705b,stroke-width:2px

## %%{init: {'themeVariables': {'fontSize': '18px'}, 'flowchart': {'curve': 'basis'}}}%%
## flowchart TB
##   A["📥 Address vector"] --> B["1 · Parse and standardise in R"]
##   B --> C["2 · Exact / cache lookup; retrieve candidates"]
##   D[("DuckDB<br/>G-NAF + aliases + custom rows")] --> C
##   C --> E["3 · Score six components in DuckDB"]
##   E --> F["4 · Combine candidates and rank"]
##   E -.->|weak evidence or no result| G["Broaden the search<br/>locality → number → optional street-only"]
##   G -.->|score new candidates| E
##   F --> H["5 · Resolve linked address; append geographies"]
##   H --> I["📤 data.table: labels, PIDs, coordinates, scores"]
##   style B fill:#e9f2fa,stroke:#236188
##   style C fill:#e9f2fa,stroke:#236188
##   style E fill:#e3f2ef,stroke:#21705b
##   style F fill:#e3f2ef,stroke:#21705b
##   style G fill:#fff1db,stroke:#b77526
##   style I fill:#12405c,color:#fff,stroke:#12405c

address_parse("U 2, 16 wally st, nundah qld 4012")

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

gnaf_status(con)
addresses <- c(
  "U 2, 16 wally st, nundah qld 4012",
  "27 Ivo Stret, Nundah QLD 4012",
  "61A William Street, Portsmith QLD 4870",
  "15 Cumming Pde, Point Lookout QLD 4183",
  "Unknown address"
)
results <- gnaf_match(addresses, con, max_results = 3L,
                      min_score = 60L, cache = FALSE)
results[, .(input_id, address_label, matched, match_rank,
            total_score, score_number, score_flat, alias_type)]

core_only <- gnaf_match(addresses, con, include_aliases = FALSE)

## flowchart LR
##   A["Matched alias<br/>UNIT 2 10 OLD STREET"] -->|principal_pid| B["Principal address<br/>UNIT 2 20 NEW ROAD"]
##   B -->|primary_pid| C["Primary address<br/>20 NEW ROAD"]
##   style A fill:#fff1db,stroke:#b77526
##   style B fill:#e3f2ef,stroke:#21705b
##   style C fill:#e9f2fa,stroke:#236188

resolved <- gnaf_match(addresses, con, return_principal = TRUE,
                       return_primary = TRUE, cache = FALSE)
resolved[, .(matched_address_label, address_label,
             longitude, latitude, geocode_type, score_number)]

review_file <- file.path(
  tempdir(), "gnafr-review.html"
)
gnaf_threshold_filter(
  results,
  html = review_file,
  launch.browser = TRUE
)

review <- gnaf_threshold_filter(
  results,
  html = FALSE,
  max_rows = 30L
)

# Illustrative rule: replace with the rule agreed from labelled examples.
accepted <- results[
  matched == TRUE & match_rank == 1L &
  total_score >= 90L & score_number == 10L & score_flat == 5L
]

source_rows <- data.table(input_id = seq_along(addresses),
                          original_address = addresses)
final <- accepted[source_rows, on = "input_id"]

office_sites <- data.table(
  address_detail_pid = "DEMO_SITE_001",
  number_first = 42L, street_name = "EXAMPLE", street_type = "ROAD",
  locality_name = "BRISBANE", state = "QLD", postcode = 4000L
)
gnaf_add(con, office_sites)
custom_result <- gnaf_match("42 Example Rd, Brisbane QLD 4000", con)
custom_result[, .(address_label, source, total_score, longitude, latitude)]

## flowchart LR
##   A[("Address coordinates")] --> B["Distinct coordinate pairs"]
##   P["SA2 polygons + attributes"] --> C["sf point-in-polygon lookup in R"]
##   B --> C
##   C --> D[("Saved PID → SA2 table<br/>inside DuckDB")]
##   M["Final returned address PID"] --> J["Join saved attributes"]
##   D --> J --> R["Match result + SA2 code / name"]
##   style C fill:#e3f2ef,stroke:#21705b
##   style D fill:#e9f2fa,stroke:#236188

sa2_file <- system.file("extdata", "SA2", "SA2_2021_AUST_GDA2020.shp",
                         package = "gnafr")
sa2 <- sf::st_read(sa2_file, quiet = TRUE)
gnaf_add_geography(con, "sa2_2021", sa2,
  return_cols = c(sa2_code = "SA2_CODE21", sa2_name = "SA2_NAME21"),
  points_crs = 7844
)

gnaf_list_geographies(con)
with_sa2 <- gnaf_match(addresses, con, return_principal = TRUE,
                       geographies = "sa2_2021")
with_sa2[, .(input_id, address_label, sa2_code, sa2_name)]

gnaf_disconnect(con)
