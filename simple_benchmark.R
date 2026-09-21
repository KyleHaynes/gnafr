# simple_benchmark.R
#
# One-off benchmark:
#   1. Build a fresh QLD-only GNAF database named "test" from the raw
#      G-NAF "Standard" PSV product.
#   2. Load simulated_inputs.rds.
#   3. Time a default gnaf_match() call on the first 100,000 simulated
#      addresses with system.time().
#
# Run from the package root (where simulated_inputs.rds lives) so the
# relative path resolves. Uses devtools::load_all() so the source package is
# exercised rather than an installed version.

# Use the source package rather than the installed namespace
devtools::load_all(".", quiet = TRUE)

# ---- 1. Build a fresh QLD-only database named "test" ------------------------

gnaf_dir <- "C:/temp/gnaf/G-NAF/G-NAF MAY 2026/Standard"
db_path  <- "C:/temp/test3d.duckdb"   # the "test" database

if (!dir.exists(gnaf_dir)) {
  stop("G-NAF Standard directory not found: ", gnaf_dir)
}

# Remove any previous test database so the build starts from a clean slate.
# DuckDB may also leave a spill directory (<path>.tmp) and a .wal file behind.
for (path in c(db_path, paste0(db_path, ".tmp"), paste0(db_path, ".wal"))) {
  if (file.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
}

con <- gnaf_connect(
  db_path
)

# Builds the schema, loads every QLD record/alias, and derives street aliases.
gnaf_build_db(con, gnaf_dir, states = "QLD")

# ---- 2. Load the simulated inputs ------------------------------------------

simulated_inputs <- readRDS("simulated_inputs.rds")

# ---- 3. Benchmark default gnaf_match() on the first 100,000 inputs ----------
set.seed(1)
inputs <- sample(simulated_inputs$simulated_address, 8000)

timing <- system.time(
  result <- gnaf_match(inputs, con)   # all defaults (max_results = 1, min_score = 60, cache = TRUE)
)

gnaf_threshold_filter(result)

result[input_id %in% c(5950, 2809, 14)]
address_parse(result[input_id %in% c(5950, 2809, 14)]$input_raw)[]


gnaf_match(c("190 MUSGRAVE RD, RED HILL QLD 4059", "190 MUSGRAVE RD, RED HILL QLD 4000"), con = con)


print(timing)
cat("Rows returned:  ", nrow(result), "\n")
cat("Matched inputs: ",
    data.table::uniqueN(result[!is.na(address_detail_pid)]$input_id), "\n")

gnaf_disconnect(con)

# 2026-09-18: Before a tune up.
> print(timing)
   user  system elapsed 
4362.61   51.66  575.43 
> cat("Rows returned:  ", nrow(result), "\n")
Rows returned:   50000 
> cat("Matched inputs: ",
+     data.table::uniqueN(result[!is.na(address_detail_pid)]$input_id), "\n")
Matched inputs:  49837 