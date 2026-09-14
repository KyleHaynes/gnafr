# Reproduce the slow-address benchmark without changing the database.
# See MATCHING_PERFORMANCE.md for before/after comparison instructions.
devtools::load_all(".", quiet = TRUE)

run_match_performance_benchmark <- function() {
  db_path <- Sys.getenv("GNAFR_BENCH_DB", "")
  input_path <- Sys.getenv("GNAFR_PERF_INPUTS", "simulated_inputs.rds")
  compare_path <- Sys.getenv("GNAFR_PERF_COMPARE", "")
  save_path <- Sys.getenv("GNAFR_PERF_SAVE", "")
  integer_setting <- function(name, default, minimum = 1L, maximum = .Machine$integer.max) {
    value <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
    if (length(value) != 1L || !is.finite(value) || value != trunc(value) ||
        value < minimum || value > maximum) {
      stop(name, " must be an integer between ", minimum, " and ", maximum)
    }
    as.integer(value)
  }
  start <- integer_setting("GNAFR_PERF_START", 200000L)
  n <- integer_setting("GNAFR_PERF_N", 100001L)
  min_score <- integer_setting("GNAFR_PERF_MIN_SCORE", 80L, 0L, 100L)
  max_results <- integer_setting("GNAFR_PERF_MAX_RESULTS", 1L)
  if (!nzchar(db_path) || !file.exists(db_path)) stop("Set GNAFR_BENCH_DB to an existing DuckDB file")
  if (!file.exists(input_path)) stop("Input RDS does not exist: ", input_path)
  if (nzchar(compare_path) && !file.exists(compare_path)) stop("Comparison RDS does not exist: ", compare_path)
  if (nzchar(save_path) && (file.exists(save_path) || !dir.exists(dirname(save_path)))) {
    stop("GNAFR_PERF_SAVE must be a new file in an existing directory")
  }

  data <- readRDS(input_path)
  if (!is.character(data$simulated_address)) stop("Input RDS needs a character simulated_address column")
  end <- as.double(start) + n - 1
  if (end > length(data$simulated_address)) stop("Requested slice exceeds the input length")
  inputs <- data$simulated_address[seq.int(start, end)]
  comparison <- if (nzchar(compare_path)) readRDS(compare_path) else NULL
  if (!is.null(comparison) && !identical(inputs, comparison$inputs)) {
    stop("Comparison must use exactly the same input addresses and order")
  }
  settings <- list(max_results = max_results, min_score = min_score, cache = FALSE)
  if (!is.null(comparison$settings) && !identical(settings, comparison$settings)) {
    stop("Comparison must use the same matching settings")
  }

  con <- gnaf_connect(db_path, read_only = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  database_settings <- DBI::dbGetQuery(con, paste(
    "SELECT version() AS duckdb_version, current_setting('threads') AS threads,",
    "current_setting('memory_limit') AS memory_limit"
  ))
  print(database_settings)
  elapsed <- system.time(result <- gnaf_match(
    inputs, con, max_results = max_results, min_score = min_score,
    cache = FALSE, verbose = TRUE
  ))[["elapsed"]]
  summary <- data.table::data.table(
    inputs = length(inputs), elapsed_seconds = elapsed,
    inputs_per_second = round(length(inputs) / elapsed),
    matched_inputs = data.table::uniqueN(result[!is.na(address_detail_pid), input_id])
  )
  if (!is.null(comparison)) {
    # Compare every returned column and its type, including unmatched rows,
    # ranking and component scores. Ignore data.table's internal pointer.
    summary[, `:=`(
      previous_seconds = comparison$elapsed,
      speedup = comparison$elapsed / elapsed,
      identical_results = identical(as.list(result), as.list(comparison$result))
    )]
  }
  print(summary)
  output <- list(
    result = result, elapsed = elapsed, inputs = inputs, settings = settings,
    database = normalizePath(db_path, winslash = "/"),
    database_settings = database_settings, summary = summary,
    session = utils::sessionInfo()
  )
  if (nzchar(save_path)) saveRDS(output, save_path)
  if (!is.null(comparison) && !summary$identical_results) {
    stop("Benchmark results changed; inspect the saved output before accepting the speedup")
  }
  invisible(output)
}

benchmark <- run_match_performance_benchmark()
