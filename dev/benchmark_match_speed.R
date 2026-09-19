# Reproduce simple_benchmark.R's seed-1 sample without rebuilding the database.
# Set GNAFR_SPEED_N for a prefix of the same 50,000-input sample, GNAFR_SPEED_SAVE
# to a new RDS path, and GNAFR_SPEED_COMPARE to a saved baseline. Optional SQL
# profiles go to GNAFR_SPEED_PROFILE (an existing directory).
pkgload::load_all(".", quiet = TRUE)

run_match_speed_benchmark <- function() {
  n <- as.integer(Sys.getenv("GNAFR_SPEED_N", "50000"))
  stopifnot(length(n) == 1L, !is.na(n), n > 0L, n <= 50000L)
  path <- Sys.getenv("GNAFR_SPEED_DB", "C:/temp/test.duckdb")
  save_path <- Sys.getenv("GNAFR_SPEED_SAVE", "")
  compare_path <- Sys.getenv("GNAFR_SPEED_COMPARE", "")
  profile_dir <- Sys.getenv("GNAFR_SPEED_PROFILE", "")
  if (nzchar(save_path) && file.exists(save_path)) stop("Refusing to overwrite ", save_path)
  if (nzchar(profile_dir) && !dir.exists(profile_dir)) stop("Profile directory does not exist")
  data <- readRDS("simulated_inputs.rds")
  set.seed(1L)
  inputs <- sample(data$simulated_address, 50000L)[seq_len(n)]
  rm(data)
  con <- gnaf_connect(path, read_only = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  print(DBI::dbGetQuery(con, paste(
    "SELECT version() AS version, current_setting('threads') AS threads,",
    "current_setting('memory_limit') AS memory_limit")))
  if (nzchar(profile_dir)) {
    assign(".speed_profile_dir", normalizePath(profile_dir, winslash = "/"), .GlobalEnv)
    trace(".run_duckdb_score_query", where = asNamespace("gnafr"), print = FALSE,
      tracer = quote({
        profile_path <- file.path(.GlobalEnv$.speed_profile_dir, "current.json")
        DBI::dbExecute(con, "PRAGMA enable_profiling='json'")
        DBI::dbExecute(con, paste0("SET profiling_output = '", gsub("'", "''", profile_path), "'"))
      }), exit = quote({
        file_name <- gsub("[^a-zA-Z0-9]+", "_", label)
        if (exists("sql", inherits = FALSE)) {
          writeLines(sql, file.path(.GlobalEnv$.speed_profile_dir, paste0(file_name, ".sql")))
          if (file.exists(profile_path)) file.copy(profile_path, file.path(.GlobalEnv$.speed_profile_dir,
            paste0(file_name, ".json")), overwrite = TRUE)
        }
        DBI::dbExecute(con, "PRAGMA disable_profiling")
      }))
    on.exit({
      untrace(".run_duckdb_score_query", where = asNamespace("gnafr"))
      rm(".speed_profile_dir", envir = .GlobalEnv)
    }, add = TRUE)
  }
  timing <- system.time(result <- gnaf_match(inputs, con, cache = FALSE))
  print(timing)
  summary <- data.table::data.table(inputs = length(inputs), elapsed = unname(timing[["elapsed"]]),
    matched = data.table::uniqueN(result[matched == TRUE, input_id]))
  if (nzchar(compare_path)) {
    previous <- readRDS(compare_path)
    stopifnot(identical(inputs, previous$inputs))
    summary[, `:=`(previous_elapsed = previous$timing[["elapsed"]],
      speedup = previous$timing[["elapsed"]] / timing[["elapsed"]],
      identical_results = identical(as.list(result), as.list(previous$result)))]
  }
  print(summary)
  if (nzchar(save_path)) saveRDS(list(inputs = inputs, timing = timing, result = result,
    summary = summary, database = normalizePath(path, winslash = "/")), save_path)
  if ("identical_results" %in% names(summary) && !summary$identical_results) {
    stop("Results differ from the baseline")
  }
  invisible(summary)
}

run_match_speed_benchmark()
