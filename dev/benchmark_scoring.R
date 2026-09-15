# Compare component scores and runtime with a committed baseline, without a
# database download. Run from the repository root; optionally set
# GNAFR_SCORE_BASELINE to a Git revision and GNAFR_SCORE_N to the pair count.
devtools::load_all(".", quiet = TRUE)

benchmark_scoring <- function() {
  revision <- Sys.getenv("GNAFR_SCORE_BASELINE", "HEAD")
  n <- suppressWarnings(as.integer(Sys.getenv("GNAFR_SCORE_N", "20000")))
  if (is.na(n) || n < 1L) stop("GNAFR_SCORE_N must be a positive integer")
  previous <- new.env(parent = asNamespace("gnafr"))
  for (path in c("R/score_components.R", "R/score.R")) {
    source <- system2("git", c("show", shQuote(paste0(revision, ":", path))), stdout = TRUE)
    if (!is.null(attr(source, "status"))) stop("Cannot read baseline ", revision)
    eval(parse(text = source), envir = previous)
  }
  pairs <- data.table::data.table(
    scenario = c("Leading typo: correct name", "Leading typo: shared-prefix decoy",
      "Transposed name", "Opposing direction", "Postcode transposition",
      "Street type abbreviation", "Zero-padded lot", "Zero-padded unit", "Unrelated name"),
    in_street_name = c("XILLIAM", "XILLIAM", "BIRSTOL", "MOUNT GRAVATT EAST", rep("MAIN", 4), "CERIUM"),
    street_name = c("WILLIAM", "XILLIAMSON", "BRISTOL", "MOUNT GRAVATT WEST", rep("MAIN", 4), "TUCKEROO"),
    in_street_type = "ROAD", street_type = "ROAD",
    in_postcode = 4067L, postcode = 4067L,
    in_locality = "BRISBANE", locality_name = "BRISBANE",
    in_number_first = 10L, number_first = 10L,
    in_number_last = NA_integer_, number_last = NA_integer_,
    in_number_suffix = NA_character_, address_label = NA_character_,
    in_street_suffix = NA_character_, street_suffix = NA_character_,
    in_lot_number = NA_character_, lot_number = NA_character_,
    in_flat_number = NA_character_, flat_number = NA_character_,
    in_flat_type = NA_character_, flat_type = NA_character_,
    in_level_number = NA_character_, level_number = NA_character_,
    in_level_type = NA_character_, level_type = NA_character_)
  pairs[5L, postcode := 4076L]
  pairs[6L, in_street_type := "RD"]
  pairs[7L, `:=`(in_lot_number = "007", lot_number = "7")]
  pairs[8L, `:=`(in_flat_number = "003", flat_number = "3")]
  old <- previous$.score_pairs(data.table::copy(pairs))
  new <- .score_pairs(data.table::copy(pairs))
  print(data.table::data.table(scenario = pairs$scenario,
    old_total = old$total_score, new_total = new$total_score,
    old_name = old$score_street_name, new_name = new$score_street_name))

  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  bulk <- pairs[rep(seq_len(nrow(pairs)), length.out = n)]
  duckdb::duckdb_register(con, "benchmark_pairs", bulk)
  timings <- data.table::rbindlist(lapply(c("previous", "current"), function(version) {
    engine <- if (version == "previous") previous else asNamespace("gnafr")
    expressions <- engine$.score_sql_exprs(.default_match_weights(), i = "p", g = "p")
    sql <- paste("SELECT", paste(sprintf("%s AS %s", expressions, names(expressions)),
                                  collapse = ", "), "FROM benchmark_pairs p")
    # Warm each implementation before measuring three runs.
    engine$.score_pairs(data.table::copy(bulk))
    DBI::dbGetQuery(con, sql)
    r_time <- replicate(3L, system.time(engine$.score_pairs(data.table::copy(bulk)))[["elapsed"]])
    sql_time <- replicate(3L, system.time(DBI::dbGetQuery(con, sql))[["elapsed"]])
    data.table::data.table(version, pairs = n,
      r_seconds = median(r_time), sql_seconds = median(sql_time))
  }))
  print(timings)
  invisible(list(examples = new, timings = timings))
}

benchmark_scoring()
