# Run from the package root with GNAFR_BENCH_DB pointing to the reference DB.
# Outputs are returned for inspection; the reference database is read-only.
pkgload::load_all(".", quiet = TRUE)

audit_match_identity <- function(
    db_path = Sys.getenv("GNAFR_BENCH_DB", "C:/temp/gnafx23.duckdb"),
    input_path = "simulated_inputs.rds", n = 2000L) {
  data <- data.table::as.data.table(readRDS(input_path))
  # Deterministic spread across the same slice as the reported slow benchmark.
  indices <- unique(as.integer(seq(200000L, min(300000L, nrow(data)), length.out = n)))
  truth <- data[indices, .(input_raw = simulated_address,
    expected_pid = ADDRESS_DETAIL_PID, expected_label = ADDRESS_LABEL,
    perturbations)]
  rm(data)
  con <- gnaf_connect(db_path, read_only = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  run <- function() {
    elapsed <- system.time(result <- gnaf_match(truth$input_raw, con,
      min_score = 80L, max_results = 1L, cache = FALSE, verbose = FALSE))[["elapsed"]]
    list(result = result, elapsed = elapsed)
  }
  previous_rules <- function() {
    # Reconstruct the pre-fix marker handling and name-credit formula, holding
    # database, candidate retrieval, weights and every other rule constant.
    # Both runs use the current shared implicit-number-pair parser, including
    # building prefixes. This isolates marker/name scoring rather than
    # reconstructing an entire historical parser release.
    testthat::local_mocked_bindings(
      .repair_flat_markers = function(x, ft_map) x,
      .COMPONENT_SIM_HIGH = 0.85,
      .score_name = function(input, candidate, weight) {
        out <- integer(length(input))
        ok <- !is.na(input) & !is.na(candidate)
        out[ok] <- as.integer(round(weight * gnafr:::.component_similarity_factor(
          fast.string::jaro_winkler(input[ok], candidate[ok], p = 0.1))))
        out
      },
      .score_name_sql = function(input, candidate, weight, similarity) {
        sprintf(paste0("CASE WHEN %s IS NOT NULL AND %s IS NOT NULL ",
          "THEN CAST(ROUND_EVEN(%g * %s, 0) AS INTEGER) ELSE 0 END"),
          input, candidate, weight, gnafr:::.component_similarity_sql(similarity))
      }, .package = "gnafr"
    )
    run()
  }
  before <- previous_rules()
  after <- run()
  summarise <- function(run, version) {
    p <- run$result[truth[, .(input_id = seq_len(.N), expected_pid)], on = "input_id"]
    p[, correct := !is.na(address_detail_pid) & address_detail_pid == expected_pid]
    data.table::data.table(version, inputs = nrow(truth), seconds = run$elapsed,
      correct_pid = sum(p$correct),
      other_pid = sum(!is.na(p$address_detail_pid) & !p$correct),
      unmatched = sum(is.na(p$address_detail_pid)),
      other_pid_scored_100 = sum(!p$correct & p$total_score == 100L, na.rm = TRUE))
  }
  summary <- data.table::rbindlist(list(summarise(before, "before"), summarise(after, "after")))
  print(summary)
  review <- after$result[truth[, .(input_id = seq_len(.N), expected_pid,
    expected_label, perturbations)], on = "input_id"]
  review <- review[!is.na(address_detail_pid) & address_detail_pid != expected_pid]
  duckdb::duckdb_register(con, "__identity_review__",
    unique(review[, .(expected_pid)]))
  on.exit(duckdb::duckdb_unregister(con, "__identity_review__"), add = TRUE, after = FALSE)
  reference <- data.table::as.data.table(DBI::dbGetQuery(con, paste(
    "SELECT g.address_detail_pid AS expected_pid,",
    "g.address_label AS reference_label, g.principal_pid AS reference_principal",
    "FROM gnaf_addresses g JOIN __identity_review__ r",
    "ON g.address_detail_pid = r.expected_pid"
  )))
  review <- reference[review, on = "expected_pid"]
  print(review[total_score == 100L, .(
    count = .N,
    same_reference_label = sum(reference_label == address_label, na.rm = TRUE),
    linked_principal = sum(reference_principal == address_detail_pid, na.rm = TRUE)
  )])
  print(review[total_score == 100L & (is.na(reference_label) | reference_label != address_label),
    .(input_raw, reference_label, address_label)][1:min(.N, 15L)])
  # PID equality is deliberately strict: aliases and multiple reference PIDs
  # for a dwelling need separate adjudication before calling them errors.
  invisible(list(summary = summary, truth = truth, before = before, after = after,
    review = review))
}

audit <- audit_match_identity()
