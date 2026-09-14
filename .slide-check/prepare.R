pkgload::load_all(".", quiet = TRUE)
library(data.table)
dir.create("slides/assets", showWarnings = FALSE)

# Read public reference rows without modifying the existing database.
reference <- gnaf_connect("C:/temp/gnaf.duckdb", read_only = TRUE)
rows <- as.data.table(DBI::dbGetQuery(reference, paste(
  "SELECT * FROM gnaf_addresses WHERE postcode IN (4012, 4870, 4183)",
  "AND street_name IN ('WALLY', 'IVO', 'WILLIAM', 'CUMMING')"
)))
gnaf_disconnect(reference)
con <- gnaf_connect(":memory:")
gnaf_init(con)
DBI::dbAppendTable(con, "gnaf_addresses", as.data.frame(rows))
gnaf_rebuild_locality_index(con)

lines <- readLines("slides/gnafr-overview.qmd", encoding = "UTF-8")
starts <- which(lines == "```{r}")
chunks <- lapply(starts, function(start) {
  end <- start + which(lines[(start + 1L):length(lines)] == "```")[1L]
  lines[(start + 1L):(end - 1L)]
})
names(chunks) <- vapply(chunks, function(x) sub("#\\| label: ", "", x[1L]), character(1L))
run_chunk <- function(name) eval(parse(text = chunks[[name]]), envir = .GlobalEnv)
for (name in c("demo-parse-one", "demo-parse-cases", "demo-match",
               "demo-alias-control", "demo-linked-return", "demo-acceptance-rule",
               "demo-custom-address", "demo-add-sa2", "demo-use-sa2")) {
  cat("Checking", name, "\n")
  run_chunk(name)
}
stopifnot(nrow(final) == length(addresses), any(!results$matched),
          all(c("sa2_code", "sa2_name") %in% names(with_sa2)),
          any(!is.na(with_sa2$sa2_code)), custom_result$source[1L] == "custom")
gnaf_threshold_filter(results, html = ".slide-check/review.html", launch.browser = FALSE)
stopifnot(inherits(gnaf_threshold_filter(results, run = FALSE), "shiny.appobj"))
saveRDS(results, ".slide-check/results.rds")
gnaf_disconnect(con)
cat("Demo checks passed; real reference subset:", nrow(rows), "rows.\n")
