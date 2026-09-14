browser <- chromote::ChromoteSession$new(width = 1400, height = 950)
on.exit(browser$close(), add = TRUE)
browser$Page$navigate(paste0("file:///", normalizePath(".slide-check/review.html", winslash = "/")))
Sys.sleep(1)
browser$screenshot("slides/assets/threshold-static.png")

root <- normalizePath(".", winslash = "/")
app <- callr::r_bg(function(root) {
  setwd(root)
  pkgload::load_all(".", quiet = TRUE)
  x <- readRDS(".slide-check/results.rds")
  shiny::runApp(gnaf_threshold_filter(x, name = "results", max_rows = 30L,
                                    run = FALSE),
                host = "127.0.0.1", port = 7453L, launch.browser = FALSE)
}, args = list(root), stdout = ".slide-check/shiny.log", stderr = ".slide-check/shiny-error.log")
on.exit(app$kill(), add = TRUE)
for (attempt in seq_len(40L)) {
  Sys.sleep(0.25)
  log <- readLines(".slide-check/shiny-error.log", warn = FALSE)
  if (any(grepl("Listening on", log, fixed = TRUE))) break
}
browser$Page$navigate("http://127.0.0.1:7453")
Sys.sleep(1)
for (attempt in seq_len(60L)) {
  ready <- browser$Runtime$evaluate("document.querySelectorAll('.rt-tbody .rt-tr').length > 0 && !document.querySelector('.shiny-busy')")$result$value
  if (isTRUE(ready)) break
  Sys.sleep(0.25)
}
browser$screenshot("slides/assets/threshold-shiny.png")
cat("Captured both review modes.\n")
app$kill()
browser$close()
