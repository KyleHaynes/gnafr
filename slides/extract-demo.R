# Run from the repository root to refresh the companion script from the slides.
slide_lines <- readLines("slides/gnafr-overview.qmd", encoding = "UTF-8")
chunk_starts <- which(slide_lines == "```{r}")
demo_chunks <- lapply(chunk_starts, function(start) {
  remaining <- slide_lines[seq.int(start + 1L, length(slide_lines))]
  end <- match("```", remaining)
  if (is.na(end)) stop("Unclosed R chunk at line ", start)
  chunk <- remaining[seq_len(end - 1L)]
  label <- sub("^#\\| label: ", "", chunk[1L])
  c(paste0("# ", label, " ----"), chunk[-1L], "")
})
writeLines(c(
  "# Generated from gnafr-overview.qmd by slides/extract-demo.R.",
  "# Run one labelled section at a time during the presentation.",
  "# Set GNAF_STANDARD_DIR to the extracted G-NAF Standard folder beforehand.",
  "# To use a prepared demo database, set GNAF_DEMO_DB and skip demo-build.",
  "# Skip demo-add-sa2 if that database already contains sa2_2021.",
  "# Press Done in the Shiny app before continuing to the next section.",
  "", unlist(demo_chunks, use.names = FALSE)
), "slides/gnafr-demo.R", useBytes = TRUE)
