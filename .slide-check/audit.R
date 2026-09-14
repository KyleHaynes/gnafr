browser <- chromote::ChromoteSession$new(width = 1440, height = 900)
browser$Page$navigate(paste0("file:///", normalizePath("slides/gnafr-overview.html", winslash = "/")))
for (attempt in seq_len(80L)) {
  ready <- browser$Runtime$evaluate("Boolean(window.Reveal && Reveal.isReady() && document.querySelectorAll('svg[id^=\"mermaid-figure-\"]').length === 5)")$result$value
  if (isTRUE(ready)) break
  Sys.sleep(0.25)
}
cat("Diagrams ready:", ready, "\n")
browser$Runtime$evaluate("Reveal.configure({transition: 'none'}); Reveal.layout();")
total <- browser$Runtime$evaluate("Reveal.getTotalSlides()")$result$value
reports <- vector("list", total)
for (index in seq_len(total)) {
  browser$Runtime$evaluate(sprintf("Reveal.slide(%d)", index - 1L))
  Sys.sleep(0.4)
  js <- "(() => {
    const s = Reveal.getCurrentSlide(), b = s.getBoundingClientRect();
    const items = [...s.querySelectorAll('h2,p,table,pre,svg[id^=mermaid-figure-],figure,.callout,ul')]
      .filter(x => !x.closest('aside.notes') && !x.closest('foreignObject') && getComputedStyle(x).display !== 'none');
    const outside = items.filter(x => {const r = x.getBoundingClientRect(); return r.bottom > b.bottom + 3 || r.right > b.right + 3 || r.left < b.left - 3;})
      .map(x => ({tag:x.tagName, text:x.textContent.slice(0,65), bottom:Math.round(x.getBoundingClientRect().bottom-b.top)}));
    return {title:(s.querySelector('h2,h1')||s).textContent.slice(0,100),height:Math.round(b.height),outside,notes:s.querySelectorAll('aside.notes li').length};
  })()"
  reports[[index]] <- browser$Runtime$evaluate(js, returnByValue = TRUE)$result$value
  if (index %in% c(1L, 2L, 4L, 5L, 6L, 7L, 9L, 11L, 14L, 18L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L)) {
    shot <- browser$Page$captureScreenshot(format = "png", captureBeyondViewport = FALSE)
    writeBin(base64enc::base64decode(shot$data), sprintf(".slide-check/slide-%02d.png", index))
  }
}
jsonlite::write_json(reports, ".slide-check/layout.json", auto_unbox = TRUE, pretty = TRUE)
cat("Slides:", total, "\n")
cat("Missing notes:", sum(vapply(reports, function(x) x$notes == 0L, logical(1L))), "\n")
for (r in reports) if (length(r$outside)) cat(jsonlite::toJSON(r, auto_unbox = TRUE), "\n")
browser$close()
