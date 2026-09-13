#' Interactively threshold `gnaf_match()` results and get the filter as code
#'
#' Opens a minimal Shiny app over an existing [gnaf_match()] result. Set a
#' minimum/maximum on every score, watch rows move between the in-scope and
#' out-of-scope tables (each with the input/matched diff), flag individual
#' inputs as false positives, and copy the equivalent `data.table` or `dplyr`
#' filter. Pressing "Done" (or closing the window) prints both snippets to the
#' console, ready to paste into a script, and returns them invisibly.
#'
#' Thresholds cover `total_score`, every `score_*` component in `x`, and the
#' full-string text similarities from [gnaf_text_scores()] (Jaro-Winkler,
#' Jaccard, Levenshtein and their combined text score). Because the text
#' columns are not part of `x`, the generated code wraps `x` in
#' `gnaf_text_scores()` whenever one of those thresholds is active, so the
#' snippet runs as printed.
#'
#' Flagging a row excludes its whole `input_id` (every rank for that input),
#' which is what a false positive usually means in practice.
#'
#' \strong{Large results} (hundreds of thousands of rows): the app window opens
#' immediately and shows a progress bar while [gnaf_text_scores()] computes -
#' this is the one step whose cost scales with `nrow(x)` and previously ran
#' silently before the window appeared. Set `text_scores = FALSE` to skip it
#' entirely if you only need the component scores. Threshold sliders are
#' debounced, so dragging one only recomputes once you pause. The in-scope and
#' out-of-scope tables render an input/matched diff for up to `max_rows` rows
#' each; that diff is the slowest part of every redraw, so a lower `max_rows`
#' (the default) keeps every interaction responsive regardless of how large
#' `x` is - counts and the generated filter always describe every row
#' regardless of `max_rows`.
#'
#' @param x A `data.table` returned by [gnaf_match()].
#' @param name Object name used in the generated code. Defaults to the
#'   expression passed as `x`, and can be edited inside the app.
#' @param max_rows Maximum rows rendered in each table, and the dominant cost
#'   of every redraw (each row renders an input/matched diff). Default `200L`;
#'   raise it if you want to browse more rows at once and don't mind slower
#'   redraws, lower it for very large `x` on a slow machine.
#' @param text_scores If `TRUE` (default), compute [gnaf_text_scores()] so
#'   Jaro-Winkler/Jaccard/Levenshtein thresholds are available. This is the
#'   one setup cost that scales with `nrow(x)`; set `FALSE` to skip it for very
#'   large results when only the component scores are needed.
#' @param plot_sample Maximum rows used to draw the score-distribution
#'   histograms. Default `50000L`; sampled fresh each time thresholds change,
#'   since the shape is unaffected by sampling at that size and it keeps the
#'   plot responsive for large `x`.
#' @param launch.browser Passed to [shiny::runApp()] when `run = TRUE`.
#' @param run If `TRUE` (default), launches the app, prints the resulting code
#'   when it closes and returns it invisibly. If `FALSE`, returns the
#'   [shiny::shinyApp()] object.
#' @return Invisibly, an object of class `gnaf_threshold_filter`: a list with
#'   `code` (`$data.table` and `$dplyr`, each holding `in_scope` and
#'   `out_of_scope` snippets), `conditions`, `thresholds`, `matched_only`,
#'   `top_rank_only`, `flagged_input_ids`, `n_in_scope` and `n_out_of_scope`.
#'   Printing it shows the counts and both snippets again.
#' @examples
#' \dontrun{
#' results <- gnaf_match(addresses, con)
#' gnaf_threshold_filter(results)
#' # ... adjust, press Done; the console then shows e.g.
#' # results[matched == TRUE & total_score >= 80 & score_street_name >= 30]
#'
#' # 500k+ rows, component scores only:
#' gnaf_threshold_filter(results, text_scores = FALSE)
#' }
#' @export
gnaf_threshold_filter <- function(x, name = deparse(substitute(x)),
                                  max_rows = 200L, text_scores = TRUE,
                                  plot_sample = 50000L,
                                  launch.browser = interactive(), run = TRUE) {
  .gnaf_require_app_packages()
  name <- paste(name, collapse = "")

  if (!is.data.table(x)) stop("'x' must be a data.table returned by gnaf_match()")
  required <- c("input_id", "input_raw", "input_standardised", "match_rank",
                "matched", "address_label", "total_score")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0L) {
    stop("'x' is missing columns required by the app: ",
         paste(missing_cols, collapse = ", "))
  }
  max_rows <- .as_positive_integer(max_rows, "max_rows")
  plot_sample <- .as_positive_integer(plot_sample, "plot_sample")
  if (!is.logical(text_scores) || length(text_scores) != 1L || is.na(text_scores))
    stop("'text_scores' must be TRUE or FALSE", call. = FALSE)

  # Cheap, structural setup only: no gnaf_text_scores() call here, so the app
  # window can open before that per-row cost is paid. Text-score maxes are
  # always 100 by construction, so the sliders don't need the real columns.
  component_vars <- setdiff(.gnaf_threshold_vars(x), .GNAF_TEXT_VARS)
  text_vars <- if (isTRUE(text_scores)) .GNAF_TEXT_VARS else character()
  vars <- c(component_vars, text_vars)
  maxes <- .gnaf_threshold_maxes(x, component_vars)
  if (length(text_vars) > 0L) maxes[text_vars] <- 100L
  # If x already carries the text columns, the snippet need not recompute them.
  wrap_text <- isTRUE(text_scores) && !all(.GNAF_TEXT_VARS %in% names(x))
  original_cols <- names(x)
  extra_choices <- setdiff(
    original_cols,
    c("input_id", "match_rank", "matched", "input_raw", "input_standardised",
      "address_label", vars)
  )

  slider <- function(var) {
    shiny::sliderInput(
      paste0("thr_", var),
      sprintf("%s (0-%d)", .GNAF_THRESHOLD_LABELS[[var]], maxes[[var]]),
      min = 0L, max = maxes[[var]], value = c(0L, maxes[[var]]), step = 1L
    )
  }

  ui <- bslib::page_sidebar(
    title = "gnafr threshold filter",
    window_title = "gnafr threshold filter",
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      width = 340,
      open = "desktop",
      shiny::textInput("obj_name", "Object name in generated code", value = name),
      shiny::checkboxInput("matched_only", "Matched rows only", value = TRUE),
      shiny::checkboxInput("top_rank_only", "Top-ranked match only", value = FALSE),
      shiny::actionButton("reset", "Reset thresholds", class = "btn-outline-secondary btn-sm"),
      bslib::accordion(
        id = "threshold-groups",
        open = if (length(text_vars) > 0L) c("components", "text") else "components",
        bslib::accordion_panel(
          "Component scores",
          value = "components",
          icon = shiny::icon("scale-balanced"),
          lapply(component_vars, slider)
        ),
        if (length(text_vars) > 0L) bslib::accordion_panel(
          "Text similarity",
          value = "text",
          icon = shiny::icon("text-width"),
          shiny::div(class = "help-text", "Whole-string comparison of the input with the matched label (see gnaf_text_scores())."),
          lapply(text_vars, slider)
        ),
        bslib::accordion_panel(
          "Compare",
          value = "compare",
          icon = shiny::icon("code-compare"),
          shiny::radioButtons(
            "diff_pair", "Diff columns",
            choices = c(
              "Input vs Standardised" = "raw_std",
              "Input vs Matched address" = "raw_match",
              "Standardised vs Matched address" = "std_match"
            ),
            selected = "std_match"
          ),
          shiny::radioButtons(
            "diff_granularity", "Diff level",
            choices = c("Words" = "diff_words", "Characters" = "diff_chars"),
            selected = "diff_chars", inline = TRUE
          ),
          shiny::selectizeInput(
            "extra_cols", "Additional columns",
            choices = stats::setNames(extra_choices, vapply(extra_choices, .gnaf_pretty_colname, character(1))),
            multiple = TRUE,
            options = list(placeholder = "None")
          )
        )
      ),
      shiny::actionButton("done", "Done - print filter code", class = "btn-success", width = "100%")
    ),
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(
        "function gnafrCopyText(id) {\n",
        "  var el = document.getElementById(id);\n",
        "  if (!el) return false;\n",
        "  var text = el.innerText || el.textContent;\n",
        "  function legacyCopy() {\n",
        "    var ta = document.createElement('textarea');\n",
        "    ta.value = text;\n",
        "    ta.setAttribute('readonly', '');\n",
        "    ta.style.position = 'fixed';\n",
        "    ta.style.top = '0';\n",
        "    ta.style.left = '0';\n",
        "    ta.style.opacity = '0';\n",
        "    document.body.appendChild(ta);\n",
        "    ta.focus();\n",
        "    ta.select();\n",
        "    var ok = false;\n",
        "    try { ok = document.execCommand('copy'); } catch (e) {}\n",
        "    document.body.removeChild(ta);\n",
        "    return ok;\n",
        "  }\n",
        "  if (navigator.clipboard && navigator.clipboard.writeText) {\n",
        "    navigator.clipboard.writeText(text).catch(legacyCopy);\n",
        "  } else {\n",
        "    legacyCopy();\n",
        "  }\n",
        "}\n"
      )),
      shiny::tags$style(shiny::HTML(jsdiffr::diff_css_default())),
      shiny::tags$style(shiny::HTML(
        ".panel {background: #f8fbff; border: 1px solid #d9e2ec; border-radius: 14px; padding: 16px 18px; margin-bottom: 18px;}\n",
        ".panel-head {display: flex; align-items: center; justify-content: space-between; gap: 12px; margin-bottom: 10px;}\n",
        ".panel-title {font-size: 15px; font-weight: 700; color: #102a43; margin: 0;}\n",
        ".metric-grid {display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; margin: 0 0 18px;}\n",
        ".metric-card {background: linear-gradient(135deg, #102a43, #1f5f8b); color: #fff; border-radius: 14px; padding: 14px 16px;}\n",
        ".metric-card--out {background: linear-gradient(135deg, #7f1d1d, #b45309);}\n",
        ".metric-card--flag {background: linear-gradient(135deg, #3f3f46, #71717a);}\n",
        ".metric-label {font-size: 12px; text-transform: uppercase; letter-spacing: 0.08em; opacity: 0.8;}\n",
        ".metric-value {font-size: 26px; font-weight: 700; line-height: 1.2; margin-top: 6px;}\n",
        ".help-text {color: #52606d; font-size: 13px;}\n",
        ".reactable {font-size: 12px;}\n",
        ".reactable .rt-th, .reactable .rt-td {padding: 6px 8px;}\n",
        ".reactable .rt-tr-group {min-height:auto;}\n",
        ".details-shell {max-width: 900px; padding: 8px 10px; background: #f8fbff; border-radius: 10px;}\n",
        ".details-title {font-size: 12px; font-weight: 700; margin-bottom: 8px; color: #102a43;}\n",
        ".diff-cell {padding: 0px 1px;}\n",
        ".diff-cell .jsdiff-pre {white-space: pre-wrap; font-size: 12px;}\n",
        ".code-head {display: flex; justify-content: space-between; align-items: center; margin: 8px 0 4px;}\n",
        "@media (max-width: 900px) {.metric-grid {grid-template-columns: 1fr;}}"
      ))
    ),
    shiny::uiOutput("metrics"),
    shiny::div(
      class = "panel",
      shiny::div(
        class = "panel-head",
        shiny::h3(class = "panel-title", shiny::textOutput("in_title", inline = TRUE)),
        shiny::actionButton("flag_selected", "Flag selected as false positive", class = "btn-outline-danger btn-sm")
      ),
      reactable::reactableOutput("in_table")
    ),
    shiny::div(
      class = "panel",
      shiny::div(
        class = "panel-head",
        shiny::h3(class = "panel-title", shiny::textOutput("out_title", inline = TRUE)),
        shiny::div(
          shiny::actionButton("unflag_selected", "Unflag selected", class = "btn-outline-secondary btn-sm"),
          shiny::actionButton("clear_flags", "Clear all flags", class = "btn-outline-secondary btn-sm")
        )
      ),
      reactable::reactableOutput("out_table")
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Score distributions",
        icon = shiny::icon("chart-column"),
        shiny::div(class = "help-text", "Histograms of every score split by scope; dashed lines mark active thresholds."),
        shiny::plotOutput("score_plot", height = "520px")
      )
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Apply this filter in R",
        icon = shiny::icon("code"),
        shiny::div(class = "help-text", "Updates as you move the sliders. Unmatched rows have NA scores and drop out of the in-scope filter automatically. Pressing Done prints these to the console."),
        shiny::div(
          class = "code-head",
          shiny::tags$strong("data.table"),
          .gnaf_copy_button("code_dt")
        ),
        shiny::verbatimTextOutput("code_dt"),
        shiny::div(
          class = "code-head",
          shiny::tags$strong("dplyr"),
          .gnaf_copy_button("code_dplyr")
        ),
        shiny::verbatimTextOutput("code_dplyr")
      )
    )
  )

  server <- function(input, output, session) {
    flagged <- shiny::reactiveVal(integer())
    done <- FALSE

    # The one setup cost that scales with nrow(x). Runs after the window is
    # already open (unlike computing it before shiny::shinyApp(), which left
    # the console - and the browser - blank for the whole duration on large x).
    data <- shiny::reactiveVal(NULL)
    shiny::observe({
      shiny::withProgress(
        message = sprintf("Preparing %s row%s", format(nrow(x), big.mark = ","),
                          if (nrow(x) == 1L) "" else "s"),
        value = 0.2, {
          if (isTRUE(text_scores)) shiny::incProgress(0.1, detail = "Computing text similarity scores")
          prepared <- .gnaf_threshold_prepare(x, text_scores = text_scores)
          shiny::incProgress(0.7, detail = "Ready")
          data(prepared)
        }
      )
    })

    thresholds_raw <- shiny::reactive({
      stats::setNames(lapply(vars, function(var) {
        value <- input[[paste0("thr_", var)]]
        if (is.null(value)) c(0L, maxes[[var]]) else as.integer(round(value))
      }), vars)
    })
    # Debounced so dragging a slider recomputes scope/tables/plot once you
    # pause, not on every intermediate tick - the dominant per-interaction
    # cost is the diff column in the two tables below.
    thresholds <- shiny::debounce(thresholds_raw, .GNAF_THRESHOLD_DEBOUNCE_MS)

    obj_name <- shiny::reactive({
      value <- trimws(input$obj_name %||% "")
      if (nzchar(value)) value else "x"
    })

    scope <- shiny::reactive({
      shiny::req(data())
      .gnaf_threshold_scope(
        data(), thresholds(), maxes,
        matched_only = isTRUE(input$matched_only),
        top_rank_only = isTRUE(input$top_rank_only),
        flagged = flagged()
      )
    })

    conditions <- shiny::reactive({
      .gnaf_threshold_conditions(
        thresholds(), maxes,
        matched_only = isTRUE(input$matched_only),
        top_rank_only = isTRUE(input$top_rank_only),
        flagged = flagged()
      )
    })

    code <- shiny::reactive({
      .gnaf_threshold_code(
        obj_name(), conditions(),
        wrap_text = wrap_text && .gnaf_threshold_uses_text(thresholds(), maxes)
      )
    })

    in_data <- shiny::reactive({
      out <- data()[scope()]
      setorder(out, total_score, na.last = TRUE)
      out
    })
    out_data <- shiny::reactive({
      out <- data()[!scope()]
      out[, flagged := input_id %in% flagged()]
      setorder(out, -total_score, na.last = TRUE)
      out
    })

    result <- shiny::reactive({
      shiny::req(data())
      keep <- scope()
      structure(
        list(
          code = code(),
          conditions = conditions(),
          thresholds = thresholds(),
          matched_only = isTRUE(input$matched_only),
          top_rank_only = isTRUE(input$top_rank_only),
          flagged_input_ids = flagged(),
          n_in_scope = sum(keep),
          n_out_of_scope = sum(!keep)
        ),
        class = "gnaf_threshold_filter"
      )
    })

    shiny::observeEvent(input$reset, {
      for (var in vars) {
        shiny::updateSliderInput(session, paste0("thr_", var), value = c(0L, maxes[[var]]))
      }
    })

    shiny::observeEvent(input$flag_selected, {
      selected <- reactable::getReactableState("in_table", "selected")
      if (length(selected) == 0L) {
        shiny::showNotification("Select rows in the in-scope table first.", type = "warning")
        return(invisible(NULL))
      }
      shown <- utils::head(in_data(), max_rows)
      flagged(sort(union(flagged(), shown$input_id[selected])))
    })

    shiny::observeEvent(input$unflag_selected, {
      selected <- reactable::getReactableState("out_table", "selected")
      if (length(selected) == 0L) {
        shiny::showNotification("Select flagged rows in the out-of-scope table first.", type = "warning")
        return(invisible(NULL))
      }
      shown <- utils::head(out_data(), max_rows)
      flagged(setdiff(flagged(), shown$input_id[selected]))
    })

    shiny::observeEvent(input$clear_flags, flagged(integer()))

    shiny::observeEvent(input$done, {
      shiny::req(data())
      done <<- TRUE
      shiny::stopApp(result())
    })
    session$onSessionEnded(function() {
      if (!done) {
        res <- tryCatch(shiny::isolate(result()), error = function(e) NULL)
        shiny::stopApp(res)
      }
    })

    output$metrics <- shiny::renderUI({
      keep <- scope()
      shiny::div(
        class = "metric-grid",
        .gnaf_metric_card("In scope", format(sum(keep), big.mark = ",")),
        shiny::div(
          class = "metric-card metric-card--out",
          shiny::div(class = "metric-label", "Out of scope"),
          shiny::div(class = "metric-value", format(sum(!keep), big.mark = ","))
        ),
        shiny::div(
          class = "metric-card metric-card--flag",
          shiny::div(class = "metric-label", "Flagged inputs"),
          shiny::div(class = "metric-value", format(length(flagged()), big.mark = ","))
        )
      )
    })

    output$in_title <- shiny::renderText(
      .gnaf_threshold_title("In scope", nrow(in_data()), max_rows)
    )
    output$out_title <- shiny::renderText(
      .gnaf_threshold_title("Out of scope", nrow(out_data()), max_rows)
    )

    method_fn <- shiny::reactive({
      if (identical(input$diff_granularity, "diff_words")) jsdiffr::diff_words else jsdiffr::diff_chars
    })

    output$in_table <- reactable::renderReactable({
      .gnaf_threshold_table(
        utils::head(in_data(), max_rows),
        pair = input$diff_pair %||% "std_match", method_fn = method_fn(),
        extra_cols = intersect(input$extra_cols %||% character(0), original_cols),
        vars = vars, maxes = maxes, show_flagged = FALSE
      )
    })

    output$out_table <- reactable::renderReactable({
      .gnaf_threshold_table(
        utils::head(out_data(), max_rows),
        pair = input$diff_pair %||% "std_match", method_fn = method_fn(),
        extra_cols = intersect(input$extra_cols %||% character(0), original_cols),
        vars = vars, maxes = maxes, show_flagged = TRUE
      )
    })

    output$code_dt <- shiny::renderText(.gnaf_threshold_code_text(code()$data.table))
    output$code_dplyr <- shiny::renderText(.gnaf_threshold_code_text(code()$dplyr))

    output$score_plot <- shiny::renderPlot({
      shiny::req(data())
      .gnaf_threshold_plot(data(), vars, scope(), thresholds(), maxes, sample_n = plot_sample)
    })
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  if (!isTRUE(run)) {
    return(app)
  }
  result <- shiny::runApp(app, launch.browser = launch.browser)
  if (inherits(result, "gnaf_threshold_filter")) print(result)
  invisible(result)
}

#' @export
#' @noRd
print.gnaf_threshold_filter <- function(x, ...) {
  cat(sprintf(
    "gnaf_threshold_filter: %s rows in scope, %s out of scope, %s flagged input(s)\n",
    format(x$n_in_scope, big.mark = ","),
    format(x$n_out_of_scope, big.mark = ","),
    format(length(x$flagged_input_ids), big.mark = ",")
  ))
  if (length(x$conditions) == 0L) {
    cat("No conditions set: every row is in scope.\n")
  }
  cat("\n## data.table\n")
  cat(.gnaf_threshold_code_text(x$code$data.table), "\n")
  cat("\n## dplyr\n")
  cat(.gnaf_threshold_code_text(x$code$dplyr), "\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Non-reactive helpers (unit-tested directly)
# ---------------------------------------------------------------------------

.GNAF_TEXT_VARS <- c("text_similarity", "jarowinkler_score", "jaccard_score", "levenshtein_score")

# Milliseconds a threshold slider must be still before scope/tables/plot
# recompute. The dominant per-interaction cost is the diff column rendered in
# up to 2 * max_rows rows, so this avoids paying it on every drag tick.
.GNAF_THRESHOLD_DEBOUNCE_MS <- 300

.GNAF_THRESHOLD_LABELS <- c(
  total_score = "Total score",
  score_postcode = "Postcode",
  score_suburb = "Suburb",
  score_street_name = "Street name",
  score_street_type = "Street type",
  score_number = "Number",
  score_flat = "Flat / level",
  text_similarity = "Text score (JW + Jaccard)",
  jarowinkler_score = "Jaro-Winkler",
  jaccard_score = "Jaccard (bigrams)",
  levenshtein_score = "Levenshtein"
)

.gnaf_threshold_vars <- function(x) {
  intersect(names(.GNAF_THRESHOLD_LABELS), names(x))
}

# Slider upper bound: 100 for totals and text scores, otherwise the component's
# default weight, or higher if custom weights produced larger observed scores.
.gnaf_threshold_maxes <- function(x, vars) {
  stats::setNames(lapply(vars, function(var) {
    known <- if (var %in% c("total_score", .GNAF_TEXT_VARS)) 100L else .WEIGHTS[[sub("^score_", "", var)]]
    observed <- suppressWarnings(max(x[[var]], na.rm = TRUE))
    if (!is.finite(observed)) observed <- 0L
    as.integer(max(known, ceiling(observed)))
  }), vars)
}

.gnaf_threshold_prepare <- function(x, text_scores = TRUE) {
  if (!isTRUE(text_scores)) return(copy(x))
  if (nrow(x) > 0L) return(gnaf_text_scores(x))
  out <- copy(x)
  out[, (.GNAF_TEXT_VARS) := lapply(.GNAF_TEXT_VARS, function(v) numeric())]
  out
}

# Logical scope vector. Mirrors data.table's `[` semantics so the rows equal
# what the generated code produces: NA scores (unmatched rows) never pass.
.gnaf_threshold_scope <- function(x, thresholds, maxes, matched_only = TRUE,
                                  top_rank_only = FALSE, flagged = integer()) {
  keep <- rep(TRUE, nrow(x))
  if (isTRUE(matched_only)) keep <- keep & x$matched %in% TRUE
  if (isTRUE(top_rank_only)) keep <- keep & x$match_rank %in% 1L
  for (var in names(thresholds)) {
    bounds <- thresholds[[var]]
    value <- x[[var]]
    if (bounds[1L] > 0L) keep <- keep & !is.na(value) & value >= bounds[1L]
    if (bounds[2L] < maxes[[var]]) keep <- keep & !is.na(value) & value <= bounds[2L]
  }
  if (length(flagged) > 0L) keep <- keep & !(x$input_id %in% flagged)
  keep
}

# Only bounds that actually restrict something appear, so the generated filter
# stays as short as the user's intent.
.gnaf_threshold_conditions <- function(thresholds, maxes, matched_only = TRUE,
                                       top_rank_only = FALSE, flagged = integer()) {
  conds <- character()
  if (isTRUE(matched_only)) conds <- c(conds, "matched == TRUE")
  if (isTRUE(top_rank_only)) conds <- c(conds, "match_rank == 1")
  for (var in names(thresholds)) {
    bounds <- thresholds[[var]]
    if (bounds[1L] > 0L) conds <- c(conds, paste0(var, " >= ", bounds[1L]))
    if (bounds[2L] < maxes[[var]]) conds <- c(conds, paste0(var, " <= ", bounds[2L]))
  }
  if (length(flagged) > 0L) {
    conds <- c(conds, paste0("!input_id %in% c(", paste(flagged, collapse = ", "), ")"))
  }
  conds
}

.gnaf_threshold_uses_text <- function(thresholds, maxes) {
  any(vapply(intersect(names(thresholds), .GNAF_TEXT_VARS), function(var) {
    bounds <- thresholds[[var]]
    bounds[1L] > 0L || bounds[2L] < maxes[[var]]
  }, logical(1)))
}

# The out-of-scope snippet negates via `%in% TRUE` so rows with NA scores
# (unmatched inputs) land out of scope instead of vanishing from both sets.
# With wrap_text the source becomes gnaf_text_scores(name) so text-score
# conditions can run against a plain gnaf_match() result.
.gnaf_threshold_code <- function(name, conds, wrap_text = FALSE) {
  dt_source <- if (wrap_text) paste0("gnaf_text_scores(", name, ")") else name
  dplyr_source <- if (wrap_text) paste0(name, " |>\n  gnaf_text_scores()") else name
  if (length(conds) == 0L) {
    return(list(
      data.table = c(in_scope = dt_source, out_of_scope = paste0(dt_source, "[0L]")),
      dplyr = c(in_scope = dplyr_source, out_of_scope = paste0(dplyr_source, " |>\n  dplyr::filter(FALSE)"))
    ))
  }
  expr <- paste(conds, collapse = " & ")
  list(
    data.table = c(
      in_scope = paste0(dt_source, "[", expr, "]"),
      out_of_scope = paste0(dt_source, "[!((", expr, ") %in% TRUE)]")
    ),
    dplyr = c(
      in_scope = paste0(dplyr_source, " |>\n  dplyr::filter(", paste(conds, collapse = ", "), ")"),
      out_of_scope = paste0(dplyr_source, " |>\n  dplyr::filter(!((", expr, ") %in% TRUE))")
    )
  )
}

.gnaf_threshold_code_text <- function(snippets) {
  paste0(
    "# in scope\n", snippets[["in_scope"]], "\n\n",
    "# out of scope\n", snippets[["out_of_scope"]]
  )
}

.gnaf_threshold_title <- function(label, n, max_rows) {
  if (n > max_rows) {
    sprintf("%s (%s rows, showing first %s)", label, format(n, big.mark = ","), format(max_rows, big.mark = ","))
  } else {
    sprintf("%s (%s rows)", label, format(n, big.mark = ","))
  }
}

.gnaf_copy_button <- function(target_id) {
  shiny::tags$button(
    type = "button",
    class = "btn btn-outline-secondary btn-sm",
    onclick = sprintf("gnafrCopyText('%s')", target_id),
    shiny::icon("copy"), " Copy"
  )
}

.gnaf_threshold_table <- function(table_data, pair, method_fn, extra_cols, vars,
                                  maxes, show_flagged = FALSE) {
  pair_cols <- .gnaf_diff_pair_columns(pair)
  extra_cols <- setdiff(extra_cols, c(pair_cols$left, pair_cols$right))
  component_vars <- setdiff(vars, .GNAF_TEXT_VARS)
  text_vars <- intersect(vars, .GNAF_TEXT_VARS)
  shown <- c(
    "input_id", "match_rank", pair_cols$left, pair_cols$right,
    extra_cols, component_vars, text_vars,
    if (show_flagged) "flagged"
  )
  shown <- unique(intersect(shown, names(table_data)))
  display <- table_data[, shown, with = FALSE]
  display[, diff := ""]
  setcolorder(display, c("input_id", "match_rank", pair_cols$left, pair_cols$right, "diff"))

  diff_cell <- function(value, index) {
    row <- table_data[index, ]
    .gnaf_diff_cell(row[[pair_cols$left]], row[[pair_cols$right]], method_fn)
  }

  score_defs <- stats::setNames(
    lapply(vars, function(var) {
      .gnaf_score_col(
        .GNAF_THRESHOLD_LABELS[[var]],
        digits = if (var %in% .GNAF_TEXT_VARS) 1 else 0,
        max = maxes[[var]]
      )
    }),
    vars
  )
  extra_defs <- stats::setNames(
    lapply(extra_cols, function(col) reactable::colDef(name = .gnaf_pretty_colname(col), minWidth = 120)),
    extra_cols
  )
  pair_defs <- stats::setNames(
    list(
      reactable::colDef(name = pair_cols$left_label, minWidth = 200),
      reactable::colDef(name = pair_cols$right_label, minWidth = 200)
    ),
    c(pair_cols$left, pair_cols$right)
  )

  column_groups <- list()
  if (length(component_vars) > 0L) {
    column_groups <- c(column_groups, list(reactable::colGroup(name = "Component scores", columns = component_vars)))
  }
  if (length(text_vars) > 0L) {
    column_groups <- c(column_groups, list(reactable::colGroup(name = "Text similarity", columns = text_vars)))
  }

  reactable::reactable(
    display,
    defaultPageSize = 10,
    searchable = TRUE,
    filterable = TRUE,
    highlight = TRUE,
    striped = TRUE,
    resizable = TRUE,
    selection = "multiple",
    onClick = "select",
    columns = c(
      list(
        input_id = reactable::colDef(name = "Input", maxWidth = 80),
        match_rank = reactable::colDef(name = "Rank", maxWidth = 70),
        diff = reactable::colDef(name = "Diff", cell = diff_cell, html = TRUE, minWidth = 320)
      ),
      if (show_flagged) list(
        flagged = reactable::colDef(name = "Flagged", maxWidth = 90, cell = function(value) if (isTRUE(value)) "Yes" else "")
      ),
      pair_defs, extra_defs, score_defs
    ),
    columnGroups = if (length(column_groups) > 0L) column_groups,
    defaultColDef = reactable::colDef(na = "-", minWidth = 70),
    theme = .gnaf_reactable_theme(),
    details = function(index) .gnaf_threshold_details(table_data[index, ])
  )
}

# Component-by-component view of what the parser saw against what matched,
# which is usually where a false positive gives itself away.
.gnaf_threshold_details <- function(row) {
  pairs <- c(
    postcode = "in_postcode", locality_name = "in_locality",
    street_name = "in_street_name", street_type = "in_street_type",
    street_suffix = "in_street_suffix", number_first = "in_number_first",
    number_last = "in_number_last", flat_type = "in_flat_type",
    flat_number = "in_flat_number"
  )
  pairs <- pairs[names(pairs) %in% names(row) & pairs %in% names(row)]
  if (length(pairs) == 0L) return(NULL)

  comparison <- data.table(side = c("Input", "Matched"))
  for (matched_col in names(pairs)) {
    comparison[, (matched_col) := as.character(c(row[[pairs[[matched_col]]]], row[[matched_col]]))]
  }
  shiny::tags$div(
    class = "details-shell",
    shiny::div(class = "details-title", "Parsed input vs matched components"),
    reactable::reactable(
      comparison,
      compact = TRUE,
      bordered = FALSE,
      pagination = FALSE,
      fullWidth = FALSE,
      columns = list(side = reactable::colDef(name = "", maxWidth = 80)),
      defaultColDef = reactable::colDef(minWidth = 90, na = "-"),
      theme = .gnaf_details_theme()
    )
  )
}

# Downsampling keeps the histogram responsive on large x; shape is unaffected
# by sampling at tens of thousands of rows, and this is redrawn on every
# (debounced) threshold change while the accordion is open.
.gnaf_threshold_sample_rows <- function(data, keep, sample_n) {
  if (nrow(data) <= sample_n) return(list(data = data, keep = keep))
  idx <- sample.int(nrow(data), sample_n)
  list(data = data[idx], keep = keep[idx])
}

.gnaf_threshold_plot <- function(data, vars, keep, thresholds, maxes, sample_n = 50000L) {
  sampled <- .gnaf_threshold_sample_rows(data, keep, sample_n)
  data <- sampled$data
  keep <- sampled$keep
  plot_data <- data[, vars, with = FALSE]
  plot_data[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
  plot_data[, scope := fifelse(keep, "In scope", "Out of scope")]
  plot_data <- plot_data[!is.na(total_score)]
  if (nrow(plot_data) == 0L) return(NULL)
  long <- melt(plot_data, id.vars = "scope", variable.name = "component", value.name = "score")
  long[, component := factor(component, levels = vars, labels = .GNAF_THRESHOLD_LABELS[vars])]

  lines <- rbindlist(lapply(vars, function(var) {
    bounds <- thresholds[[var]]
    at <- c(if (bounds[1L] > 0L) bounds[1L], if (bounds[2L] < maxes[[var]]) bounds[2L])
    if (length(at) == 0L) return(NULL)
    data.table(component = .GNAF_THRESHOLD_LABELS[[var]], at = at)
  }))

  p <- ggplot2::ggplot(long, ggplot2::aes(x = score, fill = scope)) +
    ggplot2::geom_histogram(bins = 25, position = "identity", alpha = 0.7) +
    ggplot2::facet_wrap(~component, scales = "free") +
    ggplot2::scale_fill_manual(values = c("In scope" = "#1f5f8b", "Out of scope" = "#b45309")) +
    ggplot2::labs(x = NULL, y = "Rows", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")
  if (nrow(lines) > 0L) {
    lines[, component := factor(component, levels = levels(long$component))]
    p <- p + ggplot2::geom_vline(data = lines, ggplot2::aes(xintercept = at), linetype = "dashed")
  }
  p
}
