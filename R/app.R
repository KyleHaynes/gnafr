#' Launch an interactive geocoding app for GNAF matching
#'
#' Opens a Shiny app that lets you connect to a DuckDB database, submit one
#' address per line, review `gnaf_match()` results, and inspect full-string
#' Jaro-Winkler and Jaccard similarity diagnostics between the input string and
#' the matched address label.
#'
#' @param con Optional DBI connection created by [gnaf_connect()]. If omitted,
#'   the app can open a connection from `db_path`.
#' @param db_path Optional DuckDB path to pre-fill in the app and connect to on
#'   launch when `con` is `NULL`.
#' @param launch.browser Passed to [shiny::runApp()] when `run = TRUE`.
#' @param run If `TRUE` (default), launches the app immediately. If `FALSE`,
#'   returns the [shiny::shiny.appobj()] for embedding or testing.
#' @return Invisibly returns the app object when `run = TRUE`, otherwise returns
#'   the app object.
#' @export
gnaf_app <- function(con = NULL, db_path = NULL,
                     launch.browser = interactive(), run = TRUE) {
  .gnaf_require_app_packages()

  if (!is.null(con) && !inherits(con, "DBIConnection")) {
    stop("'con' must be a DBI connection returned by gnaf_connect()")
  }
  if (!is.null(db_path) && (!is.character(db_path) || length(db_path) != 1L)) {
    stop("'db_path' must be a single character string")
  }

  ui <- bslib::page_sidebar(
    title = "gnafr geocoder",
    window_title = "gnafr geocoder",
    theme = bslib::bs_theme(version = 5),
    sidebar = bslib::sidebar(
      id = "gnaf-sidebar",
      width = 360,
      open = "desktop",
      bslib::accordion(
        id = "gnaf-controls",
        open = c("connection", "addresses", "run"),
        bslib::accordion_panel(
          "Connection",
          value = "connection",
          icon = shiny::icon("plug"),
          shiny::textInput("db_path", "DuckDB path", value = if (is.null(db_path)) "" else db_path),
          shiny::actionButton("connect", "Connect", class = "btn-primary"),
          shiny::div(class = "section-gap"),
          shiny::uiOutput("connection_status")
        ),
        bslib::accordion_panel(
          "Addresses",
          value = "addresses",
          icon = shiny::icon("map-location-dot"),
          shiny::textAreaInput(
            "addresses",
            "Addresses (one per line)",
            rows = 12,
            placeholder = paste(
              "10 St James Ct, Tamborine Mountain QLD 4272",
              "77 broadwater rd mount gravatt east 4122",
              sep = "\n"
            )
          )
        ),
        bslib::accordion_panel(
          "Match options",
          value = "match",
          icon = shiny::icon("sliders"),
          shiny::numericInput("max_results", "Matches per input", value = 3, min = 1, max = 50, step = 1),
          shiny::numericInput("min_score", "Minimum total score", value = 40, min = 0, max = 100, step = 1),
          shiny::checkboxInput("include_custom", "Include custom addresses", value = TRUE),
          shiny::checkboxInput("include_aliases", "Include aliases", value = TRUE),
          shiny::selectizeInput(
            "alias_types", "Alias types",
            choices = .gnaf_alias_type_choices(), multiple = TRUE,
            options = list(placeholder = "All rows (default)")
          ),
          shiny::div(
            class = "help-text",
            "Leave empty to match every row. Unchecking 'Include aliases' is equivalent to 'Core rows only'."
          ),
          shiny::checkboxInput("resolve_principal", "Resolve principal (add principal_* columns)", value = FALSE),
          shiny::checkboxInput("return_principal", "Return principal address", value = FALSE),
          shiny::checkboxInput("return_primary", "Return primary address", value = FALSE),
          shiny::checkboxInput("locality_fallback", "Locality fallback", value = TRUE),
          shiny::checkboxInput("street_only_fallback", "Street-only fallback", value = FALSE),
          shiny::numericInput("fallback_threshold", "Fallback threshold", value = 80, min = 0, max = 100, step = 1)
        ),
        bslib::accordion_panel(
          "Scoring weights",
          value = "weights",
          icon = shiny::icon("scale-balanced"),
          shiny::div(class = "help-text", "Weights must sum to 100."),
          shiny::numericInput("w_postcode", "Postcode", value = 20, min = 0, max = 100, step = 1),
          shiny::numericInput("w_suburb", "Suburb", value = 15, min = 0, max = 100, step = 1),
          shiny::numericInput("w_street_name", "Street name", value = 40, min = 0, max = 100, step = 1),
          shiny::numericInput("w_street_type", "Street type", value = 10, min = 0, max = 100, step = 1),
          shiny::numericInput("w_number", "Number", value = 10, min = 0, max = 100, step = 1),
          shiny::numericInput("w_flat", "Flat / level", value = 5, min = 0, max = 100, step = 1)
        ),
        bslib::accordion_panel(
          "Advanced",
          value = "advanced",
          icon = shiny::icon("gears"),
          shiny::checkboxInput("normalize", "Normalise input", value = TRUE),
          shiny::checkboxInput("cache", "Use match cache", value = TRUE),
          shiny::numericInput("cache_threshold", "Cache threshold", value = 95, min = 0, max = 100, step = 1),
          shiny::checkboxInput("verbose", "Verbose output", value = FALSE),
          shiny::selectizeInput(
            "geographies", "Geographies",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "None")
          )
        ),
        bslib::accordion_panel(
          "Run",
          value = "run",
          icon = shiny::icon("play"),
          shiny::actionButton("run_match", "Geocode", class = "btn-success", width = "100%"),
          shiny::downloadButton("download_results", "Download CSV"),
          shiny::div(
            class = "help-text section-gap",
            "The results table adds full-string Jaro-Winkler and Jaccard similarity scores between each input string and its matched address label."
          )
        )
      )
    ),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(jsdiffr::diff_css_default())),
      shiny::tags$style(shiny::HTML(
        ".app-subtitle {margin: 0 0 16px; color: #486581;}\n",
        ".panel {background: #f8fbff; border: 1px solid #d9e2ec; border-radius: 14px; padding: 18px;}\n",
        ".metric-grid {display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; margin: 16px 0 20px;}\n",
        ".metric-card {background: linear-gradient(135deg, #102a43, #1f5f8b); color: #fff; border-radius: 14px; padding: 16px 18px;}\n",
        ".metric-label {font-size: 12px; text-transform: uppercase; letter-spacing: 0.08em; opacity: 0.8;}\n",
        ".metric-value {font-size: 28px; font-weight: 700; line-height: 1.2; margin-top: 6px;}\n",
        ".status-pill {display: inline-block; padding: 6px 10px; border-radius: 999px; background: #d9f99d; color: #365314; font-weight: 600;}\n",
        ".status-pill--warn {background: #fee2e2; color: #991b1b;}\n",
        ".help-text {color: #52606d; font-size: 13px;}\n",
        ".section-gap {margin-top: 16px;}\n",
        ".reactable {font-size: 12px;}\n",
        ".reactable .rt-th, .reactable .rt-td {padding: 6px 8px;}\n",
        ".reactable .rt-tr-group {min-height:auto;}\n",
        ".details-shell {max-width: 780px; padding: 8px 10px; background: #f8fbff; border-radius: 10px;}\n",
        ".details-title {font-size: 12px; font-weight: 700; margin-bottom: 8px; color: #102a43;}\n",
        ".diff-cell {padding: 0px 1px;}\n",
        ".diff-cell .jsdiff-pre {white-space: pre-wrap; font-size: 12px;}\n",
        ".diff-controls {margin: 16px 0;}\n",
        "@media (max-width: 900px) {.metric-grid {grid-template-columns: 1fr;}}"
      ))
    ),
    shiny::div(
      class = "app-subtitle",
      "Run address matching against your DuckDB-backed GNAF database and inspect text similarity diagnostics for each match."
    ),
    shiny::uiOutput("metrics"),
    shiny::tabsetPanel(
      shiny::tabPanel("Matches", reactable::reactableOutput("results_table")),
      shiny::tabPanel("Parsed Inputs", reactable::reactableOutput("parsed_table")),
      shiny::tabPanel(
        "Compare",
        shiny::div(
          class = "panel diff-controls",
          shiny::fluidRow(
            shiny::column(
              5,
              shiny::radioButtons(
                "diff_pair", "Compare",
                choices = c(
                  "Input vs Standardised"           = "raw_std",
                  "Input vs Matched address"         = "raw_match",
                  "Standardised vs Matched address"  = "std_match"
                ),
                selected = "std_match"
              )
            ),
            shiny::column(
              4,
              shiny::radioButtons(
                "diff_granularity", "Diff level",
                choices = c("Words" = "diff_words", "Characters" = "diff_chars"),
                selected = "diff_chars", inline = TRUE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput("diff_changes_only", "Only rows with differences", value = FALSE)
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::selectizeInput(
                "diff_extra_cols", "Additional columns",
                choices = .gnaf_diff_extra_choices(), multiple = TRUE,
                width = "100%",
                options = list(placeholder = "Add columns to show before the comparison columns")
              )
            )
          )
        ),
        reactable::reactableOutput("diff_table")
      )
    )
  )

  server <- function(input, output, session) {
    # Each session owns its connection state. Reactive values also invalidate
    # current_con() when the user connects or replaces a connection.
    app_state <- shiny::reactiveValues(con = con, owns_connection = FALSE)
    connection_info <- shiny::reactiveVal(list(connected = !is.null(con), path = db_path, status = NULL))
    results_rv <- shiny::reactiveVal(.gnaf_empty_app_results())
    parsed_rv <- shiny::reactiveVal(.gnaf_empty_parsed())

    release_connection <- function() {
      owned <- shiny::isolate(app_state$owns_connection)
      connection <- shiny::isolate(app_state$con)
      if (isTRUE(owned) && !is.null(connection)) {
        try(gnaf_disconnect(connection), silent = TRUE)
      }
      app_state$con <- NULL
      app_state$owns_connection <- FALSE
    }

    current_con <- shiny::reactive({
      app_state$con
    })

    if (!is.null(con)) {
      connection_info(list(
        connected = TRUE,
        path = if (is.null(db_path)) "Existing connection" else db_path,
        status = tryCatch(gnaf_status(con), error = function(e) NULL)
      ))
    } else if (!is.null(db_path) && nzchar(trimws(db_path))) {
      tryCatch({
        app_state$con <- gnaf_connect(trimws(db_path))
        app_state$owns_connection <- TRUE
        connection_info(list(
          connected = TRUE,
          path = trimws(db_path),
          status = gnaf_status(app_state$con)
        ))
      }, error = function(e) {
        release_connection()
        connection_info(list(connected = FALSE, path = trimws(db_path), status = conditionMessage(e)))
      })
    }

    shiny::observeEvent(input$connect, {
      shiny::req(nzchar(trimws(input$db_path)))

      tryCatch({
        release_connection()
        app_state$con <- gnaf_connect(trimws(input$db_path))
        app_state$owns_connection <- TRUE
        connection_info(list(
          connected = TRUE,
          path = trimws(input$db_path),
          status = gnaf_status(app_state$con)
        ))
        shiny::showNotification("Connected to DuckDB database.", type = "message")
      }, error = function(e) {
        release_connection()
        connection_info(list(connected = FALSE, path = trimws(input$db_path), status = conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    shiny::observeEvent(input$run_match, {
      shiny::req(current_con())

      addresses <- trimws(unlist(strsplit(input$addresses %||% "", "\\r?\\n", perl = TRUE)))
      addresses <- addresses[nzchar(addresses)]

      if (length(addresses) == 0L) {
        shiny::showNotification("Enter at least one address.", type = "warning")
        return(invisible(NULL))
      }

      # Alias filtering: "Include aliases" unchecked is equivalent to
      # alias_types = NA (core rows only), and cannot be combined with an
      # explicit alias_types selection.
      include_aliases_arg <- isTRUE(input$include_aliases)
      alias_types_raw <- input$alias_types %||% character(0)
      if (!include_aliases_arg) {
        alias_types_arg <- NULL
      } else if (length(alias_types_raw) == 0L) {
        alias_types_arg <- NULL
      } else {
        alias_types_arg <- if ("__core__" %in% alias_types_raw) {
          c(NA_character_, setdiff(alias_types_raw, "__core__"))
        } else {
          alias_types_raw
        }
      }

      # Geographies: empty means none, "__all__" means every registered layer.
      geographies_raw <- input$geographies %||% character(0)
      geographies_arg <- if (length(geographies_raw) == 0L) {
        NULL
      } else if ("__all__" %in% geographies_raw) {
        TRUE
      } else {
        setdiff(geographies_raw, "__all__")
      }

      # Scoring weights must sum to 100 before they reach gnaf_match().
      weights_arg <- list(
        postcode = as.numeric(input$w_postcode),
        suburb = as.numeric(input$w_suburb),
        street_name = as.numeric(input$w_street_name),
        street_type = as.numeric(input$w_street_type),
        number = as.numeric(input$w_number),
        flat = as.numeric(input$w_flat)
      )
      if (!isTRUE(all.equal(sum(unlist(weights_arg)), 100, tolerance = 1e-8))) {
        shiny::showNotification(
          "Scoring weights must sum to 100. Adjust the weights and try again.",
          type = "error", duration = NULL
        )
        return(invisible(NULL))
      }

      tryCatch({
        shiny::withProgress(message = "Geocoding", value = 0.1, {
          parsed <- address_parse(addresses, normalize = isTRUE(input$normalize))
          shiny::incProgress(0.35, detail = "Parsed input addresses")

          matches <- gnaf_match(
            addresses = addresses,
            con = current_con(),
            max_results = as.integer(input$max_results),
            min_score = as.integer(input$min_score),
            include_custom = isTRUE(input$include_custom),
            include_aliases = include_aliases_arg,
            alias_types = alias_types_arg,
            resolve_principal = isTRUE(input$resolve_principal),
            return_principal = isTRUE(input$return_principal),
            return_primary = isTRUE(input$return_primary),
            locality_fallback = isTRUE(input$locality_fallback),
            street_only_fallback = isTRUE(input$street_only_fallback),
            fallback_threshold = as.integer(input$fallback_threshold),
            weights = weights_arg,
            normalize = isTRUE(input$normalize),
            cache = isTRUE(input$cache),
            cache_threshold = as.integer(input$cache_threshold),
            verbose = isTRUE(input$verbose),
            geographies = geographies_arg
          )
          shiny::incProgress(0.45, detail = "Matched against database")

          results_rv(.gnaf_prepare_app_results(matches))
          parsed_rv(parsed)
          shiny::incProgress(0.1, detail = "Rendered tables")
        })
      }, error = function(e) {
        results_rv(.gnaf_empty_app_results())
        parsed_rv(.gnaf_empty_parsed())
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # Refresh the available geography layers whenever the connection changes.
    shiny::observe({
      con2 <- current_con()
      geo_choices <- if (is.null(con2)) {
        character(0)
      } else {
        geo_names <- tryCatch(
          gnaf_list_geographies(con2)$name,
          error = function(e) character(0)
        )
        if (length(geo_names) > 0L) {
          c("All registered" = "__all__", geo_names)
        } else {
          character(0)
        }
      }
      shiny::updateSelectizeInput(
        session, "geographies",
        choices = geo_choices, selected = character(0)
      )
    })

    session$onSessionEnded(function() {
      release_connection()
    })

    output$connection_status <- shiny::renderUI({
      info <- connection_info()
      if (isTRUE(info$connected)) {
        status_rows <- info$status
        rows_text <- if (is.data.table(status_rows) && nrow(status_rows) > 0L) {
          paste(sprintf("%s: %s", status_rows$table, format(status_rows$rows, big.mark = ",")), collapse = " | ")
        } else {
          "Connection ready"
        }

        shiny::tagList(
          shiny::div(class = "status-pill", "Connected"),
          shiny::p(shiny::tags$strong("Database:"), info$path %||% "Existing connection"),
          shiny::p(rows_text, class = "help-text")
        )
      } else {
        message_text <- info$status %||% "Not connected"
        shiny::tagList(
          shiny::div(class = "status-pill status-pill--warn", "Disconnected"),
          shiny::p(message_text, class = "help-text")
        )
      }
    })

    output$metrics <- shiny::renderUI({
      results <- results_rv()
      best <- results[matched %in% TRUE & match_rank == 1L]
      matched_inputs <- uniqueN(best$input_id)
      unmatched_inputs <- uniqueN(results[matched %in% FALSE, input_id])
      match_rate <- if (uniqueN(results$input_id) > 0L) {
        round(100 * matched_inputs / uniqueN(results$input_id), 1)
      } else {
        NA_real_
      }
      avg_score <- if (nrow(best) > 0L) round(mean(best$total_score, na.rm = TRUE), 1) else NA_real_
      avg_text <- if (nrow(best) > 0L) round(mean(best$text_similarity, na.rm = TRUE), 1) else NA_real_

      shiny::div(
        class = "metric-grid",
        .gnaf_metric_card("Matched inputs", format(matched_inputs, big.mark = ",")),
        .gnaf_metric_card("Match rate", if (is.na(match_rate)) "-" else sprintf("%.1f%%", match_rate)),
        .gnaf_metric_card("Unmatched inputs", format(unmatched_inputs, big.mark = ","))
      )
    })

    output$results_table <- reactable::renderReactable({
      results <- results_rv()
      score_fill <- function(score) .gnaf_score_fill(score)
      comparison_cell <- function(value, index) {
        row <- results[index, ]
        fill <- if (isTRUE(row$matched)) score_fill(row$text_similarity) else "#e5e7eb"
        match_text <- if (isTRUE(row$matched) && !is.na(row$address_label)) {
          row$address_label
        } else {
          sprintf("No match (%s)", row$match_status %||% "unmatched")
        }

        shiny::div(
          style = sprintf(
            "background:%s; border-radius:12px; padding:10px 12px;",
            fill
          ),
          shiny::div(style = "font-size:11px; text-transform:uppercase; letter-spacing:0.08em; opacity:0.75;", "Input"),
          shiny::div(style = "font-weight:700; margin-bottom:8px;", row$input_raw),
          shiny::div(style = "font-size:11px; text-transform:uppercase; letter-spacing:0.08em; opacity:0.75;", "Matched"),
          shiny::div(style = "font-weight:700;", match_text)
        )
      }

      reactable::reactable(
        results,
        defaultPageSize = 12,
        searchable = TRUE,
        filterable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        resizable = TRUE,
        defaultSorted = list(matched = "desc", total_score = "desc"),
        columns = list(
          input_id = reactable::colDef(name = "Input", maxWidth = 80),
          match_rank = reactable::colDef(name = "Rank", maxWidth = 80),
          matched = reactable::colDef(name = "Matched", maxWidth = 90),
          match_status = reactable::colDef(name = "Status", minWidth = 130),
          input_standardised = reactable::colDef(name = "Standardised", minWidth = 260),
          comparison = reactable::colDef(name = "Input / matched", minWidth = 420, cell = comparison_cell),
          total_score = .gnaf_score_col("Total", digits = 0),
          text_similarity = .gnaf_score_col("Text score", digits = 1),
          jarowinkler_score = .gnaf_score_col("Jaro-Winkler", digits = 1),
          jaccard_score = .gnaf_score_col("Jaccard", digits = 1),
          levenshtein_score = .gnaf_score_col("Levenshtein", digits = 1),
          longitude = reactable::colDef(format = reactable::colFormat(digits = 6)),
          latitude = reactable::colDef(format = reactable::colFormat(digits = 6))
        ),
        defaultColDef = reactable::colDef(na = "-", minWidth = 80),
        theme = .gnaf_reactable_theme(),
        columnGroups = list(
          reactable::colGroup(name = "Comparison", columns = c("input_standardised", "comparison")),
          reactable::colGroup(name = "Diagnostics", columns = c("total_score", "text_similarity", "jarowinkler_score", "jaccard_score", "levenshtein_score"))
        ),
        details = function(index) {
          row <- results[index, ]
          parsed <- parsed_rv()[input_id == row$input_id, .(
            in_locality,
            in_street_name,
            in_street_type,
            in_number_first,
            in_number_last,
            in_flat_type,
            in_flat_number,
            in_building_name
          )]

          shiny::tagList(
            shiny::tags$div(
              class = "details-shell",
              shiny::div(class = "details-title", "Parsed input"),
              reactable::reactable(
                parsed,
                compact = TRUE,
                bordered = FALSE,
                pagination = FALSE,
                fullWidth = FALSE,
                defaultColDef = reactable::colDef(minWidth = 76, na = "-"),
                theme = .gnaf_details_theme()
              )
            )
          )
        }
      )
    })

    output$parsed_table <- reactable::renderReactable({
      reactable::reactable(
        parsed_rv(),
        defaultPageSize = 10,
        searchable = TRUE,
        filterable = TRUE,
        striped = TRUE,
        resizable = TRUE,
        theme = .gnaf_reactable_theme(),
        columns = list(input_raw = reactable::colDef(minWidth = 280))
      )
    })

    output$diff_table <- reactable::renderReactable({
      pair_cols <- .gnaf_diff_pair_columns(input$diff_pair %||% "raw_std")
      method_fn <- if (identical(input$diff_granularity, "diff_chars")) {
        jsdiffr::diff_chars
      } else {
        jsdiffr::diff_words
      }

      results <- results_rv()
      # Drop any picked column that duplicates the active left/right pair, and
      # any that no longer exists on the current results (e.g. stale picks
      # left over from before a re-match).
      extra_cols <- setdiff(
        intersect(input$diff_extra_cols %||% character(0), names(results)),
        c(pair_cols$left, pair_cols$right)
      )

      diff_data <- results[, c(
        "input_id", "match_rank", "matched", "match_status",
        extra_cols, pair_cols$left, pair_cols$right
      ), with = FALSE]
      data.table::setnames(diff_data, c(pair_cols$left, pair_cols$right), c("left", "right"))
      diff_data[, has_diff := !is.na(right) & nzchar(right) & (is.na(left) | left != right)]
      diff_data[, diff := ""]
      # Extra columns sit between the always-shown identity columns and the
      # standardised/matched-address/diff columns, per the column picker above.
      data.table::setcolorder(diff_data, c(
        "input_id", "match_rank", "matched", "match_status",
        extra_cols, "left", "right", "has_diff", "diff"
      ))

      if (isTRUE(input$diff_changes_only)) {
        diff_data <- diff_data[has_diff == TRUE]
      }

      diff_cell <- function(value, index) {
        row <- diff_data[index, ]
        .gnaf_diff_cell(row$left, row$right, method_fn)
      }

      extra_col_defs <- stats::setNames(
        lapply(extra_cols, function(col) reactable::colDef(name = .gnaf_pretty_colname(col), minWidth = 120)),
        extra_cols
      )

      reactable::reactable(
        diff_data,
        defaultPageSize = 12,
        searchable = TRUE,
        filterable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        resizable = TRUE,
        defaultSorted = list(has_diff = "desc"),
        columns = c(
          list(
            input_id     = reactable::colDef(name = "Input", maxWidth = 80),
            match_rank   = reactable::colDef(name = "Rank", maxWidth = 80),
            matched      = reactable::colDef(name = "Matched", maxWidth = 90),
            match_status = reactable::colDef(name = "Status", minWidth = 130)
          ),
          extra_col_defs,
          list(
            left         = reactable::colDef(name = pair_cols$left_label, minWidth = 220),
            right        = reactable::colDef(name = pair_cols$right_label, minWidth = 220),
            has_diff     = reactable::colDef(show = FALSE),
            diff         = reactable::colDef(name = "Diff", cell = diff_cell, html = TRUE, minWidth = 380)
          )
        ),
        defaultColDef = reactable::colDef(na = "-", minWidth = 80),
        theme = .gnaf_reactable_theme()
      )
    })

    output$download_results <- shiny::downloadHandler(
      filename = function() {
        sprintf("gnafr-geocode-%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S"))
      },
      content = function(file) {
        data.table::fwrite(results_rv(), file)
      }
    )
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  if (!isTRUE(run)) {
    return(app)
  }

  shiny::runApp(app, launch.browser = launch.browser)
  invisible(app)
}

.gnaf_require_app_packages <- function() {
  needed <- c("shiny", "bslib", "reactable", "jsdiffr")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "Install app dependencies first: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))",
      call. = FALSE
    )
  }
}

# Alias-type choices offered in the Match options panel. "__core__" is a sentinel
# for core (non-alias) rows, translated to NA in the run_match observer.
.gnaf_alias_type_choices <- function() {
  c(
    "Core rows (no alias)" = "__core__",
    "Street-only aliases" = "street_only",
    "Address aliases (ADDRESS:RA)" = "ADDRESS:RA",
    "Address synonyms (ADDRESS:SYN)" = "ADDRESS:SYN",
    "Locality synonyms (LOCALITY:SYN)" = "LOCALITY:SYN",
    "Locality street refs (LOCALITY:SR)" = "LOCALITY:SR",
    "Street synonyms (STREET:SYN)" = "STREET:SYN",
    "Street alternatives (STREET:ALT)" = "STREET:ALT"
  )
}

#' Add full-string text similarity scores to `gnaf_match()` results
#'
#' Compares each input string with its matched address label (both normalised
#' the same way as [gnaf_match()] does) and appends four 0-100 similarity
#' columns: `jarowinkler_score`, `jaccard_score` (character bigrams),
#' `levenshtein_score` (1 minus edit distance over the longer string) and
#' `text_similarity` (the mean of the Jaro-Winkler and Jaccard scores).
#' Unmatched rows get `NA`.
#' When linked addresses are returned, uses `matched_address_label` so these
#' diagnostics still describe the candidate that was scored and ranked.
#'
#' These are whole-string diagnostics that complement the component scores
#' from matching, and are the extra columns shown in [gnaf_app()] and
#' [gnaf_threshold_filter()]. The filter code generated by the threshold app
#' calls this function when a text-score threshold is in use.
#'
#' @param x A `data.table` returned by [gnaf_match()].
#' @return A copy of `x` with the four score columns appended.
#' @examples
#' \dontrun{
#' scored <- gnaf_text_scores(gnaf_match(addresses, con))
#' scored[jarowinkler_score < 85]
#' }
#' @export
gnaf_text_scores <- function(x) {
  if (!is.data.table(x)) stop("'x' must be a data.table returned by gnaf_match()")
  missing_cols <- setdiff(c("input_raw", "address_label", "matched"), names(x))
  if (length(missing_cols) > 0L) {
    stop("'x' is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  out <- copy(x)
  input_norm <- .normalize_addr(out$input_raw)
  match_label <- if ("matched_address_label" %in% names(out)) {
    out$matched_address_label
  } else {
    out$address_label
  }
  match_norm <- .normalize_addr(fifelse(is.na(match_label), "", match_label))
  matched_idx <- out$matched %in% TRUE & !is.na(match_label)

  jw <- jaccard <- lev <- rep(NA_real_, nrow(out))
  if (any(matched_idx)) {
    left <- input_norm[matched_idx]
    right <- match_norm[matched_idx]
    jw[matched_idx] <- fast.string::jaro_winkler(left, right, p = 0.1)
    jaccard[matched_idx] <- 1 - stringdist::stringdist(left, right, method = "jaccard", q = 2)
    lev[matched_idx] <- 1 - stringdist::stringdist(left, right, method = "lv") /
      pmax(nchar(left), nchar(right), 1L)
  }

  out[, jarowinkler_score := round(pmax(jw, 0) * 100, 1)]
  out[, jaccard_score := round(pmax(jaccard, 0) * 100, 1)]
  out[, levenshtein_score := round(pmax(lev, 0) * 100, 1)]
  out[, text_similarity := round((jarowinkler_score + jaccard_score) / 2, 1)]
  out[]
}

.gnaf_prepare_app_results <- function(results) {
  if (!is.data.table(results) || nrow(results) == 0L) {
    return(.gnaf_empty_app_results())
  }
  out <- gnaf_text_scores(results)
  out[, comparison := ""]
  out[]
}

.gnaf_empty_app_results <- function() {
  data.table(
    input_id = integer(),
    input_raw = character(),
    input_standardised = character(),
    match_rank = integer(),
    matched = logical(),
    match_status = character(),
    total_score = integer(),
    score_postcode = integer(),
    score_suburb = integer(),
    score_street_name = integer(),
    score_street_type = integer(),
    score_number = integer(),
    score_flat = integer(),
    address_detail_pid = character(),
    address_label = character(),
    building_name = character(),
    flat_type = character(),
    flat_number = character(),
    number_first = integer(),
    number_last = integer(),
    street_name = character(),
    street_type = character(),
    street_suffix = character(),
    locality_name = character(),
    state = character(),
    postcode = integer(),
    longitude = numeric(),
    latitude = numeric(),
    source = character(),
    in_postcode = integer(),
    in_state = character(),
    in_locality = character(),
    in_street_name = character(),
    in_street_type = character(),
    in_street_suffix = character(),
    in_number_last = integer(),
    in_flat_type = character(),
    in_flat_number = character(),
    in_building_name = character(),
    in_number_first = integer(),
    comparison = character(),
    jarowinkler_score = numeric(),
    jaccard_score = numeric(),
    levenshtein_score = numeric(),
    text_similarity = numeric()
  )
}

.gnaf_empty_parsed <- function() {
  data.table(
    input_id = integer(),
    input_raw = character(),
    in_postcode = integer(),
    in_state = character(),
    in_locality = character(),
    in_street_name = character(),
    in_street_type = character(),
    in_street_suffix = character(),
    in_number_first = integer(),
    in_number_last = integer(),
    in_flat_type = character(),
    in_flat_number = character(),
    in_building_name = character()
  )
}

.gnaf_metric_card <- function(label, value) {
  shiny::div(
    class = "metric-card",
    shiny::div(class = "metric-label", label),
    shiny::div(class = "metric-value", value)
  )
}

.gnaf_reactable_theme <- function() {
  reactable::reactableTheme(
    borderColor = "#d9e2ec",
    stripedColor = "#f8fbff",
    highlightColor = "#eef2ff",
    cellPadding = "6px 8px",
    style = list(fontSize = "12px", lineHeight = "1.35"),
    headerStyle = list(fontSize = "11px", textTransform = "uppercase", letterSpacing = "0.05em"),
    rowSelectedStyle = list(backgroundColor = "#dbeafe")
  )
}

.gnaf_details_theme <- function() {
  reactable::reactableTheme(
    borderColor = "#e2e8f0",
    cellPadding = "4px 6px",
    style = list(fontSize = "11px", lineHeight = "1.25"),
    headerStyle = list(fontSize = "10px", textTransform = "uppercase", letterSpacing = "0.05em")
  )
}

.gnaf_score_fill <- function(score) {
  if (length(score) == 0L || is.na(score)) return("#e5e7eb")
  palette <- grDevices::colorRampPalette(c("#7f1d1d", "#b45309", "#f59e0b", "#84cc16", "#166534"))(101)
  idx <- max(1L, min(101L, as.integer(round(score)) + 1L))
  palette[idx]
}

# Colour relative to `max` (a component's weight) rather than 100, so a full
# street_type score of 10 reads as strong instead of as a weak red.
.gnaf_score_col <- function(name, digits = 1, max = 100) {
  if (!is.finite(max) || max <= 0) max <- 100
  reactable::colDef(
    name = name,
    align = "center",
    format = reactable::colFormat(digits = digits),
    style = function(value) {
      if (length(value) == 0L || is.na(value)) {
        return(list(background = "#f3f4f6", color = "#6b7280", fontWeight = 500))
      }
      pct <- 100 * value / max
      list(
        background = .gnaf_score_fill(pct),
        color = if (isTRUE(pct >= 70)) "#f8fafc" else "#102a43",
        fontWeight = 700
      )
    }
  )
}

.gnaf_pretty_colname <- function(x) {
  x <- gsub("_", " ", x)
  tools::toTitleCase(x)
}

# Candidate columns offered in the Compare tab's "Additional columns" picker.
# Derived from the canonical results schema so it stays in sync with
# .gnaf_empty_app_results(); excludes columns that are either always shown
# (input_id, match_rank, matched, match_status) or internal-only (comparison).
.gnaf_diff_extra_choices <- function() {
  all_cols <- names(.gnaf_empty_app_results())
  always_shown <- c("input_id", "match_rank", "matched", "match_status", "comparison")
  candidates <- setdiff(all_cols, always_shown)
  stats::setNames(candidates, vapply(candidates, .gnaf_pretty_colname, character(1)))
}

.gnaf_diff_pair_columns <- function(pair) {
  switch(
    pair,
    raw_std = list(
      left = "input_raw", right = "input_standardised",
      left_label = "Input", right_label = "Standardised"
    ),
    raw_match = list(
      left = "input_raw", right = "address_label",
      left_label = "Input", right_label = "Matched address"
    ),
    std_match = list(
      left = "input_standardised", right = "address_label",
      left_label = "Standardised", right_label = "Matched address"
    ),
    stop("Unknown diff pair: ", pair, call. = FALSE)
  )
}

.gnaf_diff_cell <- function(left, right, method_fn) {
  if (is.na(right) || !nzchar(right)) {
    return('<div class="help-text">No match to compare</div>')
  }
  if (is.na(left)) left <- ""
  changes <- method_fn(left, right)
  sprintf(
    '<div class="diff-cell">%s</div>',
    jsdiffr::diff_to_html(changes, wrap = TRUE, pre = FALSE)
  )
}
