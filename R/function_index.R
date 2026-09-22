# Single source of truth for the categorised function index. It drives the
# startup banner (.onAttach) and the "Function index" section of ?gnafr (via
# @eval in gnafr-package.R).
#
# A node is a named list. An element that is a string is a function (name ->
# short description); an element that is itself a list is a sub-category.
# Keep descriptions under ~36 characters so banner lines fit in 80 columns.
.function_index <- list(
    "Database setup" = list(
        gnaf_connect  = "connect to a gnafr database",
        gnaf_disconnect = "disconnect from a database",
        gnaf_init     = "initialise the gnafr schema",
        gnaf_status   = "row counts for all tables",
        gnaf_build_db = "build DB from raw G-NAF extract"
    ),
    "Data loading" = list(
        gnaf_load                      = "load GNAF data from CSV",
        gnaf_load_psv                  = "load GNAF data from PSV files",
        gnaf_build_street_aliases      = "street-only fallback rows",
        gnaf_canonicalize_street_types = "canonicalise street types",
        gnaf_rebuild_locality_index    = "rebuild locality search index",
        gnaf_rebuild_street_type_index = "rebuild street-type index"
    ),
    "Custom addresses" = list(
        gnaf_add           = "add custom addresses",
        gnaf_remove_custom = "remove custom addresses"
    ),
    "Matching" = list(
        address_parse         = "parse address into components",
        gnaf_match             = "match addresses against GNAF",
        gnaf_match_features    = "agreement/conflict features",
        gnaf_text_scores       = "full-string similarity scores",
        gnaf_threshold_filter  = "interactive match thresholding"
    ),
    "Geographies" = list(
        gnaf_add_geography      = "add a geography (e.g. SA2)",
        gnaf_register_geography = "register existing geography table",
        gnaf_list_geographies   = "list registered geographies",
        gnaf_join_geographies   = "join geography attributes",
        gnaf_geography_coverage = "check geography coverage",
        gnaf_remove_geography   = "remove a saved geography"
    ),
    "Spatial & shapefiles" = list(
        gnaf_add_spatial        = "store polygon attributes",
        read_shapefile          = "read shapefile, list columns",
        subset_shapefile        = "subset sf object by column",
        spatial_lookup          = "point-in-polygon lookup",
        plot_boundaries_heatmap = "plot boundaries + heatmap"
    ),
    "Match cache" = list(
        gnaf_cache_status   = "current match cache state",
        gnaf_cache_clear    = "clear the match cache",
        gnaf_cache_rollback = "roll back cache after a time",
        gnaf_cache_history  = "summarise cache over time",
        gnaf_cache_sample   = "sample rows from the cache",
        sample_gnaf         = "sample rows from DB tables"
    ),
    "App & simulation" = list(
        gnaf_app               = "launch interactive matching app",
        address_perturb_sample = "sample + perturb test addresses"
    )
)

# Flatten the index to a named character vector of function -> description.
.index_functions <- function(node = .function_index) {
    out <- lapply(seq_along(node), function(i) {
        if (is.list(node[[i]])) return(.index_functions(node[[i]]))
        fn <- node[[i]]
        names(fn) <- names(node)[i]
        fn
    })
    unlist(out)
}

.tree_glyphs <- function(utf8) {
    if (utf8) {
        c(tee = "├─ ", last = "└─ ",
          pipe = "│  ", blank = "   ")
    } else {
        c(tee = "+- ", last = "\\- ", pipe = "|  ", blank = "   ")
    }
}

# Draw one node of the index as `tree`-style lines. Descriptions are aligned
# within each group of sibling functions, so short names stay close to their
# text even when another group has very long names.
#
# Widths are measured on the plain text and colour is applied afterwards, so
# ANSI codes never disturb the alignment. cli emits no codes at all when the
# console has no colour support (piped output, NO_COLOR, Rgui, ...).
.index_tree_lines <- function(node = .function_index,
                              utf8 = cli::is_utf8_output(),
                              prefix = "") {
    glyphs <- .tree_glyphs(utf8)
    n <- length(node)
    is_fn <- !vapply(node, is.list, logical(1L))
    branch <- rep(glyphs[["tee"]], n)
    branch[n] <- glyphs[["last"]]
    tree <- paste0(prefix, branch)
    name <- paste0(names(node), ifelse(is_fn, "()", ""))
    width <- nchar(tree, type = "width") + nchar(name, type = "width")
    pad <- if (any(is_fn)) max(width[is_fn]) else 0L
    # Top-level categories (empty prefix) are bold cyan, nested ones plain cyan.
    category <- if (nzchar(prefix)) cli::col_cyan
                else cli::combine_ansi_styles("bold", "cyan")

    lines <- character()
    for (i in seq_len(n)) {
        if (is_fn[i]) {
            lines <- c(lines, paste0(
                cli::col_grey(tree[i]), cli::col_green(name[i]),
                strrep(" ", pad - width[i]), "  ", node[[i]]
            ))
        } else {
            child_prefix <- paste0(
                prefix, glyphs[[if (i == n) "blank" else "pipe"]]
            )
            lines <- c(
                lines, paste0(cli::col_grey(tree[i]), category(name[i])),
                .index_tree_lines(node[[i]], utf8, child_prefix)
            )
        }
    }
    lines
}

.index_banner <- function(version, utf8 = cli::is_utf8_output()) {
    c(
        paste0(
            cli::style_bold(paste("gnafr", version)),
            ": Australian address matching against G-NAF"
        ),
        .index_tree_lines(.function_index, utf8),
        cli::col_grey(
            "Help: ?gnafr  |  Silence: options(gnafr.verbose = FALSE)"
        )
    )
}

# The same index as nested markdown bullets, for the ?gnafr help page.
.index_markdown <- function(node = .function_index, depth = 0L) {
    indent <- strrep("  ", depth)
    unlist(lapply(seq_along(node), function(i) {
        if (is.list(node[[i]])) {
            label <- if (depth == 0L) "**%s**" else "*%s*"
            c(sprintf(paste0("%s* ", label), indent, names(node)[i]),
              .index_markdown(node[[i]], depth + 1L))
        } else {
            sprintf("%s* [%s()] - %s", indent, names(node)[i], node[[i]])
        }
    }), use.names = FALSE)
}

# Roxygen lines for the "Function index" section of ?gnafr, inserted by
# `@eval` in gnafr-package.R so the help page is built from the same
# registry as the startup banner.
.index_roxygen <- function() {
    c("@section Function index:", .index_markdown())
}
