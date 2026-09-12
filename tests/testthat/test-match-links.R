linked_address_rows <- function() {
  data.table::data.table(
    address_detail_pid = c("PRIMARY", "SECONDARY", "ADDRESS_ALIAS",
                           "STREET_ALIAS", "LOCALITY_ALIAS"),
    address_label = c(
      "20 NEW ROAD, BRISBANE QLD 4000",
      "UNIT 2 20 NEW ROAD, BRISBANE QLD 4000",
      "UNIT 2 10 OLD STREET, ST LUCIA QLD 4067",
      "UNIT 2 10 FORMER STREET, ST LUCIA QLD 4067",
      "UNIT 2 10 OLD STREET, TOOWONG QLD 4066"
    ),
    number_first = c(20L, 20L, 10L, 10L, 10L),
    street_name = c("NEW", "NEW", "OLD", "FORMER", "OLD"),
    street_type = c("ROAD", "ROAD", "STREET", "STREET", "STREET"),
    locality_name = c("BRISBANE", "BRISBANE", "ST LUCIA", "ST LUCIA", "TOOWONG"),
    state = "QLD", postcode = c(4000L, 4000L, 4067L, 4067L, 4066L),
    flat_type = c(NA_character_, rep("UNIT", 4L)),
    flat_number = c(NA_character_, rep("2", 4L)),
    address_site_name = c("MAIN SITE", "UNIT SITE", rep("OLD SITE", 3L)),
    building_name = c(NA_character_, "NEW BUILDING", rep("OLD BUILDING", 3L)),
    longitude = c(153.01, 153.02, 152.99, 152.98, 152.97),
    latitude = c(-27.41, -27.42, -27.49, -27.48, -27.47),
    legal_parcel_id = paste0("PARCEL", 1:5), mb_code = as.character(101:105),
    date_created = as.Date("2020-01-01") + 1:5,
    geocode_type = c("PROPERTY CENTROID", rep("BUILDING CENTROID", 4L)),
    alias_type = c(NA_character_, NA_character_, "ADDRESS:RA",
                   "STREET:SYN", "LOCALITY:SYN"),
    alias_principal = c("PRINCIPAL", "PRINCIPAL", rep("ALIAS", 3L)),
    principal_pid = c(NA_character_, NA_character_, rep("SECONDARY", 3L)),
    primary_secondary = c("PRIMARY", "SECONDARY", rep(NA_character_, 3L)),
    primary_pid = c(NA_character_, "PRIMARY", rep(NA_character_, 3L))
  )
}

new_linked_connection <- function(custom = FALSE) {
  con <- gnaf_connect(":memory:")
  gnaf_init(con)
  suppressMessages(gnaf_add(con, linked_address_rows()))
  if (!custom) {
    DBI::dbExecute(con, "INSERT INTO gnaf_addresses SELECT * FROM custom_addresses")
    DBI::dbExecute(con, "UPDATE gnaf_addresses SET source = 'gnaf'")
    DBI::dbExecute(con, "DELETE FROM custom_addresses")
  }
  con
}

test_that("return options require a single non-missing logical value", {
  for (arg in c("return_principal", "return_primary")) {
    for (value in list(NA, NULL, logical(), c(TRUE, FALSE), 1L, "TRUE")) {
      args <- c(list(addresses = "x", con = NULL), stats::setNames(list(value), arg))
      expect_error(do.call(gnaf_match, args), paste0("'", arg, "' must be TRUE or FALSE"))
    }
  }
})

test_that("principal returns replace all address fields for each alias kind", {
  con <- new_linked_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  inputs <- linked_address_rows()$address_label[c(3L, 4L, 5L, 3L)]
  original <- gnaf_match(inputs, con, cache = FALSE, verbose = FALSE)
  out <- gnaf_match(inputs, con, return_principal = TRUE,
                    cache = FALSE, verbose = FALSE)
  target <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT * FROM gnaf_addresses WHERE address_detail_pid = 'SECONDARY'"))
  fields <- names(target)
  expect_equal(out[, ..fields], target[rep(1L, length(inputs))])
  expect_identical(out$matched_address_detail_pid, original$address_detail_pid)
  expect_identical(out$matched_address_label, original$address_label)
  scores_and_input <- grep("^(in_|input_|score_|total_score|match_rank|matched$|match_status)",
                           names(original), value = TRUE)
  expect_identical(out[, ..scores_and_input], original[, ..scores_and_input])
  expect_false("matched_address_detail_pid" %in% names(original))
})

test_that("primary returns are optional and compose after principal returns", {
  con <- new_linked_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  inputs <- linked_address_rows()$address_label[1:3]
  original <- gnaf_match(inputs, con, cache = FALSE, verbose = FALSE)
  expect_identical(original$address_detail_pid, c("PRIMARY", "SECONDARY", "ADDRESS_ALIAS"))
  primary <- gnaf_match(inputs, con, return_primary = TRUE,
                        cache = FALSE, verbose = FALSE)
  expect_identical(primary$address_detail_pid, c("PRIMARY", "PRIMARY", "ADDRESS_ALIAS"))
  both <- gnaf_match(inputs, con, return_principal = TRUE, return_primary = TRUE,
                     resolve_principal = TRUE, cache = FALSE, verbose = FALSE)
  target <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT * FROM gnaf_addresses WHERE address_detail_pid = 'PRIMARY'"))
  fields <- names(target)
  expect_equal(both[, ..fields], target[rep(1L, 3L)])
  expect_identical(both$matched_address_detail_pid, original$address_detail_pid)
  expect_identical(both$total_score, original$total_score)
  expect_identical(both$principal_address_label,
                   c(NA_character_, NA_character_, inputs[2L]))
})

test_that("returning linked records preserves separate ranked matches", {
  con <- new_linked_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  input <- linked_address_rows()$address_label[2L]
  original <- gnaf_match(input, con, max_results = 2L, cache = FALSE, verbose = FALSE)
  out <- gnaf_match(input, con, max_results = 2L, return_primary = TRUE,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, rep("PRIMARY", 2L))
  expect_identical(out$matched_address_detail_pid, original$address_detail_pid)
  expect_identical(out$match_rank, original$match_rank)
  expect_identical(out$total_score, original$total_score)
})

test_that("missing links and unmatched inputs retain their original fields and types", {
  con <- new_linked_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE gnaf_addresses SET principal_pid = CASE
    WHEN address_detail_pid = 'ADDRESS_ALIAS' THEN 'UNAVAILABLE'
    WHEN address_detail_pid = 'STREET_ALIAS' THEN '' ELSE NULL END,
    primary_pid = 'UNAVAILABLE'")
  inputs <- c(linked_address_rows()$address_label, NA_character_, "")
  original <- gnaf_match(inputs, con, cache = FALSE, verbose = FALSE)
  out <- gnaf_match(inputs, con, return_principal = TRUE, return_primary = TRUE,
                    cache = FALSE, verbose = FALSE)
  fields <- names(original)
  expect_identical(out[, ..fields], original)
  expect_identical(out$matched_address_detail_pid, original$address_detail_pid)
  expect_false(any(tail(out$matched, 2L)))
  missing <- gnaf_match(c(NA_character_, ""), con,
    return_principal = TRUE, return_primary = TRUE, cache = FALSE, verbose = FALSE)
  expect_identical(missing$matched_address_detail_pid, rep(NA_character_, 2L))
  expect_type(missing$postcode, "integer")
  expect_type(missing$longitude, "double")
})

test_that("return options keep cache entries tied to the original candidate", {
  con <- new_linked_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  # Deliberately misspelled (not just abbreviated) so it never equals a stored
  # address_label verbatim, even after standardisation - this must always
  # resolve via fuzzy scoring on the slow path, not the exact-label fast path,
  # so the second call below genuinely exercises the cache-lookup path.
  input <- "Unit 2 10 Olde Street, St Lucia QLD 4067"
  first <- gnaf_match(input, con, return_principal = TRUE, return_primary = TRUE,
                      verbose = FALSE)
  expect_identical(first$address_detail_pid, "PRIMARY")
  cached_pid <- DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_match_cache")$address_detail_pid
  expect_identical(cached_pid, "ADDRESS_ALIAS")
  DBI::dbExecute(con, "UPDATE gnaf_match_cache SET cached_at = '2026-01-01 00:00:00'")
  original <- gnaf_match(input, con, verbose = FALSE)
  expect_identical(original$address_detail_pid, "ADDRESS_ALIAS")
  cached <- gnaf_match(input, con, return_principal = TRUE, return_primary = TRUE,
                       verbose = FALSE)
  uncached <- gnaf_match(input, con, return_principal = TRUE, return_primary = TRUE,
                         cache = FALSE, verbose = FALSE)
  expect_equal(cached, uncached)
  expect_equal(as.Date(DBI::dbGetQuery(con,
    "SELECT cached_at FROM gnaf_match_cache")$cached_at), as.Date("2026-01-01"))
})

test_that("custom links are resolved and GNAF takes precedence for duplicate PIDs", {
  con <- new_linked_connection(custom = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  input <- linked_address_rows()$address_label[3L]
  out <- gnaf_match(input, con, return_principal = TRUE, return_primary = TRUE,
                    cache = FALSE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "PRIMARY")
  expect_identical(out$source, "custom")
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses SELECT * FROM custom_addresses
    WHERE address_detail_pid IN ('ADDRESS_ALIAS', 'PRIMARY')")
  DBI::dbExecute(con, "UPDATE gnaf_addresses SET source = 'gnaf'")
  excluded <- gnaf_match(input, con, include_custom = FALSE, return_principal = TRUE,
                         cache = FALSE, verbose = FALSE)
  expect_identical(excluded$address_detail_pid, "ADDRESS_ALIAS")
  included <- gnaf_match(input, con, return_principal = TRUE, return_primary = TRUE,
                         cache = FALSE, verbose = FALSE)
  expect_identical(included$address_detail_pid, "PRIMARY")
  expect_identical(included$source, "gnaf")
})

test_that("linked returns work on read-only databases with compact relationship flags", {
  path <- tempfile(fileext = ".duckdb")
  con <- gnaf_connect(path)
  gnaf_init(con)
  suppressMessages(gnaf_add(con, linked_address_rows()))
  DBI::dbExecute(con, "UPDATE custom_addresses SET alias_type = NULL,
    alias_principal = CASE WHEN principal_pid IS NULL THEN 'P' ELSE 'A' END,
    primary_secondary = CASE WHEN primary_pid IS NULL THEN 'P' ELSE 'S' END")
  gnaf_disconnect(con)
  on.exit(unlink(path), add = TRUE)
  con <- gnaf_connect(path, read_only = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  out <- gnaf_match(linked_address_rows()$address_label[3L], con,
                    return_principal = TRUE, return_primary = TRUE, verbose = FALSE)
  expect_identical(out$address_detail_pid, "PRIMARY")
})
