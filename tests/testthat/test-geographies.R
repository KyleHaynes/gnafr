geography_shapes <- function() {
  square <- function(x) sf::st_polygon(list(matrix(
    c(x, 0, x + 1, 0, x + 1, 2, x, 2, x, 0), ncol = 2L, byrow = TRUE)))
  sf::st_sf(code = c("WEST", "EAST"), title = c("West area", "East area"),
    effective = as.Date(c("2021-01-01", "2021-02-01")),
    geometry = sf::st_sfc(square(0), square(2), crs = 4326))
}

geography_connection <- function(path = ":memory:") {
  con <- gnaf_connect(path)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, address_label, number_first, street_name, street_type,
     locality_name, state, postcode, longitude, latitude, source,
     alias_type, principal_pid, primary_pid, flat_type, flat_number) VALUES
    ('PRIMARY', '10 MAIN ROAD, BRISBANE QLD 4000', 10, 'MAIN', 'ROAD',
      'BRISBANE', 'QLD', 4000, 2.5, 1, 'gnaf', NULL, NULL, NULL, NULL, NULL),
    ('UNIT', 'UNIT 2 10 MAIN ROAD, BRISBANE QLD 4000', 10, 'MAIN', 'ROAD',
      'BRISBANE', 'QLD', 4000, 0.5, 1, 'gnaf', NULL, NULL, 'PRIMARY', 'UNIT', '2'),
    ('ALIAS', '10 OLD ROAD, BRISBANE QLD 4000', 10, 'OLD', 'ROAD',
      'BRISBANE', 'QLD', 4000, NULL, NULL, 'gnaf', 'ADDRESS:SYN', 'UNIT', NULL, NULL, NULL),
    ('VOID', '99 LOST ROAD, BRISBANE QLD 4000', 99, 'LOST', 'ROAD',
      'BRISBANE', 'QLD', 4000, NULL, NULL, 'gnaf', NULL, NULL, NULL, NULL, NULL)")
  con
}

test_that("geographies can be added, listed, measured and removed without modifying addresses", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  before <- DBI::dbReadTable(con, "gnaf_addresses")
  expect_equal(nrow(gnaf_list_geographies(con)), 0L)
  expect_equal(nrow(gnaf_geography_coverage(con)), 0L)
  shapes <- geography_shapes()
  original_shapes <- shapes
  n <- gnaf_add_geography(con, "SA2_2021", shapes,
    return_cols = c(sa2_code = "code", sa2_name = "title", start_date = "effective"),
    verbose = FALSE)
  expect_equal(n, 4)
  expect_identical(shapes, original_shapes)
  listed <- gnaf_list_geographies(con)
  expect_identical(listed$name, "sa2_2021")
  expect_identical(listed$points_crs, "EPSG:4326")
  expect_identical(listed$columns[[1L]], c("sa2_code", "sa2_name", "start_date"))
  expect_true(listed$available)
  coverage <- gnaf_geography_coverage(con, "SA2_2021")
  expect_identical(coverage$non_missing, rep(2, 3L))
  expect_identical(coverage$coverage_pct, rep(50, 3L))
  expect_identical(coverage$stored_rows, rep(4, 3L))
  expect_error(gnaf_add_geography(con, "sa2_2021", shapes), "already registered")
  gnaf_remove_geography(con, "SA2_2021")
  expect_false(DBI::dbExistsTable(con, "gnaf_geo_sa2_2021"))
  expect_equal(nrow(gnaf_list_geographies(con)), 0L)
  expect_identical(DBI::dbReadTable(con, "gnaf_addresses"), before)
})

test_that("existing enrichment tables can be adopted without a spatial rebuild", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_add_spatial(con, geography_shapes(), "gnaf_sa2_2021", return_cols = "code", verbose = FALSE)
  before <- DBI::dbReadTable(con, "gnaf_sa2_2021")
  expect_no_error(gnaf_register_geography(con, "sa2", "gnaf_sa2_2021", points_crs = 4326))
  expect_no_error(gnaf_register_geography(con, "sa2", "gnaf_sa2_2021", points_crs = 4326))
  expect_identical(DBI::dbReadTable(con, "gnaf_sa2_2021"), before)
  expect_error(gnaf_register_geography(con, "other", "gnaf_sa2_2021"), "already registered")
  expect_error(gnaf_register_geography(con, "other", "GNAF_SA2_2021"), "already registered")
  expect_error(gnaf_register_geography(con, "core", "gnaf_addresses"), "source or system")
  DBI::dbRemoveTable(con, "gnaf_sa2_2021")
  expect_false(gnaf_list_geographies(con)$available)
  expect_error(gnaf_geography_coverage(con, "sa2"), "table is missing")
  expect_no_error(gnaf_remove_geography(con, "sa2"))
})

test_that("failed registration during creation rolls back the new geography table", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_add_spatial(con, geography_shapes(), "gnaf_geo_new", return_cols = "code", verbose = FALSE)
  gnaf_register_geography(con, "old", "gnaf_geo_new")
  DBI::dbRemoveTable(con, "gnaf_geo_new")
  tables <- DBI::dbListTables(con)
  expect_error(gnaf_add_geography(con, "new", geography_shapes(),
                                  return_cols = "code", verbose = FALSE), "already registered")
  expect_setequal(DBI::dbListTables(con), tables)
  expect_identical(gnaf_list_geographies(con)$name, "old")
})

test_that("temporary tables and views cannot be registered as persistent geographies", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TEMP TABLE transient AS
    SELECT address_detail_pid, state FROM gnaf_addresses")
  DBI::dbExecute(con, "CREATE VIEW geo_view AS
    SELECT address_detail_pid, state FROM gnaf_addresses")
  expect_error(gnaf_register_geography(con, "temporary", "transient"), "persistent")
  expect_error(gnaf_register_geography(con, "view", "geo_view"), "persistent")
  expect_false(DBI::dbExistsTable(con, "gnaf_geographies"))
})

test_that("joins preserve order, repeated and unmatched rows, types and caller ownership", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_add_geography(con, "sa2", geography_shapes(),
    return_cols = c(sa2_code = "code", start_date = "effective"), verbose = FALSE)
  results <- data.table::data.table(id = 1:5,
    address_detail_pid = c("UNIT", "PRIMARY", "UNIT", NA_character_, "PRIMARY"),
    matched = c(TRUE, TRUE, TRUE, FALSE, FALSE), source = "gnaf")
  before <- data.table::copy(results)
  out <- gnaf_join_geographies(results, con, "sa2")
  expect_identical(out$id, 1:5)
  expect_identical(out$sa2_code, c("WEST", "EAST", "WEST", NA_character_, NA_character_))
  expect_s3_class(out$start_date, "Date")
  expect_identical(results, before)
  empty <- gnaf_join_geographies(results[0L], con, "sa2")
  expect_equal(nrow(empty), 0L)
  expect_identical(empty$sa2_code, character())
  expect_s3_class(empty$start_date, "Date")
  expect_identical(gnaf_join_geographies(results, con, NULL), results)
  expect_error(gnaf_join_geographies(out, con, "sa2"), "overlap")
})

test_that("match geographies follow principal and primary resolution including cached matches", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_add_geography(con, "sa2", geography_shapes(), return_cols = c(sa2_code = "code"), verbose = FALSE)
  input <- "10 Old Rd, Brisbane QLD 4000"
  original <- gnaf_match(input, con, verbose = FALSE)
  alias <- gnaf_match(input, con, geographies = "sa2", verbose = FALSE)
  unit <- gnaf_match(input, con, geographies = TRUE, return_principal = TRUE, verbose = FALSE)
  primary <- gnaf_match(input, con, geographies = "sa2",
    return_principal = TRUE, return_primary = TRUE, verbose = FALSE)
  expect_identical(original$address_detail_pid, "ALIAS")
  expect_identical(alias$sa2_code, NA_character_)
  expect_identical(unit$sa2_code, "WEST")
  expect_identical(primary$sa2_code, "EAST")
  expect_identical(primary$total_score, original$total_score)
  expect_identical(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_match_cache")$address_detail_pid, "ALIAS")
  DBI::dbExecute(con, "UPDATE gnaf_geo_sa2 SET sa2_code = 'UPDATED' WHERE address_detail_pid = 'PRIMARY'")
  refreshed <- gnaf_match(input, con, geographies = "sa2",
    return_principal = TRUE, return_primary = TRUE, verbose = FALSE)
  expect_identical(refreshed$sa2_code, "UPDATED")
  uncached <- gnaf_match(input, con, geographies = "sa2", cache = FALSE,
    return_principal = TRUE, return_primary = TRUE, verbose = FALSE)
  expect_equal(refreshed, uncached)
  unmatched <- gnaf_match(NA_character_, con, geographies = "sa2", verbose = FALSE)
  expect_false(unmatched$matched)
  expect_identical(unmatched$sa2_code, NA_character_)
  expect_error(gnaf_match(input, con, geographies = "unknown"), "Unknown geography")
})

test_that("multiple geographies use their registered sources and reject ambiguous columns", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO custom_addresses (address_detail_pid, longitude, latitude)
    VALUES ('PRIMARY', 0.5, 1)")
  gnaf_add_geography(con, "gnaf_area", geography_shapes(), return_cols = c(gnaf_area = "code"), verbose = FALSE)
  gnaf_add_geography(con, "custom_area", geography_shapes(),
    return_cols = c(custom_area = "code"), address_table = "custom_addresses", verbose = FALSE)
  input <- data.table::data.table(address_detail_pid = c("PRIMARY", "PRIMARY"),
                                  source = c("gnaf", "custom"))
  out <- gnaf_join_geographies(input, con, TRUE)
  expect_identical(out$gnaf_area, c("EAST", NA_character_))
  expect_identical(out$custom_area, c(NA_character_, "WEST"))
  gnaf_add_geography(con, "conflicting", geography_shapes(),
    return_cols = c(gnaf_area = "title"), verbose = FALSE)
  expect_error(gnaf_join_geographies(input, con, TRUE), "overlap")
})

test_that("coverage uses current addresses and reports missing attributes and orphaned rows", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_add_geography(con, "sa2", geography_shapes(), return_cols = c("code", "title"), verbose = FALSE)
  DBI::dbExecute(con, "UPDATE gnaf_geo_sa2 SET title = NULL WHERE address_detail_pid = 'UNIT'")
  DBI::dbExecute(con, "DELETE FROM gnaf_addresses WHERE address_detail_pid = 'ALIAS'")
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses (address_detail_pid) VALUES ('NEW')")
  out <- gnaf_geography_coverage(con)
  expect_identical(out$stored_rows, c(3, 3))
  expect_identical(out$address_rows, c(4, 4))
  expect_identical(out$orphaned_rows, c(1, 1))
  expect_identical(out$non_missing, c(2, 1))
  expect_identical(out$coverage_pct, c(50, 25))
  gnaf_add_geography(con, "empty", geography_shapes(), address_table = "custom_addresses",
    return_cols = "code", verbose = FALSE)
  expect_identical(gnaf_geography_coverage(con, "empty")$coverage_pct, NA_real_)
})

test_that("invalid keys are rejected and failed registration leaves no catalog", {
  con <- geography_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE bad_geo AS SELECT 'UNIT' AS address_detail_pid, 'A' AS code
    UNION ALL SELECT 'UNIT', 'B'")
  expect_error(gnaf_register_geography(con, "bad", "bad_geo"), "duplicate")
  expect_false(DBI::dbExistsTable(con, "gnaf_geographies"))
  DBI::dbExecute(con, "DELETE FROM bad_geo")
  DBI::dbExecute(con, "INSERT INTO bad_geo VALUES (NULL, 'A')")
  expect_error(gnaf_register_geography(con, "bad", "bad_geo"), "missing")
  DBI::dbExecute(con, "DELETE FROM bad_geo")
  DBI::dbExecute(con, "INSERT INTO bad_geo VALUES ('UNIT', 'A')")
  gnaf_register_geography(con, "bad", "bad_geo")
  DBI::dbExecute(con, "INSERT INTO bad_geo VALUES ('UNIT', 'B')")
  input <- data.table::data.table(address_detail_pid = "UNIT")
  tables <- DBI::dbListTables(con)
  expect_error(gnaf_join_geographies(input, con, "bad"), "duplicate")
  expect_setequal(DBI::dbListTables(con), tables)
  expect_error(gnaf_join_geographies(input, con, NA), "geographies")
  expect_error(gnaf_remove_geography(con, "unknown"), "Unknown geography")
})

test_that("geography listing, coverage and matching work after reopening read-only", {
  path <- tempfile(fileext = ".duckdb")
  con <- geography_connection(path)
  gnaf_add_geography(con, "sa2", geography_shapes(), return_cols = "code", verbose = FALSE)
  gnaf_disconnect(con)
  on.exit(unlink(path), add = TRUE)
  con <- gnaf_connect(path, read_only = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_identical(gnaf_list_geographies(con)$name, "sa2")
  expect_identical(gnaf_geography_coverage(con)$coverage_pct, 50)
  out <- gnaf_match("10 Main Rd, Brisbane QLD 4000", con,
    geographies = "sa2", return_primary = TRUE, verbose = FALSE)
  expect_identical(out$code, "EAST")
  expect_error(gnaf_remove_geography(con, "sa2"), "read.only")
  expect_true(DBI::dbExistsTable(con, "gnaf_geo_sa2"))
  expect_identical(gnaf_list_geographies(con)$name, "sa2")
})
