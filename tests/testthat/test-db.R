test_that("connections reject a cached mode mismatch and allow explicit reopening", {
  path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(path), add = TRUE)
  writer <- gnaf_connect(path)
  DBI::dbExecute(writer, "CREATE TABLE demo (id INTEGER)")
  expect_error(gnaf_connect(path, read_only = TRUE), "already open.*read-write")
  expect_equal(DBI::dbGetQuery(writer, "SELECT count(*) AS n FROM demo")$n, 0)
  gnaf_disconnect(writer)

  reader <- gnaf_connect(path, read_only = TRUE)
  expect_error(gnaf_connect(path), "already open.*read-only")
  expect_error(DBI::dbRemoveTable(reader, "demo"), "read.only")
  gnaf_disconnect(reader)

  writer <- gnaf_connect(path, read_only = FALSE)
  on.exit(gnaf_disconnect(writer), add = TRUE)
  expect_no_error(DBI::dbRemoveTable(writer, "demo"))
  expect_false(DBI::dbExistsTable(writer, "demo"))
})

test_that("shutdown releases overwritten legacy connections after a failed DROP", {
  path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(path), add = TRUE)
  con <- gnaf_connect(path)
  DBI::dbExecute(con, "CREATE TABLE demo (id INTEGER)")
  gnaf_disconnect(con)
  # Reproduce the old helper's silently reused read-only instance and lost handle.
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = FALSE)
  expect_error(DBI::dbRemoveTable(con, "demo"), "read.only")
  gnaf_disconnect(con)
  con <- gnaf_connect(path, read_only = FALSE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_no_error(DBI::dbRemoveTable(con, "demo"))
})

test_that("disconnecting without shutdown preserves other shared connections", {
  path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(path), add = TRUE)
  first <- gnaf_connect(path)
  second <- gnaf_connect(path)
  on.exit(gnaf_disconnect(second), add = TRUE)
  gnaf_disconnect(first, shutdown = FALSE)
  expect_no_error(DBI::dbExecute(second, "CREATE TABLE demo (id INTEGER)"))
  expect_true(DBI::dbExistsTable(second, "demo"))
})

test_that("initialising a legacy schema restores address fields and versions its cache", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  for (index in c("idx_gnaf_pc", "idx_cust_pc", "idx_gnaf_label", "idx_cust_label"))
    DBI::dbExecute(con, paste("DROP INDEX", index))
  for (table in c("gnaf_addresses", "custom_addresses")) {
    for (column in c("address_site_name", "level_type", "level_number", "lot_number"))
      DBI::dbExecute(con, sprintf("ALTER TABLE %s DROP COLUMN %s", table, column))
  }
  DBI::dbExecute(con, "ALTER TABLE gnaf_match_cache DROP COLUMN algorithm_version")
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score) VALUES ('old', 'A', 100)")
  gnaf_init(con)
  for (table in c("gnaf_addresses", "custom_addresses"))
    expect_true(all(c("address_site_name", "level_type", "level_number", "lot_number") %in%
      DBI::dbListFields(con, table)))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT algorithm_version FROM gnaf_match_cache")$algorithm_version, 1L)
  expect_no_error(gnaf_init(con))
})
