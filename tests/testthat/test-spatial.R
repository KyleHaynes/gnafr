lookup_shapes <- function() {
  polygon <- function(xmin, xmax) sf::st_polygon(list(matrix(
    c(xmin, 0, xmax, 0, xmax, 2, xmin, 2, xmin, 0), ncol = 2L, byrow = TRUE)))
  sf::st_sf(
    area = c("west", "east"), code = c(7L, 8L),
    date = as.Date(c("2026-01-01", "2026-02-01")),
    category = factor(c("A", "B")),
    geometry = sf::st_sfc(polygon(0, 2), polygon(1, 3), crs = 4326)
  )
}

test_that("spatial first matches preserve attribute types and caller columns", {
  shapes <- lookup_shapes()
  points <- data.table::data.table(
    .point_id = letters[1:5], longitude = c(NA, 0.5, 1.5, 4, Inf), latitude = 1
  )
  original <- data.table::copy(points)
  out <- spatial_lookup(points, shapes, chunk_size = 1L, verbose = FALSE)
  expect_identical(points, original)
  expect_identical(out$.point_id, points$.point_id)
  expect_identical(out$area, c(NA_character_, "west", "west", NA_character_, NA_character_))
  expect_identical(out$code, c(NA_integer_, 7L, 7L, NA_integer_, NA_integer_))
  expect_identical(out$date, shapes$date[c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_)])
  expect_identical(out$category, shapes$category[c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_)])
})

test_that("spatial all matches retain missing and unmatched points in order", {
  points <- data.table::data.table(id = 1:3, longitude = c(1.5, NA, 4), latitude = 1)
  out <- spatial_lookup(points, lookup_shapes(), multiple = "all", verbose = FALSE)
  expect_identical(out$id, c(1L, 1L, 2L, 3L))
  expect_identical(out$area, c("west", "east", NA_character_, NA_character_))
  expect_identical(out$code, c(7L, 8L, NA_integer_, NA_integer_))
  points_only <- spatial_lookup(points, lookup_shapes(), multiple = "all",
                               return_cols = character(), verbose = FALSE)
  expect_identical(points_only$id, out$id)
  expect_named(points_only, names(points))
})

test_that("empty spatial lookups preserve their schema and validate chunk sizes", {
  points <- data.table::data.table(id = integer(), longitude = numeric(), latitude = numeric())
  shapes <- lookup_shapes()
  out <- spatial_lookup(points, shapes, verbose = FALSE)
  expect_named(out, c(names(points), "area", "code", "date", "category"))
  expect_identical(out$code, integer())
  expect_identical(out$date, as.Date(character()))
  expect_error(spatial_lookup(points, shapes, chunk_size = 0), "chunk_size")
  expect_error(spatial_lookup(points, shapes, chunk_size = 1.5), "chunk_size")
})

test_that("the interactive boundaries plot works without an attached pipe package", {
  out <- plot_boundaries_heatmap(lookup_shapes(), use_leaflet = TRUE, verbose = FALSE)
  expect_s3_class(out, "leaflet")
})

test_that("duplicate coordinates preserve overlap order, missing rows and input ownership", {
  shapes <- lookup_shapes()
  points <- data.table::data.table(
    id = 1:7, longitude = c(1.5, 0.5, 1.5, NA, 4, 0.5, 181), latitude = 1)
  before <- data.table::copy(points)
  for (multiple in c("first", "all")) {
    one_at_a_time <- spatial_lookup(points, shapes, multiple = multiple,
                                    chunk_size = 1L, verbose = FALSE)
    batched <- spatial_lookup(points, shapes, multiple = multiple,
                              chunk_size = 7L, verbose = FALSE)
    all_at_once <- spatial_lookup(points, shapes, multiple = multiple,
                                  chunk_size = NULL, verbose = FALSE)
    expect_identical(batched, one_at_a_time)
    expect_identical(all_at_once, one_at_a_time)
    expect_identical(points, before)
  }
})

test_that("input CRS supports projected coordinates and rejects missing datums", {
  shapes <- lookup_shapes()
  lonlat <- data.table::data.table(longitude = c(0.5, 1.5, 4), latitude = 1)
  projected <- sf::st_transform(sf::st_as_sf(lonlat,
    coords = c("longitude", "latitude"), crs = 4326), 3857)
  xy <- data.table::as.data.table(sf::st_coordinates(projected))
  out <- spatial_lookup(xy, shapes, lon = "X", lat = "Y", points_crs = 3857,
                         verbose = FALSE)
  expect_identical(out$area, c("west", "west", NA_character_))
  expect_identical(out$X, xy$X)
  expect_error(spatial_lookup(lonlat, shapes, points_crs = NA), "known CRS")
})

test_that("empty polygons preserve original polygon attribute indexing", {
  shapes <- lookup_shapes()
  empty <- shapes[1L, ]
  sf::st_geometry(empty) <- sf::st_sfc(sf::st_polygon(), crs = 4326)
  shapes <- rbind(empty, shapes)
  points <- data.table::data.table(longitude = c(0.5, 2.5), latitude = 1)
  out <- spatial_lookup(points, shapes, verbose = FALSE)
  expect_identical(out$code, c(7L, 8L))
  out <- spatial_lookup(points, empty, verbose = FALSE)
  expect_identical(out$code, c(NA_integer_, NA_integer_))
  out <- spatial_lookup(points, shapes[0L, ], verbose = FALSE)
  expect_identical(out$area, c(NA_character_, NA_character_))
})

test_that("quiet shapefile reads suppress metadata messages", {
  path <- tempfile(fileext = ".shp")
  sf::st_write(lookup_shapes()[, "area", drop = FALSE], path, quiet = TRUE)
  on.exit(unlink(paste0(tools::file_path_sans_ext(path),
                       c(".shp", ".shx", ".dbf", ".prj"))), add = TRUE)
  expect_message(out <- read_shapefile(path, quiet = TRUE), NA)
  expect_identical(out$area, c("west", "east"))
})

new_spatial_connection <- function(path = ":memory:") {
  con <- gnaf_connect(path)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, longitude, latitude) VALUES
    ('A', 0.5, 1), ('B', 1.5, 1), ('C', 0.5, 1),
    ('MISSING', NULL, 1), ('OUTSIDE', 4, 1), ('INVALID', 181, 1)")
  con
}

test_that("database spatial enrichment retains every PID and typed attributes", {
  con <- new_spatial_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  before <- DBI::dbGetQuery(con, "SELECT * FROM gnaf_addresses ORDER BY address_detail_pid")
  n <- gnaf_add_spatial(con, lookup_shapes(), "SA2 demo's table",
                         return_cols = c("area", "code", "date"), verbose = FALSE)
  expect_equal(n, 6)
  out <- DBI::dbReadTable(con, "SA2 demo's table")
  out <- out[match(before$address_detail_pid, out$address_detail_pid), ]
  expect_identical(out$area, c("west", "west", "west", NA_character_, NA_character_, NA_character_))
  expect_identical(out$code, c(7L, 7L, 7L, NA_integer_, NA_integer_, NA_integer_))
  expect_s3_class(out$date, "Date")
  expect_identical(DBI::dbGetQuery(con,
    "SELECT * FROM gnaf_addresses ORDER BY address_detail_pid"), before)
  expect_error(gnaf_add_spatial(con, lookup_shapes(), "SA2 demo's table"), "already exists")
  expect_setequal(DBI::dbListTables(con), c("gnaf_addresses", "custom_addresses",
    "gnaf_locality_index", "gnaf_street_type_index", "gnaf_match_cache", "SA2 demo's table"))
  DBI::dbRemoveTable(con, "SA2 demo's table")
  expect_identical(DBI::dbGetQuery(con,
    "SELECT * FROM gnaf_addresses ORDER BY address_detail_pid"), before)
})

test_that("database spatial enrichment handles empty and all-missing coordinates", {
  con <- new_spatial_connection()
  on.exit(gnaf_disconnect(con), add = TRUE)
  expect_equal(gnaf_add_spatial(con, lookup_shapes(), "empty_sa2",
    address_table = "custom_addresses", return_cols = "code", verbose = FALSE), 0)
  expect_identical(DBI::dbReadTable(con, "empty_sa2")$code, integer())
  DBI::dbExecute(con, "UPDATE gnaf_addresses SET longitude = NULL")
  expect_equal(gnaf_add_spatial(con, lookup_shapes(), "missing_sa2",
    return_cols = "code", verbose = FALSE), 6)
  expect_identical(DBI::dbReadTable(con, "missing_sa2")$code, rep(NA_integer_, 6L))
  expect_error(gnaf_add_spatial(con, lookup_shapes(), "bad", return_cols = character()),
               "at least one polygon attribute")
  expect_error(gnaf_add_spatial(con, lookup_shapes(), "bad", return_cols = "missing"),
               "distinct columns")
  expect_error(gnaf_add_spatial(con, lookup_shapes(), NA_character_), "table name")
})

test_that("a failed database enrichment removes staging tables and preserves addresses", {
  path <- tempfile(fileext = ".duckdb")
  con <- new_spatial_connection(path)
  gnaf_disconnect(con)
  on.exit(unlink(path), add = TRUE)
  con <- gnaf_connect(path, read_only = TRUE)
  on.exit(gnaf_disconnect(con), add = TRUE)
  tables <- DBI::dbListTables(con)
  expect_error(gnaf_add_spatial(con, lookup_shapes(), "sa2", return_cols = "code",
                                verbose = FALSE), "read.only")
  expect_setequal(DBI::dbListTables(con), tables)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM gnaf_addresses")$n, 6)
})
