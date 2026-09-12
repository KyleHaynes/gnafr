# gnafr example usage

A growing set of examples for the presentation. Start with the small demo below
to show exactly how an alias, a non-alias address and a primary address relate.

## Demo: alias to non-alias to primary

These are synthetic records for demonstrating the options, not real GNAF data.
The demo uses a temporary in-memory database and needs no downloaded dataset.
Run the R blocks in this section in order with the updated package installed
(or run `devtools::load_all()` from this repository first).

```r
library(gnafr)
library(data.table)

demo_con <- gnaf_connect(":memory:")
gnaf_init(demo_con)

# OLD ROAD is an alias for a unit on MAIN ROAD.
# The unit is a secondary address linked to the main property's primary address.
demo_addresses <- data.table(
  address_detail_pid = c("DEMO_PRIMARY", "DEMO_UNIT", "DEMO_ALIAS"),
  address_label = c(
    "10 MAIN ROAD, BRISBANE QLD 4000",
    "UNIT 2 10 MAIN ROAD, BRISBANE QLD 4000",
    "UNIT 2 10 OLD ROAD, BRISBANE QLD 4000"
  ),
  number_first = 10L,
  street_name = c("MAIN", "MAIN", "OLD"),
  street_type = "ROAD",
  locality_name = "BRISBANE",
  state = "QLD",
  postcode = 4000L,
  flat_type = c(NA_character_, "UNIT", "UNIT"),
  flat_number = c(NA_character_, "2", "2"),
  longitude = c(153.0200, 153.0201, 153.0199),
  latitude = c(-27.4700, -27.4701, -27.4699),
  alias_type = c(NA_character_, NA_character_, "ADDRESS:RA"),
  alias_principal = c("PRINCIPAL", "PRINCIPAL", "ALIAS"),
  principal_pid = c(NA_character_, NA_character_, "DEMO_UNIT"),
  primary_secondary = c("PRIMARY", "SECONDARY", NA_character_),
  primary_pid = c(NA_character_, "DEMO_PRIMARY", NA_character_)
)

gnaf_add(demo_con, demo_addresses)
alias_input <- "Unit 2 10 Old Rd, Brisbane QLD 4000"
unit_input <- "Unit 2 10 Main Rd, Brisbane QLD 4000"
```

With the defaults, the result describes the address that matched the input.

```r
as_matched <- gnaf_match(alias_input, demo_con, verbose = FALSE)
as_matched[, .(address_detail_pid, address_label, alias_principal, total_score)]
# address_detail_pid: DEMO_ALIAS
```

Use `return_principal = TRUE` to return the non-alias address. It still describes
the same unit, with the canonical street and that record's coordinates.

```r
canonical <- gnaf_match(
  alias_input, demo_con, return_principal = TRUE, verbose = FALSE
)
canonical[, .(
  matched_address_detail_pid, matched_address_label,
  address_detail_pid, address_label, flat_number, longitude, latitude
)]
# matched_address_detail_pid: DEMO_ALIAS
# address_detail_pid: DEMO_UNIT
# address_label: UNIT 2 10 MAIN ROAD, BRISBANE QLD 4000
```

Use `return_primary = TRUE` to return the main property's address when a secondary
address matches. Its address components replace the unit's components, so the
returned `flat_number` is missing in this example.

```r
primary <- gnaf_match(
  unit_input, demo_con, return_primary = TRUE, verbose = FALSE
)
primary[, .(
  matched_address_detail_pid, address_detail_pid, address_label,
  primary_secondary, flat_number, longitude, latitude
)]
# matched_address_detail_pid: DEMO_UNIT
# address_detail_pid: DEMO_PRIMARY
# address_label: 10 MAIN ROAD, BRISBANE QLD 4000
```

Enable both options to follow the complete relationship:
`DEMO_ALIAS -> DEMO_UNIT -> DEMO_PRIMARY`.

```r
canonical_primary <- gnaf_match(
  alias_input, demo_con,
  return_principal = TRUE,
  return_primary = TRUE,
  verbose = FALSE
)
canonical_primary[, .(
  matched_address_detail_pid, matched_address_label,
  address_detail_pid, address_label,
  matched_longitude, matched_latitude, longitude, latitude,
  alias_principal, primary_secondary, total_score
)]
# matched_address_detail_pid: DEMO_ALIAS
# address_detail_pid: DEMO_PRIMARY

# A compact comparison to show in the presentation.
comparison <- rbindlist(list(
  as_matched[, .(option = "Defaults", address_detail_pid, address_label)],
  canonical[, .(option = "return_principal", address_detail_pid, address_label)],
  primary[, .(option = "return_primary (unit input)", address_detail_pid, address_label)],
  canonical_primary[, .(option = "Both (alias input)", address_detail_pid, address_label)]
))
comparison[]

gnaf_disconnect(demo_con)
```

## Use an existing GNAF database

Change the path and inputs to your own data. The database must contain the linked
records and `principal_pid` / `primary_pid` values for resolution to work. A
principal is the non-alias address; it can itself be a secondary/unit address.

```r
library(gnafr)
library(data.table)

con <- gnaf_connect("C:/temp/gnafx23.duckdb", read_only = TRUE)
```

### A real alias, sitting next to its principal and primary

Before matching anything, it's worth looking at what an alias relationship
actually looks like once real GNAF data is loaded. This unit in Nundah, QLD has
three linked records in `gnaf_addresses`: an alias under a different street name
(`IVO STREET`), the principal (non-alias) record for that same unit under its
real street name (`WALLY STREET`), and the primary property the unit belongs to.

```r
chain <- setDT(DBI::dbGetQuery(con, "
  SELECT address_detail_pid, address_label, alias_type, alias_principal, primary_secondary, principal_pid, primary_pid
  FROM gnaf_addresses
  WHERE address_detail_pid IN ('GAQLD155672969', 'GAQLD158440593', 'GAQLD156981590')
  ORDER BY address_detail_pid
"))
chain[]
#    address_detail_pid                           address_label  alias_type alias_principal primary_secondary  principal_pid    primary_pid
#                <char>                                  <char>      <char>          <char>            <char>         <char>         <char>
# 1:     GAQLD155672969          27 IVO STREET, NUNDAH QLD 4012 ADDRESS:SYN           ALIAS                 S GAQLD158440593 GAQLD156981590
# 2:     GAQLD156981590        16 WALLY STREET, NUNDAH QLD 4012        <NA>       PRINCIPAL                 P           <NA>           <NA>
# 3:     GAQLD158440593 UNIT 2 16 WALLY STREET, NUNDAH QLD 4012        <NA>       PRINCIPAL                 S           <NA> GAQLD156981590
```

`27 IVO STREET` (`GAQLD155672969`) is a synonym alias (`ADDRESS:SYN`) whose
`principal_pid` points at `UNIT 2 16 WALLY STREET` (`GAQLD158440593`) - the real,
non-alias record for the same unit. That unit is in turn a secondary address
(`primary_secondary = "S"`) linked to the primary property `16 WALLY STREET`
(`GAQLD156981590`) via its own `primary_pid`. Same `ALIAS -> PRINCIPAL -> PRIMARY`
shape as the synthetic demo above, on a real GNAF record.

### Match the alias, then choose what comes back

```r
alias_input <- "27 Ivo Stret, Nundahh QLD 4012"

# Defaults: describes the address that actually matched - the alias itself.
as_matched <- gnaf_match(alias_input, con, verbose = FALSE)
as_matched[, .(alias_input, address_detail_pid, address_label, alias_principal, total_score)]
# address_detail_pid: GAQLD155672969 (27 IVO STREET, NUNDAH QLD 4012)

# return_principal = TRUE: same unit, described under its real street name.
canonical <- gnaf_match(alias_input, con, return_principal = TRUE, verbose = FALSE)
canonical[, .(
  alias_input, matched_address_detail_pid, matched_address_label,
  address_detail_pid, address_label
)]
# address_detail_pid: GAQLD158440593 (UNIT 2 16 WALLY STREET, NUNDAH QLD 4012)

# Add return_primary = TRUE: the primary property instead of the unit.
canonical_primary <- gnaf_match(
  alias_input, con, return_principal = TRUE, return_primary = TRUE, verbose = FALSE
)
canonical_primary[, .(
  matched_address_detail_pid, address_detail_pid, address_label, primary_secondary
)]
# address_detail_pid: GAQLD156981590 (16 WALLY STREET, NUNDAH QLD 4012)
```

### Batch match your own addresses

```r
addresses <- c(
  "10 110-120 Musgrave Road, Red Hill QLD 4059",
  "10 St James Ct, Tamborine Mountain QLD 4272"
)

# Keep unit addresses, but return canonical details for any alias matches.
results <- gnaf_match(addresses, con, return_principal = TRUE)
results[, .(
  input_raw, matched_address_label, address_label,
  longitude, latitude, matched, total_score
)]

# Return primary property details, including when the input matched an alias.
properties <- gnaf_match(
  addresses, con, return_principal = TRUE, return_primary = TRUE
)
properties[, .(
  input_raw, matched_address_detail_pid, address_detail_pid,
  address_label, primary_secondary, longitude, latitude
)]

# Keep the matched address and add selected canonical fields alongside it.
alongside <- gnaf_match(addresses, con, resolve_principal = TRUE)
alongside[, .(input_raw, address_label, principal_address_label)]

gnaf_disconnect(con)
```

## How the options affect the output

| Option | Default | Effect |
|--------|---------|--------|
| `return_principal` | `FALSE` | Replace address fields using `principal_pid`. |
| `return_primary` | `FALSE` | Replace address fields using `primary_pid`, after principal resolution if enabled. |
| `resolve_principal` | `FALSE` | Add selected `principal_*` fields alongside the original match. |
| `include_aliases` | `TRUE` | Allow aliases to participate in matching. |

The return options replace the complete address record: PID, label, components,
coordinates, parcel/mesh-block information and relationship fields. When a link
is missing or its target is absent from the database, the current address stays
as it is. Street-only matches usually have no principal link. Unmatched inputs
remain unmatched. Custom records can also be linked when `include_custom = TRUE`;
GNAF takes precedence if a target PID exists in both tables.

Aliases compete with core addresses regardless of `fallback_threshold`. Returning
the principal or primary happens after ranking, even for a perfect match. The
threshold controls locality fallback; it does not control relationship lookups.

Scores and ranks describe the original candidate. Every original address field
is preserved with a `matched_` prefix whenever either return option is enabled:
`matched_address_detail_pid`, `matched_address_label`, `matched_longitude`,
`matched_latitude`, `matched_principal_pid`, `matched_primary_pid`, and the other
address components. Unprefixed fields describe the returned address.
`gnaf_text_scores()` also compares the input with the original matched label.
Cached matches retain the original candidate too, so these
options can be changed between calls. With `max_results > 1`, multiple candidates
can resolve to the same address; their rows and original ranks are retained.

```r
# Even with locality fallback disabled, resolve a strong alias match.
con <- gnaf_connect("C:/temp/gnafx23.duckdb", read_only = TRUE)
comparison <- gnaf_match(
  "27 Ivo Street, Nundah QLD 4012", con,
  fallback_threshold = 0L, locality_fallback = FALSE,
  return_principal = TRUE, return_primary = TRUE
)
comparison[, .(
  input_raw, total_score,
  matched_address_label, address_label,
  matched_longitude, longitude, matched_latitude, latitude,
  matched_principal_pid, matched_primary_pid, address_detail_pid
)]
gnaf_disconnect(con)
```

## Add SA2 variables to the database

GNAF already contains longitude and latitude. Here we use those coordinates for
a **point-in-polygon lookup** against the SA2 boundaries, then ingest the SA2 code
and name into `C:/temp/gnafx23.duckdb`.

The geography is named `sa2_2021`. Its attributes live in a separate table with
one row per `address_detail_pid`, and are requested directly in `gnaf_match()`.
The suffix records the boundary edition: these are **2021 SA2s**.

| Task | Helper |
|------|--------|
| Calculate and save a geography | `gnaf_add_geography()` |
| Adopt an existing enrichment table | `gnaf_register_geography()` |
| See available layers and columns | `gnaf_list_geographies()` |
| Check current coverage | `gnaf_geography_coverage()` |
| Include attributes while matching | `gnaf_match(..., geographies = "sa2_2021")` |
| Enrich results already in memory | `gnaf_join_geographies()` |
| Remove a saved geography | `gnaf_remove_geography()` |

### Read the boundaries

Run from the repository root with the updated package loaded. The local database
currently contains Queensland addresses, so select Queensland boundaries. For
a national database, keep all states. Retain the `.shp`, `.shx`, `.dbf` and `.prj`
files together in `inst/extdata/SA2`.

```r
library(gnafr)
library(data.table)

sa2 <- read_shapefile(
  "inst/extdata/SA2/SA2_2021_AUST_GDA2020.shp",
  quiet = TRUE
)
sa2 <- subset_shapefile(sa2, "STE_CODE21", "3")  # Queensland
sa2 <- sa2[order(sa2$SA2_CODE21), c("SA2_CODE21", "SA2_NAME21")]
names(sa2)[1:2] <- c("sa2_code_2021", "sa2_name_2021")

# Prepare the boundaries once in GDA2020 / Australian Albers.
# The point coordinates will be transformed into this CRS for the lookup.
sa2 <- sf::st_transform(sa2, 9473)

# Confirmed source datum for C:/temp/gnafx23.duckdb.
gnaf_crs <- 7844  # GDA2020
```

G-NAF Standard is available in both GDA94 and GDA2020, so the shapefile's datum
doesn't tell us which datum the database coordinates use. The source for
`C:/temp/gnafx23.duckdb` is confirmed as GDA2020, so this example uses EPSG:7844.
For a different database built from a GDA94 download, use EPSG:4283 instead.
[Geoscape reference systems](https://docs.geoscape.com.au/projects/gnaf_desc/en/stable/reference_system.html).

### Add SA2 once, or register the existing table

This is one operation with **no batching**. SQL retrieves distinct coordinate
pairs, `spatial_lookup()` assigns their SA2 attributes in one call, and a single
SQL join writes the attributes back against every address PID. Addresses sharing
coordinates reuse the same spatial result.

```r
con <- gnaf_connect("C:/temp/gnafx23.duckdb")  # writable connection

# This block can be rerun: it reuses an existing registration.
if (!"sa2_2021" %in% gnaf_list_geographies(con)$name) {
  if (DBI::dbExistsTable(con, "gnaf_sa2_2021")) {
    # Adopt the table from the earlier demo. No spatial lookup is repeated.
    gnaf_register_geography(
      con, name = "sa2_2021", table = "gnaf_sa2_2021", points_crs = gnaf_crs
    )
  } else {
    # Create gnaf_geo_sa2_2021 and register it in one transaction.
    gnaf_add_geography(
      con, name = "sa2_2021", shapes = sa2,
      return_cols = c("sa2_code_2021", "sa2_name_2021"),
      points_crs = gnaf_crs
    )
  }
}

# Cheap metadata lookup: names, tables, coordinate CRS and attribute columns.
gnaf_list_geographies(con)

# One row per attribute; denominator includes every current source address.
gnaf_geography_coverage(con, "sa2_2021")
# Or check every available registered geography:
# gnaf_geography_coverage(con)
```

The inspected database had 13,869,745 address rows and 2,403,914 distinct valid
coordinate pairs. Deduplication therefore reduces the spatial work by about 83%.

The completed GDA2020 run created `gnaf_sa2_2021` in this database in 39.55 seconds,
without batching. The saved results were:

| Check | Result |
|-------|--------|
| Address rows / distinct PIDs | 13,869,745 / 13,869,745 |
| Rows assigned an SA2 | 13,746,894 (99.11%) |
| Rows missing coordinates | 120,536 |
| Valid-coordinate rows with no intersecting Queensland SA2 | 2,315 |
| Nundah alias, principal and primary records | All map to `302031040` / `Nundah` |

Those are historical results from the original run. The add/register block above
adopts that table if it still exists, or rebuilds it if it has been removed.
Use `gnaf_geography_coverage()` for current figures.

All distinct points are processed together in R, so allow memory for their
point geometries as well as their coordinates. Addresses with missing/invalid
coordinates or no intersecting SA2 retain `NA` attributes. Overlaps use the first
polygon in the supplied order; sorting by SA2 code makes that choice repeatable.

The ingestion is transactional: a failure rolls back the new table and its
registration. Existing layers are never silently replaced. This is a snapshot: rebuild it
after loading a new GNAF release, editing coordinates or choosing new boundaries.
Only `gnaf_addresses` is enriched here; use `address_table = "custom_addresses"`
and a different geography name and attribute names to enrich custom records too.

Coverage reports `non_missing`, `missing` and `coverage_pct` for each attribute.
`stored_rows` counts current PIDs present in the saved table; `orphaned_rows`
counts saved PIDs that no longer exist in the source. Coverage can therefore
reveal added/deleted addresses, but it cannot detect coordinate edits on the
same PID. Empty source tables report `NA` coverage rather than 100%.

### Include the new variables with match results

Pass the registered name in `geographies`. The join uses the final returned PID
and source, so SA2 describes the address selected by `return_principal` /
`return_primary`. Only needed PIDs are fetched; no manual SQL or joins are required.

```r
addresses <- c(
  "27 Ivo Street, Nundah QLD 4012",
  "10 St James Ct, Tamborine Mountain QLD 4272"
)
results_sa2 <- gnaf_match(
  addresses, con,
  return_principal = TRUE, return_primary = TRUE,
  geographies = "sa2_2021"
)
results_sa2[, .(
  input_raw, address_label, sa2_code_2021, sa2_name_2021, total_score
)]

# Include every available registered layer:
results_all <- gnaf_match(addresses, con, geographies = TRUE)

# Or name several layers after adding them:
# gnaf_match(addresses, con, geographies = c("sa2_2021", "lga_2025"))

# If you already have results, enrich them without matching again:
results <- gnaf_match(addresses, con, return_principal = TRUE, return_primary = TRUE)
results_with_sa2 <- gnaf_join_geographies(results, con, "sa2_2021")

gnaf_disconnect(con)
```

`geographies = NULL` (the default) adds no columns. Both entry points preserve
result order, repeated matches, unmatched inputs, scores and ranks; missing
assignments remain `NA`. Geography values are fetched fresh even when a match
comes from cache. The standalone join returns a copy of the input table.

Attribute names must be distinct across requested layers and existing result
columns. If a shapefile uses generic names, rename them while adding it:

```r
# Example for a future layer with columns called CODE and NAME:
# gnaf_add_geography(
#   con, "lga_2025", lga_shapes,
#   return_cols = c(lga_code_2025 = "CODE", lga_name_2025 = "NAME"),
#   points_crs = 7844
# )
```

Listing, coverage, matching and joins also work with read-only connections.
If a saved table was deleted manually, the list marks it `available = FALSE`;
requesting it by name gives a clear error. Remove its registration with the
removal helper before rebuilding it.

### Use the spatial helper directly without saving anything

For a quick presentation example, enrich just the current results in memory:

```r
results_sa2_in_memory <- spatial_lookup(
  results, sa2,
  return_cols = c("sa2_code_2021", "sa2_name_2021"),
  points_crs = gnaf_crs,
  chunk_size = NULL  # all points at once; no batching
)
results_sa2_in_memory[, .(address_label, sa2_code_2021, sa2_name_2021)]
```

The spatial helpers now support an explicit input CRS, skip empty polygons, and
look up repeated coordinates only once per operation when `chunk_size = NULL`.
Preparing projected boundaries once also avoids repeated spherical geometry
conversion. On a 10,000-coordinate sample from this database, the projected
lookup took 0.33 seconds versus 1.27 seconds using geographic boundaries, with
the same SA2 assignments for that sample. These are local sample timings, not
a guarantee for other datasets. The lookup uses sf's indexed spatial predicates;
see the [sf predicate documentation](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).

### Remove the SA2 variables when finished

Run this separately when you want to remove the saved enrichment (or before
rebuilding it). The address rows and original GNAF columns remain available.

If the database was already open read-only in this R session, close its other
connections first. Assigning a new connection to `con` does not close the old
one or change the shared database instance's access mode. With the updated
package, `gnaf_disconnect(con, shutdown = TRUE)` explicitly releases the instance
so it can be reopened writable. `gnaf_connect()` now rejects a conflicting mode
instead of silently returning a connection with the old mode.

```r
# Reload the updated code first if working from this repository:
# devtools::load_all()
if (exists("con") && DBI::dbIsValid(con)) {
  gnaf_disconnect(con, shutdown = TRUE)
}
con <- gnaf_connect("C:/temp/gnafx23.duckdb", read_only = FALSE)
if ("sa2_2021" %in% gnaf_list_geographies(con)$name) {
  gnaf_remove_geography(con, "sa2_2021")
}
stopifnot(!"sa2_2021" %in% gnaf_list_geographies(con)$name)
gnaf_disconnect(con)

# Optional: remove the extra columns from the in-memory result as well.
results_sa2[, c("sa2_code_2021", "sa2_name_2021") := NULL]
```
