# Adding the Queensland Property Location Index (PLI)

G-NAF lags the ground: new subdivisions, new strata buildings and individual units
appear in Queensland's own address dataset before they reach G-NAF. The **Property
Location Index** (PLI, "Property address Queensland") is that dataset. `gnaf_load_pli()`
adds the PLI addresses that G-NAF does not already have, as an extra step after the
database is built, so `gnaf_match()` can find them like any other address.

## 1. Download the data

The PLI is free, licensed CC BY 4.0 (© State of Queensland), and updated weekly.

1. Open the Queensland Spatial Catalogue record:
   <https://qldspatial.information.qld.gov.au/catalogue/custom/detail.page?fid={F878C43D-3087-4102-8F28-1CFEA49B34F1}>
2. Order **Property address Queensland - Text data package** and download
   `DP_PROP_LOCATION_INDEX_QLD.zip` (about 100 MB; the text file inside is about
   350 MB).

Keep the `.zip`, or unzip it: `gnaf_load_pli()` takes either. The dataset's metadata
is in [PLI.xml](PLI.xml).

## 2. Load it

G-NAF (Queensland) must already be in the database:

```r
library(gnafr)

con <- gnaf_connect("C:/temp/gnaf.duckdb")     # a database built with gnaf_build_db()

pli <- gnaf_load_pli(con, "C:/Users/me/Downloads/DP_PROP_LOCATION_INDEX_QLD.zip")
```

It takes about a minute for the whole of Queensland (most of it unzipping and reading the file) and prints a summary of what
it found. On a Queensland G-NAF database and the 26 September 2026 extract it looked
like this:

```
PLI load summary
  PLI records read                                          2,842,481  100.00% of records read
  Records considered (status filters)                       2,841,920   99.98% of records read
  Unique addresses                                          2,839,988   99.93% of records considered
  Unusable: unit number not understood                             57    0.00% of unique addresses
  Unusable: no street number or lot                               818    0.03% of unique addresses
  Unusable: no postcode found in G-NAF                            135    0.00% of unique addresses
  Usable addresses                                          2,838,978   99.96% of unique addresses
  Already in G-NAF (same label)                             2,440,501   85.96% of usable addresses
  Already in G-NAF (same label ignoring building name / flat type)
                                                              272,342    9.59% of usable addresses
  Already loaded from a previous PLI file                           0    0.00% of usable addresses
  Duplicate label within the PLI                                2,096    0.07% of usable addresses
  Added                                                       124,039    4.37% of usable addresses

Coordinates: for 20,000 addresses in both, the PLI point was 1.52 m from G-NAF's as
supplied and 0.00 m after conversion to GDA2020.
```

The same table is returned invisibly (`stage`, `n`, `pct`, `pct_of`), with the datum and
coordinate check as attributes:

```r
pli
attr(pli, "coordinate_check")
```

Most of the PLI is already in G-NAF (about 95% here), so expect the added addresses to
be a small share: mostly units in strata buildings and retirement villages, lots in new
subdivisions, and rural lot-only addresses.

Options: `overwrite = TRUE` replaces the PLI rows from an earlier load,
`address_status = "P"` keeps only primary addresses (the default also keeps the
PLI's alternate addresses), and `include_deleted_lotplans = TRUE` also considers
addresses on a lot on plan the PLI flags as deleted.

## 3. What it does to the data

The PLI is wrangled the way the G-NAF loaders wrangle G-NAF, so its addresses match, rank
and index identically.

| PLI | Becomes |
|---|---|
| `ADDRESS_PID` | `PLI<pid>` (for example `PLI2793749`). G-NAF PIDs start `GA`, so they cannot collide; the load checks this, and checks custom addresses too, before adding anything. |
| label | Built by the same code as G-NAF's: flat, number, street, then `LOCALITY QLD POSTCODE`, e.g. `UNIT 50 11 ROLLESTON STREET, KEPERRA QLD 4054`. |
| `UNIT_TYPE` | `flat_type`; `U` is G-NAF's `UNIT`. |
| `UNIT_NUMBER`, `UNIT_SUFFIX` | `flat_number`, and both appear in the label (`UNIT 2C`, `UNIT 3B`). |
| `STREET_NO_1`, `STREET_NO_2` and their suffixes | `number_first`, `number_last`; the suffixes stay in the label (`15A-17`), as in G-NAF, and are not stored separately. |
| `LOT` | Used only when there is **no street number**: then the label reads `LOT 24 RURAL ROAD, ...` and `lot_number` is set. Otherwise it is ignored (the placeholder lots `0` and `9999` never count). The lot and plan are always kept, in `legal_parcel_id` as `lot/plan`. |
| `PROPERTY_NAME` | `address_site_name`, not the label (G-NAF's labels do not print the site name). |
| `STREET_TYPE` | Canonical street type. The PLI's `XXX` placeholder means no type. |
| `STREET_SUFFIX` | G-NAF's code (`EAST` is `E`, `CENTRAL` is `CN`), as G-NAF stores and prints it. |
| `LOCALITY` | Upper case, with the PLI's `(LGA)` qualifier on ambiguous names dropped (`WEST END (BRISBANE CITY)` is `WEST END`). |
| state | `QLD`, imputed: every PLI address comes via Queensland. |
| postcode | Not in the PLI, so taken from G-NAF: the locality's only postcode; else the postcode G-NAF gives that street in the locality; else the postcode of the nearest G-NAF address in the locality. An address in a locality G-NAF does not have cannot be labelled and is counted as unusable. |
| `LATITUDE`, `LONGITUDE` | Converted to **GDA2020**, the datum G-NAF uses. See below. |
| `GEOCODE_TYPE` | Kept as supplied. |
| all | `source = "pli"`, and they are principal addresses (`alias_type` is `NA`). |

One address can appear several times in the PLI (several parcels, several geocodes); it is
one address here and keeps its property-centroid (`PC`) geocode.

### Only new addresses are added

An address counts as already in G-NAF when its label matches one of G-NAF's principal or
alias labels, comparing with case, commas, full stops and spacing ignored. Two sources
often describe one address slightly differently, so the load also treats a label as
already present when it matches once the **building name** is set aside, or when it matches
with the **flat type** made generic (the PLI says `UNIT` where G-NAF says `SHOP`, `SHED` or
`SUITE`). The summary reports the two separately, so you can see how much the relaxed test
contributes. A label occurring twice within the PLI is added once, and an address from an
earlier PLI load counts as already loaded.

A few added addresses will still have a G-NAF look-alike written differently, such as
`UNIT 139 176 TORRENS ROAD` against `UNIT 139 176-208 TORRENS ROAD` or `17A` against `17`.
They are a small share (2-3% of those added) and sit at the same point.

### Coordinates and datum

The PLI states its datum in the file (`GDA94`) and G-NAF is GDA2020, a difference of about
1.5 m to the north-east. `gnaf_load_pli()` converts with PROJ's standard GDA94 to GDA2020
transformation (through `sf`), leaves points already in GDA2020 alone, and refuses any other
datum. It then checks itself: for a sample of addresses in both datasets it reports how
far the PLI point is from G-NAF's as supplied and after conversion. The second figure should
be close to zero, as it is above.

## 4. Using the new addresses

Nothing else changes: `gnaf_match()` searches them along with G-NAF, and the `source`
column of a result says which they came from.

```r
res <- gnaf_match(addresses, con)
res[source == "pli"]
```

The load also rebuilds what depends on the address table: the locality and street-type
indexes, the exact-label index (`gnaf_rebuild_exact_index()`) if the database has one
(keeping its settings), and it clears the match cache.

To use the PLI in a database built without an exact-label index, nothing more is needed;
to add that index, run `gnaf_rebuild_exact_index(con)`.

## 5. Keeping it up to date

* **A new PLI extract** (weekly): load it with `overwrite = TRUE`. This replaces the earlier
  PLI rows and re-tests every PLI address against G-NAF.
* **A new G-NAF release:** reloading G-NAF does not remove PLI rows, so run
  `gnaf_load_pli(con, path, overwrite = TRUE)` afterwards. Addresses G-NAF now has are
  then no longer added.
* **Removing the PLI:** `DBI::dbExecute(con, "DELETE FROM gnaf_addresses WHERE source = 'pli'")`,
  then `gnaf_rebuild_locality_index(con)`, `gnaf_rebuild_street_type_index(con)` and
  `gnaf_rebuild_exact_index(con)` if you use it.

## Caveats

* The PLI's postcode is imputed. For the 26 September 2026 extract, 97.8% of addresses took
  their locality's only postcode; 2.2% (61,622) sit in a locality with several postcodes and
  took their street's; 0.07% (2,122) took the postcode of the nearest G-NAF address, which
  can be wrong close to a postcode boundary; 135 could not be given one and are left out
  rather than guessed. `attr(pli, "postcode_methods")` gives the breakdown for your file.
* The PLI's code lists are in the support article linked from its metadata
  ([PLI.xml](PLI.xml)). `gnaf_load_pli()` reads `ADDRESS_STATUS` `P` and `A` as primary and
  alternate addresses and `LOTPLAN_STATUS` `D` as a deleted lot on plan, judging from the
  data; confirm that against the article if it matters to you. By default it keeps `P` and
  `A` and drops `D`.
* The metadata's reference-system block names GDA2020 (EPSG:4938), but every record in the
  file declares `DATUM = GDA94` and the coordinate check confirms it; the conversion follows
  the file, not the metadata.
* Attribute the data as the PLI's licence asks: © State of Queensland (Department of Natural
  Resources and Mines, Manufacturing and Regional and Rural Development), CC BY 4.0.
