# silvaR 1.0.0

First numbered release. Earlier work carried the devtools placeholder
`0.0.0.9000`; see the note on 0.9.0 below for the state cited in the
literature.

## Breaking changes

* `zd_share()` has been removed.
* `th_calc()` no longer accepts the `f` argument. It was declared but never
  used, so a call passing it now raises an error instead of silently ignoring
  the value.

## Changed results

* `quad_dbh()` now computes the quadratic mean (root mean square) diameter in
  the `only_measured_h = FALSE` branch as well. That branch previously
  returned the arithmetic mean, which duplicated `av_dbh()` and contradicted
  both the name of the function and its documentation.

  **This changes returned values.** They are higher wherever a
  species/age/layer group within a plot holds trees of differing diameters,
  and the gap widens as the diameters spread. A group of a 20 cm and a 30 cm
  tree now returns 25.50 cm instead of 25.00 cm; across the bundled `PPdemo`
  data the affected group values rise by up to about 1 cm. Groups holding a
  single tree are unaffected, the two means coinciding there.

  Calls that keep the default `only_measured_h = TRUE` are unaffected, and so
  are `h_tree()` and `v_tree_bruchwald()`, which both call `quad_dbh()` with
  `only_measured_h = TRUE`. Only code calling `quad_dbh()` with
  `only_measured_h = FALSE` will see different numbers.

## Improvements

* `th_calc()` accepts rasters finer than 1 m. Such input previously aborted
  while the resampling template was built. Aggregation now goes straight to
  the 10 m grid.
* `th_calc()` warns when the input resolution does not divide 10 m, so that
  the result lands on a grid other than the one the method is defined on, and
  when pixels are not square.
* `sp_clean()` matches bare Polish common names such as "sosna" or "świerk",
  including misspellings, instead of returning `NA` for them.

## Example data

`inst/raster/chm.tif` and `inst/vector/stands.gpkg` have been replaced with a
contemporaneous pair, both describing 2023:

* a 1 m canopy height model derived from GUGiK airborne laser scanning tile
  M-34-52-B-b-1-1-2, flown on 11 May 2023;
* seven Scots pine subareas of compartments 151 and 152 in the Koniecpol
  forest district, from the Polish Forest Data Bank (BDL).

The previous pair was not contemporaneous, which showed: subareas recorded as
clear-cut returned a canopy several metres tall. Within the new data the
age-to-top-height relationship is monotonic.

Sources, terms of reuse and the attribution both providers require are set out
in `DATA_SOURCES.md`, installed at the root of the package. Scripts in
`raw-data/` reproduce the raster, the stand layer and both README figures.

## Documentation

* The `th_calc()` example in `README.md` now runs. The `system.file()` path no
  longer begins with `inst`, the function is called with a polygon layer as
  its current signature requires, and the surrounding text no longer claims
  the method works at any spatial resolution.
* `h_growth()` in the README has been corrected to `th_growth()`; no function
  of the former name has ever existed.
* Code comments, messages and test names are in English throughout.

## Dependencies

* R (>= 3.5), unchanged.
* `testthat` (>= 3.0.0) is now declared in `Suggests` and edition 3 is
  configured. `tests/testthat.R` has been added, so `R CMD check` runs the
  test suite; it previously skipped `tests/` altogether.
* Reproducing `inst/raster/chm.tif` through `raw-data/create_chm.R` requires
  `lidR`. As of July 2026 neither `lidR` nor `rlas` is available on CRAN for
  R 4.6; both install from the maintainer's repositories under
  <https://github.com/r-lidar>.


# silvaR 0.9.0

A retrospective tag, applied in 2026 to the commit of 11 December 2025. It
marks the state of the package described in Socha et al. (2026), *Forest
Ecology and Management* 603, 123433, which cites silvaR by repository address
without naming a version. The tag exists so that state stays recoverable;
`DESCRIPTION` at that commit still reads `0.0.0.9000`, and `zd_share()` is
still present.
