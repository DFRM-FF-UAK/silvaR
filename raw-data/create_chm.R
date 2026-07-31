# Provenance and reproduction script for inst/raster/chm.tif
#
# The point cloud this raster is built from is far too large to ship inside the
# package - the full tile is about 170 MB, and even a fragment covering the
# example stands at the delivered density would exceed 100 MB. The cloud is
# therefore downloaded on demand from the national geoportal by step 1 below,
# so the raster remains reproducible without inflating the package.
#
# SOURCE
#   Airborne laser scanning, GUGiK "Dane pomiarowe LIDAR 2023"
#     tile          M-34-52-B-b-1-1-2 (godlo, 1:2500 archiving module)
#     acquired      2023-05-11, leaf-on
#     published     2023-09-26
#     delivered     33 488 047 points over 130.9 ha = 25.6 points/m2
#                   (the GUGiK index advertises a nominal 4 points/m2)
#     format        LAS 1.2, point format 1, fully classified
#     reference     PL-1992 (EPSG:2180), heights PL-EVRF2007-NH
#   Terms of reuse and the required attribution are set out in
#   DATA_SOURCES.md, installed at the root of this package.
#
# PROCESSING
#   Noise (class 7) and flight-line overlap (class 12) are dropped on read.
#   Terrain model: 1 m, TIN interpolation over ground points (class 2).
#   Canopy model:  1 m, point-to-raster with a 0.15 m subcircle.
#   Heights below zero are clamped to zero.
#   The result is cropped to the extent of the seven example stands plus a
#   20 m margin, snapped to a 10 m grid. The snapping matters: th_calc()
#   aggregates to 10 m relative to the raster origin, so an arbitrary crop
#   shifts the aggregation grid and moves top height slightly. Narrow stands
#   feel this most - subarea 152c moved by 0.49 m under an unaligned crop
#   while the other six moved by 0.14 m or less.
#
# REQUIREMENTS
#   lidR. As of July 2026 neither lidR nor rlas is present on CRAN for
#   R 4.6; install them from the author's repository:
#     remotes::install_github("r-lidar/rlas")
#     remotes::install_github("r-lidar/lidR")
#   Reading the full tile needs roughly 2 GB of memory. On a desktop machine
#   the download takes about a minute and the processing about ninety seconds.

library(lidR)
library(sf)
library(terra)

## ---- Step 1: fetch the source point cloud ---------------------------------

laz_url <- paste0("https://opendata.geoportal.gov.pl/NumDaneWys/",
                  "DanePomiaroweLAZ/77650/77650_1373379_M-34-52-B-b-1-1-2.laz")
laz <- "M-34-52-B-b-1-1-2.laz"

if (!file.exists(laz)) {
  message("Downloading ", basename(laz_url), " (about 170 MB) ...")
  # The default 60 s timeout is not enough for a file this size.
  old <- options(timeout = 1800)
  on.exit(options(old), add = TRUE)
  download.file(laz_url, laz, mode = "wb")
}

# Confirm the download is a complete LAS/LAZ file rather than an error page.
stopifnot(file.exists(laz), file.info(laz)$size > 1e8)
con <- file(laz, "rb"); sig <- rawToChar(readBin(con, "raw", 4)); close(con)
if (!identical(sig, "LASF"))
  stop("Downloaded file is not a LAS/LAZ archive (signature: ", sig, ")")

hdr <- rlas::read.lasheader(laz)
message("Points: ", format(hdr$`Number of point records`, big.mark = " "))

## ---- Step 2: build the canopy height model --------------------------------

las <- readLAS(laz, filter = "-drop_class 7 12", select = "xyzc")
st_crs(las) <- 2180        # the delivered file carries no CRS

dtm  <- rasterize_terrain(las, res = 1, algorithm = tin())
nlas <- normalize_height(las, dtm)
chm  <- rasterize_canopy(nlas, res = 1, algorithm = p2r(subcircle = 0.15))
chm[chm < 0] <- 0

## ---- Step 3: crop to the example stands, aligned to a 10 m grid -----------

stands <- st_read(system.file("vector/stands.gpkg", package = "silvaR"))
b   <- st_bbox(stands)
buf <- 20
e   <- ext(floor((b[["xmin"]] - buf) / 10) * 10, ceiling((b[["xmax"]] + buf) / 10) * 10,
           floor((b[["ymin"]] - buf) / 10) * 10, ceiling((b[["ymax"]] + buf) / 10) * 10)
chm <- crop(chm, e)

writeRaster(chm, "inst/raster/chm.tif", overwrite = TRUE)
message("Saved: inst/raster/chm.tif (", paste(dim(chm)[1:2], collapse = " x "),
        " cells at ", xres(chm), " m)")
