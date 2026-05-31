library(terra)
library(sf)

# load CHM to get extent and CRS
chm <- terra::rast(system.file('raster/chm.tif', package = 'silvaR'))

e   <- terra::ext(chm)
crs <- terra::crs(chm)

xmin <- e$xmin
xmax <- e$xmax
ymin <- e$ymin
ymax <- e$ymax

# CHM extent: ~750 x 560 m (EPSG:2180)
# generate a regular 3x2 grid of stands with small gaps between them
n_col <- 3
n_row <- 2
gap   <- 10   # gap between stands (m)

w <- (xmax - xmin - gap * (n_col + 1)) / n_col
h <- (ymax - ymin - gap * (n_row + 1)) / n_row

polys <- vector("list", n_col * n_row)
ids   <- character(n_col * n_row)
k     <- 1L

for (r in seq_len(n_row)) {
  for (c in seq_len(n_col)) {
    x0 <- xmin + gap * c       + w * (c - 1)
    x1 <- x0  + w
    y0 <- ymin + gap * r       + h * (r - 1)
    y1 <- y0  + h
    polys[[k]] <- sf::st_polygon(list(matrix(
      c(x0, y0,
        x1, y0,
        x1, y1,
        x0, y1,
        x0, y0),
      ncol = 2, byrow = TRUE
    )))
    ids[k] <- sprintf("stand_%02d", k)
    k <- k + 1L
  }
}

stands <- sf::st_sf(
  stand_id = ids,
  geometry = sf::st_sfc(polys, crs = crs)
)

out_path <- system.file('vector', package = 'silvaR')
if (!dir.exists(out_path)) {
  out_path <- file.path(dirname(system.file('raster/chm.tif', package = 'silvaR')),
                        '..', 'vector')
  dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
}

sf::st_write(stands, file.path(out_path, 'stands.gpkg'), delete_dsn = TRUE)
message("Saved: ", file.path(out_path, 'stands.gpkg'))



