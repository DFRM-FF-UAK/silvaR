# Reproduction script for man/figures/th_calc_example.png
#
# The figure illustrates the th_calc() section of README.md. It shows the two
# halves of the calculation side by side: the canopy height model the function
# reads, with the stand boundaries drawn over it, and the stands themselves
# filled with the top height the function returns.
#
# INPUTS
#   Both come from the installed package and are documented in DATA_SOURCES.md:
#     inst/raster/chm.tif    1 m canopy model, see raw-data/create_chm.R
#     inst/vector/stands.gpkg  seven Scots pine subareas, Koniecpol 2023
#
# DESIGN NOTES
#   Both panels share one extent and one colour ramp family, so a stand can be
#   followed by eye from the left panel to the right one. The two colour bars
#   deliberately span different ranges: the canopy model runs from bare ground
#   to the tallest crown, while top height spans only the stand values. A
#   shared scale would flatten the right panel into near-identical shades.
#   Boundaries are drawn in red because viridis contains no red, so the lines
#   stay legible over both the dark and the bright end of the ramp.
#   Each stand is labelled with the top height th_calc() returned for it and
#   nothing else. The figure exists to show what the function computes, so the
#   label carries the result rather than the stand's identity or its age; those
#   are attributes of the input and are already in stands.gpkg.

library(silvaR)
library(sf)
library(terra)

chm    <- rast(system.file("raster/chm.tif", package = "silvaR"))
stands <- st_read(system.file("vector/stands.gpkg", package = "silvaR"), quiet = TRUE)

th <- th_calc(chm, stands)

th$label <- sprintf("%.1f m", th$top_height)

v <- vect(th)
e <- ext(chm)

png("man/figures/th_calc_example.png", width = 1700, height = 760, res = 150)
par(mfrow = c(1, 2), mar = c(2.8, 3.0, 2.6, 0.6), mgp = c(2, 0.6, 0))

# Left: the input canopy model, with the stand boundaries over it
plot(chm, main = "Canopy Height Model", ext = e,
     col = hcl.colors(100, "viridis"), plg = list(title = "m"))
plot(v, add = TRUE, border = "red", lwd = 2)

# Right: the output, one top height per stand
plot(v, "top_height", type = "continuous", ext = e,
     col = hcl.colors(100, "viridis"), border = "grey25", lwd = 1.2,
     main = "Top height per stand", plg = list(title = "m"))
text(v, labels = v$label, halo = TRUE, hc = "white", col = "black", cex = 0.8)

dev.off()
message("Saved: man/figures/th_calc_example.png")
