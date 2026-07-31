# Reproduction script for man/figures/plot_sample_plot_example*.png
#
# The three figures illustrate the plot-level section of README.md. Each shows
# one of the three sample plots in the PPdemo dataset, drawn by
# plot_sample_plot(): trees positioned from their azimuth and distance to the
# plot centre, sized by diameter and coloured by species, inside the dashed
# plot boundary.
#
# INPUT
#   data(PPdemo) from this package. It holds exactly three sample plots, and
#   the mapping below is not in ascending order - it follows the numbering the
#   figures already had in README.md:
#     plot_sample_plot_example.png    SAMPLE_NR  72   CZM, SO, SW
#     plot_sample_plot_example2.png   SAMPLE_NR 561   BRZ, DB, OL, SW
#     plot_sample_plot_example3.png   SAMPLE_NR 250   BK, SO
#
# NOTES
#   plot_radius is 11.28 m, the radius of the 400 m2 circular plot the demo
#   data were measured on.
#   Species codes are cleaned inside plot_sample_plot(), so no call to
#   sp_clean() is needed here. The README applies it to the same column before
#   calling h_tree() and v_tree(), which do need clean codes; that call has no
#   effect on these figures.
#   Colours come from the species dictionary in the package's internal data,
#   so a species keeps the same colour across all three plots. Species absent
#   from the dictionary fall back to grey.
#   The figures are saved at the size the README displays them at, 250 px wide
#   in a row of three, rendered at 750 px so they stay sharp when opened.

library(silvaR)
library(ggplot2)

data(PPdemo)

figures <- list(
  list(file = "man/figures/plot_sample_plot_example.png",  plot_nr =  72),
  list(file = "man/figures/plot_sample_plot_example2.png", plot_nr = 561),
  list(file = "man/figures/plot_sample_plot_example3.png", plot_nr = 250)
)

for (f in figures) {
  single <- PPdemo[PPdemo$SAMPLE_NR == f$plot_nr, ]
  stopifnot(nrow(single) > 0)

  p <- plot_sample_plot(data = single, plot_radius = 11.28)

  ggsave(f$file, p, width = 7.5, height = 5.2, dpi = 100, bg = "white")
  message("Saved: ", f$file, "  (plot ", f$plot_nr, ", ", nrow(single), " trees)")
}
