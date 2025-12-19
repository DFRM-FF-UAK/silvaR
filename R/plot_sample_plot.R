# ============================================================
# Internal helpers
# ============================================================

detect_column <- function(data, candidates, name) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) {
    stop(
      sprintf(
        "plot_szkic(): could not detect %s column. Tried: %s",
        name, paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  found[1]
}

calculate_rel_coordinates <- function(azimuth_deg, distance_cm) {
  azimuth_rad <- azimuth_deg * pi / 180

  list(
    x = distance_cm / 100 * sin(azimuth_rad),
    y = distance_cm / 100 * cos(azimuth_rad)
  )
}

# ============================================================
# Exported function
# ============================================================

#' Plot schematic view of a forest sample plot
#'
#' Creates a schematic (top-down) visualization of a forest plot with trees
#' positioned using azimuth and distance measurements from the plot center.
#' Column names are detected automatically using silvaR conventions.
#'
#' @param data A data frame with tree-level measurements.
#' @param plot_radius Radius of the plot in meters.
#' @param azimuth Optional name of azimuth column.
#' @param distance Optional name of distance column (cm).
#' @param dbh Optional name of diameter at breast height column (cm).
#' @param species Optional name of species column.
#' @param species_colors Optional named vector of colors for species.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point coord_fixed labs theme_minimal
#' @importFrom ggforce geom_circle
#'
#' @export
plot_sample_plot <- function(data,
                       plot_radius,
                       azimuth = NULL,
                       distance = NULL,
                       dbh = NULL,
                       species = NULL,
                       species_colors = NULL) {

  stopifnot('"plot_radius" parameter must be numeric' = is.numeric(plot_radius))

  # ---- detect columns (or use user-provided) ----
  if (is.null(azimuth)) {
    azimuth <- detect_column(
      data,
      c("AZIMUTH", "azimuth", "Azimuth"),
      "azimuth"
    )
  }

  if (is.null(distance)) {
    distance <- detect_column(
      data,
      c("DISTANCE", "distance", "Dist"),
      "distance"
    )
  }

  if (is.null(dbh)) {
    dbh <- detect_column(
      data,
      c("DBH", "BREAST_HEIGHT", "dbh"),
      "dbh"
    )
  }

  if (is.null(species)) {
    species <- detect_column(
      data,
      c("SPECIES", "SPECIES_CD", "species"),
      "species"
    )
  }

  if (is.null(species_colors)) {
    species_colors <- species_colors
  }

  # ---- clean species names ----
  data[[species]] <- sp_clean(data[[species]])

  # ---- calculate local coordinates ----
  coords <- calculate_rel_coordinates(
    azimuth_deg = data[[azimuth]],
    distance_cm = data[[distance]]
  )

  data$x <- coords$x
  data$y <- coords$y

  # ---- plot ----
  p <- ggplot2::ggplot() +
    ggforce::geom_circle(
      ggplot2::aes(x0 = 0, y0 = 0, r = plot_radius),
      color = "gray40",
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(
        x = x,
        y = y,
        size = .data[[dbh]],
        color = .data[[species]]
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 0, y = 0),
      shape = 4,
      size = 1,
      color = "black",
      stroke = 1.2
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = "X [m]",
      y = "Y [m]",
      color = "Gatunek",
      size = "Pierśnica [cm]"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(species_colors)) {
    p <- p + ggplot2::scale_color_manual(values = species_colors)
  }

  p
}
