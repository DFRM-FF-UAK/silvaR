#' Example dataset: Sample forest plots
#'
#' This dataset contains example plot-level data used to demonstrate
#' the functionality of the silvaR package.
#'
#' @format A data frame with 72 rows and 9 variables:
#' \describe{
#'   \item{SAMPLE_NR}{Unique ID of the sample plot}
#'   \item{SAMPLE_RECORD_NR}{Unique ID of the tree within the sample plot}
#'   \item{SPECIES_CD}{Tree species in Polish abbreviations}
#'   \item{BREAST_HEIGHT}{Diameter at breast height (DBH), measured at 1.3 m (cm)}
#'   \item{TREE_HEIGHT}{Total tree height (m)}
#'   \item{TREE_AGE}{Tree age (years)}
#'   \item{STOREY_NR}{Storey (canopy layer) in which the tree occurs}
#'   \item{AZIMUTH}{Azimuth of the tree from the plot center (degrees)}
#'   \item{DISTANCE}{Horizontal distance of the tree from the plot center (meters)}
#' }
#'
#' @usage data(rybnikPP)
#' @keywords datasets
#'
"rybnikPP"
