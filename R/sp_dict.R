#' Example dataset: Species dictionary with coding and grouping information
#'
#' A reference table of tree species and their attributes used for testing
#' species-related functionality, including code matching, grouping, and
#' visualization. The dataset contains multiple naming conventions,
#' grouping identifiers, and auxiliary fields such as color codes and
#' common typographical variants.
#'
#' @format A data frame with 2132 rows and 11 variables:
#' \describe{
#'   \item{SPECIES_CD}{Unique species code identifier}
#'   \item{species_name}{Species name in Polish}
#'   \item{latin_name}{Scientific (Latin) species name}
#'   \item{english_name}{Species name in English (may be empty)}
#'   \item{BUL_SPECIES_NR}{Numeric species identifier}
#'   \item{SPECIES_GROUP}{Species group code}
#'   \item{SPCIES_GROUP_NUM}{Numeric species group identifier}
#'   \item{AGE_GROUP}{Age class group}
#'   \item{code}{Combined species and age group code}
#'   \item{col}{Hex color code used for visualization}
#'   \item{typos}{Comma-separated list of common code variants and typos}
#' }
#'
#' @usage data(sp_dict)
#' @keywords datasets
#'
"sp_dict"
