#' Example elicitation data (fair dust on lettuce)
#'
#' An example of the elicitation data format to be input into the package function
#'
#' @format A data frame with an expanding number of rows and 4 variables:
#' \describe{
#'   \item{title}{The user provided title for the comparison, should include organization, crop, and intervention}
#'   \item{weight}{The weight assigned to each of the six impact categories}
#'   \item{short}{The short way to refer to the impact category}
#'   \item{ccp_rating}{The rating for the current commercial practice (CCP)
#'   scenario for a given impact category
#'   (very low impact, low impact, medium impact, high impact, very high impact)}
#'   \item{ccp_confidence}{The confidence the user has in the given rating (L, M, H, VH)}
#'   \item{ipm_rating}{The rating for the new integrated pest management (IPM) scenario for a given impact category}
#'   \item{ipm_confidence}{The confidence the user has in the given rating}
#' }
"pesto_exinput"

#' More detailed description of the six impact categories
#'
#' Includes the question used in the elicitation form, the short description, and the long description
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{short}{The short notation to refer to the impact category}
#'   \item{value_metric}{A longer description of the impact category}
#'   \item{question}{The question used in the elicitation paperwork (more or less)}
#' }
"pesto_impactcatsinfo"
