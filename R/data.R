#' Example elicitation data (fair dust on lettuce)
#'
#' An example of the elicitation data format to be input into the package function
#'
#' @format A data frame with an expanding number of rows and 4 variables:
#' \describe{
#'   \item{title}{The user provided title for the comparison(s), should include the country, crop, and intervention(s)}
#'   \item{package_title}{The user provided title for the package being evaluated}
#'   \item{weight}{The weight assigned to each of the six impact categories, must add to 100}
#'   \item{assessment_category}{The performance category}
#'   \item{assignment_text}{The text assignment of the package performance given by the user to that category
#'   (very low, low, medium, high, very high)}
#'   \item{assignment_confidence}{The confidence the user has in the given assignment (L, M, H, VH)}
#' }
"pesto_exinput"

#' More detailed description of the six performance categories
#'
#' Includes the question (more or less) used in the survey
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{assessment_category}{The short notation to refer to the performance category}
#'   \item{assessment_question}{The question (more or less) posed in the survey to the user to assess the package's performance in the category}
#'   \item{performance_metric}{A nice, concise wording for use in figures}
#' }
"pesto_assessmentkey"
