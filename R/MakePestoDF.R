#' Intakes qualitative survey data
#'
#' @param f_dat The elicited data, default is to use pesto_exinput
#' @returns A data frame with value dist for each category in each package
#' @export


MakePestoDF <- function(f_dat = pesto_exinput) {

  a1 <-
    f_dat %>%
    janitor::clean_names() %>%
    tidyr::fill(title, package_title) %>%
    dplyr::mutate(weight = as.numeric(weight)) %>%
    #--make sure assessments are lower case
    dplyr::mutate_at(c("assignment_text", "assignment_confidence"),
                     stringr::str_to_lower)

  # a. make sure input data is good --------------------------------------------

  #--column names
  t1 <- (setdiff(names(a1), names(internal_exinput)))

  m1 <- NULL
  if (length(t1) > 0) {
    m1 <- paste("Hi! This column name doesn't match the template:", t1)
    stop(m1)
  }

  #--weights add up to 100
  t2 <-
    a1 %>%
    dplyr::group_by(package_title) %>%
    dplyr::summarise(weight = round(sum(weight)))

  for (i in 1:nrow(t2)) {
    m2 <-
      t2 %>%
      dplyr::slice(i) %>%
      dplyr::pull(weight)
    if (m2 != 100) {
      stop("Hi! Weights do not add up to 100, please check them")
    }
    i <- i + 1

  }

  #--weights for all packages are the same
  t3 <-
    a1 %>%
    dplyr::select(package_title, assessment_category, weight) %>%
    tidyr::pivot_wider(names_from = package_title, values_from = weight) %>%
    janitor::clean_names()

  for (j in 1:nrow(t3)) {
    m3 <-
      t3 %>%
      dplyr::select(-assessment_category) %>%
      dplyr::slice(j) %>%
      as.vector() %>%
      unname() %>%
      unlist()

    if (var(m3) != 0) {
      stop("Hi! The weights for the packages are different, that is not allowed.")
    }
    j <- j + 1

  }

  #b. use lookup table of beta distributions----------------------

  #--merge beta dist with data
  b1 <-
    a1 %>%
    dplyr::rename(rating_text = assignment_text, confidence = assignment_confidence) %>%
    dplyr::left_join(internal_binnedbetas, relationship = "many-to-many")



  return(b1)


}
