#' Intakes qualitative survey data
#'
#' @param f_dat3 Output from SummarizePestoDF, default is to use pesto_exsumdf
#' @returns A data frame with utility for each package
#' @export


CalcPestoUtil <- function(f_dat3 = pesto_exsumdf) {

  a1 <- f_dat3

  #--assign fake 'utility' scores to each bin
  a2 <-
    a1 %>%
    dplyr::mutate(value = dplyr::case_when(
      value_bin == 5 ~ 100,
      value_bin == 4 ~ 75,
      value_bin == 3 ~ 50,
      value_bin == 2 ~ 25,
      value_bin == 1 ~ 0,
      TRUE ~ 9999
    ))

  #--utility weighted by score - summed
  a3 <-
    a2 %>%
    dplyr::group_by(package_title) %>%
    dplyr::summarise(util = round(weighted.mean(x = value, w = score), 0))


return(a3)

}
