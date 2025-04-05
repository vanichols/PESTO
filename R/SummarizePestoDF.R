#' Intakes qualitative survey data
#'
#' @param f_dat2 Output from MakePestoDF, default is to use pesto_exdf
#' @returns A data frame with value dist for each category in each package
#' @export


SummarizePestoDF <- function(f_dat2 = pesto_exdf) {

  a1 <- f_dat2

  t.cats <-
     a1 %>%
    dplyr::pull(assessment_category) %>%
    unique()

  t.pkgs <-
    a1 %>%
    dplyr::pull(package_title) %>%
    unique()

  value_bin_options <-
    a1 %>%
    dplyr::select(value_bin) %>%
    dplyr::distinct()

  #--loop through each package

  a2 <- NULL

  for (i in 1:length(t.pkgs)){

    t1 <-
      a1 %>%
      dplyr::filter(package_title == t.pkgs[i])

    #--build a sample for each category
    bayes.value.vector1 <- NULL

    for(j in 1:length(t.cats)){

      tmp.df <-
        t1 %>%
        dplyr::select(assessment_category, value_bin, score) %>%
        dplyr::filter(assessment_category == t.cats[j])

      tmp.wt <-
        t1 %>%
        dplyr::filter(assessment_category == t.cats[j]) %>%
        dplyr::pull(weight) %>%
        unique()

      tmp.samp <-
        sample(x = tmp.df$value_bin,
               prob = tmp.df$score,
               size = 100000*tmp.wt,
               replace = TRUE)

      bayes.value.vector1 <- c(bayes.value.vector1, tmp.samp)

      j <- j + 1

    }

    #--use that bayes sampling vector, summarise by value bin appearance

    t2 <-
      tibble::tibble(value_bin = bayes.value.vector1) %>%
      dplyr::group_by(value_bin) %>%
      dplyr::summarise(score = dplyr::n()/100000)

    #should be 100
    # t2 %>%
    #   summarise(score = sum(score))

    t3 <-
      value_bin_options %>%
      dplyr::left_join(t2) %>%
      dplyr::mutate(
        score = ifelse(is.na(score), 0, score),
        package_title = t.pkgs[i])

    #--bind it to the other package results
    a2 <-
      a2 %>%
      dplyr::bind_rows(t3)


    i <- i + 1

    }

#--add assessment title
  a3 <-
    a2 %>%
    dplyr::mutate(title = a1$title %>% unique()) %>%
    dplyr::select(title, package_title, value_bin, score)




return(a3)

}
