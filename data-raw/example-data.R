# all data to be used internally and available externally should be created here
#--last updated 5 april 2025, remade example data formats

rm(list = ls())


# a. good input file example---------------------------------------------------------

a1 <- system.file("extdata", "ABC-fairy dust lettuce-long.csv", package = "PESTO")

a2 <-
  readr::read_delim(a1, delim = ";")

#--internal data
internal_exinput <- a2

#--externally available dataset
pesto_exinput <- a2
usethis::use_data(pesto_exinput, overwrite = TRUE)

# b. crappy input file example for testing fxns---------------------------------------------------------

b1 <-
  readr::read_delim(a1, delim = ";") %>%
  dplyr::mutate(weight = ifelse(assessment_category == 'crop value losses', 45, weight)) %>%
  dplyr::rename(weirdcolname = package_title)

internal_exinputBAD <- b1

# c. impact factor info ---------------------------------------------------

c <- system.file("extdata", "byhand_assessment-question-key.csv", package = "PESTO")

c1 <-
  readr::read_delim(c, delim = ";")

#--internal data
internal_assessmentkey <- c1

#--external data
pesto_assessmentkey <- c1
usethis::use_data(pesto_assessmentkey, overwrite = TRUE)

# d. binned beta distributions ---------------------------------------------------------

d <- system.file("extdata", "byhand_distribution-cheat-sheet3.xlsx", package = "PESTO")

d1 <- readxl::read_excel(d, skip = 5)

#--process into tidy data
d2 <-
  d1 %>%
  dplyr::select(-tot) %>%
  tidyr::fill(confidence) %>%
  janitor::clean_names() %>%
  tidyr::pivot_longer(x1:x5) %>%
  dplyr::mutate(name = readr::parse_number(name),
                value_bin = dplyr::case_when(
                  name == 1 ~ 5,
                  name == 2 ~ 4,
                  name == 3 ~ 3,
                  name == 4 ~ 2,
                  name == 5 ~ 1,
                  TRUE ~ 9999
                )) %>%
  dplyr::rename(score = value) %>%
  dplyr::select(-name) %>%
  dplyr::mutate_if(is.character, stringr::str_to_lower)


#--check it
d2 %>%
  dplyr::mutate(rating_textF = forcats::fct_inorder(rating_text),
                confidenceF = forcats::fct_inorder(confidence)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(rating_textF ~confidenceF)


internal_binnedbetas <- d2



# e. ex output from MakePestoDF -------------------------------------------

# pesto_exdf <- MakePestoDF()
# usethis::use_data(pesto_exdf, overwrite = TRUE)
#
# pesto_exsumdf <- SummarizePestoDF()
# usethis::use_data(pesto_exsumdf, overwrite = TRUE)
#
# pesto_exutil <- CalcPestoUtil()
# usethis::use_data(pesto_exutil, overwrite = TRUE)

# write all internal data -----------------------------------------------------------------

usethis::use_data(
  internal_exinput,
  internal_exinputBAD,
  internal_assessmentkey,
  internal_binnedbetas,
  internal = TRUE, overwrite = TRUE)
