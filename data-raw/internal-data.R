# all data to be used internally should be created here
#--last updated 4 april 2025, making sure terminology is consistent (package 1, value, impact, etc)

rm(list = ls())

# internal file - good input file ---------------------------------------------------------

a1 <- system.file("extdata", "ABC-fairy dust lettuce.csv", package = "PESTO")

a2 <-
  readr::read_delim(a1, delim = ";") %>%
  select(-question, -value_metric)

internal_exinput <- a2


# internal file - crappy input file for testing---------------------------------------------------------

a3 <-
  readr::read_delim(a1, delim = ";") %>%
  dplyr::mutate(weight = ifelse(short == 'crop value', 45, weight)) %>%
  dplyr::rename(package1_weirdcolname = package1_text)

internal_exinputBAD <- a3


# c. impact factor info ---------------------------------------------------

c <- system.file("extdata", "ABC-fairy dust lettuce.csv", package = "PESTO")

c1 <-
  read_delim(d, delim = ";") %>%
  janitor::remove_empty()

c2 <-
  c1 %>%
  select(short, value_metric, question)


internal_impactcatsinfo <- c2

# internal file - binned beta distributions ---------------------------------------------------------

d1 <- system.file("extdata", "byhand_distribution-cheat-sheet3.xlsx", package = "PESTO")

d2 <- readxl::read_excel(d1, skip = 5)

#--process into tidy data
d3 <-
  d2 %>%
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
d3 %>%
  dplyr::mutate(rating_textF = forcats::fct_inorder(rating_text),
                confidenceF = forcats::fct_inorder(confidence)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(rating_textF ~confidenceF)


internal_binned_betas <- d3


# write all internal data -----------------------------------------------------------------


usethis::use_data(
  internal_exinput,
  internal_exinputBAD,
  internal_impactcatsinfo,
  internal_binned_betas,
  internal = TRUE, overwrite = TRUE)
