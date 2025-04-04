
d <- system.file("extdata", "ABC-fairy dust lettuce.csv", package = "PESTO")

d1 <-
  readr::read_delim(d, delim = ";") %>%
  janitor::remove_empty()

d2 <-
  d1 %>%
  dplyr::select(short, value_metric, question)


pesto_impactcatsinfo <- d2

usethis::use_data(pesto_impactcatsinfo, overwrite = TRUE)
