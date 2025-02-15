#--just to have as reference

VisualizeResultsDotFig <- function(f_dat1 = ipm_exampprocdat, f_dat2 = ipm_exampcombdat){


  #--needed things
  library(ggplot2)

  av1 <-"#A50026"
  av2 <-"#FDAE61"
  av3 <-"#FFFFBF"
  av4 <- "#ABD9E9"
  av5 <- "#313695"

  th1 <- theme(axis.title.y = element_text(size = rel(1.5)),
               axis.text.y = element_blank(),
               axis.text = element_text(size = rel(0.8)),
               axis.ticks.y = element_blank(),
               strip.background = element_rect(fill = "gray80", color = "black"),
               plot.title = element_text(hjust = 0.5, size = rel(2)),
               strip.text.y.left = element_text(angle = 0),
               strip.text.x = element_text(size = rel(1.5)),
               panel.background = element_rect(fill = "white"),
               axis.text.x = element_text(angle = 45, hjust = 1))


  th2 <- theme(axis.title.y = element_text(size = rel(1.5)),
               axis.text = element_text(size = rel(0.8)),
               plot.title = element_text(size = rel(2), hjust = 0.5),
               strip.background = element_rect(fill = "white", color = "black"),
               strip.text = element_text(size = rel(1.5)),
               axis.text.y = element_text(face = c("italic", "italic", "italic", "italic", "italic", "italic", "bold")))

  #--function

  tmp.d1 <-
    f_dat1 %>%
    dplyr::bind_rows(f_dat2 %>% select(-value_metric)) %>%
    dplyr::left_join(ipm_shortinfo) %>%
    dplyr::mutate(mini_facet = ifelse(short == "all", "Combined", "Individual"),
                  desc = paste0(value_metric, " (", weight, "%)"))

  tmp.title <- tmp.d1$title %>% unique()

  p1 <-
    tmp.d1 %>%
    dplyr::arrange(scenario, weight) %>%
    dplyr::mutate(value_binF = as.factor(value_bin),
                  short = stringr::str_to_sentence(short),
                  descF = forcats::fct_inorder(desc),
                  scenarioF = factor(scenario, levels = c("CCP", "IPM"))) %>%
    dplyr::arrange(scenario, descF) %>%
    ggplot2::ggplot(aes(value_bin, reorder(desc, weight))) +
    geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 6.5, ymax = 7.5), fill = "white") +
    geom_point(aes(fill = value_binF, size = score*10, alpha = mini_facet), show.legend = F, pch = 21, stroke = 1.2) +
    scale_size(range = c(0, 14)) +
    scale_fill_manual(values = c(av1, av2, av3, av4, av5)) +
    scale_alpha_manual(values = c(1, 0.15)) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(0.5, 5.5),
      breaks = c(1, 2, 3, 4, 5),
      labels = c("Very low value", "Low value", "Medium value", "Highly valuable", "Very highly valuable"),
      #position = "top"
    ) +
    labs(x = NULL,
         y = NULL,
         title = stringr::str_wrap(tmp.title, 40)) +
    th2 +
    facet_grid(.~scenarioF) +
    theme(panel.background = element_rect(fill = 'gray95', color = 'black'),
          panel.grid.major.y = element_line(color = "gray"),
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(p1)

}
