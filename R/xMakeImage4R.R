
MakeImage4R <- function(f_dat = pesto_exinputOLD){

  #--A. use lookup beta distributions to match to elicitation data----------------------
  #--make sure input data is correct
  a_dat1 <-
    f_dat %>%
    janitor::clean_names() %>%
    tidyr::fill(title) %>%
    dplyr::mutate(weight = as.numeric(weight))

  t1 <- (setdiff(names(a_dat1), names(internal_exinput)))

  m1 <- NULL
  if(length(t1) > 0){

    m1 <- paste("Hi! This column name doesn't match the template:", t1)
    stop(m1)
  }

  a_dat2 <-
    a_dat1 %>%
    dplyr::select(-question, -value_metric) %>%
    dplyr::mutate_if(is.character, stringr::str_to_lower)


  if(round(sum(a_dat1$weight)) != 100){
    stop("Hi! Weights do not add up to 100, please check them")
  }

  #----start pre-processing---------------------------------

  #--get list of the six metrics
  t.cats <-
    a_dat2 %>%
    dplyr::pull(short)

  # #--separate data into pkg1 and pkg2 data
  a_dat_pkg1 <-
    a_dat2 %>%
    dplyr::select(title, weight, short,
                  rating_text = package1_text,
                  confidence = package1_conf)

  t.sum_pkg1 <- NULL

  suppressMessages(
    for(i in 1:length(t.cats)){

      #--get single metric rating and confidence combo
      t1_pkg1 <-
        a_dat_pkg1 %>%
        dplyr::filter(short == t.cats[i])

      #--merge with beta dist for that combo
      t2_pkg1 <-
        t1_pkg1 %>%
        dplyr::left_join(
          internal_binnedbetas %>%
            dplyr::filter(rating_text == t1_pkg1$rating_text,
                          confidence == t1_pkg1$confidence)
        ) %>%
        dplyr::mutate(scenario = "Package #1")

      t.sum_pkg1 <-
        dplyr::bind_rows(t.sum_pkg1, t2_pkg1)

      i <- i + 1

    }
  )

    #--now do ipm data--
    a_dat_pkg2 <-
      a_dat2 %>%
      dplyr::select(title, weight, short,
             rating_text = package2_text,
             confidence = package2_conf)


    t.sum_pkg2 <- NULL

    suppressMessages(
      for(i in 1:length(t.cats)){

        #--get single metric rating and confidence combo
        t1_pkg2 <-
          a_dat_pkg2 %>%
          dplyr::filter(short == t.cats[i])

        #--merge with beta dist for that combo
        t2_pkg2 <-
          t1_pkg2 %>%
          dplyr::left_join(
            internal_binnedbetas %>%
              dplyr::filter(rating_text == t1_pkg2$rating_text,
                            confidence == t1_pkg2$confidence)
          ) %>%
          dplyr::mutate(scenario = "Package #2")

        t.sum_pkg2 <-
          dplyr::bind_rows(t.sum_pkg2, t2_pkg2)

        i <- i + 1

      }
      )

    dat.part.a <- dplyr::bind_rows(t.sum_pkg1, t.sum_pkg2)

    #--B. make a summary data table----------------------------------


    #-- t.cats from above lists the impact categories

    t.scen <-
        dat.part.a %>%
        dplyr::pull(scenario) %>%
        unique()

      value_bin_options <-
        dat.part.a %>%
        dplyr::select(value_bin) %>%
        dplyr::distinct()

      #--first scenario
      b_dat1 <-
        dat.part.a %>%
        dplyr::filter(scenario == t.scen[1])

      bayes.value.vector1 <- NULL

      for(k in 1:length(t.cats)){

        tmp.impact <- t.cats[k]

        tmp.df <-
          b_dat1 %>%
          dplyr::select(short, value_bin, score) %>%
          dplyr::filter(short == tmp.impact)

        tmp.wt <-
          a_dat2 %>%
          dplyr::select(weight, short) %>%
          dplyr::filter(short == tmp.impact) %>%
          dplyr::distinct() %>%
          dplyr::pull(weight)

        tmp.samp <-
          sample(x = tmp.df$value_bin, prob = tmp.df$score, size = 10000*tmp.wt, replace = TRUE)

        bayes.value.vector1 <- c(bayes.value.vector1, tmp.samp)

        k <- k + 1

      }

      dat.part.b1 <-
        value_bin_options %>%
        dplyr::left_join(
          tibble::tibble(value_bin = bayes.value.vector1) %>%
            dplyr::group_by(value_bin) %>%
            dplyr::summarise(score = dplyr::n()/10000)
        ) %>%
        dplyr::mutate(
          score = ifelse(is.na(score), 0, score),
          weight = 100,
          confidence = "-",
          value_metric  = "All categories combined",
          short = "all",
          scenario = t.scen[1])

      #--second scenario
      b_dat2 <-
        dat.part.a %>%
        dplyr::filter(scenario == t.scen[2])

      bayes.value.vector2 <- NULL

      for(k in 1:length(t.cats)){

        tmp.impact <- t.cats[k]

        tmp.df <-
          b_dat2 %>%
          dplyr::select(short, value_bin, score) %>%
          dplyr::filter(short == tmp.impact)

        tmp.wt <-
          a_dat2 %>%
          dplyr::select(weight, short) %>%
          dplyr::filter(short == tmp.impact) %>%
          dplyr::distinct() %>%
          dplyr::pull(weight)

        tmp.samp <-
          sample(x = tmp.df$value_bin, prob = tmp.df$score, size = 10000*tmp.wt, replace = TRUE)

        bayes.value.vector2 <- c(bayes.value.vector2, tmp.samp)

        k <- k + 1

      }


      dat.part.b2 <-
        value_bin_options %>%
        dplyr::left_join(
          tibble::tibble(value_bin = bayes.value.vector2) %>%
            dplyr::group_by(value_bin) %>%
            dplyr::summarise(score = dplyr::n()/10000)
        ) %>%
        dplyr::mutate(
          score = ifelse(is.na(score), 0, score),
          weight = 100,
          confidence = "-",
          value_metric  = "All categories combined",
          short = "all",
          scenario = t.scen[2])

      dat.part.b <-
        dplyr::bind_rows(dat.part.b1, dat.part.b2) %>%
        dplyr::mutate(title = dat.part.a$title %>% unique()) %>%
        dplyr::select(title, scenario, short, value_metric, value_bin, score)

      ##-----visualize----------------------------------

       av1 <-"#A50026"
      av2 <-"#FDAE61"
      av3 <-"#FFFFBF"
      av4 <- "#ABD9E9"
      av5 <- "#313695"

      th1 <- ggplot2::theme(axis.title.y = ggplot2::element_text(size = rel(1.5)),
                   axis.text.y = ggplot2::element_text(color = "gray80"),
                   axis.text = ggplot2::element_text(size = rel(0.8)),
                   #axis.ticks.y = element_blank(),
                   strip.background = ggplot2::element_rect(fill = "gray80", color = "black"),
                   plot.title = ggplot2::element_text(hjust = 0.5, size = rel(2)),
                   strip.text.y.left = ggplot2::element_text(angle = 0),
                   strip.text.x = ggplot2::element_text(size = rel(1.5)),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.grid.major.y = ggplot2::element_line(color = "gray80"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      dat.part.c <-
        dat.part.a %>%
        dplyr::left_join(internal_impactcatsinfo) %>%
        dplyr::bind_rows(dat.part.b %>%
                           dplyr::mutate(weight = 100)) %>%
        dplyr::mutate(desc = paste0(value_metric, " (", weight, "%)"))

      t.title <- a_dat1$title %>% unique() %>% stringr::str_to_sentence()


      p1 <-
        dat.part.c %>%
        dplyr::filter(short != "all") %>%
        dplyr::arrange(scenario, -weight) %>%
        dplyr::mutate(value_binF = as.factor(value_bin),
                      value_metric = stringr::str_to_sentence(value_metric),
                      descF = forcats::fct_inorder(desc),
                      scenarioF = factor(scenario, levels = c("Package #1",
                                                              "Package #2"))) %>%
        ggplot(aes(value_bin, score)) +
        geom_col(aes(fill = value_binF), linewidth = 1, color = "black", show.legend = F) +
        scale_fill_manual(values = c(av1, av2, av3, av4, av5)) +
        scale_y_continuous(limits = c(0, 100),
                           breaks = c(0, 25, 50, 75, 100),
                           position = "right") +
        scale_x_continuous(
          breaks = c(1, 2, 3, 4, 5),
          labels = c("Very low", "Low", "Medium", "High", "Very high")) +
        labs(y = NULL,
             x = "Performance") +
        th1 +
        facet_grid(descF ~ scenarioF, labeller = label_wrap_gen(width = 20), switch = "y")



      dat.part.c.util <-
        dat.part.c %>%
        dplyr::filter(short == "all") %>%
        dplyr::arrange(scenario, -weight) %>%
        dplyr::select(scenario, score, value_bin) %>%
        dplyr::mutate(value = dplyr::case_when(
          value_bin == 5 ~ 100,
          value_bin == 4 ~ 75,
          value_bin == 3 ~ 50,
          value_bin == 2 ~ 25,
          value_bin == 1 ~ 0,
          TRUE ~ 9999
        )) %>%
          dplyr::group_by(scenario) %>%
          dplyr::summarise(util = round(weighted.mean(x = value, w = score), 0)) %>%
        dplyr::mutate(scenarioF = factor(scenario, levels = c("Package #1",
                                                              "Package #2")),
                      util_label = paste0("Utility = ", util, "%"))

      p2 <-
        dat.part.c %>%
        dplyr::filter(short == "all") %>%
        dplyr::mutate(desc = "Weighted combination of all categories") %>%
        dplyr::arrange(scenario, -weight) %>%
        dplyr::mutate(value_binF = as.factor(value_bin),
                      value_metric = stringr::str_to_sentence(value_metric),
                      descF = forcats::fct_inorder(desc),
                      scenarioF = factor(scenario, levels = c("Package #1",
                                                              "Package #2"))) %>%
        ggplot(aes(value_bin, score)) +
        geom_col(aes(fill = value_binF), linewidth = 1, color = "black", show.legend = F) +
        geom_text(data = dat.part.c.util,
                  aes(x = 3, y = 90, label = util_label,
                      fontface = "italic"),
                  hjust = 0.5) +
        scale_fill_manual(values = c(av1, av2, av3, av4, av5)) +
        scale_y_continuous(limits = c(0, 100),
                           breaks = c(0, 25, 50, 75, 100),
                           position = "right") +
        scale_x_continuous(
          breaks = c(1, 2, 3, 4, 5),
          labels = c("Very low", "Low", "Medium", "High", "Very high")) +
        labs(y = NULL,
             x = "Performance") +
        th1 +
        facet_grid(descF ~ scenarioF, labeller = label_wrap_gen(width = 20), switch = "y")


      layout <- "
AABB
AA##
AA##"

      theme_border <-
        ggplot2::theme_gray() +
        ggplot2::theme(plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
              plot.title = element_text(size = 20, hjust = 0.5))

      t.title <- a_dat1$title %>% unique()


      p_all <-
        p1 +
        p2 +
        patchwork::plot_layout(design = layout) +
        patchwork::plot_annotation(title =   stringr::str_wrap(t.title, 40),
                                   theme = theme_border)

      return(p_all)


}
