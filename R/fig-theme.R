#' Testy function #1
#'
#' @returns a theme
#' @export

pesto_th1 <- ggplot2::theme(axis.title.y = ggplot2::element_text(size = rel(1.5)),
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
