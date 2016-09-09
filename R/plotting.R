#' Generate a list with custom themes for scatter (y ~ x) plots
#'
#' @return A list containing ggplot2 themes
#' @import ggplot2
#' @export
get_scatter_themes <- function(){

  custom_themes <- list()

  custom_themes$scatter_facet_theme <-
    theme(text = element_text(size = 23),
          panel.background = element_rect(fill = "gray93"),
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.5, color = "gray50"),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(color = "black"),
          strip.background = element_rect(fill = "cadetblue2"),
          panel.margin = grid::unit(1, "lines"))

  custom_themes$scatter_theme <-
    theme(legend.key.size = grid::unit(0.6, "inches"),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          axis.title = element_text(color = "black", size = 25),
          panel.background = element_rect(fill = "white"),
          axis.text = element_text(size = 18, hjust = 0.5, vjust = 0.5),
          axis.ticks = element_line(size = 2),
          axis.ticks.length = grid::unit(0.3, "cm"))

  custom_themes$scatter_theme_background <-
    theme(plot.title = element_text(size = 28, color = "black"),
          legend.key.size = grid::unit(0.6, "inches"),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          axis.title = element_text(color = "black", size = 25),
          panel.background = element_rect(fill = "gray93"),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray70", size = 1),
          axis.text = element_text(size = 18, hjust = 0.5, vjust = 0.5),
          axis.ticks = element_line(size = 1),
          axis.ticks.length = grid::unit(0.3, "cm"))

  custom_themes
}
