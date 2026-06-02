utils::globalVariables(c("year", "fertility_rate"))

#' Animate Fertility Rate Over Time
#'
#' Creates an animated line plot howing fertility rate over time for the
#' Philippines. The dashed horizontal line shows the replacement rate.
#'
#' @param combined A data frame containing year and fertility_rate columns.
#'
#' @return An animated GIF showing fertility rate over time.
#'
#' @export
animate_fertility_country <- function(combined) {

  p <- ggplot2::ggplot(
    combined,
    ggplot2::aes(x = year, y = fertility_rate, group = 1)
  ) +
    ggplot2::geom_line(
      color = "steelblue",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      color = "red",
      size = 3
    ) +
    ggplot2::geom_hline(
      yintercept = 2.1,
      color = "red",
      linetype = "dashed"
    ) +
    ggplot2::annotate(
      "text",
      x = min(combined$year),
      y = 2.1,
      label = "Replacement rate (2.1)",
      color = "red",
      vjust = -0.5,
      hjust = 0,
      size = 3.5
    ) +
    ggplot2::labs(
      title = "Fertility Rate in the Philippines Over Time",
      subtitle = "Year: {frame_along}",
      x = "Year",
      y = "Births per Woman"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.8
      )
    ) +
    gganimate::transition_reveal(along = year)

  anim <- gganimate::animate(
    p,
    duration = 10,
    fps = 20,
    renderer = gganimate::gifski_renderer()
  )

  anim
}
