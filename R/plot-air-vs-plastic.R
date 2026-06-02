utils::globalVariables(c("total_plastic", "avg_aqi", "aqi_category", "country"))

#' Plot Plastic Waste Against Air Quality
#'
#' Creates a bubble plot comparing total plastic waste and average AQI across
#' countries. Bubble size represents total plastic waste, and color represents
#' AQI category.
#'
#' @param air_vs_plastic_clean A data frame containing country, total_plastic,
#' and avg_aqi columns.
#'
#' @return A ggplot object showing total plastic waste and average AQI.
#'
#' @export
plot_air_vs_plastic <- function(air_vs_plastic_clean) {
  air_vs_plastic_clean |>
    dplyr::mutate(aqi_category = dplyr::case_when(
      avg_aqi <= 50  ~ "Good",
      avg_aqi <= 100 ~ "Moderate",
      avg_aqi <= 150 ~ "Unhealthy for Sensitive Groups",
      TRUE           ~ "Unhealthy"
    )) |>
    ggplot2::ggplot(ggplot2::aes(x = total_plastic, y = avg_aqi,
                                 size = total_plastic, color = aqi_category)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = country),
      size = 2.5,
      max.overlaps = 10
    ) +
    ggplot2::scale_size_continuous(range = c(2, 12)) +
    ggplot2::labs(
      title = "Plastic Waste and Air Quality Across Countries",
      x = "Total Plastic Waste",
      y = "Average AQI",
      color = "AQI Category"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::guides(size = "none")
}
