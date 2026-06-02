utils::globalVariables(c("country", "total_plastic", "avg_aqi"))

#' Summarize Pollution for One Country
#'
#' Creates a small table showing total plastic waste and average AQI for one
#' selected country.
#'
#' @param air_vs_plastic A data frame containing country, total_plastic, and
#' avg_aqi columns.
#' @param country_name The country to summarize. Default is "Philippines".
#'
#' @return A gt table showing pollution information for one country.
#'
#' @export
summarize_country_pollution <- function(air_vs_plastic, country_name = "Philippines") {
  air_vs_plastic |>
    dplyr::filter(country == country_name) |>
    dplyr::select(country, total_plastic, avg_aqi) |>
    gt::gt() |>
    gt::tab_header(title = "Philippines - Key Stats") |>
    gt::cols_label(
      country       = "Country",
      total_plastic = "Total Plastic Waste",
      avg_aqi       = "Average AQI"
    ) |>
    gt::fmt_number(columns = total_plastic, decimals = 0) |>
    gt::fmt_number(columns = avg_aqi, decimals = 1)
}
