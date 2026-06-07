utils::globalVariables(c("country", "total_plastic", "avg_aqi"))

#' Summarize Pollution for One Country
#'
#' Creates a small table showing total plastic waste and average AQI for one
#' selected country.
#'
#' @param air_vs_plastic A data frame containing country, total_plastic, and
#'   avg_aqi columns.
#' @param country_name The country to summarize. Must be a single non-empty
#'   string. Default is `"Philippines"`.
#'
#' @return A gt table showing pollution information for one country.
#'
#' @importFrom dplyr filter select
#' @importFrom gt gt tab_header cols_label fmt_number
#' @export
#'
#' @examples
#' \dontrun{
#'   summarize_country_pollution(air_vs_plastic_clean, country_name = "Philippines")
#' }
summarize_country_pollution <- function(air_vs_plastic, country_name = "Philippines") {
  validate_cols(
    air_vs_plastic,
    required = c("country", "total_plastic", "avg_aqi"),
    arg_name = "air_vs_plastic"
  )
  if (!is.character(country_name) || length(country_name) != 1 ||
      nchar(trimws(country_name)) == 0) {
    stop("`country_name` must be a single non-empty string.", call. = FALSE)
  }

  filtered <- dplyr::filter(air_vs_plastic, country == country_name)

  if (nrow(filtered) == 0) {
    warning("`country_name` \"", country_name,
            "\" was not found in the data. Returning empty table.",
            call. = FALSE)
  }

  filtered |>
    dplyr::select(country, total_plastic, avg_aqi) |>
    gt::gt() |>
    gt::tab_header(title = paste(country_name, "- Key Stats")) |>
    gt::cols_label(
      country       = "Country",
      total_plastic = "Total Plastic Waste",
      avg_aqi       = "Average AQI"
    ) |>
    gt::fmt_number(columns = total_plastic, decimals = 0) |>
    gt::fmt_number(columns = avg_aqi,       decimals = 1)
}
