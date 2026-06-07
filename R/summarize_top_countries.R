utils::globalVariables(c("total_plastic", "country", "avg_aqi", "volunteers"))

#' Summarize Top Countries by Plastic Waste
#'
#' Returns a formatted table of the top N countries ranked by total plastic
#' waste, optionally including average AQI and volunteer counts.
#'
#' @param air_vs_plastic A data frame containing at minimum `country` and
#'   `total_plastic` columns. Optionally `avg_aqi` and `volunteers`.
#' @param n Number of top countries to return. Must be a positive integer.
#'   Default is `10`.
#' @param include_aqi Logical. Whether to include the `avg_aqi` column if
#'   present. Default is `TRUE`.
#'
#' @return A `gt` table showing the top `n` countries by total plastic waste.
#'
#' @importFrom dplyr arrange desc slice_head select any_of
#' @importFrom gt gt tab_header cols_label fmt_number tab_style cell_text cells_column_labels
#' @export
#'
#' @examples
#' \dontrun{
#'   summarize_top_countries(air_vs_plastic_clean, n = 5)
#' }
summarize_top_countries <- function(air_vs_plastic, n = 10, include_aqi = TRUE) {
  validate_cols(
    air_vs_plastic,
    required = c("country", "total_plastic"),
    arg_name = "air_vs_plastic"
  )
  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }
  if (!is.logical(include_aqi) || length(include_aqi) != 1) {
    stop("`include_aqi` must be TRUE or FALSE.", call. = FALSE)
  }
  if (nrow(air_vs_plastic) == 0) {
    stop("`air_vs_plastic` has no rows.", call. = FALSE)
  }

  keep_cols <- c("country", "total_plastic")
  if (include_aqi && "avg_aqi" %in% names(air_vs_plastic)) {
    keep_cols <- c(keep_cols, "avg_aqi")
  }
  if ("volunteers" %in% names(air_vs_plastic)) {
    keep_cols <- c(keep_cols, "volunteers")
  }

  tbl <- air_vs_plastic |>
    dplyr::arrange(dplyr::desc(total_plastic)) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(dplyr::any_of(keep_cols))

  gt_tbl <- tbl |>
    gt::gt() |>
    gt::tab_header(
      title    = paste("Top", n, "Countries by Plastic Waste"),
      subtitle = "Ranked by total plastic items collected"
    ) |>
    gt::fmt_number(columns = total_plastic, decimals = 0) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )

  if ("avg_aqi" %in% names(tbl)) {
    gt_tbl <- gt_tbl |>
      gt::fmt_number(columns = avg_aqi, decimals = 1) |>
      gt::cols_label(
        country       = "Country",
        total_plastic = "Total Plastic",
        avg_aqi       = "Avg AQI"
      )
  } else {
    gt_tbl <- gt_tbl |>
      gt::cols_label(
        country       = "Country",
        total_plastic = "Total Plastic"
      )
  }

  gt_tbl
}
