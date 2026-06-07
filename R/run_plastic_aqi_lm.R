utils::globalVariables(c("total_plastic", "avg_aqi", "estimate", "std_error", "statistic", "p_value"))

#' Fit a Linear Model: AQI Predicted by Plastic Waste
#'
#' Fits a simple linear regression of average AQI on total plastic waste
#' across countries, prints a tidy summary table, and returns the model object
#' invisibly for further use.
#'
#' @param air_vs_plastic A data frame containing `total_plastic` and `avg_aqi`
#'   columns, as produced by joining the plastics and air quality datasets.
#' @param log_transform Logical. If `TRUE`, log-transforms both variables
#'   before fitting (useful when distributions are right-skewed). Default
#'   is `FALSE`.
#'
#' @return Invisibly returns the fitted `lm` object. Also prints a `gt` table
#'   of model coefficients with standard errors and p-values.
#'
#' @importFrom dplyr filter mutate
#' @importFrom tibble tibble
#' @importFrom gt gt tab_header cols_label fmt_number fmt_scientific tab_style cell_text cells_column_labels
#' @importFrom stats lm
#' @export
#'
#' @examples
#' \dontrun{
#'   model <- run_plastic_aqi_lm(air_vs_plastic_clean)
#'   model <- run_plastic_aqi_lm(air_vs_plastic_clean, log_transform = TRUE)
#' }
run_plastic_aqi_lm <- function(air_vs_plastic, log_transform = FALSE) {
  validate_cols(
    air_vs_plastic,
    required = c("total_plastic", "avg_aqi"),
    arg_name = "air_vs_plastic"
  )
  if (!is.logical(log_transform) || length(log_transform) != 1) {
    stop("`log_transform` must be TRUE or FALSE.", call. = FALSE)
  }

  df <- air_vs_plastic |>
    dplyr::filter(
      !is.na(total_plastic),
      !is.na(avg_aqi),
      total_plastic > 0,
      avg_aqi > 0
    )

  if (nrow(df) < 3) {
    stop("At least 3 complete observations are required to fit a model. ",
         "Only ", nrow(df), " found after removing NAs.", call. = FALSE)
  }

  if (log_transform) {
    df <- df |>
      dplyr::mutate(
        total_plastic = log(total_plastic),
        avg_aqi       = log(avg_aqi)
      )
    x_label <- "log(Total Plastic)"
    y_label <- "log(Avg AQI)"
  } else {
    x_label <- "Total Plastic"
    y_label <- "Avg AQI"
  }

  fit <- lm(avg_aqi ~ total_plastic, data = df)
  s   <- summary(fit)
  cf  <- as.data.frame(s$coefficients)

  result_tbl <- tibble::tibble(
    term      = rownames(cf),
    estimate  = cf[, "Estimate"],
    std_error = cf[, "Std. Error"],
    statistic = cf[, "t value"],
    p_value   = cf[, "Pr(>|t|)"]
  )

  gt_tbl <- result_tbl |>
    gt::gt() |>
    gt::tab_header(
      title    = paste("Linear Regression:", y_label, "~", x_label),
      subtitle = paste0(
        "R\u00b2 = ", round(s$r.squared, 3),
        "  |  Adj. R\u00b2 = ", round(s$adj.r.squared, 3),
        "  |  n = ", nrow(df)
      )
    ) |>
    gt::cols_label(
      term      = "Term",
      estimate  = "Estimate",
      std_error = "Std. Error",
      statistic = "t Statistic",
      p_value   = "p-value"
    ) |>
    gt::fmt_number(columns = c(estimate, std_error, statistic), decimals = 4) |>
    gt::fmt_scientific(columns = p_value, decimals = 3) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )

  print(gt_tbl)
  invisible(fit)
}
