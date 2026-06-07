#' Load the merged plastic waste and air quality dataset
#'
#' Loads the pre-built merged dataset from the package's inst/extdata folder.
#'
#' @return A tibble containing country-level plastic waste and AQI data.
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' load_data()
load_data <- function() {
  path <- system.file("extdata", "air_vs_plastic.csv", package = "plasticair")
  if (path == "") {
    stop("Data file not found. Try reinstalling the package.", call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE)
}
