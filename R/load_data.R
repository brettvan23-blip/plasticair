#' Loads the Break Free From Plastic dataset.
#'
#' @return A tibble containing plastic waste data.
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' load_data()
load_data <- function() {
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv"
  )
}

