utils::globalVariables(c(
  "country", "year", "parent_company", "grand_total",
  "num_events", "volunteers", "total_plastic"
))

#' Clean and Wrangle the Plastics Dataset
#'
#' Filters out summary rows, removes countries with missing or zero total
#' plastic counts, standardizes country name casing, and aggregates to one
#' row per country per year. Uses `data.table` internally for fast aggregation.
#'
#' @param plastics A data frame containing the raw Break Free From Plastic
#'   dataset, as returned by `load_data()`. Expected columns: `country`,
#'   `year`, `parent_company`, `grand_total`, `num_events`, `volunteers`.
#'
#' @return A tibble with one row per country-year, containing columns
#'   `country`, `year`, `total_plastic`, `num_events`, and `volunteers`.
#'
#' @importFrom data.table as.data.table setDT
#' @importFrom dplyr filter mutate group_by summarise ungroup
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   raw <- load_data()
#'   cleaned <- clean_plastics(raw)
#' }
clean_plastics <- function(plastics) {
  validate_cols(
    plastics,
    required  = c("country", "year", "parent_company", "grand_total",
                  "num_events", "volunteers"),
    arg_name  = "plastics"
  )

  # Convert to data.table for fast in-memory operations
  dt <- data.table::as.data.table(plastics)

  # Use dplyr for filtering/mutating to avoid data.table NSE scoping issues
  filtered <- tibble::as_tibble(dt) |>
    dplyr::filter(
      !is.na(parent_company),
      parent_company != "Grand Total",
      !is.na(grand_total),
      grand_total > 0
    ) |>
    dplyr::mutate(country = tools::toTitleCase(tolower(country)))

  if (nrow(filtered) == 0) {
    warning("No rows remain after filtering. Returning empty tibble.",
            call. = FALSE)
    return(tibble::tibble(
      country = character(), year = integer(),
      total_plastic = numeric(), num_events = integer(),
      volunteers = integer()
    ))
  }

  filtered |>
    dplyr::group_by(country, year) |>
    dplyr::summarise(
      total_plastic = sum(grand_total, na.rm = TRUE),
      num_events    = max(num_events,  na.rm = TRUE),
      volunteers    = max(volunteers,  na.rm = TRUE),
      .groups       = "drop"
    ) |>
    tibble::as_tibble()
}
