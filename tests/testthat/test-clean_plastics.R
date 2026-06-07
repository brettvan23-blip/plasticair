test_that("clean_plastics removes Grand Total rows", {
  raw <- tibble::tibble(
    country        = c("Philippines", "Philippines", "philippines"),
    year           = c(2019, 2019, 2019),
    parent_company = c("Grand Total", "Unbranded", "Some Brand"),
    grand_total    = c(500, 200, 300),
    num_events     = c(3, 3, 3),
    volunteers     = c(100, 100, 100)
  )
  result <- clean_plastics(raw)
  expect_false(any(result$country == "Grand Total"))
})

test_that("clean_plastics aggregates to one row per country-year", {
  raw <- tibble::tibble(
    country        = c("Philippines", "Philippines"),
    year           = c(2019, 2019),
    parent_company = c("Brand A", "Brand B"),
    grand_total    = c(100, 200),
    num_events     = c(2, 2),
    volunteers     = c(50, 50)
  )
  result <- clean_plastics(raw)
  expect_equal(nrow(result), 1)
  expect_equal(result$total_plastic, 300)
})

test_that("clean_plastics returns a tibble", {
  raw <- tibble::tibble(
    country        = "India",
    year           = 2019,
    parent_company = "Nestlé",
    grand_total    = 50,
    num_events     = 1,
    volunteers     = 20
  )
  result <- clean_plastics(raw)
  expect_s3_class(result, "tbl_df")
})

# Validation checks
test_that("clean_plastics errors on non-data-frame input", {
  expect_error(clean_plastics("not a data frame"), "must be a data frame")
})

test_that("clean_plastics errors on missing required columns", {
  bad <- tibble::tibble(country = "X", year = 2019)
  expect_error(clean_plastics(bad), "missing required column")
})

test_that("clean_plastics warns and returns empty tibble when all rows filtered", {
  raw <- tibble::tibble(
    country        = "Philippines",
    year           = 2019,
    parent_company = "Grand Total",
    grand_total    = 500,
    num_events     = 1,
    volunteers     = 10
  )
  expect_warning(result <- clean_plastics(raw), "No rows remain")
  expect_equal(nrow(result), 0)
})
