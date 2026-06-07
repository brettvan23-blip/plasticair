test_that("summarize_top_countries returns a gt table", {
  dat <- tibble::tibble(
    country       = c("Philippines", "India", "China", "USA"),
    total_plastic = c(5000, 3000, 8000, 2000),
    avg_aqi       = c(90, 80, 110, 50)
  )
  result <- summarize_top_countries(dat, n = 3)
  expect_s3_class(result, "gt_tbl")
})

test_that("summarize_top_countries respects n argument", {
  dat <- tibble::tibble(
    country       = paste0("Country", 1:10),
    total_plastic = seq(1000, 10000, by = 1000),
    avg_aqi       = seq(30, 120, length.out = 10)
  )
  result <- summarize_top_countries(dat, n = 4)
  expect_equal(nrow(result$`_data`), 4)
})

# Validation checks
test_that("summarize_top_countries errors on non-data-frame input", {
  expect_error(summarize_top_countries(list(a = 1)), "must be a data frame")
})

test_that("summarize_top_countries errors on missing required columns", {
  bad <- tibble::tibble(country = "X")
  expect_error(summarize_top_countries(bad), "missing required column")
})

test_that("summarize_top_countries errors on invalid n", {
  dat <- tibble::tibble(country = "X", total_plastic = 100)
  expect_error(summarize_top_countries(dat, n = -1), "positive integer")
  expect_error(summarize_top_countries(dat, n = "five"), "positive integer")
})

test_that("summarize_top_countries errors on empty data frame", {
  dat <- tibble::tibble(country = character(), total_plastic = numeric())
  expect_error(summarize_top_countries(dat), "no rows")
})
