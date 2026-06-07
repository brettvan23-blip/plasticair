test_that("plot_air_vs_plastic returns a ggplot", {
  dat <- tibble::tibble(
    country       = c("Philippines", "India", "Mexico"),
    total_plastic = c(1000, 800, 600),
    avg_aqi       = c(90, 70, 40)
  )
  result <- plot_air_vs_plastic(dat)
  expect_s3_class(result, "ggplot")
})

test_that("plot_air_vs_plastic errors on non-data-frame input", {
  expect_error(plot_air_vs_plastic(NULL), "must be a data frame")
})

test_that("plot_air_vs_plastic errors on missing columns", {
  bad <- tibble::tibble(country = "X", total_plastic = 100)
  expect_error(plot_air_vs_plastic(bad), "missing required column")
})

test_that("plot_air_vs_plastic errors on empty data frame", {
  empty <- tibble::tibble(
    country = character(), total_plastic = numeric(), avg_aqi = numeric()
  )
  expect_error(plot_air_vs_plastic(empty), "no rows")
})
