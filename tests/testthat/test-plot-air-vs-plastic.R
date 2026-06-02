test_that("plot_air_vs_plastic works", {
  dat <- tibble::tibble(
    country = c("Philippines", "India", "Mexico"),
    total_plastic = c(1000, 800, 600),
    avg_aqi = c(90, 70, 40)
  )

  result <- plot_air_vs_plastic(dat)

  expect_s3_class(result, "ggplot")
})
