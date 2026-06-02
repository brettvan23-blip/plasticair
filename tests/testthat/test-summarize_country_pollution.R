test_that("summarize_country_pollution works", {
  dat <- tibble::tibble(
    country = c("Philippines", "India"),
    total_plastic = c(1000, 800),
    avg_aqi = c(90, 70)
  )

  result <- summarize_country_pollution(dat, country_name = "Philippines")

  expect_s3_class(result, "gt_tbl")
})
