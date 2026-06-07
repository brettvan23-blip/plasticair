test_that("summarize_country_pollution returns a gt_tbl", {
  dat <- tibble::tibble(
    country       = c("Philippines", "India"),
    total_plastic = c(1000, 800),
    avg_aqi       = c(90, 70)
  )
  result <- summarize_country_pollution(dat, country_name = "Philippines")
  expect_s3_class(result, "gt_tbl")
})

test_that("summarize_country_pollution errors on non-data-frame input", {
  expect_error(summarize_country_pollution("oops"), "must be a data frame")
})

test_that("summarize_country_pollution errors on missing columns", {
  bad <- tibble::tibble(country = "Philippines")
  expect_error(summarize_country_pollution(bad), "missing required column")
})

test_that("summarize_country_pollution errors on invalid country_name", {
  dat <- tibble::tibble(
    country = "Philippines", total_plastic = 100, avg_aqi = 80
  )
  expect_error(summarize_country_pollution(dat, country_name = 123),
               "single non-empty string")
  expect_error(summarize_country_pollution(dat, country_name = ""),
               "single non-empty string")
})

test_that("summarize_country_pollution warns when country not found", {
  dat <- tibble::tibble(
    country = "Philippines", total_plastic = 100, avg_aqi = 80
  )
  expect_warning(
    summarize_country_pollution(dat, country_name = "Narnia"),
    "was not found"
  )
})
