test_that("run_plastic_aqi_lm returns an lm object", {
  dat <- tibble::tibble(
    country       = c("Philippines", "India", "China", "USA", "Brazil"),
    total_plastic = c(5000, 3000, 8000, 2000, 4000),
    avg_aqi       = c(90, 80, 110, 50, 75)
  )
  result <- run_plastic_aqi_lm(dat)
  expect_s3_class(result, "lm")
})

test_that("run_plastic_aqi_lm works with log_transform = TRUE", {
  dat <- tibble::tibble(
    country       = c("Philippines", "India", "China", "USA", "Brazil"),
    total_plastic = c(5000, 3000, 8000, 2000, 4000),
    avg_aqi       = c(90, 80, 110, 50, 75)
  )
  result <- run_plastic_aqi_lm(dat, log_transform = TRUE)
  expect_s3_class(result, "lm")
})

test_that("run_plastic_aqi_lm drops NA rows silently", {
  dat <- tibble::tibble(
    country       = c("Philippines", "India", "China", "USA", "Brazil"),
    total_plastic = c(5000, NA, 8000, 2000, 4000),
    avg_aqi       = c(90, 80, NA, 50, 75)
  )
  result <- run_plastic_aqi_lm(dat)
  expect_s3_class(result, "lm")
})

# Validation checks
test_that("run_plastic_aqi_lm errors on non-data-frame input", {
  expect_error(run_plastic_aqi_lm(42), "must be a data frame")
})

test_that("run_plastic_aqi_lm errors on missing required columns", {
  bad <- tibble::tibble(total_plastic = 100)
  expect_error(run_plastic_aqi_lm(bad), "missing required column")
})

test_that("run_plastic_aqi_lm errors on invalid log_transform", {
  dat <- tibble::tibble(
    total_plastic = c(100, 200, 300),
    avg_aqi       = c(50, 60, 70)
  )
  expect_error(run_plastic_aqi_lm(dat, log_transform = "yes"), "TRUE or FALSE")
})

test_that("run_plastic_aqi_lm errors when too few rows remain after NA removal", {
  dat <- tibble::tibble(
    total_plastic = c(100, NA),
    avg_aqi       = c(50, 60)
  )
  expect_error(run_plastic_aqi_lm(dat), "At least 3 complete observations")
})
