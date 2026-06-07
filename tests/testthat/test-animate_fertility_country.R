test_that("animate_fertility_country works", {
  dat <- tibble::tibble(
    year           = c(2000, 2001, 2002),
    fertility_rate = c(3.5, 3.4, 3.3)
  )
  result <- animate_fertility_country(dat)
  expect_s3_class(result, "gif_image")
})

test_that("animate_fertility_country errors on non-data-frame input", {
  expect_error(animate_fertility_country("nope"), "must be a data frame")
})

test_that("animate_fertility_country errors on missing columns", {
  bad <- tibble::tibble(year = 2000)
  expect_error(animate_fertility_country(bad), "missing required column")
})

test_that("animate_fertility_country errors when year is not numeric", {
  dat <- tibble::tibble(
    year           = c("2000", "2001", "2002"),
    fertility_rate = c(3.5, 3.4, 3.3)
  )
  expect_error(animate_fertility_country(dat), "`year` column must be numeric")
})

test_that("animate_fertility_country errors when fertility_rate is not numeric", {
  dat <- tibble::tibble(
    year           = c(2000, 2001, 2002),
    fertility_rate = c("high", "medium", "low")
  )
  expect_error(animate_fertility_country(dat),
               "`fertility_rate` column must be numeric")
})

test_that("animate_fertility_country errors with fewer than 2 rows", {
  dat <- tibble::tibble(year = 2000, fertility_rate = 3.5)
  expect_error(animate_fertility_country(dat), "at least 2 rows")
})
