test_that("animate_fertility_country works", {
  dat <- tibble::tibble(
    year = c(2000, 2001, 2002),
    fertility_rate = c(3.5, 3.4, 3.3)
  )

  result <- animate_fertility_country(dat)

  expect_s3_class(result, "gif_image")
})
