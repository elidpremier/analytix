library(testthat)
library(analytix)

test_that("analyse_descriptive_multiple works with mtcars", {
  res <- analyse_descriptive_multiple(mtcars, vars = c("mpg", "cyl", "am"))
  expect_type(res, "list")
  expect_named(res, c("mpg", "cyl", "am"))
  expect_s3_class(res$mpg, "descr_numeric")
  expect_s3_class(res$cyl, "freq_table")
  expect_s3_class(res$am, "descr_binary")
})

test_that("descr_numeric works", {
  res <- descr_numeric(mtcars, mpg)
  expect_s3_class(res$flextable, "flextable")
})

test_that("cross_multi works", {
  res <- cross_multi(mtcars, am, c("cyl", "vs"))
  expect_s3_class(res, "flextable")
})
