test_that("clean_names works on character vector", {
  expect_equal(
    clean_names("Nom Patient"),
    "nom_patient"
  )
  expect_equal(
    clean_names(c("Âge/Ans", "123_test")),
    c("age_ou_ans", "v_123_test")
  )
})

test_that("clean_names works on data.frame", {
  df <- data.frame("Nom Patient" = 1:2, "Âge/Ans" = 3:4)
  result <- clean_names(df)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("nom_patient", "age_ou_ans"))
  expect_equal(result$nom_patient, 1:2)  # Data preserved
})

test_that("clean_names preserves data.frame structure", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  result <- clean_names(df)

  expect_equal(dim(result), dim(df))
  expect_equal(result$a, df$a)
  expect_equal(result$b, df$b)
})

test_that("clean_names throws error on invalid input", {
  expect_error(clean_names(123), "`x` must be either a character vector or a data.frame")
  expect_error(clean_names(list(a = 1)), "`x` must be either a character vector or a data.frame")
})

test_that("parameters are passed correctly to data.frame method", {
  df <- data.frame("123_long_name_here" = 1:2)
  result <- clean_names(df, max_length = 10, prefix = "x_")
  expect_equal(names(result), "x_123_long")  # Truncated to 10 chars
})
