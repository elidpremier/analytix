test_that("clean_names handles basic cases", {
  expect_equal(
    clean_names("Nom Patient"),
    "nom_patient"
  )
  
  expect_equal(
    clean_names("Âge/Ans"),
    "age_ou_ans"
  )
  
  expect_equal(
    clean_names("Température (°C)"),
    "temperature_c"
  )
})

test_that("names starting with numbers get prefixed", {
  expect_equal(
    clean_names("123_variable"),
    "v_123_variable"
  )
})

test_that("duplicates are made unique", {
  result <- clean_names(c("test", "test", "test"))
  expect_equal(length(result), 3)
  expect_true(length(unique(result)) == 3)
})

test_that("max_length truncation works", {
  long_name <- paste(rep("a", 100), collapse = "")
  result <- clean_names(long_name)
  expect_lte(nchar(result), 64)
})

test_that("empty input returns empty character", {
  expect_equal(
    clean_names(character(0)),
    character(0)
  )
})

test_that("non-character input throws error", {
  expect_error(
    clean_names(1:5),
    "`names_vector` must be a character vector"
  )
})