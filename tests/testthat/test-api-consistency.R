test_that("tbl_roc respecte le contrat analytix_table", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("pROC")

  data <- data.frame(
    outcome = c(0, 0, 1, 1, 0, 1),
    score = c(0.1, 0.2, 0.7, 0.9, 0.3, 0.8)
  )
  result <- tbl_roc(data, outcome = "outcome", predictor = "score",
                    return_plot = FALSE)

  expect_s3_class(result, "analytix_table")
  expect_s3_class(result$flextable, "flextable")
  expect_true(is.data.frame(result$data))
  expect_true(is.numeric(result$auc))
})

test_that("tbl_km respecte le contrat analytix_table", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("survival")

  data <- data.frame(time = c(1, 2, 3, 4), event = c(1, 1, 0, 1))
  result <- tbl_km(data, time_var = "time", event_var = "event")

  expect_s3_class(result, "analytix_table")
  expect_s3_class(result$flextable, "flextable")
  expect_true(is.data.frame(result$data))
})

test_that("l'alias format_flextable émet un seul avertissement", {
  skip_if_not_installed("flextable")

  ft <- flextable::flextable(data.frame(x = 1))
  warnings <- character()
  withCallingHandlers(
    format_flextable(ft),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1)
  expect_match(warnings, "format_flextable")
})
