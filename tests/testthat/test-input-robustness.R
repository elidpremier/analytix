test_that("clean_numeric gère les formats numériques français", {
  expect_warning(
    result <- clean_numeric(c("1\u202f234,5", "25%", "invalide")),
    "n'ont pas pu"
  )
  expect_equal(result, c(1234.5, 25, NA_real_))
})

test_that("clean_numeric signale les conversions invalides", {
  expect_warning(clean_numeric(c("10", "x")), "n'ont pas pu")
})

test_that("tbl_correlation signale les colonnes ignorées et refuse la sélection vide", {
  skip_if_not_installed("flextable")

  data <- data.frame(a = 1:3, b = 3:1, label = c("x", "y", "z"))
  expect_warning(
    result <- tbl_correlation(data, cols = names(data)),
    "non numériques"
  )
  expect_s3_class(result, "flextable")
  expect_error(
    tbl_correlation(data, cols = "label"),
    "Aucune colonne numérique"
  )
})

test_that("tbl_cross_unique inclut les valeurs manquantes explicitement", {
  skip_if_not_installed("flextable")

  data <- data.frame(
    outcome = c("Oui", "Non", NA, "Oui"),
    exposure = c("A", NA, "B", "A")
  )
  result <- tbl_cross_unique(
    data, outcome, exposure,
    include_na = TRUE,
    test = "fisher"
  )
  expect_true(any(is.na(result$data$Modalité) | result$data$Modalité == "<NA>"))
})

test_that("plot_pie accepte plus de sept modalités", {
  skip_if_not_installed("ggplot2")

  result <- plot_pie(
    factor(paste0("modalite_", seq_len(10))),
    title = "Dix modalités"
  )
  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})
