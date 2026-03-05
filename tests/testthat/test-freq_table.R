test_that("freq_table fonctionne avec des données catégorielles", {
  data <- data.frame(
    categorie = factor(c("A", "B", "A", "B", "A", "B")),
    valeur = c(1, 2, 3, 4, 5, 6)
  )

  # Test avec variable factor
  result <- freq_table(data, categorie)
  expect_s3_class(result, "freq_table")
  expect_true("flextable" %in% names(result))
  expect_true("data" %in% names(result))

  # Test avec variable numérique discrète
  result_num <- freq_table(data, valeur)
  expect_s3_class(result_num, "freq_table")
})

test_that("freq_table gère les valeurs manquantes", {
  data <- data.frame(
    var = c("A", "B", NA, "A", "B")
  )

  result <- freq_table(data, var, include_na = TRUE)
  expect_s3_class(result, "freq_table")
})
