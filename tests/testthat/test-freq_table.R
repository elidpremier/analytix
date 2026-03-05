test_that("descr_categorial fonctionne avec des données catégorielles", {
  data <- data.frame(
    categorie = factor(c("A", "B", "A", "B", "A", "B")),
    valeur = c(1, 2, 3, 4, 5, 6)
  )

  # Test avec variable factor
  result <- descr_categorial(data, categorie)
  expect_s3_class(result, "freq_table")
  expect_true("flextable" %in% names(result))
  expect_true("data" %in% names(result))

  # Test avec variable numérique discrète
  result_num <- descr_categorial(data, valeur)
  expect_s3_class(result_num, "freq_table")
})

test_that("descr_categorial gère les valeurs manquantes", {
  data <- data.frame(
    var = c("A", "B", NA, "A", "B")
  )

  result <- descr_categorial(data, var, include_na = TRUE)
  expect_s3_class(result, "freq_table")
})
