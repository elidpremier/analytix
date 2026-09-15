test_that("desc_categorical et descr_categorial fonctionnent avec des données catégorielles", {
  data <- data.frame(
    categorie = factor(c("A", "B", "A", "B", "A", "B")),
    valeur = c(1, 2, 3, 4, 5, 6)
  )

  # Test nouvelle fonction
  result <- desc_categorical(data, categorie)
  expect_s3_class(result, "freq_table")
  expect_true("flextable" %in% names(result))
  expect_true("data" %in% names(result))

  # Test avec alias déprécié (avertissement)
  expect_warning(descr_categorial(data, categorie), "dépréciée")

  # Test avec variable numérique discrète
  result_num <- desc_categorical(data, valeur)
  expect_s3_class(result_num, "freq_table")
})

test_that("desc_categorical gère les valeurs manquantes", {
  data <- data.frame(
    var = c("A", "B", NA, "A", "B")
  )

  result <- desc_categorical(data, var, include_na = TRUE)
  expect_s3_class(result, "freq_table")
})
