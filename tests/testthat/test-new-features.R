test_that("descr_categorial handles subsetting", {
  res <- descr_categorial(iris, Species, subset = Sepal.Length > 5)
  expect_s3_class(res, "freq_table")
  expect_true(res$n_total < nrow(iris))
})

test_that("descr_numeric handles digits correctly", {
  res <- descr_numeric(iris, Sepal.Length, digits = 3)
  expect_s3_class(res, "descr_numeric")
  # Vérifier que la moyenne est formatée avec 3 décimales
  moy_str <- res$data$Valeur[res$data$Statistique == "Moyenne"]
  expect_match(moy_str, ",\\d{3}$")
})

test_that("descr_binary works", {
  df <- data.frame(x = c("Oui", "Non", "Oui", NA))
  res <- descr_binary(df, x, target_level = "Oui")
  expect_s3_class(res, "descr_binary")
  expect_equal(res$data$Effectif, 2)
})

test_that("descr_by_group handles categorical vs categorical", {
  ft <- descr_by_group(mtcars, am, cyl)
  expect_s3_class(ft, "flextable")
})

test_that("export_to_word handles nested lists", {
  res_multi <- analyse_descriptive_multiple(iris, vars = c("Species", "Sepal.Length"))
  # On vérifie juste qu'on ne crash pas lors de l'extraction interne
  extracted <- .extract_tables_recursive(res_multi)
  expect_equal(length(extracted), 2)
})
