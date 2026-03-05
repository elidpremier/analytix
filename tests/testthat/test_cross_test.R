test_that("cross_table_uniq_mod fonctionne avec un jeu de données minimal", {
  data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    target = factor(c("Oui", "Non", "Oui", "Oui", "Non", "Non"))
  )
  expect_error({
    ft <- cross_table_uniq_mod(data, target, var1, outcome_of_interest = "Oui")
    expect_s3_class(ft, "flextable")
  }, NA)
})

test_that("cross_table_uniq_mod gère les paramètres pct", {
  data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    target = factor(c("Oui", "Non", "Oui", "Oui", "Non", "Non"))
  )

  # Test avec pourcentages en ligne
  result_row <- cross_table_uniq_mod(data, target, var1, pct = "row")
  expect_s3_class(result_row, "flextable")

  # Test avec pourcentages en colonne
  result_col <- cross_table_uniq_mod(data, target, var1, pct = "col")
  expect_s3_class(result_col, "flextable")
})
