test_that("cross_test fonctionne avec un jeu de données minimal", {
  data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    target = factor(c("Oui", "Non", "Oui", "Oui", "Non", "Non"))
  )
  expect_error({
    ft <- cross_test(data, target, var1, outcome_of_interest = "Oui")
    expect_s3_class(ft, "flextable")
  }, NA)
})

test_that("cross_test gère les paramètres pct", {
  data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    target = factor(c("Oui", "Non", "Oui", "Oui", "Non", "Non"))
  )

  # Test avec pourcentages en ligne
  result_row <- cross_test(data, target, var1, pct = "row")
  expect_s3_class(result_row, "flextable")

  # Test avec pourcentages en colonne
  result_col <- cross_test(data, target, var1, pct = "col")
  expect_s3_class(result_col, "flextable")
})
