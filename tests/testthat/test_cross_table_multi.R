test_that("cross_table_multi fonctionne avec un jeu de données minimal", {
  data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A", "B")),
    target = factor(c("Oui", "Non", "Oui", "Oui", "Non", "Non"))
  )
  expect_error({
    ft <- cross_table_multi(data, target, var1, outcome_of_interest = "Oui")
    print(ft)
  }, NA)
})
