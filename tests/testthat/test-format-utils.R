test_that("les helpers de formatage utilisent la virgule décimale", {
  expect_identical(analytix:::.fmt_decimal(c(1.2, 10), 1), c("1,2", "10,0"))
  expect_identical(analytix:::.fmt_percent(12.5, 1, suffix = TRUE), "12,5 %")
  expect_identical(analytix:::.fmt_count(c(3, 4.8)), c("3", "5"))
})

test_that("les p-values sont formatées avec un seuil explicite", {
  expect_identical(analytix:::.fmt_pvalue(c(0.1234, 0.0002), 3), c("0,123", "< 0,001"))
})

test_that("les intervalles de confiance utilisent un libellé uniforme", {
  expect_identical(
    analytix:::.fmt_ci_percent(10, 20, digits = 1, level = 95),
    "[IC95 % : 10,0 - 20,0 %]"
  )
})

test_that("les sorties descriptives utilisent les helpers centralisés", {
  numeric_result <- desc_numeric(data.frame(score = c(1.2, 2.3, 3.4)), score, digits = 1)
  categorical_result <- desc_categorical(
    data.frame(status = c("Oui", "Non", "Oui")),
    status,
    digits = 1
  )
  prevalence_result <- desc_prevalence(
    data.frame(status = c(1, 0, 1, 0)),
    status,
    cases_val = 1,
    digits = 1
  )

  expect_true(any(numeric_result$data$Valeur == "2,3"))
  expect_true(any(categorical_result$data$pourcentage_formate == "66,7"))
  expect_match(prevalence_result$Formate, "IC95 %")
  expect_match(prevalence_result$Formate, "50,0 %")
})

test_that("les tableaux croisés utilisent le formatage centralisé", {
  result <- tbl_cross_multi(
    data.frame(
      outcome = c("Oui", "Oui", "Non", "Non"),
      exposure = c("A", "B", "A", "B")
    ),
    outcome = outcome,
    predictors = "exposure",
    method = "level"
  )

  expect_true(any(grepl("50,0 %", unlist(result), fixed = TRUE)))
})
