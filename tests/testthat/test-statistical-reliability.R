test_that("tbl_cross_unique reste reproductible avec le test exact de Fisher", {
  skip_if_not_installed("flextable")

  data <- data.frame(
    outcome = c("Oui", "Oui", "Non", "Non", "Oui", "Non"),
    exposition = c("A", "A", "A", "B", "B", "B")
  )

  first <- tbl_cross_unique(data, outcome, exposition, test = "fisher")
  second <- tbl_cross_unique(data, outcome, exposition, test = "fisher")
  expect_identical(first$data, second$data)
})

test_that("stat_sens_spec distingue un dénominateur nul de zéro", {
  skip_if_not_installed("flextable")

  expect_warning(
    result <- stat_sens_spec(
      actual = c(NA, 0, 0),
      predicted = c(1, 0, 1)
    ),
    "exclue"
  )
  values <- result$body$dataset$Valeur
  expect_true(any(values == "NA"))
})

test_that("tbl_logistic refuse les modèles qui ne sont pas binomiaux", {
  skip_if_not_installed("flextable")

  poisson_model <- glm(
    counts ~ group,
    data = data.frame(counts = c(1, 2, 3, 4), group = c(0, 0, 1, 1)),
    family = poisson()
  )
  expect_error(tbl_logistic(poisson_model), "glm.*binomial")
})

test_that("tbl_bivariate_or traite une exposition continue par unité", {
  skip_if_not_installed("flextable")

  data <- data.frame(
    outcome = c(0, 0, 1, 1, 1, 0, 1, 0),
    age = c(20, 25, 30, 35, 40, 45, 50, 55)
  )
  result <- tbl_bivariate_or(
    data,
    outcome = "outcome",
    exposures = "age",
    outcome_positive_val = 1
  )

  expect_equal(result$data$Modalite, "Par unité")
  expect_true(grepl("Par unité", result$flextable$body$dataset$Modalité))
})

test_that("les analyses Kaplan-Meier refusent les temps négatifs", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("survival")

  data <- data.frame(time = c(1, -1), event = c(0, 1))
  expect_error(
    tbl_km(data, time_var = "time", event_var = "event"),
    "temps numériques positifs"
  )
})

test_that("les fonctions qui utilisent une graine restaurent l'état aléatoire", {
  before <- {
    set.seed(123)
    .Random.seed
  }
  analytix_sample_data(n = 20, seed = 42)
  expect_identical(.Random.seed, before)
})

test_that("n_sentences est intégré au prompt LLM", {
  prompt <- analytix:::.build_prompt("Stat: val", n_sentences = 8)
  expect_match(prompt, "exactement 8 phrase")
})
