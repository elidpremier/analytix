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

test_that("les fonctions de formatage avancées utilisent le helper décimal", {
  expect_identical(analytix:::.fmt_decimal(c(1.234, 9.8), 2), c("1,23", "9,80"))
  expect_identical(analytix:::.fmt_percent(c(12.34, 56.7), 1), c("12,3", "56,7"))
})

test_that("les tableaux descriptifs secondaires utilisent le formatage centralisé", {
  binary <- desc_binary(
    data.frame(status = c("Oui", "Non")),
    status,
    target_level = "Oui"
  )
  multi <- desc_multi_choice(
    data.frame(q1 = c("Oui", "Non"), q2 = c("Oui", "Oui")),
    cols = c("q1", "q2"),
    positive = "Oui"
  )
  score <- desc_score(
    data.frame(q1 = c(1, 0), q2 = c(1, 1)),
    cols = c("q1", "q2"),
    breaks = c(-Inf, 1, Inf)
  )

  expect_identical(binary$data$`Pourcentage (%)`, "50,0")
  expect_true(any(grepl("50,0 %", multi$flextable$body$dataset[[3]], fixed = TRUE)))
  expect_true(any(grepl("50,0 %", score$data$summary$Valeur, fixed = TRUE)))
})

test_that("les graphiques et tableaux Likert utilisent le formatage centralisé", {
  bar <- plot_bar(data.frame(status = c("Oui", "Non")), status)
  likert <- desc_likert(
    data.frame(satisfaction = c(1, 2, 2, 3)),
    satisfaction
  )

  bar_labels <- ggplot2::ggplot_build(bar)$data[[2]]$label
  expect_true(any(grepl("50,0 %", bar_labels, fixed = TRUE)))
  expect_true(any(grepl("50,0 %", likert$body$dataset$Pourcentage, fixed = TRUE)))
})

test_that("les tableaux statistiques spécialisés utilisent le formatage centralisé", {
  age <- desc_age(data.frame(age = c(20, 20, 40, 40)), age)
  grouped <- desc_by_group(
    data.frame(value = c(1, 2, 3, 4), group = c("A", "A", "B", "B")),
    value,
    group
  )

  expect_true(any(grepl("50,0 %", age$body$dataset$Valeur, fixed = TRUE)))
  expect_true(any(grepl("1,5", grouped$body$dataset$A, fixed = TRUE)))
})

test_that("les sorties avancées restantes utilisent le formatage centralisé", {
  heatmap <- plot_heatmap(
    data.frame(x = "A", y = "B", value = 12.5),
    x, y, value
  )
  survival <- tbl_km(
    data.frame(time = c(1, 2, 3, 4), event = c(1, 1, 0, 1)),
    time_var = "time",
    event_var = "event"
  )

  heatmap_labels <- ggplot2::ggplot_build(heatmap)$data[[2]]$label
  expect_identical(heatmap_labels, "12,5 %")
  expect_true(all(grepl(",", survival$body$dataset$Valeur, fixed = TRUE)[3:5]))
})

test_that("les formats de rapport utilisent les helpers statistiques", {
  expect_identical(analytix:::.fmt_decimal(0.875, 3), "0,875")
  expect_identical(analytix:::.fmt_percent(12.5, 1, suffix = TRUE), "12,5 %")
})

test_that("la charte de présentation respecte le contraste WCAG AA", {
  expect_gte(
    analytix:::.analytix_contrast_ratio(
      analytix:::.analytix_accessible_header,
      "#FFFFFF"
    ),
    4.5
  )
  expect_length(analytix:::.analytix_accessible_palette, 7)
  expect_true(all(vapply(
    analytix:::.analytix_accessible_palette,
    function(color) analytix:::.analytix_contrast_ratio(color, "#FFFFFF") >= 3,
    logical(1)
  )))
})
