test_that("recode_likert convertit correctement", {
  x <- c("Pas du tout", "Assez", "Tout à fait", NA)
  map <- c("pas du tout" = 1, "peu" = 2, "moyennement" = 3,
           "assez" = 4, "tout à fait" = 5)
  res <- recode_likert(x, map)
  expect_equal(res[1], 1)
  expect_equal(res[2], 4)
  expect_equal(res[3], 5)
  expect_true(is.na(res[4]))
})

test_that("recode_likert gere la casse et les espaces", {
  x <- c("  PAS DU TOUT  ", "ASSEZ")
  map <- c("pas du tout" = 1, "assez" = 4)
  res <- recode_likert(x, map)
  expect_equal(res[1], 1)
  expect_equal(res[2], 4)
})

test_that("descr_likert retourne un flextable", {
  df <- data.frame(sat = sample(1:5, 30, replace = TRUE))
  ft <- descr_likert(df, sat, var_name = "Satisfaction")
  expect_s3_class(ft, "flextable")
})

test_that("descr_likert avec plot retourne une liste", {
  df <- data.frame(sat = sample(1:5, 30, replace = TRUE))
  res <- descr_likert(df, sat, var_name = "Satisfaction", plot = TRUE)
  expect_type(res, "list")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$plot, "ggplot")
})

test_that("multi_likert_table retourne un flextable", {
  df <- data.frame(
    q1 = sample(1:5, 20, replace = TRUE),
    q2 = sample(1:5, 20, replace = TRUE)
  )
  ft <- multi_likert_table(df, cols = c("q1", "q2"))
  expect_s3_class(ft, "flextable")
})

test_that("descr_age retourne un flextable", {
  df <- data.frame(age = c(23, 31, 45, 52, 18, 67, 29, NA))
  ft <- descr_age(df, age, var_name = "Age")
  expect_s3_class(ft, "flextable")
})

test_that("descr_age gere les valeurs manquantes", {
  df <- data.frame(age = c(NA, NA, 30, 40))
  ft <- descr_age(df, age)
  expect_s3_class(ft, "flextable")
})

test_that("calc_sensitivity_specificity retourne un flextable", {
  actual    <- c(1, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  predicted <- c(1, 1, 0, 0, 0, 1, 1, 0, 0, 0)
  ft <- calc_sensitivity_specificity(actual, predicted)
  expect_s3_class(ft, "flextable")
})

test_that("multivariable_logistic_table accepte un objet glm", {
  mod <- stats::glm(am ~ cyl + wt, data = mtcars, family = stats::binomial())
  ft <- multivariable_logistic_table(mod)
  expect_s3_class(ft, "flextable")
})

test_that("multivariable_logistic_table accepte une formule + data", {
  ft <- multivariable_logistic_table(am ~ cyl + wt, data = mtcars)
  expect_s3_class(ft, "flextable")
})

test_that("anova_table retourne une liste avec anova et tukey", {
  res <- anova_table(iris, Sepal.Length, Species)
  expect_type(res, "list")
  expect_s3_class(res$anova, "flextable")
  expect_s3_class(res$tukey, "flextable")
})

test_that("correlation_table retourne un flextable", {
  ft <- correlation_table(mtcars, cols = c("mpg", "cyl", "hp"))
  expect_s3_class(ft, "flextable")
})

test_that("correlation_table fonctionne avec spearman", {
  ft <- correlation_table(mtcars, cols = c("mpg", "cyl"), method = "spearman")
  expect_s3_class(ft, "flextable")
})

test_that("detect_outliers retourne une liste avec summary et outlier_rows", {
  df <- data.frame(age = c(23, 31, 45, 52, 18, 120, 29, 34, -5, 41))
  res <- detect_outliers(df, age)
  expect_type(res, "list")
  expect_s3_class(res$summary, "flextable")
  expect_true(is.integer(res$outlier_rows) || is.numeric(res$outlier_rows))
})

test_that("detect_outliers methode zscore fonctionne", {
  df <- data.frame(x = c(rnorm(50), 100))
  res <- detect_outliers(df, x, method = "zscore")
  expect_gte(res$n_outliers, 1)
})

test_that("label_vars attache les labels", {
  df <- data.frame(age = c(25, 30), sexe = c("H", "F"))
  df2 <- label_vars(df, c(age = "Âge", sexe = "Sexe"))
  expect_equal(attr(df2$age,  "label"), "Âge")
  expect_equal(attr(df2$sexe, "label"), "Sexe")
})

test_that("label_vars avertit si variable inexistante", {
  df <- data.frame(age = c(25, 30))
  expect_warning(label_vars(df, c(age = "Âge", poids = "Poids")))
})

test_that("plot_likert_divergent retourne un ggplot", {
  df <- data.frame(
    q1 = sample(1:5, 30, replace = TRUE),
    q2 = sample(1:5, 30, replace = TRUE)
  )
  p <- plot_likert_divergent(df, cols = c("q1", "q2"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_missing_map retourne un ggplot", {
  df <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3), c = c(1, 2, NA))
  p <- plot_missing_map(df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_correlation retourne un ggplot", {
  p <- plot_correlation(mtcars, cols = c("mpg", "cyl", "hp"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_correlation fonctionne avec spearman", {
  p <- plot_correlation(iris[, 1:4], method = "spearman")
  expect_s3_class(p, "ggplot")
})

test_that("les fonctions lisent automatiquement les attributs label", {
  df <- data.frame(age = c(20, 30, 40), score = c(1, 2, 3), cat = c("A", "B", "A"))
  df <- label_vars(df, c(age = "Âge en années", score = "Score de satisfaction", cat = "Catégorie"))
  
  # descr_age
  ft_age <- descr_age(df, age)
  expect_s3_class(ft_age, "flextable")
  
  # calc_prevalence
  prev <- calc_prevalence(df, cat, cases_val = "A")
  expect_equal(prev$Variable[1], "Catégorie")
  
  # detect_outliers
  out <- detect_outliers(df, age)
  expect_s3_class(out$summary, "flextable")
  
  # plot_barplot
  p_bar <- plot_barplot(df, cat)
  expect_equal(p_bar$labels$title, "Catégorie")
})

