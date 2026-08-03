test_that("clean_utils work correctly", {
  expect_equal(clean_text(c(" homme ", "", "NA")), c("homme", NA, NA))
  
  bin_res <- clean_binary(c("oui", "NON", "1", "0", "Yes"))
  expect_equal(as.character(bin_res), c("Oui", "Non", "Oui", "Non", "Oui"))
  
  expect_equal(clean_numeric(c("12,5", " 15 ", "NA")), c(12.5, 15.0, NA))
  
  expect_equal(impute_mode(c("A", "A", "B", NA)), c("A", "A", "B", "A"))
  expect_equal(impute_mean(c(10, 20, NA, 30)), c(10, 20, 20, 30))
})

test_that("descr_multi_choice generates a valid flextable", {
  df <- data.frame(
    q1_a = c(1, 1, 0, 0, 1),
    q1_b = c(1, 0, 1, 0, 0),
    q1_c = c(0, 0, 1, 1, 1)
  )
  ft <- descr_multi_choice(df, cols = c("q1_a", "q1_b", "q1_c"),
                           var_labels = c(q1_a = "Opt A", q1_b = "Opt B", q1_c = "Opt C"))
  expect_s3_class(ft, "flextable")
})

test_that("calc_prevalence calculates correct proportions and IC95%", {
  df <- data.frame(blse = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1))
  res <- calc_prevalence(df, blse, cases_val = 1, method = "wilson")
  expect_equal(res$Cas, 6)
  expect_equal(res$Total, 10)
  expect_equal(res$Proportion, 0.6)
  expect_true(nchar(res$Formate) > 0)
})

test_that("bivariate_or_table builds logistic OR table flextable", {
  df <- data.frame(
    reussite = c("Oui", "Non", "Oui", "Oui", "Non", "Non", "Oui", "Non"),
    bourse = c("Oui", "Oui", "Non", "Oui", "Non", "Non", "Non", "Oui"),
    sexe = c("F", "M", "F", "M", "F", "M", "F", "F")
  )
  ft <- bivariate_or_table(df, outcome = "reussite", exposures = c("bourse", "sexe"), outcome_positive_val = "Oui")
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable formats flextable", {
  ft <- flextable::flextable(head(mtcars))
  ft_fmt <- format_flextable(ft)
  expect_s3_class(ft_fmt, "flextable")
})

test_that("plot_heatmap_matrix returns ggplot object", {
  df <- data.frame(
    Source = rep(c("Environnement", "Portage"), each = 2),
    Antibio = rep(c("Ampicilline", "Céfotaxime"), 2),
    Taux = c(85.5, 42.0, 90.0, 60.1)
  )
  p <- plot_heatmap_matrix(df, x = Source, y = Antibio, fill = Taux)
  expect_s3_class(p, "ggplot")
})

test_that("descr_by_group computes numeric stats with statistical test", {
  ft <- descr_by_group(mtcars, mpg, cyl, test_stat = TRUE)
  expect_s3_class(ft, "flextable")
})
