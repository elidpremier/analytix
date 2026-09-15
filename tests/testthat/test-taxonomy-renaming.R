test_that("Toutes les nouvelles fonctions avec préfixes et leurs alias dépréciés fonctionnent", {
  df <- data.frame(
    id = 1:10,
    age = c(20, 25, 30, 35, 40, 45, 50, 55, 60, NA),
    sexe = factor(c("H", "F", "H", "F", "H", "F", "H", "F", "H", "F")),
    gueri = c(1, 0, 1, 1, 0, 1, 0, 1, 0, 1),
    score = c(1, 2, 3, 4, 5, 4, 3, 2, 1, 5)
  )

  # Clean / Prep family
  expect_no_error(clean_colnames(df))
  expect_no_error(prep_impute_mode(df$sexe))
  expect_no_error(prep_impute_mean(df$age))
  expect_no_error(prep_labels(df, c(age = "Âge")))

  # Desc family
  expect_no_error(desc_numeric(df, age))
  expect_no_error(desc_categorical(df, sexe))
  expect_no_error(desc_binary(df, gueri))
  expect_no_error(desc_age(df, age))
  expect_no_error(desc_prevalence(df, gueri))

  # Tbl & Stat family
  expect_no_error(tbl_cross_unique(df, sexe, gueri))
  expect_no_error(tbl_bivariate_or(df, outcome = "gueri", exposures = "sexe"))
  expect_no_error(tbl_correlation(df, cols = c("age", "score")))
  expect_no_error(stat_sens_spec(df$gueri, df$gueri))

  # Interp & Fmt & Report & Export family
  expect_equal(interp_pvalue(0.01), "La différence observée est statistiquement significative (p = 0.010). On peut conclure qu'il existe un lien réel.")
  expect_no_error(fmt_flextable(flextable::flextable(head(df))))
  expect_no_error(report_missing(df))

  # Deprecated aliases issue warnings
  expect_warning(impute_mode(df$sexe), "dépréciée")
  expect_warning(impute_mean(df$age), "dépréciée")
  expect_warning(descr_numeric(df, age), "dépréciée")
  expect_warning(descr_binary(df, gueri), "dépréciée")
  expect_warning(calc_prevalence(df, gueri), "dépréciée")
})
