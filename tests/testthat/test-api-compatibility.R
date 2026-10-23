legacy_api <- c(
  recode_odk_binary = "prep_recode_odk",
  clean_column_names = "clean_colnames",
  impute_mode = "prep_impute_mode",
  impute_mean = "prep_impute_mean",
  impute_mice = "prep_impute_mice",
  categorize_numeric = "prep_categorize",
  collapse_categories = "prep_collapse",
  detect_outliers = "prep_outliers",
  label_vars = "prep_labels",
  import_clean = "prep_import",
  quick_code = "prep_quick_code",
  recode_likert = "prep_recode_likert",
  descr_numeric = "desc_numeric",
  descr_categorial = "desc_categorical",
  descr_binary = "desc_binary",
  descr_age = "desc_age",
  descr_likert = "desc_likert",
  descr_multi_choice = "desc_multi_choice",
  descr_grouped_categories = "desc_grouped",
  descr_by_group = "desc_by_group",
  calc_prevalence = "desc_prevalence",
  auto_describe = "desc_auto",
  analyse_descriptive_multiple = "desc_multiple",
  multi_likert_table = "tbl_likert_multi",
  bivariate_or_table = "tbl_bivariate_or",
  cross_multi = "tbl_cross_multi",
  cross_table_uniq_mod = "tbl_cross_unique",
  calc_sensitivity_specificity = "stat_sens_spec",
  multivariable_logistic_table = "tbl_logistic",
  anova_table = "tbl_anova",
  correlation_table = "tbl_correlation",
  roc_table = "tbl_roc",
  km_table = "tbl_km",
  km_plot = "plot_km",
  plot_barplot = "plot_bar",
  plot_boxplot = "plot_box",
  plot_pie_chart = "plot_pie",
  plot_stacked_bar_100 = "plot_bar_stacked",
  plot_grouped_bar = "plot_bar_grouped",
  apply_custom_theme = "fmt_apply_theme",
  plot_heatmap_matrix = "plot_heatmap",
  plot_likert_divergent = "plot_likert",
  plot_missing_map = "plot_missing",
  interpret_pvalue = "interp_pvalue",
  interpret_or = "interp_or",
  interpret_association = "interp_association",
  format_flextable = "fmt_flextable",
  fmt_regression_fr = "fmt_regression",
  generate_report = "report_generate",
  magic_report = "report_magic",
  missing_report = "report_missing",
  compile_custom_report = "report_compile",
  export_all_tables = "export_tables",
  export_to_word = "export_word"
)

test_that("chaque alias historique pointe vers une API canonique", {
  expect_true(all(names(legacy_api) %in% getNamespaceExports("analytix")))
  expect_true(all(unname(legacy_api) %in% getNamespaceExports("analytix")))
})

test_that("chaque alias historique avertit avant de déléguer", {
  for (alias in names(legacy_api)) {
    warning_texts <- character()
    tryCatch(
      withCallingHandlers(
        do.call(getExportedValue("analytix", alias), list()),
        warning = function(condition) {
          warning_texts <<- c(warning_texts, conditionMessage(condition))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(condition) invisible(NULL)
    )
    expect_match(
      paste(warning_texts, collapse = "\n"),
      paste0("`", alias, "\\(\\)` est obsolète"),
      info = paste("Alias contrôlé :", alias)
    )
  }
})
