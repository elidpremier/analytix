#' @title Fonctions dépréciées du package analytix
#' @description Ces fonctions sont conservées pour des raisons de rétrocompatibilité,
#' mais seront supprimées dans une future version. Veuillez utiliser les nouvelles fonctions basées sur les préfixes.
#' @param ... Arguments transmis à la nouvelle fonction.
#' @name deprecated
NULL

#' @rdname deprecated
#' @export
recode_odk_binary <- function(...) {
  warning("`recode_odk_binary()` est obsolète. Utilisez `prep_recode_odk()` à la place.", call. = FALSE)
  prep_recode_odk(...)
}

#' @rdname deprecated
#' @export
clean_column_names <- function(...) {
  warning("`clean_column_names()` est obsolète. Utilisez `clean_colnames()` ou `clean_names()` à la place.", call. = FALSE)
  clean_colnames(...)
}

#' @rdname deprecated
#' @export
impute_mode <- function(...) {
  warning("`impute_mode()` est obsolète. Utilisez `prep_impute_mode()` à la place.", call. = FALSE)
  prep_impute_mode(...)
}

#' @rdname deprecated
#' @export
impute_mean <- function(...) {
  warning("`impute_mean()` est obsolète. Utilisez `prep_impute_mean()` à la place.", call. = FALSE)
  prep_impute_mean(...)
}

#' @rdname deprecated
#' @export
impute_mice <- function(...) {
  warning("`impute_mice()` est obsolète. Utilisez `prep_impute_mice()` à la place.", call. = FALSE)
  prep_impute_mice(...)
}

#' @rdname deprecated
#' @export
categorize_numeric <- function(...) {
  warning("`categorize_numeric()` est obsolète. Utilisez `prep_categorize()` à la place.", call. = FALSE)
  prep_categorize(...)
}

#' @rdname deprecated
#' @export
collapse_categories <- function(...) {
  warning("`collapse_categories()` est obsolète. Utilisez `prep_collapse()` à la place.", call. = FALSE)
  prep_collapse(...)
}

#' @rdname deprecated
#' @export
detect_outliers <- function(...) {
  warning("`detect_outliers()` est obsolète. Utilisez `prep_outliers()` à la place.", call. = FALSE)
  prep_outliers(...)
}

#' @rdname deprecated
#' @export
label_vars <- function(...) {
  warning("`label_vars()` est obsolète. Utilisez `prep_labels()` à la place.", call. = FALSE)
  prep_labels(...)
}

#' @rdname deprecated
#' @export
import_clean <- function(...) {
  warning("`import_clean()` est obsolète. Utilisez `prep_import()` à la place.", call. = FALSE)
  prep_import(...)
}

#' @rdname deprecated
#' @export
quick_code <- function(...) {
  warning("`quick_code()` est obsolète. Utilisez `prep_quick_code()` à la place.", call. = FALSE)
  prep_quick_code(...)
}

#' @rdname deprecated
#' @export
descr_numeric <- function(...) {
  warning("`descr_numeric()` est obsolète. Utilisez `desc_numeric()` à la place.", call. = FALSE)
  desc_numeric(...)
}

#' @rdname deprecated
#' @export
descr_categorial <- function(...) {
  warning("`descr_categorial()` est obsolète. Utilisez `desc_categorical()` à la place.", call. = FALSE)
  desc_categorical(...)
}

#' @rdname deprecated
#' @export
descr_binary <- function(...) {
  warning("`descr_binary()` est obsolète. Utilisez `desc_binary()` à la place.", call. = FALSE)
  desc_binary(...)
}

#' @rdname deprecated
#' @export
descr_age <- function(...) {
  warning("`descr_age()` est obsolète. Utilisez `desc_age()` à la place.", call. = FALSE)
  desc_age(...)
}

#' @rdname deprecated
#' @export
recode_likert <- function(...) {
  warning("`recode_likert()` est obsolète. Utilisez `prep_recode_likert()` à la place.", call. = FALSE)
  prep_recode_likert(...)
}

#' @rdname deprecated
#' @export
descr_likert <- function(...) {
  warning("`descr_likert()` est obsolète. Utilisez `desc_likert()` à la place.", call. = FALSE)
  desc_likert(...)
}

#' @rdname deprecated
#' @export
multi_likert_table <- function(...) {
  warning("`multi_likert_table()` est obsolète. Utilisez `tbl_likert_multi()` à la place.", call. = FALSE)
  tbl_likert_multi(...)
}

#' @rdname deprecated
#' @export
descr_multi_choice <- function(...) {
  warning("`descr_multi_choice()` est obsolète. Utilisez `desc_multi_choice()` à la place.", call. = FALSE)
  desc_multi_choice(...)
}

#' @rdname deprecated
#' @export
descr_grouped_categories <- function(...) {
  warning("`descr_grouped_categories()` est obsolète. Utilisez `desc_grouped()` à la place.", call. = FALSE)
  desc_grouped(...)
}

#' @rdname deprecated
#' @export
descr_by_group <- function(...) {
  warning("`descr_by_group()` est obsolète. Utilisez `desc_by_group()` à la place.", call. = FALSE)
  desc_by_group(...)
}

#' @rdname deprecated
#' @export
calc_prevalence <- function(...) {
  warning("`calc_prevalence()` est obsolète. Utilisez `desc_prevalence()` à la place.", call. = FALSE)
  desc_prevalence(...)
}

#' @rdname deprecated
#' @export
auto_describe <- function(...) {
  warning("`auto_describe()` est obsolète. Utilisez `desc_auto()` à la place.", call. = FALSE)
  desc_auto(...)
}

#' @rdname deprecated
#' @export
analyse_descriptive_multiple <- function(...) {
  warning("`analyse_descriptive_multiple()` est obsolète. Utilisez `desc_multiple()` à la place.", call. = FALSE)
  desc_multiple(...)
}

#' @rdname deprecated
#' @export
bivariate_or_table <- function(...) {
  warning("`bivariate_or_table()` est obsolète. Utilisez `tbl_bivariate_or()` à la place.", call. = FALSE)
  tbl_bivariate_or(...)
}

#' @rdname deprecated
#' @export
cross_multi <- function(...) {
  warning("`cross_multi()` est obsolète. Utilisez `tbl_cross_multi()` à la place.", call. = FALSE)
  tbl_cross_multi(...)
}

#' @rdname deprecated
#' @export
cross_table_uniq_mod <- function(...) {
  warning("`cross_table_uniq_mod()` est obsolète. Utilisez `tbl_cross_unique()` à la place.", call. = FALSE)
  tbl_cross_unique(...)
}

#' @rdname deprecated
#' @export
calc_sensitivity_specificity <- function(...) {
  warning("`calc_sensitivity_specificity()` est obsolète. Utilisez `stat_sens_spec()` à la place.", call. = FALSE)
  stat_sens_spec(...)
}

#' @rdname deprecated
#' @export
multivariable_logistic_table <- function(...) {
  warning("`multivariable_logistic_table()` est obsolète. Utilisez `tbl_logistic()` à la place.", call. = FALSE)
  tbl_logistic(...)
}

#' @rdname deprecated
#' @export
anova_table <- function(...) {
  warning("`anova_table()` est obsolète. Utilisez `tbl_anova()` à la place.", call. = FALSE)
  tbl_anova(...)
}

#' @rdname deprecated
#' @export
correlation_table <- function(...) {
  warning("`correlation_table()` est obsolète. Utilisez `tbl_correlation()` à la place.", call. = FALSE)
  tbl_correlation(...)
}

#' @rdname deprecated
#' @export
roc_table <- function(...) {
  warning("`roc_table()` est obsolète. Utilisez `tbl_roc()` à la place.", call. = FALSE)
  tbl_roc(...)
}

#' @rdname deprecated
#' @export
km_table <- function(...) {
  warning("`km_table()` est obsolète. Utilisez `tbl_km()` à la place.", call. = FALSE)
  tbl_km(...)
}

#' @rdname deprecated
#' @export
km_plot <- function(...) {
  warning("`km_plot()` est obsolète. Utilisez `plot_km()` à la place.", call. = FALSE)
  plot_km(...)
}

#' @rdname deprecated
#' @export
plot_barplot <- function(...) {
  warning("`plot_barplot()` est obsolète. Utilisez `plot_bar()` à la place.", call. = FALSE)
  plot_bar(...)
}

#' @rdname deprecated
#' @export
plot_boxplot <- function(...) {
  warning("`plot_boxplot()` est obsolète. Utilisez `plot_box()` à la place.", call. = FALSE)
  plot_box(...)
}

#' @rdname deprecated
#' @export
plot_pie_chart <- function(...) {
  warning("`plot_pie_chart()` est obsolète. Utilisez `plot_pie()` à la place.", call. = FALSE)
  plot_pie(...)
}

#' @rdname deprecated
#' @export
plot_stacked_bar_100 <- function(...) {
  warning("`plot_stacked_bar_100()` est obsolète. Utilisez `plot_bar_stacked()` à la place.", call. = FALSE)
  plot_bar_stacked(...)
}

#' @rdname deprecated
#' @export
plot_grouped_bar <- function(...) {
  warning("`plot_grouped_bar()` est obsolète. Utilisez `plot_bar_grouped()` à la place.", call. = FALSE)
  plot_bar_grouped(...)
}

#' @rdname deprecated
#' @export
apply_custom_theme <- function(...) {
  warning("`apply_custom_theme()` est obsolète. Utilisez `fmt_apply_theme()` à la place.", call. = FALSE)
  fmt_apply_theme(...)
}

#' @rdname deprecated
#' @export
plot_heatmap_matrix <- function(...) {
  warning("`plot_heatmap_matrix()` est obsolète. Utilisez `plot_heatmap()` à la place.", call. = FALSE)
  plot_heatmap(...)
}

#' @rdname deprecated
#' @export
plot_likert_divergent <- function(...) {
  warning("`plot_likert_divergent()` est obsolète. Utilisez `plot_likert()` à la place.", call. = FALSE)
  plot_likert(...)
}

#' @rdname deprecated
#' @export
plot_missing_map <- function(...) {
  warning("`plot_missing_map()` est obsolète. Utilisez `plot_missing()` à la place.", call. = FALSE)
  plot_missing(...)
}

#' @rdname deprecated
#' @export
interpret_pvalue <- function(...) {
  warning("`interpret_pvalue()` est obsolète. Utilisez `interp_pvalue()` à la place.", call. = FALSE)
  interp_pvalue(...)
}

#' @rdname deprecated
#' @export
interpret_or <- function(...) {
  warning("`interpret_or()` est obsolète. Utilisez `interp_or()` à la place.", call. = FALSE)
  interp_or(...)
}

#' @rdname deprecated
#' @export
interpret_association <- function(...) {
  warning("`interpret_association()` est obsolète. Utilisez `interp_association()` à la place.", call. = FALSE)
  interp_association(...)
}

#' @rdname deprecated
#' @export
format_flextable <- function(...) {
  warning("`format_flextable()` est obsolète. Utilisez `fmt_flextable()` à la place.", call. = FALSE)
  fmt_flextable(...)
}

#' @rdname deprecated
#' @export
fmt_regression_fr <- function(...) {
  warning("`fmt_regression_fr()` est obsolète. Utilisez `fmt_regression()` à la place.", call. = FALSE)
  fmt_regression(...)
}

#' @rdname deprecated
#' @export
generate_report <- function(...) {
  warning("`generate_report()` est obsolète. Utilisez `report_generate()` à la place.", call. = FALSE)
  report_generate(...)
}

#' @rdname deprecated
#' @export
magic_report <- function(...) {
  warning("`magic_report()` est obsolète. Utilisez `report_magic()` à la place.", call. = FALSE)
  report_magic(...)
}

#' @rdname deprecated
#' @export
missing_report <- function(...) {
  warning("`missing_report()` est obsolète. Utilisez `report_missing()` à la place.", call. = FALSE)
  report_missing(...)
}

#' @rdname deprecated
#' @export
compile_custom_report <- function(...) {
  warning("`compile_custom_report()` est obsolète. Utilisez `report_compile()` à la place.", call. = FALSE)
  report_compile(...)
}

#' @rdname deprecated
#' @export
export_all_tables <- function(...) {
  warning("`export_all_tables()` est obsolète. Utilisez `export_tables()` à la place.", call. = FALSE)
  export_tables(...)
}

#' @rdname deprecated
#' @export
export_to_word <- function(...) {
  warning("`export_to_word()` est obsolète. Utilisez `export_word()` à la place.", call. = FALSE)
  export_word(...)
}
