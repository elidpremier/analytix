# Matrice de compatibilité de l'API

Date de référence : 2026-10-23  
Version : `0.6.0.9000`

Les fonctions de la colonne **Alias historique** restent exportées pour la
rétrocompatibilité. Elles émettent un avertissement et délèguent à la fonction
**API canonique**. Les nouvelles fonctionnalités doivent utiliser les fonctions
canoniques.

## Préparation et nettoyage

| Alias historique | API canonique |
|---|---|
| `recode_odk_binary()` | `prep_recode_odk()` |
| `clean_column_names()` | `clean_colnames()` |
| `impute_mode()` | `prep_impute_mode()` |
| `impute_mean()` | `prep_impute_mean()` |
| `impute_mice()` | `prep_impute_mice()` |
| `categorize_numeric()` | `prep_categorize()` |
| `collapse_categories()` | `prep_collapse()` |
| `detect_outliers()` | `prep_outliers()` |
| `label_vars()` | `prep_labels()` |
| `import_clean()` | `prep_import()` |
| `quick_code()` | `prep_quick_code()` |
| `recode_likert()` | `prep_recode_likert()` |

## Description et statistiques

| Alias historique | API canonique |
|---|---|
| `descr_numeric()` | `desc_numeric()` |
| `descr_categorial()` | `desc_categorical()` |
| `descr_binary()` | `desc_binary()` |
| `descr_age()` | `desc_age()` |
| `descr_likert()` | `desc_likert()` |
| `descr_multi_choice()` | `desc_multi_choice()` |
| `descr_grouped_categories()` | `desc_grouped()` |
| `descr_by_group()` | `desc_by_group()` |
| `calc_prevalence()` | `desc_prevalence()` |
| `auto_describe()` | `desc_auto()` |
| `analyse_descriptive_multiple()` | `desc_multiple()` |
| `multi_likert_table()` | `tbl_likert_multi()` |
| `bivariate_or_table()` | `tbl_bivariate_or()` |
| `cross_multi()` | `tbl_cross_multi()` |
| `cross_table_uniq_mod()` | `tbl_cross_unique()` |
| `calc_sensitivity_specificity()` | `stat_sens_spec()` |
| `multivariable_logistic_table()` | `tbl_logistic()` |
| `anova_table()` | `tbl_anova()` |
| `correlation_table()` | `tbl_correlation()` |
| `roc_table()` | `tbl_roc()` |
| `km_table()` | `tbl_km()` |

## Graphiques, interprétation et rapports

| Alias historique | API canonique |
|---|---|
| `km_plot()` | `plot_km()` |
| `plot_barplot()` | `plot_bar()` |
| `plot_boxplot()` | `plot_box()` |
| `plot_pie_chart()` | `plot_pie()` |
| `plot_stacked_bar_100()` | `plot_bar_stacked()` |
| `plot_grouped_bar()` | `plot_bar_grouped()` |
| `apply_custom_theme()` | `fmt_apply_theme()` |
| `plot_heatmap_matrix()` | `plot_heatmap()` |
| `plot_likert_divergent()` | `plot_likert()` |
| `plot_missing_map()` | `plot_missing()` |
| `interpret_pvalue()` | `interp_pvalue()` |
| `interpret_or()` | `interp_or()` |
| `interpret_association()` | `interp_association()` |
| `format_flextable()` | `fmt_flextable()` |
| `fmt_regression_fr()` | `fmt_regression()` |
| `generate_report()` | `report_generate()` |
| `magic_report()` | `report_magic()` |
| `missing_report()` | `report_missing()` |
| `compile_custom_report()` | `report_compile()` |
| `export_all_tables()` | `export_tables()` |
| `export_to_word()` | `export_word()` |

## Politique de transition

- Les alias restent disponibles pendant la bêta `0.6.x`.
- Chaque appel à un alias émet un avertissement explicite vers l'API canonique.
- Aucun alias ne doit recevoir une nouvelle fonctionnalité avant migration de
  l'appelant vers la fonction canonique.
- Toute suppression éventuelle doit être annoncée dans `NEWS.md`, documentée
  au minimum pendant un cycle mineur et accompagnée d'une alternative stable.
