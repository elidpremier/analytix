# analytix News

---

## Version 0.3.0 — 2026-08-03

### Nouvelles fonctions : Analyse Likert

- **`recode_likert()`** : Recodage automatique d'un vecteur textuel Likert en numérique via mapping nommé. Gère la casse et les espaces.
- **`descr_likert()`** : Tableau de fréquences + graphique ggplot2 pour une variable Likert. Retourne un `flextable` ou une liste `list(table, plot)`.
- **`multi_likert_table()`** : Tableau récapitulatif (moyenne, médiane, écart-type, min-max) de plusieurs variables Likert en une seule commande.

### Nouvelles fonctions : Description avancée

- **`descr_age()`** : Résumé standardisé d'une variable âge : statistiques complètes et tranches d'âge automatiques ou personnalisées.

### Nouvelles fonctions : Statistiques épidémiologiques

- **`calc_sensitivity_specificity()`** : Calcul de la sensibilité, spécificité, VPP, VPN et rapports de vraisemblance (LR+, LR-) avec IC95% (méthode de Wilson). Détection automatique de la valeur positive.
- **`multivariable_logistic_table()`** : Tableau formaté d'une régression logistique multivariée (ORa, IC95%, p-value). Accepte un objet `glm` ou une formule + data.
- **`anova_table()`** : ANOVA à un facteur avec test post-hoc de Tukey (HSD). Retourne une liste `list(anova, tukey)` de deux `flextable`.
- **`correlation_table()`** : Matrice de corrélations (Pearson ou Spearman) avec mise en gras des valeurs significatives.

### Nouvelles fonctions : Préparation des données

- **`detect_outliers()`** : Détection des valeurs aberrantes par méthode IQR (Tukey), Z-score ou les deux. Retourne un rapport `flextable` et les indices des observations aberrantes.
- **`label_vars()`** : Attache des attributs `label` à plusieurs variables d'un data.frame en une seule opération.
- **`import_clean()`** : Import automatique depuis Excel (.xlsx, .xls) ou CSV avec nettoyage des noms de colonnes.

### Nouvelles fonctions : Visualisations avancées

- **`plot_likert_divergent()`** : Graphique en barres divergentes centré sur le point neutre, idéal pour les études de satisfaction.
- **`plot_missing_map()`** : Heatmap ggplot2 des données manquantes par variable et observation. Les variables sont triées par taux de manquants décroissant.
- **`plot_correlation()`** : Heatmap ggplot2 de la matrice de corrélations avec palette divergente rouge-blanc-bleu. Alternative intégrée à `corrplot`.

### Nouvelles fonctions : Export

- **`export_all_tables()`** : Export d'une liste nommée de `flextable` dans un document Word structuré avec titre principal, sous-titre, auteur et sections automatiques. Gère aussi les listes imbriquées (ex. sortie de `anova_table()`).

### Corrections & améliorations

- Correction de l'utilisation de `flextable::colnames_ptype()` (non exporté) dans `correlation_table()`, remplacé par `ft$col_keys`.
- Suppression des appels `library()` dans tous les fichiers R (conformité CRAN).
- Tous les `tryCatch` sur `stats::confint()` pour la régression logistique supprimés des warnings non bloquants.

### Tests

- **87 tests unitaires** — `FAIL 0 | WARN 0 | SKIP 0 | PASS 87`
- Nouveau fichier : `tests/testthat/test-v3-features.R` (21 tests pour les nouvelles fonctions)

---

## Version 0.2.0 — 2026-07-xx

### Nouvelles fonctions

- **`calc_prevalence()`** : Calcul de prévalence avec IC95% (Wilson).
- **`bivariate_or_table()`** : Tableau d'Odds Ratios bivariés avec IC95% et p-value.
- **`descr_by_group()`** : Statistiques descriptives par groupe avec tests (T-test, ANOVA, Mann-Whitney, Kruskal-Wallis).
- **`descr_multi_choice()`** : Analyse des questions à choix multiples.
- **`plot_barplot()`**, **`plot_pie_chart()`**, **`plot_stacked_bar_100()`**, **`plot_grouped_bar()`**, **`plot_boxplot()`**, **`plot_heatmap_matrix()`** : Suite complète de visualisations ggplot2.
- **`clean_text()`**, **`clean_binary()`**, **`clean_numeric()`** : Fonctions de nettoyage typées.
- **`impute_mode()`**, **`impute_mean()`** : Imputation simple.
- **`theme_analytique()`** unifié avec **`format_flextable()`** (suppression de la redondance).

### Tests

- 55 tests unitaires passés — `FAIL 0 | WARN 0 | SKIP 0 | PASS 55`

---

## Version 0.1.0 — 2026-07-xx

### Nouvelles fonctions

- **`descr_categorial()`** : Fréquences pour variables catégorielles.
- **`descr_numeric()`** : Statistiques descriptives pour variables numériques.
- **`descr_binary()`** : Prévalence pour variables binaires.
- **`analyse_descriptive_multiple()`** : Analyse multi-variables automatique.
- **`cross_table_uniq_mod()`** : Tableau croisé avec test χ²/Fisher.
- **`cross_multi()`** : Croisement multi-variables.
- **`quick_code()`**, **`collapse_categories()`**, **`categorize_numeric()`** : Utilitaires de recodage.
- **`clean_names()`** : Nettoyage des noms de colonnes.
- **`missing_report()`** : Rapport des valeurs manquantes.
- **`plot_distribution()`** : Visualisation adaptative.
- **`export_to_word()`** : Export Word professionnel.
- **`run_analytix_ui()`** : Interface Shiny interactive.

---

## Version 0.0.0.9000 — Initial

- Création du package `analytix`.
- Infrastructure de base : `DESCRIPTION`, `NAMESPACE`, `roxygen2`, `testthat`.
