# analytix News

## Version 0.4.0 — 2026-08-17

### 🚀 Nouveautés Phase 2 (Survie & ROC)

- **`roc_table()`** : Calcule la courbe ROC, l'AUC (avec IC95% de DeLong) et le seuil optimal (Youden) pour un prédicteur. Retourne une liste contenant un tableau `flextable` formaté et le graphique `ggplot2` (via pROC).
- **`km_table()` / `km_plot()`** : Analyse de survie Kaplan-Meier. Retourne les médianes de survie, les effectifs et le test du Log-Rank sous forme de tableau formaté et de courbe de survie (via survival et survminer). Intègre automatiquement les covariables.
- **`analytix_sample_data()`** : Générateur intégré d'un jeu de données cliniques fictives de 200 patients (âge, IMC, statut tabagique, outcome décès, délai de survie). Idéal pour illustrer les fonctions du package.
- **Rapport pleine page** : Amélioration de `generate_report()`, qui intègre automatiquement l'analyse de survie et ROC (si les variables adéquates sont fournies) et applique dorénavant le thème `theme_analytique()` en *pleine page (fit_to_width)* à tous les tableaux générés pour un rendu Word parfaitement aligné.

---

### 🚀 Nouveautés Phase 1 (Rapport & Détection Auto)

- **`generate_report()`** : Génération automatique d'un rapport Word complet (`.docx`) à partir d'un `data.frame`. En une seule commande, produit un rapport incluant : page de titre, synthèse du jeu de données, données manquantes (tableau + heatmap), statistiques descriptives avec graphiques, analyses bivariées (Odds Ratios), régression logistique multivariée et matrice de corrélations. Paramétrable par sections, digits, outcome et auteur.
  ```r
  generate_report(mon_df, output = "rapport.docx",
                  title = "Étude clinique", author = "Dr. IDO",
                  outcome = "deces")
  ```

- **`auto_describe()`** : Détection automatique du type de chaque variable (binaire, catégorielle, numérique continue) et dispatch vers la fonction descriptive appropriée. Retourne une liste nommée de résultats avec l'attribut `var_types` (tableau récapitulatif des types détectés).
  ```r
  res <- auto_describe(mon_df, verbose = TRUE)
  attr(res, "var_types")  # tableau des types
  ```

### 🛠 Corrections & améliorations — GUI (analytix.gui)

- **Nouveau module `⚡ Rapport Auto`** : Onglet dédié dans l'interface Shiny permettant la génération one-click du rapport Word complet. Inclut : sélection de l'outcome, choix des sections, aperçu des métriques (N, variables, complétude), tableau interactif DT des variables avec types détectés et taux de NA colorés.
- **Fix export global** : Le module `mod_export_server` accepte maintenant `model_reactive` pour intégrer les résultats du module Modélisation dans le rapport. Logique de fallback vers `bivar_reactive` si la modélisation n'a pas été effectuée.
- **Version** : 0.4.0

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
