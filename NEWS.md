# analytix News

## Version 0.5.0 — 2026-09-15 (Optimisations & Harmonisation)

### 🐛 Corrections de bugs Majeurs & Mineurs
- **`tbl_cross_unique()`** : Correction du bug d'extraction de colonne lorsque l'argument `target_name` était renseigné avec un libellé d'affichage. Séparation stricte entre le nom de colonne de données et le libellé de présentation.
- **`export_tables()`** : Extraction récursive et sécurisée des objets `flextable` depuis n'importe quel conteneur (objets directs, classe `analytix_table`, listes d'analyses). Élimination des titres de section orphelins sans tableau et ajout d'un `warning()` explicatif pour les objets non convertibles.
- **`theme_analytique()` & `fmt_flextable()`** : Prise en compte explicite du paramètre `caption` sans générer d'erreur `unused argument`. `fmt_flextable()` est désormais marqué déprécié au profit d'un appel direct à `theme_analytique()`.

### 🚀 Nouvelles fonctionnalités & Harmonisation
- **`tbl_cross_unique()` (p global)** : Ajout automatique d'une colonne dédiée à droite `"p global"` calculant le test d'indépendance global du bloc de variable (Fisher si effectifs théoriques < 5, sinon $\chi^2$).
- **`as_analytix_table()`** : Classe S3 unifiée `analytix_table` (portant `$data` et `$flextable`) pour l'ensemble des fonctions du package, avec méthodes `print` et `as_flextable` dédiées.
- **`desc_score()`** : Nouvelle fonction permettant le calcul d'un score/indice de complétude (nombre d'items prescrits/validés), le calcul des indicateurs continus (moyenne, médiane, min, max) et le découpage optionnel par tranches (`breaks` & `labels`).
- **`desc_multi_choice()`** : Prise en charge de l'argument `positive` (par défaut `c(1, TRUE, "Oui", "Yes", "true", "vrai", "coché")`) permettant d'analyser des réponses textuelles ou facteurs sans conversion préalable.
- **`desc_grouped()`** : Séparation du `data.frame` plat tidy dans `$data` (sans lignes `NA` artificielles) et de la structure `as_grouped_data` utilisée exclusivement pour la génération du `$flextable`.
- **`interp_pvalue()` & `interp_association()`** : Reformulation rigoureuse et non-causale des textes d'interprétation statistique en français.
- **Flexibilité NSE / Chaînes** : Standardisation du support des symboles non quotés et des chaînes de caractères sur toutes les fonctions de description et de modélisation.

### 🧪 Tests & Qualité
- **141 tests unitaires validés avec succès** (`FAIL 0 | WARN 77 | SKIP 0 | PASS 141`).

---

## Version 0.4.0 — 2026-09-02 (Mise à jour majeure)

### 🎨 Améliorations Design & Style
- **Arrière-plan d'entête transparent par défaut** : La couleur par défaut d'arrière-plan des entêtes de tableaux `color` est désormais fixée à `"transparent"` sur **toutes** les fonctions du package (`theme_analytique()`, `format_flextable()`, `descr_numeric()`, `descr_categorial()`, `bivariate_or_table()`, `generate_report()`, etc.).
- **Page d'aide globale du package (`?analytix`)** : Ajout de la documentation au niveau du package (`man/analytix.Rd`) permettant d'afficher la fiche d'aide officielle complète avec `?analytix` dans R, RStudio, VS Code et Positron.

### 🚀 Nouvelles fonctions
- **`recode_odk_binary()`** : Recodage automatique des variables binaires issues d'exports ODK / KoboToolbox / REDCap (convertit `NA`, `""` ou `0` en `"Non"` et les valeurs renseignées/textes en `"Oui"`). Disponible également via l'option `na_as_no = TRUE` dans `clean_binary()`.
- **`descr_grouped_categories()`** : Génération de tableaux récapitulatifs pour des sous-catégories/modalités groupées sous leurs catégories parentes (ex: molécules regroupées par classe thérapeutique). Prend en charge les formats large et long, avec tri automatique et pourcentages calculés sur $N$ total ou par groupe.

### 🧪 Tests & Qualité
- **102 tests unitaires validés** (`FAIL 0 | WARN 0 | SKIP 0 | PASS 102`).

---

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

### 🛠 Intégration complète de l'Interface Graphique (analytix.gui)

- **Embarquement natif** : L'application Web Shiny (`analytix.gui`) est désormais incluse nativement dans le package. Plus besoin de dépôt séparé ni de téléchargements complexes.
- **Nouvelle commande de lancement** : Utilisation de `analytix::run_gui()` au lieu de l'ancienne fonction `run_analytix_ui()` (qui reste disponible comme alias déprécié pour la rétrocompatibilité).
- **Addin RStudio** : Ajout d'un Addin RStudio ("Lancer Analytix GUI") permettant d'ouvrir l'interface graphique en 1 seul clic depuis n'importe quel projet, sans taper de code.
- **Nouveau module `⚡ Rapport Auto`** : Onglet dédié dans l'interface Shiny permettant la génération one-click du rapport Word complet. Inclut : sélection de l'outcome, choix des sections, aperçu des métriques (N, variables, complétude), tableau interactif DT des variables avec types détectés et taux de NA colorés.
- **Fix export global** : Le module `mod_export_server` accepte maintenant `model_reactive` pour intégrer les résultats du module Modélisation dans le rapport. Logique de fallback vers `bivar_reactive` si la modélisation n'a pas été effectuée.
- **Version** : 0.4.0
