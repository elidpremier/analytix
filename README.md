# analytix

<div align="center">

**Outils d'automatisation de l'analyse de données pour la génération de rapports professionnels**

Nettoyage, description, statistiques, visualisation et export Word — entièrement en français

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Version](https://img.shields.io/badge/Version-0.3.0-blue.svg)
![Tests](https://img.shields.io/badge/Tests-87%20pass-brightgreen.svg)
![Fonctions](https://img.shields.io/badge/Fonctions-50-informational.svg)

</div>

---

## 🎯 Caractéristiques principales

- **🧹 Nettoyage & Préparation** : nettoyage de texte, recodage, imputation, import Excel/CSV, labellisation
- **📊 Analyses univariées** : catégorielles, numériques, âge, Likert, choix multiples, prévalences IC95%
- **🔀 Analyses bivariées** : tableaux croisés, OR bivariés, comparaison par groupes
- **🔬 Statistiques avancées** : ANOVA + Tukey, régression logistique multivariée, indicateurs diagnostiques, corrélations
- **📈 Visualisations** : barplots, camemberts, boxplots, Likert divergent, heatmap de corrélations, carte des manquants
- **📤 Export professionnel** : tableaux flextable, export Word structuré (tableau unique ou liste complète)
- **🇫🇷 100% francophone** : virgule décimale, libellés métier, format épidémiologiquement rigoureux

---

## 📦 Installation

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer analytix depuis GitHub
devtools::install_github("elidpremier/analytix")

# Charger le package
library(analytix)
```

> 💡 Les dépendances (`dplyr`, `flextable`, `officer`, `ggplot2`, etc.) sont installées automatiquement.

---

## 🗂️ Vue d'ensemble des fonctions

### 🧹 Nettoyage & Préparation des données

| Fonction | Description |
|---|---|
| `import_clean()` | Import Excel/CSV + nettoyage automatique des noms |
| `clean_names()` | Nettoyage des noms de colonnes (accents, espaces, casse) |
| `clean_text()` | Nettoyage d'une variable textuelle |
| `clean_binary()` | Standardisation d'une variable binaire (Oui/Non, 0/1) |
| `clean_numeric()` | Nettoyage d'une variable numérique (virgules, espaces) |
| `label_vars()` | Attacher des libellés à plusieurs variables en une seule opération |
| `detect_outliers()` | Détection des valeurs aberrantes (IQR, Z-score) avec rapport |
| `quick_code()` | Recodage rapide d'une variable catégorielle |
| `collapse_categories()` | Regroupement de modalités |
| `categorize_numeric()` | Conversion numérique → catégorielle par tranches |
| `impute_mode()` | Imputation par le mode |
| `impute_mean()` | Imputation par la moyenne |
| `missing_report()` | Rapport des taux de valeurs manquantes |

---

### 📊 Analyse univariée

| Fonction | Description |
|---|---|
| `descr_categorial()` | Fréquences et pourcentages pour variables catégorielles |
| `descr_numeric()` | Statistiques descriptives pour variables numériques |
| `descr_binary()` | Prévalence et IC95% pour variables binaires |
| `descr_age()` | Résumé standardisé d'une variable âge (stats + tranches) |
| `descr_likert()` | Tableau + graphique pour une variable Likert |
| `recode_likert()` | Recodage texte → numérique selon un mapping Likert |
| `multi_likert_table()` | Tableau récapitulatif de plusieurs variables Likert |
| `descr_multi_choice()` | Analyse des questions à choix multiples |
| `calc_prevalence()` | Calcul de prévalence avec IC95% (Wilson) |
| `analyse_descriptive_multiple()` | Analyse automatisée de plusieurs variables mixtes |

---

### 🔀 Analyse bivariée

| Fonction | Description |
|---|---|
| `cross_table_uniq_mod()` | Tableau croisé avec test χ² ou Fisher |
| `cross_multi()` | Tableau croisé multi-variables (outcome vs plusieurs prédicteurs) |
| `descr_by_group()` | Statistiques descriptives numériques par groupe + tests |
| `bivariate_or_table()` | Tableau d'Odds Ratios bivariés (régression logistique) |

---

### 🔬 Statistiques avancées

| Fonction | Description |
|---|---|
| `multivariable_logistic_table()` | Tableau de régression logistique multivariée (ORa, IC95%, p) |
| `anova_table()` | ANOVA à un facteur + post-hoc Tukey |
| `correlation_table()` | Matrice de corrélations (Pearson/Spearman) formatée |
| `calc_sensitivity_specificity()` | Se, Sp, VPP, VPN, LR+/- avec IC95% |

---

### 📈 Visualisations

| Fonction | Description |
|---|---|
| `plot_barplot()` | Graphique en barres pour variables catégorielles |
| `plot_pie_chart()` | Camembert pour variables catégorielles |
| `plot_boxplot()` | Boxplot d'une variable numérique par groupe |
| `plot_grouped_bar()` | Barres groupées pour variables bivariées |
| `plot_stacked_bar_100()` | Barres empilées à 100% |
| `plot_distribution()` | Histogramme/densité adaptatif |
| `plot_likert_divergent()` | Graphique divergent pour échelles de Likert |
| `plot_correlation()` | Heatmap de corrélations (ggplot2) |
| `plot_missing_map()` | Carte visuelle des données manquantes |
| `plot_heatmap_matrix()` | Heatmap d'une matrice de données |

---

### 📤 Mise en forme & Export

| Fonction | Description |
|---|---|
| `theme_analytique()` | Thème flextable professionnel (en-têtes, bordures, police) |
| `format_flextable()` | Formatage avancé d'un flextable existant |
| `export_to_word()` | Export Word d'objets individuels ou de l'environnement global |
| `export_all_tables()` | Export Word structuré d'une liste nommée de tableaux |
| `fmt_regression_fr()` | Formatage francophone des résultats de régression |

---

## 🚀 Exemples rapides

### Importer et nettoyer des données

```r
library(analytix)

# Import Excel + nettoyage automatique
df <- import_clean("data/enquete.xlsx", sheet = 1)

# Attacher des libellés
df <- label_vars(df, c(
  age   = "Âge en années",
  sexe  = "Sexe du participant",
  score = "Score de satisfaction (1-5)"
))

# Vérifier les valeurs aberrantes
res <- detect_outliers(df, age, var_name = "Âge")
res$summary  # flextable
```

### Analyse descriptive complète

```r
# Variable catégorielle
descr_categorial(df, sexe, var_name = "Sexe")

# Variable âge avec tranches
descr_age(df, age, var_name = "Âge des participants")

# Variable Likert (avec graphique)
descr_likert(df, score, var_name = "Satisfaction globale",
             plot = TRUE)$plot

# Plusieurs variables Likert en un tableau
multi_likert_table(df,
  cols = c("q1", "q2", "q3"),
  var_labels = c(q1 = "Accessibilité", q2 = "Qualité", q3 = "Délai"))
```

### Statistiques avancées

```r
# Régression logistique multivariée
mod <- glm(issue ~ age + sexe + groupe, data = df, family = binomial())
multivariable_logistic_table(mod)

# ANOVA + Tukey
res <- anova_table(df, score, groupe, var_name = "Score", group_name = "Groupe")
res$anova   # tableau ANOVA
res$tukey   # comparaisons par paires

# Matrice de corrélations
correlation_table(df, cols = c("age", "score", "poids"))
plot_correlation(df, cols = c("age", "score", "poids"))

# Indicateurs diagnostiques
calc_sensitivity_specificity(actual = df$reference, predicted = df$test)
```

### Visualisation Likert divergente

```r
plot_likert_divergent(df,
  cols = c("q1", "q2", "q3"),
  var_labels = c(q1 = "Accessibilité", q2 = "Qualité", q3 = "Délai"),
  title = "Satisfaction des bénéficiaires")
```

### Export Word complet

```r
# Option 1 : exporter une liste nommée de tableaux
export_all_tables(
  tables = list(
    "Description de la population"  = descr_age(df, age),
    "Répartition par sexe"          = descr_categorial(df, sexe),
    "Satisfaction globale (Likert)" = descr_likert(df, score)
  ),
  file     = "rapport_final.docx",
  title    = "Rapport d'analyse — Enquête 2025",
  author   = "IDO Esliée"
)

# Option 2 : exporter des objets individuels
export_to_word(tab1, tab2, tab3, path = "rapport.docx")
```

---

## 📚 Dépendances

| Package | Rôle |
|---|---|
| `dplyr` | Manipulation de données |
| `flextable` | Création de tableaux professionnels |
| `officer` | Export vers Word (.docx) |
| `ggplot2` | Visualisations |
| `tidyr` | Remise en forme des données |
| `rlang` | Programmation non-standard (NSE) |
| `stringr` | Manipulation de chaînes |
| `readxl` | Lecture de fichiers Excel |
| `stats` | Fonctions statistiques de base (R base) |
| `colorspace` | Gestion des couleurs |
| `shiny` / `bslib` | Interface utilisateur interactive |

---

## 📖 Workflow complet recommandé

```r
library(analytix)

# ── 1. Import & Nettoyage ──────────────────────────────────────
df <- import_clean("data/enquete.xlsx")
df <- label_vars(df, c(age = "Âge", sexe = "Sexe", note = "Note /5"))

# ── 2. Qualité des données ─────────────────────────────────────
missing_report(df)
detect_outliers(df, age)$summary

# ── 3. Descriptif univarié ─────────────────────────────────────
t_age  <- descr_age(df, age, var_name = "Âge")
t_sexe <- descr_categorial(df, sexe, var_name = "Sexe")
t_note <- descr_likert(df, note, var_name = "Note globale")

# ── 4. Analyses bivariées ──────────────────────────────────────
t_croise <- cross_table_uniq_mod(df, issue, sexe,
  var1_name = "Issue", var2_name = "Sexe")
t_or     <- bivariate_or_table(df, issue, sexe)

# ── 5. Multivarié ─────────────────────────────────────────────
mod <- glm(issue ~ age + sexe + groupe, data = df, family = binomial())
t_multi <- multivariable_logistic_table(mod)

# ── 6. Export Word ─────────────────────────────────────────────
export_all_tables(
  list("Âge" = t_age, "Sexe" = t_sexe, "Note" = t_note,
       "Croisement" = t_croise, "OR bivariés" = t_or,
       "Multivariée" = t_multi),
  file  = "rapport_complet.docx",
  title = "Rapport d'analyse"
)
```

---

## 📚 Documentation & Ressources

- **[Getting Started](GETTING_STARTED.md)** — Guide rapide pour débuter en 5 minutes
- **[News & Changelog](NEWS.md)** — Historique des versions
- **[Contributing Guide](CONTRIBUTING.md)** — Comment contribuer
- **[Code of Conduct](CODE_OF_CONDUCT.md)** — Normes communautaires
- **Aide intégrée** : `?descr_likert`, `?correlation_table`, `?export_all_tables`, etc.

---

## 🤝 Contribution

Les contributions sont bienvenues !

1. Ouvrez une **issue** pour discuter de vos modifications
2. Créez une **branche** à partir de `main`
3. Soumettez une **pull request** avec une description claire

---

## 📜 Licence

Ce projet est sous licence **MIT** — voir le fichier [LICENSE](LICENSE) pour les détails.
