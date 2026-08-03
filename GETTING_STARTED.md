# Démarrage rapide avec analytix

## Installation

```r
# Depuis GitHub
devtools::install_github("elidpremier/analytix")
library(analytix)
```

---

## En 5 minutes

### 1. Importer et préparer des données

```r
library(analytix)

# Import Excel avec nettoyage automatique des noms de colonnes
df <- import_clean("data/enquete.xlsx", sheet = 1)

# Attacher des libellés lisibles
df <- label_vars(df, c(
  age   = "Âge en années",
  sexe  = "Sexe du participant",
  score = "Score de satisfaction (1–5)"
))

# Vérifier les données manquantes
missing_report(df)

# Détecter les valeurs aberrantes
detect_outliers(df, age, var_name = "Âge")$summary
```

---

### 2. Analyses descriptives univariées

```r
# Variable catégorielle
descr_categorial(df, sexe, var_name = "Sexe")

# Variable numérique
descr_numeric(df, age, var_name = "Âge", digits = 1)

# Variable âge (stats + tranches automatiques)
descr_age(df, age, var_name = "Âge des participants")

# Variable Likert (1–5)
descr_likert(df, score, var_name = "Satisfaction globale")

# Plusieurs variables Likert en un tableau
multi_likert_table(df,
  cols        = c("q1", "q2", "q3"),
  var_labels  = c(q1 = "Accessibilité", q2 = "Qualité", q3 = "Délai"))
```

---

### 3. Analyses bivariées

```r
# Tableau croisé avec test χ²
cross_table_uniq_mod(df, issue, sexe,
  var1_name = "Issue", var2_name = "Sexe")

# Plusieurs croisements en une commande
cross_multi(df, outcome = issue, predictors = c("sexe", "groupe", "tranche_age"))

# OR bivariés (régression logistique simple)
bivariate_or_table(df, issue, sexe)

# Comparaison de moyennes par groupe
descr_by_group(df, score, groupe,
  var_name = "Score", by_name = "Groupe")
```

---

### 4. Statistiques avancées

```r
# Régression logistique multivariée
mod <- glm(issue ~ age + sexe + groupe, data = df, family = binomial())
multivariable_logistic_table(mod)

# ANOVA + Tukey
res <- anova_table(df, score, groupe)
res$anova   # tableau ANOVA
res$tukey   # comparaisons post-hoc

# Matrice de corrélations (tableau + graphique)
correlation_table(df, cols = c("age", "score", "duree"))
plot_correlation(df, cols = c("age", "score", "duree"))

# Indicateurs diagnostiques (Se, Sp, VPP, VPN)
calc_sensitivity_specificity(actual = df$reference, predicted = df$test)
```

---

### 5. Visualisations

```r
# Barplot simple
plot_barplot(df, sexe, var_name = "Sexe")

# Likert divergent (satisfaction)
plot_likert_divergent(df,
  cols       = c("q1", "q2", "q3"),
  var_labels = c(q1 = "Accessibilité", q2 = "Qualité", q3 = "Délai"))

# Carte des données manquantes
plot_missing_map(df)

# Boxplot par groupe
plot_boxplot(df, score, groupe, var_name = "Score", by_name = "Groupe")
```

---

### 6. Export Word

```r
# Option 1 — Liste nommée de tableaux → Word structuré
export_all_tables(
  tables = list(
    "Description de la population"  = descr_age(df, age),
    "Répartition par sexe"          = descr_categorial(df, sexe),
    "Satisfaction (Likert)"         = descr_likert(df, score),
    "Tableau croisé"                = cross_table_uniq_mod(df, issue, sexe),
    "Régression multivariée"        = multivariable_logistic_table(mod)
  ),
  file     = "rapport_final.docx",
  title    = "Rapport d'analyse — Enquête 2025",
  author   = "IDO Esliée"
)

# Option 2 — Objets individuels
export_to_word(tab1, tab2, tab3, path = "rapport.docx")
```

---

## Workflow complet

```r
library(analytix)

# ── 1. Données ─────────────────────────────────────────────────
df <- import_clean("data/enquete.xlsx")
df <- label_vars(df, c(age = "Âge", sexe = "Sexe", score = "Score"))

# ── 2. Qualité ──────────────────────────────────────────────────
missing_report(df)

# ── 3. Univarié ─────────────────────────────────────────────────
t1 <- descr_age(df, age)
t2 <- descr_categorial(df, sexe)
t3 <- descr_likert(df, score)

# ── 4. Bivarié ──────────────────────────────────────────────────
t4 <- cross_table_uniq_mod(df, issue, sexe)
t5 <- bivariate_or_table(df, issue, sexe)

# ── 5. Multivarié ───────────────────────────────────────────────
mod <- glm(issue ~ age + sexe, data = df, family = binomial())
t6  <- multivariable_logistic_table(mod)

# ── 6. Export ───────────────────────────────────────────────────
export_all_tables(
  list("Âge" = t1, "Sexe" = t2, "Score" = t3,
       "Croisement" = t4, "OR" = t5, "Multivarié" = t6),
  file  = "rapport.docx",
  title = "Rapport d'analyse"
)
```

---

## Aide intégrée

```r
?descr_likert
?anova_table
?correlation_table
?export_all_tables
?calc_sensitivity_specificity
```

- **README complet** → [README.md](README.md)
- **Changelog** → [NEWS.md](NEWS.md)
- **Contribution** → [CONTRIBUTING.md](CONTRIBUTING.md)
