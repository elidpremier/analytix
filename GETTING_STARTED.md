# 🚀 Démarrage rapide avec analytix

> Guide d'installation et de prise en main en 5 minutes.

---

## 1️⃣ Installation

### 🪟 Windows (RStudio / Positron) — sans Rtools

analytix est un package **100% R** (sans code C/C++). Utilisez `remotes` avec `build = FALSE` pour installer **sans avoir besoin de Rtools** :

```r
# Installer remotes si nécessaire
install.packages("remotes")

# Installer analytix SANS compilation
remotes::install_github("elidpremier/analytix", build = FALSE)

# Charger le package
library(analytix)
```

> ✅ Fonctionne **sans Rtools** sur Windows, RStudio et Positron.

---

### 🍎 macOS

```r
install.packages("pak")
pak::pak("elidpremier/analytix")
library(analytix)
```

> 💡 Si vous avez une erreur de compilation, installez les **Command Line Tools** :
> ```bash
> xcode-select --install
> ```

---

### 🐧 Linux (Ubuntu/Debian)

Installez d'abord les dépendances système si nécessaire :

```bash
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev
```

Puis dans R :

```r
install.packages("pak")
pak::pak("elidpremier/analytix")
library(analytix)
```

---

## 🔍 Résolution des erreurs courantes

| Message d'erreur | Cause | Solution |
|---|---|---|
| `Could not find tools necessary to compile` | Compilation forcée sur Windows | Utiliser `remotes::install_github(..., build = FALSE)` |
| `Rtools is required to build R packages` | Rtools absent (Windows) | Idem — `build = FALSE` contourne Rtools |
| `cannot open URL 'https://api.github.com/...'` | Pas d'accès internet / proxy | Vérifier la connexion réseau |
| `input string is invalid` | Ancienne version d'analytix | Réinstaller avec `remotes::install_github(..., build = FALSE)` |
| `package 'xxx' was built under R version` | Avertissement mineur | Mettre à jour R ou ignorer |

---

## 2️⃣ En 5 minutes — Workflow type

### Importer et préparer des données

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

### Analyses descriptives univariées

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

### Analyses bivariées

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

### Statistiques avancées

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

### Visualisations

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

### Export Word

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
  author   = "IDO Elisée"
)

# Option 2 — Objets individuels
export_to_word(tab1, tab2, tab3, path = "rapport.docx")
```

---

## 3️⃣ Workflow complet

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

## 4️⃣ L'Interface Graphique (GUI) Sans Code

Vous préférez ne pas coder du tout ? **analytix** intègre nativement une interface web complète.

Pour l'ouvrir :
1. Dans RStudio / Positron, cliquez sur **"Addins"** dans la barre d'outils.
2. Cliquez sur **"Lancer Analytix GUI"**.

Vous pouvez également la lancer depuis la console :
```r
analytix::run_gui()
```

L'interface vous guide pas à pas, de l'importation de vos fichiers jusqu'à la génération automatique du rapport Word !

---

## 📖 Aide intégrée

```r
?descr_likert
?anova_table
?correlation_table
?export_all_tables
?calc_sensitivity_specificity
```

---

## 🔗 Liens utiles

| Ressource | Lien |
|---|---|
| README complet | [README.md](README.md) |
| Changelog | [NEWS.md](NEWS.md) |
| Contribuer | [CONTRIBUTING.md](CONTRIBUTING.md) |
| Rtools (Windows) | [cran.r-project.org](https://cran.r-project.org/bin/windows/Rtools/) |
