# 🚀 Démarrage rapide avec analytix

> Guide d'installation et de prise en main en 5 minutes.

---

## 1️⃣ Installation

### ⚡ Méthode recommandée — `pak` (plus rapide, toutes plateformes)

```r
# Installer pak si nécessaire (une seule fois)
install.packages("pak")

# Installer analytix depuis GitHub
pak::pak("elidpremier/analytix")

# Charger le package
library(analytix)
```

### 🔧 Méthode alternative — `devtools`

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer analytix depuis GitHub
devtools::install_github("elidpremier/analytix")

# Charger le package
library(analytix)
```

> 💡 Toutes les dépendances (`dplyr`, `flextable`, `officer`, `ggplot2`, etc.) sont installées **automatiquement**.

---

## 🪟 Installation sur Windows (étapes complètes)

Windows nécessite **Rtools** pour compiler les packages R depuis les sources.

### Étape 1 — Vérifier votre version de R

```r
R.version$major  # ex: "4"
R.version$minor  # ex: "4.1"
```

### Étape 2 — Installer Rtools

Téléchargez et installez la version correspondant à votre R :

| Version R | Lien de téléchargement |
|---|---|
| **R ≥ 4.4** | 👉 [Rtools 4.5](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html) |
| **R 4.3** | 👉 [Rtools 4.3](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html) |
| **R 4.2** | 👉 [Rtools 4.2](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html) |

> ✅ Lors de l'installation, cochez **"Add Rtools to PATH"**.  
> 🔄 **Redémarrez RStudio ou Positron** après l'installation.

### Étape 3 — Vérifier que Rtools est bien installé

```r
pkgbuild::check_build_tools()
# → doit afficher : "Your system is ready to build R packages!"
```

### Étape 4 — Installer analytix

```r
install.packages("pak")
pak::pak("elidpremier/analytix")
library(analytix)
```

---

## 🍎 Installation sur macOS

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

## 🐧 Installation sur Linux (Ubuntu/Debian)

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
| `Rtools is required to build R packages` | Rtools absent (Windows) | Installer Rtools (voir ci-dessus) |
| `cannot open URL 'https://api.github.com/...'` | Pas d'accès internet / proxy | Vérifier la connexion réseau |
| `input string is invalid` | Ancienne version d'analytix | Mettre à jour via `pak::pak("elidpremier/analytix")` |
| `Could not find tools necessary to compile` | Rtools non détecté | Redémarrer RStudio/Positron après installation Rtools |
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
  author   = "IDO Esliée"
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
