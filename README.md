# analytix

Outils d’analyse descriptive pour la génération de tableaux professionnels avec **flextable**, optimisés pour les rapports francophones.

---

##  Installation

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer analytix depuis GitHub
devtools::install_github("elidpremier/analytix")

# Charger le package
library(analytix)
```

> 💡 **Note** : les dépendances (`dplyr`, `flextable`, `officer`, etc.) sont installées automatiquement.
> Aucun `library(dplyr)` ou `library(flextable)` n’est requis après `library(analytix)`.

---

##  Fonctions d'Analyse Univariée

### `freq_table()`

Analyse fréquentielle pour **variables catégorielles** (y compris numériques discrètes).

* Effectifs et pourcentages.
* Gestion des valeurs manquantes.
* Formats **compact** ou détaillé.

```r
# Exemple avec le jeu de données iris
freq_table(iris, Species, var_name = "Espèce", digits = 1, compact = TRUE)
```

### `descr_numeric()`

Statistiques descriptives pour **variables numériques** continues.

* Moyenne, médiane, écart-type, min/max, quartiles.
* Option pour l'**asymétrie** (`show_skewness`).
* Gestion des valeurs manquantes.

```r
# Exemple avec la longueur des sépales
descr_numeric(iris, Sepal.Length, digits = 2, show_skewness = TRUE)
```

### `analyse_descriptive_multiple()`

Analyse automatisée de **plusieurs variables** (catégorielles + numériques).

* Détection automatique du type de variable.
* Possibilité de forcer le type (`categorical` / `numeric`).
* Libellés personnalisables.

```r
# Exemple mixte avec mtcars
analyse_descriptive_multiple(
  mtcars,
  vars = c("cyl", "mpg"),
  var_labels = c("cyl" = "Cylindres", "mpg" = "Consommation"),
  var_types = c("mpg" = "numeric")
)
```

---

##  Fonctions d'Analyse Bivariée

### `freq_cross()`

Génère un **tableau croisé (contingence)** de deux variables catégorielles.

* Affiche l'effectif (`n`) et les pourcentages (**lignes, colonnes, total**) dans chaque cellule.
* Calcule et affiche le résultat du **test du χ²**.

```r
# Croisement entre type de transmission (am) et cylindres (cyl) dans mtcars
freq_cross(
  mtcars, am, cyl,
  var1_name = "Boîte automatique",
  var2_name = "Cylindres"
)
```

### `descr_by_group()`

Calcule les **statistiques descriptives** d'une variable numérique **par groupe** d'une variable catégorielle.

* Moyenne, médiane, écart-type, min, max et effectifs par catégorie.
* Tableau idéal pour la comparaison de distributions.

```r
# Consommation (mpg) par nombre de cylindres (cyl)
descr_by_group(
  mtcars, mpg, cyl,
  var_name = "Consommation",
  by_name = "Cylindres"
)
```

---

## 🛠️ Outils Utilitaires pour la Préparation

### `quick_code()`

Recodage rapide et intuitif d’une variable catégorielle.

* Syntaxe courte : `"ancien" = "nouveau"`.
* Gestion des `NA` via l'argument `.na`.
* Non-destructif : les valeurs non recodées sont conservées.

```r
patients <- data.frame(
  sexe = c("H", "F", NA),
  statut = c(1, 2, 1)
)

quick_code(
  patients,
  sexe,
  "H" = "Homme",
  "F" = "Femme",
  .na = "Inconnu"
)
```

### `collapse_categories()`

Regroupement de plusieurs modalités d'une variable catégorielle sous de **nouvelles catégories**.

* Permet de simplifier des variables avec trop de modalités.
* Gestion flexible des modalités non regroupées et des `NA`.

```r
# Regrouper 'setosa' et 'versicolor' sous 'SetosaVersicolor'
collapse_categories(
  iris,
  Species,
  groups = list(SetosaVersicolor = c("setosa", "versicolor")),
  other_label = "Virginica"
)
```

### `missing_report()`

Génère un tableau récapitulatif du **taux de valeurs manquantes** par variable.

* Tableau prêt à l'emploi pour l'évaluation de la qualité des données.
* Affiche l'effectif total, le nombre de manquants et le taux en pourcentage.

```r
# Exemple avec airquality
missing_report(airquality)
```

---

##  Export et Rapport

### `export_to_word()`

Export **professionnel vers Word (.docx)** de tous les tableaux produits par `analytix`.

* Prend en entrée des objets individuels, des listes de résultats, ou scanne l'environnement global.
* Ajout automatique d'un titre (`heading 2`) et de sauts de page optionnels.

```r
# 1. Créer vos tableaux
tab_freq <- freq_table(mtcars, cyl)
tab_descr <- descr_numeric(mtcars, mpg)

# 2. Exporter tout l'environnement
export_to_word(path = "rapport_analytix.docx")
```

---

## 📚 Dépendances

* **dplyr**
* **flextable**
* **officer**
* **tibble**
* **rlang**
* **stats**
* **tidyr** 
---

##  Orientation du package

Conçu pour les **contextes d’expertise analytique francophones** :

* Utilisation de la **virgule** comme séparateur décimal (`12,5 %`).
* Libellés métier clairs.
* Tableaux immédiatement exploitables pour les rapports officiels.


