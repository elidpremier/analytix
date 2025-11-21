
### analytix

Outils d’analyse descriptive pour la génération de tableaux professionnels avec **flextable**.

---

## 📦 Fonctions principales

### `freq_table()`

Analyse fréquentielle pour variables catégorielles (y compris numériques discrètes).

- Effectifs et pourcentages  
- Gestion des valeurs manquantes  
- Formats compact ou détaillé  
- Export prêt pour les rapports  

```r
# Exemple avec le jeu de données iris
freq_table(iris, Species, var_name = "Espèce", digits = 1)
````


### `descr_numeric()`

Statistiques descriptives pour variables numériques continues.

* Moyenne, médiane, écart-type, min/max, quartiles
* Option pour l’asymétrie (skewness)
* Gestion des valeurs manquantes

```r
# Exemple avec la longueur des sépales
descr_numeric(iris, Sepal.Length, digits = 2, show_skewness = TRUE)
```

---

### `analyse_descriptive_multiple()`

Analyse automatisée de plusieurs variables (catégorielles + numériques).

* Détection automatique du type
* Possibilité de forcer le type (`categorical` / `numeric`)
* Libellés personnalisables

```r
# Exemple mixte avec iris
analyse_descriptive_multiple(
  iris,
  vars = c("Species", "Sepal.Length"),
  var_labels = c(
    "Species" = "Espèce",
    "Sepal.Length" = "Longueur des sépales"
  ),
  var_types = c("Sepal.Length" = "numeric")
)
```

---

### `export_to_word()`

Export vers Word de tableaux individuels, listes ou de tout l’environnement.

* Un seul fichier de sortie
* Sauts de page optionnels
* Titres automatiques

```r
# Depuis une liste (ex : avec mtcars)
resultats <- analyse_descriptive_multiple(
  mtcars,
  c("cyl", "mpg"),
  var_labels = c("cyl" = "Cylindres", "mpg" = "Consommation")
)
export_to_word(resultats, "rapport_analytix.docx")

# Depuis l'environnement
tab1 <- freq_table(iris, Species)
export_to_word(path = "frequences_iris.docx")
```

---

## 🚀 Installation

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer analytix depuis GitHub
devtools::install_github("elidpremier/analytix")

# Charger le package
library(analytix)
```

> 💡 **Note** : les dépendances (`dplyr`, `flextable`, etc.) sont installées automatiquement.
> Aucun `library(dplyr)` ou `library(flextable)` n’est requis après `library(analytix)`.

---

## 📚 Dépendances

* dplyr
* flextable
* officer
* tibble
* rlang
* stats

---

## 🎯 Orientation du package

Conçu pour les **contextes d’expertise analytique francophones** :

* Utilisation de la virgule comme séparateur décimal (`12,5 %`)
* Libellés métier clairs
* Tableaux immédiatement exploitables pour les rapports officiels

```
