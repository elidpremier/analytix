# analytix

<div align="center">

**Outils d'analyse descriptive pour la génération de tableaux professionnels**

Création de rapports analytiques automatisés en français, avec **flextable** et export **Word (.docx)**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Version](https://img.shields.io/badge/Version-0.0.0.9000-blue.svg)

</div>

---

## 🎯 Caractéristiques principales

- **📊 Analyses univariées** : fréquences, statistiques descriptives, rapports de manquants
- **🔀 Analyses bivariées** : tableaux croisés avec tests statistiques (χ², Fisher)
- **🎨 Sorties professionnelles** : tableaux flextable formatés, export Word intégré
- **🇫🇷 100% francophone** : virgule décimale, libellés métier, format épidémiologiquement rigoureux
- **⚙️ Flexible** : détection automatique de type de variable, surcharges manuelles disponibles
- **🔧 Utilitaires** : recodage rapide, regroupement de catégories, nettoyage de données

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

### `descr_categorial()`

Analyse fréquentielle pour **variables catégorielles** (y compris numériques discrètes).

* Effectifs et pourcentages.
* Gestion des valeurs manquantes.
* Formats **compact** ou détaillé.

```r
# Exemple avec le jeu de données iris
descr_categorial(iris, Species, var_name = "Espèce", digits = 1, compact = TRUE)
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

### `cross_table_uniq_mod()`

Génère un tableau croisé professionnel de deux variables catégorielles, avec effectifs, pourcentages (au choix : par ligne, colonne ou total) et un test d’association (khi² ou Fisher) adapté aux données.

* Affichage clair au format n (p%), conforme aux standards des rapports épidémiologiques.
* Choix explicite du type de pourcentage → interprétation épidémiologique rigoureuse.
* Test automatique ou manuel : khi² (avec ou sans correction de Yates), Fisher exact ou simulé.
* Gestion des valeurs manquantes (NA incluses si présentes).
* Format francophone : virgule décimale, espaces fines insécables (via flextable).
* Tableau directement exportable avec export_to_word().

```r
# Exemple de base : croisement entre transmission et cylindres
cross_table_uniq_mod(
  mtcars, am, cyl,
  var1_name = "Boîte automatique",
  var2_name = "Cylindres"
)

# Exemple avancé : pourcentages en colonne + test de Fisher forcé
cross_table_uniq_mod(
  mtcars, am, cyl,
  pct = "col",
  test = "fisher",
  yates = FALSE
)
```

### `cross_multi`

Générez en un seul appel un tableau synthétique croisant une variable dépendante catégorielle avec plusieurs variables explicatives. Idéal pour les rapports épidémiologiques ou les tableaux descriptifs.


```r
# Tableau bivarié : boîte de vitesse (am) vs cylindres et forme du moteur
cross_multi(
  data = mtcars2,
  outcome = am,
  predictors = c("cyl", "vs")
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

### `categorize_numeric()`

Conversion d'une variable numérique continue en variable catégorielle via discrétisation.

* Création automatique de **bins** (intervalle) basée sur la distribution.
* Étiquétage personnalisable des catégories.
* Gestion intelligente des limites et des `NA`.

```r
# Catégoriser l'âge en groupes cliniquement pertinents
categorize_numeric(
  data = iris,
  var = Sepal.Length,
  breaks = c(0, 5, 6, 8),
  labels = c("Petit", "Moyen", "Grand")
)
```

---

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

### `plot_distribution()`

Génère des **visualisations automatiques** des distributions à partir des résultats d'analyse.

* Graphiques adaptatifs selon le type de variable (barplot, histogramme).
* Compatible avec tous les résultats `analytix` (fréquences, statistiques).
* Thème épidémiologique cohérent.

```r
# Visualiser la distribution d'une variable catégorielle
result <- descr_categorial(iris, Species)
plot_distribution(result)
```

---

## 🎨 Personnalisation et Thème

### `theme_analytique()`

Applique un thème **professionnel et cohérent** aux tableaux flextable.

* En-têtes gris clair avec texte noir.
* Formatage optimisé pour l'impression et le Word.
* Bordures légères, polices légibles.
* Compatible avec tous les objets flextable.

```r
# Appliquer le thème à un tableau existant
tab <- descr_numeric(iris, Sepal.Length)
tab$flextable <- theme_analytique(tab$flextable)
```

---

##  Export et Rapport

### `export_to_word()`

Export **professionnel vers Word (.docx)** de tous les tableaux produits par `analytix`.

* Prend en entrée des objets individuels, des listes de résultats, ou scanne l'environnement global.
* Ajout automatique d'un titre (`heading 2`) et de sauts de page optionnels.

```r
# 1. Créer vos tableaux
tab_freq <- descr_categorial(mtcars, cyl)
tab_descr <- descr_numeric(mtcars, mpg)

# 2. Exporter tout l'environnement
export_to_word(path = "rapport_analytix.docx")
```

---

## 📚 Dépendances

* **dplyr** - Manipulation de données
* **flextable** - Création de tableaux professionnels
* **officer** - Export vers Word
* **tibble** - Structures de données modernes
* **rlang** - Programmation non-standard
* **stats** - Fonctions statistiques de base
* **tidyr** - Remise en forme de données
* **ggplot2** - Visualisations (optionnel)
* **colorspace** - Gestion des couleurs

---

##  Orientation du package

Conçu pour les **contextes d'expertise analytique francophones** :

* ✅ Utilisation de la **virgule** comme séparateur décimal (`12,5 %`)
* ✅ Libellés métier clairs et adaptés
* ✅ Tableaux immédiatement exploitables pour les rapports officiels
* ✅ Format conforme aux standards épidémiologiques
* ✅ Export prêt pour PowerPoint, Word ou impression

---

## 📖 Exemple complet de workflow

```r
library(analytix)

# 1. Charger et préparer les données
data <- mtcars

# 2. Analyses univariées
freq_cyl <- descr_categorial(data, cyl, var_name = "Nombre de cylindres")
desc_mpg <- descr_numeric(data, mpg, var_name = "Consommation (mpg)")

# 3. Analyse bivariée
cross <- cross_table_uniq_mod(
  data, am, cyl,
  var1_name = "Transmission",
  var2_name = "Cylindres",
  pct = "col"
)

# 4. Visualisations
plot_distribution(freq_cyl)
plot_distribution(desc_mpg)

# 5. Export vers Word
export_to_word(
  freq_cyl, desc_mpg, cross,
  path = "rapport_analytique.docx"
)
```

---

## 📚 Documentation & Ressources

- **[Getting Started](GETTING_STARTED.md)** - Guide rapide pour débuter en 5 minutes
- **[News & Changelog](NEWS.md)** - Historique des versions et changements
- **[Contributing Guide](CONTRIBUTING.md)** - Comment contribuer au projet
- **[Code of Conduct](CODE_OF_CONDUCT.md)** - Normes communautaires

---

## 🤝 Contribution

Les contributions sont bienvenues ! Si vous trouvez un bug ou avez une idée de fonctionnalité :

1. Ouvrez une **issue** pour discuter de vos modifications
2. Créez une **branche** à partir de `main`
3. Soumettez une **pull request** avec une description claire

---

## 📜 Licence

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

---

## 💡 Support

Pour toute question sur l'utilisation du package :
- Consultez la documentation des fonctions : `?descr_categorial`, `?export_to_word`, etc.
- Vérifiez les exemples dans cette documentation
- Ouvrez une **discussion** ou une **issue** sur le repository


