# analytix News

## Version 0.5.1 — 2026-09-15 (Résolution Évaluation Boucle & Validation Stricte)

### 🐛 Corrections de bugs & Améliorations Épidémiologiques
- **Résolution universelle des variables en boucle (`.resolve_var_name`)** : Correction du bug où `desc_categorical(df, v)` ou `desc_numeric(df, v)` dans une boucle `for (v in c("col1", "col2"))` échouait avec `"la variable 'v' n'existe pas"`. L'utilitaire `.resolve_var_name` évalue dynamiquement les symboles, chaînes littérales et variables R d'itération.
- **Validation des arguments de `tbl_cross_unique()`** : Ajout d'une vérification explicite qui interrompt l'exécution avec un message clair si aucune variable en ligne n'est fournie dans `...` (`stop("Aucune variable en ligne fournie dans '...'. Usage : tbl_cross_unique(data, target, var1, var2, ...)")`).
- **Clarification des signatures** : Documentation de la distinction entre `target` (colonne cible) et `target_name` (libellé d'affichage uniquement) dans le README.

---

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
- **143 tests unitaires validés avec succès** (`FAIL 0 | WARN 77 | SKIP 0 | PASS 143`).

---

## Version 0.4.0 — 2026-09-02 (Mise à jour majeure)

### 🎨 Améliorations Design & Style
- **Arrière-plan d'entête transparent par défaut** : La couleur par défaut d'arrière-plan des entêtes de tableaux `color` est désormais fixée à `"transparent"` sur **toutes** les fonctions du package (`theme_analytique()`, `format_flextable()`, `descr_numeric()`, `descr_categorial()`, `bivariate_or_table()`, `generate_report()`, etc.).
- **Page d'aide globale du package (`?analytix`)** : Ajout de la documentation au niveau du package (`man/analytix.Rd`) permettant d'afficher la fiche d'aide officielle complète avec `?analytix` dans R, RStudio, VS Code et Positron.
