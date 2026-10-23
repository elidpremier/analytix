# Audit de normalisation et de présentation

Date de l'audit : 2026-10-23

## Synthèse

Le projet est fonctionnellement riche, mais il mélange encore plusieurs conventions
historiques. La priorité est de stabiliser l'API bêta, de rendre la documentation
reproductible et d'appliquer une présentation homogène aux sorties statistiques.
La suppression de la télémétrie automatique est traitée dans la version
`0.6.0.9000`.

## Référentiels utilisés

- conventions de structure et de contrôle des packages R (`DESCRIPTION`, roxygen2,
  `R CMD check`, tests testthat) ;
- versionnement sémantique, avec `0.x` pour une API encore susceptible d'évoluer
  et un suffixe de développement `.9000` pour une version bêta non publiée ;
- conventions de nommage cohérentes de type `famille_action()` ;
- principes d'accessibilité des documents et tableaux : contraste lisible,
  information non portée uniquement par la couleur, titres et notes explicites ;
- bonnes pratiques de confidentialité : minimisation des données, consentement
  explicite et absence de transfert automatique.

## Constats et ajustements proposés

| Priorité | Constat | Ajustement recommandé | Critère d'acceptation |
|---|---|---|---|
| P0 | L'ancienne télémétrie pouvait créer un identifiant persistant et un journal local. | Supprimer toute collecte automatique ; conserver uniquement les retours initiés par l'utilisateur. | Aucun appel réseau ou fichier de suivi au chargement ou à l'appel d'une fonction analytique. |
| P0 | Le README affichait `0.4.0` alors que le package avait évolué. | Utiliser une source unique de version et vérifier le badge à chaque release. | README, `DESCRIPTION` et `NEWS.md` concordants. |
| P1 | Des doublons historiques existent (`desc_*`/`descr_*`, plusieurs alias `plot_*`). | Documenter les alias comme compatibilité, puis choisir une famille canonique pour les nouvelles fonctions. | Chaque export possède une fonction canonique documentée et un alias marqué comme tel. |
| P1 | Les nombres de tests et de fonctions affichés dans le README sont statiques. | Remplacer les badges manuels par des indicateurs mis à jour lors de la release, ou les retirer. | Aucun compteur obsolète après ajout ou retrait de tests. |
| P1 | Les sorties utilisent plusieurs libellés et conventions de présentation. | Centraliser les libellés (`n`, `%`, IC95 %, p-value), les arrondis, la virgule décimale et les notes de méthode. | Une même statistique est présentée de façon identique dans les fonctions `desc_*`, `tbl_*` et les rapports. |
| P1 | Les tableaux et graphiques n'ont pas encore une charte d'accessibilité formalisée. | Définir une palette contrastée, des titres/captions, des unités et des textes alternatifs ; ne pas coder une information uniquement par la couleur. | Vérification visuelle et test de contraste pour les thèmes exportés. |
| P2 | La documentation est principalement en français mais les noms d'arguments et certains textes sont mixtes. | Conserver les noms d'API stables, franciser les messages utilisateur et fournir un glossaire bilingue pour les termes statistiques. | Messages et exemples cohérents, sans changement silencieux de signature. |
| P2 | Les appels LLM sont externes et peuvent transmettre des statistiques agrégées. | Documenter clairement le périmètre des données envoyées, le fournisseur et la responsabilité de l'utilisateur. | Option désactivée par défaut et avertissement dans la documentation des fonctions LLM. |

## Plan de normalisation recommandé

1. **Avant chaque version bêta** : `roxygen2::roxygenise()`, `R CMD check`,
   tests testthat et vérification des fichiers inclus dans le tarball.
2. **Avant la version stable 1.0.0** : figer les fonctions canoniques, déprécier
   les alias historiques avec une période de transition et publier une matrice
   de compatibilité.
3. **Présentation** : créer un thème unique pour `flextable` et `ggplot2`, avec
   une règle documentée pour les arrondis, les intervalles de confiance, les
   valeurs manquantes et les p-values.
4. **Qualité** : ajouter des tests de non-régression sur les formats de sortie,
   la confidentialité (aucun fichier de suivi) et l'accessibilité minimale des
   documents exportés.
5. **Publication** : conserver `NEWS.md` comme journal de référence, dater chaque
   entrée, et vérifier que la version de `DESCRIPTION` correspond au tag GitHub.

## Baseline phase 0 — 2026-10-23

### Environnement

- R `4.6.1` ;
- `testthat` disponible ;
- `pROC`, `survminer` et `bsicons` absents de l'environnement local ;
- version contrôlée : `0.6.0.9000`.

### Commandes exécutées

```r
roxygen2::roxygenise(clean = TRUE)
testthat::test_dir("tests/testthat")
```

```bash
R CMD build . --no-build-vignettes
_R_CHECK_FORCE_SUGGESTS_=false \
  R CMD check --no-manual --no-vignettes --no-build-vignettes \
  analytix_0.6.0.9000.tar.gz
```

### Résultats

- génération roxygen : réussie ;
- tests `testthat` : réussis, avec avertissements fonctionnels déjà présents ;
- construction du tarball : réussie ;
- installation et chargement du tarball : réussis ;
- `R CMD check` : les erreurs bloquantes des exemples `desc_multi_choice()`,
  `plot_missing()` et `tbl_bivariate_or()` ont été corrigées ; le contrôle sans
  les suggestions indisponibles termine avec succès (`0` erreur, `3` warnings,
  `2` notes) et les tests du package passent.

### Limites conservées volontairement

Les avertissements relatifs aux caractères non ASCII, aux dépendances déclarées
mais non utilisées, aux imports implicites et à la vignette sans artefact sont
enregistrés comme travaux ultérieurs. Ils ne sont pas corrigés dans cette phase
pour éviter de mélanger la baseline avec la normalisation du code et de la
présentation.

### Corrections incluses dans la phase 0

- exemples roxygen corrigés pour utiliser des noms de colonnes explicites ;
- `plot_missing()` rendue robuste aux colonnes de types mixtes ;
- arguments publics manquants ajoutés à la documentation roxygen ;
- fichiers d'environnement et répertoires de contrôle exclus du tarball via
  `.Rbuildignore`.

## Phase 1 — Sécurité et confidentialité

La phase 1 ajoute des tests de non-régression couvrant :

- l'absence de fichier `.analytix_config` ou `.analytix_log.json` après une
  analyse ;
- l'absence des anciennes surfaces de télémétrie et de `httr2` ;
- la préparation locale d'un courriel pour un bug ou une suggestion ;
- le fonctionnement local du résumé LLM sans clé API.

La documentation LLM précise désormais qu'un appel externe est possible
uniquement lorsqu'il est explicitement demandé et qu'une clé API est disponible.
Les tableaux et contextes fournis restent sous la responsabilité de l'utilisateur
et doivent être agrégés et exempts de données sensibles.

## Phase 2 — Release et documentation

La procédure de release est désormais vérifiable sans compteur manuel :

- `scripts/check_release.R` compare la version de `DESCRIPTION`, le badge du
  README et la dernière version de `NEWS.md` ;
- les badges statiques de nombre de tests et de fonctions ont été supprimés ;
- le README décrit l'ordre de validation : cohérence, roxygen2, build, check et
  tests ;
- la procédure ne crée ni commit ni publication automatiquement.

La phase 2 est considérée comme valide lorsque le script de cohérence passe et
que le tarball construit avec cette version passe les tests et le contrôle du
package. Les warnings ou notes de `R CMD check` restent suivis séparément et ne
doivent pas être masqués par la procédure.

## Phase 3 — Compatibilité de l'API

La matrice [`API_COMPATIBILITY.md`](./API_COMPATIBILITY.md) formalise les
fonctions canoniques et les alias historiques. Les alias sont conservés pendant
la bêta, émettent un avertissement et délèguent vers leur fonction canonique.
Le test `test-api-compatibility.R` vérifie que chaque alias et chaque cible sont
exportés et que chaque alias avertit avant sa délégation.

La phase 3 ne supprime aucune fonction et ne modifie aucune signature publique.
Une suppression future nécessitera une entrée `NEWS.md`, une période de
transition et une alternative documentée.

### Validation de la phase 3

- matrice de compatibilité ajoutée ;
- 56 alias historiques et leurs cibles canoniques contrôlés ;
- chaque alias est exporté et émet un avertissement avant délégation ;
- suite complète `testthat` réussie avec le package chargé ;
- aucun changement de signature publique ;
- aucun commit automatique créé.

### Lot suivant de la phase 4

Le formatage centralisé est maintenant utilisé par `tbl_cross_multi()` et les
résultats diagnostiques/régressions de `epid_stats.R`, sans changement de
signature publique. Les sorties existantes restent structurées de la même façon ;
seules les chaînes numériques utilisent les helpers communs.

Le même socle est désormais utilisé par `fmt_regression()` et `tbl_roc()` pour
les estimations, intervalles de confiance, AUC et seuils. Ces fonctions
conservent leurs dépendances optionnelles et leur structure de retour.

Les tableaux descriptifs secondaires (`desc_binary()`, `desc_multi_choice()`,
`desc_grouped()`, `desc_score()`, `report_missing()` et `prep_outliers()`)
utilisent également les helpers communs pour les pourcentages et les valeurs
numériques, sans modification de leur API publique.

Les étiquettes des graphiques de `plot_utils.R` et les tableaux Likert utilisent
également ce socle. Les calculs internes restent numériques ; seul l'affichage
des pourcentages et scores est normalisé.

Les tableaux croisés spécialisés, les odds ratios bivariés, les analyses par
groupe et le descriptif de l'âge utilisent désormais les mêmes helpers pour les
pourcentages, intervalles, rapports et p-values.

Les tableaux ANOVA et corrélation, les heatmaps, les rapports de valeurs
manquantes avancés et les tableaux Kaplan-Meier utilisent également les helpers
centraux.

Le générateur de rapports Word réutilise désormais les mêmes helpers pour les
taux de complétude, les indicateurs ROC, les seuils, les intervalles et les
résultats Kaplan-Meier. Les dates, identifiants et messages techniques restent
formatés selon leur nature propre.

## Phase 4 — Normalisation des sorties statistiques

Le premier lot introduit les helpers internes de formatage dans
`R/format_utils.R` et les applique à `desc_numeric()`, `desc_categorical()` et
`desc_prevalence()`. Il uniformise la virgule décimale, les pourcentages, les
effectifs, les p-values et les intervalles de confiance.

Les autres fonctions ne sont pas encore migrées : elles feront l'objet de lots
successifs avec tests de non-régression afin d'éviter une modification globale
des sorties existantes.

### Validation du premier lot de la phase 4

- helpers de formatage testés pour les décimales, effectifs, pourcentages,
  p-values et IC95 % ;
- sorties de `desc_numeric()`, `desc_categorical()` et `desc_prevalence()`
  validées ;
- suite complète `testthat` réussie ;
- tarball et `R CMD check` réussis sans erreur ;
- warnings et notes restants inchangés et déjà documentés ;
- aucun commit automatique créé.

### Clôture de la phase 4

La migration progressive couvre désormais les sorties descriptives, tableaux
croisés, régressions, ROC, épidémiologie, survie, graphiques, heatmaps, Likert
et rapports Word. Les helpers internes de `R/format_utils.R` sont utilisés pour
les décimales, pourcentages, effectifs, p-values et intervalles de confiance.

Les formats non statistiques (dates, identifiants, messages de progression et
messages d'erreur) restent volontairement hors de ce socle. Les signatures
publiques et les structures de retour ont été conservées.

Validation finale :

- suite complète `testthat` réussie ;
- tarball et `R CMD check` réussis sans erreur ;
- warnings et notes connus inchangés ;
- absence d'artefact de validation dans le dépôt ;
- aucun commit automatique créé.

La phase 5 peut commencer avec la charte d'accessibilité : contraste des
palettes, titres explicites, unités, légendes, captions et lisibilité des
tableaux et graphiques.

## Phase 5 — Accessibilité et présentation

Le premier lot de la phase 5 introduit une charte interne de présentation :

- en-tête de tableau accessible par défaut (`#1F4E79`, contraste WCAG AA
  supérieur ou égal à 4,5:1 avec le texte blanc) ;
- validation explicite des couleurs d'en-tête personnalisées ;
- palette qualitative `Analytix` à contraste renforcé ;
- tests automatisés du contraste et de la taille de la palette.

Les couleurs personnalisées restent possibles. Lorsqu'une couleur d'en-tête
personnalisée est sous le seuil WCAG AA, un avertissement explicite est émis
plutôt qu'un remplacement silencieux.
