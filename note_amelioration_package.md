# Note d'amélioration du package (suggestions & remarques)
- [x] ~~inclure la possibilité de filtre dans la fonction d'analyse catégorielle de sorte à pourvoir choisir un sous-groupe~~
- [x] ~~il faut documenter les tests (dans le dossier correspondant)~~
- [x] ~~Refaire la fonction de graphique a partir de rien~~
- [x] ~~Ajouter l'argument de précision de la nature d'une variable dépendante dans la fonction d'analyse multiple (numérique ou catégorielle)~~
- [x] ~~desc_by_group, doit être revu pour ne pas prendre uniquement une variable numéric contre une variable catégorielle~~
- [x] ~~Ajouter la possibilité de pouvoir choisir l'issue d'intérêt à la fonction de mesures d'association bivariée~~
- [x] ~~Mettre en place une fonction de description pour les variables binaires particulièrement~~
- [x] ~~ma fonction d'analyse à un soucis concernant la présence des valeurs manquantes, revoir. l'effectif total est également mal calculé~~
- [x] ~~il faut ajouter le cacul de OR dans le package "cross_multi" et ajouter l'argument de choix du test à appliquer~~
- [x] ~~les pourcentages des tableaux croisée sont mals calculés, il faut cree ces fonctions de façon similaire à celui de SPSS avec peu d'argument~~
- [x] ~~Créer une fonction d'imputation par la methode mice~~
- [x] ~~corriger la fonction d'export qui n'enregistre au nom mentionné~~
- [x] ~~le digit ne fonctionne pas avec les variables numérique, certains arguments ne fonctionnent pas~~
- [x] ~~Ajouter cette fonction à mon package sans le calcul des IC (fmt_regression_fr)~~

# Étapes réalisées dans la version récente (Basé sur l'analyse des projets)
- [x] ~~Créer une vignette (tutoriel complet) illustrant le workflow de A à Z (de l'imputation à l'export Word)~~ (`vignettes/workflow_analytix.Rmd`).
- [x] ~~Ajouter des tests statistiques de comparaison de moyennes (T-test, ANOVA, Mann-Whitney, Kruskal-Wallis) dans `descr_by_group`~~ (`R/descr_by_group.R`).
- [x] ~~Créer un module d'utilitaires de nettoyage et d'imputation~~ (`R/clean_utils.R` : `clean_binary`, `clean_numeric`, `clean_text`, `impute_mode`, `impute_mean`).
- [x] ~~Créer une fonction d'analyse des questions à choix multiples (réponses multiples)~~ (`R/descr_multi_choice.R`).
- [x] ~~Créer une fonction de calcul de prévalences et proportions avec IC95% (Wilson/Exact)~~ (`R/calc_prevalence.R`).
- [x] ~~Créer une fonction de génération de table synthétique d'Odds Ratios bivariés~~ (`R/bivariate_or_table.R`).
- [x] ~~Créer une fonction de formatage académique `flextable` pour rapports Word~~ (`R/format_flextable.R`).
- [x] ~~Créer une fonction de génération de Heatmaps ggplot2 pour les prévalences et profils de résistance~~ (`R/plot_heatmap_matrix.R`).

# Prochaines étapes recommandées
- [ ] Développer une interface Shiny simple pour utiliser le package sans coder (mode "Point & Click").
- [ ] Harmoniser totalement la gestion des données labellisées (attributs `label` et `value.labels`).
