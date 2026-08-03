# Suppression des avertissements "no visible binding for global variable"
# générés par dplyr, ggplot2 et tidyr dans les fonctions du package.
# Déclaration de toutes les variables utilisées dans les pipelines NSE.

utils::globalVariables(c(
  # dplyr / tidyr column refs
  "n", "pct", "total_grp", "label_val", "variable", "valeur",
  "manquant", "obs_id", "niveau", "pct_centred",
  # flextable column refs (from as.data.frame outputs)
  "Statistique", "Valeur", "Modalite", "Effectif", "Pourcentage",
  "Effectif (%)", "Taux (%)",
  # ggplot2 aes refs
  "effectif", "modalite", "etiquette", "label", "Freq", "val",
  "Var1", "Var2", "correlation",
  # dplyr NSE
  ":=", "pourcentage", "pourcentage_formate",
  # cross_table NSE columns
  "Modalite", "Target", "cellule",
  # fmt_regression_fr
  "estimate", "conf.low", "conf.high",
  # descr_by_group
  "Valeur",
  # shiny UI server
  "iris"
))
