#' @title Génération de langage naturel pour l'interprétation statistique
#' @description Fonctions heuristiques pour traduire les résultats statistiques complexes
#' en phrases claires et compréhensibles par des non-statisticiens.
#'
#' @name interpret_stats
NULL

#' @describeIn interpret_stats Interprétation générique d'une p-value
#' @param p_val Valeur de la p-value
#' @param alpha Seuil de significativité (défaut: 0.05)
#' @return Une chaîne de caractères explicative.
#' @export
interpret_pvalue <- function(p_val, alpha = 0.05) {
  if (is.na(p_val) || !is.numeric(p_val)) return("")
  
  if (p_val < 0.001) {
    return("La différence observée est hautement significative sur le plan statistique (p < 0.001). Il est extrêmement improbable que ce résultat soit dû au hasard.")
  } else if (p_val < alpha) {
    return(paste0("La différence observée est statistiquement significative (p = ", formatC(p_val, format = "f", digits = 3), "). On peut conclure qu'il existe un lien réel."))
  } else if (p_val < 0.1) {
    return(paste0("La différence n'est pas tout à fait significative au seuil strict de 5% (p = ", formatC(p_val, format = "f", digits = 3), "), mais on observe une tendance qui mériterait d'être explorée sur un plus grand échantillon."))
  } else {
    return(paste0("Aucune différence statistiquement significative n'a été mise en évidence (p = ", formatC(p_val, format = "f", digits = 3), "). Les écarts observés peuvent être dus aux seules fluctuations d'échantillonnage."))
  }
}

#' @describeIn interpret_stats Interprétation d'un Odds Ratio (OR)
#' @param or_val Valeur de l'Odds Ratio
#' @param p_val Valeur de la p-value
#' @return Une chaîne de caractères explicative.
#' @export
interpret_or <- function(or_val, p_val = NULL) {
  if (is.na(or_val) || !is.numeric(or_val)) return("")
  
  signif_text <- ""
  if (!is.null(p_val) && is.numeric(p_val)) {
    if (p_val < 0.05) {
      signif_text <- " Cette association est statistiquement significative."
    } else {
      signif_text <- " Attention, cette association n'est pas statistiquement significative."
    }
  }
  
  if (abs(or_val - 1) < 0.05) {
    return(paste0("L'Odds Ratio étant très proche de 1 (OR = ", formatC(or_val, format="f", digits=2), "), il n'y a pas d'association notable entre ces facteurs.", signif_text))
  }
  
  if (or_val > 1) {
    pct_increase <- round((or_val - 1) * 100)
    if (or_val > 2) {
      return(paste0("La probabilité est multipliée par ", formatC(or_val, format="f", digits=1), ".", signif_text))
    } else {
      return(paste0("La probabilité est augmentée de ", pct_increase, "%.", signif_text))
    }
  } else {
    pct_decrease <- round((1 - or_val) * 100)
    if (or_val < 0.5) {
      div_factor <- 1 / or_val
      return(paste0("La probabilité est divisée par ", formatC(div_factor, format="f", digits=1), " (effet protecteur).", signif_text))
    } else {
      return(paste0("La probabilité est diminuée de ", pct_decrease, "% (effet protecteur).", signif_text))
    }
  }
}

#' @describeIn interpret_stats Interprétation globale d'une comparaison de groupes (Chi2 / ANOVA)
#' @param var_x Nom de la variable explicative
#' @param var_y Nom de la variable expliquée
#' @param p_val p-value du test global
#' @return Une chaîne de caractères explicative.
#' @export
interpret_association <- function(var_x, var_y, p_val) {
    if (is.na(p_val) || !is.numeric(p_val)) return("")
    if (p_val < 0.05) {
        return(paste0("L'analyse révèle une association statistiquement significative entre '", var_x, "' et '", var_y, "' (p = ", formatC(p_val, format="f", digits=3), "). Cela indique que la distribution de '", var_y, "' varie de façon probante selon les différentes modalités de '", var_x, "'."))
    } else {
         return(paste0("L'analyse ne montre pas d'association statistiquement significative entre '", var_x, "' et '", var_y, "' (p = ", formatC(p_val, format="f", digits=3), "). Les variations observées pourraient être dues au hasard."))
    }
}
