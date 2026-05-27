#' @title Imputation multiple par la méthode MICE
#' @description Wrapper simplifié autour de mice pour imputer les données manquantes rapidement.
#' @param data Le dataframe à imputer
#' @param m Nombre d'imputations (défaut: 5)
#' @param maxit Nombre d'itérations (défaut: 5)
#' @param seed Graine aléatoire pour la reproductibilité
#' @param ... Autres arguments passés à mice::mice
#'
#' @return Un dataframe imputé (par défaut, la moyenne des imputations ou une imputation simple complète)
#' @export
impute_mice <- function(data, m = 5, maxit = 5, seed = 123, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Le package 'mice' est requis pour cette fonction. Installez-le avec install.packages('mice')")
  }
  
  message("Lancement de l'imputation multiple (MICE)...")
  imp <- mice::mice(data, m = m, maxit = maxit, seed = seed, printFlag = FALSE, ...)
  
  # Retourner le premier dataset complet pour un usage simple
  # (On pourrait proposer une version qui retourne l'objet 'mids' complet)
  res <- mice::complete(imp, 1)
  
  message("✅ Imputation terminée. Retour du premier jeu de données complet.")
  return(res)
}
