#' @title Calcul des prévalences et proportions avec Intervalles de Confiance à 95%
#' @description Calcule l'effectif, le total, la proportion/prévalence et l'intervalle de confiance à 95%
#' (Wilson, Exact ou Asymptotique), et retourne un résultat propre prêt pour la publication.
#' 
#' @param data data.frame ou vecteur binaire/catégoriel.
#' @param var Variable à analyser (si `data` est un data.frame).
#' @param cases_val Valeur considérée comme le cas positif (ex: "Oui", 1, "BLSE", "+"). Si NULL, la première modalité ou la valeur positive courante est détectée.
#' @param conf_level Niveau de confiance (défaut: 0.95).
#' @param method Méthode de calcul de l'IC: "wilson" (défaut), "exact" (Clopper-Pearson), ou "asymptotic".
#' @param digits Nombre de décimales pour l'affichage (défaut: 1).
#' 
#' @return Une liste ou un data.frame contenant les statistiques et la chaîne formatée.
#' 
#' @examples
#' df <- data.frame(blse = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1))
#' calc_prevalence(df, blse, cases_val = 1)
#' 
#' @export
calc_prevalence <- function(data, var = NULL, cases_val = NULL,
                            conf_level = 0.95,
                            method = c("wilson", "exact", "asymptotic"),
                            digits = 1) {
  method <- match.arg(method)
  
  if (is.data.frame(data)) {
    if (missing(var)) stop("Veuillez specifier la variable a analyser.")
    var_enq  <- rlang::enquo(var)
    var_nm   <- rlang::as_name(var_enq)
    var_name <- .get_label(data, var_nm, var_nm)
    vec      <- dplyr::pull(data, !!var_enq)
  } else {
    var_name <- "Variable"
    vec <- data
  }
  
  vec_clean <- vec[!is.na(vec)]
  total <- length(vec_clean)
  
  if (total == 0) {
    stop("Aucune donnée valide (non-NA) à analyser.")
  }
  
  if (is.null(cases_val)) {
    if (is.logical(vec_clean)) {
      cases_val <- TRUE
    } else if (is.numeric(vec_clean)) {
      cases_val <- 1
    } else {
      possibles <- c("Oui", "oui", "1", "+", "BLSE", "Positive", "Vrai", "TRUE")
      match_val <- intersect(unique(vec_clean), possibles)
      cases_val <- if (length(match_val) > 0) match_val[1] else unique(vec_clean)[1]
    }
  }
  
  cases <- sum(vec_clean == cases_val)
  p <- cases / total
  
  # Calcul IC
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  
  if (method == "wilson") {
    num <- p + (z^2) / (2 * total)
    denom <- 1 + (z^2) / total
    pm <- num / denom
    margin <- (z / denom) * sqrt((p * (1 - p) / total) + ((z^2) / (4 * total^2)))
    lower <- max(0, pm - margin)
    upper <- min(1, pm + margin)
  } else if (method == "exact") {
    lower <- if (cases == 0) 0 else stats::qbeta(alpha / 2, cases, total - cases + 1)
    upper <- if (cases == total) 1 else stats::qbeta(1 - alpha / 2, cases + 1, total - cases)
  } else { # asymptotic
    se <- sqrt((p * (1 - p)) / total)
    lower <- max(0, p - z * se)
    upper <- min(1, p + z * se)
  }
  
  pct <- p * 100
  pct_lower <- lower * 100
  pct_upper <- upper * 100
  
  str_pct <- format(round(pct, digits), nsmall = digits, decimal.mark = ",")
  str_low <- format(round(pct_lower, digits), nsmall = digits, decimal.mark = ",")
  str_upp <- format(round(pct_upper, digits), nsmall = digits, decimal.mark = ",")
  
  formatted <- paste0(cases, "/", total, " (", str_pct, "% [IC", round(conf_level * 100), "%: ", str_low, " - ", str_upp, "])")
  
  res <- data.frame(
    Variable    = var_name,
    Cas         = cases,
    Total       = total,
    Proportion  = p,
    Pourcentage = pct,
    IC_Inf      = pct_lower,
    IC_Sup      = pct_upper,
    Formate     = formatted,
    stringsAsFactors = FALSE
  )
  
  res
}
