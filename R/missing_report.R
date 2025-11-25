#' @title Rapport des valeurs manquantes
#' @description Tableau du taux de valeurs manquantes par variable.
#' @param data data.frame
#' @param vars vecteur de noms de variables (par défaut : toutes)
#' @param digits nombre de décimales
#' @param color couleur de l'en-tête
#' @return un objet de classe "missing_report"
#' @examples
#' missing_report(airquality)
#' @export
missing_report <- function(data, vars = base::names(data), digits = 1, color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")

  n_total <- base::nrow(data)
  miss_data <- tibble::tibble(
    Variable = vars,
    `Effectif total` = base::rep(n_total, base::length(vars)),
    Manquants = base::sapply(vars, function(v) base::sum(base::is.na(data[[v]]))),
    `Taux de manquants (%)` = base::sapply(vars, function(v) {
      pct <- base::mean(base::is.na(data[[v]])) * 100
      base::format(base::round(pct, digits), nsmall = digits, decimal.mark = ",")
    })
  )

  ft <- flextable::flextable(miss_data) %>%
    flextable::set_caption("Taux de valeurs manquantes par variable") %>%
    theme_analytique(color = color)

  structure(list(flextable = ft, data = miss_data), class = "missing_report")
}
