#' @title Rapport des valeurs manquantes
#' @description Tableau détaillé du taux de valeurs manquantes par variable.
#' @param data data.frame
#' @param vars vecteur de noms de variables (par défaut : toutes)
#' @param digits nombre de décimales
#' @param color couleur de l'en-tête
#' @return un objet de classe "missing_report"
#' @export
missing_report <- function(data, vars = base::names(data), digits = 1, color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble requis")

  n_total <- base::nrow(data)
  
  res_list <- lapply(vars, function(v) {
    # Récupérer le label
    attr_l <- attr(data[[v]], "label")
    label_v <- if (!is.null(attr_l)) attr_l else v
    
    n_miss <- sum(is.na(data[[v]]))
    pct_miss <- (n_miss / n_total) * 100
    
    tibble::tibble(
      Variable = label_v,
      `Effectif total` = n_total,
      Manquants = n_miss,
      `Taux (%)` = pct_miss
    )
  })

  miss_data <- dplyr::bind_rows(res_list) %>%
    dplyr::mutate(
      `Taux (%)` = base::format(base::round(`Taux (%)`, digits), nsmall = digits, decimal.mark = ",")
    )

  ft <- flextable::flextable(miss_data) %>%
    flextable::set_caption("Rapport des valeurs manquantes par variable") %>%
    theme_analytique(color = color) %>%
    flextable::color(i = ~ Manquants > 0, j = "Manquants", color = "red")

  structure(list(flextable = ft, data = miss_data), class = "missing_report")
}
