#' @title Analyse descriptive pour variables binaires
#' @description Calcule la fréquence et le pourcentage d'un événement d'intérêt pour une variable binaire
#' @param data Le dataframe
#' @param var La variable binaire
#' @param target_level La modalité d'intérêt (ex: "Oui", 1, TRUE)
#' @param var_name Nom personnalisé pour la variable
#' @param subset Expression de filtrage
#' @param digits Nombre de décimales
#' @param include_na Inclure les NA dans le dénominateur ?
#' @param color Couleur de l'en-tête
#'
#' @return Un objet freq_table simplifié
#' @export
descr_binary <- function(data, var, target_level = NULL, var_name = NULL, subset = NULL,
                         digits = 1, include_na = FALSE, color = "#D3D3D3") {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")

  subset_enq <- rlang::enquo(subset)
  if (!rlang::quo_is_null(subset_enq)) {
    data <- dplyr::filter(data, !!subset_enq)
  }

  var_name_auto <- deparse(substitute(var))
  if (is.null(var_name)) {
    attr_label <- attr(data[[var_name_auto]], "label")
    var_name <- if (!is.null(attr_label)) attr_label else var_name_auto
  }

  x <- data[[var_name_auto]]

  if (!include_na) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) return(NULL)

  # Déterminer target_level si non fourni
  if (is.null(target_level)) {
    levs <- sort(unique(na.omit(x)), decreasing = TRUE)
    target_level <- levs[1]
  }

  n_target <- sum(x == target_level, na.rm = TRUE)
  n_total <- length(x)
  pct <- (n_target / n_total) * 100

  res_df <- tibble::tibble(
    Variable = var_name,
    Modalité = as.character(target_level),
    Effectif = n_target,
    `Total (N)` = n_total,
    `Pourcentage (%)` = format(round(pct, digits), nsmall = digits, decimal.mark = ",")
  )

  ft <- theme_analytique(res_df, color = color) %>%
    flextable::set_caption(paste("Proportion de :", var_name, "(", target_level, ")"))

  structure(
    list(
      data = res_df,
      flextable = ft,
      variable_name = var_name,
      target_level = target_level,
      n_total = n_total,
      raw_data = x
    ),
    class = "descr_binary"
  )
}
