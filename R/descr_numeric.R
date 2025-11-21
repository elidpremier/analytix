#' @title Analyse descriptive pour variables numériques
#' @description Génère un tableau récapitulatif des statistiques descriptives d'une variable numérique
#' @param data data.frame
#' @param var variable numérique à analyser (sans guillemets)
#' @param var_name nom personnalisé (optionnel)
#' @param digits nombre de décimales (défaut: 2)
#' @param na_rm supprimer les NA dans les calculs ? (défaut: TRUE)
#' @param show_valid afficher la ligne "Valeurs valides" ? (défaut: FALSE)
#' @param show_skewness afficher l'asymétrie (skewness) ? (défaut: FALSE)
#' @param caption titre du tableau
#' @param color couleur de l'en-tête (défaut: "#D3D3D3")
#'
#' @return un objet de classe "descr_numeric" contenant les données et le flextable
#' @examples
#' descr_numeric(mtcars, mpg)
#'
#' @export
descr_numeric <- function(data, var, var_name = NULL, digits = 2,
                          na_rm = TRUE,
                          show_valid = FALSE,
                          show_skewness = FALSE,
                          caption = NULL,
                          color = "#D3D3D3") {

  # Vérifications
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' requis")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' requis")
  if (!requireNamespace("stats", quietly = TRUE)) stop("Package 'stats' requis")

  var_name_auto <- base::deparse(base::substitute(var))
  if (is.null(var_name)) var_name <- var_name_auto

  if (!var_name_auto %in% base::names(data)) {
    stop("La variable '", var_name_auto, "' n'existe pas.")
  }

  x <- data[[var_name_auto]]

  if (!base::is.numeric(x)) {
    stop("La variable doit être numérique.")
  }

  n_total <- base::length(x)
  n_valid <- base::sum(!base::is.na(x))
  n_missing <- n_total - n_valid

  if (n_valid == 0) stop("Aucune valeur valide dans la variable.")

  x_clean <- if (na_rm) x[!base::is.na(x)] else x

  # Statistiques de base (toujours présentes)
  stats_list <- base::list(
    "Effectif total" = n_total,
    "Valeurs manquantes" = n_missing,
    "Moyenne" = base::round(base::mean(x_clean, na.rm = TRUE), digits),
    "Écart-type" = base::round(stats::sd(x_clean, na.rm = TRUE), digits),
    "Médiane" = base::round(stats::median(x_clean, na.rm = TRUE), digits),
    "Minimum" = base::min(x_clean, na.rm = TRUE),
    "Maximum" = base::max(x_clean, na.rm = TRUE),
    "Premier quartile (Q1)" = base::round(stats::quantile(x_clean, 0.25, na.rm = TRUE), digits),
    "Troisième quartile (Q3)" = base::round(stats::quantile(x_clean, 0.75, na.rm = TRUE), digits)
  )

  # Ajouter "Valeurs valides" si demandé
  if (show_valid) {
    stats_list[["Valeurs valides"]] <- n_valid
  }

  # Ajouter skewness si demandé
  if (show_skewness) {
    # Calcul manuel du skewness (pas de dépendance à e1071)
    if (n_valid > 2) {
      m <- base::mean(x_clean, na.rm = TRUE)
      s <- stats::sd(x_clean, na.rm = TRUE)
      if (s > 0) {
        skew <- base::mean(((x_clean - m) / s)^3, na.rm = TRUE)
      } else {
        skew <- 0
      }
    } else {
      skew <- base::as.numeric(NA)
    }
    stats_list[["Asymétrie (skewness)"]] <- base::round(skew, digits)
  }

  # Créer le tibble
  stats <- tibble::tibble(
    Statistique = base::names(stats_list),
    Valeur = base::unlist(stats_list)
  )

  # Formatage : entiers vs décimaux
  stats <- stats %>%
    dplyr::mutate(
      Valeur = dplyr::if_else(
        Statistique %in% c("Effectif total", "Valeurs manquantes", "Valeurs valides"),
        base::as.character(Valeur),  # Pas de décimale pour les effectifs
        base::format(Valeur, nsmall = digits, decimal.mark = ",")
      )
    )

  # Titre
  if (is.null(caption)) {
    caption <- base::paste("Analyse descriptive de :", var_name)
  }

  # Flextable
  ft <- flextable::flextable(stats) %>%
    flextable::set_caption(caption) %>%
    flextable::set_table_properties(align = "left") %>%
    flextable::theme_booktabs() %>%
    flextable::bg(bg = color, part = "header") %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(j = "Statistique", align = "left") %>%
    flextable::align(j = "Valeur", align = "right") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::autofit()

  # Retour
  structure(
    list(
      data = stats,
      flextable = ft,
      variable_name = var_name,
      n_valid = n_valid,
      n_missing = n_missing
    ),
    class = "descr_numeric"
  )
}
