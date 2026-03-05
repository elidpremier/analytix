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

  var_name_auto <- deparse(substitute(var))
  if (is.null(var_name)) var_name <- var_name_auto

  if (!var_name_auto %in% names(data)) {
    stop("La variable '", var_name_auto, "' n'existe pas.")
  }

  x <- data[[var_name_auto]]

  if (!is.numeric(x)) {
    stop("La variable doit être numérique.")
  }

  n_total <- length(x)
  n_valid <- sum(!is.na(x))
  n_missing <- n_total - n_valid

  if (n_valid == 0) stop("Aucune valeur valide dans la variable.")

  x_clean <- if (na_rm) x[!is.na(x)] else x

  # Statistiques de base (toujours présentes)
  stats_list <- list(
    "Effectif total" = n_total,
    "Valeurs manquantes" = n_missing,
    "Moyenne" = round(mean(x_clean, na.rm = TRUE), digits),
    "Écart-type" = round(stats::sd(x_clean, na.rm = TRUE), digits),
    "Médiane" = round(stats::median(x_clean, na.rm = TRUE), digits),
    "Minimum" = min(x_clean, na.rm = TRUE),
    "Maximum" = max(x_clean, na.rm = TRUE),
    "Premier quartile (Q1)" = round(stats::quantile(x_clean, 0.25, na.rm = TRUE), digits),
    "Troisième quartile (Q3)" = round(stats::quantile(x_clean, 0.75, na.rm = TRUE), digits)
  )

  # Ajouter "Valeurs valides" si demandé
  if (show_valid) {
    stats_list[["Valeurs valides"]] <- n_valid
  }

  # Ajouter skewness si demandé
  if (show_skewness) {
    # Calcul manuel du skewness (pas de dépendance à e1071)
    if (n_valid > 2) {
      m <- mean(x_clean, na.rm = TRUE)
      s <- stats::sd(x_clean, na.rm = TRUE)
      if (s > 0) {
        skew <- mean(((x_clean - m) / s)^3, na.rm = TRUE)
      } else {
        skew <- 0
      }
    } else {
      skew <- as.numeric(NA)
    }
    stats_list[["Asymétrie (skewness)"]] <- round(skew, digits)
  }

  # Créer le tibble
  stats <- tibble::tibble(
    Statistique = names(stats_list),
    Valeur = unlist(stats_list)
  )

  # Formatage : entiers vs décimaux
  stats <- stats %>%
    dplyr::mutate(
      Valeur = dplyr::if_else(
        Statistique %in% c("Effectif total", "Valeurs manquantes", "Valeurs valides"),
        as.character(Valeur),  # Pas de décimale pour les effectifs
        format(Valeur, nsmall = digits, decimal.mark = ",")
      )
    )

  # Titre
  if (is.null(caption)) {
    caption <- paste("Analyse descriptive de :", var_name)
  }

  # Flextable
  ft <- flextable::flextable(stats) %>%
    flextable::set_caption(caption) %>%
    theme_analytique(color = color)
  # Retour
  structure(
    list(
      data = stats,
      flextable = ft,
      variable_name = var_name,
      n_valid = n_valid,
      n_missing = n_missing,
      raw_data = x  # ←←← Ajout crucial pour la visualisation
    ),
    class = "descr_numeric"
  )
}
