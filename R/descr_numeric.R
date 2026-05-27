#' @title Analyse descriptive pour variables numériques
#' @description Génère un tableau récapitulatif des statistiques descriptives d'une variable numérique
#' @param data data.frame
#' @param var variable numérique à analyser (sans guillemets)
#' @param var_name nom personnalisé (optionnel)
#' @param subset Expression logique pour filtrer les données (ex: sexe == "M")
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
#' descr_numeric(mtcars, mpg, subset = cyl == 4)
#'
#' @export
descr_numeric <- function(data, var, var_name = NULL, subset = NULL, digits = 2,
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
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' requis")

  # Gestion du filtrage
  subset_enq <- rlang::enquo(subset)
  if (!rlang::quo_is_null(subset_enq)) {
    data <- dplyr::filter(data, !!subset_enq)
  }

  var_enq <- rlang::enquo(var)
  var_name_auto <- rlang::as_name(var_enq)
  
  # Récupération du label si var_name est NULL
  if (is.null(var_name)) {
    attr_label <- attr(data[[var_name_auto]], "label")
    if (!is.null(attr_label)) {
      var_name <- attr_label
    } else {
      var_name <- var_name_auto
    }
  }

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

  if (n_valid == 0) {
    warning("Aucune valeur valide dans la variable après filtrage.")
    return(NULL)
  }

  x_clean <- if (na_rm) x[!is.na(x)] else x

  # Statistiques de base
  stats_list <- list(
    "Effectif total" = n_total,
    "Valeurs manquantes" = n_missing,
    "Moyenne" = mean(x_clean, na.rm = TRUE),
    "Écart-type" = stats::sd(x_clean, na.rm = TRUE),
    "Médiane" = stats::median(x_clean, na.rm = TRUE),
    "Minimum" = min(x_clean, na.rm = TRUE),
    "Maximum" = max(x_clean, na.rm = TRUE),
    "Premier quartile (Q1)" = stats::quantile(x_clean, 0.25, na.rm = TRUE),
    "Troisième quartile (Q3)" = stats::quantile(x_clean, 0.75, na.rm = TRUE)
  )

  if (show_valid) {
    stats_list[["Valeurs valides"]] <- n_valid
  }

  if (show_skewness) {
    if (n_valid > 2) {
      m <- mean(x_clean, na.rm = TRUE)
      s <- stats::sd(x_clean, na.rm = TRUE)
      skew <- if (s > 0) mean(((x_clean - m) / s)^3, na.rm = TRUE) else 0
    } else {
      skew <- as.numeric(NA)
    }
    stats_list[["Asymétrie (skewness)"]] <- skew
  }

  # Créer le tibble
  stats <- tibble::tibble(
    Statistique = names(stats_list),
    Valeur_num = unname(unlist(stats_list))
  )

  # Formatage
  format_val <- function(val, stat_name) {
    if (stat_name %in% c("Effectif total", "Valeurs manquantes", "Valeurs valides")) {
      return(as.character(as.integer(val)))
    } else {
      return(format(round(val, digits), nsmall = digits, decimal.mark = ","))
    }
  }

  stats$Valeur <- mapply(format_val, stats$Valeur_num, stats$Statistique)

  # Titre
  if (is.null(caption)) {
    caption <- paste("Analyse descriptive de :", var_name)
  }

  # Flextable
  ft_data <- stats %>% dplyr::select(Statistique, Valeur)
  ft <- flextable::flextable(ft_data) %>%
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
      raw_data = x
    ),
    class = "descr_numeric"
  )
}
