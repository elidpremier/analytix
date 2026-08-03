#' @title Description d'une variable âge
#' @description Génère un résumé statistique standardisé d'une variable âge :
#' effectif valide, moyenne, médiane, écart-type, min/max, et tableau de
#' tranches d'âge automatiques ou personnalisées.
#'
#' @param data data.frame.
#' @param var Variable âge numérique.
#' @param var_name Libellé pour le titre (défaut: "Âge").
#' @param breaks Vecteur de seuils pour les tranches d'âge.
#'   Si NULL, utilise des tranches automatiques de 10 ans.
#' @param labels Vecteur de libellés pour les tranches.
#' @param digits Nombre de décimales (défaut: 1).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Un objet `flextable` avec les statistiques et les tranches d'âge.
#'
#' @examples
#' df <- data.frame(age = c(23, 31, 45, 52, 18, 67, 29, 34, NA, 41))
#' descr_age(df, age, var_name = "Âge des participants")
#'
#' @export
descr_age <- function(data, var, var_name = NULL, breaks = NULL,
                      labels = NULL, digits = 1, color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")

  var_enq <- rlang::enquo(var)
  var_nm  <- rlang::as_name(var_enq)
  if (is.null(var_name)) var_name <- .get_label(data, var_nm, var_nm)
  vec       <- dplyr::pull(data, !!var_enq)
  vec_clean <- as.numeric(vec[!is.na(vec)])
  n_val     <- length(vec_clean)
  n_miss    <- sum(is.na(vec))

  moy   <- round(mean(vec_clean), digits)
  med   <- round(stats::median(vec_clean), digits)
  ec_t  <- round(stats::sd(vec_clean), digits)
  q1    <- round(stats::quantile(vec_clean, 0.25), digits)
  q3    <- round(stats::quantile(vec_clean, 0.75), digits)

  stats_df <- data.frame(
    Statistique = c("Effectif valide", "Valeurs manquantes",
                    "Moyenne", "Médiane", "Écart-type",
                    "Minimum", "Maximum", "Q1", "Q3"),
    Valeur = c(
      n_val, n_miss,
      format(moy, nsmall = digits, decimal.mark = ","),
      format(med, nsmall = digits, decimal.mark = ","),
      format(ec_t, nsmall = digits, decimal.mark = ","),
      format(round(min(vec_clean), digits), nsmall = digits, decimal.mark = ","),
      format(round(max(vec_clean), digits), nsmall = digits, decimal.mark = ","),
      format(q1, nsmall = digits, decimal.mark = ","),
      format(q3, nsmall = digits, decimal.mark = ",")
    ),
    stringsAsFactors = FALSE
  )

  # Tranches d'age
  if (is.null(breaks)) {
    mn <- floor(min(vec_clean) / 10) * 10
    mx <- ceiling(max(vec_clean) / 10) * 10
    breaks <- seq(mn, mx, by = 10)
  }

  if (is.null(labels)) {
    labels <- paste0(breaks[-length(breaks)], " - ", breaks[-1] - 1)
  }

  tranches <- cut(vec_clean, breaks = breaks, labels = labels,
                  include.lowest = TRUE, right = FALSE)
  tab_t <- table(tranches)
  pct_t <- round(100 * as.numeric(tab_t) / n_val, digits)

  tranches_df <- data.frame(
    Statistique = paste0("  ", names(tab_t)),
    Valeur = paste0(as.numeric(tab_t), " (", format(pct_t, nsmall = digits, decimal.mark = ","), "%)"),
    stringsAsFactors = FALSE
  )

  # Entete de séparation
  sep_row <- data.frame(Statistique = "Tranches d'âge – n (%)", Valeur = "",
                        stringsAsFactors = FALSE)

  final_df <- rbind(stats_df, sep_row, tranches_df)

  ft <- flextable::flextable(final_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(var_name) %>%
    flextable::bold(i = nrow(stats_df) + 1, part = "body")

  ft
}
