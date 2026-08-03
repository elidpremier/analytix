#' @title Recodage d'une variable Likert textuelle en numérique
#' @description Convertit une variable Likert textuelle en valeur numérique
#' selon un mapping personnalisé. Gère la casse et les espaces.
#'
#' @param x Vecteur de caractères à recoder.
#' @param mapping Vecteur nommé de correspondances texte -> numérique.
#'   Ex: c("pas du tout" = 1, "peu" = 2, "assez" = 4, "tout à fait" = 5).
#'
#' @return Un vecteur numérique.
#'
#' @examples
#' x <- c("Pas du tout", "Assez", "Tout à fait", NA)
#' map <- c("pas du tout" = 1, "peu" = 2, "moyennement" = 3,
#'          "assez" = 4, "tout à fait" = 5)
#' recode_likert(x, map)
#'
#' @export
recode_likert <- function(x, mapping) {
  x_trim <- stringr::str_trim(tolower(as.character(x)))
  result <- mapping[x_trim]
  as.numeric(result)
}

#' @title Description univariée d'une variable Likert
#' @description Génère un tableau de fréquences et un graphique ggplot2 pour
#' une variable Likert numérique (1 à N niveaux). Le tableau inclut effectifs,
#' pourcentages, et score moyen.
#'
#' @param data data.frame.
#' @param var Variable Likert (numérique ou facteur ordonné).
#' @param var_name Libellé de la variable pour le titre et le tableau.
#' @param levels_labels Vecteur nommé de libellés pour chaque niveau numérique.
#'   Ex: c("1" = "Pas du tout", "5" = "Tout à fait").
#' @param digits Nombre de décimales (défaut: 1).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#' @param plot Logique. Retourner aussi le graphique ? (défaut: FALSE).
#'
#' @return Un objet `flextable` ou une liste contenant `table` et `plot`.
#'
#' @examples
#' df <- data.frame(satisfaction = sample(1:5, 50, replace = TRUE))
#' descr_likert(df, satisfaction, var_name = "Satisfaction globale")
#'
#' @export
descr_likert <- function(data, var, var_name = NULL, levels_labels = NULL,
                         digits = 1, color = "#D3D3D3", plot = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")

  var_enq <- rlang::enquo(var)
  var_nm  <- rlang::as_name(var_enq)
  if (is.null(var_name)) {
    attr_l <- attr(data[[var_nm]], "label")
    var_name <- if (!is.null(attr_l)) attr_l else var_nm
  }

  vec <- data[[var_nm]]
  vec <- vec[!is.na(vec)]
  n_total <- length(vec)

  niveaux <- sort(unique(as.numeric(vec)))
  freqs <- sapply(niveaux, function(k) sum(vec == k))
  pcts  <- round(100 * freqs / n_total, digits)
  score_moy <- round(mean(as.numeric(vec), na.rm = TRUE), digits)

  if (!is.null(levels_labels)) {
    labels_niv <- sapply(as.character(niveaux), function(k) {
      if (k %in% names(levels_labels)) levels_labels[[k]] else k
    })
  } else {
    labels_niv <- as.character(niveaux)
  }

  res_df <- data.frame(
    Niveau = labels_niv,
    Effectif = freqs,
    Pourcentage = paste0(format(pcts, nsmall = digits, decimal.mark = ","), " %"),
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(res_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste0(var_name, " (Moy. = ",
      format(score_moy, nsmall = digits, decimal.mark = ","), ")")) %>%
    flextable::add_footer_lines(paste0("N = ", n_total,
      " | Score moyen = ",
      format(score_moy, nsmall = digits, decimal.mark = ",")))

  if (!plot) return(ft)

  df_plot <- data.frame(
    niveau = factor(labels_niv, levels = labels_niv),
    pct = pcts
  )
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = niveau, y = pct)) +
    ggplot2::geom_col(fill = "#2C6E9B", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(pct, "%")),
                       vjust = -0.4, size = 3.5, fontface = "bold") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = var_name,
                  subtitle = paste0("N = ", n_total, " | Moy. = ", score_moy),
                  x = NULL, y = "%") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.major.x = ggplot2::element_blank()
    )

  list(table = ft, plot = p)
}

#' @title Tableau récapitulatif de plusieurs variables Likert
#' @description Génère un tableau synthétique comparant plusieurs variables
#' Likert : effectifs valides, moyenne, médiane, écart-type et distribution
#' des pourcentages par niveau.
#'
#' @param data data.frame.
#' @param cols Vecteur de noms de colonnes Likert (numériques).
#' @param var_labels Vecteur nommé de libellés pour chaque colonne.
#' @param digits Nombre de décimales (défaut: 1).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Un objet `flextable`.
#'
#' @examples
#' df <- data.frame(
#'   q1 = sample(1:5, 30, replace = TRUE),
#'   q2 = sample(1:5, 30, replace = TRUE),
#'   q3 = sample(1:5, 30, replace = TRUE)
#' )
#' multi_likert_table(df, cols = c("q1", "q2", "q3"),
#'   var_labels = c(q1 = "Accessibilité", q2 = "Qualité", q3 = "Satisfaction"))
#'
#' @export
multi_likert_table <- function(data, cols, var_labels = NULL,
                               digits = 1, color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")

  rows <- lapply(cols, function(cn) {
    lab <- if (!is.null(var_labels) && cn %in% names(var_labels)) {
      var_labels[[cn]]
    } else {
      .get_label(data, cn, cn)
    }
    vec <- as.numeric(data[[cn]])
    vec_clean <- vec[!is.na(vec)]
    n_val  <- length(vec_clean)
    moy    <- round(mean(vec_clean), digits)
    med    <- round(stats::median(vec_clean), digits)
    ec_t   <- round(stats::sd(vec_clean), digits)
    min_v  <- min(vec_clean)
    max_v  <- max(vec_clean)
    data.frame(
      Variable   = lab,
      N          = n_val,
      Moyenne    = format(moy, nsmall = digits, decimal.mark = ","),
      Mediane    = format(med, nsmall = digits, decimal.mark = ","),
      Ecart_type = format(ec_t, nsmall = digits, decimal.mark = ","),
      Min_Max    = paste0(min_v, " - ", max_v),
      stringsAsFactors = FALSE
    )
  })

  res_df <- dplyr::bind_rows(rows)
  names(res_df)[names(res_df) == "Ecart_type"] <- "Écart-type"
  names(res_df)[names(res_df) == "Min_Max"]    <- "Min – Max"

  flextable::flextable(res_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption("Résumé des scores Likert")
}
