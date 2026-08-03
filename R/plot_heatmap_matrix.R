#' @title Heatmap de prévalences ou pourcentages par contexte
#' @description Génère une carte thermique (`ggplot2`) optimisée pour représenter des pourcentages ou des taux
#' (ex: taux de résistance antibiotique par contexte d'isolement ou source).
#' 
#' @param data data.frame au format long (ou avec colonnes spécifiées).
#' @param x Variable pour l'axe X (catégorie/contexte/origine).
#' @param y Variable pour l'axe Y (antibiotique/variable).
#' @param fill Variable numérique pour la couleur de remplissage (pourcentage).
#' @param low_color Couleur pour les valeurs faibles (défaut: "#E8F5E9").
#' @param high_color Couleur pour les valeurs élevées (défaut: "#D32F2F").
#' @param title Titre du graphique.
#' @param x_lab Libellé de l'axe X.
#' @param y_lab Libellé de l'axe Y.
#' @param fill_lab Titre de la légende (défaut: "Taux (%)").
#' @param show_values Logique. Afficher les valeurs numériques dans chaque tuile ? (défaut: TRUE).
#' @param digits Nombre de décimales pour l'affichage dans les tuiles (défaut: 1).
#' 
#' @return Un objet `ggplot2`.
#' 
#' @examples
#' df <- data.frame(
#'   Source = rep(c("Environnement", "Portage", "Chaîne alim."), each = 3),
#'   Antibio = rep(c("Ampicilline", "Céfotaxime", "Ciprofloxacine"), 3),
#'   Taux = c(85.5, 42.0, 15.2, 90.0, 60.1, 20.0, 70.3, 35.0, 10.5)
#' )
#' plot_heatmap_matrix(df, x = Source, y = Antibio, fill = Taux)
#' 
#' @export
plot_heatmap_matrix <- function(data, x, y, fill,
                                low_color = "#E8F5E9",
                                high_color = "#D32F2F",
                                title = NULL,
                                x_lab = NULL,
                                y_lab = NULL,
                                fill_lab = "Taux (%)",
                                show_values = TRUE,
                                digits = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")

  x_enq <- rlang::enquo(x)
  y_enq <- rlang::enquo(y)
  fill_enq <- rlang::enquo(fill)

  x_nm <- rlang::as_name(x_enq)
  y_nm <- rlang::as_name(y_enq)
  fill_nm <- rlang::as_name(fill_enq)

  if (is.null(x_lab)) x_lab <- x_nm
  if (is.null(y_lab)) y_lab <- y_nm

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_enq, y = !!y_enq, fill = !!fill_enq)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient(low = low_color, high = high_color, name = fill_lab) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 11, angle = 0, hjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      panel.grid = ggplot2::element_blank()
    )

  if (show_values) {
    data_val <- data
    data_val$label_val <- paste0(format(round(data_val[[fill_nm]], digits), nsmall = digits, decimal.mark = ","), "%")
    p <- p + ggplot2::geom_text(
      data = data_val,
      ggplot2::aes(x = !!x_enq, y = !!y_enq, label = label_val),
      color = "black", size = 3.5
    )
  }

  p
}
