#' @title Fonctions graphiques pour l'analyse descriptive
#' @description Ensemble de fonctions graphiques `ggplot2` pretes a l'emploi pour generer
#' des diagrammes en barres, camemberts, diagrammes empiles 100%, barres groupees et boxplots.
#'
#' @param data data.frame ou vecteur/tableau d'effectifs.
#' @param x Variable principale / axe X (ou formule pour boxplot).
#' @param y Variable de mesure / axe Y.
#' @param fill Variable de remplissage / groupement.
#' @param title Titre du graphique.
#' @param subtitle Sous-titre du graphique (defaut: affichage automatique du N).
#' @param horiz Logique. Si TRUE, oriente le graphique horizontalement (defaut: FALSE).
#' @param col Couleur principale des barres (defaut: "#2C6E9B").
#' @param palette Palette de couleurs pour les graphiques multi-groupes.
#' @param file Chemin du fichier image pour sauvegarde optionnelle (ex: "barplot.png").
#' @param width Largeur de l'image sauvegardee en pouces (defaut: 8).
#' @param height Hauteur de l'image sauvegardee en pouces (defaut: 6).
#' @param show_labels Logique. Afficher les etiquettes de pourcentage sur les barres (defaut: TRUE).
#' @param digits Nombre de decimales pour les etiquettes (defaut: 1).
#' @param legend_title Titre de la legende pour les graphiques multi-groupes.
#' @param xlab Libelle de l'axe X (defaut: nom de la variable).
#' @param ylab Libelle de l'axe Y (defaut: "Effectif" ou "Pourcentage (%)").
#' @param show_pct Logique. Afficher les pourcentages au lieu des effectifs dans les barres groupees (defaut: FALSE).
#'
#' @return Un objet `ggplot2`.
#' 
#' @examples
#' # Diagramme en barres
#' plot_barplot(iris, Species, title = "Distribution des espèces")
#' 
#' # Camembert
#' plot_pie_chart(table(iris$Species), title = "Répartition des espèces")
#' 
#' # Barres 100% empilées
#' plot_stacked_bar_100(mtcars, x = cyl, fill = am, title = "Transmission par cylindres")
#' 
#' # Boxplot par groupe
#' plot_boxplot(mtcars, x = cyl, y = mpg, title = "Consommation par nombre de cylindres")
#' 
#' @name plot_utils
NULL

#' @rdname plot_utils
#' @export
plot_barplot <- function(data, x = NULL, title = NULL, subtitle = NULL,
                         horiz = FALSE, col = "#2C6E9B", show_labels = TRUE,
                         digits = 1, file = NULL, width = 8, height = 6) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  
  if (is.vector(data) || is.table(data)) {
    t_val <- if (is.table(data)) data else table(data, useNA = "no")
    df_plot <- data.frame(
      modalite = names(t_val),
      effectif = as.numeric(t_val),
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(data)) {
    x_enq <- rlang::enquo(x)
    vec <- dplyr::pull(data, !!x_enq)
    t_val <- table(vec, useNA = "no")
    df_plot <- data.frame(
      modalite = names(t_val),
      effectif = as.numeric(t_val),
      stringsAsFactors = FALSE
    )
  } else {
    stop("`data` doit être un data.frame, un vecteur ou une table.")
  }
  
  n_total <- sum(df_plot$effectif)
  if (is.null(subtitle)) subtitle <- paste0("N = ", n_total)
  
  df_plot$pct <- round(100 * df_plot$effectif / n_total, digits)
  df_plot$etiquette <- paste0(format(df_plot$pct, nsmall = digits, decimal.mark = ","), "%")
  
  if (horiz) {
    df_plot <- df_plot[order(df_plot$effectif), ]
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = effectif, y = stats::reorder(modalite, effectif))) +
      ggplot2::geom_col(fill = col, width = 0.7) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(title = title, subtitle = subtitle, x = "Effectif", y = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle = ggplot2::element_text(size = 10, color = "grey40", hjust = 0),
        panel.grid.major.y = ggplot2::element_blank()
      )
    if (show_labels) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = etiquette),
        hjust = -0.2, size = 3.5, fontface = "bold"
      )
    }
  } else {
    df_plot <- df_plot[order(df_plot$effectif, decreasing = TRUE), ]
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = stats::reorder(modalite, -effectif), y = effectif)) +
      ggplot2::geom_col(fill = col, width = 0.7) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = "Effectif") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle = ggplot2::element_text(size = 10, color = "grey40", hjust = 0),
        panel.grid.major.x = ggplot2::element_blank()
      )
    if (show_labels) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = etiquette),
        vjust = -0.5, size = 3.5, fontface = "bold"
      )
    }
  }
  
  if (!is.null(file)) {
    ggplot2::ggsave(file, plot = p, width = width, height = height, dpi = 150)
  }
  
  p
}

#' @rdname plot_utils
#' @export
plot_pie_chart <- function(data, x = NULL, title = NULL, palette = NULL,
                           legend_title = "Modalités", digits = 1,
                           file = NULL, width = 8, height = 6) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  
  if (is.vector(data) || is.table(data)) {
    t_val <- if (is.table(data)) data else table(data, useNA = "no")
    df_plot <- data.frame(modalite = names(t_val), effectif = as.numeric(t_val), stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    x_enq <- rlang::enquo(x)
    vec <- dplyr::pull(data, !!x_enq)
    t_val <- table(vec, useNA = "no")
    df_plot <- data.frame(modalite = names(t_val), effectif = as.numeric(t_val), stringsAsFactors = FALSE)
  } else {
    stop("`data` doit être un data.frame, un vecteur ou une table.")
  }
  
  n_total <- sum(df_plot$effectif)
  df_plot$pct <- round(100 * df_plot$effectif / n_total, digits)
  
  # Masquer les étiquettes < 5% pour éviter les chevauchements
  df_plot$label <- ifelse(df_plot$pct >= 5, paste0(format(df_plot$pct, nsmall = digits, decimal.mark = ","), "%"), "")
  df_plot <- df_plot[order(df_plot$effectif, decreasing = TRUE), ]
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = "", y = effectif, fill = modalite)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3.8, color = "white", fontface = "bold"
    ) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::labs(title = title, fill = legend_title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
      legend.position = "right"
    )
  
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  } else {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
  }
  
  if (!is.null(file)) {
    ggplot2::ggsave(file, plot = p, width = width, height = height, dpi = 150)
  }
  
  p
}

#' @rdname plot_utils
#' @export
plot_stacked_bar_100 <- function(data, x, fill, title = NULL, xlab = NULL,
                                 ylab = "Pourcentage (%)", legend_title = NULL,
                                 palette = NULL, file = NULL, width = 8, height = 6) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  if (!requireNamespace("scales", quietly = TRUE)) stop("scales requis")
  
  x_enq <- rlang::enquo(x)
  fill_enq <- rlang::enquo(fill)
  
  x_nm <- rlang::as_name(x_enq)
  fill_nm <- rlang::as_name(fill_enq)
  
  if (is.null(xlab)) xlab <- x_nm
  if (is.null(legend_title)) legend_title <- fill_nm
  
  df_plot <- data %>%
    dplyr::filter(!is.na(!!x_enq) & !is.na(!!fill_enq)) %>%
    dplyr::group_by(!!x_enq, !!fill_enq) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(!!x_enq), y = n, fill = factor(!!fill_enq))) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(title = title, x = xlab, y = ylab, fill = legend_title) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      legend.position = "top"
    )
  
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  } else {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set1")
  }
  
  if (!is.null(file)) {
    ggplot2::ggsave(file, plot = p, width = width, height = height, dpi = 150)
  }
  
  p
}

#' @rdname plot_utils
#' @export
plot_grouped_bar <- function(data, x, fill, title = NULL, xlab = NULL,
                             ylab = "Effectif", legend_title = NULL,
                             palette = NULL, show_pct = FALSE,
                             file = NULL, width = 9, height = 6) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  
  x_enq <- rlang::enquo(x)
  fill_enq <- rlang::enquo(fill)
  
  x_nm <- rlang::as_name(x_enq)
  fill_nm <- rlang::as_name(fill_enq)
  
  if (is.null(xlab)) xlab <- x_nm
  if (is.null(legend_title)) legend_title <- fill_nm
  
  df_plot <- data %>%
    dplyr::filter(!is.na(!!x_enq) & !is.na(!!fill_enq)) %>%
    dplyr::group_by(!!x_enq, !!fill_enq) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  if (show_pct) {
    df_plot <- df_plot %>%
      dplyr::group_by(!!x_enq) %>%
      dplyr::mutate(total_grp = sum(n), pct = round(100 * n / total_grp, 1), label_val = paste0(pct, "%"))
  } else {
    df_plot$label_val <- as.character(df_plot$n)
  }
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(!!x_enq), y = n, fill = factor(!!fill_enq))) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.9), width = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_val),
      position = ggplot2::position_dodge(0.9),
      vjust = -0.5, size = 3.3, fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = title, x = xlab, y = ylab, fill = legend_title) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      legend.position = "top"
    )
  
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  } else {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
  }
  
  if (!is.null(file)) {
    ggplot2::ggsave(file, plot = p, width = width, height = height, dpi = 150)
  }
  
  p
}

#' @rdname plot_utils
#' @export
plot_boxplot <- function(data, x, y, title = NULL, xlab = NULL, ylab = NULL,
                         palette = NULL, file = NULL, width = 8, height = 6) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  
  x_enq <- rlang::enquo(x)
  y_enq <- rlang::enquo(y)
  
  x_nm <- rlang::as_name(x_enq)
  y_nm <- rlang::as_name(y_enq)
  
  if (is.null(xlab)) xlab <- x_nm
  if (is.null(ylab)) ylab <- y_nm
  
  df_plot <- data %>%
    dplyr::filter(!is.na(!!x_enq) & !is.na(!!y_enq))
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(!!x_enq), y = !!y_enq, fill = factor(!!x_enq))) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.colour = "red") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      legend.position = "none"
    )
  
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  } else {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set2")
  }
  
  if (!is.null(file)) {
    ggplot2::ggsave(file, plot = p, width = width, height = height, dpi = 150)
  }
  
  p
}
