#' @title Graphique divergent pour les échelles de Likert
#' @description Génère un graphique en barres divergentes centré sur le point
#' neutre d'une échelle de Likert. Idéal pour visualiser la distribution des
#' réponses de type "Accord / Désaccord".
#'
#' @param data data.frame.
#' @param cols Vecteur de noms de colonnes Likert (variables numériques 1 à n_levels).
#' @param n_levels Nombre de niveaux de l'échelle (défaut: 5).
#' @param var_labels Vecteur nommé de libellés pour chaque variable.
#' @param level_labels Vecteur de libellés pour chaque niveau (du + négatif au + positif).
#' @param title Titre du graphique.
#' @param neutral Niveau neutre de l'échelle (défaut: ceiling(n_levels / 2)).
#' @param palette Vecteur de couleurs (longueur = n_levels).
#'
#' @return Un objet `ggplot2`.
#'
#' @examples
#' df <- data.frame(
#'   q1 = sample(1:5, 40, replace = TRUE),
#'   q2 = sample(1:5, 40, replace = TRUE),
#'   q3 = sample(1:5, 40, replace = TRUE)
#' )
#' plot_likert_divergent(df, cols = c("q1", "q2", "q3"),
#'   var_labels = c(q1 = "Accessibilité", q2 = "Qualité", q3 = "Satisfaction"))
#'
#' @export
plot_likert_divergent <- function(data, cols, n_levels = 5,
                                  var_labels = NULL, level_labels = NULL,
                                  title = "Répartition des réponses (Likert)",
                                  neutral = ceiling(n_levels / 2),
                                  palette = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")

  if (is.null(level_labels)) {
    level_labels <- as.character(seq_len(n_levels))
  }

  if (is.null(palette)) {
    # Rouge -> gris -> vert
    neg_pal <- grDevices::colorRampPalette(c("#D32F2F", "#FFCDD2"))(floor(n_levels / 2))
    pos_pal <- grDevices::colorRampPalette(c("#C8E6C9", "#2E7D32"))(ceiling(n_levels / 2))
    if (n_levels %% 2 == 1) {
      palette <- c(neg_pal, "#E0E0E0", pos_pal)
    } else {
      palette <- c(neg_pal, pos_pal)
    }
  }

  # Calcul des proportions par variable et niveau
  rows <- lapply(cols, function(cn) {
    lab <- if (!is.null(var_labels) && cn %in% names(var_labels)) var_labels[[cn]] else cn
    vec <- as.numeric(data[[cn]])
    n   <- sum(!is.na(vec))
    sapply(seq_len(n_levels), function(k) {
      pct <- 100 * sum(vec == k, na.rm = TRUE) / n
      data.frame(variable = lab, niveau = level_labels[k],
                 niveau_num = k, pct = pct, stringsAsFactors = FALSE)
    }, simplify = FALSE)
  })

  df_long <- dplyr::bind_rows(unlist(rows, recursive = FALSE))
  df_long$niveau <- factor(df_long$niveau, levels = level_labels)

  # Centrage : les niveaux négatifs sont tracés à gauche (pct négatif)
  df_long$pct_centred <- ifelse(
    df_long$niveau_num < neutral, -df_long$pct,
    ifelse(df_long$niveau_num == neutral, df_long$pct / 2, df_long$pct)
  )

  df_long$variable <- factor(df_long$variable,
                              levels = rev(unique(df_long$variable)))

  p <- ggplot2::ggplot(df_long,
      ggplot2::aes(x = pct_centred, y = variable, fill = niveau)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = FALSE)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.8, color = "grey30") +
    ggplot2::scale_fill_manual(values = palette, name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(abs(x), "%")) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
  p
}

#' @title Carte visuelle des données manquantes
#' @description Génère une heatmap `ggplot2` représentant la présence (blanc)
#' ou l'absence (couleur) de données pour chaque variable et observation,
#' ainsi qu'un résumé des taux de manquants par variable.
#'
#' @param data data.frame.
#' @param vars Vecteur de noms de colonnes à inspecter. Si NULL, toutes les colonnes.
#' @param color_missing Couleur des cellules manquantes (défaut: "#D32F2F").
#' @param title Titre du graphique (défaut: "Carte des données manquantes").
#' @param max_obs Nombre maximum d'observations à afficher (défaut: 200).
#'
#' @return Un objet `ggplot2`.
#'
#' @examples
#' df <- data.frame(
#'   age  = c(25, NA, 30, NA, 45),
#'   sexe = c("H", "F", NA, "F", "H"),
#'   note = c(NA, 15, 12, NA, 18)
#' )
#' plot_missing_map(df)
#'
#' @export
plot_missing_map <- function(data, vars = NULL,
                              color_missing = "#D32F2F",
                              title = "Carte des données manquantes",
                              max_obs = 200) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")

  if (is.null(vars)) vars <- names(data)
  vars <- intersect(vars, names(data))

  df_sub <- data[seq_len(min(nrow(data), max_obs)), vars, drop = FALSE]
  df_sub$obs_id <- seq_len(nrow(df_sub))

  df_long <- tidyr::pivot_longer(df_sub, cols = -obs_id,
                                  names_to = "variable", values_to = "valeur")
  df_long$manquant <- is.na(df_long$valeur)

  # Taux de manquants pour ordonner les variables
  taux_na <- sapply(vars, function(v) mean(is.na(data[[v]])))
  var_order <- names(sort(taux_na, decreasing = TRUE))
  df_long$variable <- factor(df_long$variable, levels = var_order)

  p <- ggplot2::ggplot(df_long,
      ggplot2::aes(x = obs_id, y = variable, fill = manquant)) +
    ggplot2::geom_tile(linewidth = 0) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "#E8F5E9", "TRUE" = color_missing),
      labels = c("Présent", "Manquant"),
      name = NULL
    ) +
    ggplot2::labs(title = title,
                  subtitle = paste0("N = ", nrow(data), " obs. | ",
                    length(vars), " variables | ",
                    format(round(100 * mean(is.na(df_sub[, vars])), 1),
                           decimal.mark = ","), "% de manquants"),
                  x = "N° observation", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "top",
      panel.grid    = ggplot2::element_blank(),
      axis.text.x   = ggplot2::element_text(size = 7)
    )
  p
}

#' @title Heatmap de corrélations (ggplot2)
#' @description Génère une heatmap ggplot2 de la matrice de corrélations
#' (alternative visuelle à `corrplot`). Les valeurs sont affichées dans chaque
#' cellule avec une palette divergente.
#'
#' @param data data.frame.
#' @param cols Vecteur de noms de colonnes numériques. Si NULL, toutes les variables numériques.
#' @param method Méthode de corrélation: "pearson" (défaut) ou "spearman".
#' @param digits Nombre de décimales (défaut: 2).
#' @param title Titre du graphique.
#' @param low_color Couleur pour les corrélations négatives (défaut: "#D32F2F").
#' @param high_color Couleur pour les corrélations positives (défaut: "#1565C0").
#'
#' @return Un objet `ggplot2`.
#'
#' @examples
#' plot_correlation(mtcars, cols = c("mpg", "cyl", "hp", "wt", "qsec"))
#'
#' @export
plot_correlation <- function(data, cols = NULL,
                              method = c("pearson", "spearman"),
                              digits = 2,
                              title = "Matrice de corrélations",
                              low_color = "#D32F2F",
                              high_color = "#1565C0") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  method <- match.arg(method)

  if (is.null(cols)) {
    cols <- names(data)[sapply(data, is.numeric)]
  }
  mat_data <- data[, cols, drop = FALSE]
  mat_data <- mat_data[, sapply(mat_data, is.numeric), drop = FALSE]
  cols <- names(mat_data)

  cor_mat <- stats::cor(mat_data, use = "pairwise.complete.obs", method = method)

  # Passage en format long
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "correlation")

  cor_df$Var1 <- factor(cor_df$Var1, levels = cols)
  cor_df$Var2 <- factor(cor_df$Var2, levels = rev(cols))

  cor_df$label <- ifelse(
    cor_df$Var1 == cor_df$Var2, "",
    format(round(cor_df$correlation, digits), nsmall = digits, decimal.mark = ",")
  )

  p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.2, color = "white",
                       fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low = low_color, mid = "white", high = high_color,
      midpoint = 0, limits = c(-1, 1),
      name = paste0("r (", method, ")")
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = 12),
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid   = ggplot2::element_blank()
    )
  p
}
