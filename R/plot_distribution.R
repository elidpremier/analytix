#' @title Visualisation automatique des distributions
#' @description Génère un graphique ggplot2 adapté au type de variable
#' @param data data.frame ou résultat d'analyse (ex: issu de \code{freq_table} ou \code{descr_numeric})
#' @param var variable à visualiser (optionnel si \code{data} est un objet d'analyse)
#' @param type type de graphique ("auto", "histogram", "density", "bar", "boxplot")
#' @param color couleur de contour (défaut: "#3366CC")
#' @param fill couleur de remplissage (défaut: "#3366CC")
#' @param alpha transparence (défaut: 0.7)
#' @param add_stats ajouter les statistiques sur le graphique ? (défaut: TRUE)
#' @param theme style du thème ("minimal", "classic", "light")
#'
#' @return un objet \code{ggplot}
#'
#' @examples
#' # Depuis un dataframe
#' plot_distribution(iris, Species)
#' plot_distribution(iris, Sepal.Length)
#'
#' # Depuis un objet freq_table ou descr_numeric
#' resultat <- freq_table(iris, Species)
#' plot_distribution(resultat)
#'
#' @export
plot_distribution <- function(data, var = NULL, type = "auto",
                              color = "#3366CC", fill = "#3366CC", alpha = 0.7,
                              add_stats = TRUE, theme = "minimal") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' requis pour les visualisations.", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' requis pour la manipulation de variables.", call. = FALSE)
  }

  # ---------- Fonctions internes ----------

  .apply_theme <- function(p, theme) {
    base_theme <- switch(theme,
                         "minimal" = ggplot2::theme_minimal(),
                         "classic" = ggplot2::theme_classic(),
                         "light"  = ggplot2::theme_light(),
                         ggplot2::theme_minimal())

    p + base_theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
        axis.title = ggplot2::element_text(face = "bold")
      )
  }

  .generate_palette <- function(n, base_color) {
    if (n == 1) return(base_color)
    if (!requireNamespace("colorspace", quietly = TRUE)) {
      # Fallback simple si colorspace absent
      return(rep(base_color, n))
    }
    colorspace::sequential_hcl(n, h = base_color)
  }

  .plot_from_vector <- function(x, var_name, type, color, fill, alpha, add_stats, theme) {
    if (type == "auto") {
      if (is.factor(x) || is.character(x)) {
        type <- "bar"
      } else if (is.numeric(x)) {
        unique_vals <- length(unique(stats::na.omit(x)))
        type <- if (unique_vals <= 10) "histogram" else "density"
      } else {
        stop("Type de variable non supporté pour la visualisation.")
      }
    }

    df <- data.frame(x = x)

    if (type == "histogram") {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(fill = fill, color = color, alpha = alpha, bins = 30) +
        ggplot2::labs(title = paste("Distribution de", var_name), x = var_name, y = "Fréquence")
    } else if (type == "density") {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_density(fill = fill, color = color, alpha = alpha) +
        ggplot2::labs(title = paste("Densité de", var_name), x = var_name, y = "Densité")
    } else if (type == "bar") {
      tab <- as.data.frame(table(x, useNA = "ifany"))
      colnames(tab) <- c("x", "n")
      tab$pourcentage <- 100 * tab$n / sum(tab$n, na.rm = TRUE)

      p <- ggplot2::ggplot(tab, ggplot2::aes(x = x, y = n)) +
        ggplot2::geom_col(fill = fill, color = color, alpha = alpha) +
        ggplot2::labs(title = paste("Distribution de", var_name), x = var_name, y = "Effectif") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      if (add_stats && nrow(tab) <= 15) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = paste0(n, "\n(", round(pourcentage, 1), "%)")),
          vjust = -0.5, size = 3
        )
      }
    } else if (type == "boxplot") {
      p <- ggplot2::ggplot(df, ggplot2::aes(y = x)) +
        ggplot2::geom_boxplot(fill = fill, color = color, alpha = alpha) +
        ggplot2::labs(title = paste("Boxplot de", var_name), x = "", y = var_name)
    } else {
      stop("Type de graphique non reconnu. Utilisez 'auto', 'histogram', 'density', 'bar' ou 'boxplot'.")
    }

    # Ajout de statistiques numériques si demandé (uniquement pour variables numériques)
    if (add_stats && is.numeric(x) && type %in% c("histogram", "density")) {
      moy <- round(mean(x, na.rm = TRUE), 2)
      med <- round(median(x, na.rm = TRUE), 2)
      sd_val <- round(stats::sd(x, na.rm = TRUE), 2)
      stats_text <- paste0("Moyenne: ", moy, " | Médiane: ", med, " | Écart-type: ", sd_val)
      p <- p + ggplot2::labs(subtitle = stats_text)
    }

    return(.apply_theme(p, theme))
  }

  .plot_categorical_from_freq <- function(freq_obj, color, fill, alpha, add_stats, theme) {
    data_plot <- freq_obj$data
    var_name <- freq_obj$variable_name

    if ("Total" %in% data_plot$variable) {
      data_plot <- data_plot[data_plot$variable != "Total", ]
    }

    # Calculer le pourcentage (si pas déjà présent)
    if (!"pourcentage" %in% names(data_plot)) {
      data_plot$pourcentage <- 100 * data_plot$n / sum(data_plot$n, na.rm = TRUE)
    }

    # Trier par fréquence décroissante
    data_plot$variable <- factor(data_plot$variable,
                                 levels = data_plot$variable[order(data_plot$n, decreasing = TRUE)])

    p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = variable, y = pourcentage)) +  # ← ICI : y = pourcentage
      ggplot2::geom_col(fill = fill, color = color, alpha = alpha) +
      ggplot2::labs(title = paste("Distribution de", var_name),
                    x = var_name, y = "Pourcentage (%)") +  # ← Mise à jour du label
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)), limits = c(0, NA))

    # Ajouter les étiquettes (effectif + pourcentage) seulement si demandé et peu de catégories
    if (add_stats && nrow(data_plot) <= 15) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = paste0(n, "\n(", round(pourcentage, 1), "%)")),
        vjust = -0.5, size = 3
      )
    }

    return(.apply_theme(p, theme))
  }

  .plot_numeric_from_descr <- function(descr_obj, type, color, fill, alpha, add_stats, theme) {
    if (is.null(descr_obj$raw_data)) {
      stop("L'objet 'descr_numeric' ne contient pas de données brutes (champ 'raw_data'). Mettez à jour votre fonction descr_numeric().")
    }
    x <- descr_obj$raw_data
    var_name <- descr_obj$variable_name
    return(.plot_from_vector(x, var_name, type, color, fill, alpha, add_stats, theme))
  }

  .plot_crosstab <- function(cross_obj, color, fill, alpha, theme) {
    tab_data <- as.data.frame(cross_obj$data)
    p <- ggplot2::ggplot(tab_data, ggplot2::aes(x = Var1, y = Freq, fill = Var2)) +
      ggplot2::geom_col(position = "dodge", alpha = alpha) +
      ggplot2::scale_fill_manual(values = .generate_palette(length(unique(tab_data$Var2)), fill)) +
      ggplot2::labs(title = "Tableau croisé",
                    x = names(tab_data)[1], y = "Effectif", fill = names(tab_data)[2]) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    return(.apply_theme(p, theme))
  }

  .plot_by_group <- function(obj, color, fill, alpha, theme) {
    warning("La visualisation pour 'descr_by_group' n'est pas encore implémentée.")
    return(ggplot2::ggplot())
  }

  .plot_from_analysis_object <- function(obj, type, color, fill, alpha, add_stats, theme) {
    if (inherits(obj, "freq_table")) {
      return(.plot_categorical_from_freq(obj, color, fill, alpha, add_stats, theme))
    } else if (inherits(obj, "descr_numeric")) {
      return(.plot_numeric_from_descr(obj, type, color, fill, alpha, add_stats, theme))
    } else if (inherits(obj, "freq_cross")) {
      return(.plot_crosstab(obj, color, fill, alpha, theme))
    } else if (inherits(obj, "descr_by_group")) {
      return(.plot_by_group(obj, color, fill, alpha, theme))
    } else {
      stop("Type d'objet non supporté pour la visualisation.")
    }
  }

  # ---------- Logique principale ----------

  if (inherits(data, c("freq_table", "descr_numeric", "freq_cross", "descr_by_group"))) {
    return(.plot_from_analysis_object(data, type, color, fill, alpha, add_stats, theme))
  }

  if (!is.null(var)) {

    sym <- rlang::ensym(var)          # capture du symbole
    var_name <- rlang::as_string(sym) # conversion propre en string

    if (!var_name %in% names(data)) {
      stop("Variable '", var_name, "' non trouvée dans le dataframe.", call. = FALSE)
    }

    x <- data[[var_name]]             # extraction robuste
    return(.plot_from_vector(x, var_name, type, color, fill, alpha, add_stats, theme))
  }


  stop("Fournir soit un objet d'analyse, soit un dataframe et une variable.", call. = FALSE)
}
#
