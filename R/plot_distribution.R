#' @title Visualisation automatique des distributions
#' @description Génère un graphique ggplot2 adapté au type de variable.
#' @param data data.frame ou résultat d'analyse (ex: issu de \code{descr_categorial})
#' @param var variable à visualiser (si data est un dataframe)
#' @param type type de graphique ("auto", "histogram", "density", "bar", "boxplot")
#' @param fill couleur de remplissage (défaut: "#3366CC")
#' @param theme style du thème ("minimal", "classic", "light")
#'
#' @return un objet \code{ggplot}
#' @export
plot_distribution <- function(data, var = NULL, type = "auto",
                              fill = "#3366CC", theme = "minimal") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis")

  # Extraction des données brutes selon le type d'objet
  x <- NULL
  var_name <- ""

  if (inherits(data, c("freq_table", "descr_numeric", "descr_binary"))) {
    x <- data$raw_data
    var_name <- data$variable_name
  } else if (is.data.frame(data)) {
    var_enq <- rlang::enquo(var)
    var_name <- rlang::quo_name(var_enq)
    x <- data[[var_name]]
  } else {
    stop("Data doit être un dataframe ou un résultat d'analyse analytix.")
  }

  if (is.null(x)) stop("Données introuvables.")

  # Détection automatique du type de graphique
  if (type == "auto") {
    if (is.numeric(x)) {
      unique_vals <- length(unique(stats::na.omit(x)))
      type <- if (unique_vals <= 15) "bar" else "histogram"
    } else {
      type <- "bar"
    }
  }

  p <- ggplot2::ggplot(data.frame(val = x), ggplot2::aes(x = val))

  if (type == "histogram") {
    p <- p + ggplot2::geom_histogram(fill = fill, color = "white", bins = 30, alpha = 0.8) +
      ggplot2::labs(y = "Effectif")
  } else if (type == "density") {
    p <- p + ggplot2::geom_density(fill = fill, alpha = 0.5) +
      ggplot2::labs(y = "Densité")
  } else if (type == "bar") {
    p <- ggplot2::ggplot(as.data.frame(table(val = x, useNA = "ifany")), ggplot2::aes(x = val, y = Freq)) +
      ggplot2::geom_col(fill = fill, alpha = 0.8) +
      ggplot2::labs(y = "Effectif") +
      ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = -0.5, size = 3)
  } else if (type == "boxplot") {
    p <- ggplot2::ggplot(data.frame(val = x), ggplot2::aes(y = val)) +
      ggplot2::geom_boxplot(fill = fill, alpha = 0.8) +
      ggplot2::labs(x = "")
  }

  # Thème et cosmétique
  thm <- switch(theme,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "light" = ggplot2::theme_light(),
                ggplot2::theme_minimal())

  p <- p + thm +
    ggplot2::labs(title = paste("Distribution de :", var_name), x = var_name) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  return(p)
}
