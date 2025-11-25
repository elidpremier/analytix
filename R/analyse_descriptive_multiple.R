#' @title Analyse descriptive de plusieurs variables (catégorielles et numériques)
#' @description Applique automatiquement \code{freq_table} ou \code{descr_numeric}
#' selon le type de chaque variable.
#' @param data data.frame
#' @param vars vecteur de noms de variables (chaînes de caractères).
#'             Si NULL, analyse toutes les variables du dataframe.
#' @param var_labels (optionnel) libellés : c("VAR" = "Libellé")
#' @param var_types (optionnel) c("VAR" = "categorical", "AUTRE" = "numeric")
#' @param exclude_vars variables à exclure de l'analyse
#' @param ... arguments passés aux fonctions d'analyse
#'
#' @return Une liste nommée d'objets \code{freq_table} ou \code{descr_numeric}
#'
#' @examples
#' # Analyser seulement certaines variables
#' analyse_descriptive_multiple(iris, c("Species", "Sepal.Length"))
#'
#' # Analyser TOUTES les variables
#' analyse_descriptive_multiple(iris)
#'
#' # Analyser toutes sauf certaines
#' analyse_descriptive_multiple(iris, exclude_vars = "Species")
#'
#' @export
analyse_descriptive_multiple <- function(data, vars = NULL, var_labels = NULL,
                                         var_types = NULL, exclude_vars = NULL, ...) {

  # Si vars n'est pas spécifié, prendre toutes les variables
  if (is.null(vars)) {
    vars <- base::names(data)
  }

  # Exclure les variables spécifiées
  if (!is.null(exclude_vars)) {
    vars <- base::setdiff(vars, exclude_vars)
  }

  # Vérification des variables manquantes
  missing_vars <- base::setdiff(vars, base::names(data))
  if (base::length(missing_vars) > 0) {
    base::stop("Variables non trouvées : ", base::paste(missing_vars, collapse = ", "))
  }

  # Vérifier qu'il reste des variables à analyser
  if (base::length(vars) == 0) {
    base::stop("Aucune variable à analyser.")
  }

  # Libellés
  if (is.null(var_labels)) {
    var_labels <- stats::setNames(vars, vars)
  } else {
    missing_labels <- base::setdiff(vars, base::names(var_labels))
    if (base::length(missing_labels) > 0) {
      base::warning("Libellés manquants pour : ", base::paste(missing_labels, collapse = ", "))
      var_labels[missing_labels] <- missing_labels
    }
    var_labels <- var_labels[vars]
  }

  # Types : "auto", "categorical", ou "numeric"
  if (is.null(var_types)) {
    var_types <- stats::setNames(base::rep("auto", base::length(vars)), vars)
  } else {
    missing_types <- base::setdiff(vars, base::names(var_types))
    if (base::length(missing_types) > 0) {
      var_types[missing_types] <- "auto"
    }
    var_types <- var_types[vars]
  }

  extra_args <- list(...)
  results <- base::list()

  for (i in base::seq_along(vars)) {
    var_name <- vars[i]
    label <- var_labels[var_name]
    type <- var_types[var_name]
    x <- data[[var_name]]

    # Déterminer le type d'analyse
    if (type == "auto") {
      # Logique améliorée pour la détection automatique
      if (base::is.numeric(x)) {
        # Pour les numériques, vérifier si c'est vraiment continu ou catégoriel déguisé
        unique_vals <- base::length(base::unique(stats::na.omit(x)))
        if (unique_vals <= 10 && base::all(x %% 1 == 0, na.rm = TRUE)) {
          analyse_type <- "categorical"  # Ex: variable 0/1, ou échelle 1-5
        } else {
          analyse_type <- "numeric"
        }
      } else {
        analyse_type <- "categorical"
      }
    } else {
      analyse_type <- type
    }

    # Appliquer la bonne fonction
    if (analyse_type == "numeric") {
      args <- base::c(base::list(data = data, var = base::as.name(var_name), var_name = label), extra_args)
      results[[var_name]] <- base::do.call(descr_numeric, args)
    } else {  # "categorical"
      args <- base::c(base::list(data = data, var = base::as.name(var_name), var_name = label), extra_args)
      results[[var_name]] <- base::do.call(freq_table, args)
    }
  }

  return(results)
}
