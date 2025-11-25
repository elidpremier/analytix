#' @title Analyse descriptive robuste pour variables numériques et catégorielles
#' @description Analyse automatiquement une sélection de variables, avec
#' une détection de type améliorée et des warnings intelligents.
#'
#' @param data data.frame
#' @param vars vecteur de noms de variables à analyser.
#'             Si NULL → toutes les variables.
#' @param var_labels nommage personnalisé : c("VAR" = "Libellé")
#' @param var_types typage manuel : c("VAR" = "numeric", "VAR2" = "categorical")
#' @param exclude_vars variables à exclure
#' @param integer_as_category logique : TRUE = treat integers with few levels as categorical
#' @param max_char_levels seuil max pour analyser une variable texte
#' @param ... arguments passés aux fonctions d’analyse
#' @examples
#'
#' Analyser seulement certaines variables
#'  analyse_descriptive_multiple(iris, c("Species", "Sepal.Length"))
#'
#'
#'  Analyser TOUTES les variables
#'  analyse_descriptive_multiple(iris)
#'
#'
#'  Analyser toutes sauf certaines
#'  analyse_descriptive_multiple(iris, exclude_vars = "Species")
#'
#'
#' @return Une liste contenant des objets freq_table ou descr_numeric
#'
#' @export
analyse_descriptive_multiple <- function(
    data,
    vars = NULL,
    var_labels = NULL,
    var_types = NULL,
    exclude_vars = NULL,
    integer_as_category = TRUE,
    max_char_levels = 30,
    ...
) {

  #---------------------------------------------------------------------------
  # 1. Sélection des variables
  #---------------------------------------------------------------------------

  if (is.null(vars)) vars <- names(data)

  if (!is.null(exclude_vars)) {
    unknown_excluded <- setdiff(exclude_vars, names(data))
    if (length(unknown_excluded) > 0) {
      warning("Variables dans exclude_vars non trouvées : ",
              paste(unknown_excluded, collapse=", "))
    }
    vars <- setdiff(vars, exclude_vars)
  }

  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables non trouvées dans les données : ",
         paste(missing_vars, collapse=", "))
  }

  if (length(vars) == 0) stop("Aucune variable à analyser.")

  #---------------------------------------------------------------------------
  # 2. Gestion des libellés
  #---------------------------------------------------------------------------

  if (is.null(var_labels)) {
    var_labels <- stats::setNames(vars, vars)
  } else {
    missing_labels <- setdiff(vars, names(var_labels))
    if (length(missing_labels) > 0) {
      warning("Libellés manquants pour : ",
              paste(missing_labels, collapse=", "),
              ". Libellés remplacés par le nom de la variable.")
      var_labels[missing_labels] <- missing_labels
    }
    var_labels <- var_labels[vars]
  }

  #---------------------------------------------------------------------------
  # 3. Typage robuste des variables
  #---------------------------------------------------------------------------

  detect_type <- function(x, name) {
    # Facteurs
    if (is.factor(x)) {
      if (is.ordered(x)) return("numeric_ordinal")
      return("categorical")
    }

    # Logiques
    if (is.logical(x)) return("categorical")

    # Dates
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      return("numeric_date")
    }

    # Numériques
    if (is.numeric(x)) {
      unique_vals <- length(unique(na.omit(x)))

      # entiers déguisés
      is_integer_like <- all(abs(x - round(x)) < 1e-9, na.rm=TRUE)

      # Numeric mais codes discrets
      if (is_integer_like && integer_as_category && unique_vals <= 10) {
        return("categorical")
      }

      # Trop peu de variabilité → freq table utile
      if (unique_vals <= 5) return("categorical")

      return("numeric")
    }

    # Caractères
    if (is.character(x)) {
      unique_vals <- length(unique(na.omit(x)))

      if (unique_vals > max_char_levels) {
        warning("La variable '", name,
                "' contient ", unique_vals,
                " modalités uniques (texte libre). Elle est ignorée.")
        return("ignore")
      }
      return("categorical")
    }

    # Type inconnu → ignorer proprement
    warning("Type non pris en charge pour la variable '", name,
            "'. Variable ignorée.")
    return("ignore")
  }

  # Gestion des types manuels vs automatiques
  if (is.null(var_types)) {
    var_types <- setNames(rep("auto", length(vars)), vars)
  } else {
    missing_types <- setdiff(vars, names(var_types))
    var_types[missing_types] <- "auto"
    var_types <- var_types[vars]
  }

  #---------------------------------------------------------------------------
  # 4. Boucle d'analyse
  #---------------------------------------------------------------------------

  results <- list()
  extra_args <- list(...)

  for (v in vars) {
    x <- data[[v]]
    label <- var_labels[[v]]
    declared_type <- var_types[[v]]

    # Détection automatique si "auto"
    type <- if (declared_type == "auto") detect_type(x, v) else declared_type

    # Décisions
    if (type == "ignore") next

    if (type %in% c("numeric", "numeric_ordinal", "numeric_date")) {
      analyse_type <- "numeric"
    } else {
      analyse_type <- "categorical"
    }

    # Construction des arguments
    args <- c(list(data = data,
                   var = as.name(v),
                   var_name = label),
              extra_args)

    # Appel aux bonnes fonctions
    if (analyse_type == "numeric") {
      results[[v]] <- do.call(descr_numeric, args)
    } else {
      results[[v]] <- do.call(freq_table, args)
    }
  }

  return(results)
}
