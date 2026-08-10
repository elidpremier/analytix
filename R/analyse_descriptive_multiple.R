#' @title Analyse descriptive robuste pour variables numériques et catégorielles
#' @description Analyse automatiquement une sélection de variables, avec
#' une détection de type améliorée et des warnings intelligents.
#'
#' @param data data.frame
#' @param vars vecteur de noms de variables à analyser. Si NULL → toutes.
#' @param subset Expression de filtrage globale appliquée avant l'analyse.
#' @param var_labels nommage personnalisé : c("VAR" = "Libellé")
#' @param var_types typage manuel : c("VAR" = "numeric", "VAR2" = "categorical", "VAR3" = "binary")
#' @param exclude_vars variables à exclure
#' @param integer_as_category logique : TRUE = treat integers with few levels as categorical
#' @param ... arguments passés aux fonctions sous-jacentes (descr_numeric, descr_categorial, descr_binary)
#'
#' @return Une liste d'objets d'analyse
#' 
#' @examples
#' # Analyse globale
#' analyse_descriptive_multiple(iris)
#' 
#' # Avec filtrage (subset)
#' analyse_descriptive_multiple(iris, subset = Sepal.Length > 5)
#' 
#' # Forcer un type binaire
#' mtcars$cyl_bin <- ifelse(mtcars$cyl > 6, 1, 0)
#' analyse_descriptive_multiple(mtcars, vars = "cyl_bin", var_types = c("cyl_bin" = "binary"))
#'
#' @export
analyse_descriptive_multiple <- function(
    data,
    vars = NULL,
    subset = NULL,
    var_labels = NULL,
    var_types = NULL,
    exclude_vars = NULL,
    integer_as_category = TRUE,
    ...
) {

  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")
  
  # Filtrage global
  subset_enq <- rlang::enquo(subset)
  if (!rlang::quo_is_null(subset_enq)) {
    data <- dplyr::filter(data, !!subset_enq)
  }

  if (is.null(vars)) vars <- names(data)
  vars <- setdiff(vars, exclude_vars)

  results <- list()
  
  for (v in vars) {
    x <- data[[v]]
    label <- if (!is.null(var_labels[[v]])) var_labels[[v]] else {
      attr_l <- attr(x, "label")
      if (!is.null(attr_l)) attr_l else v
    }
    
    type <- if (!is.null(var_types[[v]])) var_types[[v]] else "auto"
    
    if (type == "auto") {
      if (is.numeric(x)) {
        u <- length(unique(na.omit(x)))
        if (u == 2) type <- "binary"
        else if (u <= 5 && integer_as_category) type <- "categorical"
        else type <- "numeric"
      } else {
        type <- "categorical"
      }
    }

    args <- list(data = data, var = as.name(v), var_name = label, ...)
    
    target_fn <- if (type == "binary") {
      descr_binary
    } else if (type == "numeric") {
      descr_numeric
    } else {
      descr_categorial
    }
    
    # Filtrer les arguments pour ne garder que ceux acceptés par la fonction cible
    valid_arg_names <- intersect(names(args), names(formals(target_fn)))
    clean_args <- args[valid_arg_names]
    
    results[[v]] <- tryCatch({
      do.call(target_fn, clean_args)
    }, error = function(e) {
      warning("Erreur lors de l'analyse de ", v, " : ", e$message)
      NULL
    })
  }
  
  return(results)
}
