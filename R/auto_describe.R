#' @title Analyse descriptive automatique de toutes les variables
#' @description Détecte automatiquement le type de chaque variable (binaire, catégorielle,
#' numérique continue) et applique la fonction descriptive appropriée. Retourne une liste
#' structurée de résultats prête à être exportée vers Word.
#'
#' @param data data.frame à analyser.
#' @param vars Vecteur de noms de colonnes à analyser. Si NULL (défaut), toutes les
#'   colonnes sont analysées.
#' @param digits Nombre de décimales pour les statistiques (défaut: 1).
#' @param color Couleur d'en-tête des flextables (défaut: "#D3D3D3").
#' @param binary_threshold Nombre maximum de valeurs uniques pour qu'une variable
#'   soit considérée binaire (défaut: 2).
#' @param cat_threshold Nombre maximum de valeurs uniques pour qu'une variable
#'   soit considérée catégorielle (défaut: 10).
#' @param verbose Afficher les messages de progression (défaut: FALSE).
#'
#' @return Une liste nommée (par variable ou label) contenant :
#'   \describe{
#'     \item{chaque élément}{Résultat de la fonction descriptive correspondante (liste avec `$flextable`)}
#'     \item{attr "var_types"}{data.frame avec colonnes `variable`, `label`, `type_detected`}
#'   }
#'
#' @examples
#' # Analyser toutes les variables d'iris
#' res <- auto_describe(iris)
#' names(res)  # variables détectées
#' attr(res, "var_types")  # tableau des types détectés
#'
#' # Analyser seulement certaines variables
#' res2 <- auto_describe(iris, vars = c("Sepal.Length", "Species"))
#'
#' @export
auto_describe <- function(data, vars = NULL, digits = 1, color = "#D3D3D3",
                           binary_threshold = 2, cat_threshold = 10,
                           verbose = FALSE) {

  if (!is.data.frame(data)) stop("`data` doit être un data.frame.")
  if (nrow(data) == 0) stop("`data` est vide (0 lignes).")

  if (is.null(vars)) {
    vars <- names(data)
  } else {
    vars_manquantes <- setdiff(vars, names(data))
    if (length(vars_manquantes) > 0) {
      warning("Variables absentes du data.frame : ", paste(vars_manquantes, collapse = ", "))
      vars <- intersect(vars, names(data))
    }
  }

  if (length(vars) == 0) {
    stop("Aucune variable valide à analyser.")
  }

  # Détection du type de chaque variable
  .detect_type <- function(x, var_nm) {
    x_clean <- x[!is.na(x)]
    n_unique <- length(unique(x_clean))

    if (is.logical(x)) return("binaire")

    if (is.numeric(x)) {
      if (n_unique <= binary_threshold) return("binaire")
      if (n_unique <= cat_threshold) return("categorielle")
      return("numerique")
    }

    if (is.factor(x) || is.character(x)) {
      if (n_unique <= binary_threshold) return("binaire")
      return("categorielle")
    }

    if (inherits(x, "Date") || inherits(x, "POSIXt")) return("date")

    # Haven labelled
    if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
      x_num <- suppressWarnings(as.numeric(x_clean))
      n_unique_num <- length(unique(na.omit(x_num)))
      if (n_unique_num <= binary_threshold) return("binaire")
      if (n_unique_num <= cat_threshold) return("categorielle")
      return("numerique")
    }

    return("autre")
  }

  results  <- list()
  type_df_rows <- list()

  for (var_nm in vars) {
    x <- data[[var_nm]]
    label_var <- .get_label(data, var_nm, var_nm)
    type_det <- .detect_type(x, var_nm)

    if (verbose) message("  Variable : ", var_nm, " → type : ", type_det)

    res_var <- tryCatch({
      switch(type_det,
        "binaire" = {
          if (exists("descr_binary", where = asNamespace("analytix"))) {
            descr_binary(data, !!rlang::sym(var_nm), digits = digits, color = color)
          } else {
            descr_categorial(data, !!rlang::sym(var_nm), digits = digits, color = color)
          }
        },
        "categorielle" = {
          descr_categorial(data, !!rlang::sym(var_nm), digits = digits, color = color)
        },
        "numerique" = {
          descr_numeric(data, !!rlang::sym(var_nm), digits = digits, color = color)
        },
        NULL  # types "date" et "autre" ignorés
      )
    }, error = function(e) {
      warning("Erreur pour la variable '", var_nm, "' : ", conditionMessage(e))
      NULL
    })

    if (!is.null(res_var)) {
      results[[label_var]] <- res_var
    }

    type_df_rows[[length(type_df_rows) + 1]] <- data.frame(
      variable     = var_nm,
      label        = label_var,
      type_detected = type_det,
      stringsAsFactors = FALSE
    )
  }

  var_types <- do.call(rbind, type_df_rows)
  attr(results, "var_types") <- var_types

  if (verbose) message("✅ auto_describe : ", length(results), " variables analysées.")

  results
}
