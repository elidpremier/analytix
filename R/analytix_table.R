#' @title Constructeur pour les objets de résultats de tableaux analytix
#' @description Encapsule un jeu de données tidy ($data) et son objet flextable ($flextable)
#' dans une structure standardisée de classe `analytix_table`.
#'
#' @param data data.frame des résultats.
#' @param flextable objet flextable associé.
#' @param ... Métadonnées additionnelles.
#' @return Un objet de classe `analytix_table`.
#' @export
as_analytix_table <- function(data, flextable, ...) {
  res <- list(
    data = data,
    flextable = flextable,
    ...
  )
  class(res) <- c("analytix_table", "list")
  res
}

#' @export
print.analytix_table <- function(x, ...) {
  if (!is.null(x$flextable) && inherits(x$flextable, "flextable")) {
    print(x$flextable, ...)
  } else {
    print.default(x, ...)
  }
}

#' @export
as_flextable.analytix_table <- function(x, ...) {
  x$flextable
}

#' Résolution robuste du nom de variable (Symbole non quoté, Chaîne littérale ou Variable R dans une boucle)
#' @param data data.frame source
#' @param var_quo quosure de la variable
#' @return Chaîne de caractères correspondant au nom de colonne dans data
#' @noRd
.resolve_var_name <- function(data, var_quo) {
  expr <- rlang::quo_get_expr(var_quo)

  # 1. Si c'est une chaîne littérale ("sexe")
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }

  # 2. Si c'est un symbole non quoté directement présent dans data (ex: sexe)
  sym_name <- tryCatch(rlang::as_name(var_quo), error = function(e) NULL)
  if (!is.null(sym_name) && sym_name %in% names(data)) {
    return(sym_name)
  }

  # 3. Tenter d'évaluer var_quo dans l'environnement de l'appelant (ex: v <- "sexe" dans une boucle)
  eval_val <- tryCatch(rlang::eval_tidy(var_quo), error = function(e) NULL)
  if (is.character(eval_val) && length(eval_val) == 1 && eval_val %in% names(data)) {
    return(eval_val)
  }

  # Fallback
  if (!is.null(sym_name)) sym_name else as.character(eval_val)
}
