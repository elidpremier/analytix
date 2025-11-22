#' Recodage rapide d'une variable catégorielle
#'
#' @description
#' Recoder rapidement une variable dans un data.frame avec une syntaxe courte :
#' `quick_code(df, sexe, "H" = "Homme", "F" = "Femme")`.
#'
#' - Par défaut, les valeurs `NA` ne sont pas modifiées.
#' - Pour recoder les `NA`, utiliser l'argument `.na = "Inconnu"` (ou autre).
#'
#' @param data Un data.frame ou tibble.
#' @param var Variable à recoder (unquoted).
#' @param ... Paires "ancien" = "nouveau" indiquant les recodages.
#' @param .na Valeur de remplacement pour les NA (par défaut NULL : les NA sont conservés).
#' @param to Type final de la variable : "character" (par défaut), "factor", ou "keep".
#'
#' @return Un tibble avec la variable recodée.
#' @export
#'
#' @examples
#' df <- data.frame(sexe = c("H", "F", NA))
#' # NA non modifié (par défaut)
#' quick_code(df, sexe, "H" = "Homme", "F" = "Femme")
#'
#' # NA recodé
#' quick_code(df, sexe, "H" = "Homme", "F" = "Femme", .na = "Inconnu")
quick_code <- function(data, var, ..., .na = NULL,
                       to = c("character", "factor", "keep")) {

  to <- match.arg(to)

  # Vérifications
  if (!is.data.frame(data)) stop("'data' doit être un data.frame.")

  var_quo <- rlang::enquo(var)
  var_name <- rlang::as_name(var_quo)

  if (!var_name %in% names(data)) {
    stop(sprintf("La variable '%s' n'existe pas dans 'data'.", var_name))
  }

  recodes <- list(...)
  if (length(recodes) == 0) {
    stop("Aucun recodage fourni. Utiliser 'ancien' = 'nouveau'.")
  }

  # préparation old/new
  old <- names(recodes)
  new_vals <- unlist(recodes, use.names = FALSE)

  # sauvegarde type d'origine
  orig_vec <- data[[var_name]]
  orig_type <- typeof(orig_vec)

  # convertir en caractère pour un recodage sûr
  char_default <- as.character(orig_vec)

  # liste pour dplyr::recode
  recode_list <- stats::setNames(as.list(as.character(new_vals)), old)

  # application du recodage sur les valeurs non manquantes
  new_vec <- dplyr::recode(
    .x = char_default,
    !!!recode_list,
    .default = char_default
  )

  # gestion des NA
  if (!is.null(.na)) {
    new_vec[is.na(orig_vec)] <- .na
  }

  # type final
  if (to == "factor") {
    levels_order <- unique(new_vec)
    new_vec <- factor(new_vec, levels = levels_order)
  } else if (to == "keep") {
    # essayer de reconvertir vers le type original
    new_vec <- tryCatch({
      if (orig_type %in% c("double", "integer")) {
        as.numeric(new_vec)
      } else if (orig_type == "logical") {
        as.logical(new_vec)
      } else {
        new_vec
      }
    },
    warning = function(w) new_vec,
    error   = function(e) new_vec)
  }

  data[[var_name]] <- new_vec
  tibble::as_tibble(data)
}
