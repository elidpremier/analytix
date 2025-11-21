#' Recodage rapide d'une variable catégorielle
#'
#' @description
#' Fonction simple et intuitive pour recoder rapidement une variable
#' catégorielle dans un data.frame.
#' Permet une syntaxe courte du type :
#' `quick_code(df, sexe, "H" = "Homme", "F" = "Femme")`
#'
#' @param data Un data frame ou tibble.
#' @param var La variable à recoder (sans guillemets).
#' @param ... Paires "ancien" = "nouveau" indiquant les recodages.
#'
#' @return Un nouveau data.frame avec la variable recodée.
#' @export
#'
#' @examples
#' df <- data.frame(sexe = c("H", "F", "H"))
#' quick_code(df, sexe, "H" = "Homme", "F" = "Femme")
#'
quick_code <- function(data, var, ...) {

  # Vérifications
  if (missing(data)) stop("Argument 'data' manquant.")
  if (!is.data.frame(data)) stop("'data' doit être un data.frame.")

  var_quo <- rlang::enquo(var)
  var_name <- rlang::as_name(var_quo)

  if (!var_name %in% names(data)) {
    stop(paste0("La variable '", var_name, "' n'existe pas dans 'data'."))
  }

  # Liste des recodages
  recodes <- list(...)

  if (length(recodes) == 0) {
    stop("Aucun recodage fourni dans '...'.")
  }

  # Convertir en nom -> valeur
  old <- names(recodes)
  new <- unlist(recodes, use.names = FALSE)

  # Recodage
  data[[var_name]] <- dplyr::recode(
    .x = data[[var_name]],
    !!!stats::setNames(as.list(new), old),
    .default = data[[var_name]]
  )

  return(tibble::as_tibble(data))
}
