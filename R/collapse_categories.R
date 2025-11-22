#' Regroupement de catégories d'une variable factor/catégorielle
#'
#' @description
#' Transforme une variable catégorielle en regroupant certaines de ses modalités
#' sous de nouvelles catégories, selon une liste de regroupements fournie.
#' La fonction convertit automatiquement les facteurs en caractères avant regroupement,
#' puis reconvertit en factor si `as_factor = TRUE`.
#'
#' @param data Un data.frame ou tibble contenant la variable à transformer.
#' @param var La variable à regrouper (non quote, ex: Species).
#' @param groups Liste nommée où chaque nom est le **nouveau nom de catégorie**
#'   et chaque élément est un vecteur des anciennes modalités à regrouper.
#' @param keep_na Logique, si FALSE les NA seront remplacés par `other_label`.
#' @param as_factor Logique, si TRUE la variable retournée sera un factor.
#' @param other_label Caractère, label pour les modalités non listées dans `groups`.
#'   Si NULL, elles sont conservées inchangées.
#'
#' @return Un tibble avec la variable regroupée.
#'
#' @examples
#' # Exemple 1 : regrouper certaines espèces d'iris
#' collapse_categories(
#'   iris,
#'   Species,
#'   groups = list(SetosaVersicolor = c("setosa", "versicolor"))
#' )
#'
#' # Exemple 2 : regrouper cyl dans mtcars
#' collapse_categories(
#'   mtcars,
#'   cyl,
#'   groups = list(
#'     Petit = c(4, 6),
#'     Gros = 8
#'   ),
#'   other_label = "Autre",
#'   as_factor = TRUE
#' )
#'
#' # Exemple 3 : avec NA dans diamonds
#' library(ggplot2)
#' df <- diamonds
#' df$clarity[sample(1:nrow(df), 20)] <- NA
#' collapse_categories(
#'   df,
#'   clarity,
#'   groups = list(
#'     Bon = c("SI1", "SI2"),
#'     Excellent = c("IF", "VVS1")
#'   ),
#'   keep_na = FALSE,
#'   other_label = "Autre"
#' )
#'
#' @export
collapse_categories <- function(data, var, groups,
                                keep_na = TRUE,
                                as_factor = TRUE,
                                other_label = NULL) {

  var_quo <- rlang::enquo(var)
  var_name <- rlang::as_name(var_quo)

  # Récupérer les valeurs existantes
  new_var <- dplyr::pull(data, !!var_quo)

  # Convertir en character si factor
  if (is.factor(new_var)) {
    new_var <- as.character(new_var)
  }

  # Vérification : existence des catégories
  all_groups <- unlist(groups)
  missing_vals <- setdiff(all_groups, unique(new_var))

  if (length(missing_vals) > 0) {
    warning("Les catégories suivantes n'existent pas dans la variable : ",
            paste(missing_vals, collapse = ", "))
  }

  # Nouvelle variable recodée
  for (name in names(groups)) {
    new_var[new_var %in% groups[[name]]] <- name
  }

  # Traiter NA
  if (!keep_na) {
    new_var[is.na(new_var)] <- ifelse(is.null(other_label), "Autre", other_label)
  }

  # Catégories non regroupées
  if (!is.null(other_label)) {
    new_var[!(new_var %in% names(groups)) & !is.na(new_var)] <- other_label
  }

  # Sortie factor si demandé
  if (as_factor) {
    new_var <- factor(new_var)
  }

  data[[var_name]] <- new_var
  tibble::as_tibble(data)
}
