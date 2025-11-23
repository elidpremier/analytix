#' Catégorisation (discrétisation) d'une variable numérique
#'
#' @description
#' Transforme une variable numérique continue en une variable catégorielle selon des seuils définis.
#' Utile pour regrouper l'âge, les scores ou d'autres indicateurs continus.
#'
#' @param data Un data.frame ou tibble contenant la variable à transformer.
#' @param var La variable numérique à discrétiser (non quote, ex: age).
#' @param breaks Vecteur de seuils numériques ou nombre de classes si discretisation automatique.
#' @param labels Optionnel. Vecteur de labels pour les catégories. Si NULL, R génère des intervalles.
#' @param include_lowest Logique, si TRUE, le premier intervalle inclut la borne inférieure.
#' @param right Logique, si TRUE, les intervalles sont fermés à droite (par défaut TRUE).
#' @param as_factor Logique, si TRUE, retourne un factor. Sinon character.
#'
#' @return Un tibble avec la variable transformée en catégories.
#'
#' @examples
#' # Exemple 1 : âge en classes
#' df <- data.frame(age = c(5, 12, 17, 23, 35, 45, 60, 78))
#' categorize_numeric(df, age, breaks = c(0, 18, 35, 50, 100))
#'
#' # Exemple 2 : avec labels personnalisés
#' categorize_numeric(
#'   df, age,
#'   breaks = c(0, 18, 35, 50, 100),
#'   labels = c("Enfant", "Jeune adulte", "Adulte", "Senior")
#' )
#'
#' # Exemple 3 : discretisation automatique en 4 classes
#' categorize_numeric(df, age, breaks = 4)
#'
#' @export
categorize_numeric <- function(data, var, breaks, labels = NULL,
                               include_lowest = TRUE, right = TRUE,
                               as_factor = TRUE) {

  var_quo <- rlang::enquo(var)
  var_name <- rlang::as_name(var_quo)

  vec <- dplyr::pull(data, !!var_quo)

  # Si breaks est un entier, utiliser cut pour découper en n classes égales
  if (length(breaks) == 1 && is.numeric(breaks)) {
    breaks <- seq(min(vec, na.rm = TRUE), max(vec, na.rm = TRUE), length.out = breaks + 1)
  }

  # Discrétisation
  cat_var <- cut(vec,
                 breaks = breaks,
                 labels = labels,
                 include.lowest = include_lowest,
                 right = right)

  if (!as_factor) {
    cat_var <- as.character(cat_var)
  }

  data[[var_name]] <- cat_var
  tibble::as_tibble(data)
}
