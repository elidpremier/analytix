#' @title Mise en forme académique pour objets flextable (Déprécié)
#' @description \code{fmt_flextable()} est déprécié. Utilisez directement \code{\link{theme_analytique}} à la place.
#'
#' @param ft Objet `flextable`, `analytix_table` ou dataframe.
#' @param max_width Largeur maximale de la table en pouces (défaut: 6.3 pouces).
#' @param font_size Taille de police en points (défaut: 9).
#' @param font_family Nom de la police (défaut: "Times New Roman").
#' @param compact Logique (défaut: TRUE).
#' @param color Couleur d'en-tête (défaut: "transparent").
#' @param caption Titre / Légende du tableau.
#' @param ... Autres arguments transmis à \code{theme_analytique()}.
#'
#' @return L'objet `flextable` mis en forme.
#' @export
fmt_flextable <- function(ft, max_width = 6.3, font_size = 9,
                             font_family = "Times New Roman",
                             compact = TRUE, color = "transparent",
                             caption = NULL, ...) {
  warning("`fmt_flextable()` est déprécié. Utilisez `theme_analytique()` à la place.")
  page_width_cm <- max_width * 2.54
  theme_analytique(
    data = ft,
    page_width = page_width_cm,
    font_size = font_size,
    font_family = font_family,
    compact = compact,
    color = color,
    caption = caption,
    ...
  )
}
