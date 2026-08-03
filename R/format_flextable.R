#' @title Mise en forme académique pour objets flextable (Rapports Word)
#' @description Alias / Wrapper autour de `theme_analytique()` qui applique un formatage compact
#' et adapté aux normes de publication Word (Times New Roman 9pt, marges réduites, largeur max).
#' 
#' @param ft Objet `flextable` ou dataframe.
#' @param max_width Largeur maximale de la table en pouces (défaut: 6.3 pouces = ~16 cm).
#' @param font_size Taille de police en points (défaut: 9).
#' @param font_family Nom de la police de caractères (défaut: "Times New Roman").
#' @param compact Logique. Si TRUE, applique un espacement resserré (défaut: TRUE).
#' @param color Couleur d'en-tête (défaut: "#D3D3D3").
#' @param ... Autres arguments passés à `theme_analytique()`.
#' 
#' @return L'objet `flextable` mis en forme.
#' 
#' @examples
#' ft <- flextable::flextable(head(mtcars))
#' format_flextable(ft)
#' 
#' @export
format_flextable <- function(ft, max_width = 6.3, font_size = 9,
                             font_family = "Times New Roman",
                             compact = TRUE, color = "#D3D3D3", ...) {
  page_width_cm <- max_width * 2.54
  theme_analytique(
    data = ft,
    page_width = page_width_cm,
    font_size = font_size,
    font_family = font_family,
    compact = compact,
    color = color,
    ...
  )
}
