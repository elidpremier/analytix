#' Applique un thème analytique à une flextable
#'
#' @description
#' Applique un formatage cohérent pour tous les tableaux du package analytique.
#' Par défaut, le tableau s'ajuste à la largeur d'une page Word standard (16.5 cm).
#'
#' @param ft Un objet flextable
#' @param page_width Largeur de la zone utile de la page en cm.
#'   Par défaut : 16.5 cm (largeur Word standard A4 avec marges)
#' @param color Couleur de l'en-tête (défaut: "#D3D3D3")
#'
#' @return Un objet flextable formaté
#'
#' @examples
#' \dontrun{
#' ft <- flextable::flextable(head(iris))
#' theme_analytique(ft)
#'
#' # Personnalisation
#' theme_analytique(ft, page_width = 14, color = "blue")
#' }
#'
#' @export
theme_analytique <- function(ft, page_width = 16, color = "#D3D3D3") {

  # Validation des paramètres
  if (!is.numeric(page_width) || page_width <= 0) {
    stop("page_width doit être un nombre positif")
  }

  # Forcer le layout fixe pour Word
  ft <- ft %>%
    flextable::theme_booktabs() %>%
    flextable::set_table_properties(
      layout = "fixed",  # ← Essentiel pour Word
      align = "left"
    ) %>%
    flextable::bg(bg = color, part = "header") %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::font(part = "all", fontname = "Times New Roman")

  # Alignement centre pour colonnes 2+
  n_cols <- flextable::ncol_keys(ft)
  if (n_cols >= 2) {
    ft <- ft %>% flextable::align(j = 2:n_cols, align = "center", part = "all")
  }

  # Répartition proportionnelle de la largeur (en cm)
  col_widths <- rep(page_width / n_cols, n_cols)
  ft <- ft %>% flextable::width(width = col_widths, unit = "cm")

  return(ft)
}
