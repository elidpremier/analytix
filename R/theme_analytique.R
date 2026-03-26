#' Applique un thème analytique à un tableau
#'
#' @description
#' Convertit automatiquement les données en flextable et applique un formatage cohérent.
#' Par défaut, le tableau s'ajuste à la largeur d'une page Word standard (16.5 cm).
#'
#' @param data Un objet flextable, dataframe, tibble, ou toute structure convertible en flextable
#' @param page_width Largeur de la zone utile de la page en cm.
#'   Par défaut : 16.5 cm (largeur Word standard A4 avec marges)
#' @param color Couleur de l'en-tête (défaut: "#D3D3D3")
#' @param ... Autres arguments passés à flextable::flextable() si conversion nécessaire
#'
#' @return Un objet flextable formaté
#'
#' @examples
#' \dontrun{
#' # Avec un dataframe
#' theme_analytique(head(iris))
#'
#' # Avec un tibble
#' theme_analytique(tibble::tibble(x = 1:5, y = letters[1:5]))
#'
#' # Avec une flextable existante
#' ft <- flextable::flextable(head(iris))
#' theme_analytique(ft)
#'
#' # Personnalisation
#' theme_analytique(head(mtcars), page_width = 14, color = "blue")
#' }
#'
#' @export
theme_analytique <- function(data, page_width = 16, color = "#D3D3D3", ...) {

  # Validation des paramètres
  if (!is.numeric(page_width) || page_width <= 0) {
    stop("page_width doit être un nombre positif")
  }

  # Vérifier si data est déjà une flextable, sinon la convertir
  if (inherits(data, "flextable")) {
    ft <- data
  } else if (is.data.frame(data) || tibble::is_tibble(data)) {
    # Convertir les données en flextable
    ft <- flextable::flextable(data, ...)
  } else {
    stop("L'argument data doit être une flextable, un dataframe, un tibble ou une structure convertible en flextable")
  }

  # Appliquer le thème booktabs comme base
  ft <- ft %>%
    flextable::theme_booktabs() %>%
    flextable::set_table_properties(
      layout = "fixed",  # Essentiel pour Word
      align = "left"
    ) %>%
    flextable::bg(i = seq(1, flextable::nrow_part(ft, "body"), by = 2),
                  bg = "#F2F2F2") %>%  # Zébrure pour les lignes du corps
    flextable::color(color = "black", part = "header") %>%  # Couleur du texte pour l'en-tête
    flextable::bold(part = "header") %>%  # Gras pour l'en-tête
    flextable::fontsize(size = 11, part = "all") %>%  # Taille de police pour tout le tableau
    flextable::font(part = "all", fontname = "Times New Roman")  # Police pour tout le tableau

  # Appliquer la couleur d'en-tête personnalisée
  ft <- ft %>% flextable::bg(part = "header", bg = color)

  # Alignement à gauche pour la première colonne
  ft <- ft %>% flextable::align(j = 1, align = "left", part = "all")

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
