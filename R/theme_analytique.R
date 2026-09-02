#' Applique un thème analytique à un tableau
#'
#' @description
#' Convertit automatiquement les données en flextable et applique un formatage cohérent.
#' Les colonnes sont redimensionnées proportionnellement pour que le tableau occupe
#' exactement la largeur utile de la page Word (layout "fixed" — garantit le pleine largeur).
#'
#' @param data Un objet flextable, dataframe, tibble, ou toute structure convertible en flextable
#' @param page_width Largeur de la zone utile de la page en cm.
#'   Par défaut : 16 cm (largeur Word standard A4 avec marges 2,5 cm)
#' @param color Couleur de l'en-tête (défaut: "transparent")
#' @param zebre Logique. Applique un zébrage alterné sur les lignes du corps (défaut: FALSE)
#' @param zebre_color Couleur utilisée pour le zébrage (défaut: "#F2F2F2")
#' @param font_size Taille de police en points (défaut: 11)
#' @param font_family Nom de la police (défaut: "Times New Roman")
#' @param compact Logique. Si TRUE, réduit les marges internes (padding) (défaut: FALSE)
#' @param ... Autres arguments passés à flextable::flextable() si conversion nécessaire
#'
#' @return Un objet flextable formaté, pleine largeur de page
#'
#' @examples
#' theme_analytique(head(iris))
#' theme_analytique(head(mtcars), compact = TRUE, font_size = 9)
#'
#' @export
theme_analytique <- function(data, page_width = 16, color = "transparent",
                               zebre = FALSE, zebre_color = "#F2F2F2",
                               font_size = 11, font_family = "Times New Roman",
                               compact = FALSE, ...) {

  if (!is.numeric(page_width) || page_width <= 0) {
    stop("page_width doit être un nombre positif")
  }

  if (inherits(data, "flextable")) {
    ft <- data
  } else if (is.data.frame(data) || tibble::is_tibble(data)) {
    ft <- flextable::flextable(data, ...)
  } else {
    stop("L'argument data doit être une flextable, un dataframe, un tibble ou une structure convertible en flextable")
  }

  # --- Étape 1 : Thème et formatage de base ---
  ft <- ft %>%
    flextable::theme_booktabs() %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::font(part = "all", fontname = font_family)

  if (compact) {
    ft <- ft %>%
      flextable::padding(padding.top = 2, padding.bottom = 2,
                         padding.left = 4, padding.right = 4, part = "all")
  }

  ft <- ft %>% flextable::bg(part = "header", bg = color)

  if (isTRUE(zebre)) {
    ft <- ft %>%
      flextable::bg(i = seq(1, flextable::nrow_part(ft, "body"), by = 2),
                    bg = zebre_color, part = "body")
  }

  ft <- ft %>% flextable::align(j = 1, align = "left", part = "all")

  n_cols <- flextable::ncol_keys(ft)
  if (n_cols >= 2) {
    ft <- ft %>% flextable::align(j = 2:n_cols, align = "center", part = "all")
  }

  # --- Étape 2 : Forcer la pleine largeur de page (layout "fixed") ---
  # La conversion cm -> inches est : 1 cm = 1/2.54 inches
  page_width_in <- page_width / 2.54

  # Récupérer les largeurs actuelles depuis le slot interne (plus fiable que flextable::dim)
  col_widths <- ft$body$colwidths
  if (is.null(col_widths) || length(col_widths) == 0) {
    col_widths <- rep(0.75, n_cols)  # fallback : 0.75 inch par colonne (défaut flextable)
  }

  # Redistribuer proportionnellement pour atteindre exactement page_width_in
  total_natural <- sum(col_widths)
  if (total_natural > 0) {
    col_widths_scaled <- col_widths * (page_width_in / total_natural)
  } else {
    col_widths_scaled <- rep(page_width_in / n_cols, n_cols)
  }

  # Appliquer layout fixed + largeurs forcées
  ft <- ft %>%
    flextable::set_table_properties(layout = "fixed", align = "center") %>%
    flextable::width(j = seq_len(n_cols), width = col_widths_scaled)

  return(ft)
}
