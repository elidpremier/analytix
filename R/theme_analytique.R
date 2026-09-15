#' Applique un thème analytique à un tableau
#'
#' @description
#' Convertit automatiquement les données en flextable et applique un formatage cohérent.
#' Les colonnes sont redimensionnées proportionnellement pour que le tableau occupe
#' exactement la largeur utile de la page Word (layout "fixed" — garantit la pleine largeur).
#' Ajuste automatiquement la taille de police et le padding si le tableau possède un grand nombre de colonnes (>= 6).
#'
#' @param data Un objet flextable, analytix_table, dataframe, tibble, ou structure convertible en flextable
#' @param page_width Largeur de la zone utile de la page en cm.
#'   Par défaut : 16 cm (largeur Word standard A4 avec marges 2,5 cm)
#' @param color Couleur de l'en-tête (défaut: "transparent")
#' @param zebre Logique. Applique un zébrage alterné sur les lignes du corps (défaut: FALSE)
#' @param zebre_color Couleur utilisée pour le zébrage (défaut: "#F2F2F2")
#' @param font_size Taille de police en points (défaut: 11)
#' @param font_family Nom de la police (défaut: "Times New Roman")
#' @param compact Logique. Si TRUE, réduit les marges internes (padding) (défaut: FALSE)
#' @param caption Titre / Légende personnalisé du tableau (optionnel)
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
                               compact = FALSE, caption = NULL, ...) {

  if (!is.numeric(page_width) || page_width <= 0) {
    stop("page_width doit être un nombre positif")
  }

  dots <- list(...)
  if ("caption" %in% names(dots)) {
    if (is.null(caption)) caption <- dots$caption
    dots$caption <- NULL
  }

  if (inherits(data, "analytix_table") || (is.list(data) && "flextable" %in% names(data) && inherits(data$flextable, "flextable"))) {
    ft <- data$flextable
  } else if (inherits(data, "flextable")) {
    ft <- data
  } else if (is.data.frame(data) || tibble::is_tibble(data)) {
    ft <- do.call(flextable::flextable, c(list(data = data), dots))
  } else {
    stop("L'argument data doit être un objet flextable, analytix_table, dataframe ou tibble.")
  }

  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }

  n_cols <- flextable::ncol_keys(ft)

  # Auto-scaling dynamique si nombre élevé de colonnes pour éviter tout débordement Word
  if (n_cols >= 8) {
    font_size <- min(font_size, 8.0)
    compact <- TRUE
  } else if (n_cols >= 6) {
    font_size <- min(font_size, 8.5)
  }

  # --- Étape 1 : Thème et formatage de base ---
  ft <- ft %>%
    flextable::theme_booktabs() %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::font(part = "all", fontname = font_family)

  if (compact || n_cols >= 6) {
    pad_val <- if (n_cols >= 7) 2 else 3
    ft <- ft %>%
      flextable::padding(padding.top = 2, padding.bottom = 2,
                         padding.left = pad_val, padding.right = pad_val, part = "all")
  }

  ft <- ft %>% flextable::bg(part = "header", bg = color)

  if (isTRUE(zebre)) {
    ft <- ft %>%
      flextable::bg(i = seq(1, flextable::nrow_part(ft, "body"), by = 2),
                    bg = zebre_color, part = "body")
  }

  ft <- ft %>% flextable::align(j = 1, align = "left", part = "all")

  if (n_cols >= 2) {
    ft <- ft %>% flextable::align(j = 2:n_cols, align = "center", part = "all")
  }

  # --- Étape 2 : Forcer la pleine largeur de page (layout "fixed") ---
  page_width_in <- page_width / 2.54

  col_widths <- ft$body$colwidths
  if (is.null(col_widths) || length(col_widths) == 0) {
    col_widths <- rep(0.75, n_cols)
  }

  total_natural <- sum(col_widths)
  if (total_natural > 0) {
    col_widths_scaled <- col_widths * (page_width_in / total_natural)
  } else {
    col_widths_scaled <- rep(page_width_in / n_cols, n_cols)
  }

  ft <- ft %>%
    flextable::set_table_properties(layout = "fixed", align = "center") %>%
    flextable::width(j = seq_len(n_cols), width = col_widths_scaled)

  return(ft)
}
