# Palette et contrôles internes de présentation accessible.

.analytix_accessible_header <- "#1F4E79"

.analytix_accessible_palette <- c(
  "#1F4E79", "#B03A2E", "#117864", "#9A6700",
  "#6C3483", "#7D3C98", "#145A7A"
)

.analytix_luminance <- function(color) {
  rgb <- grDevices::col2rgb(color, alpha = FALSE) / 255
  rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}

.analytix_contrast_ratio <- function(foreground, background = "#FFFFFF") {
  foreground_lum <- .analytix_luminance(foreground)
  background_lum <- .analytix_luminance(background)
  (max(foreground_lum, background_lum) + 0.05) /
    (min(foreground_lum, background_lum) + 0.05)
}

.analytix_validate_header_color <- function(color) {
  if (identical(color, "transparent")) return(.analytix_accessible_header)
  ratio <- .analytix_contrast_ratio(color, "#FFFFFF")
  if (ratio < 4.5) {
    warning(
      "La couleur d'en-tête n'atteint pas le contraste WCAG AA de 4,5:1 avec le blanc.",
      call. = FALSE
    )
  }
  color
}
