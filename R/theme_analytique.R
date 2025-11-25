#' Applique un thème analytique à une flextable
theme_analytique <- function(ft, color = "#D3D3D3") {
  ft %>%
    flextable::theme_booktabs() %>%
    flextable::bg(bg = color, part = "header") %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::align(j = 1, align = "left") %>%
    flextable::align(j = 2:ncol(ft$body$dataset), align = "center")
}
