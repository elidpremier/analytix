#' @title Tableau croisé de deux variables catégorielles
#' @description Effectifs et pourcentages (lignes, colonnes, total) avec test du khi2.
#' @param data data.frame
#' @param var1 première variable
#' @param var2 deuxième variable
#' @param var1_name libellé de var1
#' @param var2_name libellé de var2
#' @param digits nombre de décimales
#' @param show_test afficher le test du khi2 ?
#' @param color couleur de l'en-tête
#' @return un objet de classe "freq_cross"
#' @examples
#' freq_cross(mtcars, am, cyl, var1_name = "Boîte automatique", var2_name = "Cylindres")
#' @export
freq_cross <- function(data, var1, var2, var1_name = NULL, var2_name = NULL,
                       digits = 1, show_test = TRUE, color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble requis")

  v1 <- base::deparse(base::substitute(var1))
  v2 <- base::deparse(base::substitute(var2))

  if (is.null(var1_name)) var1_name <- v1
  if (is.null(var2_name)) var2_name <- v2

  tab <- base::table(data[[v1]], data[[v2]], useNA = "ifany")

  # Calcul des pourcentages
  tab_row <- base::prop.table(tab, margin = 1) * 100
  tab_col <- base::prop.table(tab, margin = 2) * 100
  tab_tot <- base::prop.table(tab) * 100

  # Préparer les données pour le flextable
  tab_df <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(tab_df) <- c("Ligne", "Colonne", "n")

  # Ajouter les pourcentages
  tab_df <- tab_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pct_ligne = tab_row[Ligne, Colonne],
      pct_colonne = tab_col[Ligne, Colonne],
      pct_total = tab_tot[Ligne, Colonne]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cellule = base::sprintf(
        "%s\n(%s%% ; %s%% ; %s%%)",
        n,
        base::format(base::round(pct_ligne, digits), nsmall = digits, decimal.mark = ","),
        base::format(base::round(pct_colonne, digits), nsmall = digits, decimal.mark = ","),
        base::format(base::round(pct_total, digits), nsmall = digits, decimal.mark = ",")
      )
    )

  # Pivot en tableau large
  cross_wide <- tab_df %>%
    dplyr::select(Ligne, Colonne, cellule) %>%
    tidyr::pivot_wider(names_from = Colonne, values_from = cellule, values_fill = "0\n(0,0% ; 0,0% ; 0,0%)")

  # Ajouter ligne "Total"
  total_row <- tibble::tibble(Ligne = "Total")
  for (col in names(cross_wide)[-1]) {
    n_col <- base::sum(tab[, col], na.rm = TRUE)
    total_row[[col]] <- base::as.character(n_col)
  }
  cross_wide <- dplyr::bind_rows(cross_wide, total_row)

  # Test du khi2
  test_res <- base::tryCatch(
    base::chisq.test(tab, simulate.p.value = TRUE),
    error = function(e) NULL
  )
  p_val <- if (!is.null(test_res)) test_res$p.value else NA
  p_str <- if (!is.na(p_val)) {
    if (p_val < 0.001) "<0,001" else base::format(base::round(p_val, 3), decimal.mark = ",")
  } else "Non calculé"

  caption <- base::paste("Croisement :", var1_name, "×", var2_name)
  if (show_test) caption <- base::paste(caption, "— Test χ² p =", p_str)

  ft <- flextable::flextable(cross_wide) %>%
    flextable::set_caption(caption) %>%
    theme_analytique(color = color)

  structure(list(flextable = ft, data = tab, p.value = p_val), class = "freq_cross")
}
