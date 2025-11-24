#' @title Statistiques descriptives par groupe
#' @description Tableau de statistiques (moyenne, médiane, etc.) par catégorie d'une variable de groupe.
#' @param data data.frame
#' @param var variable numérique à décrire
#' @param by variable catégorielle de groupement
#' @param var_name libellé de la variable numérique
#' @param by_name libellé de la variable de groupe
#' @param digits nombre de décimales
#' @param color couleur de l'en-tête
#' @return un objet de classe "descr_by_group" contenant un flextable
#' @examples
#' descr_by_group(mtcars, mpg, cyl, var_name = "Consommation", by_name = "Cylindres")
#' @export
descr_by_group <- function(data, var, by, var_name = NULL, by_name = NULL, digits = 1, color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")

  var_nm <- base::deparse(base::substitute(var))
  by_nm <- base::deparse(base::substitute(by))

  if (!var_nm %in% base::names(data)) stop("Variable '", var_nm, "' non trouvée.")
  if (!by_nm %in% base::names(data)) stop("Variable '", by_nm, "' non trouvée.")

  if (is.null(var_name)) var_name <- var_nm
  if (is.null(by_name)) by_name <- by_nm

  x <- data[[var_nm]]
  g <- data[[by_nm]]

  if (!base::is.numeric(x)) stop("`var` doit être numérique.")

  # Calcul par groupe
  stats_df <- data %>%
    dplyr::group_by(dplyr::across({{ by }})) %>%
    dplyr::summarise(
      n = base::sum(!base::is.na({{ var }})),
      Moyenne = base::round(base::mean({{ var }}, na.rm = TRUE), digits),
      Mediane = base::round(stats::median({{ var }}, na.rm = TRUE), digits),
      Ecart_type = base::round(stats::sd({{ var }}, na.rm = TRUE), digits),
      Minimum = base::min({{ var }}, na.rm = TRUE),
      Maximum = base::max({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )

  # Ligne des effectifs (convertie en caractères dès le début)
  n_row <- stats_df %>%
    dplyr::select({{ by }}, n) %>%
    tidyr::pivot_wider(names_from = {{ by }}, values_from = n) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(base::is.numeric), as.character),
      Statistique = "Effectif",
      .before = 1
    )

  # Lignes des statistiques (déjà en caractères)
  stats_wide <- stats_df %>%
    dplyr::select(-n) %>%
    tidyr::pivot_longer(
      cols = -{{ by }},
      names_to = "Statistique",
      values_to = "Valeur"
    ) %>%
    dplyr::mutate(
      Valeur = base::format(Valeur, nsmall = digits, decimal.mark = ",")
    ) %>%
    tidyr::pivot_wider(names_from = {{ by }}, values_from = Valeur)

  # Combiner : maintenant les types sont compatibles (tous character)
  final_df <- dplyr::bind_rows(n_row, stats_wide)

  caption <- base::paste("Distribution de", var_name, "par", by_name)

  ft <- flextable::flextable(final_df) %>%
    flextable::set_caption(caption) %>%
    flextable::theme_booktabs() %>%
    flextable::bg(bg = color, part = "header") %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::align(j = 2:ncol(final_df), align = "center", part = "all") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::width(width = 16)

  structure(list(flextable = ft, data = final_df), class = "descr_by_group")
}
