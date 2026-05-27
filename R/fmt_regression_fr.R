#' @title Formater les tableaux de régression gtsummary en français
#' @description Applique un formatage professionnel aux tableaux de régression (OR, IC 95%, p-value)
#' @param x Un objet \code{tbl_regression} ou \code{tbl_uvregression}
#' @param digits Nombre de décimales pour les OR et IC
#' @return Un objet gtsummary modifié
#' @export
fmt_regression_fr <- function(x, digits = 2) {
  if (!requireNamespace("gtsummary", quietly = TRUE)) stop("gtsummary requis")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")

  x %>%
    gtsummary::modify_table_body(
      ~ .x %>%
        dplyr::mutate(
          estimate_text = dplyr::case_when(
            reference_row == TRUE ~ "(Réf.)",
            row_type == "level" ~ paste0(
              format(round(estimate, digits), nsmall = digits, decimal.mark = ","),
              " [",
              format(round(conf.low, digits), nsmall = digits, decimal.mark = ","),
              " - ",
              format(round(conf.high, digits), nsmall = digits, decimal.mark = ","),
              "]"
            ),
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::relocate(dplyr::any_of("p.value"), .after = dplyr::last_col())
    ) %>%
    gtsummary::modify_column_hide(column = c(estimate, conf.low, conf.high)) %>%
    gtsummary::modify_header(estimate_text ~ "**OR (IC 95%)**")
}
