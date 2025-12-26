#' @title Tableau croisé multi-variables avec tests statistiques par modalité
#' @description Génère un tableau croisé comparant plusieurs variables (en lignes) contre une variable cible (en colonne),
#' en calculant pour chaque modalité non-référence :
#' - l'effectif et le pourcentage (ligne/colonne/total),
#' - la p-value (Fisher ou χ² selon les effectifs observés),
#' - l'odds ratio et son intervalle de confiance 95% (vs référence).
#' @param data data.frame contenant les données.
#' @param target variable en COLONNE (ex: \code{sexe}, \code{outcome}).
#' @param ... variables en LIGNES (ex: \code{var1, var2, var3}).
#' @param refs_list (optionnel) Liste nommée des modalités de référence, ex: \code{list(var1="Non", var2="Stade 1")}.
#' @param outcome_of_interest (optionnel) Modalité de la variable cible à considérer comme événement d'intérêt pour le calcul de l'OR.
#'   Si NULL, utilise la première modalité non-NA de \code{target}.
#' @param pct Type de pourcentage à afficher : \code{"row"} (par ligne), \code{"col"} (par colonne), \code{"total"} (global).
#' @param test Choix du test statistique : \code{"auto"} (choisi selon effectifs observés), \code{"chisq"}, \code{"fisher"}.
#' @param digits Nombre de décimales pour les pourcentages.
#' @param color Couleur de fond pour les titres de variables (par défaut : gris clair).
#' @param include_na Inclure les valeurs manquantes dans les tableaux ? (\code{TRUE} ou \code{FALSE}).
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom flextable as_grouped_data as_flextable set_caption set_header_labels add_footer_lines theme_vanilla bold bg italic fontsize align autofit
#' @importFrom rlang enquo enquos quo_name
#'
#' @export
cross_table_uniq_mod <- function(data, target, ...,
                              target_name = NULL,
                              refs_list = list(),
                              outcome_of_interest = NULL,
                              pct = c("row", "col", "total"),
                              test = c("auto", "chisq", "fisher"),
                              digits = 1,
                              color = "#D3D3D3",
                              include_na = FALSE) {

  # --- Dépendances ---
  library(dplyr)
  library(tidyr)
  library(flextable)
  library(rlang)

  pct <- match.arg(pct)
  test <- match.arg(test)

  # --- Extraction des variables ---
  target_enq <- rlang::enquo(target)
  vars_enq <- rlang::enquos(...)

  if (is.null(target_name)) target_name <- rlang::quo_name(target_enq)

  y_cols <- data[[target_name]]
  y_cols_factor <- as.factor(y_cols)

  # Détermination de l'issue d'intérêt
  if (is.null(outcome_of_interest)) {
    outcome_of_interest <- levels(y_cols_factor)[1]
  } else {
    if (!outcome_of_interest %in% levels(y_cols_factor)) {
      stop("La modalité '", outcome_of_interest, "' n'est pas présente dans les niveaux de la variable cible '", target_name, "'.")
    }
  }

  all_results <- list()
  has_infinite <- FALSE
  has_na_or <- FALSE

  for (v_enq in vars_enq) {
    v_name <- rlang::quo_name(v_enq)

    x_rows <- data[[v_name]]
    levs_x <- levels(as.factor(x_rows))
    ref_val <- if (!is.null(refs_list[[v_name]])) refs_list[[v_name]] else levs_x[1]

    use_na_arg <- if (include_na) "ifany" else "no"
    tab_full <- table(x_rows, y_cols, useNA = use_na_arg)

    if (!outcome_of_interest %in% colnames(tab_full)) {
      stop("La modalité '", outcome_of_interest, "' n'est pas dans les colonnes du tableau de contingence pour la variable '", v_name, "'.")
    }

    other_cols <- setdiff(colnames(tab_full), outcome_of_interest)
    ordered_cols <- c(outcome_of_interest, other_cols)
    tab_mat <- tab_full[, ordered_cols, drop = FALSE]

    pct_mat <- switch(pct,
                      "row" = prop.table(tab_mat, margin = 1) * 100,
                      "col" = prop.table(tab_mat, margin = 2) * 100,
                      "total" = prop.table(tab_mat) * 100)

    p_vals <- c()
    or_vals <- c()

    for (mod in rownames(tab_mat)) {
      if (mod == ref_val) {
        p_vals[mod] <- "Réf."
        or_vals[mod] <- "1,00 [Réf.]"
      } else {
        if (ncol(tab_mat) > 2) {
          event_col <- tab_mat[mod, outcome_of_interest, drop = FALSE]
          non_event_row_mod <- sum(tab_mat[mod, -1])
          event_ref <- tab_mat[ref_val, outcome_of_interest, drop = FALSE]
          non_event_ref <- sum(tab_mat[ref_val, -1])
          sub_tab <- matrix(c(event_col, non_event_row_mod, event_ref, non_event_ref),
                            nrow = 2, byrow = TRUE,
                            dimnames = list(c(mod, ref_val), c(outcome_of_interest, "Autre")))
        } else {
          sub_tab <- tab_mat[c(ref_val, mod), , drop = FALSE]
          if (rownames(sub_tab)[1] != ref_val) sub_tab <- sub_tab[c(2,1), ]
        }

        use_fisher <- FALSE
        if (test == "auto") {
          # Critère basé sur les effectifs OBSERVÉS (≥ 5 dans toutes les cellules)
          if (any(sub_tab < 5, na.rm = TRUE)) {
            use_fisher <- TRUE
          }
        } else if (test == "fisher") {
          use_fisher <- TRUE
        }

        if (use_fisher) {
          res_test <- tryCatch(fisher.test(sub_tab), error = function(e) list(p.value = NA, estimate = NA, conf.int = c(NA, NA)))
        } else {
          chi_test <- tryCatch(chisq.test(sub_tab, correct = FALSE), error = function(e) NULL)
          if (is.null(chi_test)) {
            res_test <- list(p.value = NA, estimate = NA, conf.int = c(NA, NA))
          } else {
            a <- sub_tab[2,1]; b <- sub_tab[2,2]
            c <- sub_tab[1,1]; d <- sub_tab[1,2]
            or_val <- if (b == 0 || c == 0 || d == 0) NA else (a * d) / (b * c)
            se_log_or <- if (all(sub_tab > 0)) sqrt(1/a + 1/b + 1/c + 1/d) else NA
            if (!is.na(or_val) && !is.na(se_log_or) && is.finite(or_val) && is.finite(se_log_or)) {
              log_or <- log(or_val)
              ci_low <- exp(log_or - 1.96 * se_log_or)
              ci_up  <- exp(log_or + 1.96 * se_log_or)
              ci <- c(ci_low, ci_up)
            } else {
              or_val <- NA
              ci <- c(NA, NA)
            }
            res_test <- list(p.value = chi_test$p.value, estimate = or_val, conf.int = ci)
          }
        }

        p_vals[mod] <- if (is.na(res_test$p.value)) "-"
        else if (res_test$p.value < 0.001) "<0,001"
        else format(round(res_test$p.value, 3), decimal.mark = ",")

        # ✅ Formatage avec virgule pour OR et IC
        if (!is.na(res_test$estimate) && is.finite(res_test$estimate)) {
          or_fmt   <- format(round(res_test$estimate, 2), nsmall = 2, decimal.mark = ",")
          ci_low_fmt <- format(round(res_test$conf.int[1], 2), nsmall = 2, decimal.mark = ",")
          ci_up_fmt  <- format(round(res_test$conf.int[2], 2), nsmall = 2, decimal.mark = ",")

          ci_low_fmt <- if (is.infinite(res_test$conf.int[1])) "∞" else ci_low_fmt
          ci_up_fmt  <- if (is.infinite(res_test$conf.int[2])) "∞" else ci_up_fmt

          or_str <- paste0(or_fmt, " [", ci_low_fmt, " - ", ci_up_fmt, "]")
          or_vals[mod] <- or_str
          if (any(is.infinite(res_test$conf.int))) has_infinite <- TRUE
        } else {
          or_vals[mod] <- "N/A"
          has_na_or <- TRUE
        }
      }
    }

    df_n <- as.data.frame(tab_mat) %>% setNames(c("Modalité", "Target", "n"))
    df_pct <- as.data.frame(pct_mat) %>% setNames(c("Modalité", "Target", "pct"))

    block <- merge(df_n, df_pct) %>%
      mutate(
        cellule = sprintf(
          "%s (%s%%)",
          n,
          format(round(pct, digits), nsmall = digits, decimal.mark = ",")
        )
      ) %>%
      select(Modalité, Target, cellule) %>%
      pivot_wider(names_from = Target, values_from = cellule)

    stats_df <- data.frame(Modalité = names(p_vals), p_value = as.character(p_vals), or_ic = as.character(or_vals))
    block <- merge(block, stats_df, by = "Modalité", sort = FALSE)
    block <- cbind(Variable = v_name, block)
    all_results[[v_name]] <- block
  }

  final_df <- do.call(rbind, all_results)

  # --- Note dynamique ---
  note_parts <- c(
    paste0("n (%) ; % = pourcentage (", pct, ")"),
    "Réf. = référence",
    "p-value vs réf.",
    paste0("OR [IC 95%] (vs '", outcome_of_interest, "')")
  )
  if (has_na_or) note_parts <- c(note_parts, "N/A = non calculable")
  if (has_infinite) note_parts <- c(note_parts, "∞ = intervalle de confiance infini")
  note_text <- paste0("Notes : ", paste(note_parts, collapse = " ; "), ".")

  # --- Titre incluant l'issue d'intérêt ---
  caption_text <- paste0("Analyse multi-variables contre : ", target_name, " – Issue d’intérêt : ", outcome_of_interest)

  ft <- as_grouped_data(final_df, groups = "Variable") %>%
    as_flextable() %>%
    set_caption(caption_text) %>%
    set_header_labels(Modalité = "Modalités", p_value = "p-value", or_ic = "OR [IC 95%]") %>%
    add_footer_lines(note_text) %>%
    theme_vanilla() %>%
    bold(i = ~ !is.na(Variable), bold = TRUE) %>%
    bg(i = ~ !is.na(Variable), bg = color) %>%
    italic(part = "footer") %>%
    fontsize(size = 9, part = "footer") %>%
    align(align = "center", part = "all") %>%
    autofit()

  return(ft)
}
