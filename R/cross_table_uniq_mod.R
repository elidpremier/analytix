#' @title Tableau croisé multi-variables avec p-values par modalité (sans référence)
#' @description Génère un tableau croisé comparant plusieurs variables (en lignes) contre une variable cible (en colonne),
#' en calculant pour chaque modalité :
#' - l'effectif et le pourcentage (ligne/colonne/total),
#' - la p-value du test d'indépendance (Fisher ou χ²) entre cette modalité (vs le reste) et l'issue d'intérêt.
#'
#' Aucune modalité de référence n'est utilisée. Chaque modalité est testée indépendamment.
#'
#' @param data data.frame contenant les données.
#' @param target variable en COLONNE (ex: \code{sexe}, \code{outcome}).
#' @param ... variables en LIGNES (ex: \code{var1, var2, var3}).
#' @param outcome_of_interest (optionnel) Modalité de la variable cible à considérer comme événement d'intérêt.
#'   Si NULL, utilise la première modalité non-NA de \code{target}.
#' @param pct Type de pourcentage à afficher : \code{"row"}, \code{"col"}, \code{"total"}.
#' @param test Choix du test statistique : \code{"auto"}, \code{"chisq"}, \code{"fisher"}.
#' @param digits Nombre de décimales pour les pourcentages.
#' @param color Couleur de fond pour les titres de variables (par défaut : gris clair).
#' @param include_na Inclure les valeurs manquantes ? (\code{TRUE} ou \code{FALSE}).
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom flextable as_grouped_data as_flextable set_caption set_header_labels add_footer_lines theme_vanilla bold bg italic fontsize align autofit
#' @importFrom rlang enquo enquos quo_name
#' @export
cross_table_uniq_mod <- function(data, target, ...,
                                 target_name = NULL,
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

  y_full <- data[[target_name]]
  y_factor <- as.factor(y_full)

  # Détermination de l'issue d'intérêt
  if (is.null(outcome_of_interest)) {
    outcome_of_interest <- levels(y_factor)[1]
  } else {
    if (!outcome_of_interest %in% levels(y_factor)) {
      stop("La modalité '", outcome_of_interest, "' n'est pas présente dans les niveaux de '", target_name, "'.")
    }
  }

  all_results <- list()

  for (v_enq in vars_enq) {
    v_name <- rlang::quo_name(v_enq)
    x_full <- data[[v_name]]

    # Inclure ou non les NA
    use_na_arg <- if (include_na) "ifany" else "no"
    tab_full <- table(x_full, y_full, useNA = use_na_arg)

    if (!outcome_of_interest %in% colnames(tab_full)) {
      stop("La modalité '", outcome_of_interest, "' n'est pas dans les colonnes du tableau pour '", v_name, "'.")
    }

    other_cols <- setdiff(colnames(tab_full), outcome_of_interest)
    ordered_cols <- c(outcome_of_interest, other_cols)
    tab_mat <- tab_full[, ordered_cols, drop = FALSE]

    pct_mat <- switch(pct,
                      "row" = prop.table(tab_mat, margin = 1) * 100,
                      "col" = prop.table(tab_mat, margin = 2) * 100,
                      "total" = prop.table(tab_mat) * 100)

    # Initialisation des p-values (une par modalité)
    p_vals <- c()

    for (mod in rownames(tab_mat)) {
      # Construction du tableau 2x2 : [mod vs non-mod] x [outcome vs non-outcome]
      event_in_mod     <- tab_mat[mod, outcome_of_interest]
      non_event_in_mod <- sum(tab_mat[mod, other_cols])

      event_out_mod     <- sum(tab_mat[setdiff(rownames(tab_mat), mod), outcome_of_interest])
      non_event_out_mod <- sum(tab_mat[setdiff(rownames(tab_mat), mod), other_cols])

      sub_tab <- matrix(
        c(event_in_mod, non_event_in_mod,
          event_out_mod, non_event_out_mod),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(c(mod, "Autre"), c(outcome_of_interest, "Autre"))
      )

      # Choix du test
      use_fisher <- FALSE
      if (test == "auto") {
        if (any(sub_tab < 5, na.rm = TRUE)) use_fisher <- TRUE
      } else if (test == "fisher") {
        use_fisher <- TRUE
      }

      # Exécution du test
      if (use_fisher) {
        res_test <- tryCatch(fisher.test(sub_tab), error = function(e) list(p.value = NA))
      } else {
        res_test <- tryCatch(chisq.test(sub_tab, correct = FALSE), error = function(e) list(p.value = NA))
      }

      # Formatage de la p-value
      p_val <- res_test$p.value
      if (is.na(p_val)) {
        p_str <- "-"
      } else if (p_val < 0.001) {
        p_str <- "<0,001"
      } else {
        p_str <- format(round(p_val, 3), decimal.mark = ",")
      }
      p_vals[mod] <- p_str
    }

    # Construction du bloc de cellules
    df_n   <- as.data.frame(tab_mat)   %>% setNames(c("Modalité", "Target", "n"))
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

    # Ajout de la p-value
    stats_df <- data.frame(Modalité = names(p_vals), p_value = as.character(p_vals))
    block <- merge(block, stats_df, by = "Modalité", sort = FALSE)
    block <- cbind(Variable = v_name, block)
    all_results[[v_name]] <- block
  }

  final_df <- do.call(rbind, all_results)

  # --- Note dynamique ---
  note_parts <- c(
    paste0("n (%) ; % = pourcentage (", pct, ")"),
    "p-value : test d'indépendance (modalité vs reste de la population)"
  )
  note_text <- paste0("Notes : ", paste(note_parts, collapse = " ; "), ".")

  # --- Titre ---
  caption_text <- paste0("Analyse multi-variables contre : ", target_name,
                         if (!is.null(outcome_of_interest)) paste0(" – Issue d’intérêt : ", outcome_of_interest))

  # --- Flextable ---
  ft <- as_grouped_data(final_df, groups = "Variable") %>%
    as_flextable() %>%
    set_caption(caption_text) %>%
    set_header_labels(Modalité = "Modalités", p_value = "p-value") %>%
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


