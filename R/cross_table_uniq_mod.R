#' @title Tableau croisé multi-variables avec p-values par modalité (sans référence) et p global
#' @description Génère un tableau croisé comparant plusieurs variables (en lignes) contre une variable cible (en colonne),
#' en calculant pour chaque modalité :
#' - l'effectif et le pourcentage (ligne/colonne/total),
#' - la p-value du test d'indépendance (Fisher ou χ²) entre cette modalité (vs le reste) et l'issue d'intérêt,
#' - la p-value globale du test d'indépendance de la variable (en colonne dédiée à droite).
#'
#' Aucune modalité de référence n'est utilisée. Chaque modalité est testée indépendamment.
#'
#' @param data data.frame contenant les données.
#' @param target variable en COLONNE (ex: \code{sexe}, \code{outcome}).
#' @param ... variables en LIGNES (ex: \code{var1, var2, var3}).
#' @param target_name Nom/libellé de la variable cible pour l'affichage.
#' @param outcome_of_interest (optionnel) Modalité de la variable cible à considérer comme événement d'intérêt.
#'   Si NULL, utilise la première modalité non-NA de \code{target}.
#' @param pct Type de pourcentage à afficher : \code{"row"}, \code{"col"}, \code{"total"}.
#' @param test Choix du test statistique : \code{"auto"}, \code{"chisq"}, \code{"fisher"}.
#' @param digits Nombre de décimales pour les pourcentages.
#' @param color Couleur de fond pour les titres de variables (par défaut : transparent).
#' @param include_na Inclure les valeurs manquantes ? (\code{TRUE} ou \code{FALSE}).
#'
#' @return Un objet \code{analytix_table} contenant \code{$data} et \code{$flextable}.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom flextable as_grouped_data as_flextable set_caption set_header_labels add_footer_lines theme_vanilla bold bg italic fontsize align autofit
#' @importFrom rlang enquo enquos quo_name
#' @export
tbl_cross_unique <- function(data, target, ...,
                                 target_name = NULL,
                                 outcome_of_interest = NULL,
                                 pct = c("row", "col", "total"),
                                 test = c("auto", "chisq", "fisher"),
                                 digits = 1,
                                 color = "transparent",
                                 include_na = FALSE) {

  # --- Dépendances ---
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")

  pct <- match.arg(pct)
  test <- match.arg(test)

  # --- Extraction des variables ---
  target_enq <- rlang::enquo(target)
  target_nm  <- .resolve_var_name(data, target_enq)

  vars_enq <- rlang::enquos(...)
  if (length(vars_enq) == 0) {
    stop("Aucune variable en ligne fournie dans '...'. Usage : tbl_cross_unique(data, target, var1, var2, ...)")
  }

  if (is.null(target_name)) {
    target_name <- .get_label(data, target_nm, target_nm)
  }

  if (!target_nm %in% names(data)) {
    stop("La variable cible '", target_nm, "' n'existe pas dans le jeu de données.")
  }

  y_full   <- data[[target_nm]]
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
  has_small_counts <- FALSE

  for (v_enq in vars_enq) {
    v_name   <- .resolve_var_name(data, v_enq)
    if (!v_name %in% names(data)) next
    v_label  <- .get_label(data, v_name, v_name)
    x_full   <- data[[v_name]]

    # Inclure ou non les NA
    use_na_arg <- if (include_na) "ifany" else "no"
    tab_full <- table(x_full, y_full, useNA = use_na_arg)

    if (!outcome_of_interest %in% colnames(tab_full)) {
      stop("La modalité '", outcome_of_interest, "' n'est pas dans les colonnes du tableau pour '", v_name, "'.")
    }

    # --- Test global ---
    exp_counts <- suppressWarnings(tryCatch(chisq.test(tab_full)$expected, error = function(e) NULL))
    use_fisher_global <- FALSE
    if (test == "auto") {
      if (is.null(exp_counts) || any(exp_counts < 5, na.rm = TRUE)) {
        use_fisher_global <- TRUE
        has_small_counts <- TRUE
      }
    } else if (test == "fisher") {
      use_fisher_global <- TRUE
    }

    if (use_fisher_global) {
      res_global <- tryCatch(suppressWarnings(fisher.test(tab_full, simulate.p.value = (nrow(tab_full) > 2 || ncol(tab_full) > 2))), error = function(e) list(p.value = NA))
    } else {
      res_global <- tryCatch(suppressWarnings(chisq.test(tab_full, correct = FALSE)), error = function(e) list(p.value = NA))
    }

    p_glob_val <- res_global$p.value
    p_glob_str <- if (is.na(p_glob_val)) {
      "-"
    } else if (p_glob_val < 0.001) {
      "<0,001"
    } else {
      format(round(p_glob_val, 3), decimal.mark = ",")
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
        if (any(sub_tab < 5, na.rm = TRUE)) {
          use_fisher <- TRUE
          has_small_counts <- TRUE
        }
      } else if (test == "fisher") {
        use_fisher <- TRUE
      }

      if (use_fisher) {
        res_test <- tryCatch(suppressWarnings(fisher.test(sub_tab)), error = function(e) list(p.value = NA))
      } else {
        res_test <- tryCatch(suppressWarnings(chisq.test(sub_tab, correct = FALSE)), error = function(e) list(p.value = NA))
      }

      p_val <- res_test$p.value
      p_str <- if (is.na(p_val)) {
        "-"
      } else if (p_val < 0.001) {
        "<0,001"
      } else {
        format(round(p_val, 3), decimal.mark = ",")
      }
      p_vals[mod] <- p_str
    }

    # Construction du bloc de cellules
    df_n   <- as.data.frame(tab_mat)   %>% setNames(c("Modalité", "Target", "n"))
    df_pct <- as.data.frame(pct_mat) %>% setNames(c("Modalité", "Target", "pct"))

    block <- merge(df_n, df_pct) %>%
      dplyr::mutate(
        cellule = sprintf(
          "%s (%s%%)",
          n,
          format(round(pct, digits), nsmall = digits, decimal.mark = ",")
        )
      ) %>%
      dplyr::select(Modalité, Target, cellule) %>%
      tidyr::pivot_wider(names_from = Target, values_from = cellule)

    # Ajout des p-values
    stats_df <- data.frame(
      Modalité = names(p_vals),
      p_value = as.character(p_vals),
      p_global = p_glob_str,
      stringsAsFactors = FALSE
    )

    block <- merge(block, stats_df, by = "Modalité", sort = FALSE)
    block <- cbind(Variable = v_label, block)
    all_results[[v_name]] <- block
  }

  if (length(all_results) == 0) {
    stop("Aucune variable en ligne valide trouvée dans le jeu de données.")
  }

  final_df <- do.call(rbind, all_results)

  # --- Note dynamique ---
  note_parts <- c(
    paste0("n (%) ; % = pourcentage (", pct, ")"),
    "p-value : test d'indépendance modalité vs reste",
    "p global : test d'indépendance global de la variable (Fisher / χ²)"
  )
  if (has_small_counts) {
    note_parts <- c(note_parts, "effectifs théoriques < 5 détectés (test exact de Fisher appliqué)")
  }

  note_text <- paste0("Notes : ", paste(note_parts, collapse = " ; "), ".")

  # --- Titre ---
  caption_text <- paste0("Analyse multi-variables contre : ", target_name,
                         if (!is.null(outcome_of_interest)) paste0(" – Issue d’intérêt : ", outcome_of_interest))

  # --- Flextable ---
  ft <- as_grouped_data(final_df, groups = "Variable") %>%
    as_flextable() %>%
    set_caption(caption_text) %>%
    set_header_labels(Modalité = "Modalités", p_value = "p (modalité)", p_global = "p global") %>%
    add_footer_lines(note_text) %>%
    theme_vanilla() %>%
    bold(i = ~ !is.na(Variable), bold = TRUE) %>%
    bg(i = ~ !is.na(Variable), bg = color) %>%
    italic(part = "footer") %>%
    fontsize(size = 9, part = "footer") %>%
    align(align = "center", part = "all") %>%
    autofit()

  as_analytix_table(data = final_df, flextable = ft)
}
