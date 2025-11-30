#' @title Tableau bivarié unifié : plusieurs variables vs une variable dépendante
#' @description
#' Génère un tableau synthétique unique croisant une variable dépendante catégorielle
#' avec plusieurs variables explicatives. Deux formats disponibles :
#' \itemize{
#'   \item{\strong{Format hiérarchique} (`tidy_layout = FALSE`, par défaut) :}
#'     titres de variable en gras, modalités indentées, p-value alignée à droite.
#'   \item{\strong{Format tidy} (`tidy_layout = TRUE`) :}
#'     une colonne "Variable", une colonne "Modalité", une ligne par modalité —
#'     idéal pour l'export ou les données structurées.
#' }
#'
#' Les pourcentages sont **conditionnels à la colonne** (i.e. par modalité de la variable dépendante).
#' Le test d'association (khi² ou Fisher) est automatique.
#'
#' @param data data.frame
#' @param outcome Variable dépendante (symbole non évalué, ex: SARM)
#' @param predictors Vecteur de **chaînes** (noms de colonnes, ex: c("Sexe", "Service"))
#' @param include_na Inclure les NA dans les tableaux ? (défaut = FALSE)
#' @param digits Décimales pour les pourcentages (défaut = 1)
#' @param color Couleur d'en-tête (défaut = "#D3D3D3")
#' @param tidy_layout Si TRUE, utilise le format "tidy" avec colonnes "Variable" et "Modalité"
#'                    (défaut = FALSE)
#'
#' @return Objet `flextable` avec classe `"analytix_table"`
#'
#' @examples
#' # Exemple reproductible avec mtcars (inclus dans R)
#' mtcars2 <- transform(mtcars,
#'   am = factor(am, labels = c("Manuelle", "Automatique")),
#'   cyl = factor(cyl),
#'   vs = factor(vs, labels = c("V", "Ligne"))
#' )
#'
#' # Format hiérarchique (par défaut)
#' \dontrun{
#' cross_multi(mtcars2, am, c("cyl", "vs"))
#' }
#'
#' # Format tidy (export-friendly)
#' \dontrun{
#' cross_multi(mtcars2, am, c("cyl", "vs"), tidy_layout = TRUE)
#' }
#'
#' @export
cross_multi <- function(data,
                        outcome,
                        predictors,
                        include_na = FALSE,
                        digits = 1,
                        color = "#D3D3D3",
                        tidy_layout = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")

  if (!is.character(predictors) || length(predictors) == 0) {
    stop("L'argument 'predictors' doit être un vecteur de chaînes.")
  }

  missing_vars <- predictors[!predictors %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Colonnes manquantes : ", paste(missing_vars, collapse = ", "))
  }

  outcome_enq <- rlang::enquo(outcome)
  outcome_name <- rlang::quo_name(outcome_enq)
  y <- dplyr::pull(data, !!outcome_enq)

  # --- Modalités de y (variable dépendante) ---
  use_na_arg <- if (include_na) "ifany" else "no"
  y_tab_full <- table(y, useNA = use_na_arg)
  y_levels <- names(y_tab_full)
  n_per_level <- as.numeric(y_tab_full)

  if (length(y_levels) == 0) stop("La variable dépendante n'a aucune modalité valide.")

  # Libellés des colonnes de y
  col_labels_y <- mapply(function(val, n) {
    val_lab <- if (is.na(val)) "<NA>" else as.character(val)
    sprintf("%s (n = %s)", val_lab, n)
  }, names(y_tab_full), n_per_level, SIMPLIFY = TRUE)

  # ==============================
  # FORMAT TIDY
  # ==============================
  if (tidy_layout) {
    tidy_rows <- list()

    for (var_name in predictors) {
      x_raw <- data[[var_name]]

      # Modalités de x
      x_tab <- table(x_raw, useNA = use_na_arg)
      x_levels <- names(x_tab)
      if (length(x_levels) == 0) next

      # Tableau croisé complet
      tab <- table(
        x = factor(x_raw, levels = x_levels),
        y = factor(y, levels = y_levels),
        useNA = "no"
      )
      pct_mat <- prop.table(tab, margin = 2) * 100

      # Test
      test_res <- test_association_internal_multi(y, x_raw, "auto")
      p_val_str <- if (is.na(test_res$p.value)) "NA" else if (test_res$p.value < 0.001) "<0,001" else format(round(test_res$p.value, 3), decimal.mark = ",")

      # Une ligne par modalité
      for (i in seq_len(nrow(tab))) {
        mod_label <- rownames(tab)[i]
        mod_label <- if (is.na(mod_label)) "<NA>" else as.character(mod_label)

        # Créer cellules
        cells <- character(length(y_levels))
        for (j in seq_along(y_levels)) {
          n_val <- tab[i, j]
          p_val <- pct_mat[i, j]
          cells[j] <- sprintf(
            "%s (%s%%)",
            n_val,
            format(round(p_val, digits), nsmall = digits, decimal.mark = ",")
          )
        }

        row_vec <- c(
          Variable = var_name,
          Modalite = mod_label,
          cells,
          `p-value` = p_val_str
        )
        tidy_rows[[length(tidy_rows) + 1]] <- row_vec
      }
    }

    if (length(tidy_rows) == 0) stop("Aucune donnée à afficher.")

    # Assembler
    df_out <- as.data.frame(do.call(rbind, tidy_rows), stringsAsFactors = FALSE)
    colnames_y_with_labels <- setNames(y_levels, col_labels_y)
    names(df_out)[3:(2 + length(y_levels))] <- col_labels_y

    # ==============================
    # FORMAT HIÉRARCHIQUE (par défaut)
    # ==============================
  } else {
    internal_col_names <- c("Variable", y_levels, "p_value")
    result_rows <- list()

    for (var_name in predictors) {
      x_raw <- data[[var_name]]

      x_tab <- table(x_raw, useNA = use_na_arg)
      x_levels <- names(x_tab)
      if (length(x_levels) == 0) next

      tab <- table(
        x = factor(x_raw, levels = x_levels),
        y = factor(y, levels = y_levels),
        useNA = "no"
      )
      pct_mat <- prop.table(tab, margin = 2) * 100

      test_res <- test_association_internal_multi(y, x_raw, "auto")
      p_val_str <- if (is.na(test_res$p.value)) "NA" else if (test_res$p.value < 0.001) "<0,001" else format(round(test_res$p.value, 3), decimal.mark = ",")

      # Ligne de titre
      header_row <- rep("", length(internal_col_names))
      names(header_row) <- internal_col_names
      header_row["Variable"] <- var_name
      header_row["p_value"] <- p_val_str
      result_rows[[length(result_rows) + 1]] <- header_row

      # Lignes de modalités
      for (i in seq_len(nrow(tab))) {
        mod_row <- rep("", length(internal_col_names))
        names(mod_row) <- internal_col_names
        mod_label <- rownames(tab)[i]
        mod_label <- if (is.na(mod_label)) "<NA>" else as.character(mod_label)
        mod_row["Variable"] <- paste("  ", mod_label)
        for (j in seq_along(y_levels)) {
          n_val <- tab[i, j]
          p_val <- pct_mat[i, j]
          mod_row[y_levels[j]] <- sprintf(
            "%s (%s%%)",
            n_val,
            format(round(p_val, digits), nsmall = digits, decimal.mark = ",")
          )
        }
        result_rows[[length(result_rows) + 1]] <- mod_row
      }
    }

    if (length(result_rows) == 0) stop("Aucune donnée à afficher.")

    df_list <- lapply(result_rows, function(r) as.data.frame(t(r), stringsAsFactors = FALSE))
    df_out <- dplyr::bind_rows(df_list)
    names(df_out) <- c("Variable", col_labels_y, "p-value")
  }

  # ==============================
  # GÉNÉRATION DU FLEXTABLE
  # ==============================
  ft <- flextable::flextable(df_out) %>%
    flextable::set_caption(sprintf("Associations bivariées avec %s", outcome_name)) %>%
    theme_analytique(color = color)

  if (!tidy_layout) {
    # En format hiérarchique, mettre en gras les titres (non indentés)
    is_header <- !startsWith(df_out$Variable, "  ")
    ft <- flextable::bold(ft, i = which(is_header), bold = TRUE)
  }

  class(ft) <- c("analytix_table", class(ft))
  return(ft)
}


# --- Fonction interne de test (inchangée) ---
test_association_internal_multi <- function(x, y, test = "auto") {
  tab <- table(x, y, useNA = "no")
  if (sum(tab) == 0) return(list(p.value = NA, test = "vide", warning = NULL))
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  expected <- chi$expected
  low5 <- sum(expected < 5)
  prop_low <- low5 / length(expected)
  is_2x2 <- all(dim(tab) == c(2, 2))
  p_val <- NA_real_; test_name <- ""; warning_msg <- NULL
  if (test == "chisq") {
    p_val <- chi$p.value; test_name <- "Khi²"
    if (prop_low > 0.20) warning_msg <- "Conditions du khi² non respectées"
  } else if (test == "fisher") {
    ft <- if (is_2x2) fisher.test(tab) else fisher.test(tab, simulate.p.value = TRUE, B = 2000)
    p_val <- ft$p.value; test_name <- if (is_2x2) "Fisher exact" else "Fisher simulé"
  } else {
    if (is_2x2) {
      if (sum(tab) < 20 || any(tab < 5)) {
        ft <- fisher.test(tab); p_val <- ft$p.value; test_name <- "Fisher exact"
      } else { p_val <- chi$p.value; test_name <- "Khi²" }
    } else {
      if (prop_low > 0.20) {
        ft <- fisher.test(tab, simulate.p.value = TRUE, B = 2000)
        p_val <- ft$p.value; test_name <- "Fisher simulé"
        warning_msg <- "Conditions du khi² non respectées"
      } else { p_val <- chi$p.value; test_name <- "Khi²"
      }
    }
  }
  list(p.value = p_val, test = test_name, warning = warning_msg)
}
