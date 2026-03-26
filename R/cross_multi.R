#' Tableaux croisés épidémiologiques pour plusieurs variables prédictives
#'
#' Génère un tableau hiérarchique présentant les effectifs, pourcentages (conditionnels à la colonne),
#' les Odds Ratios (OR) bruts avec intervalles de confiance à 95\%, et les p-values par modalité.
#'
#' \strong{Caractéristiques principales :}
#' \itemize{
#'   \item Les pourcentages sont calculés par colonne (selon la variable dépendante).
#'   \item Le test d'association global (Khi² ou Fisher) est calculé en interne mais \strong{n'est pas affiché} dans le tableau final.
#'   \item Seules les p-values des comparaisons spécifiques (Modalité vs Référence) apparaissent.
#'   \item Formatage automatique avec virgule décimale et crochets pour les IC.
#' }
#'
#' @param data Un \code{data.frame}.
#' @param outcome Variable dépendante (binaire de préférence). Symbole non évalué (ex: \code{SARM}).
#' @param predictors Vecteur de chaînes de caractères contenant les noms des colonnes indépendantes.
#' @param include_na Logique. Inclure les valeurs manquantes (NA) comme une catégorie ? Défaut: \code{FALSE}.
#' @param digits Entier. Nombre de décimales pour les pourcentages. Défaut: \code{1}.
#' @param color Caractère. Couleur de fond pour l'en-tête du tableau flextable. Défaut: \code{"#D3D3D3"}.
#' @param tidy_layout Logique. Si \code{TRUE}, retourne un format "long" (tidy). Défaut: \code{FALSE}.
#' @param method Caractère. Méthode de calcul des OR : \code{"logistic"} ou \code{"level"}. Défaut: \code{"level"}.
#' @param ref_levels Liste nommée définissant les niveaux de référence.
#'
#' @return Un objet \code{flextable} avec la classe additionnelle \code{"analytix_table"}.
#'
#' @section Exemples:
#' \preformatted{
#' # Charger les données exemple (mtcars intégré à R)
#' data(mtcars)
#'
#' # Préparation des données
#' mtcars2 <- transform(mtcars,
#'   am = factor(am, labels = c("Manuelle", "Automatique")),
#'   cyl = factor(cyl, labels = c("4 cyl", "6 cyl", "8 cyl")),
#'   vs = factor(vs, labels = c("V-Engine", "Ligne"))
#' )
#'
#' # 1. Format Hiérarchique (Défaut)
#' # Tableau classique avec variables en gras et modalités indentées
#' # (Ne pas exécuter automatiquement lors du check CRAN si flextable interactif)
#' \dontrun{
#' cross_multi(mtcars2, am, c("cyl", "vs"))
#' }
#'
#' # 2. Format Tidy (Pour export ou manipulation ultérieure)
#' \dontrun{
#' cross_multi(mtcars2, am, c("cyl", "vs"), tidy_layout = TRUE)
#' }
#'
#' # 3. Avec définition des références personnalisées
#' \dontrun{
#' cross_multi(mtcars2, am, c("cyl", "vs"),
#'             ref_levels = list(cyl = "8 cyl", vs = "Ligne"))
#' }
#' }
#'
#' @importFrom rlang enquo quo_name :=
#' @importFrom dplyr bind_rows pull
#' @importFrom flextable flextable set_caption theme_zebra bold align bg
#' @importFrom stats fisher.test chisq.test qnorm relevel setNames na.omit
#' @export
cross_multi <- function(data,
                        outcome,
                        predictors,
                        include_na = FALSE,
                        digits = 1,
                        color = "#D3D3D3",
                        tidy_layout = TRUE,
                        method = c("logistic", "level"),
                        ref_levels = NULL) {

  method <- match.arg(method)

  # Vérification des dépendances (sans stopper si installé, juste warning si manquant)
  deps <- c("dplyr", "rlang", "flextable", "tidyr")
  for (pkg in deps) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Le package '%s' est requis pour cette fonction.", pkg))
    }
  }

  if (!is.character(predictors) || length(predictors) == 0) {
    stop("L'argument 'predictors' doit être un vecteur de chaînes non vide.")
  }

  missing_vars <- predictors[!predictors %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Colonnes manquantes dans les données : ", paste(missing_vars, collapse = ", "))
  }

  outcome_enq <- rlang::enquo(outcome)
  outcome_name <- rlang::quo_name(outcome_enq)
  y <- dplyr::pull(data, !!outcome_enq)

  # --- Gestion de la variable dépendante (Y) ---
  use_na_arg <- if (include_na) "ifany" else "no"
  y_tab_full <- base::table(y, useNA = use_na_arg)
  y_levels <- names(y_tab_full)
  n_per_level <- as.numeric(y_tab_full)

  if (length(y_levels) == 0) stop("La variable dépendante n'a aucune modalité valide.")

  # Libellés des colonnes de Y
  col_labels_y <- mapply(function(val, n) {
    val_lab <- if (is.na(val)) "<NA>" else as.character(val)
    sprintf("%s (n=%s)", val_lab, n)
  }, names(y_tab_full), n_per_level, SIMPLIFY = TRUE)

  # Helper: Déterminer le niveau de référence
  get_ref_for_var <- function(var_name, x_levels) {
    if (!is.null(ref_levels)) {
      if (!is.null(names(ref_levels)) && var_name %in% names(ref_levels)) {
        return(as.character(ref_levels[[var_name]]))
      }
      if (is.null(names(ref_levels)) && length(ref_levels) == length(predictors)) {
        idx <- which(predictors == var_name)
        return(as.character(ref_levels[idx]))
      }
    }
    return(x_levels[1])
  }

  # Helpers de formatage
  fmt_num <- function(x) format(round(x, 2), nsmall = 2, decimal.mark = ",")
  fmt_pct <- function(x) format(round(x, digits), nsmall = digits, decimal.mark = ",")
  fmt_p <- function(p) {
    if (is.na(p)) return("")
    if (p >= 1) return("1")
    if (p < 0.001) return("< 0,001")
    format(round(p, 3), decimal.mark = ",")
  }

  z <- stats::qnorm(0.975)

  # ==============================
  # FORMAT TIDY
  # ==============================
  if (tidy_layout) {
    tidy_rows <- list()

    for (var_name in predictors) {
      x_raw <- data[[var_name]]
      x_tab <- base::table(x_raw, useNA = use_na_arg)
      x_levels <- names(x_tab)
      if (length(x_levels) == 0) next

      tab <- base::table(
        x = factor(x_raw, levels = x_levels),
        y = factor(y, levels = y_levels),
        useNA = "no"
      )
      pct_mat <- prop.table(tab, margin = 2) * 100

      or_per_level <- stats::setNames(rep("", length(x_levels)), x_levels)
      pval_per_level <- stats::setNames(rep("", length(x_levels)), x_levels)

      if (length(y_levels) == 2) {
        ref <- get_ref_for_var(var_name, x_levels)
        if (!ref %in% x_levels) {
          warning(sprintf("Référence '%s' introuvable pour %s. Utilisation de '%s'.", ref, var_name, x_levels[1]))
          ref <- x_levels[1]
        }

        or_per_level[ref] <- "Réf."

        if (method == "logistic") {
          y_bin <- as.integer(factor(y, levels = y_levels) == y_levels[2])
          fac <- stats::relevel(factor(x_raw, levels = x_levels), ref)
          fit <- try(stats::glm(y_bin ~ fac, family = stats::binomial()), silent = TRUE)

          if (!inherits(fit, "try-error")) {
            coefs <- summary(fit)$coefficients
            for (lvl in x_levels) {
              if (lvl == ref) next
              row_idx <- grep(paste0(lvl, "$"), rownames(coefs))
              if (length(row_idx) == 1) {
                est <- coefs[row_idx, 1]; se <- coefs[row_idx, 2]; pval <- coefs[row_idx, 4]
                or_val <- exp(est); ci <- exp(est + c(-1, 1) * z * se)
                or_per_level[lvl] <- sprintf("%s [%s – %s]", fmt_num(or_val), fmt_num(ci[1]), fmt_num(ci[2]))
                pval_per_level[lvl] <- fmt_p(pval)
              }
            }
          }
        } else if (method == "level") {
          for (lvl in x_levels) {
            if (lvl == ref) next
            a <- sum(x_raw == lvl & y == y_levels[1], na.rm = TRUE)
            b <- sum(x_raw == lvl & y == y_levels[2], na.rm = TRUE)
            c <- sum(x_raw == ref & y == y_levels[1], na.rm = TRUE)
            d <- sum(x_raw == ref & y == y_levels[2], na.rm = TRUE)

            if (a + b + c + d == 0) next
            a0 <- a; b0 <- b; c0 <- c; d0 <- d
            if (any(c(a0, b0, c0, d0) == 0)) { a0 <- a0 + 0.5; b0 <- b0 + 0.5; c0 <- c0 + 0.5; d0 <- d0 + 0.5 }

            or_val <- (a0 * d0) / (b0 * c0)
            log_or <- log(or_val)
            se_log_or <- sqrt(1/a0 + 1/b0 + 1/c0 + 1/d0)
            ci <- exp(log_or + c(-1, 1) * z * se_log_or)

            pval <- tryCatch(stats::fisher.test(matrix(c(a, b, c, d), nrow = 2, byrow = TRUE))$p.value, error = function(e) NA_real_)

            or_per_level[lvl] <- sprintf("%s [%s – %s]", fmt_num(or_val), fmt_num(ci[1]), fmt_num(ci[2]))
            pval_per_level[lvl] <- fmt_p(pval)
          }
        }
      }

      for (i in seq_len(nrow(tab))) {
        mod_label <- rownames(tab)[i]
        mod_clean <- if (is.na(mod_label)) "<NA>" else as.character(mod_label)

        cells <- character(length(y_levels))
        for (j in seq_along(y_levels)) {
          n_val <- tab[i, j]
          p_val <- pct_mat[i, j]
          cells[j] <- sprintf("%s (%s%%)", n_val, fmt_pct(p_val))
        }

        row_vec <- c(
          Variable = var_name,
          Modalite = mod_clean,
          cells,
          `p-value` = if(mod_clean %in% names(pval_per_level)) pval_per_level[mod_clean] else "",
          OR = if(mod_clean %in% names(or_per_level)) or_per_level[mod_clean] else ""
        )
        tidy_rows[[length(tidy_rows) + 1]] <- row_vec
      }
    }

    if (length(tidy_rows) == 0) stop("Aucune donnée à afficher.")
    df_out <- as.data.frame(do.call(rbind, tidy_rows), stringsAsFactors = FALSE)
    names(df_out)[3:(2 + length(y_levels))] <- col_labels_y

  } else {
    # ==============================
    # FORMAT HIÉRARCHIQUE

    # ==============================
    # On utilise directement les noms finaux désirés pour éviter les erreurs de mapping
    result_rows <- list()
    
    for (var_name in predictors) {
      x_raw <- data[[var_name]]
      x_tab <- table(x_raw, useNA = use_na_arg)
      x_levels <- names(x_tab)
      if (length(x_levels) == 0) next
      
      n_var <- sum(x_tab, na.rm = TRUE)
      
      tab <- table(
        x = factor(x_raw, levels = x_levels),
        y = factor(y, levels = y_levels),
        useNA = "no"
      )
      pct_mat <- prop.table(tab, margin = 2) * 100
      
      # Initialisation des vecteurs de résultats
      or_per_level <- setNames(rep("", length(x_levels)), x_levels)
      pval_per_level <- setNames(rep("", length(x_levels)), x_levels)
      
      # Calculs uniquement si Y est binaire
      if (length(y_levels) == 2) {
        ref <- get_ref_for_var(var_name, x_levels)
        if (!ref %in% x_levels) {
          warning(sprintf("Référence '%s' non trouvée pour %s — utilisation de '%s'", ref, var_name, x_levels[1]))
          ref <- x_levels[1]
        }
        
        or_per_level[ref] <- "Réf." # Marqueur explicite
        
        if (method == "logistic") {
          y_bin <- as.integer(factor(y, levels = y_levels) == y_levels[2])
          fac <- relevel(factor(x_raw, levels = x_levels), ref)
          fit <- try(stats::glm(y_bin ~ fac, family = stats::binomial()), silent = TRUE)
          
          if (!inherits(fit, "try-error")) {
            coefs <- summary(fit)$coefficients
            for (lvl in x_levels) {
              if (lvl == ref) next
              row_idx <- grep(paste0(lvl, "$"), rownames(coefs))
              if (length(row_idx) == 1) {
                est <- coefs[row_idx, 1]; se <- coefs[row_idx, 2]; pval <- coefs[row_idx, 4]
                or_val <- exp(est); ci <- exp(est + c(-1, 1) * z * se)
                or_per_level[lvl] <- sprintf("%s [%s – %s]", fmt_num(or_val), fmt_num(ci[1]), fmt_num(ci[2]))
                pval_per_level[lvl] <- fmt_p(pval)
              }
            }
          }
        } else if (method == "level") {
          for (lvl in x_levels) {
            if (lvl == ref) next
            a <- sum(x_raw == lvl & y == y_levels[1], na.rm = TRUE)
            b <- sum(x_raw == lvl & y == y_levels[2], na.rm = TRUE)
            c <- sum(x_raw == ref & y == y_levels[1], na.rm = TRUE)
            d <- sum(x_raw == ref & y == y_levels[2], na.rm = TRUE)
            
            if (a + b + c + d == 0) next
            
            a0 <- a; b0 <- b; c0 <- c; d0 <- d
            if (any(c(a0, b0, c0, d0) == 0)) { a0 <- a0 + 0.5; b0 <- b0 + 0.5; c0 <- c0 + 0.5; d0 <- d0 + 0.5 }
            
            or_val <- (a0 * d0) / (b0 * c0)
            log_or <- log(or_val)
            se_log_or <- sqrt(1/a0 + 1/b0 + 1/c0 + 1/d0)
            ci <- exp(log_or + c(-1, 1) * z * se_log_or)
            
            pval <- tryCatch(stats::fisher.test(matrix(c(a, b, c, d), nrow = 2, byrow = TRUE))$p.value, error = function(e) NA_real_)
            
            or_per_level[lvl] <- sprintf("%s [%s – %s]", fmt_num(or_val), fmt_num(ci[1]), fmt_num(ci[2]))
            pval_per_level[lvl] <- fmt_p(pval)
          }
        }
      }
      
      # 1. Ligne Titre (Variable + N)
      # IMPORTANT: On met "" pour p-value et OR car on ne veut pas la p-value globale ici
      header_row <- list(
        Variable = sprintf("%s (n=%s)", var_name, n_var),
        `OR brute (IC 95%)` = "",
        `p-value` = "" 
      )
      # Ajout dynamique des colonnes de résultats (BLSE-, BLSE+) vides
      for (col_lbl in col_labels_y) {
        header_row[[col_lbl]] <- ""
      }
      # Réordonner pour que Variable soit en premier, puis les cols Y, puis OR, puis p-value
      # L'ordre final sera fixé après le bind_rows, ici on s'assure juste que les clés existent
      
      result_rows[[length(result_rows) + 1]] <- header_row
      
      # 2. Lignes Modalités
      for (i in seq_len(nrow(tab))) {
        mod_label <- rownames(tab)[i]
        mod_clean <- if (is.na(mod_label)) "<NA>" else as.character(mod_label)
        row_data <- list(Variable = paste("  ", mod_clean))
        
        # Cellules de comptage (%)
        for (j in seq_along(y_levels)) {
          n_val <- tab[i, j]
          p_val <- pct_mat[i, j]
          lbl <- col_labels_y[j]
          row_data[[lbl]] <- sprintf("%s (%s%%)", n_val, fmt_pct(p_val))
        }
        
        # Assignation OR et p-value (clé "p-value" avec tiret)
        val_or <- or_per_level[mod_clean]
        val_p <- pval_per_level[mod_clean]
        
        # Sécurité si NA
        if (is.na(val_or)) val_or <- ""
        if (is.na(val_p)) val_p <- ""
        
        row_data[["OR brute (IC 95%)"]] <- val_or
        row_data[["p-value"]] <- val_p
        
        result_rows[[length(result_rows) + 1]] <- row_data
      }
    }
    
    if (length(result_rows) == 0) stop("Aucune donnée à afficher.")
    
    # Conversion en dataframe
    df_list <- lapply(result_rows, function(r) {
      # S'assurer que toutes les colonnes attendues sont présentes même si vides
      full_row <- as.data.frame(t(unlist(r)), stringsAsFactors = FALSE)
      return(full_row)
    })
    
    df_out <- dplyr::bind_rows(df_list)
    
    # Définition stricte de l'ordre des colonnes
    final_cols <- c("Variable", col_labels_y, "OR brute (IC 95%)", "p-value")
    
    # Vérification que toutes les colonnes existent (sinon création de colonnes vides)
    for (col in final_cols) {
      if (!col %in% names(df_out)) {
        df_out[[col]] <- ""
      }
    }
    
    df_out <- df_out[, final_cols, drop = FALSE]
  }

  # ==============================
  # GÉNÉRATION FLEXTABLE
  # ==============================

  ft <- flextable::flextable(df_out) %>%
    flextable::set_caption(sprintf("Associations bivariées avec %s", outcome_name)) %>%
    flextable::theme_zebra() # Remplacement de theme_default

  if (!tidy_layout) {
    is_header <- !startsWith(df_out$Variable, "  ")
    if (any(is_header)) {
      ft <- flextable::bold(ft, i = which(is_header), part = "body")
    }
  }

  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")

  class(ft) <- c("analytix_table", class(ft))
  return(ft)
}

# --- Fonction interne de test ---
test_association_internal_multi <- function(x, y, test = "auto") {
  tab <- base::table(x, y, useNA = "no")
  if (sum(tab) == 0) return(list(p.value = NA, test = "vide", warning = NULL))

  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  expected <- chi$expected
  low5 <- sum(expected < 5)
  prop_low <- low5 / length(expected)
  is_2x2 <- all(dim(tab) == c(2, 2))

  p_val <- NA_real_; test_name <- ""; warning_msg <- NULL

  if (test == "chisq") {
    p_val <- chi$p.value; test_name <- "Khi²"
    if (prop_low > 0.20) warning_msg <- "Conditions du khi² non respectées"
  } else if (test == "fisher") {
    ft_res <- if (is_2x2) stats::fisher.test(tab) else stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)
    p_val <- ft_res$p.value; test_name <- if (is_2x2) "Fisher exact" else "Fisher simulé"
  } else {
    if (is_2x2) {
      if (sum(tab) < 20 || any(tab < 5)) {
        ft_res <- stats::fisher.test(tab); p_val <- ft_res$p.value; test_name <- "Fisher exact"
      } else { p_val <- chi$p.value; test_name <- "Khi²" }
    } else {
      if (prop_low > 0.20) {
        ft_res <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)
        p_val <- ft_res$p.value; test_name <- "Fisher simulé"
        warning_msg <- "Conditions du khi² non respectées"
      } else { p_val <- chi$p.value; test_name <- "Khi²" }
    }
  }
  list(p.value = p_val, test = test_name, warning = warning_msg)
}