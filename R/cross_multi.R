#' Tableaux croisés épidémiologiques pour plusieurs variables prédictives
#'
#' Génère un tableau hiérarchique présentant les effectifs, pourcentages (conditionnels à la colonne),
#' les Odds Ratios (OR) bruts avec intervalles de confiance à 95\%, et les p-values par modalité.
#'
#' @param data Un \code{data.frame}.
#' @param outcome Variable dépendante (binaire de préférence).
#' @param predictors Vecteur de chaînes de caractères contenant les noms des colonnes indépendantes.
#' @param outcome_level La modalité de l'outcome à considérer comme l'événement (ex: "Oui", 1). Si NULL, utilise la 2ème modalité.
#' @param include_na Logique. Inclure les valeurs manquantes (NA) ? Défaut: \code{FALSE}.
#' @param digits Entier. Nombre de décimales pour les pourcentages. Défaut: \code{1}.
#' @param color Caractère. Couleur de fond pour l'en-tête. Défaut: \code{"#D3D3D3"}.
#' @param tidy_layout Logique. Si \code{TRUE}, retourne un format "long". Défaut: \code{FALSE}.
#' @param method Caractère. Méthode : \code{"logistic"} ou \code{"level"}. Défaut: \code{"logistic"}.
#' @param ref_levels Liste nommée définissant les niveaux de référence.
#'
#' @examples
#' # Exemple simple
#' cross_multi(mtcars, am, c("cyl", "vs"))
#' 
#' # Avec spécification de l'outcome d'intérêt
#' mtcars$am_f <- factor(mtcars$am, labels = c("Auto", "Manuelle"))
#' cross_multi(mtcars, am_f, c("cyl", "vs"), outcome_level = "Manuelle")
#'
#' @export
cross_multi <- function(data,
                        outcome,
                        predictors,
                        outcome_level = NULL,
                        include_na = FALSE,
                        digits = 1,
                        color = "#D3D3D3",
                        tidy_layout = FALSE,
                        method = c("logistic", "level"),
                        ref_levels = NULL) {

  method <- match.arg(method)

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")

  outcome_enq <- rlang::enquo(outcome)
  outcome_name <- rlang::quo_name(outcome_enq)
  
  # Label de l'outcome
  attr_outcome_label <- attr(data[[outcome_name]], "label")
  outcome_label <- if (!is.null(attr_outcome_label)) attr_outcome_label else outcome_name

  y <- data[[outcome_name]]
  y_levels <- if (is.factor(y)) levels(y) else sort(unique(na.omit(y)))
  
  if (length(y_levels) < 2) stop("L'outcome doit avoir au moins 2 modalités.")

  if (is.null(outcome_level)) {
    outcome_level <- y_levels[2] # Par défaut la 2ème (souvent "Oui", "1", "Malade")
  }
  
  ref_y <- setdiff(y_levels, outcome_level)[1]
  y_factor <- factor(y, levels = c(ref_y, outcome_level))

  # Dénominateurs par colonne
  y_tab_full <- table(y_factor, useNA = if(include_na) "ifany" else "no")
  col_labels_y <- mapply(function(val, n) {
    sprintf("%s (n=%s)", val, n)
  }, names(y_tab_full), as.numeric(y_tab_full))

  # Helpers
  fmt_num <- function(x) format(round(x, 2), nsmall = 2, decimal.mark = ",")
  fmt_pct <- function(x) format(round(x, digits), nsmall = digits, decimal.mark = ",")
  fmt_p <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("< 0,001")
    format(round(p, 3), decimal.mark = ",")
  }

  z <- stats::qnorm(0.975)
  result_rows <- list()

  for (var_name in predictors) {
    x_raw <- data[[var_name]]
    
    # Label predictor
    attr_x_label <- attr(data[[var_name]], "label")
    x_label <- if (!is.null(attr_x_label)) attr_x_label else var_name

    x_levels <- if (is.factor(x_raw)) levels(x_raw) else sort(unique(na.omit(x_raw)))
    if (include_na && any(is.na(x_raw))) x_levels <- c(x_levels, NA)
    
    if (length(x_levels) == 0) next
    
    tab <- table(factor(x_raw, levels = x_levels, exclude = NULL), y_factor, useNA = "no")
    pct_mat <- prop.table(tab, margin = 2) * 100
    
    ref_x <- if (!is.null(ref_levels[[var_name]])) ref_levels[[var_name]] else x_levels[1]
    
    or_per_level <- setNames(rep("", length(x_levels)), as.character(x_levels))
    pval_per_level <- setNames(rep("", length(x_levels)), as.character(x_levels))
    
    or_per_level[as.character(ref_x)] <- "Réf."

    if (method == "logistic") {
      y_bin <- as.integer(y_factor == outcome_level)
      fac <- stats::relevel(factor(x_raw, levels = x_levels, exclude = NULL), ref = as.character(ref_x))
      fit <- try(stats::glm(y_bin ~ fac, family = stats::binomial()), silent = TRUE)
      
      if (!inherits(fit, "try-error")) {
        sum_fit <- summary(fit)$coefficients
        for (lvl in setdiff(x_levels, ref_x)) {
          lvl_char <- as.character(lvl)
          row_nm <- paste0("fac", lvl_char)
          if (row_nm %in% rownames(sum_fit)) {
            est <- sum_fit[row_nm, 1]; se <- sum_fit[row_nm, 2]
            or_val <- exp(est); ci <- exp(est + c(-1, 1) * z * se)
            or_per_level[lvl_char] <- sprintf("%s [%s – %s]", fmt_num(or_val), fmt_num(ci[1]), fmt_num(ci[2]))
            pval_per_level[lvl_char] <- fmt_p(sum_fit[row_nm, 4])
          }
        }
      }
    } else {
      # Method "level" (Fisher/Matrix)
      for (lvl in setdiff(x_levels, ref_x)) {
        lvl_char <- as.character(lvl)
        m <- matrix(c(tab[lvl_char, outcome_level], tab[lvl_char, ref_y],
                      tab[as.character(ref_x), outcome_level], tab[as.character(ref_x), ref_y]), 
                    nrow = 2, byrow = TRUE)
        
        # Haldane-Anscombe correction if zero
        if (any(m == 0)) m <- m + 0.5
        or_val <- (m[1,1]*m[2,2])/(m[1,2]*m[2,1])
        se_log_or <- sqrt(sum(1/m))
        ci <- exp(log(or_val) + c(-1, 1) * z * se_log_or)
        
        pval <- tryCatch(stats::fisher.test(matrix(c(tab[lvl_char, outcome_level], tab[lvl_char, ref_y],
                                                   tab[as.character(ref_x), outcome_level], tab[as.character(ref_x), ref_y]), 
                                                 nrow = 2))$p.value, error = function(e) NA)
        
        or_per_level[lvl_char] <- sprintf("%s [%s – %s]", fmt_num(or_val), fmt_num(ci[1]), fmt_num(ci[2]))
        pval_per_level[lvl_char] <- fmt_p(pval)
      }
    }

    # Header row for the variable
    header_row <- list(Variable = x_label, `OR brute (IC 95%)` = "", `p-value` = "")
    for (lbl in col_labels_y) header_row[[lbl]] <- ""
    result_rows[[length(result_rows) + 1]] <- header_row

    # Modality rows
    for (lvl in x_levels) {
      lvl_char <- as.character(lvl)
      row_data <- list(Variable = paste0("  ", ifelse(is.na(lvl), "<NA>", lvl_char)))
      for (j in seq_along(col_labels_y)) {
        row_data[[col_labels_y[j]]] <- sprintf("%s (%s%%)", tab[lvl_char, j], fmt_pct(pct_mat[lvl_char, j]))
      }
      row_data[["OR brute (IC 95%)"]] <- or_per_level[lvl_char]
      row_data[["p-value"]] <- pval_per_level[lvl_char]
      result_rows[[length(result_rows) + 1]] <- row_data
    }
  }

  df_out <- dplyr::bind_rows(lapply(result_rows, as.data.frame, stringsAsFactors = FALSE))
  
  if (tidy_layout) return(df_out)

  ft <- flextable::flextable(df_out) %>%
    flextable::set_caption(sprintf("Associations bivariées avec %s (%s)", outcome_label, outcome_level)) %>%
    flextable::theme_zebra() %>%
    flextable::bold(i = ~ !startsWith(Variable, "  "), part = "body") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::bg(i = ~ !startsWith(Variable, "  "), bg = color)
    
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