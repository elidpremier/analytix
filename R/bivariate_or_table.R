#' @title Tableau synthétique d'Odds Ratios (Régression logistique bivariée)
#' @description Génère un tableau synthétique contenant les effectifs, pourcentages, Odds Ratios (OR bruts),
#' leurs intervalles de confiance à 95% et les p-values pour plusieurs variables explicatives face à une variable dépendant binaire.
#' 
#' @param data data.frame
#' @param outcome Variable dépendante binaire.
#' @param exposures Liste/vecteur des noms des variables explicatives (catégorielles ou binaires).
#' @param outcome_positive_val Valeur représentant l'événement positif dans la variable dépendant (ex: "Oui", 1).
#' @param conf_level Niveau de confiance (défaut: 0.95).
#' @param var_labels Vecteur nommé des libellés pour chaque variable explicative.
#' @param digits Nombre de décimales pour les OR et IC (défaut: 2).
#' @param color Couleur d'en-tête pour le tableau flextable.
#' 
#' @return Un objet `flextable` prêt pour l'export.
#' 
#' @examples
#' df <- data.frame(
#'   reussite = c("Oui", "Non", "Oui", "Oui", "Non", "Non", "Oui", "Non"),
#'   bourse = c("Oui", "Oui", "Non", "Oui", "Non", "Non", "Non", "Oui"),
#'   sexe = c("F", "M", "F", "M", "F", "M", "F", "F")
#' )
#' bivariate_or_table(df, outcome = "reussite", exposures = c("bourse", "sexe"), outcome_positive_val = "Oui")
#' 
#' @export
bivariate_or_table <- function(data, outcome, exposures,
                               outcome_positive_val = NULL,
                               conf_level = 0.95,
                               var_labels = NULL,
                               digits = 2,
                               color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("stats", quietly = TRUE)) stop("stats requis")
  
  outcome_nm <- if (is.character(outcome)) outcome else rlang::as_name(rlang::enquo(outcome))
  
  if (!outcome_nm %in% names(data)) {
    stop(paste("La colonne outcome", outcome_nm, "n'existe pas dans les données."))
  }
  
  out_vec <- data[[outcome_nm]]
  out_clean <- out_vec[!is.na(out_vec)]
  
  if (is.null(outcome_positive_val)) {
    if (is.logical(out_clean)) {
      outcome_positive_val <- TRUE
    } else if (is.numeric(out_clean)) {
      outcome_positive_val <- 1
    } else {
      possibles <- c("Oui", "oui", "1", "Reussite", "Positive", "BLSE", "+", "TRUE")
      match_val <- intersect(unique(out_clean), possibles)
      outcome_positive_val <- if (length(match_val) > 0) match_val[1] else unique(out_clean)[1]
    }
  }
  
  # Variable dépendante binaire 0 / 1 pour glm
  data_mod <- data
  data_mod$outcome_bin <- ifelse(data_mod[[outcome_nm]] == outcome_positive_val, 1, 0)
  
  rows_list <- list()
  
  for (exp_var in exposures) {
    if (!exp_var %in% names(data_mod)) next
    
    var_lab <- if (!is.null(var_labels) && exp_var %in% names(var_labels)) {
      var_labels[[exp_var]]
    } else {
      attr_l <- attr(data_mod[[exp_var]], "label")
      if (!is.null(attr_l)) attr_l else exp_var
    }
    
    exp_vec <- data_mod[[exp_var]]
    if (is.numeric(exp_vec)) {
      exp_vec <- factor(exp_vec)
      data_mod[[exp_var]] <- exp_vec
    }
    
    formula_obj <- stats::as.formula(paste("outcome_bin ~", exp_var))
    fit <- tryCatch(
      stats::glm(formula_obj, data = data_mod, family = stats::binomial()),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    co <- summary(fit)$coefficients
    ci <- tryCatch(suppressMessages(stats::confint(fit, level = conf_level)), error = function(e) NULL)
    
    levels_val <- levels(factor(exp_vec[!is.na(exp_vec)]))
    
    for (i in seq_along(levels_val)) {
      mod <- levels_val[i]
      sub_df <- data_mod[data_mod[[exp_var]] == mod & !is.na(data_mod[[exp_var]]) & !is.na(data_mod$outcome_bin), ]
      n_mod <- nrow(sub_df)
      n_pos <- sum(sub_df$outcome_bin == 1)
      pct_pos <- (n_pos / n_mod) * 100
      
      n_pct_str <- paste0(n_pos, "/", n_mod, " (", format(round(pct_pos, 1), nsmall = 1, decimal.mark = ","), "%)")
      
      if (i == 1) {
        # Modallité de référence
        or_str <- "1.00 (Réf.)"
        p_str <- "-"
      } else {
        coef_row_name <- paste0(exp_var, mod)
        if (coef_row_name %in% rownames(co)) {
          or_val <- exp(co[coef_row_name, "Estimate"])
          p_val <- co[coef_row_name, "Pr(>|z|)"]
          
          if (!is.null(ci) && coef_row_name %in% rownames(ci)) {
            ci_low <- exp(ci[coef_row_name, 1])
            ci_high <- exp(ci[coef_row_name, 2])
          } else {
            se <- co[coef_row_name, "Std. Error"]
            z <- stats::qnorm(1 - (1 - conf_level) / 2)
            ci_low <- exp(co[coef_row_name, "Estimate"] - z * se)
            ci_high <- exp(co[coef_row_name, "Estimate"] + z * se)
          }
          
          or_fmt <- format(round(or_val, digits), nsmall = digits, decimal.mark = ",")
          low_fmt <- format(round(ci_low, digits), nsmall = digits, decimal.mark = ",")
          high_fmt <- format(round(ci_high, digits), nsmall = digits, decimal.mark = ",")
          
          or_str <- paste0(or_fmt, " [", low_fmt, " - ", high_fmt, "]")
          p_str <- if (p_val < 0.001) "< 0,001" else format(round(p_val, 3), decimal.mark = ",")
        } else {
          or_str <- "-"
          p_str <- "-"
        }
      }
      
      rows_list[[length(rows_list) + 1]] <- data.frame(
        Variable = var_lab,
        Modalite = mod,
        Effectif_Pct = n_pct_str,
        OR_IC95 = or_str,
        P_value = p_str,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows_list) == 0) {
    stop("Impossible d'ajuster les modèles de régression bivariés.")
  }
  
  out_df <- dplyr::bind_rows(rows_list)
  
  # Masquer le nom de la variable sur les lignes de modalités secondaires pour un affichage propre
  out_df_display <- out_df
  dup_idx <- duplicated(out_df_display$Variable)
  out_df_display$Variable[dup_idx] <- ""
  
  names(out_df_display) <- c("Variable", "Modalité", paste0("Effectif (", outcome_positive_val, ")"), "OR brut [IC95%]", "p-value")
  
  ft <- flextable::flextable(out_df_display) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste("Association bivariée avec", outcome_nm, "(Événement :", outcome_positive_val, ")"))
  
  ft
}
