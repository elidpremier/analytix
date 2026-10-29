#' @title Tableau synthétique d'Odds Ratios (Régression logistique bivariée)
#' @description Génère un tableau synthétique contenant les effectifs, pourcentages, Odds Ratios (OR bruts),
#' leurs intervalles de confiance à 95% et les p-values pour plusieurs variables explicatives face à une variable dépendant binaire.
#'
#' @param data data.frame
#' @param outcome Variable dépendante binaire (symbole ou chaîne de caractères).
#' @param exposures Liste/vecteur des noms des variables explicatives (symboles non quotés ou caractères).
#' @param outcome_positive_val Valeur représentant l'événement positif dans la variable dépendant (ex: "Oui", 1).
#' @param conf_level Niveau de confiance (défaut: 0.95).
#' @param var_labels Vecteur nommé des libellés pour chaque variable explicative.
#' @param digits Nombre de décimales pour les OR et IC (défaut: 2).
#' @param color Couleur d'en-tête pour le tableau flextable (défaut: "transparent").
#'
#' @return Un objet \code{analytix_table} contenant \code{$data} et \code{$flextable}.
#'
#' @examples
#' df <- data.frame(
#'   reussite = c("Oui", "Non", "Oui", "Oui", "Non", "Non", "Oui", "Non"),
#'   bourse = c("Oui", "Oui", "Non", "Oui", "Non", "Non", "Non", "Oui"),
#'   sexe = c("F", "M", "F", "M", "F", "M", "F", "F")
#' )
#' tbl_bivariate_or(df, outcome = "reussite",
#'                  exposures = c("bourse", "sexe"),
#'                  outcome_positive_val = "Oui")
#'
#' @export
tbl_bivariate_or <- function(data, outcome, exposures,
                               outcome_positive_val = NULL,
                               conf_level = 0.95,
                               var_labels = NULL,
                               digits = 2,
                               color = "transparent") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("stats", quietly = TRUE)) stop("stats requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")
  if (length(conf_level) != 1L || !is.numeric(conf_level) ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` doit être strictement compris entre 0 et 1.", call. = FALSE)
  }

  outcome_enq <- rlang::enquo(outcome)
  outcome_expr <- rlang::quo_get_expr(outcome_enq)
  outcome_nm <- if (is.character(outcome_expr)) {
    outcome_expr
  } else {
    rlang::as_name(outcome_enq)
  }

  if (!outcome_nm %in% names(data)) {
    stop(paste("La colonne outcome", outcome_nm, "n'existe pas dans les données."))
  }

  exp_enq <- rlang::enquo(exposures)
  exp_expr <- rlang::quo_get_expr(exp_enq)
  if (is.character(exp_expr)) {
    exposures_nms <- exp_expr
  } else if (rlang::is_symbol(exp_expr)) {
    exposures_nms <- rlang::as_name(exp_enq)
  } else {
    exposures_nms <- tryCatch({
      names(dplyr::select(data, !!exp_enq))
    }, error = function(e) {
      if (rlang::quo_is_call(exp_enq)) {
        vapply(rlang::quo_get_expr(exp_enq)[-1], rlang::as_name, character(1))
      } else {
        rlang::as_name(exp_enq)
      }
    })
  }

  exposures_nms <- intersect(exposures_nms, names(data))
  if (length(exposures_nms) == 0) {
    stop("Aucune variable explicative valide trouvée dans `exposures`.")
  }

  out_vec <- data[[outcome_nm]]
  out_clean <- out_vec[!is.na(out_vec)]
  if (length(unique(out_clean)) != 2L) {
    stop("La variable outcome doit comporter exactement deux modalités non manquantes.",
         call. = FALSE)
  }

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
    if (length(outcome_positive_val) != 1L ||
        is.na(outcome_positive_val) ||
        !any(out_clean == outcome_positive_val)) {
      stop("`outcome_positive_val` doit correspondre à une modalité de `outcome`.",
           call. = FALSE)
    }
  }

  # Variable dépendante binaire 0 / 1 pour glm
  data_mod <- data
  data_mod$outcome_bin <- ifelse(data_mod[[outcome_nm]] == outcome_positive_val, 1, 0)

  rows_list <- list()

  for (exp_var in exposures_nms) {
    if (!exp_var %in% names(data_mod)) next

    var_lab <- if (!is.null(var_labels) && exp_var %in% names(var_labels)) {
      var_labels[[exp_var]]
    } else {
      attr_l <- attr(data_mod[[exp_var]], "label")
      if (!is.null(attr_l)) attr_l else exp_var
    }

    exp_vec <- data_mod[[exp_var]]
    is_continuous <- is.numeric(exp_vec) && length(unique(exp_vec[!is.na(exp_vec)])) > 2L

    formula_obj <- stats::as.formula(paste("outcome_bin ~", exp_var))
    fit <- tryCatch(
      stats::glm(formula_obj, data = data_mod, family = stats::binomial()),
      error = function(e) NULL
    )

    if (is.null(fit)) next

    co <- summary(fit)$coefficients
    ci <- tryCatch(
      stats::confint.default(fit, level = conf_level),
      error = function(e) NULL
    )

    if (is_continuous) {
      estimate_name <- setdiff(names(stats::coef(fit)), "(Intercept)")
      if (length(estimate_name) != 1L) next
      coef_row <- summary(fit)$coefficients[estimate_name, , drop = FALSE]
      or_val <- exp(coef_row[1, "Estimate"])
      if (!is.finite(or_val) || abs(coef_row[1, "Estimate"]) > 30) {
        warning("Séparation parfaite ou estimation instable détectée pour '", exp_var,
                "' ; OR non publié.", call. = FALSE)
        or_str <- "NA"
        p_str <- "NA"
      } else {
        ci <- tryCatch(
          stats::confint.default(fit, level = conf_level),
          error = function(e) NULL
        )
        if (!is.null(ci) && estimate_name %in% rownames(ci)) {
          ci_low <- exp(ci[estimate_name, 1])
          ci_high <- exp(ci[estimate_name, 2])
        } else {
          z <- stats::qnorm(1 - (1 - conf_level) / 2)
          ci_low <- exp(coef_row[1, "Estimate"] - z * coef_row[1, "Std. Error"])
          ci_high <- exp(coef_row[1, "Estimate"] + z * coef_row[1, "Std. Error"])
        }
        or_str <- paste0(.fmt_decimal(or_val, digits), " [",
                         .fmt_decimal(ci_low, digits), " - ",
                         .fmt_decimal(ci_high, digits), "]")
        p_str <- .fmt_pvalue(coef_row[1, "Pr(>|z|)"], 3)
      }
      rows_list[[length(rows_list) + 1]] <- data.frame(
        Variable = var_lab,
        Modalite = "Par unité",
        Effectif_Pct = paste0(sum(!is.na(exp_vec))),
        OR_IC95 = or_str,
        P_value = p_str,
        stringsAsFactors = FALSE
      )
      next
    }

    levels_val <- levels(factor(exp_vec[!is.na(exp_vec)]))

    for (i in seq_along(levels_val)) {
      mod <- levels_val[i]
      sub_df <- data_mod[data_mod[[exp_var]] == mod & !is.na(data_mod[[exp_var]]) & !is.na(data_mod$outcome_bin), ]
      n_mod <- nrow(sub_df)
      n_pos <- sum(sub_df$outcome_bin == 1)
      pct_pos <- if (n_mod > 0) (n_pos / n_mod) * 100 else 0

      n_pct_str <- paste0(n_pos, "/", n_mod, " (", .fmt_percent(pct_pos, 1, suffix = TRUE), ")")

      if (i == 1) {
        or_str <- paste0(.fmt_decimal(1, digits), " (Réf.)")
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

          or_fmt <- .fmt_decimal(or_val, digits)
          low_fmt <- .fmt_decimal(ci_low, digits)
          high_fmt <- .fmt_decimal(ci_high, digits)

          or_str <- paste0(or_fmt, " [", low_fmt, " - ", high_fmt, "]")
          p_str <- .fmt_pvalue(p_val, 3)
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

  out_df_display <- out_df
  dup_idx <- duplicated(out_df_display$Variable)
  out_df_display$Variable[dup_idx] <- ""

  names(out_df_display) <- c("Variable", "Modalité", paste0("Effectif (", outcome_positive_val, ")"), "OR brut [IC95%]", "p-value")

  ft <- flextable::flextable(out_df_display) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste("Association bivariée avec", outcome_nm, "(Événement :", outcome_positive_val, ")"))

  as_analytix_table(data = out_df, flextable = ft)
}
