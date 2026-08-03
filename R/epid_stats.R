#' @title Indicateurs diagnostiques (Sensibilité, Spécificité, VPP, VPN, LR)
#' @description Calcule la sensibilité, spécificité, valeurs prédictives positive
#' et négative, et les rapports de vraisemblance (LR+, LR-) avec leurs IC95%
#' (méthode de Wilson) à partir d'une matrice de confusion ou de vecteurs prédit/réel.
#'
#' @param actual Vecteur de la valeur réelle (0/1 ou "Positif"/"Négatif").
#' @param predicted Vecteur de la valeur prédite/test (0/1 ou "Positif"/"Négatif").
#' @param positive_val Valeur représentant le cas positif (défaut: auto-détection).
#' @param conf_level Niveau de confiance pour les IC (défaut: 0.95).
#' @param digits Nombre de décimales (défaut: 1).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Un objet `flextable` avec les indicateurs diagnostiques.
#'
#' @examples
#' actual    <- c(1,1,1,0,0,0,1,0,1,0)
#' predicted <- c(1,1,0,0,0,1,1,0,0,0)
#' calc_sensitivity_specificity(actual, predicted)
#'
#' @export
calc_sensitivity_specificity <- function(actual, predicted,
                                         positive_val = NULL,
                                         conf_level = 0.95,
                                         digits = 1,
                                         color = "#D3D3D3") {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")

  if (is.null(positive_val)) {
    uv <- unique(c(actual, predicted))
    uv <- uv[!is.na(uv)]
    pos_candidates <- c(1, "1", "Positif", "positif", "Oui", "oui", "+", TRUE, "BLSE")
    m <- intersect(pos_candidates, uv)
    positive_val <- if (length(m) > 0) m[1] else uv[1]
  }

  tp <- sum(actual == positive_val & predicted == positive_val, na.rm = TRUE)
  fp <- sum(actual != positive_val & predicted == positive_val, na.rm = TRUE)
  fn <- sum(actual == positive_val & predicted != positive_val, na.rm = TRUE)
  tn <- sum(actual != positive_val & predicted != positive_val, na.rm = TRUE)

  .ic <- function(num, denom) {
    if (denom == 0) return(c(0, 0, 0))
    p  <- num / denom
    z  <- stats::qnorm(1 - (1 - conf_level) / 2)
    d  <- 1 + z^2 / denom
    pm <- (p + z^2 / (2 * denom)) / d
    margin <- (z / d) * sqrt(p * (1 - p) / denom + z^2 / (4 * denom^2))
    c(p, max(0, pm - margin), min(1, pm + margin))
  }

  .fmt <- function(num, denom) {
    r <- .ic(num, denom)
    pct <- format(round(r[1] * 100, digits), nsmall = digits, decimal.mark = ",")
    lo  <- format(round(r[2] * 100, digits), nsmall = digits, decimal.mark = ",")
    hi  <- format(round(r[3] * 100, digits), nsmall = digits, decimal.mark = ",")
    paste0(pct, "% [", lo, " - ", hi, "]")
  }

  se_r   <- .ic(tp, tp + fn);  sp_r  <- .ic(tn, tn + fp)
  lr_pos <- if (sp_r[1] == 1) Inf else (se_r[1]) / (1 - sp_r[1])
  lr_neg <- if (se_r[1] == 0) 0   else (1 - se_r[1]) / sp_r[1]

  res_df <- data.frame(
    Indicateur = c("VP (vrais positifs)", "FP (faux positifs)",
                   "FN (faux négatifs)", "VN (vrais négatifs)",
                   "Sensibilité [IC95%]", "Spécificité [IC95%]",
                   "VPP [IC95%]", "VPN [IC95%]",
                   "Rapport de vraisemblance + (LR+)",
                   "Rapport de vraisemblance - (LR-)"),
    Valeur = c(
      tp, fp, fn, tn,
      .fmt(tp, tp + fn),
      .fmt(tn, tn + fp),
      .fmt(tp, tp + fp),
      .fmt(tn, tn + fn),
      format(round(lr_pos, 2), decimal.mark = ","),
      format(round(lr_neg, 2), decimal.mark = ",")
    ),
    stringsAsFactors = FALSE
  )

  flextable::flextable(res_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption("Indicateurs de performance diagnostique") %>%
    flextable::bold(i = 5:10, part = "body")
}

#' @title Tableau formaté d'une régression logistique multivariée
#' @description À partir d'un objet `glm` ou d'une formule + données, génère
#' un tableau `flextable` de la régression logistique multivariée (ORa, IC95%, p-value).
#'
#' @param model Objet `glm` (famille `binomial`) ou formule R.
#' @param data data.frame (requis si `model` est une formule).
#' @param var_labels Vecteur nommé de libellés pour les variables explicatives.
#' @param conf_level Niveau de confiance (défaut: 0.95).
#' @param digits Nombre de décimales pour les OR (défaut: 2).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Un objet `flextable`.
#'
#' @examples
#' mod <- glm(am ~ cyl + wt + hp, data = mtcars, family = binomial())
#' multivariable_logistic_table(mod)
#'
#' @export
multivariable_logistic_table <- function(model, data = NULL, var_labels = NULL,
                                          conf_level = 0.95, digits = 2,
                                          color = "#D3D3D3") {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")

  if (inherits(model, "formula")) {
    if (is.null(data)) stop("Veuillez fournir `data` si `model` est une formule.")
    model <- stats::glm(model, data = data, family = stats::binomial())
  }

  co <- summary(model)$coefficients
  ci <- tryCatch(suppressMessages(stats::confint(model, level = conf_level)),
                 error = function(e) NULL)

  rows <- lapply(rownames(co)[-1], function(nm) {
    or   <- exp(co[nm, "Estimate"])
    pval <- co[nm, "Pr(>|z|)"]
    if (!is.null(ci) && nm %in% rownames(ci)) {
      lo <- exp(ci[nm, 1]);  hi <- exp(ci[nm, 2])
    } else {
      z  <- stats::qnorm(1 - (1 - conf_level) / 2)
      lo <- exp(co[nm, "Estimate"] - z * co[nm, "Std. Error"])
      hi <- exp(co[nm, "Estimate"] + z * co[nm, "Std. Error"])
    }
    lab <- if (!is.null(var_labels) && nm %in% names(var_labels)) {
      var_labels[[nm]]
    } else if (!is.null(data) && nm %in% names(data)) {
      .get_label(data, nm, nm)
    } else {
      nm
    }
    p_s <- if (pval < 0.001) "< 0,001" else format(round(pval, 3), decimal.mark = ",")
    data.frame(
      Variable    = lab,
      ORa         = paste0(format(round(or, digits), nsmall = digits, decimal.mark = ","),
                           " [", format(round(lo, digits), nsmall = digits, decimal.mark = ","),
                           " - ", format(round(hi, digits), nsmall = digits, decimal.mark = ","), "]"),
      P_value     = p_s,
      stringsAsFactors = FALSE
    )
  })

  res_df <- dplyr::bind_rows(rows)
  names(res_df)[2] <- paste0("ORa [IC", round(conf_level * 100), "%]")
  names(res_df)[3] <- "p-value"

  n_obs <- nobs(model)
  aic   <- round(AIC(model), 1)

  flextable::flextable(res_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption("Régression logistique multivariée") %>%
    flextable::add_footer_lines(paste0("N = ", n_obs, " | AIC = ", aic))
}

#' @title Tableau ANOVA à un facteur avec test post-hoc de Tukey
#' @description Effectue une ANOVA à un facteur et un test de comparaisons
#' multiples de Tukey (HSD). Retourne un `flextable` formaté avec les
#' résultats de l'ANOVA et les comparaisons par paires.
#'
#' @param data data.frame.
#' @param var Variable numérique dépendante.
#' @param group Variable catégorielle de groupement.
#' @param var_name Libellé de la variable dépendante.
#' @param group_name Libellé de la variable de groupe.
#' @param digits Nombre de décimales (défaut: 3).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Un objet `flextable`.
#'
#' @examples
#' anova_table(iris, Sepal.Length, Species,
#'   var_name = "Longueur sépale", group_name = "Espèce")
#'
#' @export
anova_table <- function(data, var, group, var_name = NULL, group_name = NULL,
                        digits = 3, color = "#D3D3D3") {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")

  var_enq   <- rlang::enquo(var)
  group_enq <- rlang::enquo(group)
  var_nm    <- rlang::as_name(var_enq)
  group_nm  <- rlang::as_name(group_enq)

  if (is.null(var_name))   var_name   <- .get_label(data, var_nm, var_nm)
  if (is.null(group_name)) group_name <- .get_label(data, group_nm, group_nm)

  formula_obj <- stats::as.formula(paste(var_nm, "~", group_nm))
  fit_aov     <- stats::aov(formula_obj, data = data)
  aov_sum     <- summary(fit_aov)[[1]]

  f_val  <- round(aov_sum[["F value"]][1], digits)
  df1    <- aov_sum[["Df"]][1]
  df2    <- aov_sum[["Df"]][2]
  p_val  <- aov_sum[["Pr(>F)"]][1]
  p_str  <- if (p_val < 0.001) "< 0,001" else format(round(p_val, digits), decimal.mark = ",")

  # ANOVA synthèse
  aov_df <- data.frame(
    Source     = c(group_name, "Résidus"),
    Df         = c(df1, df2),
    "Somme carr." = round(c(aov_sum[["Sum Sq"]][1], aov_sum[["Sum Sq"]][2]), digits),
    "Carré moy."  = round(c(aov_sum[["Mean Sq"]][1], aov_sum[["Mean Sq"]][2]), digits),
    F          = c(format(f_val, nsmall = digits, decimal.mark = ","), ""),
    "p-value"  = c(p_str, ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Post-hoc Tukey
  tukey <- stats::TukeyHSD(fit_aov)[[group_nm]]
  tuk_df <- data.frame(
    Comparaison = rownames(tukey),
    Difference  = format(round(tukey[, "diff"], digits), nsmall = digits, decimal.mark = ","),
    IC_inf      = format(round(tukey[, "lwr"],  digits), nsmall = digits, decimal.mark = ","),
    IC_sup      = format(round(tukey[, "upr"],  digits), nsmall = digits, decimal.mark = ","),
    "p adj."    = sapply(tukey[, "p adj"], function(p) {
      if (p < 0.001) "< 0,001" else format(round(p, digits), decimal.mark = ",")
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  ft_aov <- flextable::flextable(aov_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste("ANOVA :", var_name, "~", group_name)) %>%
    flextable::add_footer_lines(paste0("F(", df1, ", ", df2, ") = ", f_val,
                                       " | p = ", p_str))

  ft_tuk <- flextable::flextable(tuk_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption("Test post-hoc de Tukey (HSD)")

  list(anova = ft_aov, tukey = ft_tuk)
}

#' @title Matrice de corrélations formatée en flextable
#' @description Calcule la matrice de corrélations (Pearson ou Spearman) entre
#' plusieurs variables numériques et la formate en `flextable` avec mise en
#' évidence des corrélations significatives.
#'
#' @param data data.frame.
#' @param cols Vecteur de noms de colonnes numériques. Si NULL, toutes les variables
#'   numériques du data.frame sont utilisées.
#' @param method Méthode de corrélation: "pearson" (défaut) ou "spearman".
#' @param digits Nombre de décimales (défaut: 2).
#' @param sig_level Seuil de significativité pour la mise en gras (défaut: 0.05).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Un objet `flextable` avec la matrice de corrélations.
#'
#' @examples
#' correlation_table(mtcars, cols = c("mpg", "cyl", "hp", "wt"))
#'
#' @export
correlation_table <- function(data, cols = NULL, method = c("pearson", "spearman"),
                               digits = 2, sig_level = 0.05, color = "#D3D3D3") {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  method <- match.arg(method)

  if (is.null(cols)) {
    cols <- names(data)[sapply(data, is.numeric)]
  }
  mat_data <- data[, cols, drop = FALSE]
  mat_data <- mat_data[, sapply(mat_data, is.numeric), drop = FALSE]
  cols <- names(mat_data)

  n <- length(cols)
  cor_mat  <- matrix(NA_real_, n, n, dimnames = list(cols, cols))
  pval_mat <- matrix(NA_real_, n, n, dimnames = list(cols, cols))

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        cor_mat[i, j]  <- 1
        pval_mat[i, j] <- NA
      } else {
        ct <- tryCatch(
          stats::cor.test(mat_data[[i]], mat_data[[j]], method = method),
          error = function(e) NULL
        )
        if (!is.null(ct)) {
          cor_mat[i, j]  <- ct$estimate
          pval_mat[i, j] <- ct$p.value
        }
      }
    }
  }

  # Mise en forme : triangle inférieur seulement
  display_mat <- cor_mat
  display_mat[upper.tri(display_mat)] <- NA

  res_df <- as.data.frame(display_mat)
  res_df[] <- lapply(res_df, function(v) {
    ifelse(is.na(v), "", format(round(v, digits), nsmall = digits, decimal.mark = ","))
  })
  res_df <- data.frame(Variable = cols, res_df, check.names = FALSE)

  ft <- flextable::flextable(res_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste0("Matrice de corrélations (", method, ")"))

  # Mise en gras des corrélations significatives
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (!is.na(pval_mat[i, j]) && pval_mat[i, j] < sig_level && !is.na(cor_mat[i, j])) {
        col_name <- cols[j]
        if (col_name %in% ft$col_keys) {
          ft <- flextable::bold(ft, i = i, j = col_name, part = "body")
        }
      }
    }
  }

  ft <- flextable::add_footer_lines(ft,
    paste0("Méthode : ", method, " | Seuil de significativité : p < ", sig_level,
           " (valeurs en gras)"))
  ft
}
