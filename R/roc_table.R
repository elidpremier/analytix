#' @title Courbe ROC et tableau AUC
#' @description Calcule la courbe ROC, l'AUC (avec IC95% DeLong) et le seuil optimal
#' (index de Youden) pour un prédicteur numérique ou un score de probabilité vs un outcome binaire.
#' Retourne un tableau flextable et optionnellement le graphique ggplot2.
#'
#' @param data data.frame.
#' @param outcome Nom (character) de la variable binaire (0/1 ou deux modalités).
#' @param predictor Nom (character) du prédicteur numérique (probabilité, score, dosage...).
#'   Si NULL, un modèle logistique est ajusté sur toutes les variables numériques.
#' @param positive_val Valeur positive de l'outcome (auto-détection si NULL).
#' @param conf_level Niveau de confiance pour l'IC de l'AUC (défaut: 0.95).
#' @param digits Nombre de décimales (défaut: 3).
#' @param color Couleur d'en-tête (défaut: "#D3D3D3").
#' @param return_plot Retourner également le graphique ggplot2 (défaut: TRUE).
#'
#' @return Une liste avec :
#'   \describe{
#'     \item{flextable}{Tableau des indicateurs ROC (AUC, IC95%, seuil optimal, Se, Sp)}
#'     \item{plot}{Objet ggplot2 de la courbe ROC (si return_plot = TRUE)}
#'     \item{roc_object}{Objet pROC::roc pour usage ultérieur}
#'     \item{auc}{Valeur numérique de l'AUC}
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' df <- data.frame(
#'   score = rnorm(100, 5, 2),
#'   maladie = rbinom(100, 1, 0.4)
#' )
#' res <- roc_table(df, outcome = "maladie", predictor = "score")
#' res$flextable
#' res$plot
#' }
#'
#' @export
roc_table <- function(data, outcome, predictor = NULL, positive_val = NULL,
                       conf_level = 0.95, digits = 3, color = "#D3D3D3",
                       return_plot = TRUE) {

  if (!requireNamespace("pROC", quietly = TRUE))
    stop("Le package 'pROC' est requis. Installez-le : install.packages('pROC')")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Le package 'ggplot2' est requis.")
  if (!outcome %in% names(data))
    stop("La variable outcome '", outcome, "' est absente du data.frame.")

  outcome_vec <- data[[outcome]]

  # Binarisation de l'outcome
  if (!is.numeric(outcome_vec)) {
    uniq_vals <- sort(unique(na.omit(outcome_vec)))
    if (length(uniq_vals) != 2) stop("L'outcome doit être binaire (exactement 2 modalités).")
    if (!is.null(positive_val)) {
      outcome_num <- as.integer(outcome_vec == positive_val)
    } else {
      outcome_num <- as.integer(outcome_vec == uniq_vals[2])
      positive_val <- uniq_vals[2]
    }
  } else {
    outcome_num <- outcome_vec
    if (is.null(positive_val)) positive_val <- max(outcome_num, na.rm = TRUE)
  }

  # Calcul du prédicteur
  if (!is.null(predictor)) {
    if (!predictor %in% names(data)) stop("La variable predictor '", predictor, "' est absente.")
    pred_vec <- data[[predictor]]
    pred_label <- .get_label(data, predictor, predictor)
  } else {
    # Régression logistique sur toutes les variables numériques sauf l'outcome
    num_vars <- names(data)[sapply(data, is.numeric) & names(data) != outcome]
    num_vars <- num_vars[sapply(num_vars, function(v) !all(is.na(data[[v]])))]
    if (length(num_vars) == 0) stop("Aucune variable numérique disponible pour la ROC.")
    formula_str <- paste0("`", outcome, "` ~ ", paste(paste0("`", num_vars, "`"), collapse = " + "))
    df_mod <- data; df_mod[[outcome]] <- outcome_num
    mod_roc <- tryCatch(
      stats::glm(stats::as.formula(formula_str), data = df_mod, family = stats::binomial()),
      error = function(e) stop("Impossible d'ajuster le modèle ROC : ", e$message)
    )
    pred_vec  <- stats::fitted(mod_roc)
    pred_label <- "Score prédit (modèle logistique)"
    predictor  <- "fitted_values"
  }

  # Calculer la courbe ROC
  valid_idx  <- !is.na(outcome_num) & !is.na(pred_vec)
  roc_obj    <- pROC::roc(outcome_num[valid_idx], pred_vec[valid_idx], quiet = TRUE)
  auc_val    <- as.numeric(pROC::auc(roc_obj))
  ci_obj     <- pROC::ci.auc(roc_obj, conf.level = conf_level)

  # Seuil optimal (Youden)
  coords_opt <- tryCatch(
    pROC::coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"),
                 best.method = "youden"),
    error = function(e) NULL
  )

  # Interprétation AUC
  auc_interp <- dplyr::case_when(
    auc_val >= 0.90 ~ "Excellente (≥ 0,90)",
    auc_val >= 0.80 ~ "Bonne (0,80 – 0,89)",
    auc_val >= 0.70 ~ "Acceptable (0,70 – 0,79)",
    auc_val >= 0.60 ~ "Faible (0,60 – 0,69)",
    TRUE            ~ "Pas de discrimination (< 0,60)"
  )

  .fmt <- function(x, d = digits) format(round(x, d), nsmall = d, decimal.mark = ",")

  rows <- list(
    data.frame(Indicateur = "Prédicteur analysé",
               Valeur = pred_label, stringsAsFactors = FALSE),
    data.frame(Indicateur = "N valide",
               Valeur = as.character(sum(valid_idx)), stringsAsFactors = FALSE),
    data.frame(Indicateur = paste0("AUC [IC", round(conf_level*100), "%] (méthode DeLong)"),
               Valeur = paste0(.fmt(auc_val), " [", .fmt(ci_obj[1]), " — ", .fmt(ci_obj[3]), "]"),
               stringsAsFactors = FALSE),
    data.frame(Indicateur = "Interprétation de la discrimination",
               Valeur = auc_interp, stringsAsFactors = FALSE)
  )

  if (!is.null(coords_opt) && nrow(coords_opt) > 0) {
    rows <- c(rows, list(
      data.frame(Indicateur = "Seuil optimal (index de Youden)",
                 Valeur = .fmt(coords_opt[1, "threshold"]), stringsAsFactors = FALSE),
      data.frame(Indicateur = "Sensibilité au seuil optimal",
                 Valeur = paste0(.fmt(coords_opt[1,"sensitivity"] * 100, d = 1), " %"),
                 stringsAsFactors = FALSE),
      data.frame(Indicateur = "Spécificité au seuil optimal",
                 Valeur = paste0(.fmt(coords_opt[1,"specificity"] * 100, d = 1), " %"),
                 stringsAsFactors = FALSE)
    ))
  }

  res_df <- do.call(rbind, rows)

  ft <- theme_analytique(flextable::flextable(res_df), color = color) |>
    flextable::set_caption(
      paste0("Courbe ROC — AUC = ", .fmt(auc_val),
             " [IC", round(conf_level*100), "% : ", .fmt(ci_obj[1]), " — ", .fmt(ci_obj[3]), "]")
    ) |>
    flextable::bold(i = 3)

  # Graphique ROC
  p_roc <- NULL
  if (return_plot) {
    p_roc <- pROC::ggroc(roc_obj, colour = "#0284c7", size = 1.2, legacy.axes = FALSE) +
      ggplot2::geom_abline(slope = 1, intercept = 1, linetype = "dashed",
                           colour = "grey60", alpha = 0.8) +
      ggplot2::annotate("text", x = 0.25, y = 0.08,
                        label = paste0("AUC = ", .fmt(auc_val),
                                       "\n[", .fmt(ci_obj[1]), " — ", .fmt(ci_obj[3]), "]"),
                        size = 4.5, fontface = "bold", colour = "#0284c7", hjust = 0) +
      ggplot2::labs(
        title    = paste0("Courbe ROC — ", .get_label(data, outcome, outcome)),
        subtitle = paste0("Prédicteur : ", pred_label),
        x        = "1 — Spécificité (Taux de faux positifs)",
        y        = "Sensibilité (Taux de vrais positifs)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(colour = "grey40", size = 10)
      )

    if (!is.null(coords_opt) && nrow(coords_opt) > 0) {
      p_roc <- p_roc +
        ggplot2::geom_point(
          data = data.frame(x = 1 - coords_opt[1,"specificity"],
                            y = coords_opt[1,"sensitivity"]),
          ggplot2::aes(x = x, y = y),
          colour = "#dc2626", size = 4, shape = 18
        ) +
        ggplot2::annotate("text",
                          x = 1 - coords_opt[1,"specificity"] + 0.05,
                          y = coords_opt[1,"sensitivity"] - 0.05,
                          label = paste0("Seuil optimal\n= ", .fmt(coords_opt[1,"threshold"])),
                          size = 3.5, colour = "#dc2626")
    }
  }

  list(
    flextable  = ft,
    plot       = p_roc,
    roc_object = roc_obj,
    auc        = auc_val
  )
}
