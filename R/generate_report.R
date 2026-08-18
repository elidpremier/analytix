#' @title Générer un rapport Word complet d'analyse statistique
#' @description Génère automatiquement un rapport Word (.docx) complet à partir d'un jeu
#' de données. La fonction détecte le type de chaque variable, applique les analyses
#' appropriées (descriptives, bivariées, régression, survie, ROC) et construit un document
#' Word structuré, formaté pleine-page, prêt à la publication scientifique.
#'
#' @param data data.frame à analyser. Obligatoire.
#' @param output Chemin du fichier Word de sortie (défaut: "rapport_analytix.docx").
#' @param title Titre du rapport (défaut: "Rapport d'Analyse Statistique").
#' @param subtitle Sous-titre (défaut: "Analyse descriptive et inférentielle").
#' @param author Nom de l'auteur (défaut: "").
#' @param institution Nom de l'institution/organisme (défaut: "").
#' @param outcome Nom (character) de la variable d'intérêt/outcome pour les analyses
#'   bivariées et la régression logistique. Si NULL (défaut), seules les analyses
#'   descriptives sont générées.
#' @param time_var Nom (character) de la variable temps pour l'analyse de survie (Kaplan-Meier).
#'   Requiert que `outcome` soit aussi fourni (variable événement 0/1).
#' @param group_var Nom (character) de la variable de groupe pour la courbe KM et la ROC.
#' @param vars Vecteur de noms de variables à inclure dans les analyses descriptives.
#'   Si NULL (défaut), toutes les variables sont analysées.
#' @param bivariate_vars Vecteur de noms de variables explicatives pour les analyses bivariées et la régression.
#'   Si NULL (défaut), utilise toutes les `vars` (sauf l'outcome).
#' @param sections Vecteur des sections à inclure. Valeurs possibles :
#'   "cover", "summary", "missing", "descriptive", "bivariate", "regression",
#'   "roc", "survival", "correlation".
#' @param page_width Largeur utile de la page en cm (défaut: 16 — A4 standard avec marges).
#' @param digits Nombre de décimales (défaut: 1).
#' @param color Couleur d'en-tête des flextables (défaut: "#D3D3D3").
#' @param include_plots Inclure des graphiques dans le rapport (défaut: TRUE).
#' @param open_doc Ouvrir le document après génération (défaut: FALSE).
#' @param verbose Afficher les messages de progression (défaut: TRUE).
#'
#' @return Le chemin du fichier Word généré (invisible).
#'
#' @examples
#' \dontrun{
#' # Rapport descriptif simple
#' generate_report(iris, output = "rapport_iris.docx",
#'                 title = "Description du jeu de données Iris",
#'                 author = "Dr. Statisticien")
#'
#' # Rapport complet avec outcome binaire
#' generate_report(mon_df, output = "rapport.docx",
#'                 title = "Étude clinique", author = "Dr. IDO",
#'                 outcome = "deces",
#'                 sections = c("cover","summary","missing","descriptive",
#'                              "bivariate","regression","roc","correlation"))
#' }
#'
#' @importFrom officer read_docx body_add_par body_add_img body_add_break
#' @importFrom flextable body_add_flextable flextable theme_vanilla
#'
#' @export
generate_report <- function(data,
                             output      = "rapport_analytix.docx",
                             title       = "Rapport d'Analyse Statistique",
                             subtitle    = "Analyse descriptive et inférentielle",
                             author      = "",
                             institution = "",
                             outcome     = NULL,
                             time_var    = NULL,
                             group_var   = NULL,
                             vars        = NULL,
                             bivariate_vars = NULL,
                             sections    = c("cover", "summary", "missing", "descriptive",
                                             "bivariate", "regression", "correlation"),
                             page_width  = 16,
                             digits      = 1,
                             color       = "#D3D3D3",
                             include_plots = TRUE,
                             open_doc    = FALSE,
                             verbose     = TRUE) {

  # ============================================================
  # Validation des entrées
  # ============================================================
  if (!is.data.frame(data)) stop("`data` doit être un data.frame.")
  if (nrow(data) == 0)      stop("`data` est vide (0 lignes).")
  if (!requireNamespace("officer",   quietly = TRUE)) stop("Package 'officer' requis.")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' requis.")
  if (!requireNamespace("ggplot2",   quietly = TRUE)) stop("Package 'ggplot2' requis.")
  if (!is.null(outcome) && !outcome %in% names(data))
    stop("La variable outcome '", outcome, "' est absente du data.frame.")
  if (!is.null(time_var) && !time_var %in% names(data))
    stop("La variable time_var '", time_var, "' est absente du data.frame.")

  # ============================================================
  # Helper : applique theme_analytique + fit_to_width sur tout ft
  # Garantit que TOUS les tableaux occupent la pleine largeur de page
  # ============================================================
  .ft_page <- function(ft, cap = NULL) {
    if (is.null(ft)) return(NULL)
    # Extraire le flextable si c'est une liste analytix
    if (!inherits(ft, "flextable") && is.list(ft)) {
      ft <- ft$flextable %||% ft$table %||% ft$ft
    }
    if (!inherits(ft, "flextable")) return(NULL)
    # Appliquer theme_analytique avec largeur page complète
    ft <- theme_analytique(ft, page_width = page_width, color = color)
    # Ajouter caption si fourni
    if (!is.null(cap) && nchar(trimws(cap)) > 0)
      ft <- flextable::set_caption(ft, cap)
    ft
  }

  # Helper null-coalescing
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Helper : insérer un ft dans doc (avec thème page forcé)
  .add_ft <- function(doc, ft, cap = NULL) {
    ft_themed <- .ft_page(ft, cap)
    if (!is.null(ft_themed)) {
      doc <- flextable::body_add_flextable(doc, ft_themed)
    }
    doc
  }

  # Helper : df → flextable themé pleine largeur
  .df_to_ft <- function(df, cap = NULL) {
    ft <- flextable::flextable(df)
    .ft_page(ft, cap)
  }

  # Compteur de sections
  sec_num  <- 0L
  .next_sec <- function(label) { sec_num <<- sec_num + 1L; paste0(sec_num, ". ", label) }
  .log  <- function(...) if (verbose) cat("  ✅", ..., "\n")
  .warn <- function(...) message("  ⚠️ ", ...)

  # ============================================================
  # Initialisation du document Word
  # ============================================================
  doc <- officer::read_docx()

  # ============================================================
  # SECTION : cover
  # ============================================================
  if ("cover" %in% sections) {
    .log("Page de titre...")
    doc <- officer::body_add_par(doc, title,    style = "heading 1")
    doc <- officer::body_add_par(doc, subtitle, style = "heading 2")
    if (nchar(trimws(author)) > 0)
      doc <- officer::body_add_par(doc, paste("Auteur :", author), style = "Normal")
    if (nchar(trimws(institution)) > 0)
      doc <- officer::body_add_par(doc, paste("Institution :", institution), style = "Normal")
    doc <- officer::body_add_par(doc, paste("Date :", format(Sys.Date(), "%d %B %Y")), style = "Normal")
    doc <- officer::body_add_par(doc,
      paste("Généré par le package analytix", tryCatch(packageVersion("analytix"), error=function(e) "")),
      style = "Normal")
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_break(doc)
  }

  # ============================================================
  # SECTION : summary
  # ============================================================
  if ("summary" %in% sections) {
    .log("Synthèse du jeu de données...")
    doc <- officer::body_add_par(doc, .next_sec("Aperçu du jeu de données"), style = "heading 1")

    completude <- round((1 - sum(is.na(data)) / (nrow(data) * ncol(data))) * 100, 1)
    meta_df <- data.frame(
      Indicateur = c("Nombre d'observations", "Nombre de variables", "Taux de complétude global"),
      Valeur     = c(format(nrow(data), big.mark = "\u202f"), ncol(data), paste0(completude, " %")),
      stringsAsFactors = FALSE
    )
    doc <- .add_ft(doc, .df_to_ft(meta_df, "Synthèse du jeu de données"))
    doc <- officer::body_add_par(doc, "", style = "Normal")

    # Tableau des types de variables (via auto_describe)
    var_types_df <- tryCatch({
      attr(auto_describe(data, vars = vars, verbose = FALSE), "var_types")
    }, error = function(e) NULL)

    if (!is.null(var_types_df)) {
      doc <- officer::body_add_par(doc, "Inventaire des variables", style = "heading 2")
      doc <- .add_ft(doc, .df_to_ft(var_types_df, "Inventaire des variables"))
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }
  }

  # ============================================================
  # SECTION : missing
  # ============================================================
  if ("missing" %in% sections) {
    .log("Rapport sur les données manquantes...")
    doc <- officer::body_add_par(doc, .next_sec("Données manquantes"), style = "heading 1")

    tryCatch({
      mr <- missing_report(data, digits = digits, color = color)
      if (!is.null(mr$flextable)) {
        # Re-appliquer theme_analytique pour forcer pleine largeur
        doc <- .add_ft(doc, mr$flextable)
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }
    }, error = function(e) {
      .warn("missing_report a échoué : ", e$message)
      # Fallback tableau simple
      na_df <- data.frame(
        Variable   = names(data),
        `Nb NA`    = sapply(data, function(x) sum(is.na(x))),
        `Taux (%)`  = round(sapply(data, function(x) mean(is.na(x)) * 100), 1),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      doc <<- .add_ft(doc, .df_to_ft(na_df, "Données manquantes par variable"))
      doc <<- officer::body_add_par(doc, "", style = "Normal")
    })

    # Heatmap des manquants
    if (include_plots) {
      tryCatch({
        if (any(is.na(data))) {
          p_miss <- plot_missing_map(data)
          tmp_png <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp_png, plot = p_miss, width = 9, height = 5, dpi = 150)
          doc <- officer::body_add_par(doc, "Cartographie des valeurs manquantes", style = "heading 2")
          doc <- officer::body_add_img(doc, src = tmp_png, width = 6.3, height = 3.5)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }, error = function(e) .warn("Heatmap manquants non disponible : ", e$message))
    }
  }

  # ============================================================
  # SECTION : descriptive
  # ============================================================
  if ("descriptive" %in% sections) {
    .log("Statistiques descriptives...")
    doc <- officer::body_add_par(doc, .next_sec("Statistiques descriptives"), style = "heading 1")

    vars_desc <- if (is.null(vars)) names(data) else vars

    ad_res <- tryCatch(
      auto_describe(data, vars = vars_desc, digits = digits, color = color, verbose = FALSE),
      error = function(e) { .warn("auto_describe a échoué : ", e$message); NULL }
    )

    if (!is.null(ad_res) && length(ad_res) > 0) {
      var_types_attr <- attr(ad_res, "var_types")

      for (var_label in names(ad_res)) {
        res_var <- ad_res[[var_label]]
        if (is.null(res_var)) next

        doc <- officer::body_add_par(doc, var_label, style = "heading 2")

        # Flextable themé pleine largeur
        if (!is.null(res_var$flextable)) {
          doc <- .add_ft(doc, res_var$flextable)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }

        # Graphique associé
        if (include_plots) {
          var_nm_orig <- if (!is.null(var_types_attr)) {
            var_types_attr$variable[var_types_attr$label == var_label][1]
          } else var_label

          if (!is.na(var_nm_orig) && var_nm_orig %in% names(data)) {
            type_det <- if (!is.null(var_types_attr)) {
              var_types_attr$type_detected[var_types_attr$variable == var_nm_orig][1]
            } else "autre"

            tryCatch({
              p <- if (type_det == "numerique") {
                plot_distribution(data, var = !!rlang::sym(var_nm_orig))
              } else if (type_det %in% c("categorielle", "binaire")) {
                plot_barplot(data, x = !!rlang::sym(var_nm_orig), title = var_label)
              } else NULL

              if (!is.null(p)) {
                tmp_png <- tempfile(fileext = ".png")
                ggplot2::ggsave(tmp_png, plot = p, width = 7, height = 4, dpi = 150)
                doc <- officer::body_add_img(doc, src = tmp_png, width = 5.5, height = 3.2)
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }, error = function(e) .warn("Graphique pour '", var_label, "' non disponible."))
          }
        }
      }
    } else {
      doc <- officer::body_add_par(doc, "Aucune analyse descriptive disponible.", style = "Normal")
    }
  }

  # ============================================================
  # SECTION : bivariate (si outcome fourni)
  # ============================================================
  if ("bivariate" %in% sections && !is.null(outcome)) {
    .log("Analyses bivariées...")
    doc <- officer::body_add_par(doc, .next_sec("Analyses bivariées"), style = "heading 1")
    doc <- officer::body_add_par(doc,
      paste("Variable d'intérêt :", .get_label(data, outcome, outcome)),
      style = "heading 2")

    tryCatch({
      exposures_auto <- if (!is.null(bivariate_vars)) setdiff(bivariate_vars, outcome) else if (is.null(vars)) setdiff(names(data), outcome) else setdiff(vars, outcome)
      exposures_auto <- exposures_auto[sapply(exposures_auto, function(v) {
        x <- data[[v]]; n <- length(unique(na.omit(x)))
        n >= 2 && n <= 20 && !all(is.na(x))
      })]
      if (length(exposures_auto) == 0) stop("Aucune variable explicative compatible.")
      ft_biv <- bivariate_or_table(data, outcome = outcome,
                                    exposures = exposures_auto,
                                    digits = digits, color = color)
      doc <- .add_ft(doc, ft_biv)
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }, error = function(e) {
      .warn("bivariate_or_table a échoué : ", e$message)
      doc <<- officer::body_add_par(doc,
        paste("Analyse bivariée non disponible :", e$message), style = "Normal")
    })
  }

  # ============================================================
  # SECTION : regression (si outcome fourni et binaire)
  # ============================================================
  if ("regression" %in% sections && !is.null(outcome)) {
    is_binary_outcome <- length(unique(na.omit(data[[outcome]]))) == 2

    if (is_binary_outcome) {
      .log("Régression logistique multivariée...")
      doc <- officer::body_add_par(doc, .next_sec("Régression logistique multivariée"), style = "heading 1")

      tryCatch({
        vars_reg <- if (!is.null(bivariate_vars)) setdiff(bivariate_vars, outcome) else if (is.null(vars)) setdiff(names(data), outcome) else setdiff(vars, outcome)
        vars_reg <- vars_reg[sapply(vars_reg, function(v) {
          x <- data[[v]]; length(unique(na.omit(x))) >= 2 && !all(is.na(x))
        })]
        if (length(vars_reg) == 0) stop("Aucune variable explicative valide.")

        df_reg <- data
        if (!is.numeric(df_reg[[outcome]]) && !is.logical(df_reg[[outcome]]))
          df_reg[[outcome]] <- as.factor(df_reg[[outcome]])

        formula_str <- paste0("`", outcome, "` ~ ",
                              paste(paste0("`", vars_reg, "`"), collapse = " + "))
        mod <- stats::glm(stats::as.formula(formula_str), data = df_reg,
                          family = stats::binomial())
        ft_reg <- multivariable_logistic_table(mod, data = df_reg, digits = digits, color = color)
        doc <- .add_ft(doc, ft_reg)
        doc <- officer::body_add_par(doc, "", style = "Normal")
        doc <- officer::body_add_par(doc,
          paste0("Note : Régression logistique binaire (outcome = '", outcome, "'). ",
                 "N = ", stats::nobs(mod), " | AIC = ", round(stats::AIC(mod), 1)),
          style = "Normal")
      }, error = function(e) {
        .warn("Régression logistique a échoué : ", e$message)
        doc <<- officer::body_add_par(doc,
          paste("Régression non disponible :", e$message), style = "Normal")
      })
    } else {
      if (verbose) message("  ℹ️  Régression ignorée : l'outcome n'est pas binaire.")
    }
  }

  # ============================================================
  # SECTION : roc (courbe ROC + AUC, si outcome binaire fourni)
  # ============================================================
  if ("roc" %in% sections && !is.null(outcome)) {
    is_binary_roc <- length(unique(na.omit(data[[outcome]]))) == 2
    if (is_binary_roc && requireNamespace("pROC", quietly = TRUE)) {
      .log("Courbe ROC...")
      doc <- officer::body_add_par(doc, .next_sec("Courbe ROC et AUC"), style = "heading 1")

      tryCatch({
        # Variables numériques comme prédicteurs
        num_preds <- names(data)[sapply(data, is.numeric) & names(data) != outcome]
        if (length(num_preds) == 0) stop("Aucune variable numérique pour la ROC.")

        # Modèle logistique pour probabilités prédites
        vars_roc <- num_preds[sapply(num_preds, function(v) !all(is.na(data[[v]])))]
        df_roc <- data
        if (!is.numeric(df_roc[[outcome]])) df_roc[[outcome]] <- as.numeric(as.factor(df_roc[[outcome]])) - 1
        formula_roc <- paste0("`", outcome, "` ~ ", paste(paste0("`", vars_roc, "`"), collapse = " + "))
        mod_roc <- stats::glm(stats::as.formula(formula_roc), data = df_roc, family = stats::binomial())
        probs   <- stats::fitted(mod_roc)

        # Calcul ROC
        roc_obj  <- pROC::roc(df_roc[[outcome]], probs, quiet = TRUE)
        auc_val  <- round(as.numeric(pROC::auc(roc_obj)), 3)
        ci_auc   <- pROC::ci.auc(roc_obj, conf.level = 0.95)

        # Tableau récapitulatif ROC
        roc_df <- data.frame(
          Métrique = c("AUC (Aire sous la courbe)", "IC95% AUC (DeLong)",
                       "Seuil optimal (Youden)", "Sensibilité au seuil", "Spécificité au seuil"),
          Valeur   = c(
            format(auc_val, decimal.mark = ","),
            paste0("[", format(round(ci_auc[1], 3), decimal.mark = ","), " — ",
                   format(round(ci_auc[3], 3), decimal.mark = ","), "]"),
            tryCatch({
              coords_opt <- pROC::coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"))
              c(format(round(coords_opt[1,1], 3), decimal.mark = ","),
                paste0(format(round(coords_opt[1,2]*100, 1), decimal.mark = ","), " %"),
                paste0(format(round(coords_opt[1,3]*100, 1), decimal.mark = ","), " %"))
            }, error = function(e) rep("N/D", 3))
          ),
          stringsAsFactors = FALSE
        )
        doc <- .add_ft(doc, .df_to_ft(roc_df, "Indicateurs de performance ROC"))
        doc <- officer::body_add_par(doc, "", style = "Normal")

        # Interprétation AUC
        auc_interp <- dplyr::case_when(
          auc_val >= 0.90 ~ "Excellente discrimination (AUC ≥ 0.90)",
          auc_val >= 0.80 ~ "Bonne discrimination (0.80 ≤ AUC < 0.90)",
          auc_val >= 0.70 ~ "Discrimination acceptable (0.70 ≤ AUC < 0.80)",
          auc_val >= 0.60 ~ "Discrimination faible (0.60 ≤ AUC < 0.70)",
          TRUE            ~ "Pas de discrimination (AUC < 0.60)"
        )
        doc <- officer::body_add_par(doc,
          paste0("Interprétation : ", auc_interp, ". AUC = ", auc_val,
                 " IC95% [", round(ci_auc[1],3), " — ", round(ci_auc[3],3), "]."),
          style = "Normal")

        # Graphique ROC
        if (include_plots) {
          p_roc <- pROC::ggroc(roc_obj, colour = "#0284c7", size = 1.2) +
            ggplot2::geom_abline(slope = 1, intercept = 1, linetype = "dashed",
                                 colour = "grey60", alpha = 0.8) +
            ggplot2::annotate("text", x = 0.3, y = 0.1,
                              label = paste0("AUC = ", auc_val),
                              size = 5, fontface = "bold", colour = "#0284c7") +
            ggplot2::labs(title = paste("Courbe ROC —", .get_label(data, outcome, outcome)),
                          x = "1 — Spécificité", y = "Sensibilité") +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
          tmp_roc <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp_roc, plot = p_roc, width = 7, height = 5.5, dpi = 150)
          doc <- officer::body_add_img(doc, src = tmp_roc, width = 5, height = 4)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }, error = function(e) {
        .warn("Section ROC non disponible : ", e$message)
        doc <<- officer::body_add_par(doc,
          paste("ROC non disponible :", e$message), style = "Normal")
      })
    } else if (!is.null(outcome)) {
      if (!requireNamespace("pROC", quietly = TRUE))
        .warn("Package 'pROC' requis pour la courbe ROC. Installez-le : install.packages('pROC')")
    }
  }

  # ============================================================
  # SECTION : survival (Kaplan-Meier, si time_var + outcome fournis)
  # ============================================================
  if ("survival" %in% sections && !is.null(time_var) && !is.null(outcome)) {
    if (requireNamespace("survival", quietly = TRUE)) {
      .log("Analyse de survie (Kaplan-Meier)...")
      doc <- officer::body_add_par(doc, .next_sec("Analyse de survie (Kaplan-Meier)"), style = "heading 1")

      tryCatch({
        t_var <- data[[time_var]]
        e_var <- data[[outcome]]
        if (!is.numeric(t_var)) stop("time_var doit être numérique.")
        if (!is.numeric(e_var)) e_var <- as.numeric(as.factor(e_var)) - 1

        surv_obj <- survival::Surv(t_var, e_var)

        if (!is.null(group_var) && group_var %in% names(data)) {
          # KM par groupe
          g_var    <- data[[group_var]]
          km_fit   <- survival::survfit(surv_obj ~ g_var)
          km_diff  <- survival::survdiff(surv_obj ~ g_var)
          p_logrank <- 1 - stats::pchisq(km_diff$chisq, df = length(km_diff$n) - 1)

          # Tableau des médianes de survie
          km_sum <- summary(km_fit)$table
          med_df <- as.data.frame(km_sum[, c("records","events","median"), drop = FALSE])
          med_df <- data.frame(Groupe = rownames(med_df), med_df,
                               row.names = NULL, check.names = FALSE,
                               stringsAsFactors = FALSE)
          names(med_df) <- c("Groupe", "N", "Événements", "Médiane de survie")
          p_str  <- if (p_logrank < 0.001) "< 0,001" else format(round(p_logrank, 3), decimal.mark = ",")
          doc <- officer::body_add_par(doc,
            paste("Analyse stratifiée par :", .get_label(data, group_var, group_var)), style = "heading 2")
          doc <- .add_ft(doc, .df_to_ft(med_df, "Médianes de survie par groupe"))
          doc <- officer::body_add_par(doc, "", style = "Normal")
          doc <- officer::body_add_par(doc,
            paste0("Test du log-rank : p = ", p_str,
                   if (p_logrank < 0.05) " (différence statistiquement significative)" else " (différence non significative)"),
            style = "Normal")

          # Courbe KM
          if (include_plots && requireNamespace("survminer", quietly = TRUE)) {
            p_km <- survminer::ggsurvplot(
              km_fit, data = data,
              pval = TRUE, pval.method = TRUE,
              conf.int = TRUE, risk.table = TRUE,
              ggtheme = ggplot2::theme_minimal(base_size = 11),
              palette = c("#0284c7","#dc2626","#059669","#d97706"),
              title = paste("Courbes de Kaplan-Meier —", .get_label(data, outcome, outcome))
            )
            tmp_km <- tempfile(fileext = ".png")
            ggplot2::ggsave(tmp_km, plot = survminer:::.build_ggsurvplot(p_km),
                            width = 9, height = 6.5, dpi = 150)
            doc <- officer::body_add_img(doc, src = tmp_km, width = 6.3, height = 4.5)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          } else if (include_plots) {
            # Fallback : courbe KM avec plot de base
            tmp_km <- tempfile(fileext = ".png")
            grDevices::png(tmp_km, width = 900, height = 620, res = 120)
            plot(km_fit, col = c("#0284c7","#dc2626"),
                 xlab = .get_label(data, time_var, time_var),
                 ylab = "Probabilité de survie",
                 main = paste("Kaplan-Meier —", .get_label(data, outcome, outcome)),
                 lwd = 2)
            legend("topright", legend = levels(as.factor(data[[group_var]])),
                   col = c("#0284c7","#dc2626"), lwd = 2, bty = "n")
            grDevices::dev.off()
            doc <- officer::body_add_img(doc, src = tmp_km, width = 6.3, height = 4.3)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }

        } else {
          # KM global (sans groupe)
          km_fit  <- survival::survfit(surv_obj ~ 1)
          km_med  <- summary(km_fit)$table["median"]
          km_q1   <- km_fit$time[which.min(abs(km_fit$surv - 0.75))]
          km_q3   <- km_fit$time[which.min(abs(km_fit$surv - 0.25))]
          n_ev    <- km_fit$n.event[length(km_fit$n.event)]

          km_df <- data.frame(
            Indicateur = c("N total", "N événements", "Médiane de survie",
                           "Q1 (75% de survie)", "Q3 (25% de survie)"),
            Valeur     = c(km_fit$n, sum(km_fit$n.event),
                           format(round(km_med, 1), decimal.mark = ","),
                           format(round(km_q1, 1),  decimal.mark = ","),
                           format(round(km_q3, 1),  decimal.mark = ",")),
            stringsAsFactors = FALSE
          )
          doc <- .add_ft(doc, .df_to_ft(km_df, "Résumé de la courbe de survie"))
          doc <- officer::body_add_par(doc, "", style = "Normal")

          if (include_plots) {
            tmp_km <- tempfile(fileext = ".png")
            grDevices::png(tmp_km, width = 900, height = 600, res = 120)
            plot(km_fit, col = "#0284c7",
                 xlab = .get_label(data, time_var, time_var),
                 ylab = "Probabilité de survie",
                 main = "Courbe de Kaplan-Meier (population globale)",
                 lwd = 2, conf.int = TRUE)
            grDevices::dev.off()
            doc <- officer::body_add_img(doc, src = tmp_km, width = 6.3, height = 4.3)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
        }
      }, error = function(e) {
        .warn("Analyse de survie a échoué : ", e$message)
        doc <<- officer::body_add_par(doc,
          paste("Analyse de survie non disponible :", e$message), style = "Normal")
      })
    } else {
      .warn("Package 'survival' requis pour l'analyse de survie. Installez-le : install.packages('survival')")
    }
  }

  # ============================================================
  # SECTION : correlation
  # ============================================================
  if ("correlation" %in% sections) {
    num_vars <- names(data)[sapply(data, is.numeric)]
    if (length(num_vars) >= 2) {
      .log("Matrice de corrélations...")
      doc <- officer::body_add_par(doc, .next_sec("Matrice de corrélations"), style = "heading 1")

      tryCatch({
        ft_cor <- correlation_table(data, cols = num_vars, digits = digits, color = color)
        doc <- .add_ft(doc, ft_cor)
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }, error = function(e) .warn("correlation_table a échoué : ", e$message))

      if (include_plots && length(num_vars) >= 3) {
        tryCatch({
          p_cor <- plot_correlation(data, cols = num_vars)
          tmp_png <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp_png, plot = p_cor, width = 8, height = 7, dpi = 150)
          doc <- officer::body_add_par(doc, "Heatmap de corrélations", style = "heading 2")
          doc <- officer::body_add_img(doc, src = tmp_png, width = 6, height = 5.5)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }, error = function(e) .warn("Heatmap corrélation non disponible : ", e$message))
      }
    }
  }

  # ============================================================
  # Note méthodologique finale
  # ============================================================
  doc <- officer::body_add_par(doc, "Note méthodologique", style = "heading 1")
  doc <- officer::body_add_par(doc,
    paste0("Ce rapport a été généré automatiquement par le package R analytix",
           tryCatch(paste0(" (version ", packageVersion("analytix"), ")"), error = function(e) ""),
           ". Date de génération : ", format(Sys.Date(), "%d %B %Y"),
           ". Les analyses statistiques utilisent les fonctions de base R (stats), ",
           "flextable (mise en forme pleine page, police Times New Roman) et officer (export Word). ",
           "Les seuils de significativité retenus sont α = 0,05 sauf indication contraire."),
    style = "Normal")

  # ============================================================
  # Sauvegarde du document
  # ============================================================
  base::print(doc, target = output)
  if (verbose) cat("\n✅ Rapport généré avec succès :", output, "\n")

  if (open_doc) {
    if (.Platform$OS.type == "windows") tryCatch(shell.exec(output), error = function(e) NULL)
    else tryCatch(system(paste("xdg-open", shQuote(output)), wait = FALSE), error = function(e) NULL)
  }

  invisible(output)
}
