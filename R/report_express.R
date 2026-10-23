#' @title Rapport Express — Générateur de rapport analytique automatisé
#' @description Pipeline complet de génération d'un rapport Word professionnel à partir
#' d'un fichier brut (Excel, CSV, export Kobo/ODK). En une seule commande, le rapport
#' couvre le nettoyage des données, l'analyse descriptive univariée, l'analyse bivariée
#' (si une variable de résultat est fournie), et une synthèse rédigée en français
#' grâce à un LLM externe (Groq ou Mistral AI) ou une interprétation heuristique locale.
#'
#' **Confidentialité** : les données individuelles ne quittent jamais la machine locale.
#' Seules les statistiques agrégées (tableaux de fréquences, moyennes, p-valeurs) sont
#' transmises à l'API externe si `use_ai = TRUE`.
#'
#' @param data data.frame ou chemin vers un fichier Excel (.xlsx/.xls) ou CSV.
#' @param output Chemin du fichier Word de sortie (défaut: `"Rapport_Express.docx"`).
#' @param title Titre principal du rapport (défaut: `"Rapport Express"`).
#' @param subtitle Sous-titre (optionnel).
#' @param author Nom de l'auteur (optionnel).
#' @param outcome Nom de la variable de résultat principal pour l'analyse bivariée.
#'   Si `NULL`, la fonction tente de la détecter automatiquement.
#'   Si `NA`, aucune analyse bivariée n'est effectuée.
#' @param exclude_vars Vecteur de noms de colonnes à exclure de l'analyse (ex: identifiants).
#' @param likert_vars Vecteur de noms de colonnes Likert (détection manuelle).
#' @param multi_choice_vars Vecteur de noms de colonnes à choix multiples (préfixe commun).
#' @param digits Nombre de décimales pour les statistiques (défaut: 1).
#' @param color Couleur d'en-tête des tableaux flextable (défaut: `"#2C3E50"`).
#' @param use_ai Utiliser l'API LLM pour générer des synthèses rédigées (défaut: `TRUE`).
#'   Si aucune clé API n'est configurée, bascule automatiquement en mode heuristique local.
#' @param provider Fournisseur LLM : `"groq"` (défaut) ou `"mistral"`.
#' @param model Modèle LLM spécifique (optionnel).
#' @param context Contexte de l'analyse transmis au LLM, ex: "Enquête nutritionnelle
#'   auprès de 350 ménages au Niger, 2025."
#' @param open_doc Ouvrir le document Word après génération (défaut: `FALSE`).
#' @param verbose Afficher les messages de progression (défaut: `TRUE`).
#'
#' @return Le chemin vers le fichier `.docx` généré (invisible).
#'
#' @examples
#' \dontrun{
#'   # Exemple minimal avec un data.frame
#'   report_express(mtcars, output = "test_rapport.docx", use_ai = FALSE)
#'
#'   # Exemple complet avec un fichier Excel client
#'   report_express(
#'     data    = "data/enquete_kobo.xlsx",
#'     output  = "livrables/Rapport_Analyse_2025.docx",
#'     title   = "Analyse des données — Projet XYZ",
#'     author  = "IDO Elisée",
#'     outcome = "statut_nutrition",
#'     context = "Enquête nutritionnelle, 350 enfants < 5 ans, Bénin 2025.",
#'     use_ai  = TRUE,
#'     provider = "mistral"
#'   )
#' }
#'
#' @export
report_express <- function(data,
                            output           = "Rapport_Express.docx",
                            title            = "Rapport Express",
                            subtitle         = NULL,
                            author           = NULL,
                            outcome          = NULL,
                            exclude_vars     = NULL,
                            likert_vars      = NULL,
                            multi_choice_vars = NULL,
                            digits           = 1L,
                            color            = "#2C3E50",
                            use_ai           = TRUE,
                            provider         = c("groq", "mistral"),
                            model            = NULL,
                            context          = NULL,
                            open_doc         = FALSE,
                            verbose          = TRUE) {
  provider <- match.arg(provider)
  t_start  <- proc.time()

  .msg <- function(...) if (verbose) message(...)

  # ─────────────────────────────────────────────────────────────────────────────
  # 1. IMPORTATION & NETTOYAGE
  # ─────────────────────────────────────────────────────────────────────────────
  .msg("━━━ Étape 1 / 5 : Importation et nettoyage ━━━")

  if (is.character(data)) {
    .msg("  \u2192 Import du fichier : ", data)
    ext <- tolower(tools::file_ext(data))
    if (ext == "csv") {
      # Auto-détection du séparateur (virgule ou point-virgule)
      first_line <- readLines(data, n = 1L, warn = FALSE)
      n_comma     <- lengths(regmatches(first_line, gregexpr(",", first_line)))
      n_semicolon <- lengths(regmatches(first_line, gregexpr(";", first_line)))
      sep_detected <- if (n_comma >= n_semicolon) "," else ";"
      data <- prep_import(data, sep = sep_detected)
    } else {
      data <- prep_import(data)
    }
    .msg("  \u2192 Import OK : ", nrow(data), " lignes \u00d7 ", ncol(data), " colonnes")
  }

  if (!is.data.frame(data)) stop("`data` doit être un data.frame ou un chemin de fichier.")
  if (nrow(data) == 0)      stop("Le jeu de données est vide.")

  # Retirer les colonnes exclues
  if (!is.null(exclude_vars)) {
    vars_a_retirer <- intersect(exclude_vars, names(data))
    if (length(vars_a_retirer) > 0) {
      data <- data[, setdiff(names(data), vars_a_retirer), drop = FALSE]
      .msg("  → Colonnes exclues : ", paste(vars_a_retirer, collapse = ", "))
    }
  }

  n_lignes <- nrow(data)
  n_cols   <- ncol(data)
  .msg("  → Données prêtes : ", n_lignes, " lignes × ", n_cols, " colonnes")

  # ─────────────────────────────────────────────────────────────────────────────
  # 2. RAPPORT DE QUALITÉ DES DONNÉES
  # ─────────────────────────────────────────────────────────────────────────────
  .msg("━━━ Étape 2 / 5 : Qualité des données ━━━")

  qualite_tables <- list()

  # Rapport des valeurs manquantes
  tryCatch({
    tbl_missing <- report_missing(data, color = color)
    qualite_tables[["Valeurs manquantes"]] <- tbl_missing
    .msg("  → Rapport manquants : OK")
  }, error = function(e) {
    .msg("  → Rapport manquants : ERREUR (", conditionMessage(e), ")")
  })

  # ─────────────────────────────────────────────────────────────────────────────
  # 3. ANALYSE UNIVARIÉE AUTOMATIQUE
  # ─────────────────────────────────────────────────────────────────────────────
  .msg("━━━ Étape 3 / 5 : Analyse univariée automatique ━━━")

  # Déterminer les colonnes à analyser (hors outcome et Likert/multi-choice manuels)
  vars_speciales <- c(likert_vars, unlist(multi_choice_vars))
  vars_auto <- setdiff(names(data), c(vars_speciales,
                                       if (!is.null(outcome) && !is.na(outcome)) outcome else NULL))

  univar_tables <- list()

  # a) Analyse automatique par type (binaire, catégorielle, numérique)
  if (length(vars_auto) > 0) {
    .msg("  → desc_auto() sur ", length(vars_auto), " variables...")
    res_auto <- tryCatch(
      desc_auto(data[, vars_auto, drop = FALSE], digits = digits, color = color, verbose = verbose),
      error = function(e) { .msg("  ERREUR desc_auto : ", conditionMessage(e)); list() }
    )
    univar_tables <- c(univar_tables, res_auto)
    .msg("  → ", length(res_auto), " tableaux générés")
  }

  # b) Variables Likert (si spécifiées manuellement)
  if (!is.null(likert_vars) && length(likert_vars) > 0) {
    likert_present <- intersect(likert_vars, names(data))
    if (length(likert_present) > 0) {
      .msg("  → Analyse Likert sur ", length(likert_present), " variables...")
      for (v in likert_present) {
        tryCatch({
          res_l <- desc_likert(data, !!rlang::sym(v), digits = digits, color = color)
          lbl   <- .get_label(data, v, v)
          univar_tables[[lbl]] <- res_l
        }, error = function(e) {
          .msg("    Likert '", v, "' : ", conditionMessage(e))
        })
      }
    }
  }

  # c) Variables à choix multiples (si spécifiées)
  if (!is.null(multi_choice_vars) && length(multi_choice_vars) > 0) {
    .msg("  → Analyse multi-choix sur ", length(multi_choice_vars), " variables...")
    for (v in multi_choice_vars) {
      if (v %in% names(data)) {
        tryCatch({
          res_m <- desc_multi_choice(data, !!rlang::sym(v), digits = digits, color = color)
          lbl   <- .get_label(data, v, v)
          univar_tables[[lbl]] <- res_m
        }, error = function(e) {
          .msg("    Multi-choix '", v, "' : ", conditionMessage(e))
        })
      }
    }
  }

  .msg("  → Total tableaux univariés : ", length(univar_tables))

  # ─────────────────────────────────────────────────────────────────────────────
  # 4. ANALYSE BIVARIÉE (si variable de résultat disponible)
  # ─────────────────────────────────────────────────────────────────────────────
  bivar_tables <- list()

  do_bivariate <- !is.null(outcome) && !is.na(outcome)

  # Auto-détection de l'outcome si demandé
  if (!do_bivariate && is.null(outcome)) {
    .msg("━━━ Tentative de détection automatique de l'outcome ━━━")
    outcome <- .detect_outcome(data, exclude_vars = vars_speciales)
    if (!is.null(outcome)) {
      .msg("  → Outcome deviné : '", outcome, "'")
      do_bivariate <- TRUE
    } else {
      .msg("  → Aucun outcome évident détecté, analyse bivariée ignorée")
    }
  }

  if (do_bivariate && outcome %in% names(data)) {
    .msg("━━━ Étape 3b / 5 : Analyse bivariée (outcome = '", outcome, "') ━━━")
    vars_bivar <- setdiff(names(data), outcome)
    for (v in vars_bivar) {
      tryCatch({
        res_biv <- tbl_cross_unique(data,
                                     target = !!rlang::sym(outcome),
                                     !!rlang::sym(v),
                                     digits = digits,
                                     color  = color)
        lbl <- paste(.get_label(data, v, v), "×", .get_label(data, outcome, outcome))
        bivar_tables[[lbl]] <- res_biv
      }, error = function(e) NULL)
    }
    .msg("  → ", length(bivar_tables), " tableaux bivariés générés")
  }

  # ─────────────────────────────────────────────────────────────────────────────
  # 5. SYNTHÈSES RÉDIGÉES PAR LLM (ou fallback heuristique)
  # ─────────────────────────────────────────────────────────────────────────────
  .msg("━━━ Étape 4 / 5 : Rédaction des synthèses ━━━")

  syntheses <- list()

  if (use_ai && length(univar_tables) > 0) {
    tryCatch({
      syntheses <- llm_summarize_report(
        results  = univar_tables,
        context  = context,
        provider = provider,
        model    = model,
        verbose  = verbose
      )
      .msg("  → ", length(syntheses), " synthèses générées")
    }, error = function(e) {
      .msg("  → Erreur LLM : ", conditionMessage(e), " — mode heuristique activé")
      syntheses <<- lapply(names(univar_tables), function(nm) {
        .local_fallback_summary(.extract_table_text(univar_tables[[nm]]), nm)
      })
      names(syntheses) <<- names(univar_tables)
    })
  }

  # ─────────────────────────────────────────────────────────────────────────────
  # 6. EXPORT WORD
  # ─────────────────────────────────────────────────────────────────────────────
  .msg("━━━ Étape 5 / 5 : Export Word ━━━")

  if (!requireNamespace("officer", quietly = TRUE))   stop("Package 'officer' requis.")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' requis.")

  doc <- officer::read_docx()

  # Page de garde
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(subtitle)) doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  if (!is.null(author))   doc <- officer::body_add_par(doc, paste0("Auteur : ", author), style = "Normal")
  doc <- officer::body_add_par(doc, paste0("Date : ", format(Sys.Date(), "%d %B %Y")), style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Données : ", n_lignes, " observations × ", n_cols, " variables"), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_break(doc)

  # Section 1 : Qualité des données
  if (length(qualite_tables) > 0) {
    doc <- officer::body_add_par(doc, "1. Qualité des données", style = "heading 1")
    doc <- .add_tables_to_doc(doc, qualite_tables, syntheses = list(), verbose = verbose)
    doc <- officer::body_add_break(doc)
  }

  # Section 2 : Analyses univariées
  if (length(univar_tables) > 0) {
    doc <- officer::body_add_par(doc, "2. Analyse descriptive univariée", style = "heading 1")
    doc <- .add_tables_to_doc(doc, univar_tables, syntheses = syntheses, verbose = verbose)
    doc <- officer::body_add_break(doc)
  }

  # Section 3 : Analyses bivariées
  if (length(bivar_tables) > 0) {
    doc <- officer::body_add_par(doc,
      paste0("3. Analyse bivariée (variable de résultat : « ", outcome, " »)"),
      style = "heading 1")
    doc <- .add_tables_to_doc(doc, bivar_tables, syntheses = list(), verbose = verbose)
  }

  # Sauvegarde
  print(doc, target = output)

  t_end <- proc.time()
  elapsed <- round((t_end - t_start)["elapsed"], 1)

  .msg("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  .msg("✅ Rapport généré en ", elapsed, "s : ", output)
  .msg("   • Tableaux univariés : ", length(univar_tables))
  .msg("   • Tableaux bivariés  : ", length(bivar_tables))
  .msg("   • Synthèses rédigées : ", length(syntheses))
  .msg("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")

  if (open_doc && file.exists(output)) {
    tryCatch(
      utils::browseURL(output),
      error = function(e) .msg("  (Impossible d'ouvrir le document automatiquement)")
    )
  }

  invisible(output)
}

# ==============================================================================
# Fonctions internes
# ==============================================================================

#' Détecter automatiquement la variable de résultat la plus probable
#' @noRd
.detect_outcome <- function(data, exclude_vars = NULL) {
  candidats <- setdiff(names(data), exclude_vars)

  # Priorité 1 : variable logique
  for (v in candidats) {
    if (is.logical(data[[v]])) return(v)
  }
  # Priorité 2 : variable binaire (2 valeurs uniques) parmi les dernières colonnes
  for (v in rev(candidats)) {
    x <- data[[v]]
    if ((is.factor(x) || is.character(x) || is.numeric(x)) &&
        length(unique(stats::na.omit(x))) == 2) {
      return(v)
    }
  }
  NULL
}

#' Ajouter une liste de tableaux dans un document Word avec synthèses optionnelles
#' @noRd
.add_tables_to_doc <- function(doc, tables, syntheses = list(), verbose = FALSE) {
  nms <- names(tables)
  if (is.null(nms)) nms <- paste0("Tableau_", seq_along(tables))

  for (i in seq_along(tables)) {
    nm  <- nms[i]
    obj <- tables[[i]]

    # Titre de section
    doc <- officer::body_add_par(doc, nm, style = "heading 2")

    # Extraire et ajouter les flextables
    fts <- .extract_flextables(obj)
    for (ft in fts) {
      if (inherits(ft, "flextable")) {
        # Auto-paysage pour les tableaux larges
        n_cols_ft <- length(flextable::ncol_keys(ft))
        if (n_cols_ft >= 7) {
          doc <- officer::body_end_section_portrait(doc)
          doc <- flextable::body_add_flextable(doc, ft)
          doc <- officer::body_add_par(doc, "", style = "Normal")
          doc <- officer::body_end_section_landscape(doc)
        } else {
          doc <- flextable::body_add_flextable(doc, ft)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }
    }

    # Synthèse rédigée (si disponible pour cette variable)
    synth_text <- syntheses[[nm]]
    if (!is.null(synth_text) && nchar(trimws(synth_text)) > 0) {
      doc <- officer::body_add_par(doc,
        paste0("💡 Synthèse : ", synth_text),
        style = "Normal")
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }
  }

  doc
}

#' Extraire récursivement tous les objets flextable d'un objet analytix
#' @noRd
.extract_flextables <- function(obj) {
  if (inherits(obj, "flextable")) return(list(obj))
  if (is.list(obj)) {
    if ("flextable" %in% names(obj) && inherits(obj$flextable, "flextable")) {
      return(list(obj$flextable))
    }
    res <- list()
    for (sub in obj) {
      extracted <- .extract_flextables(sub)
      res <- c(res, extracted)
    }
    return(res)
  }
  list()
}
