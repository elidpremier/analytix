# ==============================================================================
# Module : Rapport Automatique (One-click Report)
# Objectif : Générer un rapport Word complet automatiquement à partir des données
# Utilise : analytix::generate_report() ou fallback interne
# ==============================================================================

# --- Fonction interne : détection du type de variable ---
.detect_var_type_gui <- function(x) {
  x_clean  <- x[!is.na(x)]
  n_unique <- length(unique(x_clean))
  if (is.logical(x))                      return("Binaire")
  if (is.numeric(x) && n_unique <= 2)     return("Binaire")
  if (is.numeric(x) && n_unique <= 10)    return("Numérique discrète")
  if (is.numeric(x))                      return("Numérique continue")
  if (is.factor(x) || is.character(x)) {
    if (n_unique <= 2) return("Binaire")
    return("Catégorielle")
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return("Date")
  return("Autre")
}

# --- Fallback : rapport minimal sans analytix::generate_report ---
.simple_report_fallback <- function(data, file, title, author, institution, outcome, sections) {
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (nchar(trimws(author)) > 0)
    doc <- officer::body_add_par(doc, paste("Auteur :", author), style = "Normal")
  if (nchar(trimws(institution)) > 0)
    doc <- officer::body_add_par(doc, paste("Institution :", institution), style = "Normal")
  doc <- officer::body_add_par(doc, paste("Date :", Sys.Date()), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  if ("summary" %in% sections) {
    doc <- officer::body_add_par(doc, "1. Aperçu des données", style = "heading 1")
    completude <- round((1 - sum(is.na(data)) / (nrow(data) * ncol(data))) * 100, 1)
    meta_df <- data.frame(
      Indicateur = c("Observations", "Variables", "Complétude"),
      Valeur     = c(nrow(data), ncol(data), paste0(completude, " %")),
      stringsAsFactors = FALSE
    )
    ft <- flextable::autofit(flextable::theme_vanilla(flextable::flextable(meta_df)))
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  if ("missing" %in% sections) {
    doc <- officer::body_add_par(doc, "2. Données manquantes", style = "heading 1")
    na_df <- data.frame(
      Variable    = names(data),
      `Nb NA`     = sapply(data, function(x) sum(is.na(x))),
      `Taux (%)`  = round(sapply(data, function(x) mean(is.na(x)) * 100), 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    ft_na <- flextable::autofit(flextable::theme_vanilla(flextable::flextable(na_df)))
    doc <- flextable::body_add_flextable(doc, ft_na)
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  if ("descriptive" %in% sections) {
    doc <- officer::body_add_par(doc, "3. Statistiques descriptives", style = "heading 1")
    num_vars <- names(data)[sapply(data, is.numeric)]
    if (length(num_vars) > 0) {
      doc <- officer::body_add_par(doc, "Variables numériques", style = "heading 2")
      summ_df <- as.data.frame(t(sapply(data[, num_vars, drop = FALSE], function(x) {
        c(N = sum(!is.na(x)), Moy = round(mean(x, na.rm = TRUE), 2),
          Méd = round(median(x, na.rm = TRUE), 2), SD = round(sd(x, na.rm = TRUE), 2),
          Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
      })))
      summ_df <- data.frame(Variable = rownames(summ_df), summ_df, row.names = NULL, check.names = FALSE)
      ft_summ <- flextable::autofit(flextable::theme_vanilla(flextable::flextable(summ_df)))
      doc <- flextable::body_add_flextable(doc, ft_summ)
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }
  }

  print(doc, target = file)
}

# ==============================================================================
# UI
# ==============================================================================
mod_report_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("bolt", class = "me-2 text-warning"),
        tags$strong("Rapport Automatique")
      ),
      width = 370,

      # Métadonnées
      textInput(ns("rep_title"),       "Titre du rapport :",
                value = "Rapport d'Analyse Statistique"),
      textInput(ns("rep_author"),      "Auteur / Investigateur :", value = ""),
      textInput(ns("rep_institution"), "Institution / Organisme :", value = ""),

      tags$hr(),

      # Variable outcome
      selectInput(
        ns("rep_outcome"),
        tags$span(icon("crosshairs", class = "me-1"), "Variable d'intérêt (outcome) :"),
        choices  = c("— Aucune (analyse descriptive seule) —" = ""),
        selected = ""
      ),

      tags$hr(),

      # Variables descriptives
      selectizeInput(
        ns("rep_vars"),
        tags$span(icon("list-ul", class = "me-1"), "Variables à décrire :"),
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Toutes les variables (par défaut)")
      ),

      # Variables bivariées
      selectizeInput(
        ns("rep_bivar_vars"),
        tags$span(icon("random", class = "me-1"), "Variables explicatives (Bivarié) :"),
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Mêmes que descriptives (par défaut)")
      ),

      tags$hr(),

      # Sections à inclure
      tags$h6(tags$strong("Sections à inclure :")),
      checkboxGroupInput(
        ns("rep_sections"),
        label   = NULL,
        choices = c(
          "Page de titre"            = "cover",
          "Synthèse du jeu de données" = "summary",
          "Données manquantes"       = "missing",
          "Statistiques descriptives"= "descriptive",
          "Analyses bivariées"       = "bivariate",
          "Régression logistique"    = "regression",
          "Matrice de corrélations"  = "correlation"
        ),
        selected = c("cover", "summary", "missing", "descriptive",
                     "bivariate", "regression", "correlation")
      ),

      checkboxInput(ns("rep_plots"), "Inclure les graphiques", value = TRUE),

      tags$hr(),

      # Boutons d'export
      downloadButton(
        ns("dl_magic_report_word"),
        label = tags$span(icon("wand-magic-sparkles", class = "me-2", lib="font-awesome"), "Magic Report en 1-clic !"),
        class = "btn btn-warning btn-lg w-100 mb-2 fw-bold text-dark"
      ),
      downloadButton(
        ns("dl_report_word"),
        label = tags$span(icon("file-word", class = "me-2"), "Générer le Rapport personnalisé (.docx)"),
        class = "btn btn-success btn-lg w-100 mb-2"
      ),
      downloadButton(
        ns("dl_vars_csv"),
        label = tags$span(icon("file-csv", class = "me-2"), "Export CSV des variables"),
        class = "btn btn-secondary btn-sm w-100"
      )
    ),

    # Main area
    tags$div(
      class = "container-fluid p-3",

      # --- Alertes ---
      uiOutput(ns("report_alert")),

      # --- Métriques rapides ---
      uiOutput(ns("quick_metrics")),

      # --- Tableau des variables ---
      bslib::card(
        bslib::card_header(
          tags$div(
            class = "d-flex align-items-center gap-2",
            icon("table", class = "text-primary"),
            tags$strong("Inventaire des variables détectées")
          )
        ),
        bslib::card_body(
          DT::DTOutput(ns("vars_table"))
        )
      ),

      # --- Info ---
      tags$div(
        class = "alert alert-success mt-3",
        icon("check-circle", class = "me-2"),
        tags$strong("Comment utiliser ce module : "),
        "Configurez les paramètres dans le panneau gauche, choisissez optionnellement
         une variable d'intérêt pour les analyses bivariées, puis cliquez sur
         'Générer le Rapport' pour obtenir votre document Word complet."
      )
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================
mod_report_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Mise à jour dynamique du selectInput outcome et vars ----
    observe({
      df <- data_reactive()
      if (is.null(df) || nrow(df) == 0) return()

      cols <- names(df)
      updateSelectInput(
        session, "rep_outcome",
        choices  = c("— Aucune (analyse descriptive seule) —" = "", cols),
        selected = ""
      )
      updateSelectizeInput(session, "rep_vars", choices = cols)
      updateSelectizeInput(session, "rep_bivar_vars", choices = cols)
    })

    # ---- Données de type de variable ----
    vars_summary <- reactive({
      df <- data_reactive()
      req(df, nrow(df) > 0)

      rows <- lapply(names(df), function(v) {
        lbl <- attr(df[[v]], "label")
        lbl <- if (!is.null(lbl) && nchar(trimws(lbl)) > 0) as.character(lbl) else v
        type <- .detect_var_type_gui(df[[v]])
        n_miss <- sum(is.na(df[[v]]))
        pct_miss <- round(mean(is.na(df[[v]])) * 100, 1)
        data.frame(
          Variable     = v,
          Libellé      = lbl,
          Type         = type,
          `Manquants`  = n_miss,
          `Taux NA (%)`= pct_miss,
          check.names  = FALSE,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    })

    # ---- Alerte si pas de données ----
    output$report_alert <- renderUI({
      df <- data_reactive()
      if (is.null(df) || nrow(df) == 0) {
        tags$div(
          class = "alert alert-warning d-flex align-items-center gap-2",
          icon("exclamation-triangle"),
          tags$div(
            tags$strong("Aucun jeu de données chargé. "),
            "Veuillez d'abord importer vos données dans l'onglet ",
            tags$strong("'1. Données & Libellés'"), "."
          )
        )
      }
    })

    # ---- Métriques rapides ----
    output$quick_metrics <- renderUI({
      df <- data_reactive()
      req(df, nrow(df) > 0)

      completude <- round((1 - sum(is.na(df)) / (nrow(df) * ncol(df))) * 100, 1)
      vt <- vars_summary()
      n_num  <- sum(vt$Type %in% c("Numérique continue", "Numérique discrète"))
      n_cat  <- sum(vt$Type == "Catégorielle")
      n_bin  <- sum(vt$Type == "Binaire")

      bslib::layout_column_wrap(
        width = 1/4,
        tags$div(
          class = "card text-center border-primary h-100",
          tags$div(class = "card-body py-3",
            tags$h3(class = "text-primary fw-bold", format(nrow(df), big.mark = " ")),
            tags$p(class = "text-muted mb-0 small", "Observations")
          )
        ),
        tags$div(
          class = "card text-center border-info h-100",
          tags$div(class = "card-body py-3",
            tags$h3(class = "text-info fw-bold", ncol(df)),
            tags$p(class = "text-muted mb-0 small", "Variables")
          )
        ),
        tags$div(
          class = "card text-center border-success h-100",
          tags$div(class = "card-body py-3",
            tags$h3(class = "text-success fw-bold", paste0(completude, "%")),
            tags$p(class = "text-muted mb-0 small", "Complétude")
          )
        ),
        tags$div(
          class = "card text-center border-warning h-100",
          tags$div(class = "card-body py-3",
            tags$h5(class = "fw-bold mt-1",
              tags$span(class = "badge bg-primary me-1", n_num, "Num."),
              tags$span(class = "badge bg-success me-1", n_cat, "Cat."),
              tags$span(class = "badge bg-warning text-dark", n_bin, "Bin.")
            ),
            tags$p(class = "text-muted mb-0 small", "Types de variables")
          )
        )
      )
    })

    # ---- Tableau DT des variables ----
    output$vars_table <- DT::renderDT({
      vt <- vars_summary()
      req(vt)

      DT::datatable(
        vt,
        options  = list(pageLength = 15, scrollX = TRUE, dom = "ftp"),
        rownames = FALSE,
        class    = "table table-sm table-striped"
      ) |>
        DT::formatStyle(
          "Type",
          backgroundColor = DT::styleEqual(
            c("Numérique continue", "Numérique discrète", "Catégorielle", "Binaire", "Date", "Autre"),
            c("#dbeafe", "#e0f2fe", "#dcfce7", "#fef9c3", "#f3f4f6", "#fee2e2")
          )
        ) |>
        DT::formatStyle(
          "Taux NA (%)",
          backgroundColor = DT::styleInterval(
            c(5, 20, 50),
            c("white", "#fef9c3", "#fed7aa", "#fecaca")
          )
        )
    })

    # ====================================================================
    # DOWNLOAD : Rapport Word complet
    # ====================================================================
    output$dl_report_word <- downloadHandler(
      filename = function() {
        paste0("Rapport_Analytix_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        df <- data_reactive()
        req(df, nrow(df) > 0)

        outcome_val <- if (nchar(trimws(input$rep_outcome)) == 0) NULL else input$rep_outcome
        sections_val <- input$rep_sections
        if (is.null(sections_val) || length(sections_val) == 0) {
          sections_val <- c("cover", "summary", "missing", "descriptive")
        }

        shiny::withProgress(message = "Génération du rapport Word...", value = 0, {
          shiny::incProgress(0.1, detail = "Initialisation...")

          tryCatch({
            # Essayer analytix::report_generate / generate_report si disponible
            gen_fn <- if (exists("report_generate", where = asNamespace("analytix"))) analytix::report_generate else if (exists("generate_report", where = asNamespace("analytix"))) analytix::generate_report else NULL

            if (!is.null(gen_fn)) {
              shiny::incProgress(0.3, detail = "Génération du rapport...")
              gen_fn(
                data          = df,
                output        = file,
                title         = input$rep_title,
                author        = input$rep_author,
                institution   = input$rep_institution,
                outcome       = outcome_val,
                vars          = if (length(input$rep_vars) == 0) NULL else input$rep_vars,
                bivariate_vars= if (length(input$rep_bivar_vars) == 0) NULL else input$rep_bivar_vars,
                sections      = sections_val,
                include_plots = isTRUE(input$rep_plots),
                verbose       = FALSE
              )
            } else {
              shiny::incProgress(0.3, detail = "Génération du rapport (mode simplifié)...")
              .simple_report_fallback(
                data        = df,
                file        = file,
                title       = input$rep_title,
                author      = input$rep_author,
                institution = input$rep_institution,
                outcome     = outcome_val,
                sections    = sections_val
              )
            }

            shiny::incProgress(1.0, detail = "Terminé !")
            shiny::showNotification(
              "✅ Rapport Word généré avec succès !",
              type     = "message",
              duration = 5
            )

          }, error = function(e) {
            shiny::showNotification(
              paste("❌ Erreur lors de la génération :", e$message),
              type     = "error",
              duration = 10
            )
            # Créer un document d'erreur minimal
            doc <- officer::read_docx()
            doc <- officer::body_add_par(doc, "Erreur de génération du rapport", style = "heading 1")
            doc <- officer::body_add_par(doc, as.character(e$message), style = "Normal")
            print(doc, target = file)
          })
        })
      }
    )
    # ====================================================================
    # DOWNLOAD : Magic Report Word
    # ====================================================================
    output$dl_magic_report_word <- downloadHandler(
      filename = function() {
        paste0("Magic_Report_Analytix_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        df <- data_reactive()
        req(df, nrow(df) > 0)
        
        shiny::withProgress(message = "Génération du Magic Report...", value = 0, {
          shiny::incProgress(0.2, detail = "Initialisation et nettoyage...")
          tryCatch({
            
            magic_fn <- if (exists("report_magic", where = asNamespace("analytix"))) analytix::report_magic else if (exists("magic_report", where = asNamespace("analytix"))) analytix::magic_report else NULL
            
            if (!is.null(magic_fn)) {
               magic_fn(
                 data = df, 
                 output = file, 
                 title = input$rep_title, 
                 outcome = NULL, 
                 open_doc = FALSE
               )
            } else {
               stop("La fonction report_magic n'est pas disponible. Rechargez le package.")
            }
            shiny::incProgress(1.0, detail = "Terminé !")
            shiny::showNotification("✨ Magic Report généré avec succès !", type = "message")
          }, error = function(e) {
            shiny::showNotification(paste("❌ Erreur :", e$message), type = "error")
            doc <- officer::read_docx()
            doc <- officer::body_add_par(doc, "Erreur lors du Magic Report", style = "heading 1")
            doc <- officer::body_add_par(doc, as.character(e$message), style = "Normal")
            print(doc, target = file)
          })
        })
      }
    )

    # ====================================================================
    # DOWNLOAD : CSV des variables
    # ====================================================================
    output$dl_vars_csv <- downloadHandler(
      filename = function() {
        paste0("variables_analytix_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        vt <- vars_summary()
        req(vt)
        write.csv(vt, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
}
