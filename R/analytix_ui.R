#' @title Lancer l'interface graphique Analytix-UI
#' @description Ouvre une interface Shiny interactive pour réaliser vos analyses sans coder.
#' @import shiny
#' @export
run_analytix_ui <- function() {
  
  ui <- bslib::page_navbar(
    title = "Analytix-UI",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    
    # --- Onglet 1 : Import ---
    bslib::nav_panel(
      title = "Données",
      icon = icon("table"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Chargement",
          radioButtons("data_source", "Source des données",
                       choices = c("Fichier" = "file", "Session R" = "session"),
                       selected = "file"),
          conditionalPanel(
            condition = "input.data_source == 'file'",
            fileInput("file", "Choisir un fichier (CSV, Excel, RDS)", accept = c(".csv", ".rds", ".xlsx", ".xls"))
          ),
          conditionalPanel(
            condition = "input.data_source == 'session'",
            selectInput("session_df", "Choisir un objet de la session", choices = NULL),
            actionButton("refresh_session", "Rafraîchir", icon = icon("sync"), class = "btn-sm")
          ),
          checkboxInput("use_example", "Utiliser l'exemple (iris)", FALSE),
          hr(),
          uiOutput("var_selector_ui")
        ),
        bslib::card(
          bslib::card_header("Aperçu des données"),
          tableOutput("data_preview")
        )
      )
    ),
    
    # --- Onglet 2 : Imputation ---
    bslib::nav_panel(
      title = "Imputation",
      icon = icon("magic"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          numericInput("mice_m", "Nombre d'imputations (m)", 5),
          numericInput("mice_it", "Itérations (maxit)", 5),
          actionButton("run_mice", "Lancer MICE", class = "btn-primary"),
          hr(),
          helpText("Cette opération crée un nouveau jeu de données 'donnees_imputees'.")
        ),
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Écart des données manquantes (Avant Imputation)"),
            uiOutput("missing_report_ui")
          ),
          bslib::card(
            bslib::card_header("Aperçu des données (5 premières lignes)"),
            tableOutput("data_preview")
          )
        )
      )
    ),
    
    # --- Onglet 3 : Analyse Univariée ---
    bslib::nav_panel(
      title = "Univarié",
      icon = icon("chart-bar"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          selectInput("uni_var", "Variable à analyser", choices = NULL),
          selectInput("uni_type", "Type détecté", choices = c("auto", "numeric", "categorical", "binary")),
          hr(),
          checkboxInput("uni_na", "Inclure les NA", FALSE),
          hr(),
          downloadButton("download_uni", "Télécharger Word", class = "btn-outline-secondary")
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header("Tableau de description"),
            uiOutput("uni_table_ui")
          ),
          bslib::card(
            bslib::card_header("Graphique"),
            plotOutput("uni_plot")
          )
        )
      )
    ),
    
    # --- Onglet 4 : Bivarié ---
    bslib::nav_panel(
      title = "Bivarié",
      icon = icon("project-diagram"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          selectInput("bi_target", "Variable Cible (Outcome)", choices = NULL),
          selectInput("bi_pred", "Prédicteurs", choices = NULL, multiple = TRUE),
          selectInput("bi_method", "Méthode d'analyse", choices = c("Comparaison de groupes" = "group", "OR Brute (Logistique)" = "logistic")),
          hr(),
          downloadButton("download_bi", "Télécharger Word", class = "btn-outline-secondary")
        ),
        bslib::card(
          bslib::card_header("Résultats de l'analyse bivariée"),
          uiOutput("bi_table_ui")
        )
      )
    ),
    
    # --- Onglet 5 : Recodage ---
    bslib::nav_panel(
      title = "Recodage",
      icon = icon("edit"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          selectInput("recode_var", "Variable à recoder", choices = NULL),
          uiOutput("recode_ui"),
          textInput("recode_na", "Valeur pour les NA", ""),
          actionButton("run_recode", "Appliquer le recodage", class = "btn-warning")
        ),
        bslib::card(
          bslib::card_header("Modalités après recodage potentiel"),
          tableOutput("recode_preview")
        )
      )
    ),
    
    # --- Onglet 6 : Export ---
    bslib::nav_panel(
      title = "Export",
      icon = icon("file-word"),
      bslib::card(
        bslib::card_header("Préparation du rapport Word"),
        textInput("word_path", "Nom du fichier", "rapport_analytix.docx"),
        helpText("Note : L'export inclura tous les tableaux générés dans la session actuelle."),
        actionButton("run_export", "Générer le rapport Word", class = "btn-success")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # --- Data Reactive ---
    observe({
      input$refresh_session
      dfs <- Filter(function(x) is.data.frame(get(x, envir = .GlobalEnv)), ls(envir = .GlobalEnv))
      updateSelectInput(session, "session_df", choices = dfs)
    })

    raw_data <- reactive({
      if (input$use_example) return(iris)
      
      if (input$data_source == "session") {
        req(input$session_df)
        return(get(input$session_df, envir = .GlobalEnv))
      }
      
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv = read.csv(input$file$datapath),
             xlsx = readxl::read_excel(input$file$datapath),
             xls = readxl::read_excel(input$file$datapath),
             rds = readRDS(input$file$datapath),
             stop("Format non supporté"))
    })
    
    current_data <- reactiveVal(NULL)
    observe({ 
      df <- try(raw_data(), silent = TRUE)
      if (!inherits(df, "try-error") && is.data.frame(df)) {
        current_data(df)
      }
    })
    
    # --- Update Selectors ---
    observe({
      df <- current_data()
      req(df)
      updateSelectInput(session, "uni_var", choices = names(df))
      updateSelectInput(session, "bi_target", choices = names(df))
      updateSelectInput(session, "bi_pred", choices = names(df))
      updateSelectInput(session, "recode_var", choices = names(df))
    })
    
    # --- Recode Logic ---
    output$recode_ui <- renderUI({
      req(current_data(), input$recode_var)
      mods <- unique(as.character(current_data()[[input$recode_var]]))
      mods <- mods[!is.na(mods)]
      
      lapply(mods, function(m) {
        textInput(paste0("recode_mod_", m), paste("Recoder :", m), value = m)
      })
    })

    observeEvent(input$run_recode, {
      req(current_data(), input$recode_var)
      df <- current_data()
      mods <- unique(as.character(df[[input$recode_var]]))
      mods <- mods[!is.na(mods)]
      
      recode_list <- list()
      for(m in mods) {
        new_val <- input[[paste0("recode_mod_", m)]]
        if(!is.null(new_val) && new_val != m) {
          recode_list[[m]] <- new_val
        }
      }
      
      if(length(recode_list) > 0 || input$recode_na != "") {
        na_val <- if(input$recode_na == "") NULL else input$recode_na
        
        tryCatch({
          # Utilisation de do.call pour passer la liste de recodages à quick_code
          new_df <- do.call(quick_code, c(list(data = df, var = rlang::sym(input$recode_var), .na = na_val), recode_list))
          current_data(new_df)
          showNotification("Variable recodée avec succès.", type = "message")
        }, error = function(e) {
          showNotification(paste("Erreur de recodage :", e$message), type = "error")
        })
      }
    })

    output$recode_preview <- renderTable({
      req(current_data(), input$recode_var)
      table(current_data()[[input$recode_var]], useNA = "always") |> 
        as.data.frame() |> 
        setNames(c("Modalité", "Effectif"))
    })
    
    # --- Previews & Reports ---
    output$data_preview <- renderTable({
      req(current_data())
      head(current_data())
    })
    
    output$missing_report_ui <- renderUI({
      req(current_data())
      res <- missing_report(current_data())
      flextable::htmltools_value(res$flextable)
    })
    
    # --- Imputation Logic ---
    observeEvent(input$run_mice, {
      req(current_data())
      shiny::withProgress(message = "Imputation en cours...", value = 0.5, {
        imp_data <- impute_mice(current_data(), m = input$mice_m, maxit = input$mice_it)
        current_data(imp_data)
        showNotification("Imputation terminée avec succès.", type = "message")
      })
    })

    # --- Univariate Logic ---
    uni_res <- reactive({
      req(current_data(), input$uni_var)
      type <- input$uni_type
      na_val <- if (input$uni_na) "always" else "no"
      tryCatch({
        if (type == "auto") {
          analyse_descriptive_multiple(current_data(), vars = input$uni_var)[[input$uni_var]]
        } else if (type == "numeric") {
          descr_numeric(current_data(), !!sym(input$uni_var))
        } else if (type == "categorical") {
          descr_categorial(current_data(), !!sym(input$uni_var), useNA = na_val)
        } else {
          descr_binary(current_data(), !!sym(input$uni_var))
        }
      }, error = function(e) {
        showNotification(paste("Erreur analyse univariée :", e$message), type = "error")
        NULL
      })
    })
    
    output$uni_table_ui <- renderUI({
      req(uni_res())
      flextable::htmltools_value(uni_res()$flextable)
    })
    
    output$uni_plot <- renderPlot({
      req(uni_res())
      uni_res()$plot
    })

    output$download_uni <- downloadHandler(
      filename = function() { paste0("analyse_univariee_", input$uni_var, ".docx") },
      content = function(file) {
        req(uni_res())
        export_to_word(path = file, uni_res())
      }
    )
    
    # --- Bivariate Logic ---
    bi_res <- reactive({
      req(current_data(), input$bi_target, input$bi_pred)
      
      if (input$bi_method == "logistic") {
        # Utilise cross_multi pour les OR
        cross_multi(current_data(), input$bi_target, input$bi_pred)
      } else {
        # Utilise descr_by_group pour chaque prédicteur
        # Ici on prend le premier pour la démo ou on pourrait boucler
        descr_by_group(current_data(), !!sym(input$bi_pred[1]), !!sym(input$bi_target))
      }
    })
    
    output$bi_table_ui <- renderUI({
      req(bi_res())
      # bi_res peut être un flextable (descr_by_group) ou une liste (cross_multi)
      ft <- if(inherits(bi_res(), "flextable")) bi_res() else bi_res()$flextable
      flextable::htmltools_value(ft)
    })

    output$download_bi <- downloadHandler(
      filename = function() { "analyse_bivariee.docx" },
      content = function(file) {
        req(bi_res())
        export_to_word(path = file, bi_res())
      }
    )
    
    # --- Export Logic ---
    observeEvent(input$run_export, {
      req(current_data())
      # On exporte l'environnement ou les objets créés
      # Pour l'UI, on va simplement exporter les derniers résultats affichés
      tabs <- list()
      if (!is.null(uni_res())) tabs$univarié <- uni_res()
      if (!is.null(bi_res())) tabs$bivarié <- bi_res()
      
      export_to_word(path = input$word_path, tabs)
      showNotification(paste("Rapport enregistré :", input$word_path), type = "message")
    })
  }
  
  shinyApp(ui, server)
}
