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
          actionButton("run_mice", "Lancer MICE", class = "btn-primary")
        ),
        bslib::card(
          bslib::card_header("Rapport de manquants avant imputation"),
          uiOutput("missing_report_ui")
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
          checkboxInput("uni_na", "Inclure les NA", FALSE)
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
          selectInput("bi_method", "Méthode OR", choices = c("logistic", "level"))
        ),
        bslib::card(
          bslib::card_header("Tableau d'associations (OR brute)"),
          uiOutput("bi_table_ui")
        )
      )
    ),
    
    # --- Onglet 5 : Export ---
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
      withProgress(message = 'Imputation en cours...', value = 0.5, {
        new_df <- try(impute_mice(current_data(), m = input$mice_m, maxit = input$mice_it))
        if (inherits(new_df, "try-error")) {
          showNotification(paste("Erreur lors de l'imputation :", attr(new_df, "condition")$message), type = "error")
        } else {
          current_data(new_df)
          showNotification("Données imputées avec succès !")
        }
      })
    })
    
    # --- Univariate Logic ---
    uni_res <- reactive({
      req(current_data(), input$uni_var)
      type <- input$uni_type
      tryCatch({
        if (type == "auto") {
          analyse_descriptive_multiple(current_data(), vars = input$uni_var)[[input$uni_var]]
        } else if (type == "numeric") {
          descr_numeric(current_data(), !!rlang::sym(input$uni_var))
        } else if (type == "binary") {
          descr_binary(current_data(), !!rlang::sym(input$uni_var), include_na = input$uni_na)
        } else {
          descr_categorial(current_data(), !!rlang::sym(input$uni_var), include_na = input$uni_na)
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
      plot_distribution(uni_res())
    })
    
    # --- Bivariate Logic ---
    bi_res <- reactive({
      req(current_data(), input$bi_target, input$bi_pred)
      tryCatch({
        cross_multi(current_data(), !!rlang::sym(input$bi_target), input$bi_pred, method = input$bi_method)
      }, error = function(e) {
        showNotification(paste("Erreur analyse bivariée :", e$message), type = "error")
        NULL
      })
    })
    
    output$bi_table_ui <- renderUI({
      req(bi_res())
      flextable::htmltools_value(bi_res())
    })
    
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
