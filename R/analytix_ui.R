#' @title Lancer l'interface graphique Analytix-UI
#' @description Ouvre une interface Shiny interactive pour réaliser vos analyses sans coder.
#' @import shiny
#' @import bslib
#' @import shinyWidgets
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
          fileInput("file", "Choisir un fichier (CSV, Excel, RDS)", accept = c(".csv", ".rds", ".xlsx")),
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
    raw_data <- reactive({
      if (input$use_example) return(iris)
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv = read.csv(input$file$datapath),
             rds = readRDS(input$file$datapath),
             stop("Format non supporté"))
    })
    
    current_data <- reactiveVal(NULL)
    observe({ current_data(raw_data()) })
    
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
      head(current_data())
    })
    
    output$missing_report_ui <- renderUI({
      req(current_data())
      res <- missing_report(current_data())
      htmltools::HTML(flextable::as_html_widget(res$flextable)$x$html)
    })
    
    # --- Imputation Logic ---
    observeEvent(input$run_mice, {
      req(current_data())
      withProgress(message = 'Imputation en cours...', value = 0.5, {
        new_df <- impute_mice(current_data(), m = input$mice_m, maxit = input$mice_it)
        current_data(new_df)
      })
      showNotification("Données imputées avec succès !")
    })
    
    # --- Univariate Logic ---
    uni_res <- reactive({
      req(current_data(), input$uni_var)
      type <- input$uni_type
      if (type == "auto") {
        analyse_descriptive_multiple(current_data(), vars = input$uni_var)[[input$uni_var]]
      } else if (type == "numeric") {
        descr_numeric(current_data(), !!rlang::sym(input$uni_var))
      } else if (type == "binary") {
        descr_binary(current_data(), !!rlang::sym(input$uni_var), include_na = input$uni_na)
      } else {
        descr_categorial(current_data(), !!rlang::sym(input$uni_var), include_na = input$uni_na)
      }
    })
    
    output$uni_table_ui <- renderUI({
      req(uni_res())
      htmltools::HTML(flextable::as_html_widget(uni_res()$flextable)$x$html)
    })
    
    output$uni_plot <- renderPlot({
      req(uni_res())
      plot_distribution(uni_res())
    })
    
    # --- Bivariate Logic ---
    bi_res <- reactive({
      req(current_data(), input$bi_target, input$bi_pred)
      cross_multi(current_data(), !!rlang::sym(input$bi_target), input$bi_pred, method = input$bi_method)
    })
    
    output$bi_table_ui <- renderUI({
      req(bi_res())
      htmltools::HTML(flextable::as_html_widget(bi_res())$x$html)
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
