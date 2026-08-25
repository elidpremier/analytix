# ==============================================================================
# Module : Modélisation (Régression)
# Objectif : Permettre la réalisation de régressions linéaires et logistiques simples/multiples
# Outils : gtsummary, flextable, stats
# ==============================================================================

mod_modeling_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      title = "Paramètres du Modèle",
      
      selectInput(
        ns("model_type"),
        "Type de modèle :",
        choices = c(
          "Régression Linéaire (lm)" = "linear",
          "Régression Logistique (glm)" = "logistic"
        )
      ),
      
      selectInput(
        ns("dep_var"),
        "Variable Dépendante (Y) :",
        choices = NULL
      ),
      
      selectInput(
        ns("indep_vars"),
        "Variables Explicatives (X) :",
        choices = NULL,
        multiple = TRUE
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'logistic'", ns("model_type")),
        checkboxInput(ns("show_or"), "Afficher les Odds Ratios (OR)", value = TRUE)
      ),
      
      checkboxInput(ns("global_p"), "Ajouter la p-value globale (Anova)", value = FALSE),
      
      tags$hr(),
      tags$div(
        class = "btn-group btn-group-sm w-100",
        downloadButton(ns("dl_word"), "Word (.docx)", class = "btn-success btn-sm"),
        downloadButton(ns("dl_csv"), "CSV", class = "btn-secondary btn-sm")
      )
    ),
    
    bslib::card(
      bslib::card_header(
        tags$div(
          class = "d-flex justify-content-between align-items-center w-100",
          tags$span(icon("chart-line", class = "me-2"), " Résultats de la Régression"),
          tags$span(class = "badge bg-primary", "gtsummary::tbl_regression")
        )
      ),
      uiOutput(ns("model_ui"))
    )
  )
}

mod_modeling_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update selectors based on data
    observe({
      df <- data_reactive()
      req(df)
      cols <- names(df)
      
      updateSelectInput(session, "dep_var", choices = cols)
      
      # Independent vars
      current_indep <- isolate(input$indep_vars)
      if (is.null(current_indep)) {
        updateSelectInput(session, "indep_vars", choices = cols, selected = character(0))
      } else {
        updateSelectInput(session, "indep_vars", choices = cols, selected = current_indep)
      }
    })
    
    # Run the model
    model_res <- reactive({
      df <- data_reactive()
      
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer des données."),
        shiny::need(isTruthy(input$dep_var), "Sélectionnez une variable dépendante (Y)."),
        shiny::need(isTruthy(input$indep_vars) && length(input$indep_vars) > 0, "Sélectionnez au moins une variable explicative (X).")
      )
      
      y_var <- input$dep_var
      x_vars <- input$indep_vars
      
      shiny::validate(
        shiny::need(!(y_var %in% x_vars), "La variable dépendante ne peut pas être incluse dans les variables explicatives.")
      )
      
      # Formuler l'équation
      formula_str <- paste("`", y_var, "` ~ ", paste(paste0("`", x_vars, "`"), collapse = " + "), sep = "")
      mod_formula <- as.formula(formula_str)
      
      tryCatch({
        if (input$model_type == "linear") {
          # Check if Y is numeric
          shiny::validate(
            shiny::need(is.numeric(df[[y_var]]), "Pour une régression linéaire, la variable Y doit être numérique.")
          )
          
          fit <- lm(mod_formula, data = df)
          tbl <- gtsummary::tbl_regression(fit)
          
        } else {
          # Logistic regression
          y_vals <- na.omit(unique(df[[y_var]]))
          shiny::validate(
            shiny::need(length(y_vals) == 2, "Pour une régression logistique, la variable Y doit comporter exactement 2 modalités.")
          )
          
          # Convert to factor if not numeric/logical
          if (!is.numeric(df[[y_var]]) && !is.logical(df[[y_var]])) {
             df[[y_var]] <- as.factor(df[[y_var]])
          }
          
          fit <- glm(mod_formula, data = df, family = binomial)
          tbl <- gtsummary::tbl_regression(fit, exponentiate = input$show_or)
        }
        
        if (input$global_p) {
          tbl <- gtsummary::add_global_p(tbl)
        }
        
        tbl <- gtsummary::bold_p(tbl)
        tbl <- gtsummary::bold_labels(tbl)
        
        ft <- gtsummary::as_flex_table(tbl)
        ft <- flextable::bg(ft, part = "header", bg = "#0284c7")
        ft <- flextable::color(ft, part = "header", color = "white")
        ft <- flextable::autofit(ft)
        
        # Interpretation Pédagogique Automatique
        interp <- ""
        if (input$model_type == "linear") {
          r2 <- summary(fit)$r.squared
          adj_r2 <- summary(fit)$adj.r.squared
          interp <- paste0(
            "Le modèle explique ", round(r2 * 100, 1), "% de la variance de la variable cible (R² = ", round(r2, 3), "). ",
            "Le R² ajusté est de ", round(adj_r2, 3), "."
          )
        } else {
          interp <- "Les résultats affichent les effets des variables explicatives sur la probabilité de l'événement cible (Odds Ratios > 1 = augmentation du risque/chance)."
        }
        
        list(tbl = tbl, ft = ft, interp = interp)
        
      }, error = function(e) {
        showNotification(paste("Erreur de modélisation :", e$message), type = "error")
        NULL
      })
    })
    
    output$model_ui <- renderUI({
      res <- model_res()
      req(res$ft)
      
      tags$div(
        class = "mb-4 p-3 border rounded bg-white",
        style = "overflow-x: auto; width: 100%;",
        
        tags$div(
          class = "alert alert-info mb-3",
          icon("info-circle"), tags$strong(" Interprétation : "), res$interp
        ),
        
        flextable::htmltools_value(res$ft)
      )
    })
    
    # Word Export
    output$dl_word <- downloadHandler(
      filename = function() { paste0("Regression_", input$model_type, "_", Sys.Date(), ".docx") },
      content = function(file) {
        res <- model_res()
        req(res$ft)
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Résultats - Régression", ifelse(input$model_type == "linear", "Linéaire", "Logistique")), style = "heading 1")
        doc <- officer::body_add_par(doc, res$interp, style = "Normal")
        doc <- officer::body_add_par(doc, "", style = "Normal")
        doc <- flextable::body_add_flextable(doc, res$ft)
        print(doc, target = file)
      }
    )
    
    # CSV Export
    output$dl_csv <- downloadHandler(
      filename = function() { paste0("Regression_", input$model_type, "_", Sys.Date(), ".csv") },
      content = function(file) {
        res <- model_res()
        req(res$tbl)
        df_export <- as.data.frame(res$tbl)
        write.csv(df_export, file, row.names = FALSE)
      }
    )
    
    return(model_res)
  })
}
