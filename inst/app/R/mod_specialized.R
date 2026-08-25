#' Module Analyses Spécialisées (Likert Multi-Items, Choix Multiples, Heatmap, Diagnostique)
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame

# Helpers locally defined to avoid scoping errors
get_flextable <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (inherits(obj, "flextable")) {
    return(obj)
  }
  if (is.list(obj) && !is.null(obj$flextable) && inherits(obj$flextable, "flextable")) {
    return(obj$flextable)
  }
  if (is.list(obj) && !is.null(obj$table) && inherits(obj$table, "flextable")) {
    return(obj$table)
  }
  return(NULL)
}

get_dataframe <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (is.data.frame(obj)) {
    return(obj)
  }
  if (is.list(obj) && !is.null(obj$data) && is.data.frame(obj$data)) {
    return(obj$data)
  }
  if (is.list(obj) && !is.null(obj$body$dataset) && is.data.frame(obj$body$dataset)) {
    return(obj$body$dataset)
  }
  return(NULL)
}

mod_specialized_ui <- function(id) {
  ns <- NS(id)
  
  bslib::navset_card_tab(
    # Tab 1: Likert Scale (Multi-Items)
    bslib::nav_panel(
      title = tags$span(icon("star"), " Échelles de Likert"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Configuration Likert",
          width = 320,
          tags$h6(icon("list", class = "me-1"), "Analyse Individuelle"),
          selectInput(ns("likert_var"), "Variable Likert unique :", choices = NULL),
          
          tags$hr(),
          tags$h6(icon("layer-group", class = "me-1"), "Analyse Multi-Items (Tableau récapitulatif & Graphique Divergent)"),
          tags$p(class = "text-muted small", "Sélectionnez plusieurs variables Likert numériques pour générer un tableau synthétique et un graphique en barres divergentes."),
          selectInput(ns("multi_likert_vars"), "Variables Likert (multi-items) :", choices = NULL, multiple = TRUE),
          numericInput(ns("likert_n_levels"), "Nombre de niveaux de l'échelle (ex: 5) :", value = 5, min = 2, max = 10),
          textInput(ns("likert_level_labels"), "Libellés des niveaux (du - au +, séparés par virgules) :", placeholder = "ex: Pas du tout, Peu, Moyennement, Assez, Tout à fait"),
          textInput(ns("likert_plot_title"), "Titre du graphique :", value = "Répartition des réponses Likert")
        ),
        tags$div(
          class = "container-fluid p-0",

          # Analyse individuelle
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Résumé Échelle de Likert (Variable Unique)"),
                downloadButton(ns("dl_likert_word"), "Word (.docx)", class = "btn-outline-primary btn-sm")
              )
            ),
            uiOutput(ns("likert_ui"))
          ),

          tags$div(class = "mt-3"),

          # Multi-items synthèse
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Tableau Récapitulatif Multi-Items Likert"),
                tags$div(
                  class = "btn-group btn-group-sm",
                  downloadButton(ns("dl_multi_likert_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
                  downloadButton(ns("dl_multi_likert_csv"), "CSV", class = "btn-outline-secondary btn-sm")
                )
              )
            ),
            uiOutput(ns("multi_likert_table_ui"))
          ),

          tags$div(class = "mt-3"),

          # Graphique divergent
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("chart-bar", class = "me-2"), " Graphique Divergent Likert Multi-Items"),
                tags$div(
                  class = "btn-group btn-group-sm",
                  downloadButton(ns("dl_likert_div_png"), "PNG", class = "btn-outline-success btn-sm"),
                  downloadButton(ns("dl_likert_div_pdf"), "PDF", class = "btn-outline-danger btn-sm")
                )
              )
            ),
            plotOutput(ns("likert_plot"), height = "420px")
          )
        )
      )
    ),
    
    # Tab 2: Multiple Choice
    bslib::nav_panel(
      title = tags$span(icon("list-check"), " Choix Multiples"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Questions Multiples",
          width = 320,
          selectInput(ns("multi_vars"), "Colonnes composant la question :", choices = NULL, multiple = TRUE),
          tags$hr(),
          tags$h6(icon("project-diagram", class = "me-1"), "Croisement par Groupe (Optionnel)"),
          tags$p(class = "text-muted small", "Sélectionnez un groupe pour comparer les fréquences des réponses multiples entre groupes."),
          selectInput(ns("multi_group_var"), "Variable de groupe (optionnel) :", choices = NULL),
          uiOutput(ns("multi_group_pos_ui"))
        ),
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Analyse des Réponses Multiples"),
                tags$div(
                  class = "btn-group btn-group-sm",
                  downloadButton(ns("dl_multi_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
                  downloadButton(ns("dl_multi_csv"), "CSV", class = "btn-outline-secondary btn-sm")
                )
              )
            ),
            uiOutput(ns("multi_ui"))
          )
        )
      )
    ),
    
    # Tab 3: Heatmap Correlation Matrix
    bslib::nav_panel(
      title = tags$span(icon("th"), " Heatmap & Corrélations"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Variables Numériques",
          width = 320,
          selectInput(ns("heatmap_vars"), "Sélectionner les variables (min 2) :", choices = NULL, multiple = TRUE),
          selectInput(ns("cor_method"), "Méthode de corrélation :",
                      choices = c("Pearson" = "pearson", "Spearman" = "spearman"), selected = "pearson"),
          numericInput(ns("cor_digits"), "Décimales :", value = 2, min = 0, max = 4),
          numericInput(ns("cor_sig_level"), "Seuil de significativité (gras) :", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
          textInput(ns("cor_header_color"), "Couleur de l'en-tête (Hex) :", value = "#0284c7")
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("image", class = "me-2"), " Carte Thermique des Corrélations"),
                tags$div(
                  class = "btn-group btn-group-sm",
                  downloadButton(ns("dl_hm_png"), "PNG", class = "btn-outline-success btn-sm"),
                  downloadButton(ns("dl_hm_pdf"), "PDF", class = "btn-outline-danger btn-sm")
                )
              )
            ),
            plotOutput(ns("heatmap_plot"), height = "450px")
          ),
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Matrice de Corrélations (Coefficients)"),
                downloadButton(ns("dl_cor_word"), "Word (.docx)", class = "btn-outline-primary btn-sm")
              )
            ),
            uiOutput(ns("correlation_table_ui"))
          )
        )
      )
    ),

    # Tab 4: Diagnostic Performance
    bslib::nav_panel(
      title = tags$span(icon("stethoscope"), " Performance Diagnostique"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Indicateurs Diagnostiques",
          width = 320,
          selectInput(ns("diag_actual"), "Standard de référence / Gold Standard :", choices = NULL),
          selectInput(ns("diag_predicted"), "Test diagnostique à évaluer :", choices = NULL),
          uiOutput(ns("diag_pos_ui")),
          numericInput(ns("diag_conf_level"), "Niveau de confiance (%) :", value = 95, min = 50, max = 99),
          numericInput(ns("diag_digits"), "Décimales :", value = 1, min = 0, max = 4),
          textInput(ns("diag_color"), "Couleur de l'en-tête (Hex) :", value = "#0284c7")
        ),
        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("table", class = "me-2"), " Indicateurs diagnostiques (Sensibilité, Spécificité, VPP, VPN, LR+, LR-)"),
              tags$div(
                class = "btn-group btn-group-sm",
                downloadButton(ns("dl_diag_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
                downloadButton(ns("dl_diag_csv"), "CSV", class = "btn-outline-secondary btn-sm")
              )
            )
          ),
          uiOutput(ns("diagnostic_results_ui"))
        )
      )
    )
  )
}

mod_specialized_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    last_heatmap_vars <- reactiveVal(NULL)
    
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "likert_var", choices = names(df))
      updateSelectInput(session, "multi_likert_vars", choices = names(df))
      updateSelectInput(session, "multi_vars", choices = names(df))
      updateSelectInput(session, "multi_group_var", choices = c("(Aucun)" = "", names(df)))
      
      num_vars <- names(df)[sapply(df, is.numeric)]
      
      if (!identical(num_vars, last_heatmap_vars())) {
        last_heatmap_vars(num_vars)
        
        current_hm <- isolate(input$heatmap_vars)
        if (is.null(current_hm)) {
          updateSelectInput(session, "heatmap_vars", choices = num_vars, selected = character(0))
        } else {
          valid_hm <- current_hm[current_hm %in% num_vars]
          updateSelectInput(session, "heatmap_vars", choices = num_vars, selected = valid_hm)
        }
      }
      
      updateSelectInput(session, "diag_actual", choices = names(df))
      updateSelectInput(session, "diag_predicted", choices = names(df))
    })

    output$diag_pos_ui <- renderUI({
      df <- data_reactive()
      req(df, input$diag_actual)
      vals <- unique(df[[input$diag_actual]])
      vals <- vals[!is.na(vals)]
      selectInput(ns("diag_pos_val"), "Valeur du cas positif :", choices = vals)
    })

    output$multi_group_pos_ui <- renderUI({
      df <- data_reactive()
      req(df)
      if (!isTruthy(input$multi_group_var) || input$multi_group_var == "") return(NULL)
      vals <- unique(df[[input$multi_group_var]])
      vals <- vals[!is.na(vals)]
      selectInput(ns("multi_group_pos_val"), "Valeur d'intérêt du groupe :", choices = vals)
    })
    
    # 1. Likert Individual logic
    likert_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$likert_var), "Veuillez sélectionner une variable Likert.")
      )
      var_nm <- input$likert_var
      sym_v <- rlang::sym(var_nm)
      
      if (exists("descr_likert", where = asNamespace("analytix"))) {
        res <- tryCatch(analytix::descr_likert(df, var = !!sym_v), error = function(e) NULL)
        if (!is.null(res)) return(res)
      }
      
      # Fallback Likert summary
      tb <- table(df[[var_nm]], useNA = "no")
      data.frame(Modalite = names(tb), Effectif = as.numeric(tb), Pourcentage = paste0(round(prop.table(tb)*100, 1), "%"))
    })
    
    output$likert_ui <- renderUI({
      res <- likert_res()
      req(res)
      ft <- get_flextable(res)
      if (!is.null(ft)) {
        flextable::htmltools_value(ft)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_likert"))
      }
    })
    output$raw_likert <- renderTable({ likert_res() }, striped = TRUE, hover = TRUE)

    # 2. Multi-items Likert table
    multi_likert_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données."),
        shiny::need(isTruthy(input$multi_likert_vars) && length(input$multi_likert_vars) >= 2, "Veuillez sélectionner au moins 2 variables Likert pour le tableau récapitulatif.")
      )
      vars <- input$multi_likert_vars

      if (exists("multi_likert_table", where = asNamespace("analytix"))) {
        res <- tryCatch(analytix::multi_likert_table(df, cols = vars), error = function(e) NULL)
        if (!is.null(res)) return(res)
      }

      # Fallback simple table
      rows <- lapply(vars, function(cn) {
        vec <- suppressWarnings(as.numeric(df[[cn]]))
        vec_clean <- vec[!is.na(vec)]
        data.frame(
          Variable = cn,
          N = length(vec_clean),
          Moyenne = round(mean(vec_clean), 2),
          Médiane = round(median(vec_clean), 2),
          `Écart-type` = round(sd(vec_clean), 2),
          check.names = FALSE
        )
      })
      dplyr::bind_rows(rows)
    })

    output$multi_likert_table_ui <- renderUI({
      res <- tryCatch(multi_likert_res(), error = function(e) NULL)
      req(res)
      ft <- get_flextable(res)
      if (!is.null(ft)) {
        flextable::htmltools_value(ft)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_multi_likert"))
      }
    })
    output$raw_multi_likert <- renderTable({ tryCatch(multi_likert_res(), error = function(e) NULL) }, striped = TRUE, hover = TRUE)

    # 3. Likert Divergent Plot (Multi-Items)
    likert_plot_res <- reactive({
      df <- data_reactive()

      # If multi_likert_vars selected, use those; otherwise fall back to single var
      vars_to_use <- if (isTruthy(input$multi_likert_vars) && length(input$multi_likert_vars) >= 1) {
        input$multi_likert_vars
      } else {
        req(input$likert_var)
        input$likert_var
      }

      n_levels <- as.integer(input$likert_n_levels)
      if (is.na(n_levels) || n_levels < 2) n_levels <- 5

      # Parse level labels
      level_labels_val <- NULL
      lbl_str <- trimws(input$likert_level_labels)
      if (nchar(lbl_str) > 0) {
        level_labels_val <- trimws(strsplit(lbl_str, ",")[[1]])
        if (length(level_labels_val) != n_levels) level_labels_val <- NULL
      }

      plot_title <- trimws(input$likert_plot_title)
      if (nchar(plot_title) == 0) plot_title <- "Répartition des réponses Likert"

      if (exists("plot_likert_divergent", where = asNamespace("analytix"))) {
        p <- tryCatch(
          analytix::plot_likert_divergent(
            df,
            cols = vars_to_use,
            n_levels = n_levels,
            level_labels = level_labels_val,
            title = plot_title
          ),
          error = function(e) NULL
        )
        if (!is.null(p)) return(p)
      }

      # Fallback simple bar chart for first var
      req(isTruthy(input$likert_var))
      var_nm <- input$likert_var
      vec <- df[[var_nm]]
      if (!is.numeric(vec)) {
        fact <- as.factor(vec)
        df_copy <- df
        df_copy[[var_nm]] <- as.numeric(fact)
        lvls <- levels(fact)
        n_lvls <- length(lvls)
        if (exists("plot_likert_divergent", where = asNamespace("analytix"))) {
          analytix::plot_likert_divergent(df_copy, cols = var_nm, n_levels = n_lvls, level_labels = lvls)
        } else NULL
      } else {
        n_lvls <- length(unique(na.omit(vec)))
        if (exists("plot_likert_divergent", where = asNamespace("analytix"))) {
          analytix::plot_likert_divergent(df, cols = var_nm, n_levels = max(c(5, n_lvls)))
        } else NULL
      }
    })

    output$likert_plot <- renderPlot({
      p <- likert_plot_res()
      shiny::validate(
        shiny::need(!is.null(p), "Le graphique de Likert divergent n'est pas disponible pour cette variable. Assurez-vous que la variable est numérique.")
      )
      p
    })
    
    # 4. Multi choice logic
    multi_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$multi_vars) && length(input$multi_vars) > 0, "Veuillez sélectionner des variables pour les choix multiples.")
      )
      vars <- input$multi_vars

      # Check if a grouping variable is selected
      group_var <- if (isTruthy(input$multi_group_var) && input$multi_group_var != "") input$multi_group_var else NULL

      if (exists("descr_multi_choice", where = asNamespace("analytix"))) {
        res <- tryCatch({
          if (!is.null(group_var)) {
            # Try with group argument if supported
            tryCatch(
              analytix::descr_multi_choice(df, vars = vars, group = group_var),
              error = function(e) analytix::descr_multi_choice(df, vars = vars)
            )
          } else {
            analytix::descr_multi_choice(df, vars = vars)
          }
        }, error = function(e) NULL)
        if (!is.null(res)) return(res)
      }
      
      # Fallback
      if (!is.null(group_var) && isTruthy(group_var)) {
        group_vals <- unique(na.omit(df[[group_var]]))
        rows_list <- lapply(vars, function(v) {
          row_data <- data.frame(Option = v, stringsAsFactors = FALSE)
          total_cite <- sum(df[[v]] == 1 | df[[v]] == "Oui" | df[[v]] == TRUE, na.rm = TRUE)
          row_data[["Total Citations"]] <- total_cite
          row_data[["% Global"]] <- paste0(round(total_cite / nrow(df) * 100, 1), "%")
          for (gv in group_vals) {
            sub_df <- df[!is.na(df[[group_var]]) & df[[group_var]] == gv, ]
            n_sub <- sum(sub_df[[v]] == 1 | sub_df[[v]] == "Oui" | sub_df[[v]] == TRUE, na.rm = TRUE)
            row_data[[paste0(gv, " (n, %)")]] <- sprintf("%d (%.1f%%)", n_sub, n_sub / max(nrow(sub_df), 1) * 100)
          }
          row_data
        })
        return(dplyr::bind_rows(rows_list))
      }

      counts <- sapply(df[vars], function(x) sum(x == 1 | x == "Oui" | x == TRUE, na.rm = TRUE))
      data.frame(Option = vars, Citations = counts, `% Participants` = paste0(round(counts/nrow(df)*100, 1), "%"), check.names = FALSE)
    })
    
    output$multi_ui <- renderUI({
      res <- multi_res()
      req(res)
      ft <- get_flextable(res)
      if (!is.null(ft)) {
        flextable::htmltools_value(ft)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_multi"))
      }
    })
    output$raw_multi <- renderTable({ multi_res() }, striped = TRUE, hover = TRUE)
    
    # 5. Heatmap & Correlation logic
    current_hm_plot <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$heatmap_vars) && length(input$heatmap_vars) >= 2, "Veuillez sélectionner au moins 2 variables numériques pour la heatmap.")
      )
      vars <- input$heatmap_vars
      cor_method <- input$cor_method

      if (exists("plot_correlation", where = asNamespace("analytix"))) {
        p <- tryCatch(analytix::plot_correlation(df, cols = vars, method = cor_method), error = function(e) NULL)
        if (!is.null(p) && inherits(p, "ggplot")) return(p)
      }

      if (exists("plot_heatmap_matrix", where = asNamespace("analytix"))) {
        p <- tryCatch(analytix::plot_heatmap_matrix(df, vars = vars), error = function(e) NULL)
        if (!is.null(p) && inherits(p, "ggplot")) return(p)
      }
      
      sub_df <- na.omit(df[vars])
      cm <- cor(sub_df, method = cor_method)
      df_cm <- as.data.frame(as.table(cm))
      
      ggplot2::ggplot(df_cm, ggplot2::aes(x = Var1, y = Var2, fill = Freq)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "#0284c7", high = "#e11d48", mid = "white", midpoint = 0, limit = c(-1,1)) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = "Matrice de Corrélations", x = "", y = "")
    })
    
    output$heatmap_plot <- renderPlot({ current_hm_plot() })

    cor_table_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$heatmap_vars) && length(input$heatmap_vars) >= 2, "Veuillez sélectionner au moins 2 variables numériques.")
      )
      vars <- input$heatmap_vars
      cor_method <- input$cor_method
      cor_digits <- as.integer(input$cor_digits)
      sig_level <- as.numeric(input$cor_sig_level)
      header_color <- input$cor_header_color
      if (is.null(header_color) || nchar(trimws(header_color)) == 0) header_color <- "#0284c7"

      if (exists("correlation_table", where = asNamespace("analytix"))) {
        res <- tryCatch(
          analytix::correlation_table(df, cols = vars, method = cor_method, digits = cor_digits, sig_level = sig_level, color = header_color),
          error = function(e) NULL
        )
        if (!is.null(res)) return(res)
      }

      cor_m <- cor(df[vars], use = "pairwise.complete.obs", method = cor_method)
      as.data.frame(cor_m)
    })

    output$correlation_table_ui <- renderUI({
      res <- cor_table_res()
      req(res)
      ft <- get_flextable(res)
      if (!is.null(ft)) {
        flextable::htmltools_value(ft)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_cor_table"))
      }
    })
    output$raw_cor_table <- renderTable({ cor_table_res() }, rownames = TRUE, striped = TRUE, hover = TRUE)

    # 6. Diagnostic Performance logic
    diagnostic_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$diag_actual) && isTruthy(input$diag_predicted), "Veuillez sélectionner le Gold Standard et le Test à évaluer.")
      )

      act  <- df[[input$diag_actual]]
      pred <- df[[input$diag_predicted]]
      pos_v <- input$diag_pos_val
      req(pos_v)

      conf_level <- as.numeric(input$diag_conf_level) / 100
      digits <- as.integer(input$diag_digits)
      header_color <- input$diag_color
      if (is.null(header_color) || nchar(trimws(header_color)) == 0) header_color <- "#0284c7"

      if (exists("calc_sensitivity_specificity", where = asNamespace("analytix"))) {
        res <- tryCatch({
          analytix::calc_sensitivity_specificity(act, pred, positive_val = pos_v, conf_level = conf_level, digits = digits, color = header_color)
        }, error = function(e) NULL)
        if (!is.null(res)) return(res)
      }

      tp <- sum(act == pos_v & pred == pos_v, na.rm = TRUE)
      fp <- sum(act != pos_v & pred == pos_v, na.rm = TRUE)
      fn <- sum(act == pos_v & pred != pos_v, na.rm = TRUE)
      tn <- sum(act != pos_v & pred != pos_v, na.rm = TRUE)

      sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA
      spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA
      vpp  <- if ((tp + fp) > 0) tp / (tp + fp) else NA
      vpn  <- if ((tn + fn) > 0) tn / (tn + fn) else NA

      data.frame(
        Indicateur = c("VP (Vrais Positifs)", "FP (Faux Positifs)", "FN (Faux Négatifs)", "VN (Vrais Négatifs)",
                       "Sensibilité (%)", "Spécificité (%)", "VPP (%)", "VPN (%)"),
        Valeur = c(tp, fp, fn, tn,
                   paste0(round(sens * 100, digits), "%"),
                   paste0(round(spec * 100, digits), "%"),
                   paste0(round(vpp * 100, digits), "%"),
                   paste0(round(vpn * 100, digits), "%")),
        stringsAsFactors = FALSE
      )
    })

    output$diagnostic_results_ui <- renderUI({
      res <- diagnostic_res()
      req(res)
      ft <- get_flextable(res)
      if (!is.null(ft)) {
        flextable::htmltools_value(ft)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_diagnostic_table"))
      }
    })
    output$raw_diagnostic_table <- renderTable({ diagnostic_res() }, striped = TRUE, hover = TRUE)
    
    # ⚡ EXPORT IMMÉDIAT HANDLERS
    output$dl_likert_word <- downloadHandler(
      filename = function() { paste0("Likert_", input$likert_var, ".docx") },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Analyse Likert :", input$likert_var), style = "heading 1")
        res <- likert_res()
        ft <- get_flextable(res)
        if (!is.null(ft)) doc <- flextable::body_add_flextable(doc, ft)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )

    output$dl_multi_likert_word <- downloadHandler(
      filename = function() { paste0("MultiLikert_Recap_", Sys.Date(), ".docx") },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Tableau Récapitulatif Likert Multi-Items", style = "heading 1")
        res <- tryCatch(multi_likert_res(), error = function(e) NULL)
        if (!is.null(res)) {
          ft <- get_flextable(res)
          if (!is.null(ft)) doc <- flextable::body_add_flextable(doc, ft)
          else if (is.data.frame(res)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(res)))
        }
        print(doc, target = file)
      }
    )

    output$dl_multi_likert_csv <- downloadHandler(
      filename = function() { paste0("MultiLikert_Recap_", Sys.Date(), ".csv") },
      content = function(file) {
        res <- tryCatch(multi_likert_res(), error = function(e) NULL)
        df <- if (!is.null(res)) get_dataframe(res) else NULL
        if (!is.null(df)) write.csv(df, file, row.names = FALSE)
        else if (!is.null(res) && is.data.frame(res)) write.csv(res, file, row.names = FALSE)
      }
    )

    output$dl_likert_div_png <- downloadHandler(
      filename = function() { paste0("Likert_Divergent_", Sys.Date(), ".png") },
      content = function(file) { 
        p <- tryCatch(likert_plot_res(), error = function(e) NULL)
        req(p)
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300) 
      }
    )

    output$dl_likert_div_pdf <- downloadHandler(
      filename = function() { paste0("Likert_Divergent_", Sys.Date(), ".pdf") },
      content = function(file) { 
        p <- tryCatch(likert_plot_res(), error = function(e) NULL)
        req(p)
        ggplot2::ggsave(file, plot = p, width = 10, height = 6) 
      }
    )
    
    output$dl_multi_word <- downloadHandler(
      filename = function() { paste0("Choix_Multiples_", Sys.Date(), ".docx") },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Analyse des Réponses Multiples", style = "heading 1")
        res <- multi_res()
        ft <- get_flextable(res)
        if (!is.null(ft)) doc <- flextable::body_add_flextable(doc, ft)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )

    output$dl_multi_csv <- downloadHandler(
      filename = function() { paste0("Choix_Multiples_", Sys.Date(), ".csv") },
      content = function(file) {
        res <- multi_res()
        df <- get_dataframe(res)
        if (!is.null(df)) write.csv(df, file, row.names = FALSE)
        else if (is.data.frame(res)) write.csv(res, file, row.names = FALSE)
      }
    )
    
    output$dl_hm_png <- downloadHandler(
      filename = function() { "Heatmap_Correlations.png" },
      content = function(file) { ggplot2::ggsave(file, plot = current_hm_plot(), width = 8, height = 6, dpi = 300) }
    )
    
    output$dl_hm_pdf <- downloadHandler(
      filename = function() { "Heatmap_Correlations.pdf" },
      content = function(file) { ggplot2::ggsave(file, plot = current_hm_plot(), width = 8, height = 6) }
    )

    output$dl_cor_word <- downloadHandler(
      filename = function() { "Matrice_Correlations.docx" },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Matrice de Corrélations", style = "heading 1")
        res <- cor_table_res()
        ft <- get_flextable(res)
        if (!is.null(ft)) doc <- flextable::body_add_flextable(doc, ft)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )

    output$dl_diag_word <- downloadHandler(
      filename = function() { "Performance_Diagnostique.docx" },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Performance Diagnostique du Test", style = "heading 1")
        res <- diagnostic_res()
        ft <- get_flextable(res)
        if (!is.null(ft)) doc <- flextable::body_add_flextable(doc, ft)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )

    output$dl_diag_csv <- downloadHandler(
      filename = function() { "Performance_Diagnostique.csv" },
      content = function(file) {
        res <- diagnostic_res()
        df <- get_dataframe(res)
        if (!is.null(df)) {
          write.csv(df, file, row.names = FALSE)
        } else if (is.data.frame(res)) {
          write.csv(res, file, row.names = FALSE)
        }
      }
    )
    
    return(reactive({
      list(
        likert = tryCatch(likert_res(), error = function(e) NULL),
        multi_likert = tryCatch(multi_likert_res(), error = function(e) NULL),
        likert_plot = tryCatch(likert_plot_res(), error = function(e) NULL),
        multi = tryCatch(multi_res(), error = function(e) NULL),
        heatmap = tryCatch(current_hm_plot(), error = function(e) NULL),
        correlation_matrix = tryCatch(cor_table_res(), error = function(e) NULL),
        diagnostic = tryCatch(diagnostic_res(), error = function(e) NULL)
      )
    }))
  })
}
