#' Module Importation, Labellisation & Nettoyage
#' 
#' @param id Identifiant du module Shiny
#' @return Server return: list contenant `df_reactive` et `labels_reactive`

mod_import_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("cloud-upload-alt", class = "me-2 text-primary"),
        "Chargement & Options"
      ),
      width = 320,
      
      radioButtons(
        ns("data_source"), "Source des Données :",
        choices = c(
          "📁 Téléverser un fichier" = "file",
          "💾 Environnement R" = "env",
          "🧪 Données d'exemple (Sondage)" = "example"
        ),
        selected = "file"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'file'", ns("data_source")),
        uiOutput(ns("file_input_container")),
        uiOutput(ns("excel_sheet_ui"))
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'env'", ns("data_source")),
        uiOutput(ns("env_data_container"))
      ),
      
      tags$hr(),
      tags$h6(icon("sliders-h", class = "me-1"), "Nettoyage des Noms"),
      checkboxInput(ns("clean_colnames"), "Nettoyer les noms de colonnes (snake_case)", value = TRUE),
      checkboxInput(ns("trim_whitespace"), "Supprimer les espaces superflus (trim)", value = TRUE),
      
      tags$hr(),
      tags$h6(icon("magic", class = "me-1"), "Nettoyage & Imputation Avancés"),
      selectInput(ns("adv_clean_col"), "Choisir une colonne :", choices = NULL),
      selectInput(ns("adv_clean_action"), "Action à appliquer :",
                  choices = c(
                    "Aucune" = "none",
                    "Nettoyer en Texte (clean_text)" = "clean_text",
                    "Nettoyer en Numérique (clean_numeric)" = "clean_numeric",
                    "Nettoyer en Binaire (clean_binary)" = "clean_binary",
                    "Imputer par le Mode (impute_mode)" = "impute_mode",
                    "Imputer par la Moyenne (impute_mean)" = "impute_mean",
                    "Imputer par la Médiane (impute_median)" = "impute_median"
                  )),
      actionButton(ns("apply_adv_clean"), "Appliquer l'Action", class = "btn btn-outline-primary btn-sm w-100 mb-2"),

      tags$div(
        class = "mt-2 p-2 border rounded bg-light",
        tags$strong("Imputation Multiple (MICE) :"),
        tags$p("Impute tout le jeu de données d'un coup, avec gestion automatique des colonnes problématiques (constantes, 100% NA, classes spéciales).",
               class = "text-muted small mb-2"),
        fluidRow(
          column(6, numericInput(ns("mice_m"), "Imputations (m) :", value = 5, min = 1, max = 20, step = 1)),
          column(6, numericInput(ns("mice_maxit"), "Itérations (maxit) :", value = 5, min = 1, max = 50, step = 1))
        ),
        numericInput(ns("mice_seed"), "Graine (seed) :", value = 123, min = 1, step = 1),
        selectInput(ns("mice_vars"), "Variables à imputer (vide = toutes) :", choices = NULL, multiple = TRUE),
        actionButton(ns("run_mice"), "Lancer MICE", class = "btn btn-primary btn-sm w-100 mt-2", icon = icon("magic"))
      ),

      tags$hr(),
      tags$h6(icon("download", class = "me-1"), "Exporter les données"),
      downloadButton(ns("download_cleaned_csv"), "Télécharger CSV Nettoyé", class = "btn-outline-primary btn-sm w-100 mb-2"),
      
      tags$hr(),
      actionButton(ns("reset_data"), "Réinitialiser", icon = icon("undo"), class = "btn-outline-danger btn-sm w-100")
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      # KPIs
      uiOutput(ns("kpi_boxes_ui")),
      tags$div(class = "mb-3"),
      
      # Tabs
      bslib::navset_card_tab(
        # Tab 1: Preview Table
        bslib::nav_panel(
          title = tags$span(icon("table"), " Aperçu de la Table"),
          uiOutput(ns("preview_table_container"))
        ),
        
        # Tab 2: Label Manager (Gestion des Libellés)
        bslib::nav_panel(
          title = tags$span(icon("tag"), " Éditeur de Libellés (Labels)"),
          tags$div(
            class = "p-3",
            tags$p(class = "text-muted", "Attribuez des libellés francophones lisibles à vos variables (ex: 'age' ➔ 'Âge du patient (années)'). Ces libellés seront intégrés automatiquement dans tous vos tableaux et rapports Word."),
            actionButton(ns("apply_labels"), "Enregistrer les Libellés", icon = icon("save"), class = "btn-success btn-sm mb-3"),
            uiOutput(ns("labels_editor_ui"))
          )
        ),

        # Tab 3: Variable Recoding (Recodage de Variables)
        bslib::nav_panel(
          title = tags$span(icon("exchange-alt"), " Recodage de Variables"),
          tags$div(
            class = "p-3",
            tags$p(class = "text-muted", "Modifiez, regroupez ou discrétisez vos variables à l'aide des outils du package analytix. Ces modifications s'appliqueront à toutes les analyses et exports de l'application."),

            # Recoding UI layout
            fluidRow(
              column(4,
                selectInput(ns("recode_var"), "Variable à recoder :", choices = NULL),
                selectInput(ns("recode_method"), "Méthode de recodage :",
                            choices = c(
                              "Recodage simple (quick_code)" = "quick",
                              "Regroupement de modalités (collapse_categories)" = "collapse",
                              "Découper en classes (categorize_numeric)" = "categorize"
                            ))
              ),
              column(8,
                # Dynamic UI for each method
                uiOutput(ns("recode_method_ui")),
                tags$hr(),
                actionButton(ns("btn_apply_recode"), "Appliquer le Recodage", class = "btn btn-success w-100")
              )
            )
          )
        ),
        
        # Tab 4: Outliers Diagnostic
        bslib::nav_panel(
          title = tags$span(icon("search"), " Valeurs Aberrantes (Outliers)"),
          tags$div(
            class = "p-3",
            selectInput(ns("outlier_var"), "Choisir une variable numérique :", choices = NULL),
            uiOutput(ns("outlier_report_ui"))
          )
        ),
        
        # Tab 5: Missing Values
        bslib::nav_panel(
          title = tags$span(icon("exclamation-triangle"), " Synthèse des Manquants"),
          uiOutput(ns("missing_summary_ui"))
        ),
        
        # Tab 6: Calcul de Scores & Alpha
        bslib::nav_panel(
          title = tags$span(icon("calculator"), " Scores & Fiabilité"),
          tags$div(
            class = "p-3",
            tags$h5(class = "fw-bold", "Calculer un score composite (ex: Sondages)"),
            tags$p(class = "text-muted", "Créez une nouvelle variable à partir de la somme ou la moyenne de plusieurs questions."),
            fluidRow(
              column(4,
                selectInput(ns("score_vars"), "Sélectionner les items :", choices = NULL, multiple = TRUE),
                selectInput(ns("score_method"), "Méthode de calcul :", choices = c("Somme" = "sum", "Moyenne" = "mean")),
                textInput(ns("score_name"), "Nom de la nouvelle variable :", value = "Nouveau_Score"),
                actionButton(ns("btn_calc_score"), "Calculer le score et l'Alpha", class = "btn btn-primary w-100")
              ),
              column(8,
                uiOutput(ns("score_result_ui"))
              )
            )
          )
        )
      )
    )
  )
}

mod_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to reset file input
    reset_trigger <- reactiveVal(0)

    # Dynamic fileInput container
    output$file_input_container <- renderUI({
      reset_trigger() # dependency
      fileInput(
        ns("file"),
        "Glisser-déposer ou Choisir :",
        accept = c(".xlsx", ".xls", ".csv", ".rds"),
        buttonLabel = "Parcourir...",
        placeholder = "Aucun fichier sélectionné"
      )
    })

    # 1. Excel sheet UI
    output$excel_sheet_ui <- renderUI({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("xlsx", "xls")) {
        sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
        if (!is.null(sheets) && length(sheets) > 1) {
          selectInput(ns("excel_sheet"), "Sélectionner la Feuille Excel :", choices = sheets)
        }
      }
    })
    
    # 1.5 Env data UI
    output$env_data_container <- renderUI({
      req(input$data_source == "env")
      input$refresh_env # Dependency to trigger re-evaluation
      
      # Find all objects in GlobalEnv that are data.frames
      objs <- ls(envir = .GlobalEnv)
      df_names <- Filter(function(x) is.data.frame(get(x, envir = .GlobalEnv)), objs)
      
      if (length(df_names) > 0) {
        tagList(
          selectInput(ns("env_data"), "Sélectionner un jeu de données :", choices = df_names),
          actionButton(ns("refresh_env"), "Actualiser la liste", icon = icon("sync"), class = "btn-outline-secondary btn-sm w-100 mt-2")
        )
      } else {
        tagList(
          tags$div(class = "text-danger mb-2 small fw-bold", icon("exclamation-circle"), " Aucun data.frame dans l'environnement."),
          actionButton(ns("refresh_env"), "Actualiser la liste", icon = icon("sync"), class = "btn-outline-secondary btn-sm w-100")
        )
      }
    })

    
    # 2. Raw Data
    raw_data <- reactive({
      if (input$data_source == "example") {
        set.seed(123)
        n <- 150
        df_demo <- data.frame(
          ID = 1:n,
          Sexe = sample(c("Femme", "Homme"), n, replace = TRUE, prob = c(0.6, 0.4)),
          Filiere = sample(c("Psycho", "Socio", "Bio", "Gestion"), n, replace = TRUE),
          Age = round(rnorm(n, 21, 2)),
          Heures_Revisions = round(rnorm(n, 15, 5), 1),
          Reussite_Examen = sample(c("Oui", "Non"), n, replace = TRUE, prob = c(0.75, 0.25)),
          Stress_Q1 = sample(1:5, n, replace = TRUE),
          Stress_Q2 = sample(1:5, n, replace = TRUE),
          Stress_Q3 = sample(1:5, n, replace = TRUE)
        )
        # Add some missing values for demonstration
        df_demo$Age[c(10, 25, 42)] <- NA
        df_demo$Heures_Revisions[c(5, 12, 100, 142)] <- NA
        return(df_demo)
      } else if (input$data_source == "env") {
        req(input$env_data)
        df <- tryCatch(get(input$env_data, envir = .GlobalEnv), error = function(e) NULL)
        req(is.data.frame(df))
        return(as.data.frame(df))
      } else {
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        path <- input$file$datapath
        
        df <- switch(
          ext,
          csv = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
          xlsx = {
            sheet <- if (is.null(input$excel_sheet)) 1 else input$excel_sheet
            readxl::read_excel(path, sheet = sheet)
          },
          xls = {
            sheet <- if (is.null(input$excel_sheet)) 1 else input$excel_sheet
            readxl::read_excel(path, sheet = sheet)
          },
          rds = readRDS(path),
          stop("Format de fichier non supporté.")
        )
        return(as.data.frame(df))
      }
    })
    
    # 3. Cleaned Data
    cleaned_data <- reactiveVal(NULL)
    
    # Observe Reset Event
    observeEvent(input$reset_data, {
      updateRadioButtons(session, "data_source", selected = "file")
      reset_trigger(reset_trigger() + 1)
      cleaned_data(NULL)
      shinyjs::reset("file")
      showNotification("Données réinitialisées avec succès !", type = "message")
    })

    # Initialize or reset cleaned_data when raw_data or standard cleaning checkboxes change
    observeEvent({
      raw_data()
      input$clean_colnames
      input$trim_whitespace
    }, {
      df <- raw_data()
      req(df)
      
      if (isTRUE(input$clean_colnames)) {
        if (exists("clean_names", where = asNamespace("analytix"))) {
          df <- analytix::clean_names(df)
        } else {
          names(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
          names(df) <- gsub("_+", "_", names(df))
          names(df) <- gsub("^_|_$", "", names(df))
          names(df) <- tolower(names(df))
        }
      }
      
      if (isTRUE(input$trim_whitespace)) {
        df <- as.data.frame(lapply(df, function(col) {
          if (is.character(col)) trimws(col) else col
        }), stringsAsFactors = FALSE)
      }
      
      cleaned_data(df)
    })
    
    # Update selectors whenever cleaned_data changes
    observe({
      df <- cleaned_data()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "outlier_var", choices = num_cols)
      updateSelectInput(session, "adv_clean_col", choices = names(df))
      updateSelectInput(session, "recode_var", choices = names(df))
      updateSelectInput(session, "mice_vars", choices = names(df))
      updateSelectInput(session, "score_vars", choices = num_cols)
    })

    # Recoding Dynamic UI Server Logic
    output$recode_method_ui <- renderUI({
      req(input$recode_var, input$recode_method)
      df <- cleaned_data()
      req(df)
      var_nm <- input$recode_var
      method <- input$recode_method

      if (method == "quick") {
        vals <- unique(df[[var_nm]])
        vals <- vals[!is.na(vals)]

        inputs <- lapply(vals, function(v) {
          textInput(ns(paste0("quick_val_", gsub("[^a-zA-Z0-9_]", "_", as.character(v)))), label = paste("Ancienne valeur :", v), value = as.character(v))
        })

        tagList(
          tags$h6("Recodage des valeurs individuelles :"),
          tags$div(style = "max-height: 250px; overflow-y: auto;", inputs),
          selectInput(ns("quick_to"), "Convertir en :", choices = c("Caractère" = "character", "Facteur" = "factor", "Type d'origine" = "keep")),
          textInput(ns("quick_na"), "Remplacer les NA par (optionnel) :", value = "")
        )
      } else if (method == "collapse") {
        vals <- unique(df[[var_nm]])
        vals <- vals[!is.na(vals)]

        tagList(
          tags$h6("Regroupement de modalités :"),
          textInput(ns("collapse_new_name"), "Nouveau nom de la catégorie regroupée :", placeholder = "ex: Groupe A"),
          selectInput(ns("collapse_old_vals"), "Modalités d'origine à regrouper :", choices = vals, multiple = TRUE),
          checkboxInput(ns("collapse_keep_na"), "Conserver les NA", value = TRUE),
          textInput(ns("collapse_other_label"), "Label pour les modalités non regroupées (laisser vide pour les garder inchangées) :", value = "")
        )
      } else if (method == "categorize") {
        tagList(
          tags$h6("Découpage de variable numérique en classes :"),
          textInput(ns("categorize_breaks"), "Seuils des classes (ex: 0, 18, 35, 50, 100 ou un nombre entier de classes) :", placeholder = "ex: 0, 18, 35, 50, 100"),
          textInput(ns("categorize_labels"), "Libellés des classes (séparés par des virgules, optionnel) :", placeholder = "ex: Enfant, Jeune adulte, Adulte, Senior"),
          checkboxInput(ns("categorize_as_factor"), "Retourner un facteur", value = TRUE)
        )
      }
    })

    # Observe Recode Action Button
    observeEvent(input$btn_apply_recode, {
      df <- cleaned_data()
      req(df, input$recode_var, input$recode_method)
      var_nm <- input$recode_var
      method <- input$recode_method

      tryCatch({
        if (method == "quick") {
          vals <- unique(df[[var_nm]])
          vals <- vals[!is.na(vals)]

          # Collect the mappings
          recodes <- list()
          for (v in vals) {
            new_v <- input[[paste0("quick_val_", gsub("[^a-zA-Z0-9_]", "_", as.character(v)))]]
            if (!is.null(new_v) && nchar(trimws(new_v)) > 0) {
              recodes[[as.character(v)]] <- trimws(new_v)
            }
          }

          na_val <- input$quick_na
          if (is.null(na_val) || nchar(trimws(na_val)) == 0) na_val <- NULL

          to_type <- input$quick_to

          if (length(recodes) > 0) {
            if (exists("quick_code", where = asNamespace("analytix"))) {
              # Call quick_code from package
              args <- c(list(data = df, var = as.name(var_nm)), recodes, list(.na = na_val, to = to_type))
              df <- do.call(analytix::quick_code, args)
            } else {
              # Fallback R code
              vec <- df[[var_nm]]
              char_vec <- as.character(vec)
              for (old_val in names(recodes)) {
                char_vec[char_vec == old_val] <- recodes[[old_val]]
              }
              if (!is.null(na_val)) {
                char_vec[is.na(vec)] <- na_val
              }
              if (to_type == "factor") {
                df[[var_nm]] <- factor(char_vec)
              } else {
                df[[var_nm]] <- char_vec
              }
            }
            cleaned_data(as.data.frame(df))
            showNotification("Recodage simple appliqué avec succès !", type = "message")
          }

        } else if (method == "collapse") {
          new_name <- trimws(input$collapse_new_name)
          old_vals <- input$collapse_old_vals

          shiny::validate(
            shiny::need(nchar(new_name) > 0, "Veuillez entrer un nouveau nom de catégorie."),
            shiny::need(length(old_vals) > 0, "Veuillez sélectionner au moins une modalité à regrouper.")
          )

          groups <- list()
          groups[[new_name]] <- old_vals

          other_lbl <- trimws(input$collapse_other_label)
          if (nchar(other_lbl) == 0) other_lbl <- NULL

          keep_na <- isTRUE(input$collapse_keep_na)

          if (exists("collapse_categories", where = asNamespace("analytix"))) {
            df <- analytix::collapse_categories(df, var = !!rlang::sym(var_nm), groups = groups, keep_na = keep_na, other_label = other_lbl)
          } else {
            # Fallback
            vec <- df[[var_nm]]
            char_vec <- as.character(vec)
            char_vec[char_vec %in% old_vals] <- new_name
            if (!keep_na) {
              char_vec[is.na(char_vec)] <- ifelse(is.null(other_lbl), "Autre", other_lbl)
            }
            if (!is.null(other_lbl)) {
              char_vec[char_vec != new_name & !is.na(char_vec)] <- other_lbl
            }
            df[[var_nm]] <- factor(char_vec)
          }
          cleaned_data(as.data.frame(df))
          showNotification("Regroupement de modalités appliqué avec succès !", type = "message")

        } else if (method == "categorize") {
          breaks_str <- trimws(input$categorize_breaks)
          labels_str <- trimws(input$categorize_labels)

          shiny::validate(
            shiny::need(nchar(breaks_str) > 0, "Veuillez spécifier des seuils.")
          )

          # Parse breaks
          breaks_split <- strsplit(breaks_str, ",")[[1]]
          breaks_split <- trimws(breaks_split)

          if (length(breaks_split) == 1 && !is.na(as.integer(breaks_split))) {
            breaks_val <- as.integer(breaks_split)
          } else {
            breaks_val <- as.numeric(breaks_split)
            shiny::validate(
              shiny::need(!any(is.na(breaks_val)), "Les seuils de classes doivent être des nombres valides.")
            )
          }

          # Parse labels
          labels_val <- NULL
          if (nchar(labels_str) > 0) {
            labels_val <- trimws(strsplit(labels_str, ",")[[1]])
          }

          as_fac <- isTRUE(input$categorize_as_factor)

          if (exists("categorize_numeric", where = asNamespace("analytix"))) {
            df <- analytix::categorize_numeric(df, var = !!rlang::sym(var_nm), breaks = breaks_val, labels = labels_val, as_factor = as_fac)
          } else {
            # Fallback
            vec <- df[[var_nm]]
            cat_var <- cut(vec, breaks = breaks_val, labels = labels_val, include.lowest = TRUE, right = TRUE)
            if (!as_fac) cat_var <- as.character(cat_var)
            df[[var_nm]] <- cat_var
          }
          cleaned_data(as.data.frame(df))
          showNotification("Découpage en classes appliqué avec succès !", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Erreur lors du recodage :", e$message), type = "error")
      })
    })

    # Advanced Nettoyage & Imputation Handlers
    observeEvent(input$apply_adv_clean, {
      df <- cleaned_data()
      req(df, input$adv_clean_col)
      action <- input$adv_clean_action
      col_name <- input$adv_clean_col

      if (action == "none") {
        return()
      }

      tryCatch({
        new_col <- df[[col_name]]
        if (action == "clean_text") {
          if (exists("clean_text", where = asNamespace("analytix"))) {
            new_col <- analytix::clean_text(new_col)
          } else {
            new_col <- trimws(as.character(new_col))
            new_col[new_col %in% c("", "NA", "N/A", "<NA>", "NULL")] <- NA
          }
        } else if (action == "clean_numeric") {
          if (exists("clean_numeric", where = asNamespace("analytix"))) {
            new_col <- analytix::clean_numeric(new_col)
          } else {
            new_col <- as.numeric(gsub(",", ".", as.character(new_col)))
          }
        } else if (action == "clean_binary") {
          if (exists("clean_binary", where = asNamespace("analytix"))) {
            new_col <- analytix::clean_binary(new_col)
          } else {
            new_col <- factor(ifelse(tolower(as.character(new_col)) %in% c("oui", "yes", "1", "true"), "Oui", "Non"), levels = c("Oui", "Non"))
          }
        } else if (action == "impute_mode") {
          if (exists("impute_mode", where = asNamespace("analytix"))) {
            new_col <- analytix::impute_mode(new_col)
          } else {
            ux <- unique(new_col[!is.na(new_col)])
            mode_val <- ux[which.max(tabulate(match(new_col, ux)))]
            new_col[is.na(new_col)] <- mode_val
          }
        } else if (action == "impute_mean") {
          if (exists("impute_mean", where = asNamespace("analytix"))) {
            new_col <- analytix::impute_mean(new_col, type = "mean")
          } else {
            new_col[is.na(new_col)] <- mean(new_col, na.rm = TRUE)
          }
        } else if (action == "impute_median") {
          if (exists("impute_mean", where = asNamespace("analytix"))) {
            new_col <- analytix::impute_mean(new_col, type = "median")
          } else {
            new_col[is.na(new_col)] <- median(new_col, na.rm = TRUE)
          }
        }

        df[[col_name]] <- new_col
        cleaned_data(df)
        showNotification(paste0("Action '", action, "' appliquée avec succès sur '", col_name, "' !"), type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur de nettoyage :", e$message), type = "error")
      })
    })

    observeEvent(input$run_mice, {
      df <- cleaned_data()
      req(df)

      n_na_before <- sum(is.na(df))
      if (n_na_before == 0) {
        showNotification("Aucune valeur manquante détectée dans le jeu de données.", type = "message")
        return()
      }

      m_val     <- as.integer(input$mice_m)
      maxit_val <- as.integer(input$mice_maxit)
      seed_val  <- as.integer(input$mice_seed)
      if (is.na(m_val)     || m_val     < 1) m_val     <- 5L
      if (is.na(maxit_val) || maxit_val < 1) maxit_val <- 5L
      if (is.na(seed_val))                   seed_val  <- 123L

      shiny::withProgress(
        message = sprintf("Imputation MICE (m=%d, maxit=%d)...", m_val, maxit_val),
        value = 0.1, {

        shiny::incProgress(0.2, detail = "Préparation des données...")

        tryCatch({
          if (exists("impute_mice", where = asNamespace("analytix"))) {
            shiny::incProgress(0.5, detail = "Imputation en cours...")
            imp_df <- analytix::impute_mice(
              df,
              m       = m_val,
              maxit   = maxit_val,
              seed    = seed_val,
              verbose = FALSE
            )
            n_na_after <- sum(is.na(imp_df))
            n_imputed  <- n_na_before - n_na_after

            # If the user selected specific variables, only keep those imputations
            if (length(input$mice_vars) > 0) {
              df_new <- df
              for (v in input$mice_vars) {
                if (v %in% names(df) && v %in% names(imp_df)) {
                  df_new[[v]] <- imp_df[[v]]
                }
              }
              imp_df <- df_new
              n_na_after <- sum(is.na(imp_df))
              n_imputed <- n_na_before - n_na_after
            }

            cleaned_data(imp_df)
            shiny::incProgress(1.0, detail = "Terminé !")

            if (n_na_after == 0) {
              showNotification(
                sprintf("Imputation MICE réussie : %d valeur(s) imputée(s). Aucun NA restant.", n_imputed),
                type = "message", duration = 8
              )
            } else {
              showNotification(
                sprintf("Imputation MICE partielle : %d valeur(s) imputée(s) | %d NA restants (colonnes 100%% NA ou facteurs à un seul niveau).",
                        n_imputed, n_na_after),
                type = "warning", duration = 10
              )
            }
          } else {
            showNotification("impute_mice() non disponible dans la version du package chargée.",
                             type = "warning")
          }
        }, error = function(e) {
          showNotification(
            paste0("Erreur MICE : ", e$message,
                   "\nVeuillez vérifier que le package mice est installé (install.packages('mice'))."),
            type = "error", duration = 15
          )
        })
      })
    })
    
    # 4. Labels Editor UI
    output$labels_editor_ui <- renderUI({
      df <- cleaned_data()
      req(df)
      
      vars <- names(df)
      inputs <- lapply(vars, function(v) {
        current_lbl <- attr(df[[v]], "label")
        if (is.null(current_lbl)) current_lbl <- v
        
        tags$div(
          class = "row align-items-center mb-2",
          tags$div(class = "col-md-4 fw-bold text-truncate", v),
          tags$div(
            class = "col-md-8",
            textInput(ns(paste0("lbl_", v)), label = NULL, value = current_lbl, placeholder = paste("Libellé pour", v))
          )
        )
      })
      do.call(tags$div, inputs)
    })
    
    # Apply labels event
    observeEvent(input$apply_labels, {
      df <- cleaned_data()
      req(df)
      
      lbl_vector <- c()
      for (v in names(df)) {
        val <- input[[paste0("lbl_", v)]]
        if (!is.null(val) && nchar(trimws(val)) > 0) {
          lbl_vector[v] <- trimws(val)
        }
      }
      
      if (length(lbl_vector) > 0) {
        if (exists("label_vars", where = asNamespace("analytix"))) {
          df <- analytix::label_vars(df, lbl_vector)
        } else {
          for (nm in names(lbl_vector)) {
            if (nm %in% names(df)) attr(df[[nm]], "label") <- lbl_vector[[nm]]
          }
        }
        cleaned_data(df)
        showNotification("Libellés mis à jour avec succès !", type = "message")
      }
    })
    
    # 5. KPI Boxes
    output$kpi_boxes_ui <- renderUI({
      df <- cleaned_data()
      if (is.null(df)) {
        return(
          bslib::layout_column_wrap(
            width = 1/3,
            bslib::value_box(title = "Fichier", value = "Aucun", showcase = icon("file-excel"), theme = "secondary")
          )
        )
      }
      n_rows <- nrow(df)
      n_cols <- ncol(df)
      n_na <- sum(is.na(df))
      pct_na <- round((n_na / (n_rows * n_cols)) * 100, 1)
      
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box(title = "Lignes / Participants", value = format(n_rows, big.mark = " "), showcase = icon("users"), theme = "primary"),
        bslib::value_box(title = "Variables / Colonnes", value = format(n_cols, big.mark = " "), showcase = icon("columns"), theme = "info"),
        bslib::value_box(title = "Valeurs Manquantes (NA)", value = sprintf("%s (%s%%)", format(n_na, big.mark = " "), pct_na), showcase = icon("exclamation-circle"), theme = if (pct_na > 15) "warning" else "success")
      )
    })
    
    # 6. Preview Table Container
    output$preview_table_container <- renderUI({
      if (requireNamespace("DT", quietly = TRUE)) {
        tags$div(
          style = "overflow-x: auto; width: 100%;",
          DT::DTOutput(ns("preview_dt"))
        )
      } else {
        tags$div(
          style = "overflow-x: auto; width: 100%;",
          tableOutput(ns("preview_std"))
        )
      }
    })
    
    output$preview_dt <- DT::renderDT({
      df <- cleaned_data()
      req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE), class = "compact cell-border stripe hover")
    })
    
    output$preview_std <- renderTable({
      df <- cleaned_data()
      req(df)
      head(df, 15)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 7. Outlier Report
    output$outlier_report_ui <- renderUI({
      df <- cleaned_data()
      req(df, input$outlier_var)
      var_nm <- input$outlier_var
      vec <- df[[var_nm]]

      if (exists("detect_outliers", where = asNamespace("analytix"))) {
        sym_var <- rlang::sym(var_nm)
        res <- tryCatch(analytix::detect_outliers(df, var = !!sym_var), error = function(e) NULL)
        if (!is.null(res)) {
          # Build cards with all detected outlier info
          tag_list <- tagList()
          # Résumé flextable
          ft_summary <- if (!is.null(res$summary) && inherits(res$summary, "flextable")) res$summary else NULL
          ft_outlier_vals <- if (!is.null(res$outlier_rows) && inherits(res$outlier_rows, "flextable")) res$outlier_rows else NULL
          plot_outlier <- if (!is.null(res$plot)) res$plot else NULL

          tag_list <- tagList(
            tags$div(
              class = "mb-3 p-3 border rounded bg-white",
              tags$h6(class = "fw-bold text-primary", "Résumé des valeurs aberrantes (detect_outliers)"),
              if (!is.null(ft_summary)) flextable::htmltools_value(ft_summary)
              else tableOutput(ns("outlier_fallback_table"))
            ),
            if (!is.null(ft_outlier_vals)) {
              tags$div(
                class = "mb-3 p-3 border rounded bg-white",
                tags$h6(class = "fw-bold text-warning", "Lignes identifiées comme aberrantes"),
                flextable::htmltools_value(ft_outlier_vals)
              )
            },
            if (!is.null(plot_outlier)) {
              tags$div(
                class = "mb-3 p-3 border rounded bg-white",
                tags$h6(class = "fw-bold", "Graphique des valeurs aberrantes"),
                plotOutput(ns("outlier_pkg_plot"), height = "380px")
              )
            }
          )
          return(tag_list)
        }
      }

      # Fallback outlier calculation (IQR method)
      q1 <- stats::quantile(vec, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(vec, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      outliers <- vec[!is.na(vec) & (vec < lower_bound | vec > upper_bound)]

      tagList(
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header(tags$div(class="fw-bold", "Résumé de la Distribution")),
            tableOutput(ns("outlier_fallback_table"))
          ),
          bslib::card(
            bslib::card_header(tags$div(class="fw-bold", "Boxplot - Détection Visuelle")),
            plotOutput(ns("outlier_fallback_plot"), height = "350px")
          )
        )
      )
    })

    output$outlier_pkg_plot <- renderPlot({
      df <- cleaned_data()
      req(df, input$outlier_var)
      var_nm <- input$outlier_var
      if (exists("detect_outliers", where = asNamespace("analytix"))) {
        sym_var <- rlang::sym(var_nm)
        res <- tryCatch(analytix::detect_outliers(df, var = !!sym_var), error = function(e) NULL)
        if (!is.null(res) && !is.null(res$plot)) return(res$plot)
      }
    })

    output$outlier_fallback_table <- renderTable({
      df <- cleaned_data()
      req(df, input$outlier_var)
      vec <- df[[input$outlier_var]]
      q1 <- stats::quantile(vec, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(vec, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      outliers <- vec[!is.na(vec) & (vec < lower_bound | vec > upper_bound)]
      data.frame(
        Indicateur = c("Variable", "Effectif valide", "Q1 (25%)", "Q3 (75%)", "IQR", "Seuil inférieur (Q1 - 1.5×IQR)", "Seuil supérieur (Q3 + 1.5×IQR)", "Nombre d'outliers", "Valeurs (5 premières)"),
        Valeur = c(input$outlier_var, sum(!is.na(vec)), round(q1, 2), round(q3, 2), round(iqr, 2), round(lower_bound, 2), round(upper_bound, 2), length(outliers), paste(head(outliers, 5), collapse = ", ")),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE)

    output$outlier_fallback_plot <- renderPlot({
      df <- cleaned_data()
      req(df, input$outlier_var)
      vec <- df[[input$outlier_var]]
      q1 <- stats::quantile(vec, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(vec, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      vec_df <- data.frame(val = vec[!is.na(vec)], outlier = ifelse(vec[!is.na(vec)] < lower_bound | vec[!is.na(vec)] > upper_bound, "Outlier", "Normal"))
      ggplot2::ggplot(vec_df, ggplot2::aes(x = 1, y = val, color = outlier)) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.7, size = 2.5) +
        ggplot2::geom_boxplot(color = "gray30", fill = NA, outlier.shape = NA, linewidth = 0.7) +
        ggplot2::geom_hline(yintercept = lower_bound, linetype = "dashed", color = "#e11d48", linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = upper_bound, linetype = "dashed", color = "#e11d48", linewidth = 0.8) +
        ggplot2::scale_color_manual(values = c("Normal" = "#0284c7", "Outlier" = "#e11d48")) +
        ggplot2::labs(title = paste("Détection d'outliers :", input$outlier_var), x = "", y = input$outlier_var, color = "") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
    })
    
    # 8. Missing Summary
    output$missing_summary_ui <- renderUI({
      df <- cleaned_data()
      req(df)
      tagList(
        # Intégration de missing_report() si disponible
        if (exists("missing_report", where = asNamespace("analytix"))) {
          tags$div(
            class = "mb-3",
            bslib::card(
              bslib::card_header(
                tags$div(
                  class = "d-flex justify-content-between align-items-center w-100",
                  tags$span(class = "fw-bold", icon("table", class = "me-2"), "Rapport Complet des Valeurs Manquantes (analytix::missing_report)"),
                  downloadButton(ns("dl_missing_report"), "Word (.docx)", class = "btn-outline-primary btn-sm")
                )
              ),
              uiOutput(ns("missing_report_ui"))
            )
          )
        },
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header(tags$div(class = "fw-bold", icon("table", class = "me-2"), "Tableau des Valeurs Manquantes (par variable)")),
            tableOutput(ns("missing_table"))
          ),
          bslib::card(
            bslib::card_header(tags$div(class = "fw-bold", icon("image", class = "me-2"), "Carte Visuelle des Manquants")),
            plotOutput(ns("missing_map_plot"), height = "450px")
          )
        )
      )
    })

    # missing_report (via analytix) in its own renderUI
    output$missing_report_ui <- renderUI({
      df <- cleaned_data()
      req(df)
      if (exists("missing_report", where = asNamespace("analytix"))) {
        res <- tryCatch(analytix::missing_report(df), error = function(e) NULL)
        if (!is.null(res)) {
          ft <- if (inherits(res, "flextable")) res
                else if (is.list(res) && !is.null(res$flextable)) res$flextable
                else NULL
          if (!is.null(ft)) return(flextable::htmltools_value(ft))
          if (is.data.frame(res)) return(tableOutput(ns("missing_report_raw")))
        }
      }
      tags$p(class = "text-muted", "missing_report() non disponible dans la version du package chargée.")
    })

    output$missing_report_raw <- renderTable({
      df <- cleaned_data()
      req(df)
      if (exists("missing_report", where = asNamespace("analytix"))) {
        tryCatch(as.data.frame(analytix::missing_report(df)), error = function(e) NULL)
      }
    }, striped = TRUE, hover = TRUE)

    output$dl_missing_report <- downloadHandler(
      filename = function() { paste0("Rapport_Manquants_", Sys.Date(), ".docx") },
      content = function(file) {
        df <- cleaned_data()
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Rapport des Valeurs Manquantes", style = "heading 1")
        # Via missing_report()
        if (exists("missing_report", where = asNamespace("analytix"))) {
          res <- tryCatch(analytix::missing_report(df), error = function(e) NULL)
          if (!is.null(res)) {
            ft <- if (inherits(res, "flextable")) res
                  else if (is.list(res) && !is.null(res$flextable)) res$flextable
                  else NULL
            if (!is.null(ft)) doc <- flextable::body_add_flextable(doc, ft)
          }
        }
        # Tableau simple NA
        doc <- officer::body_add_par(doc, "Détail des NA par variable", style = "heading 2")
        na_counts <- sapply(df, function(x) sum(is.na(x)))
        na_pct <- round((na_counts / nrow(df)) * 100, 1)
        na_df <- data.frame(
          Variable = names(df),
          `Nombre de NA` = na_counts,
          `Proportion (%)` = paste0(na_pct, " %"),
          check.names = FALSE
        )
        na_df <- na_df[order(-na_counts), ]
        ft_na <- flextable::theme_vanilla(flextable::flextable(na_df))
        doc <- flextable::body_add_flextable(doc, ft_na)
        print(doc, target = file)
      }
    )

    output$missing_table <- renderTable({
      df <- cleaned_data()
      req(df)
      na_counts <- sapply(df, function(x) sum(is.na(x)))
      na_pct <- round((na_counts / nrow(df)) * 100, 1)
      na_df <- data.frame(`Variable` = names(df), `Nombre de NA` = na_counts, `Proportion` = paste0(na_pct, " %"), check.names = FALSE)
      na_df[order(-na_counts), ]
    }, striped = TRUE, hover = TRUE)

    output$missing_map_plot <- renderPlot({
      df <- cleaned_data()
      req(df)
      if (exists("plot_missing_map", where = asNamespace("analytix"))) {
        tryCatch(analytix::plot_missing_map(df), error = function(e) {
          ggplot2::ggplot() + ggplot2::labs(title = "Carte Visuelle non disponible") + ggplot2::theme_void()
        })
      } else {
        # Fallback manual heatmap of NAs
        na_mat <- as.data.frame(is.na(df) + 0)
        na_df <- data.frame(
          Variable = rep(names(df), each = nrow(df)),
          Ligne = rep(seq_len(nrow(df)), times = ncol(df)),
          Manquant = unlist(na_mat)
        )
        ggplot2::ggplot(na_df, ggplot2::aes(x = Variable, y = Ligne, fill = factor(Manquant))) +
          ggplot2::geom_tile(height = 1) +
          ggplot2::scale_fill_manual(values = c("0" = "#cbd5e1", "1" = "#e11d48"), labels = c("Présent", "Manquant")) +
          ggplot2::labs(title = "Carte des Valeurs Manquantes", x = "Variable", y = "Observation", fill = "") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom")
      }
    })
    
    # Download cleaned CSV handler
    output$download_cleaned_csv <- downloadHandler(
      filename = function() { paste0("donnees_nettoyees_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(cleaned_data(), file, row.names = FALSE)
      }
    )
    
    # --------------------------------------------------------------------------
    # SCORE COMPOSITE & ALPHA DE CRONBACH
    # --------------------------------------------------------------------------
    observeEvent(input$btn_calc_score, {
      df <- cleaned_data()
      vars <- input$score_vars
      new_var <- trimws(input$score_name)
      
      if (length(vars) < 2) {
        output$score_result_ui <- renderUI({
          tags$div(class = "alert alert-warning", "Sélectionnez au moins 2 variables pour calculer un score.")
        })
        return()
      }
      if (nchar(new_var) == 0 || new_var %in% names(df)) {
        output$score_result_ui <- renderUI({
          tags$div(class = "alert alert-danger", "Nom de variable invalide ou déjà existant.")
        })
        return()
      }
      
      # Extract items
      sub_df <- df[, vars, drop = FALSE]
      
      # Calculate score
      if (input$score_method == "sum") {
        new_col <- rowSums(sub_df, na.rm = TRUE)
      } else {
        new_col <- rowMeans(sub_df, na.rm = TRUE)
      }
      
      df[[new_var]] <- new_col
      cleaned_data(df)
      
      # Calculate Cronbach's Alpha manually (to avoid psych dependency)
      # alpha = (k / (k-1)) * (1 - sum(var_i) / var_total)
      k <- length(vars)
      vars_var <- sapply(sub_df, var, na.rm = TRUE)
      total_var <- var(rowSums(sub_df, na.rm = TRUE), na.rm = TRUE)
      
      if (total_var == 0 || is.na(total_var)) {
        alpha_val <- NA
      } else {
        alpha_val <- (k / (k - 1)) * (1 - sum(vars_var) / total_var)
      }
      
      # Pédagogie de l'Alpha
      alpha_interp <- "Non calculable"
      alpha_color <- "danger"
      if (!is.na(alpha_val)) {
        if (alpha_val >= 0.9) { alpha_interp <- "Excellent"; alpha_color <- "success" }
        else if (alpha_val >= 0.8) { alpha_interp <- "Bon"; alpha_color <- "success" }
        else if (alpha_val >= 0.7) { alpha_interp <- "Acceptable"; alpha_color <- "warning" }
        else if (alpha_val >= 0.6) { alpha_interp <- "Discutable"; alpha_color <- "warning" }
        else if (alpha_val >= 0.5) { alpha_interp <- "Faible"; alpha_color <- "danger" }
        else { alpha_interp <- "Inacceptable"; alpha_color <- "danger" }
      }
      
      output$score_result_ui <- renderUI({
        tags$div(
          class = "alert alert-success",
          icon("check-circle"), tags$strong(" Succès !"),
          tags$p("La variable ", tags$code(new_var), " a été ajoutée à vos données."),
          tags$hr(),
          tags$h6("Analyse de fiabilité (Cohérence interne)"),
          tags$p(
            "Alpha de Cronbach : ", tags$strong(round(alpha_val, 3)), 
            tags$span(class = paste0("badge bg-", alpha_color, " ms-2"), alpha_interp)
          ),
          if (alpha_val < 0.7 && !is.na(alpha_val)) {
            tags$small(class = "text-muted", "ℹ️ Un Alpha < 0.7 indique que les questions sélectionnées ne mesurent peut-être pas exactement le même concept sous-jacent.")
          } else if (!is.na(alpha_val)) {
            tags$small(class = "text-muted", "ℹ️ L'échelle est suffisamment fiable pour être utilisée en recherche.")
          }
        )
      })
    })

    return(cleaned_data)
  })
}
