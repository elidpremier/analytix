#' Module Exportation du Rapport Word Global
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame
#' @param univar_reactive Reactive returning univariate results
#' @param bivar_reactive Reactive returning bivariate results
#' @param spec_reactive Reactive returning specialized results

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

safe_add_ft <- function(doc, obj) {
  ft <- get_flextable(obj)
  if (!is.null(ft)) {
    doc <- flextable::body_add_flextable(doc, ft)
  } else if (is.data.frame(obj) && nrow(obj) > 0) {
    ft_fb <- flextable::theme_vanilla(flextable::flextable(obj))
    doc <- flextable::body_add_flextable(doc, ft_fb)
  } else {
    df_inner <- get_dataframe(obj)
    if (!is.null(df_inner) && nrow(df_inner) > 0) {
      ft_fb <- flextable::theme_vanilla(flextable::flextable(df_inner))
      doc <- flextable::body_add_flextable(doc, ft_fb)
    } else {
      doc <- officer::body_add_par(doc, "Tableau non disponible pour cette analyse.", style = "Normal")
    }
  }
  doc
}

mod_export_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("file-word", class = "me-2 text-primary"),
        "Paramètres du Rapport"
      ),
      width = 360,
      
      textInput(ns("report_title"), "Titre du Rapport :", value = "Rapport d'Analyse Statistique"),
      textInput(ns("report_subtitle"), "Sous-titre / Projet :", value = "Étude Épidémiologique & Descriptive"),
      textInput(ns("report_author"), "Auteur / Investigateur :", value = "Dr / Chercheur"),
      textInput(ns("report_institution"), "Organisme / Client :", value = "Redaklab / Hôpital"),
      
      tags$hr(),
      tags$h6("Sections à Inclure :"),
      checkboxInput(ns("inc_summary"), " 1. Synthèse du Jeu de Données", value = TRUE),
      checkboxInput(ns("inc_missing"), " 2. Rapport sur les Données Manquantes", value = TRUE),
      checkboxInput(ns("inc_global_univar"), " 3. Tableau Descriptif Global", value = TRUE),
      checkboxInput(ns("inc_univar"), " 4. Analyse Univariée (variable active)", value = TRUE),
      checkboxInput(ns("inc_bivar"), " 5. Analyses Bivariées & Tests", value = TRUE),
      checkboxInput(ns("inc_cross_multi"), " 6. Tableau Croisé Multi-Prédicteurs (cross_multi)", value = TRUE),
      checkboxInput(ns("inc_multiv"), " 7. Régression Logistique Multivariée", value = TRUE),
      checkboxInput(ns("inc_biv_plots"), " 8. Graphiques Bivariés (Barres Groupées, Empilées, Boxplots)", value = TRUE),
      checkboxInput(ns("inc_spec"), " 9. Analyses Spécialisées (Likert, Multi-choix)", value = TRUE),
      checkboxInput(ns("inc_multi_likert"), " 10. Tableau Récapitulatif Likert Multi-Items", value = TRUE),
      checkboxInput(ns("inc_cor"), " 11. Matrice de Corrélations complète", value = TRUE),
      checkboxInput(ns("inc_diag"), " 12. Performance Diagnostique (Sensibilité, Spécificité, VPP, VPN, LR)", value = TRUE),
      
      tags$hr(),
      downloadButton(
        ns("download_word"),
        "Télécharger le Rapport Word (.docx)",
        class = "btn-success btn-lg w-100",
        icon = icon("file-word")
      )
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      bslib::card(
        bslib::card_header(
          tags$div(
            icon("eye", class = "me-2"), " Aperçu de la Structure du Document Word Global"
          )
        ),
        tags$div(
          class = "p-3",
          tags$h4(uiOutput(ns("prev_title"))),
          tags$p(class = "text-muted", uiOutput(ns("prev_subtitle"))),
          tags$p(tags$strong("Auteur : "), uiOutput(ns("prev_author"))),
          tags$hr(),
          tags$h5("Sommaire des Tableaux & Figures qui seront générés :"),
          tags$ol(
            tags$li("Synthèse du jeu de données (N, variables, exhaustivité)"),
            tags$li("Diagnostic des valeurs manquantes (tableau + carte visuelle)"),
            tags$li("Tableau Descriptif Global (toutes variables)"),
            tags$li("Analyse Univariée de la variable active (numérique, catégorielle, binaire, Likert ou Âge)"),
            tags$li("Comparaisons bivariées par groupes (Chi², Student, Mann-Whitney) + ANOVA & Tukey si applicable"),
            tags$li("Tableau Croisé Multi-Prédicteurs avec OR bruts [IC95%] et p-values (cross_multi)"),
            tags$li("Modélisation par Régression Logistique Multivariée (ORa [IC95%], AIC)"),
            tags$li("Graphiques bivariés (barres groupées, 100% empilées, boxplots)"),
            tags$li("Échelles de Likert (analyse univariée + récapitulatif multi-items) & Graphique Divergent"),
            tags$li("Réponses Multiples (fréquences, % participants)"),
            tags$li("Matrice de Corrélations complète (Pearson/Spearman avec mise en gras des corrélations significatives)"),
            tags$li("Performance Diagnostique (VP, FP, FN, VN, Sensibilité, Spécificité, VPP, VPN, LR+, LR-)")
          ),
          tags$div(
            class = "alert alert-success mt-4",
            icon("check-circle", class = "me-2"),
            "Le document produit est un fichier Microsoft Word (.docx) natif, entièrement modifiable et conforme aux standards de publication scientifique francophone."
          )
        )
      )
    )
  )
}

mod_export_server <- function(id, data_reactive, univar_reactive, bivar_reactive,
                              spec_reactive, model_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$prev_title <- renderText({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Attention : Aucun jeu de données chargé. Veuillez d'abord charger vos données dans l'onglet 'Données & Libellés' pour générer le rapport.")
      )
      input$report_title
    })
    output$prev_subtitle <- renderText({
      req(data_reactive())
      input$report_subtitle
    })
    output$prev_author <- renderText({
      req(data_reactive())
      paste(input$report_author, "-", input$report_institution)
    })
    
    output$download_word <- downloadHandler(
      filename = function() {
        paste0("Rapport_Analytix_Complet_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        df <- data_reactive()
        if (is.null(df)) {
          showNotification("Impossible de générer le rapport : Aucun jeu de données chargé.", type = "error")
          return(NULL)
        }
        shiny::withProgress(message = 'Génération du rapport Word global...', value = 0, {
          
          shiny::incProgress(0.05, detail = "Initialisation du document...")
          
          doc <- officer::read_docx()
          
          # Cover / Title section
          doc <- officer::body_add_par(doc, input$report_title, style = "heading 1")
          doc <- officer::body_add_par(doc, input$report_subtitle, style = "heading 2")
          doc <- officer::body_add_par(doc, paste("Auteur :", input$report_author, "| Organisme :", input$report_institution, "| Date :", Sys.Date()), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          # ============================================================
          # Section 1 - Synthèse du Jeu de Données
          # ============================================================
          shiny::incProgress(0.08, detail = "Synthèse des données...")
          
          if (isTRUE(input$inc_summary) && !is.null(df)) {
            doc <- officer::body_add_par(doc, "1. Aperçu du Jeu de Données", style = "heading 1")
            meta_df <- data.frame(
              Indicateur = c("Nombre total d'observations", "Nombre de variables", "Taux global d'exhaustivité"),
              Valeur = c(nrow(df), ncol(df), paste0(round((1 - sum(is.na(df))/(nrow(df)*ncol(df)))*100, 1), "%"))
            )
            ft_meta <- flextable::theme_vanilla(flextable::flextable(meta_df))
            doc <- flextable::body_add_flextable(doc, ft_meta)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          
          # ============================================================
          # Section 1.2 - Valeurs Manquantes
          # ============================================================
          shiny::incProgress(0.12, detail = "Rapport données manquantes...")

          if (isTRUE(input$inc_missing) && !is.null(df)) {
            doc <- officer::body_add_par(doc, "1.2. Diagnostic des Valeurs Manquantes", style = "heading 2")

            # Utiliser analytix::missing_report si disponible
            if (exists("missing_report", where = asNamespace("analytix"))) {
              mr_res <- tryCatch(analytix::missing_report(df), error = function(e) NULL)
              if (!is.null(mr_res)) {
                ft_mr <- get_flextable(mr_res)
                if (!is.null(ft_mr)) doc <- flextable::body_add_flextable(doc, ft_mr)
              }
            }

            # Toujours ajouter aussi le tableau simple des NA
            na_counts <- sapply(df, function(x) sum(is.na(x)))
            na_pct <- round((na_counts / nrow(df)) * 100, 1)
            na_df <- data.frame(
              `Variable` = names(df),
              `Nombre de NA` = na_counts,
              `Proportion` = paste0(na_pct, " %"),
              check.names = FALSE
            )
            na_df_sorted <- na_df[order(-na_counts), ]
            ft_na <- flextable::theme_vanilla(flextable::flextable(na_df_sorted))
            doc <- flextable::body_add_flextable(doc, ft_na)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }

          # ============================================================
          # Section 1.3 - Tableau Descriptif Global
          # ============================================================
          shiny::incProgress(0.20, detail = "Tableau Descriptif Global...")

          if (isTRUE(input$inc_global_univar)) {
            u_res <- tryCatch(univar_reactive(), error = function(e) NULL)
            if (!is.null(u_res) && !is.null(u_res$global_descriptive)) {
              doc <- officer::body_add_par(doc, "1.3. Tableau Descriptif Global", style = "heading 2")
              for (var_name in names(u_res$global_descriptive)) {
                obj <- u_res$global_descriptive[[var_name]]
                ft <- get_flextable(obj)
                doc <- officer::body_add_par(doc, paste("Description de :", var_name), style = "heading 3")
                doc <- safe_add_ft(doc, obj)
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }

          # ============================================================
          # Section 2 - Analyse Univariée
          # ============================================================
          shiny::incProgress(0.30, detail = "Analyse Univariée...")
          
          if (isTRUE(input$inc_univar)) {
            u_res <- tryCatch(univar_reactive(), error = function(e) NULL)
            if (!is.null(u_res) && !is.null(u_res$table)) {
              doc <- officer::body_add_par(doc, "2. Analyse Univariée", style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Variable analysée :", u_res$var), style = "heading 2")
              doc <- safe_add_ft(doc, u_res$table)
              
              # Ajouter l'analyse de prévalence si disponible
              if (!is.null(u_res$prevalence) && is.data.frame(u_res$prevalence)) {
                doc <- officer::body_add_par(doc, "Calcul de Prévalence / Proportion", style = "heading 3")
                ft_prev <- flextable::theme_vanilla(flextable::flextable(u_res$prevalence))
                doc <- flextable::body_add_flextable(doc, ft_prev)
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          # ============================================================
          # Section 3 - Analyses Bivariées
          # ============================================================
          shiny::incProgress(0.45, detail = "Analyses Bivariées...")
          
          if (isTRUE(input$inc_bivar)) {
            b_res <- tryCatch(bivar_reactive(), error = function(e) NULL)
            if (!is.null(b_res) && !is.null(b_res$res) && b_res$res$method == "group_comparison") {
              doc <- officer::body_add_par(doc, "3. Analyse Bivariée & Tests Statistiques", style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Outcome :", b_res$target), style = "heading 2")
              
              res_info <- b_res$res
              method  <- res_info$method
              type    <- res_info$type
              results <- res_info$results

              if (method == "group_comparison" && type == "list") {
                for (item in results) {
                  pred_name <- item$pred
                  val       <- item$value
                  anova_val <- item$anova
                  doc <- officer::body_add_par(doc, paste("Comparaison :", pred_name, "vs", b_res$target), style = "heading 3")
                  doc <- safe_add_ft(doc, val)

                  if (!is.null(anova_val)) {
                    doc <- officer::body_add_par(doc, "Tableau d'ANOVA à un facteur", style = "heading 4")
                    ft_anova <- get_flextable(anova_val$anova)
                    if (!is.null(ft_anova)) doc <- flextable::body_add_flextable(doc, ft_anova)
                    doc <- officer::body_add_par(doc, "Test post-hoc de Tukey (HSD)", style = "heading 4")
                    ft_tukey <- get_flextable(anova_val$tukey)
                    if (!is.null(ft_tukey)) doc <- flextable::body_add_flextable(doc, ft_tukey)
                  }
                  doc <- officer::body_add_par(doc, "", style = "Normal")
                }
              } else {
                doc <- safe_add_ft(doc, results)
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }

          # ============================================================
          # Section 3.2 - Tableau Croisé Multi-Prédicteurs (cross_multi)
          # ============================================================
          shiny::incProgress(0.52, detail = "Tableau Croisé Multi-Prédicteurs...")

          if (isTRUE(input$inc_cross_multi)) {
            b_res <- tryCatch(bivar_reactive(), error = function(e) NULL)
            if (!is.null(b_res) && !is.null(b_res$res) && b_res$res$method == "cross_multi") {
              doc <- officer::body_add_par(doc, "3.2. Tableau Croisé Multi-Prédicteurs (cross_multi)", style = "heading 2")
              res_info <- b_res$res
              doc <- safe_add_ft(doc, res_info$results)
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          # ============================================================
          # Section 3.3 - Modélisation (régression)
          # ============================================================
          shiny::incProgress(0.57, detail = "Régression Multivariée...")
          
          if (isTRUE(input$inc_multiv)) {
            # Priorité 1 : résultat du module de modélisation dédié
            m_res <- tryCatch(
              if (!is.null(model_reactive)) model_reactive() else NULL,
              error = function(e) NULL
            )
            
            if (!is.null(m_res) && !is.null(m_res$ft)) {
              doc <- officer::body_add_par(doc, "3.3. Modélisation par Régression", style = "heading 2")
              if (!is.null(m_res$interp) && nchar(m_res$interp) > 0) {
                doc <- officer::body_add_par(doc, m_res$interp, style = "Normal")
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
              doc <- flextable::body_add_flextable(doc, m_res$ft)
              doc <- officer::body_add_par(doc, "", style = "Normal")
            } else {
              # Priorité 2 : résultat de la régression bivariée
              b_res_reg <- tryCatch(bivar_reactive(), error = function(e) NULL)
              if (!is.null(b_res_reg) && !is.null(b_res_reg$res) &&
                  b_res_reg$res$method == "multivariate_or") {
                doc <- officer::body_add_par(doc, "3.3. Modélisation par Régression Logistique Multivariée", style = "heading 2")
                doc <- safe_add_ft(doc, b_res_reg$res$results)
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }

          # ============================================================
          # Section 3.4 - Graphiques Bivariés
          # ============================================================
          shiny::incProgress(0.62, detail = "Graphiques bivariés...")

          if (isTRUE(input$inc_biv_plots)) {
            b_res <- tryCatch(bivar_reactive(), error = function(e) NULL)
            if (!is.null(b_res) && !is.null(b_res$biv_plot) && inherits(b_res$biv_plot, "ggplot")) {
              doc <- officer::body_add_par(doc, "3.4. Graphique Bivarié", style = "heading 2")
              tmp_png <- tempfile(fileext = ".png")
              tryCatch({
                ggplot2::ggsave(tmp_png, plot = b_res$biv_plot, width = 9, height = 5.5, dpi = 150)
                doc <- officer::body_add_img(doc, src = tmp_png, width = 6, height = 3.7)
              }, error = function(e) {
                doc <<- officer::body_add_par(doc, "Graphique bivarié non disponible.", style = "Normal")
              })
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }

          # ============================================================
          # Section 4 - Analyses Spécialisées (Likert, Multi-choix)
          # ============================================================
          shiny::incProgress(0.70, detail = "Analyses Spécialisées...")

          if (isTRUE(input$inc_spec)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res)) {
              doc <- officer::body_add_par(doc, "4. Analyses Spécialisées", style = "heading 1")

              # 4.1 Likert individuel
              if (!is.null(s_res$likert)) {
                doc <- officer::body_add_par(doc, "4.1. Échelle de Likert (Variable unique)", style = "heading 2")
                doc <- safe_add_ft(doc, s_res$likert)
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }

              # 4.2 Graphique divergent Likert
              if (!is.null(s_res$likert_plot) && inherits(s_res$likert_plot, "ggplot")) {
                doc <- officer::body_add_par(doc, "4.2. Graphique Divergent Likert", style = "heading 2")
                tmp_lik_png <- tempfile(fileext = ".png")
                tryCatch({
                  ggplot2::ggsave(tmp_lik_png, plot = s_res$likert_plot, width = 10, height = 5.5, dpi = 150)
                  doc <- officer::body_add_img(doc, src = tmp_lik_png, width = 6.5, height = 3.6)
                }, error = function(e) {
                  doc <<- officer::body_add_par(doc, "Graphique Likert non disponible.", style = "Normal")
                })
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }

              # 4.3 Réponses Multiples
              if (!is.null(s_res$multi)) {
                doc <- officer::body_add_par(doc, "4.3. Analyse des Réponses Multiples", style = "heading 2")
                doc <- safe_add_ft(doc, s_res$multi)
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }

          # ============================================================
          # Section 4.4 - Tableau Récapitulatif Likert Multi-Items
          # ============================================================
          shiny::incProgress(0.78, detail = "Récapitulatif Likert Multi-Items...")

          if (isTRUE(input$inc_multi_likert)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res) && !is.null(s_res$multi_likert)) {
              doc <- officer::body_add_par(doc, "4.4. Tableau Récapitulatif Likert Multi-Items", style = "heading 2")
              doc <- safe_add_ft(doc, s_res$multi_likert)
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }

          # ============================================================
          # Section 4.5 - Matrice de Corrélations
          # ============================================================
          shiny::incProgress(0.85, detail = "Matrice de Corrélations...")

          if (isTRUE(input$inc_cor)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res) && !is.null(s_res$correlation_matrix)) {
              doc <- officer::body_add_par(doc, "4.5. Matrice de Corrélations complète", style = "heading 2")
              doc <- safe_add_ft(doc, s_res$correlation_matrix)
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }

          # ============================================================
          # Section 4.6 - Performance Diagnostique
          # ============================================================
          shiny::incProgress(0.92, detail = "Performance Diagnostique...")

          if (isTRUE(input$inc_diag)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res) && !is.null(s_res$diagnostic)) {
              doc <- officer::body_add_par(doc, "4.6. Performance Diagnostique du Test", style = "heading 2")
              doc <- safe_add_ft(doc, s_res$diagnostic)
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          shiny::incProgress(1.0, detail = "Finalisation...")
          print(doc, target = file)
        })
      }
    )
  })
}
