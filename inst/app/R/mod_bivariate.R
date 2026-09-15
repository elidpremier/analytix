#' Module Analyse Bivariée & Modélisation avec Exports Immédiats
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

mod_bivariate_ui <- function(id) {
  ns <- NS(id)
  
  bslib::navset_card_tab(
    # Tab 1: Analyse Statistique (Tableau)
    bslib::nav_panel(
      title = tags$span(icon("table"), " Tableaux & Tests"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = tags$div(
            icon("project-diagram", class = "me-2 text-primary"),
            "Configuration Bivariée"
          ),
          width = 340,
          
          selectInput(
            ns("target_var"),
            "Variable Cible / Outcome (ex: Groupe, Maladie) :",
            choices = NULL
          ),
          
          # Dynamic positive/interest value for the outcome
          uiOutput(ns("outcome_pos_ui")),

          selectInput(
            ns("pred_vars"),
            "Variables Prédicteurs / Explicatives :",
            choices = NULL,
            multiple = TRUE
          ),
          
          selectInput(
            ns("method"),
            "Type d'Analyse Statistical / Table :",
            choices = c(
              "📊 Tableau 1 : Comparaison de Groupes (Chi², Student, Mann-Whitney)" = "group_comparison",
              "📈 Table d'Odds Ratios (Régression Logistique Bivariée)" = "or_table",
              "🧬 Régression Logistique Multivariée (Odds Ratios ajustés)" = "multivariate_or",
              "🔀 Tableau Croisé Multi-Prédicteurs (cross_multi)" = "cross_multi"
            ),
            selected = "group_comparison"
          ),
          
          # Options spécifiques cross_multi
          conditionalPanel(
            condition = sprintf("input['%s'] == 'cross_multi'", ns("method")),
            tags$hr(),
            tags$h6(icon("sliders-h", class = "me-1"), "Options cross_multi"),
            selectInput(ns("cross_multi_method"), "Méthode statistique :",
                        choices = c("Régression logistique" = "logistic", "Niveau (Fisher)" = "level"),
                        selected = "logistic"),
            checkboxInput(ns("cross_include_na"), "Inclure les NA", value = FALSE),
            tags$p(class = "text-muted small mt-1",
                   "Génère un tableau hiérarchique multi-prédicteurs avec OR bruts [IC95%] et p-values.")
          ),
          
          tags$hr(),
          tags$h6(icon("sliders-h", class = "me-1"), "Options d'Analyse"),
          numericInput(ns("bivar_digits"), "Nombre de décimales :", value = 2, min = 0, max = 6),
          numericInput(ns("bivar_conf"), "Niveau de confiance (%) :", value = 95, min = 50, max = 99),
          textInput(ns("bivar_header_color"), "Couleur de l'en-tête (Hex) :", value = "#0284c7"),

          tags$hr(),
          tags$div(
            class = "alert alert-warning py-2 px-3",
            style = "font-size: 0.85rem;",
            icon("lightbulb", class = "me-1"),
            "Les p-values et Odds Ratios (avec IC 95%) sont calculés automatiquement selon la nature des variables."
          )
        ),
        
        tags$div(
          class = "container-fluid p-0",
          
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Résultats Bivariés & Tests Statistiques"),
                tags$div(
                  class = "btn-group btn-group-sm",
                  downloadButton(ns("dl_bivar_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
                  downloadButton(ns("dl_bivar_csv"), "CSV", class = "btn-outline-secondary btn-sm")
                )
              )
            ),
            uiOutput(ns("bivariate_results_ui"))
          )
        )
      )
    ),

    # Tab 2: Graphiques Bivariés
    bslib::nav_panel(
      title = tags$span(icon("image"), " Graphiques Bivariés"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = tags$div(
            icon("image", class = "me-2 text-primary"),
            "Configuration Graphique Bivarié"
          ),
          width = 320,
          selectInput(
            ns("plot_x_var"),
            "Variable X (Catégorielle / Groupement) :",
            choices = NULL
          ),
          selectInput(
            ns("plot_fill_var"),
            "Variable de Remplissage / Groupement :",
            choices = NULL
          ),
          selectInput(
            ns("plot_y_var"),
            "Variable Y (Numérique, pour Boxplot) :",
            choices = NULL
          ),
          tags$hr(),
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = tags$span(icon("image", class = "me-1"), "Options Graphiques Avancées"),
              value = "plot_opts",
              selectInput(
                ns("bivar_plot_type"),
                "Type de Graphique :",
                choices = c(
                  "📊 Barres Groupées (plot_grouped_bar)" = "grouped_bar",
                  "📶 Barres Empilées 100% (plot_stacked_bar_100)" = "stacked_100",
                  "📦 Boîtes à Moustaches par Groupe (plot_boxplot)" = "boxplot"
                ),
                selected = "grouped_bar"
              ),
              textInput(ns("bivar_plot_title"), "Titre du graphique :", value = ""),
              
              # Couleur/Palette
              selectInput(
                ns("plot_palette"), "Palette de couleurs :",
                choices = c("Défaut", "Analytix", "viridis", "Set1", "Set2", "Pastel1", "Dark2"),
                selected = "Défaut"
              ),
              
              # Thème
              selectInput(
                ns("plot_theme"),
                "Thème du graphique :",
                choices = c(
                  "Minimaliste" = "minimal",
                  "Classique" = "classic",
                  "Clair (Light)" = "light",
                  "Noir & Blanc (BW)" = "bw",
                  "Sombre (Dark)" = "dark"
                ),
                selected = "minimal"
              ),
              
              # Options texte/légende
              numericInput(ns("plot_base_size"), "Taille du texte (police) :", value = 12, min = 6, max = 24),
              selectInput(
                ns("plot_legend"), "Position de la légende :",
                choices = c("Droite" = "right", "Bas" = "bottom", "Haut" = "top", "Gauche" = "left", "Masquée" = "none"),
                selected = "right"
              ),
              
              # Orientations & labels
              checkboxInput(ns("plot_horiz"), "Orientation Horizontale (Barres)", value = FALSE),
              checkboxInput(ns("bivar_show_pct"), "Afficher les % (Barres Groupées)", value = FALSE)
            )
          ),
          tags$hr(),
          tags$div(
            class = "btn-group btn-group-sm w-100",
            downloadButton(ns("dl_biv_plot_png"), "PNG", class = "btn-outline-success btn-sm w-50"),
            downloadButton(ns("dl_biv_plot_pdf"), "PDF", class = "btn-outline-danger btn-sm w-50")
          )
        ),

        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("image", class = "me-2"), " Graphique Bivarié"),
              tags$span(class = "badge bg-info text-white", uiOutput(ns("biv_plot_badge_ui")))
            )
          ),
          plotOutput(ns("bivariate_plot"), height = "480px")
        )
      )
    )
  )
}

mod_bivariate_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "target_var", choices = names(df))
      updateSelectInput(session, "pred_vars", choices = names(df))
      updateSelectInput(session, "plot_x_var", choices = names(df))
      updateSelectInput(session, "plot_fill_var", choices = names(df))
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "plot_y_var", choices = num_cols)
    })

    # Render dynamic Outcome interest value selector
    output$outcome_pos_ui <- renderUI({
      df <- data_reactive()
      req(df, input$target_var)

      vals <- unique(df[[input$target_var]])
      vals <- vals[!is.na(vals)]

      selectInput(
        ns("outcome_pos_val"),
        "Valeur de référence / Événement positif :",
        choices = vals,
        selected = if (length(vals) > 0) vals[1] else NULL
      )
    })
    
    bivariate_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$target_var), "Veuillez sélectionner une variable cible (Outcome)."),
        shiny::need(isTruthy(input$pred_vars) && length(input$pred_vars) > 0, "Veuillez sélectionner au moins une variable explicative.")
      )
      
      target <- input$target_var
      preds <- input$pred_vars
      method <- input$method
      
      digits <- as.integer(input$bivar_digits)
      conf_level <- as.numeric(input$bivar_conf) / 100
      header_color <- input$bivar_header_color
      if (is.null(header_color) || nchar(trimws(header_color)) == 0) header_color <- "#0284c7"

      outcome_pos <- input$outcome_pos_val
      tryCatch({
        # --- cross_multi Method ---
        if (method == "cross_multi") {
          req(outcome_pos)
          cross_method <- input$cross_multi_method
          cross_include_na <- isTRUE(input$cross_include_na)
          cross_fn <- if (exists("tbl_cross_multi", where = asNamespace("analytix"))) analytix::tbl_cross_multi else if (exists("cross_multi", where = asNamespace("analytix"))) analytix::cross_multi else NULL
          if (!is.null(cross_fn)) {
            target_sym <- rlang::sym(target)
            res <- tryCatch({
              cross_fn(
                df,
                outcome = !!target_sym,
                predictors = preds,
                outcome_level = outcome_pos,
                include_na = cross_include_na,
                digits = digits,
                color = header_color,
                method = cross_method
              )
            }, error = function(e) {
              # Retry without color/digits if signature differs
              tryCatch(
                cross_fn(df, outcome = !!target_sym, predictors = preds, outcome_level = outcome_pos),
                error = function(e2) NULL
              )
            })
            if (!is.null(res)) {
              return(list(method = "cross_multi", type = "flextable", target = target, results = res))
            }
          }
          # Fallback simple crosstab
          res_df <- data.frame(
            Predictor = preds,
            Note = "Fonction tbl_cross_multi non disponible dans le package analytix chargé."
          )
          return(list(method = "cross_multi", type = "df", target = target, results = res_df))

        # --- group_comparison Method ---
        } else if (method == "group_comparison") {
          by_group_fn <- if (exists("desc_by_group", where = asNamespace("analytix"))) analytix::desc_by_group else if (exists("descr_by_group", where = asNamespace("analytix"))) analytix::descr_by_group else NULL
          anova_fn <- if (exists("tbl_anova", where = asNamespace("analytix"))) analytix::tbl_anova else if (exists("anova_table", where = asNamespace("analytix"))) analytix::anova_table else NULL
          
          if (!is.null(by_group_fn)) {
            target_sym <- rlang::sym(target)
            
            ft_list <- lapply(preds, function(p) {
              p_sym <- rlang::sym(p)
              res <- tryCatch(
                by_group_fn(df, var = !!p_sym, by = !!target_sym, digits = digits, color = header_color),
                error = function(e) {
                  tryCatch(
                    by_group_fn(df, var = !!p_sym, by = !!target_sym),
                    error = function(e2) NULL
                  )
                }
              )

              anova_res <- NULL
              assumptions_msg <- ""
              interp_msg <- ""
              
              p_vec <- df[[p]]
              t_vec <- df[[target]]
              
              if (is.numeric(p_vec) && length(unique(na.omit(t_vec))) >= 2) {
                # Tests d'hypothèses
                sh_p <- tryCatch(stats::shapiro.test(p_vec[1:min(length(p_vec), 5000)])$p.value, error = function(e) NULL)
                bt <- tryCatch(stats::bartlett.test(p_vec ~ as.factor(t_vec)), error = function(e) NULL)
                
                if (!is.null(sh_p) && sh_p < 0.05) {
                  assumptions_msg <- paste0(assumptions_msg, "⚠️ <b>Normalité :</b> La distribution de '", p, "' s'écarte de la normale (Shapiro-Wilk p < 0.05). Un test non paramétrique (Kruskal-Wallis) peut être préférable.<br>")
                }
                if (!is.null(bt) && bt$p.value < 0.05) {
                  assumptions_msg <- paste0(assumptions_msg, "⚠️ <b>Homoscédasticité :</b> Les variances ne sont pas homogènes (Bartlett p < 0.05). L'ANOVA classique n'est pas recommandée.<br>")
                }
                
                if (!is.null(anova_fn)) {
                  anova_res <- tryCatch(
                    anova_fn(df, var = !!p_sym, group = !!target_sym, digits = digits, color = header_color),
                    error = function(e) {
                      tryCatch(anova_fn(df, var = !!p_sym, group = !!target_sym), error = function(e2) NULL)
                    }
                  )
                }
                
                av <- tryCatch(summary(aov(p_vec ~ as.factor(t_vec))), error = function(e) NULL)
                if (!is.null(av)) {
                  pv <- av[[1]][["Pr(>F)"]][1]
                  if (!is.na(pv) && pv < 0.05) {
                    interp_msg <- "💡 Il existe une différence significative d'au moins un groupe par rapport aux autres (ANOVA p < 0.05)."
                  } else {
                    interp_msg <- "💡 Aucune différence significative entre les groupes (ANOVA p &ge; 0.05)."
                  }
                }
              } else {
                # Chi-Square pour catégoriel
                tb <- table(df[[p]], df[[target]])
                cq <- tryCatch(chisq.test(tb), error = function(e) NULL)
                if (!is.null(cq)) {
                  if (cq$p.value < 0.05) {
                    interp_msg <- "💡 Il existe une association statistiquement significative entre les deux variables (Chi-2 p < 0.05)."
                  } else {
                    interp_msg <- "💡 Pas d'association statistiquement significative entre les deux variables (Chi-2 p &ge; 0.05)."
                  }
                }
              }

              if (is.null(res)) {
                tb <- table(df[[p]], df[[target]])
                res <- data.frame(
                  `Variable/Modalité` = paste(p, "-", rownames(tb)),
                  `N` = as.numeric(rowSums(tb)),
                  `Test` = "Chi-Square",
                  check.names = FALSE
                )
              }
              list(pred = p, value = res, anova = anova_res, assumptions = assumptions_msg, interp = interp_msg)
            })
            ft_list <- Filter(function(x) !is.null(x$value), ft_list)
            return(list(method = "group_comparison", type = "list", target = target, results = ft_list))
          }
          
          res_list <- lapply(preds, function(p) {
            tb <- table(df[[p]], df[[target]])
            chi <- tryCatch(chisq.test(tb), error = function(e) NULL)
            data.frame(
              `Predictor` = p,
              `Target` = target,
              `P_Value` = if (!is.null(chi)) round(chi$p.value, digits) else "N/A",
              `Test` = "Chi-Square",
              check.names = FALSE
            )
          })
          combined_df <- do.call(rbind, res_list)
          return(list(method = "group_comparison", type = "df", target = target, results = combined_df))
          
        # --- or_table Method ---
        } else if (method == "or_table") {
          req(outcome_pos)

          biv_or_fn <- if (exists("tbl_bivariate_or", where = asNamespace("analytix"))) analytix::tbl_bivariate_or else if (exists("bivariate_or_table", where = asNamespace("analytix"))) analytix::bivariate_or_table else NULL
          if (!is.null(biv_or_fn)) {
            res <- tryCatch(
              biv_or_fn(
                df,
                outcome = target,
                exposures = preds,
                outcome_positive_val = outcome_pos,
                conf_level = conf_level,
                digits = digits,
                color = header_color
              ),
              error = function(e) {
                biv_or_fn(df, outcome = target, exposures = preds, outcome_positive_val = outcome_pos)
              }
            )
            if (!is.null(res)) {
              return(list(method = "or_table", type = "flextable", target = target, results = res))
            }
          }
          
          # Fallback OR calculation
          rows_list <- list()
          df_mod <- df
          df_mod$outcome_bin <- ifelse(df_mod[[target]] == outcome_pos, 1, 0)

          for (exp_var in preds) {
            exp_vec <- df_mod[[exp_var]]
            formula_obj <- stats::as.formula(paste("outcome_bin ~ factor(", exp_var, ")"))
            fit <- tryCatch(stats::glm(formula_obj, data = df_mod, family = stats::binomial()), error = function(e) NULL)

            if (is.null(fit)) next

            co <- summary(fit)$coefficients
            ci <- tryCatch(suppressMessages(stats::confint(fit, level = conf_level)), error = function(e) {
              ci_est <- coef(fit)
              se <- sqrt(diag(vcov(fit)))
              z <- stats::qnorm(1 - (1 - conf_level)/2)
              cbind(ci_est - z * se, ci_est + z * se)
            })

            levels_val <- levels(factor(exp_vec[!is.na(exp_vec)]))
            for (i in seq_along(levels_val)) {
              mod <- levels_val[i]
              sub_df <- df_mod[df_mod[[exp_var]] == mod & !is.na(df_mod[[exp_var]]) & !is.na(df_mod$outcome_bin), ]
              n_mod <- nrow(sub_df)
              n_pos <- sum(sub_df$outcome_bin == 1)
              pct_pos <- (n_pos / n_mod) * 100
              n_pct_str <- sprintf("%d/%d (%.1f%%)", n_pos, n_mod, pct_pos)

              if (i == 1) {
                or_str <- "1.00 (Réf.)"
                p_str <- "-"
              } else {
                coef_row_name <- paste0("factor(", exp_var, ")", mod)
                if (coef_row_name %in% rownames(co)) {
                  or_val <- exp(co[coef_row_name, "Estimate"])
                  p_val <- co[coef_row_name, "Pr(>|z|)"]
                  ci_low <- exp(ci[coef_row_name, 1])
                  ci_high <- exp(ci[coef_row_name, 2])
                  or_fmt <- format(round(or_val, digits), nsmall = digits)
                  low_fmt <- format(round(ci_low, digits), nsmall = digits)
                  high_fmt <- format(round(ci_high, digits), nsmall = digits)
                  or_str <- sprintf("%s [%s - %s]", or_fmt, low_fmt, high_fmt)
                  p_str <- if (p_val < 0.001) "< 0,001" else format(round(p_val, 3))
                } else {
                  or_str <- "-"
                  p_str <- "-"
                }
              }

              rows_list[[length(rows_list) + 1]] <- data.frame(
                Variable = exp_var,
                Modalite = mod,
                Effectif_Pct = n_pct_str,
                OR_IC95 = or_str,
                P_value = p_str,
                stringsAsFactors = FALSE
              )
            }
          }

          if (length(rows_list) > 0) {
            out_df <- dplyr::bind_rows(rows_list)
            dup_idx <- duplicated(out_df$Variable)
            out_df$Variable[dup_idx] <- ""
            names(out_df) <- c("Variable", "Modalité", paste0("Effectif (", outcome_pos, ")"), "OR brut [IC95%]", "p-value")

            ft <- flextable::flextable(out_df)
            if (exists("theme_analytique", where = asNamespace("analytix"))) {
              ft <- analytix::theme_analytique(ft, color = header_color)
            } else {
              ft <- flextable::theme_vanilla(ft)
            }
            ft <- flextable::set_caption(ft, paste("Association bivariée avec", target, "(Événement :", outcome_pos, ")"))
            return(list(method = "or_table", type = "flextable", target = target, results = ft))
          }

          res_df <- data.frame(
            Predictor = preds,
            Outcome = target,
            Odds_Ratio = "Calcul non disponible",
            IC_95 = "[ - ]"
          )
          return(list(method = "or_table", type = "df", target = target, results = res_df))

        # --- multivariate_or Method ---
        } else if (method == "multivariate_or") {
          log_fn <- if (exists("tbl_logistic", where = asNamespace("analytix"))) analytix::tbl_logistic else if (exists("multivariable_logistic_table", where = asNamespace("analytix"))) analytix::multivariable_logistic_table else NULL
          if (!is.null(log_fn)) {
            form <- stats::as.formula(paste(target, "~", paste(preds, collapse = " + ")))
            fit_multi <- tryCatch({
              df_multi <- df
              if (!is.null(outcome_pos)) {
                df_multi[[target]] <- ifelse(df_multi[[target]] == outcome_pos, 1, 0)
              }
              stats::glm(form, data = df_multi, family = stats::binomial())
            }, error = function(e) NULL)

            if (!is.null(fit_multi)) {
              res <- tryCatch({
                log_fn(fit_multi, digits = digits, color = header_color)
              }, error = function(e) {
                tryCatch({
                  log_fn(fit_multi)
                }, error = function(e2) {
                  log_fn(form, data = df)
                })
              })
              return(list(method = "multivariate_or", type = "flextable", target = target, results = res))
            }
          }

          df_multi <- df
          df_multi$outcome_bin <- ifelse(df_multi[[target]] == outcome_pos, 1, 0)
          form <- stats::as.formula(paste("outcome_bin ~", paste(preds, collapse = " + ")))
          fit <- tryCatch(stats::glm(form, data = df_multi, family = stats::binomial()), error = function(e) NULL)

          if (!is.null(fit)) {
            co <- summary(fit)$coefficients
            ci <- tryCatch(suppressMessages(stats::confint(fit, level = conf_level)), error = function(e) {
              ci_est <- coef(fit)
              se <- sqrt(diag(vcov(fit)))
              z <- stats::qnorm(1 - (1 - conf_level)/2)
              cbind(ci_est - z * se, ci_est + z * se)
            })

            res_rows <- list()
            for (rn in rownames(co)) {
              if (rn == "(Intercept)") next
              or_val <- exp(co[rn, "Estimate"])
              p_val <- co[rn, "Pr(>|z|)"]
              ci_low <- exp(ci[rn, 1])
              ci_high <- exp(ci[rn, 2])

              res_rows[[length(res_rows) + 1]] <- data.frame(
                Indicateur = rn,
                `OR ajusté` = format(round(or_val, digits), nsmall = digits),
                `IC 95%` = sprintf("[%s - %s]", format(round(ci_low, digits), nsmall = digits), format(round(ci_high, digits), nsmall = digits)),
                `p-value` = if (p_val < 0.001) "< 0,001" else format(round(p_val, 3)),
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
            }
            if (length(res_rows) > 0) {
              ft <- flextable::flextable(dplyr::bind_rows(res_rows))
              ft <- flextable::theme_vanilla(ft)
              ft <- flextable::set_caption(ft, paste("Régression Logistique Multivariée - Outcome :", target))
              return(list(method = "multivariate_or", type = "flextable", target = target, results = ft))
            }
          }

          res_df <- data.frame(
            Predictor = preds,
            Outcome = target,
            Note = "Régression multivariée non disponible"
          )
          return(list(method = "multivariate_or", type = "df", target = target, results = res_df))
        }
      }, error = function(e) {
        err_df <- data.frame(Erreur = paste("Erreur bivariée :", e$message))
        return(list(method = "error", type = "df", target = target, results = err_df))
      })
    })
    
    output$bivariate_results_ui <- renderUI({
      res_info <- bivariate_res()
      req(res_info)
      
      method <- res_info$method
      type <- res_info$type
      results <- res_info$results

      if (method == "group_comparison" && type == "list") {
        tag_list <- lapply(seq_along(results), function(i) {
          item <- results[[i]]
          pred_name <- item$pred
          val <- item$value
          anova_val <- item$anova

          ft_val <- get_flextable(val)
          ft_anova <- if (!is.null(anova_val)) get_flextable(anova_val$anova) else NULL
          ft_tukey <- if (!is.null(anova_val)) get_flextable(anova_val$tukey) else NULL

          tags$div(
            class = "mb-4 p-3 border rounded bg-white",
            tags$h5(class = "fw-bold text-slate-800 mb-3", paste("Comparaison :", pred_name, "vs", res_info$target)),
            
            if (nchar(item$assumptions) > 0) {
              tags$div(class = "alert alert-warning mb-3", HTML(item$assumptions))
            },
            if (nchar(item$interp) > 0) {
              tags$div(class = "alert alert-info mb-3", HTML(item$interp))
            },
            
            if (!is.null(ft_val)) {
              flextable::htmltools_value(ft_val)
            } else if (is.data.frame(val)) {
              ft <- flextable::theme_vanilla(flextable::flextable(val))
              flextable::htmltools_value(ft)
            },

            if (!is.null(anova_val)) {
              tags$div(
                class = "mt-3 p-3 border-start border-primary bg-light rounded",
                tags$h6(class = "fw-bold text-primary", "Analyse de Variance (ANOVA) & Test Post-Hoc de Tukey"),
                tags$div(
                  class = "mt-2",
                  if (!is.null(ft_anova)) flextable::htmltools_value(ft_anova),
                  tags$div(class = "my-3"),
                  if (!is.null(ft_tukey)) flextable::htmltools_value(ft_tukey)
                )
              )
            }
          )
        })
        do.call(tagList, tag_list)
      } else {
        ft_results <- get_flextable(results)
        if (!is.null(ft_results)) {
          flextable::htmltools_value(ft_results)
        } else if (is.data.frame(results)) {
          ft <- flextable::theme_vanilla(flextable::flextable(results))
          flextable::htmltools_value(ft)
        }
      }
    })

    # --- Graphique Bivarié Logic ---
    output$biv_plot_badge_ui <- renderUI({
      switch(input$bivar_plot_type,
        "grouped_bar"  = tags$span("analytix::plot_bar_grouped"),
        "stacked_100"  = tags$span("analytix::plot_bar_stacked"),
        "boxplot"      = tags$span("analytix::plot_box"),
        tags$span("analytix")
      )
    })

    current_biv_plot <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$plot_x_var), "Veuillez sélectionner une variable X."),
        shiny::need(isTruthy(input$plot_fill_var) || input$bivar_plot_type == "boxplot", "Veuillez sélectionner une variable de remplissage.")
      )

      x_sym   <- rlang::sym(input$plot_x_var)
      fill_sym <- rlang::sym(input$plot_fill_var)
      y_sym   <- rlang::sym(input$plot_y_var)

      custom_title <- input$bivar_plot_title
      if (is.null(custom_title) || nchar(trimws(custom_title)) == 0) {
        custom_title <- NULL
      }

      plot_type <- input$bivar_plot_type

      p <- NULL

      tryCatch({
        if (plot_type == "grouped_bar") {
          bar_grp_fn <- if (exists("plot_bar_grouped", where = asNamespace("analytix"))) analytix::plot_bar_grouped else if (exists("plot_grouped_bar", where = asNamespace("analytix"))) analytix::plot_grouped_bar else NULL
          if (!is.null(bar_grp_fn)) {
            p <- bar_grp_fn(
              df,
              x    = !!x_sym,
              fill = !!fill_sym,
              title = custom_title,
              show_pct = isTRUE(input$bivar_show_pct)
            )
          }
          if (is.null(p)) {
            # Fallback
            df_plot <- df %>%
              dplyr::filter(!is.na(!!x_sym) & !is.na(!!fill_sym)) %>%
              dplyr::group_by(!!x_sym, !!fill_sym) %>%
              dplyr::summarise(n = dplyr::n(), .groups = "drop")
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(!!x_sym), y = n, fill = factor(!!fill_sym))) +
              ggplot2::geom_col(position = ggplot2::position_dodge(0.9), width = 0.8) +
              ggplot2::labs(title = custom_title, x = input$plot_x_var, y = "Effectif", fill = input$plot_fill_var) +
              ggplot2::theme_minimal(base_size = 13) +
              ggplot2::scale_fill_brewer(palette = "Set2")
          }

        } else if (plot_type == "stacked_100") {
          bar_stk_fn <- if (exists("plot_bar_stacked", where = asNamespace("analytix"))) analytix::plot_bar_stacked else if (exists("plot_stacked_bar_100", where = asNamespace("analytix"))) analytix::plot_stacked_bar_100 else NULL
          if (!is.null(bar_stk_fn)) {
            p <- bar_stk_fn(
              df,
              x    = !!x_sym,
              fill = !!fill_sym,
              title = custom_title
            )
          }
          if (is.null(p)) {
            df_plot <- df %>%
              dplyr::filter(!is.na(!!x_sym) & !is.na(!!fill_sym)) %>%
              dplyr::group_by(!!x_sym, !!fill_sym) %>%
              dplyr::summarise(n = dplyr::n(), .groups = "drop")
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(!!x_sym), y = n, fill = factor(!!fill_sym))) +
              ggplot2::geom_col(position = "fill") +
              ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
              ggplot2::labs(title = custom_title, x = input$plot_x_var, y = "Pourcentage (%)", fill = input$plot_fill_var) +
              ggplot2::theme_minimal(base_size = 13) +
              ggplot2::scale_fill_brewer(palette = "Set1")
          }

        } else if (plot_type == "boxplot") {
          shiny::validate(
            shiny::need(isTruthy(input$plot_y_var), "Veuillez sélectionner une variable Y numérique pour le boxplot.")
          )
          box_fn <- if (exists("plot_box", where = asNamespace("analytix"))) analytix::plot_box else if (exists("plot_boxplot", where = asNamespace("analytix"))) analytix::plot_boxplot else NULL
          if (!is.null(box_fn)) {
            p <- box_fn(
              df,
              x = !!x_sym,
              y = !!y_sym,
              title = custom_title
            )
          }
          if (is.null(p)) {
            df_plot <- df %>% dplyr::filter(!is.na(!!x_sym) & !is.na(!!y_sym))
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(!!x_sym), y = !!y_sym, fill = factor(!!x_sym))) +
              ggplot2::geom_boxplot(alpha = 0.7, outlier.colour = "red") +
              ggplot2::labs(title = custom_title, x = input$plot_x_var, y = input$plot_y_var) +
              ggplot2::theme_minimal(base_size = 13) +
              ggplot2::theme(legend.position = "none") +
              ggplot2::scale_fill_brewer(palette = "Set2")
          }
        }
        
        # Application du Custom Theme
        fmt_theme_fn <- if (exists("fmt_apply_theme", where = asNamespace("analytix"))) analytix::fmt_apply_theme else if (exists("apply_custom_theme", where = asNamespace("analytix"))) analytix::apply_custom_theme else NULL
        if (!is.null(p) && !is.null(fmt_theme_fn)) {
          p <- tryCatch({
            fmt_theme_fn(
              p,
              theme_name = input$plot_theme,
              base_size = input$plot_base_size,
              legend_pos = input$plot_legend,
              palette_name = input$plot_palette,
              flip_coord = isTRUE(input$plot_horiz) # Bivarié n'a pas toujours flip natif dans ses options
            )
          }, error = function(e) p)
        }
        
      }, error = function(e) {
        p <<- NULL
        showNotification(paste("Erreur graphique bivarié :", e$message), type = "error")
      })

      shiny::validate(
        shiny::need(!is.null(p), "Impossible de générer le graphique avec les paramètres sélectionnés.")
      )
      p
    })

    output$bivariate_plot <- renderPlot({
      current_biv_plot()
    })

    output$dl_biv_plot_png <- downloadHandler(
      filename = function() { paste0("Graphique_Bivariate_", Sys.Date(), ".png") },
      content = function(file) { ggplot2::ggsave(file, plot = current_biv_plot(), width = 9, height = 6, dpi = 300) }
    )

    output$dl_biv_plot_pdf <- downloadHandler(
      filename = function() { paste0("Graphique_Bivariate_", Sys.Date(), ".pdf") },
      content = function(file) { ggplot2::ggsave(file, plot = current_biv_plot(), width = 9, height = 6) }
    )
    
    # ⚡ EXPORT IMMÉDIAT HANDLERS
    output$dl_bivar_word <- downloadHandler(
      filename = function() { paste0("Tableau_Bivar_Complet_", input$target_var, ".docx") },
      content = function(file) {
        res_info <- bivariate_res()
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Analyse Bivariée - Variable Cible (Outcome) :", res_info$target), style = "heading 1")

        method <- res_info$method
        type <- res_info$type
        results <- res_info$results

        if (method == "group_comparison" && type == "list") {
          for (item in results) {
            pred_name <- item$pred
            val <- item$value
            anova_val <- item$anova
            doc <- officer::body_add_par(doc, paste("Comparaison :", pred_name, "vs", res_info$target), style = "heading 2")

            ft_val <- get_flextable(val)
            if (!is.null(ft_val)) {
              doc <- flextable::body_add_flextable(doc, ft_val)
            } else if (is.data.frame(val)) {
              ft <- flextable::theme_vanilla(flextable::flextable(val))
              doc <- flextable::body_add_flextable(doc, ft)
            }

            if (!is.null(anova_val)) {
              doc <- officer::body_add_par(doc, "Tableau d'ANOVA à un facteur", style = "heading 3")
              ft_anova <- get_flextable(anova_val$anova)
              if (!is.null(ft_anova)) doc <- flextable::body_add_flextable(doc, ft_anova)
              doc <- officer::body_add_par(doc, "Test post-hoc de Tukey (HSD)", style = "heading 3")
              ft_tukey <- get_flextable(anova_val$tukey)
              if (!is.null(ft_tukey)) doc <- flextable::body_add_flextable(doc, ft_tukey)
            }
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
        } else {
          ft_results <- get_flextable(results)
          if (!is.null(ft_results)) {
            doc <- flextable::body_add_flextable(doc, ft_results)
          } else if (is.data.frame(results)) {
            ft <- flextable::theme_vanilla(flextable::flextable(results))
            doc <- flextable::body_add_flextable(doc, ft)
          }
        }
        print(doc, target = file)
      }
    )
    
    output$dl_bivar_csv <- downloadHandler(
      filename = function() { paste0("Tableau_Bivarié_", input$target_var, ".csv") },
      content = function(file) {
        res_info <- bivariate_res()
        method <- res_info$method
        type <- res_info$type
        results <- res_info$results

        if (method == "group_comparison" && type == "list") {
          df_list <- lapply(results, function(item) {
            val <- item$value
            df <- get_dataframe(val)
            if (is.null(df)) df <- val

            if (!is.null(df) && nrow(df) > 0) {
              df$Predictor_Variable <- item$pred
            }
            df
          })
          combined <- dplyr::bind_rows(df_list)
          write.csv(combined, file, row.names = FALSE)
        } else {
          df_results <- get_dataframe(results)
          if (!is.null(df_results)) {
            write.csv(df_results, file, row.names = FALSE)
          } else if (is.data.frame(results)) {
            write.csv(results, file, row.names = FALSE)
          }
        }
      }
    )
    
    return(reactive({
      list(
        target = input$target_var,
        preds = input$pred_vars,
        res = bivariate_res(),
        biv_plot = tryCatch(current_biv_plot(), error = function(e) NULL)
      )
    }))
  })
}
