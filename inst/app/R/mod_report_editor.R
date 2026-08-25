#' Éditeur de Rapport Modulaire UI
#' @param id ID du module
#' @export
mod_report_editor_ui <- function(id) {
  ns <- shiny::NS(id)

  # CSS inline pour le rendu WYSIWYG
  wysiwyg_css <- shiny::tags$style(shiny::HTML(sprintf("
    #%s .block-heading-view {
      font-size: 1.6rem; font-weight: 700; color: #1e293b;
      border-left: 4px solid #0284c7; padding-left: 12px; margin: 0;
      font-family: 'Segoe UI', sans-serif;
    }
    #%s .block-text-view {
      font-size: 1rem; color: #374151; line-height: 1.7;
      white-space: pre-wrap; font-family: 'Georgia', serif;
    }
    #%s .editor-card { border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.08); }
    #%s .editor-card:hover { box-shadow: 0 2px 12px rgba(0,0,0,0.14); }
    #%s .block-header { background: #f8fafc; border-bottom: 1px solid #e2e8f0; padding: 8px 14px; }
    #%s .badge-heading { background:#dbeafe; color:#1d4ed8; }
    #%s .badge-text    { background:#f0fdf4; color:#166534; }
    #%s .badge-table   { background:#fef3c7; color:#92400e; }
    #%s .badge-plot    { background:#ede9fe; color:#5b21b6; }
    #%s .add-btn { text-align:left; }
  ",
  ns(""), ns(""), ns(""), ns(""), ns(""), ns(""), ns(""), ns(""), ns(""), ns("")
  )))

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      wysiwyg_css,
      shiny::tags$div(
        class = "d-grid gap-2",
        shiny::tags$p(class = "text-muted fw-bold mb-1", style = "font-size:.75rem; letter-spacing:.06em;", "STRUCTURE"),
        shiny::actionButton(ns("add_heading"), "Grand Titre (H1)",
          icon = shiny::icon("heading"), class = "btn-outline-primary add-btn w-100"),
        shiny::actionButton(ns("add_heading2"), "Sous-Titre (H2)",
          icon = shiny::icon("heading"), class = "btn-outline-primary add-btn w-100"),
        shiny::actionButton(ns("add_text"), "Paragraphe de Texte",
          icon = shiny::icon("align-left"), class = "btn-outline-secondary add-btn w-100"),
        shiny::tags$p(class = "text-muted fw-bold mb-1 mt-3", style = "font-size:.75rem; letter-spacing:.06em;", "ANALYSES"),
        shiny::actionButton(ns("btn_modal_uni"), "Analyse Univariée",
          icon = shiny::icon("chart-pie"), class = "btn-outline-info add-btn w-100"),
        shiny::actionButton(ns("btn_modal_bi"), "Analyse Bivariée",
          icon = shiny::icon("project-diagram"), class = "btn-outline-success add-btn w-100"),
        shiny::actionButton(ns("add_model"), "Insérer Modèle Actuel",
          icon = shiny::icon("cogs"), class = "btn-outline-warning add-btn w-100")
      ),
      shiny::tags$hr(),
      shiny::downloadButton(ns("export_word"), "Exporter en Word",
        class = "btn-primary w-100")
    ),

    # Zone principale — éditeur WYSIWYG
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::tags$span(
          shiny::icon("file-alt"), " Rapport en cours",
          shiny::tags$span(class = "ms-2 badge bg-secondary",
            shiny::textOutput(ns("block_count"), inline = TRUE))
        ),
        shiny::tags$small(class = "text-muted", "Cliquez sur le crayon pour éditer un bloc")
      ),
      bslib::card_body(
        style = "min-height: 500px; background: #fcfcfc;",
        # Vrai rendu des blocs
        shiny::uiOutput(ns("blocks_ui")),
        shiny::tags$div(
          id    = ns("empty_state"),
          class = "text-center text-muted py-5",
          shiny::tags$div(style = "font-size:3.5rem; opacity:0.15; margin-bottom:12px;",
            shiny::icon("file-alt")),
          shiny::tags$h5("Commencez à composer votre rapport"),
          shiny::tags$p("Utilisez le panneau de gauche pour ajouter des blocs.")
        )
      )
    )
  )
}

#' Éditeur de Rapport Modulaire Server
#' @export
mod_report_editor_server <- function(id, data_reactive = NULL, univar_reactive = NULL,
                                      bivar_reactive = NULL, model_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── État : liste de blocs + état d'édition ────────────────────────────────
    blocks_state   <- shiny::reactiveVal(list())
    editing_state  <- shiny::reactiveVal(list())   # id -> TRUE/FALSE
    block_counter  <- shiny::reactiveVal(0)

    output$block_count <- shiny::renderText({
      n <- length(blocks_state())
      paste0(n, " bloc", if (n > 1) "s" else "")
    })

    # ── Helpers ───────────────────────────────────────────────────────────────
    add_block <- function(type, obj = NULL, default_text = "", label = "") {
      count <- block_counter() + 1
      block_counter(count)
      b_id <- paste0("block_", count)
      new_block <- list(id = b_id, type = type, content_obj = obj,
                        text_val = default_text, label = label)
      current <- blocks_state()
      current[[length(current) + 1]] <- new_block
      blocks_state(current)
      # Nouveau bloc en mode vue par défaut (sauf texte/titre → édition directe)
      es <- editing_state()
      es[[b_id]] <- type %in% c("heading1", "heading2", "text")
      editing_state(es)
      shinyjs::hide("empty_state")
    }

    toggle_edit <- function(b_id) {
      es <- editing_state()
      es[[b_id]] <- !isTRUE(es[[b_id]])
      editing_state(es)
    }

    # ── Boutons structure ──────────────────────────────────────────────────────
    shiny::observeEvent(input$add_heading,  { add_block("heading1",  default_text = "Titre principal") })
    shiny::observeEvent(input$add_heading2, { add_block("heading2",  default_text = "Sous-titre") })
    shiny::observeEvent(input$add_text,     { add_block("text",      default_text = "") })

    # ── MODALE UNIVARIÉE ──────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_modal_uni, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        shiny::showNotification("Veuillez d'abord charger un jeu de données.", type = "error"); return()
      }

      shiny::showModal(shiny::modalDialog(
        title = shiny::tags$span(shiny::icon("chart-pie"), " Analyse Univariée"),
        size  = "l",
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("m_uni_var"), "Variable :", choices = names(df)),
            shiny::checkboxGroupInput(ns("m_uni_insert"), "Insérer :",
              choices  = c("Tableau descriptif" = "Tableau", "Graphique" = "Graphique"),
              selected = c("Tableau", "Graphique")),
            shiny::tags$hr(),
            shiny::tags$strong("Options Tableau"),
            shiny::numericInput(ns("m_uni_digits"), "Décimales :", 1, 0, 5),
            shiny::checkboxInput(ns("m_uni_show_valid"), "Afficher N valide", FALSE),
            shiny::checkboxInput(ns("m_uni_include_na"), "Inclure NA", FALSE),
            shiny::textInput(ns("m_uni_color"), "Couleur en-tête :", value = "#0284c7")
          ),
          shiny::column(8,
            shiny::tags$strong("Options Graphique"), shiny::tags$hr(),
            shiny::selectInput(ns("m_uni_plot_type"), "Type de graphique :",
              choices = c(
                "Détection automatique" = "auto",
                "Histogramme"           = "histogram",
                "Courbe de densité"     = "density",
                "Barres"                = "bar",
                "Boxplot"               = "boxplot",
                "Camembert (Pie)"       = "pie"
              )
            ),
            shiny::selectInput(ns("m_uni_plot_theme"), "Thème :",
              choices = c(
                "Minimal"      = "theme_minimal",
                "Classique"    = "theme_classic",
                "Gris"         = "theme_gray",
                "Noir & Blanc" = "theme_bw",
                "Clair"        = "theme_light"
              )
            ),
            shiny::selectInput(ns("m_uni_plot_palette"), "Palette :",
              choices = c("Défaut" = "default", "Viridis" = "viridis",
                          "Set1" = "Set1", "Set2" = "Set2", "Dark2" = "Dark2",
                          "Pastel1" = "Pastel1", "Spectral" = "Spectral")
            ),
            shiny::textInput(ns("m_uni_base_color"), "Couleur principale (Hex) :", value = "#2C6E9B"),
            shiny::numericInput(ns("m_uni_text_size"), "Taille police :", 11, 7, 18),
            shiny::checkboxInput(ns("m_uni_plot_horiz"), "Axes horizontaux", FALSE)
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("m_uni_confirm"), "Insérer dans le rapport", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$m_uni_confirm, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, input$m_uni_var)
      var_nm   <- input$m_uni_var
      var_sym  <- rlang::sym(var_nm)
      col_data <- df[[var_nm]]
      n_unique <- length(unique(na.omit(col_data)))
      type <- if (is.numeric(col_data)) {
        if (n_unique <= 2) "binary" else "numeric"
      } else if (n_unique <= 2) "binary" else "categorical"

      shiny::removeModal()
      shiny::withProgress(message = "Génération de l'analyse...", value = 0.3, {

        # Tableau
        if ("Tableau" %in% input$m_uni_insert) {
          res_tab <- tryCatch({
            if (type == "numeric")
              analytix::descr_numeric(df, !!var_sym, digits = input$m_uni_digits,
                show_valid = isTRUE(input$m_uni_show_valid), color = input$m_uni_color)
            else if (type == "binary")
              analytix::descr_binary(df, !!var_sym, digits = input$m_uni_digits, color = input$m_uni_color)
            else
              analytix::descr_categorial(df, !!var_sym, digits = input$m_uni_digits,
                include_na = isTRUE(input$m_uni_include_na), color = input$m_uni_color)
          }, error = function(e) { shiny::showNotification(paste("Tableau:", e$message), type = "warning"); NULL })
          if (!is.null(res_tab) && !is.null(res_tab$flextable))
            add_block("flextable", obj = res_tab$flextable, label = paste("Tableau —", var_nm))
        }

        shiny::setProgress(0.65)

        # Graphique
        if ("Graphique" %in% input$m_uni_insert) {
          plt <- tryCatch({
            plot_type <- input$m_uni_plot_type
            p <- if (plot_type == "pie" || (plot_type == "auto" && !is.numeric(col_data))) {
              if (plot_type == "pie") {
                analytix::plot_pie_chart(df, !!var_sym)
              } else {
                analytix::plot_barplot(df, !!var_sym,
                  col   = input$m_uni_base_color,
                  horiz = isTRUE(input$m_uni_plot_horiz))
              }
            } else if (type == "numeric") {
              analytix::plot_distribution(df, !!var_sym,
                type = if (plot_type == "auto") "auto" else plot_type,
                fill = input$m_uni_base_color)
            } else {
              analytix::plot_barplot(df, !!var_sym,
                col   = input$m_uni_base_color,
                horiz = isTRUE(input$m_uni_plot_horiz))
            }
            if (!is.null(p) && plot_type != "pie") {
              p <- analytix::apply_custom_theme(p,
                theme_name   = input$m_uni_plot_theme,
                palette_name = input$m_uni_plot_palette,
                base_size    = input$m_uni_text_size)
              if (isTRUE(input$m_uni_plot_horiz) && type != "numeric")
                p <- p + ggplot2::coord_flip()
            }
            p
          }, error = function(e) { shiny::showNotification(paste("Graphique:", e$message), type = "warning"); NULL })
          if (!is.null(plt))
            add_block("plot", obj = plt, label = paste("Graphique —", var_nm))
        }
        shiny::showNotification("Analyse Univariée insérée !", type = "message")
      })
    })

    # ── MODALE BIVARIÉE ───────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_modal_bi, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        shiny::showNotification("Veuillez d'abord charger un jeu de données.", type = "error"); return()
      }
      shiny::showModal(shiny::modalDialog(
        title = shiny::tags$span(shiny::icon("project-diagram"), " Analyse Bivariée"),
        size  = "l",
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("m_bi_y"), "Variable Expliquée (Y — Outcome) :", choices = names(df)),
            shiny::selectInput(ns("m_bi_x"), "Variable Explicative (X) :", choices = names(df)),
            shiny::checkboxGroupInput(ns("m_bi_insert"), "Insérer :",
              choices  = c("Tableau" = "Tableau", "Graphique" = "Graphique"),
              selected = c("Tableau", "Graphique")),
            shiny::tags$hr(),
            shiny::tags$strong("Options Tableau"),
            shiny::numericInput(ns("m_bi_digits"), "Décimales :", 2, 0, 5),
            shiny::textInput(ns("m_bi_color"), "Couleur en-tête :", value = "#059669")
          ),
          shiny::column(8,
            shiny::tags$strong("Options Graphique"), shiny::tags$hr(),
            shiny::selectInput(ns("m_bi_plot_theme"), "Thème :",
              choices = c("Minimal" = "theme_minimal", "Classique" = "theme_classic",
                          "Gris" = "theme_gray", "Noir & Blanc" = "theme_bw", "Clair" = "theme_light")
            ),
            shiny::selectInput(ns("m_bi_plot_palette"), "Palette :",
              choices = c("Défaut" = "default", "Viridis" = "viridis",
                          "Set1" = "Set1", "Set2" = "Set2", "Dark2" = "Dark2",
                          "Pastel1" = "Pastel1", "Spectral" = "Spectral")
            ),
            shiny::textInput(ns("m_bi_base_color"), "Couleur principale :", value = "#059669"),
            shiny::numericInput(ns("m_bi_text_size"), "Taille police :", 11, 7, 18),
            shiny::checkboxInput(ns("m_bi_plot_horiz"), "Axes horizontaux", FALSE),
            shiny::checkboxInput(ns("m_bi_show_pct"), "Afficher les % (barres)", TRUE)
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("m_bi_confirm"), "Insérer dans le rapport", class = "btn-success")
        )
      ))
    })

    shiny::observeEvent(input$m_bi_confirm, {
      df <- tryCatch(data_reactive(), error = function(e) NULL)
      req(df, input$m_bi_y, input$m_bi_x)
      if (input$m_bi_y == input$m_bi_x) {
        shiny::showNotification("X et Y doivent être différents.", type = "warning"); return()
      }
      y_nm <- input$m_bi_y; x_nm <- input$m_bi_x
      x_sym <- rlang::sym(x_nm); y_sym <- rlang::sym(y_nm)
      shiny::removeModal()
      shiny::withProgress(message = "Génération de l'analyse bivariée...", value = 0.4, {
        if ("Tableau" %in% input$m_bi_insert) {
          res_tab <- tryCatch(
            analytix::bivariate_or_table(df, outcome = y_nm, exposures = x_nm, digits = input$m_bi_digits),
            error = function(e) { shiny::showNotification(paste("Tableau bivarié:", e$message), type = "warning"); NULL })
          if (!is.null(res_tab) && !is.null(res_tab$results))
            add_block("flextable", obj = res_tab$results, label = paste("Tableau bivarié —", y_nm, "~", x_nm))
        }
        shiny::setProgress(0.7)
        if ("Graphique" %in% input$m_bi_insert) {
          plt <- tryCatch({
            x_col <- df[[x_nm]]
            p <- if (is.numeric(x_col)) analytix::plot_boxplot(df, !!x_sym, !!y_sym)
                 else analytix::plot_grouped_bar(df, !!y_sym, !!x_sym, show_pct = isTRUE(input$m_bi_show_pct))
            if (!is.null(p)) {
              p <- analytix::apply_custom_theme(p,
                theme_name   = input$m_bi_plot_theme,
                palette_name = input$m_bi_plot_palette,
                base_size    = input$m_bi_text_size)
              if (isTRUE(input$m_bi_plot_horiz)) p <- p + ggplot2::coord_flip()
            }
            p
          }, error = function(e) { shiny::showNotification(paste("Graphique bivarié:", e$message), type = "warning"); NULL })
          if (!is.null(plt))
            add_block("plot", obj = plt, label = paste("Graphique bivarié —", y_nm, "~", x_nm))
        }
        shiny::showNotification("Analyse Bivariée insérée !", type = "message")
      })
    })

    # ── Modèle actuel ─────────────────────────────────────────────────────────
    shiny::observeEvent(input$add_model, {
      res <- tryCatch(if (!is.null(model_reactive)) model_reactive() else NULL, error = function(e) NULL)
      if (!is.null(res) && !is.null(res$ft)) {
        add_block("flextable", obj = res$ft, label = "Modèle de Régression")
        if (!is.null(res$interp)) add_block("text", default_text = res$interp)
        shiny::showNotification("Modèle ajouté !", type = "message")
      } else {
        shiny::showNotification("Aucun modèle multivarié disponible.", type = "warning")
      }
    })

    # ── Toggle mode édition / vue ─────────────────────────────────────────────
    shiny::observe({
      blocks <- blocks_state()
      lapply(blocks, function(blk) {
        shiny::observeEvent(input[[paste0("edit_toggle_", blk$id)]], {
          toggle_edit(blk$id)
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("del_", blk$id)]], {
          current <- blocks_state()
          current <- Filter(function(x) x$id != blk$id, current)
          blocks_state(current)
          es <- editing_state()
          es[[blk$id]] <- NULL
          editing_state(es)
          if (length(current) == 0) shinyjs::show("empty_state")
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # ── Rendu WYSIWYG des blocs ───────────────────────────────────────────────
    output$blocks_ui <- shiny::renderUI({
      blocks  <- blocks_state()
      editing <- editing_state()
      if (length(blocks) == 0) return(NULL)

      ui_list <- lapply(seq_along(blocks), function(i) {
        blk      <- blocks[[i]]
        is_edit  <- isTRUE(editing[[blk$id]])

        # Boutons actions
        edit_icon <- if (blk$type %in% c("heading1", "heading2", "text")) {
          shiny::actionButton(ns(paste0("edit_toggle_", blk$id)),
            label = NULL,
            icon  = if (is_edit) shiny::icon("eye") else shiny::icon("pencil-alt"),
            class = if (is_edit) "btn-sm btn-outline-secondary me-1" else "btn-sm btn-outline-primary me-1"
          )
        }
        del_btn <- shiny::actionButton(ns(paste0("del_", blk$id)),
          label = NULL, icon = shiny::icon("trash"), class = "btn-sm btn-outline-danger")

        # Badge + label
        badge_info <- switch(blk$type,
          "heading1"  = list(cls = "badge-heading", txt = "H1"),
          "heading2"  = list(cls = "badge-heading", txt = "H2"),
          "text"      = list(cls = "badge-text",    txt = "§"),
          "flextable" = list(cls = "badge-table",   txt = "Tableau"),
          "plot"      = list(cls = "badge-plot",    txt = "Graphique"),
          list(cls = "bg-secondary text-white", txt = "?")
        )
        lbl <- if (nchar(trimws(blk$label)) > 0) blk$label else badge_info$txt

        header <- shiny::tags$div(
          class = "block-header d-flex justify-content-between align-items-center",
          shiny::tags$span(
            shiny::tags$span(class = paste("badge me-2", badge_info$cls), badge_info$txt),
            shiny::tags$span(class = "text-muted", style = "font-size:.85rem;", lbl)
          ),
          shiny::tags$span(edit_icon, del_btn)
        )

        # ── Contenu selon type + mode ─────────────────────────────────────────
        body_content <- if (blk$type == "heading1") {
          if (is_edit) {
            shiny::textInput(ns(paste0("text_", blk$id)),
              label = NULL, value = blk$text_val, width = "100%",
              placeholder = "Titre principal...")
          } else {
            current_val <- shiny::isolate(input[[paste0("text_", blk$id)]])
            display_val <- if (!is.null(current_val) && nchar(trimws(current_val)) > 0) current_val else blk$text_val
            shiny::tags$p(class = "block-heading-view my-1", display_val)
          }
        } else if (blk$type == "heading2") {
          if (is_edit) {
            shiny::textInput(ns(paste0("text_", blk$id)),
              label = NULL, value = blk$text_val, width = "100%",
              placeholder = "Sous-titre...")
          } else {
            current_val <- shiny::isolate(input[[paste0("text_", blk$id)]])
            display_val <- if (!is.null(current_val) && nchar(trimws(current_val)) > 0) current_val else blk$text_val
            shiny::tags$p(style = "font-size:1.2rem; font-weight:600; color:#374151; border-left:3px solid #94a3b8; padding-left:10px; margin:0;",
              display_val)
          }
        } else if (blk$type == "text") {
          if (is_edit) {
            shiny::textAreaInput(ns(paste0("text_", blk$id)),
              label = NULL, value = blk$text_val, width = "100%", rows = 5,
              placeholder = "Rédigez votre texte ici...")
          } else {
            current_val <- shiny::isolate(input[[paste0("text_", blk$id)]])
            display_val <- if (!is.null(current_val) && nchar(trimws(current_val)) > 0) current_val else blk$text_val
            shiny::tags$p(class = "block-text-view my-1",
              if (nchar(trimws(display_val)) > 0) display_val
              else shiny::tags$em(class = "text-muted", "Cliquez sur le crayon pour rédiger..."))
          }
        } else if (blk$type == "flextable") {
          if (!is.null(blk$content_obj) && inherits(blk$content_obj, "flextable")) {
            tryCatch(
              shiny::tags$div(style = "overflow-x:auto;", flextable::htmltools_value(blk$content_obj)),
              error = function(e) shiny::tags$em(class = "text-muted", "Aperçu non disponible")
            )
          } else {
            shiny::tags$em(class = "text-muted", "Tableau inséré")
          }
        } else if (blk$type == "plot") {
          plot_out_id <- paste0("plot_preview_", blk$id)
          local({
            local_blk <- blk
            output[[paste0("plot_preview_", local_blk$id)]] <- shiny::renderPlot({
              req(!is.null(local_blk$content_obj))
              print(local_blk$content_obj)
            }, height = 280, bg = "white")
          })
          shiny::plotOutput(ns(plot_out_id), height = "280px")
        }

        # Padding selon le type
        body_class <- switch(blk$type,
          "heading1"  = "card-body px-3 py-2",
          "heading2"  = "card-body px-3 py-2",
          "text"      = "card-body px-3 py-2",
          "flextable" = "card-body p-2",
          "plot"      = "card-body p-1",
          "card-body p-2"
        )

        shiny::tags$div(
          id    = ns(paste0("container_", blk$id)),
          class = "card mb-3 editor-card",
          header,
          shiny::tags$div(class = body_class, body_content)
        )
      })

      shiny::tagList(ui_list)
    })

    # ── Export Word ───────────────────────────────────────────────────────────
    output$export_word <- shiny::downloadHandler(
      filename = function() paste0("rapport_analytix_", format(Sys.time(), "%Y%m%d_%H%M"), ".docx"),
      content = function(file) {
        blocks <- blocks_state()
        if (length(blocks) == 0) {
          shiny::showNotification("Le rapport est vide.", type = "error"); return(NULL)
        }
        shiny::withProgress(message = "Génération du document Word...", value = 0.2, {
          export_blocks <- lapply(blocks, function(blk) {
            if (blk$type %in% c("heading1", "heading2", "text")) {
              val <- input[[paste0("text_", blk$id)]]
              list(type = blk$type, content = if (is.null(val)) "" else val)
            } else {
              list(type = blk$type, content = blk$content_obj)
            }
          })
          shiny::setProgress(0.6)
          tryCatch(
            analytix::compile_custom_report(export_blocks, output = file),
            error = function(e) shiny::showNotification(paste("Erreur export:", e$message), type = "error")
          )
        })
      }
    )

  })
}
