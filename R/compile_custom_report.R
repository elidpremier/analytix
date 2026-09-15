#' @title Compiler un rapport modulaire personnalisé
#' @description Génère un document Word (.docx) à partir d'une liste de blocs.
#' @param blocks Une liste de listes. Chaque sous-liste doit avoir :
#'   \describe{
#'     \item{$type}{Un des types suivants : "heading1", "heading2", "text", "flextable", "plot"}
#'     \item{$content}{Le contenu : chaîne de caractères pour heading/text, objet flextable, ou objet ggplot}
#'   }
#' @param output Chemin vers le fichier de sortie (.docx).
#' @param template_path Chemin optionnel vers un template Word (.docx).
#' @return Le chemin du fichier généré (invisible).
#' @importFrom officer read_docx body_add_par body_add_img
#' @importFrom flextable body_add_flextable
#' @export
report_compile <- function(blocks,
                                   output        = "rapport_personnalise.docx",
                                   template_path = NULL) {

  if (!requireNamespace("officer",   quietly = TRUE)) stop("Le package 'officer' est requis.")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Le package 'flextable' est requis.")

  # Chargement du document de base (template ou vierge)
  doc <- if (!is.null(template_path) && file.exists(template_path)) {
    tryCatch(officer::read_docx(template_path), error = function(e) officer::read_docx())
  } else {
    tmpl <- system.file("templates", "strobe.docx", package = "analytix")
    if (nchar(tmpl) > 0) {
      tryCatch(officer::read_docx(tmpl), error = function(e) officer::read_docx())
    } else {
      officer::read_docx()
    }
  }

  for (i in seq_along(blocks)) {
    blk <- blocks[[i]]
    if (is.null(blk$type) || is.null(blk$content)) next

    tryCatch({
      if (blk$type == "heading1") {
        doc <- officer::body_add_par(doc, as.character(blk$content), style = "heading 1")
        doc <- officer::body_add_par(doc, "", style = "Normal")

      } else if (blk$type == "heading2") {
        doc <- officer::body_add_par(doc, as.character(blk$content), style = "heading 2")
        doc <- officer::body_add_par(doc, "", style = "Normal")

      } else if (blk$type == "text") {
        # Respect des retours à la ligne (paragraphes multiples)
        pars <- strsplit(as.character(blk$content), "\n")[[1]]
        for (p in pars) {
          if (trimws(p) != "") {
            doc <- officer::body_add_par(doc, p, style = "Normal")
          }
        }
        doc <- officer::body_add_par(doc, "", style = "Normal")

      } else if (blk$type == "flextable") {
        if (inherits(blk$content, "flextable")) {
          # Ajustement automatique à la largeur de la page
          ft <- flextable::fit_to_width(blk$content, max_width = 6.3)
          doc <- flextable::body_add_flextable(doc, ft)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }

      } else if (blk$type == "plot") {
        if (inherits(blk$content, "ggplot")) {
          tmp <- tempfile(fileext = ".png")
          ggplot2::ggsave(tmp, plot = blk$content,
                          width = 8, height = 5.5, dpi = 300,
                          bg = "white")
          doc <- officer::body_add_img(doc, src = tmp, width = 6.3, height = 4.3)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }
    }, error = function(e) {
      warning(sprintf("compile_custom_report: bloc %d (%s) ignoré — %s", i, blk$type, e$message))
    })
  }

  print(doc, target = output)
  invisible(output)
}
