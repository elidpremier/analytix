#' @title Export d'une liste de tableaux vers un document Word structuré
#' @description Exporte une liste nommée d'objets `flextable` dans un document
#' Word unique, avec chaque tableau précédé d'un titre de section. Idéal pour
#' générer des rapports complets en une seule commande.
#'
#' @param tables Liste nommée d'objets `flextable`. Les noms servent de titres de section.
#' @param file Chemin du fichier Word de sortie (défaut: "rapport_tableaux.docx").
#' @param title Titre principal du document Word (défaut: "Rapport d'analyse").
#' @param subtitle Sous-titre du document (défaut: NULL).
#' @param author Auteur du document (défaut: NULL).
#' @param date Chaîne de date pour la page de garde (défaut: date du jour).
#' @param section_style Style Word des titres de sections (défaut: "heading 2").
#'
#' @return Chemin vers le fichier Word créé (invisible).
#'
#' @examples
#' \dontrun{
#'   library(flextable)
#'   t1 <- as_flextable(head(iris))
#'   t2 <- as_flextable(head(mtcars))
#'   export_all_tables(
#'     tables = list("Description de l'iris" = t1, "Description des voitures" = t2),
#'     file   = "mon_rapport.docx",
#'     title  = "Rapport de tests"
#'   )
#' }
#'
#' @export
export_all_tables <- function(tables, file = "rapport_tableaux.docx",
                               title = "Rapport d'analyse",
                               subtitle = NULL, author = NULL,
                               date = format(Sys.Date(), "%d %B %Y"),
                               section_style = "heading 2") {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("officer", quietly = TRUE))   stop("officer requis")

  if (!is.list(tables) || length(tables) == 0) {
    stop("`tables` doit être une liste non vide d'objets flextable.")
  }

  doc <- officer::read_docx()

  # Page de garde
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(subtitle)) {
    doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  }
  if (!is.null(author)) {
    doc <- officer::body_add_par(doc, paste0("Auteur : ", author), style = "Normal")
  }
  doc <- officer::body_add_par(doc, paste0("Date : ", date), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # Ajout des tableaux
  section_names <- names(tables)
  if (is.null(section_names)) {
    section_names <- paste0("Tableau ", seq_along(tables))
  }

  for (i in seq_along(tables)) {
    nm  <- if (nchar(section_names[i]) > 0) section_names[i] else paste0("Tableau ", i)
    tbl <- tables[[i]]

    doc <- officer::body_add_par(doc, nm, style = section_style)

    if (inherits(tbl, "flextable")) {
      doc <- flextable::body_add_flextable(doc, tbl)
    } else if (inherits(tbl, c("list"))) {
      # Si c'est une liste (ex: output de anova_table)
      for (sub_tbl in tbl) {
        if (inherits(sub_tbl, "flextable")) {
          doc <- flextable::body_add_flextable(doc, sub_tbl)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }
    } else {
      warning(paste0("L'objet '", nm, "' n'est pas un flextable valide et a été ignoré."))
    }

    # Saut de ligne entre les sections
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }

  print(doc, target = file)
  invisible(file)
}
