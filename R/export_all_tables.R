#' @title Export d'une liste de tableaux vers un document Word structuré
#' @description Exporte une liste nommée d'objets `flextable` ou de résultats d'analyse `analytix_table`
#' dans un document Word unique, avec chaque tableau précédé d'un titre de section.
#' Gère l'orientation paysage automatique pour les très grands tableaux et le nettoyage optionnel
#' des numérotations manuelle de titres.
#'
#' @param tables Liste nommée d'objets `flextable` ou `analytix_table`. Les noms servent de titres de section.
#' @param file Chemin du fichier Word de sortie (défaut: "rapport_tableaux.docx").
#' @param title Titre principal du document Word (défaut: "Rapport d'analyse").
#' @param subtitle Sous-titre du document (défaut: NULL).
#' @param author Auteur du document (défaut: NULL).
#' @param date Chaîne de date pour la page de garde (défaut: date du jour).
#' @param section_style Style Word des titres de sections (défaut: "heading 2").
#' @param auto_landscape Logique. Si TRUE (défaut), bascule automatiquement les tableaux >= 7 colonnes en orientation paysage.
#' @param strip_manual_numbering Logique. Si TRUE, retire les préfixes numériques manuels (ex: "8.7 ") pour éviter les doubles numérotations Word.
#'
#' @return Chemin vers le fichier Word créé (invisible).
#'
#' @examples
#' \dontrun{
#'   t1 <- desc_numeric(mtcars, mpg)
#'   t2 <- desc_categorical(iris, Species)
#'   export_tables(
#'     tables = list("1.1 Description de MPG" = t1, "1.2 Description de Species" = t2),
#'     file   = "mon_rapport.docx",
#'     title  = "Rapport de tests",
#'     strip_manual_numbering = TRUE
#'   )
#' }
#'
#' @export
export_tables <- function(tables, file = "rapport_tableaux.docx",
                               title = "Rapport d'analyse",
                               subtitle = NULL, author = NULL,
                               date = format(Sys.Date(), "%d %B %Y"),
                               section_style = "heading 2",
                               auto_landscape = TRUE,
                               strip_manual_numbering = FALSE) {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("officer", quietly = TRUE))   stop("officer requis")

  if (!is.list(tables) || length(tables) == 0) {
    stop("`tables` doit être une liste non vide d'objets flextable ou analytix_table.")
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

  # Fonction d'extraction récursive
  extract_fts <- function(item) {
    if (inherits(item, "flextable")) {
      return(list(item))
    }
    if (is.list(item)) {
      if ("flextable" %in% names(item) && inherits(item$flextable, "flextable")) {
        return(list(item$flextable))
      }
      res <- list()
      for (sub in item) {
        sub_extracted <- extract_fts(sub)
        if (length(sub_extracted) > 0) {
          res <- c(res, sub_extracted)
        }
      }
      return(res)
    }
    return(list())
  }

  # Ajout des tableaux
  section_names <- names(tables)
  if (is.null(section_names)) {
    section_names <- paste0("Tableau ", seq_along(tables))
  }

  count_rendered <- 0

  for (i in seq_along(tables)) {
    nm  <- if (nchar(section_names[i]) > 0) section_names[i] else paste0("Tableau ", i)
    tbl <- tables[[i]]

    fts <- extract_fts(tbl)

    if (length(fts) == 0) {
      warning(paste0("L'objet '", nm, "' n'est pas ou ne contient pas un flextable valide et a été ignoré."))
      next
    }

    # Nettoyage optionnel de la numérotation manuelle (ex: "8.7 " -> "")
    display_title <- nm
    if (isTRUE(strip_manual_numbering)) {
      display_title <- sub("^[0-9]+(\\.[0-9]+)*\\s*[-–:]?\\s*", "", nm)
    }

    # Détection de grands tableaux nécessitant le mode paysage
    max_cols <- max(vapply(fts, function(f) length(flextable::ncol_keys(f)), integer(1)))
    is_wide  <- isTRUE(auto_landscape) && (max_cols >= 7)

    if (is_wide) {
      doc <- officer::body_end_section_portrait(doc)
    }

    doc <- officer::body_add_par(doc, display_title, style = section_style)
    for (ft in fts) {
      doc <- flextable::body_add_flextable(doc, ft)
      doc <- officer::body_add_par(doc, "", style = "Normal")
      count_rendered <- count_rendered + 1
    }

    if (is_wide) {
      doc <- officer::body_end_section_landscape(doc)
    }
  }

  print(doc, target = file)
  invisible(file)
}
