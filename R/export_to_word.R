#' @title Exporter des tableaux analytiques vers Word
#' @description Exporte des tableaux provenant de l'environnement, de listes, ou d'objets individuels.
#'
#' @param path Chemin du fichier Word de sortie (défaut: "rapport.docx").
#' @param ...
#'   - Un ou plusieurs objets : \code{export_to_word(tab1, tab2)}.
#'   - Une liste : \code{export_to_word(resultats)}.
#'   - Mélange : \code{export_to_word(tab1, resultats)}.
#' @param env Si \code{TRUE}, inclut aussi les tableaux de l'environnement global.
#' @param add_page_breaks Ajouter des sauts de page entre les tableaux ? (défaut: TRUE)
#'
#' @return NULL (crée un fichier Word)
#'
#' @export
export_to_word <- function(path = "rapport.docx", ..., env = FALSE, add_page_breaks = TRUE) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' requis pour l'export Word.")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' requis pour l'export Word.")
  }

  dots <- list(...)
  has_dots <- length(dots) > 0

  all_tables <- list()

  # 1. Récupérer les tableaux passés via ...
  if (has_dots) {
    all_tables <- .extract_tables_recursive(dots)
  }

  # 2. Récupérer les tableaux de l'environnement (si demandé ou si rien n'est passé)
  if (env || (!has_dots)) {
    env_tables <- .get_tables_from_env()
    all_tables <- c(all_tables, env_tables)
  }

  if (length(all_tables) == 0) {
    stop("Aucun tableau analytique exportable trouvé.")
  }

  # 3. Exporter le tout
  .export_table_list(all_tables, path, add_page_breaks)
}

# Extraction récursive pour gérer les listes de listes (ex: analyse_descriptive_multiple)
.extract_tables_recursive <- function(x) {
  tables <- list()
  
  is_exportable <- function(obj) {
    inherits(obj, "flextable") || 
    (is.list(obj) && "flextable" %in% names(obj) && inherits(obj[["flextable"]], "flextable"))
  }

  if (is_exportable(x)) {
    return(list(x))
  }

  if (is.list(x)) {
    for (i in seq_along(x)) {
      item <- x[[i]]
      nm <- names(x)[i]
      if (is.null(nm) || nm == "") nm <- paste0("tableau_", i)
      
      if (is_exportable(item)) {
        tables[[nm]] <- item
      } else if (is.list(item)) {
        sub_tables <- .extract_tables_recursive(item)
        if (length(sub_tables) > 0) {
          # On aplatit en préfixant les noms
          names(sub_tables) <- paste0(nm, "_", names(sub_tables))
          tables <- c(tables, sub_tables)
        }
      }
    }
  }
  
  return(tables)
}

.get_tables_from_env <- function(env = base::globalenv()) {
  is_exportable <- function(x) {
    base::inherits(x, "flextable") ||
      (base::is.list(x) && "flextable" %in% base::names(x) && base::inherits(x[["flextable"]], "flextable"))
  }

  obj_names <- base::ls(envir = env)
  tables <- base::list()

  for (nm in obj_names) {
    obj <- base::get(nm, envir = env)
    if (is_exportable(obj)) {
      tables[[nm]] <- obj
    }
  }
  return(tables)
}

.export_table_list <- function(table_list, path, add_page_breaks) {
  doc <- officer::read_docx()
  noms <- base::names(table_list)
  if (is.null(noms)) noms <- paste0("Tableau_", seq_along(table_list))

  for (i in base::seq_along(table_list)) {
    nm <- noms[i]
    obj <- table_list[[i]]

    ft <- if (base::inherits(obj, "flextable")) {
      obj
    } else {
      obj[["flextable"]]
    }

    doc <- officer::body_add_par(
      doc,
      value = base::paste("Tableau :", nm),
      style = "heading 2"
    )
    doc <- flextable::body_add_flextable(doc, ft)

    if (add_page_breaks && i < base::length(table_list)) {
      doc <- officer::body_add_break(doc)
    }
  }

  base::print(doc, target = path)
  base::cat("✅ Exporté", base::length(table_list), "tableau(x) vers :", path, "\n")
}
