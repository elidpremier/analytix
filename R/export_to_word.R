#' @title Exporter des tableaux analytiques vers Word
#' @description Exporte des tableaux provenant de l'environnement, de listes, ou d'objets individuels.
#'
#' @param ...
#'   - Aucun argument : exporte tous les tableaux de l'environnement global.
#'   - Un ou plusieurs objets : \code{export_to_word(tab1, tab2)}.
#'   - Une liste : \code{export_to_word(resultats)}.
#'   - Mélange : \code{export_to_word(tab1, resultats)}.
#' @param env Si \code{TRUE}, inclut aussi les tableaux de l'environnement même si des objets sont passés via \code{...}.
#'            Si omis, inclut l'environnement uniquement si aucun argument n'est fourni via \code{...}.
#' @param path Chemin du fichier Word de sortie (défaut: "rapport.docx").
#' @param add_page_breaks Ajouter des sauts de page entre les tableaux ? (défaut: TRUE)
#'
#' @return NULL (crée un fichier Word)
#'
#' @examples
#' \dontrun{
#' # Cas 1 : tout depuis l'environnement
#' tab1 <- freq_table(iris, Species)
#' tab2 <- descr_numeric(iris, Sepal.Length)
#' export_to_word(path = "rapport1.docx")
#'
#' # Cas 2 : depuis une liste
#' resultats <- analyse_descriptive_multiple(iris, c("Species", "Sepal.Length"))
#' export_to_word(resultats, path = "rapport2.docx")
#'
#' # Cas 3 : environnement + liste
#' export_to_word(resultats, env = TRUE, path = "rapport3.docx")
#' }
#'
#' @export
export_to_word <- function(..., env = NULL, path = "rapport.docx", add_page_breaks = TRUE) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' requis pour l'export Word.")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' requis pour l'export Word.")
  }

  dots <- list(...)
  has_dots <- length(dots) > 0

  # Déterminer si on inclut l'environnement
  if (is.null(env)) {
    include_env <- !has_dots  # Si pas d'arguments ..., alors on scanne l'env
  } else {
    include_env <- isTRUE(env)
  }

  all_tables <- list()

  # 1. Récupérer les tableaux de l'environnement (si demandé)
  if (include_env) {
    env_tables <- .get_tables_from_env()
    all_tables <- c(all_tables, env_tables)
  }

  # 2. Récupérer les tableaux passés via ...
  if (has_dots) {
    arg_tables <- .flatten_and_extract_tables(dots)
    all_tables <- c(all_tables, arg_tables)
  }

  if (length(all_tables) == 0) {
    stop("Aucun tableau analytique exportable trouvé.")
  }

  # 3. Exporter le tout
  .export_table_list(all_tables, path, add_page_breaks)
}



# ==============================================================================
# Fonctions internes (non exportées)
# ==============================================================================

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

.flatten_and_extract_tables <- function(args_list) {
  is_exportable <- function(x) {
    base::inherits(x, "flextable") ||
      (base::is.list(x) && "flextable" %in% base::names(x) && base::inherits(x[["flextable"]], "flextable"))
  }

  tables <- base::list()
  arg_counter <- 1L

  for (arg in args_list) {
    if (base::is.list(arg)) {
      # Si c'est une liste (ex: sortie de analyse_descriptive_multiple)
      for (nm in base::names(arg)) {
        if (nm != "" && is_exportable(arg[[nm]])) {
          tables[[nm]] <- arg[[nm]]
        }
      }
    } else if (is_exportable(arg)) {
      # Si c'est un tableau individuel (freq_table, descr_numeric, ou flextable)
      # Génère un nom temporaire s'il n'en a pas
      tables[[paste0("objet_", arg_counter)]] <- arg
      arg_counter <- arg_counter + 1L
    }
  }
  return(tables)
}

.export_table_list <- function(table_list, path, add_page_breaks) {
  doc <- officer::read_docx()
  noms <- base::names(table_list)

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
    doc <- flextable:::body_add_flextable(doc, ft)

    if (add_page_breaks && i < base::length(table_list)) {
      doc <- officer::body_add_break(doc)
    }
  }

  base::print(doc, target = path)
  base::cat("✅ Exporté", base::length(table_list), "tableau(x) vers :", path, "\n")
}
