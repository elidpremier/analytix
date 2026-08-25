#' @title Lancer l'interface graphique Analytix GUI
#' @description
#' Ouvre l'application Shiny Analytix GUI directement depuis le package,
#' sans nécessiter de dépôt séparé. Peut être lancée en un clic via
#' **Addins → Lancer Analytix GUI** dans RStudio ou Positron.
#'
#' @param launch.browser Logique. Si \code{TRUE} (défaut), ouvre l'app dans
#'   le navigateur système. Si \code{FALSE}, ouvre dans le viewer RStudio.
#' @param port Entier optionnel. Port à utiliser (ex: \code{3838}).
#'   Si \code{NULL} (défaut), un port libre est choisi automatiquement.
#'
#' @examples
#' \dontrun{
#' # Lancer l'interface graphique
#' run_gui()
#'
#' # Forcer l'ouverture dans le viewer RStudio (au lieu du navigateur)
#' run_gui(launch.browser = FALSE)
#' }
#'
#' @export
run_gui <- function(launch.browser = TRUE, port = NULL) {

  app_dir <- system.file("app", package = "analytix")

  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "L'application Analytix GUI est introuvable dans le package.\n",
      "Essayez de réinstaller analytix :\n",
      "  remotes::install_github(\"elidpremier/analytix\", build = FALSE)",
      call. = FALSE
    )
  }

  # Vérifier les dépendances essentielles
  deps_manquantes <- character(0)
  for (pkg in c("shiny", "bslib", "DT", "flextable", "officer", "readxl")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      deps_manquantes <- c(deps_manquantes, pkg)
    }
  }

  if (length(deps_manquantes) > 0) {
    stop(
      "Les packages suivants sont requis et manquants :\n",
      "  ", paste(deps_manquantes, collapse = ", "), "\n",
      "Installez-les avec :\n",
      "  install.packages(c(\"", paste(deps_manquantes, collapse = "\", \""), "\"))",
      call. = FALSE
    )
  }

  # Avertissement optionnel pour shinyjs / bsicons
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    message("Info : Le package 'shinyjs' n'est pas installé. Certaines animations de l'interface seront désactivées.")
    message("       Installez-le avec : install.packages(\"shinyjs\")")
  }

  args <- list(appDir = app_dir, launch.browser = launch.browser)
  if (!is.null(port)) args$port <- port

  do.call(shiny::runApp, args)
}
