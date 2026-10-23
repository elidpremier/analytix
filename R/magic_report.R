#' @title Le Rapport "Magique" Express en 1-Clic
#' @description Analyse automatiquement un jeu de données, détecte la variable d'intérêt si non spécifiée,
#' applique l'analyse univariée et bivariée et génère un rapport Word complet incluant des
#' synthèses rédigées par IA (ou mode heuristique) en français.
#'
#' Cette fonction est désormais un alias optimisé vers \code{\link{report_express}}.
#'
#' @param data data.frame ou chemin vers un fichier Excel/CSV.
#' @param output Chemin du fichier Word de sortie (défaut: "magic_report.docx").
#' @param title Titre du rapport (défaut: "Rapport Magique Analytix").
#' @param outcome Nom de la variable d'intérêt. Si NULL, la fonction tente de la deviner.
#' @param open_doc Ouvrir le document après génération (défaut: TRUE).
#' @param ... Arguments supplémentaires transmis à \code{\link{report_express}}.
#'
#' @return Le chemin du fichier Word généré (invisible).
#' @export
report_magic <- function(data, output = "magic_report.docx", title = "Rapport Magique Analytix", outcome = NULL, open_doc = TRUE, ...) {
  message("--- Lancement du Rapport Magique (via report_express) ---")

  report_express(
    data     = data,
    output   = output,
    title    = title,
    outcome  = outcome,
    use_ai   = TRUE,
    open_doc = open_doc,
    ...
  )
}
