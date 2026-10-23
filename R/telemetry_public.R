# ==============================================================================
# analytix — API publique de retour utilisateur
# ==============================================================================

#' Signaler un bug à l'auteur
#'
#' Ouvre un nouveau message adressé à l'auteur. Aucune donnée n'est envoyée
#' automatiquement et aucun journal local n'est créé.
#'
#' @param message Description du bug rencontré.
#' @param fn_name Nom de la fonction concernée (optionnel).
#' @param context Contexte utile à la reproduction, sans donnée sensible (optionnel).
#'
#' @return Invisible \code{TRUE} après ouverture du client de messagerie.
#' @export
#' @examples
#' \dontrun{
#' analytix_report_issue(
#'   "tbl_logistic échoue avec un jeu de données vide",
#'   fn_name = "tbl_logistic"
#' )
#' }
analytix_report_issue <- function(message, fn_name = NULL, context = NULL) {
  stopifnot(is.character(message), length(message) == 1L, nchar(trimws(message)) > 0L)
  if (!is.null(fn_name)) {
    stopifnot(is.character(fn_name), length(fn_name) == 1L)
  }
  if (!is.null(context)) {
    stopifnot(is.character(context), length(context) == 1L)
  }

  details <- c(
    "Bonjour,",
    "",
    "Je souhaite signaler le bug suivant :",
    trimws(message),
    "",
    if (!is.null(fn_name)) paste0("Fonction concernée : ", fn_name),
    if (!is.null(context)) paste0("Contexte : ", context),
    "",
    "Version d'analytix : ",
    as.character(utils::packageVersion("analytix")),
    "Version de R : ",
    paste(R.version$major, R.version$minor, sep = ".")
  )

  .analytix_open_feedback(
    subject = paste0("[analytix] Bug", if (!is.null(fn_name)) paste0(" - ", fn_name)),
    body = paste(details[nzchar(details)], collapse = "\n")
  )
}

#' Envoyer une suggestion à l'auteur
#'
#' Ouvre un nouveau message adressé à l'auteur. La suggestion n'est envoyée
#' qu'après validation par l'utilisateur dans son client de messagerie.
#'
#' @param text Texte de la suggestion.
#'
#' @return Invisible \code{TRUE} après ouverture du client de messagerie.
#' @export
#' @examples
#' \dontrun{
#' analytix_feedback("Ajouter un export PDF aux rapports.")
#' }
analytix_feedback <- function(text) {
  stopifnot(is.character(text), length(text) == 1L, nchar(trimws(text)) > 0L)

  .analytix_open_feedback(
    subject = "[analytix] Suggestion",
    body = paste(
      "Bonjour,\n\n",
      "Je souhaite proposer l'amélioration suivante :\n",
      trimws(text), "\n\n",
      "Version d'analytix : ", as.character(utils::packageVersion("analytix")),
      "\nVersion de R : ", paste(R.version$major, R.version$minor, sep = "."),
      sep = ""
    )
  )
}
