# ==============================================================================
# analytix — API publique de télémétrie
# Fichier : R/telemetry_public.R
# ==============================================================================

#' Configurer la télémétrie du package analytix
#'
#' Configure l'envoi anonyme de statistiques d'utilisation vers un webhook
#' Google Sheets. Doit être appelé une seule fois par machine.
#'
#' @param webhook_url URL du Google Apps Script déployé comme Web App.
#' @param opt_in Logique. \code{TRUE} pour activer la télémétrie (défaut).
#'
#' @return Invisible \code{TRUE} si la configuration a réussi.
#' @export
#' @examples
#' \dontrun{
#' analytix_setup(webhook_url = "https://script.google.com/macros/s/.../exec")
#' }
analytix_setup <- function(webhook_url, opt_in = TRUE) {
  stopifnot(is.character(webhook_url), length(webhook_url) == 1, nchar(webhook_url) > 0)
  cfg <- .analytix_get_config()
  cfg$webhook_url <- trimws(webhook_url)
  cfg$opt_in      <- isTRUE(opt_in)
  if (is.null(cfg$session_id) || nchar(trimws(cfg$session_id %||% "")) == 0) {
    cfg$session_id <- paste0("anx_", paste(sample(c(letters, 0:9), 16, replace = TRUE), collapse = ""))
  }
  .analytix_write_config(cfg)
  message("✅ analytix : Télémétrie configurée. Merci de votre soutien !")
  # Premier ping de test
  .analytix_send("usage", c(.analytix_base_payload("analytix_setup"), list(context = "setup")))
  invisible(TRUE)
}

#' Désactiver la télémétrie analytix
#'
#' Désactive l'envoi de toute donnée. Les préférences sont conservées localement.
#'
#' @return Invisible \code{FALSE}.
#' @export
analytix_opt_out <- function() {
  cfg        <- .analytix_get_config()
  cfg$opt_in <- FALSE
  .analytix_write_config(cfg)
  message("⛔ analytix : Télémétrie désactivée. Aucune donnée ne sera envoyée.")
  invisible(FALSE)
}

#' Signaler un incident ou une erreur
#'
#' Permet de signaler manuellement une erreur rencontrée lors de l'utilisation
#' d'une fonction du package. L'incident est envoyé au tableau de bord de l'auteur.
#'
#' @param message Message décrivant l'incident (chaîne de caractères).
#' @param fn_name Nom de la fonction concernée (optionnel).
#' @param context Contexte additionnel (données, paramètres — évitez les données sensibles).
#'
#' @return Invisible \code{TRUE} si envoyé, \code{FALSE} si hors-ligne (stocké localement).
#' @export
#' @examples
#' \dontrun{
#' analytix_report_issue("tbl_logistic plante avec un jeu de données vide", fn_name = "tbl_logistic")
#' }
analytix_report_issue <- function(message, fn_name = NULL, context = NULL) {
  stopifnot(is.character(message), length(message) == 1)
  payload <- .analytix_base_payload(fn_name %||% "")
  payload$error_message <- message
  payload$context       <- if (!is.null(context)) as.character(context) else ""
  ok <- .analytix_send("incidents", payload)
  if (ok) {
    message("📨 Incident envoyé. Merci pour votre retour !")
  } else {
    message("📥 Incident enregistré localement (sera envoyé dès que possible).")
  }
  invisible(ok)
}

#' Envoyer un commentaire ou une note à l'auteur
#'
#' Permet d'envoyer un retour libre (suggestion, bug, compliment) directement
#' à l'auteur du package.
#'
#' @param text Texte libre du commentaire.
#' @param type Type de retour : \code{"suggestion"}, \code{"bug"}, \code{"compliment"} ou \code{"autre"}.
#' @param rating Note de satisfaction de 1 à 5 (optionnel).
#'
#' @return Invisible \code{TRUE} si envoyé.
#' @export
#' @examples
#' \dontrun{
#' analytix_feedback("Le module Likert est excellent !", type = "compliment", rating = 5)
#' analytix_feedback("Il manque un export PDF", type = "suggestion")
#' }
analytix_feedback <- function(text, type = c("suggestion", "bug", "compliment", "autre"), rating = NULL) {
  type    <- match.arg(type)
  stopifnot(is.character(text), length(text) == 1, nchar(trimws(text)) > 0)
  if (!is.null(rating)) stopifnot(is.numeric(rating), rating >= 1, rating <= 5)

  payload <- .analytix_base_payload("analytix_feedback")
  payload$type   <- type
  payload$rating <- if (!is.null(rating)) round(rating, 0) else ""
  payload$text   <- trimws(text)

  ok <- .analytix_send("feedback", payload)
  if (ok) {
    message("💬 Commentaire envoyé. Merci !")
  } else {
    message("📥 Commentaire enregistré localement (sera envoyé dès que possible).")
  }
  invisible(ok)
}

#' Afficher le statut du log local de télémétrie
#'
#' Affiche un résumé des événements stockés localement (en attente d'envoi).
#'
#' @return Invisible data.frame avec le résumé.
#' @export
analytix_log_status <- function() {
  path <- .analytix_log_path()
  cfg  <- .analytix_get_config()

  cat(cli::rule(left = cli::col_blue("analytix — Statut Télémétrie")), "\n")
  cat("  Opt-in     :", if (isTRUE(cfg$opt_in)) cli::col_green("Activé") else cli::col_red("Désactivé"), "\n")
  cat("  Session ID :", cfg$session_id %||% "(non configuré)", "\n")
  cat("  Webhook    :", if (!is.null(cfg$webhook_url)) cli::col_green("Configuré") else cli::col_red("Non configuré"), "\n\n")

  if (!file.exists(path)) {
    cat("  📭 Aucun événement en attente.\n")
    return(invisible(data.frame()))
  }

  log <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) list())
  if (length(log) == 0) {
    cat("  📭 Aucun événement en attente.\n")
    return(invisible(data.frame()))
  }

  types <- vapply(log, function(x) x$type %||% "?", character(1))
  tbl   <- as.data.frame(table(Type = types), stringsAsFactors = FALSE)
  colnames(tbl) <- c("Type", "En attente")
  cat("  📬", nrow(log), "événement(s) en attente d'envoi :\n\n")
  print(tbl, row.names = FALSE)
  cat("\n  Utilisez ", cli::col_green("analytix_setup()"), " pour configurer le webhook et déclencher l'envoi.\n")
  cat(cli::rule(), "\n")
  invisible(tbl)
}

# Opérateur null-coalescing interne (si pas déjà défini)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nchar(as.character(a[1])) > 0) a else b
