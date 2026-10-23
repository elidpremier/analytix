# ==============================================================================
# analytix — Retour utilisateur explicite
# ==============================================================================

.analytix_feedback_email <- "elidopremier@gmail.com"

.analytix_open_feedback <- function(subject, body) {
  encode <- function(value) utils::URLencode(as.character(value), reserved = TRUE)
  mailto <- paste0(
    "mailto:", .analytix_feedback_email,
    "?subject=", encode(subject),
    "&body=", encode(body)
  )

  tryCatch(
    utils::browseURL(mailto),
    error = function(error) {
      stop(
        "Impossible d'ouvrir le client de messagerie. ",
        "Vous pouvez écrire à ", .analytix_feedback_email, ". ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  invisible(TRUE)
}
