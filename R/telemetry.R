# ==============================================================================
# analytix — Moteur interne de télémétrie
# Fichier : R/telemetry.R
# Usage   : Fonctions internes uniquement (non exportées)
# ==============================================================================

# Chemin du fichier de configuration utilisateur
.analytix_config_path <- function() {
  file.path(path.expand("~"), ".analytix_config")
}

# Chemin du log local (données en attente d'envoi)
.analytix_log_path <- function() {
  file.path(path.expand("~"), ".analytix_log.json")
}

# Lit la configuration (liste avec opt_in, session_id, webhook_url)
.analytix_get_config <- function() {
  path <- .analytix_config_path()
  if (!file.exists(path)) return(list(opt_in = FALSE, session_id = NULL, webhook_url = NULL))
  tryCatch(
    jsonlite::fromJSON(path),
    error = function(e) list(opt_in = FALSE, session_id = NULL, webhook_url = NULL)
  )
}

# Écrit la configuration
.analytix_write_config <- function(cfg) {
  tryCatch(
    writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE), .analytix_config_path()),
    error = function(e) invisible(NULL)
  )
}

# Génère (ou récupère) un identifiant de session anonyme et permanent
.analytix_session_id <- function() {
  cfg <- .analytix_get_config()
  if (!is.null(cfg$session_id) && nchar(cfg$session_id) > 0) return(cfg$session_id)
  id <- paste0("anx_", paste(sample(c(letters, 0:9), 16, replace = TRUE), collapse = ""))
  cfg$session_id <- id
  .analytix_write_config(cfg)
  id
}

# Payload de base commun à tous les envois
.analytix_base_payload <- function(fn_name = NA_character_) {
  list(
    session_id  = .analytix_session_id(),
    fn_name     = if (is.na(fn_name)) "" else fn_name,
    pkg_version = as.character(utils::packageVersion("analytix")),
    r_version   = paste0(R.Version()$major, ".", R.Version()$minor),
    platform    = .Platform$OS.type
  )
}

# Envoie un POST au webhook (silencieux si hors-ligne ou erreur)
# Retourne TRUE si succès, FALSE sinon
.analytix_send_http <- function(type, payload) {
  cfg <- .analytix_get_config()
  if (is.null(cfg$webhook_url) || nchar(trimws(cfg$webhook_url)) == 0) return(FALSE)
  tryCatch({
    body <- jsonlite::toJSON(list(type = type, payload = payload), auto_unbox = TRUE)
    req  <- httr2::request(cfg$webhook_url) |>
      httr2::req_body_raw(body, type = "application/json") |>
      httr2::req_timeout(5) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) == 200
  }, error = function(e) FALSE)
}

# Écrit dans le log local JSON (mode file d'attente)
.analytix_log_local <- function(type, payload) {
  path  <- .analytix_log_path()
  entry <- list(type = type, payload = payload, queued_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))
  log   <- if (file.exists(path)) {
    tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) list())
  } else {
    list()
  }
  # Garde max 500 entrées pour ne pas polluer le disque
  log   <- c(log, list(entry))
  if (length(log) > 500) log <- tail(log, 500)
  tryCatch(
    writeLines(jsonlite::toJSON(log, auto_unbox = TRUE, pretty = FALSE), path),
    error = function(e) invisible(NULL)
  )
}

# Tente de vider le log local vers le webhook (appelé à chaque envoi réussi)
.analytix_flush_pending <- function() {
  path <- .analytix_log_path()
  if (!file.exists(path)) return(invisible(NULL))
  log <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) list())
  if (length(log) == 0) return(invisible(NULL))

  cfg <- .analytix_get_config()
  if (is.null(cfg$webhook_url) || nchar(trimws(cfg$webhook_url)) == 0) return(invisible(NULL))

  tryCatch({
    body <- jsonlite::toJSON(list(type = "pending_flush", batch = log), auto_unbox = TRUE)
    req  <- httr2::request(cfg$webhook_url) |>
      httr2::req_body_raw(body, type = "application/json") |>
      httr2::req_timeout(10) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) == 200) {
      file.remove(path)  # Vide le log après flush réussi
    }
  }, error = function(e) invisible(NULL))
}

# Point d'entrée principal — appelé par .track_call() et les fonctions publiques
.analytix_send <- function(type, payload) {
  ok <- .analytix_send_http(type, payload)
  if (!ok) {
    # Hors-ligne : on met en file d'attente
    .analytix_log_local(type, payload)
  } else {
    # En ligne : on profite pour vider les entrées en attente
    .analytix_flush_pending()
  }
  invisible(ok)
}

# Macro de tracking d'utilisation appelée en tête de chaque fonction exportée
# Usage : .track_call("desc_numeric")
.track_call <- function(fn_name, context = NULL) {
  cfg <- .analytix_get_config()
  if (!isTRUE(cfg$opt_in)) return(invisible(NULL))
  payload <- .analytix_base_payload(fn_name)
  payload$context <- if (!is.null(context)) as.character(context) else ""
  .analytix_send("usage", payload)
  invisible(NULL)
}

# Affiche le message d'invitation opt-in (appelé dans .onAttach)
.analytix_invite_optin <- function() {
  cfg <- .analytix_get_config()
  # Ne montre le message que si opt_in n'a jamais été défini
  if (!is.null(cfg$opt_in)) return(invisible(NULL))
  packageStartupMessage(
    "\n",
    cli::rule(left = cli::col_blue("\U0001f4ca analytix — Statistiques d'utilisation")),
    "\n",
    "  Aidez a ameliorer le package en autorisant l'envoi\n",
    "  anonyme de statistiques d'utilisation (aucune donnee\n",
    "  personnelle — fonction appelee, version R, plateforme).\n\n",
    "  Pour activer : ", cli::col_green("analytix_setup(webhook_url = \"<votre_url>\")"), "\n",
    "  Pour refuser : ", cli::col_red("analytix_opt_out()"), "\n",
    cli::rule(), "\n"
  )
}
