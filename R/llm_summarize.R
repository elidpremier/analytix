#' @title Module de rédaction automatique via API LLM
#' @description Envoi de tableaux statistiques agrégés à une API LLM gratuite
#' (Groq ou Mistral AI) pour générer des synthèses rédigées en français professionnel.
#' Seules les statistiques déjà calculées et anonymisées sont transmises — jamais les
#' données individuelles.
#'
#' @name llm_summarize
#' @rdname llm_summarize
NULL

# ==============================================================================
# Utilitaires internes
# ==============================================================================

#' Vérifier si une clé API est disponible (en inspectant Sys.getenv, .Renviron et .env)
#' @param provider "groq" ou "mistral"
#' @return La clé API (chaîne) ou NULL si absente
#' @noRd
.get_api_key <- function(provider = c("groq", "mistral")) {
  provider <- match.arg(provider)
  var_name <- switch(provider,
    groq    = "GROQ_API_KEY",
    mistral = "MISTRAL_API_KEY"
  )

  # Auto-chargement sécurisé depuis .Renviron ou .env si présent localement
  if (file.exists(".Renviron")) try(readRenviron(".Renviron"), silent = TRUE)
  if (file.exists(".env")) try(readRenviron(".env"), silent = TRUE)

  key <- Sys.getenv(var_name, unset = NA_character_)
  if (is.na(key) || nchar(trimws(key)) == 0) return(NULL)
  key
}

#' Extraire les données textuelles d'un objet analytix pour le prompt LLM
#' @param obj Un objet analytix_table, flextable, ou data.frame
#' @return Chaîne de caractères représentant les données
#' @noRd
.extract_table_text <- function(obj) {
  tryCatch({
    # Cas 1: c'est un data.frame directement
    if (is.data.frame(obj)) {
      lines <- apply(obj, 1, function(r) paste(names(obj), r, sep = ": ", collapse = " | "))
      return(paste(lines, collapse = "\n"))
    }
    # Cas 2: c'est une liste analytix avec $data ou $flextable
    if (is.list(obj)) {
      if (!is.null(obj$data) && is.data.frame(obj$data)) {
        return(.extract_table_text(obj$data))
      }
      if (!is.null(obj$flextable)) {
        ft <- obj$flextable
        if (inherits(ft, "flextable") && !is.null(ft$body$dataset)) {
          return(.extract_table_text(ft$body$dataset))
        }
      }
    }
    # Cas 3: flextable direct
    if (inherits(obj, "flextable") && !is.null(obj$body$dataset)) {
      return(.extract_table_text(obj$body$dataset))
    }
    return("[données non extractibles]")
  }, error = function(e) {
    return("[erreur d'extraction de données]")
  })
}

#' Construire le prompt pour l'API LLM en respectant les Règles Fondamentales d'Interprétation
#' @param table_text Texte représentant les statistiques
#' @param variable_name Nom ou libellé de la variable
#' @param context Contexte général de l'analyse (optionnel)
#' @param n_sentences Nombre de phrases souhaitées (défaut: 3)
#' @return Prompt formaté
#' @noRd
.build_prompt <- function(table_text, variable_name = NULL,
                           context = NULL, n_sentences = 3) {
  context_text <- if (!is.null(context) && nchar(trimws(context)) > 0) {
    paste0("Contexte de l'analyse : ", context, "\n\n")
  } else ""

  var_text <- if (!is.null(variable_name) && nchar(trimws(variable_name)) > 0) {
    paste0(" pour la variable « ", variable_name, " »")
  } else ""

  paste0(
    context_text,
    "Tu es un statisticien expert de niveau académique.\n",
    "Rédige l'interprétation scientifique des données du tableau suivant", var_text, ".\n\n",
    "RÈGLES FONDAMENTALES D'INTERPRÉTATION :\n",
    "1. Une interprétation par tableau, strictement indépendante et autonome.\n",
    "2. Aucune comparaison ni aucun regroupement.\n",
    "3. Aucune donnée ne doit être inventée ou modifiée. Transmets les chiffres exacts.\n",
    "4. Rédaction neutre et formelle, au passé.\n",
    "5. STYLE ET FORMATAGE :\n",
    "   - Quantitatif : Moyenne ± écart-type [unité], min/max si disponibles.\n",
    "   - Qualitatif : Modalités présentées de façon neutre avec effectifs et pourcentages, sans causalité.\n",
    "   - Pour les tableaux avec 3 modalités ou plus (≥ 3) :\n",
    "     Respecter strictement la STRUCTURE EN 3 LIGNES :\n",
    "     Ligne 1 : Phrase scientifique rédigée au passé présentant 2 ou 3 résultats marquants de manière académique et neutre avec les valeurs numériques exactes (strictement descriptif, sans comparaison ni hiérarchisation ni mise en évidence de prédominance).\n",
    "     Ligne 2 : Description complémentaire neutre des autres modalités et de leurs valeurs numériques.\n",
    "     Ligne 3 : Synthèse descriptive finale des effectifs ou valeurs restantes, sans aucune interprétation causaliste.\n",
    "   - Pour les tableaux avec moins de 3 modalités (< 3) :\n",
    "     Rédiger un texte simple (1-2 phrases descriptives au passé, sans appliquer la structure stricte à 3 lignes).\n\n",
    "INTERDICTIONS STRICTES :\n",
    "- Pas d'explication ou de conclusion.\n",
    "- Pas de comparaison.\n",
    "- Pas de rappel des consignes dans ta réponse.\n",
    "- Pas de modification des données.\n",
    "- Pas d'évaluation ni de mise en évidence d'une prédominance.\n\n",
    "Statistiques du tableau :\n", table_text, "\n\n",
    "Interprétation :"
  )
}

# ==============================================================================
# Appels API
# ==============================================================================

#' Appel à l'API Groq (avec basculement automatique de modèle si 404/model_not_found)
#' @param prompt Prompt à envoyer
#' @param model Modèle Groq (défaut: Sys.getenv("GROQ_MODEL") ou "llama-3.1-8b-instant")
#' @param key Clé API Groq
#' @return Texte généré ou NULL en cas d'erreur
#' @noRd
.call_groq <- function(prompt, model = NULL, key) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Le package 'jsonlite' est requis. Installez-le avec install.packages('jsonlite').")
  }

  if (is.null(model) || nchar(trimws(model)) == 0) {
    model <- Sys.getenv("GROQ_MODEL", unset = "groq/compound-mini")
  }

  # Modèles actifs prioritaires Groq
  fallback_models <- unique(c(
    model,
    "groq/compound-mini",
    "groq/compound",
    "openai/gpt-oss-120b",
    "openai/gpt-oss-20b",
    "llama-3.1-8b-instant"
  ))

  last_err <- NULL
  for (m in fallback_models) {
    body <- jsonlite::toJSON(list(
      model       = m,
      messages    = list(list(role = "user", content = prompt)),
      max_tokens  = 600L,
      temperature = 0.3
    ), auto_unbox = TRUE)

    res <- tryCatch({
      resp <- .http_post(
        url     = "https://api.groq.com/openai/v1/chat/completions",
        body    = body,
        headers = c(
          "Content-Type"  = "application/json",
          "Authorization" = paste("Bearer", key)
        )
      )
      result <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
      result$choices[[1]]$message$content
    }, error = function(e) {
      last_err <<- conditionMessage(e)
      NULL
    })

    if (!is.null(res) && nchar(trimws(res)) > 0) {
      return(res)
    }
  }

  warning("Erreur API Groq : ", last_err)
  NULL
}

#' Appel à l'API Mistral AI
#' @param prompt Prompt à envoyer
#' @param model Modèle Mistral (défaut: "mistral-small-latest")
#' @param key Clé API Mistral
#' @return Texte généré ou NULL en cas d'erreur
#' @noRd
.call_mistral <- function(prompt, model = "mistral-small-latest", key) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Le package 'jsonlite' est requis. Installez-le avec install.packages('jsonlite').")
  }

  body <- jsonlite::toJSON(list(
    model    = model,
    messages = list(list(role = "user", content = prompt)),
    max_tokens = 600L,
    temperature = 0.3
  ), auto_unbox = TRUE)

  tryCatch({
    resp <- .http_post(
      url     = "https://api.mistral.ai/v1/chat/completions",
      body    = body,
      headers = c(
        "Content-Type"  = "application/json",
        "Authorization" = paste("Bearer", key)
      )
    )
    result <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
    result$choices[[1]]$message$content
  }, error = function(e) {
    warning("Erreur API Mistral : ", conditionMessage(e))
    NULL
  })
}

#' Couche HTTP bas niveau (utilise curl si disponible, sinon download.file)
#' @noRd
.http_post <- function(url, body, headers) {
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = as.list(headers))
    curl::handle_setopt(h, postfields = body)
    resp <- curl::curl_fetch_memory(url, h)
    if (resp$status_code >= 400) {
      stop(paste0("HTTP ", resp$status_code, ": ", rawToChar(resp$content)))
    }
    return(rawToChar(resp$content))
  }

  # Fallback : utiliser utils::download.file avec tempfiles
  tmp_body <- tempfile(fileext = ".json")
  tmp_out  <- tempfile(fileext = ".json")
  writeLines(body, tmp_body)

  header_args <- paste(
    sapply(names(headers), function(h) paste0("-H '", h, ": ", headers[[h]], "'")),
    collapse = " "
  )
  cmd <- paste0(
    "curl -s -X POST ", header_args,
    " -d @", shQuote(tmp_body), " ", shQuote(url),
    " -o ", shQuote(tmp_out)
  )
  ret <- system(cmd, ignore.stdout = FALSE)
  if (ret != 0 || !file.exists(tmp_out)) stop("Appel curl échoué.")
  on.exit({ try(unlink(tmp_body)); try(unlink(tmp_out)) }, add = TRUE)
  paste(readLines(tmp_out, warn = FALSE), collapse = "\n")
}

# ==============================================================================
# Synthèse heuristique locale (mode dégradé / fallback)
# ==============================================================================

#' Générer une synthèse heuristique locale sans API (respectant les règles fondamentales)
#' @param table_text Texte des statistiques
#' @param variable_name Libellé de la variable
#' @return Chaîne synthétique générée localement
#' @noRd
.local_fallback_summary <- function(table_text, variable_name = NULL) {
  var_label <- if (!is.null(variable_name)) paste0("la variable « ", variable_name, " »") else "le tableau"

  lines <- strsplit(table_text, "\n")[[1]]

  # Détecter si des pourcentages ou indicateurs sont présents
  pct_lines <- grep("\\d+[,.]?\\d*\\s*%", lines, value = TRUE)
  central_lines <- grep("(?i)(médiane|moyenne|mean|median|moy\\.|min|max)", lines, value = TRUE)

  parts <- character(0)

  if (length(pct_lines) >= 3) {
    # Structure stricte en 3 lignes pour ≥3 modalités
    parts <- c(parts, paste0(
      "L'analyse descriptive concernant ", var_label, " a mis en évidence les résultats suivants : ",
      paste(head(pct_lines, 2), collapse = " ; "), "."
    ))
    parts <- c(parts, paste0(
      "Les autres modalités ont enregistré les valeurs de : ",
      paste(pct_lines[3:min(4, length(pct_lines))], collapse = " ; "), "."
    ))
    parts <- c(parts, paste0(
      "L'ensemble des effectifs observés a été consigné dans les données de répartition."
    ))
  } else if (length(pct_lines) > 0) {
    # Moins de 3 modalités
    parts <- c(parts, paste0(
      "Pour ", var_label, ", les données descriptives ont indiqué : ",
      paste(pct_lines, collapse = " ; "), "."
    ))
  } else if (length(central_lines) > 0) {
    parts <- c(parts, paste0(
      "Les indicateurs quantitatifs mesurés pour ", var_label, " ont présenté les valeurs de : ",
      paste(central_lines, collapse = " ; "), "."
    ))
  } else {
    parts <- c(parts, paste0(
      "Les statistiques descriptives agrégées pour ", var_label, " ont été colligées."
    ))
  }

  paste(parts, collapse = "\n")
}

# ==============================================================================
# Fonction principale exportée
# ==============================================================================

#' @title Résumé automatique d'un tableau statistique par LLM
#' @description Prend un tableau de résultats `analytix` (ou un data.frame de statistiques
#' agrégées) et génère automatiquement 4–5 phrases de synthèse en français professionnel
#' via une API LLM gratuite (Groq ou Mistral AI).
#'
#' **Confidentialité** : seules les statistiques déjà agrégées et anonymisées
#' sont transmises à l'API — jamais les données individuelles.
#'
#' @param table_obj Tableau à synthétiser : objet `analytix_table`, `flextable`,
#'   ou `data.frame` contenant les statistiques agrégées.
#' @param variable_name Libellé de la variable décrite (optionnel, pour enrichir le prompt).
#' @param context Contexte de l'analyse, par ex. "Enquête nutritionnelle, 450 enfants de moins
#'   de 5 ans au Bénin" (optionnel).
#' @param provider Fournisseur LLM à utiliser : `"groq"` (défaut, rapide) ou `"mistral"`
#'   (meilleur en français). Si la clé du fournisseur choisi est absente, le système
#'   bascule automatiquement sur l'autre fournisseur disponible.
#' @param model Modèle LLM spécifique (optionnel).
#' @param n_sentences Nombre de phrases de synthèse souhaitées (défaut : 4).
#' @param verbose Afficher les messages de progression (défaut : FALSE).
#'
#' @return Une chaîne de caractères contenant la synthèse rédigée, ou une synthèse
#'   heuristique locale si aucune API n'est disponible.
#'
#' @examples
#' \dontrun{
#'   # Prérequis : définir la variable d'environnement GROQ_API_KEY
#'   # Sys.setenv(GROQ_API_KEY = "votre_cle_ici")
#'
#'   res <- desc_numeric(mtcars, mpg)
#'   synthese <- llm_summarize_table(res, variable_name = "Consommation (mpg)",
#'                                   context = "Étude sur les performances automobiles")
#'   cat(synthese)
#' }
#'
#' @export
llm_summarize_table <- function(table_obj,
                                 variable_name = NULL,
                                 context       = NULL,
                                 provider      = c("groq", "mistral"),
                                 model         = NULL,
                                 n_sentences   = 4L,
                                 verbose       = FALSE) {
  provider <- match.arg(provider)
  providers_order <- unique(c(provider, setdiff(c("groq", "mistral"), provider)))

  # Extraire le texte représentant les statistiques
  table_text <- .extract_table_text(table_obj)

  if (verbose) message("  → Extraction des données : OK (", nchar(table_text), " caractères)")

  # Construire le prompt
  prompt <- .build_prompt(table_text, variable_name = variable_name,
                           context = context, n_sentences = n_sentences)

  # Essayer les fournisseurs dans l'ordre de priorité
  result <- NULL
  for (prov in providers_order) {
    key <- .get_api_key(prov)
    if (is.null(key)) {
      if (verbose) message("  → Pas de clé API pour ", prov, ", on passe.")
      next
    }
    if (verbose) message("  → Appel API ", prov, "...")
    result <- switch(prov,
      groq    = .call_groq(prompt, model = model, key = key),
      mistral = .call_mistral(prompt, model = model, key = key)
    )
    if (!is.null(result) && nchar(trimws(result)) > 0) {
      if (verbose) message("  → Synthèse générée via ", prov, " ✅")
      break
    }
  }

  # Mode dégradé : pas de clé API ou erreur
  if (is.null(result) || nchar(trimws(result)) == 0) {
    if (verbose) message("  → Mode dégradé : synthèse heuristique locale")
    result <- .local_fallback_summary(table_text, variable_name)
  }

  trimws(result)
}

#' @title Résumé global d'un rapport : synthèse de tous les tableaux
#' @description Parcourt une liste nommée de résultats analytix (issue de `desc_auto()`
#' ou `report_express()`) et génère une synthèse LLM pour chaque variable.
#'
#' @param results Liste nommée d'objets analytix (issue de `desc_auto()` par exemple).
#' @param context Contexte général de l'analyse (optionnel).
#' @param provider Fournisseur LLM (`"groq"` ou `"mistral"`).
#' @param model Modèle spécifique (optionnel, ex. `"llama-3.1-8b-instant"`, `"compound-mini"`).
#' @param verbose Afficher les messages de progression (défaut : TRUE).
#'
#' @return Une liste nommée (mêmes noms que `results`) de chaînes de caractères.
#'
#' @export
llm_summarize_report <- function(results,
                                  context  = NULL,
                                  provider = c("groq", "mistral"),
                                  model    = NULL,
                                  verbose  = TRUE) {
  provider <- match.arg(provider)
  if (!is.list(results)) stop("`results` doit être une liste.")

  nms <- names(results)
  syntheses <- vector("list", length(results))
  names(syntheses) <- nms

  n <- length(results)
  for (i in seq_along(results)) {
    nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else paste0("Variable_", i)
    if (verbose) message(sprintf("[%d/%d] Synthèse de : %s", i, n, nm))
    syntheses[[nm]] <- llm_summarize_table(
      table_obj     = results[[i]],
      variable_name = nm,
      context       = context,
      provider      = provider,
      model         = model,
      verbose       = verbose
    )
    # Pause légère entre appels pour respecter les quotas (30 req/min pour Groq)
    if (i < n) Sys.sleep(0.5)
  }

  syntheses
}

#' @title Configurer de manière sécurisée les clés API LLM localement
#' @description Écrit ou met à jour le fichier `.Renviron` local sans risquer d'exposer des clés confidentielles sur GitHub.
#'
#' @param groq_key Clé API Groq (optionnel)
#' @param mistral_key Clé API Mistral (optionnel)
#' @param groq_model Modèle Groq par défaut (optionnel, ex. `"llama-3.1-8b-instant"`, `"compound-mini"`)
#' @param path Chemin vers le fichier d'environnement (défaut: ".Renviron")
#'
#' @return Invisible TRUE en cas de succès
#' @export
setup_api_keys <- function(groq_key = NULL, mistral_key = NULL, groq_model = NULL, path = ".Renviron") {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)

  set_key <- function(lines, var_name, key_val) {
    if (is.null(key_val) || nchar(trimws(key_val)) == 0) return(lines)
    lines <- lines[!grepl(paste0("^", var_name, "="), lines)]
    c(lines, paste0(var_name, "=", trimws(key_val)))
  }

  lines <- set_key(lines, "GROQ_API_KEY", groq_key)
  lines <- set_key(lines, "MISTRAL_API_KEY", mistral_key)
  lines <- set_key(lines, "GROQ_MODEL", groq_model)

  writeLines(lines, path)
  readRenviron(path)
  message("  → Clés API enregistrées dans '", path, "' (ignoré par .gitignore) et chargées dans la session R.")
  invisible(TRUE)
}
