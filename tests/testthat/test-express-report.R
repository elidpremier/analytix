# Tests unitaires pour les nouvelles fonctions de la version 0.6.0
# Composant : Rapport Express & Module LLM
# ==============================================================================

# ─── Données de test ──────────────────────────────────────────────────────────

.make_test_data <- function(n = 50) {
  set.seed(42)
  data.frame(
    id       = seq_len(n),
    age      = sample(18:65, n, replace = TRUE),
    sexe     = sample(c("Homme", "Femme"), n, replace = TRUE),
    region   = sample(c("Nord", "Sud", "Est", "Ouest"), n, replace = TRUE),
    revenu   = round(runif(n, 15000, 120000)),
    outcome  = sample(c("Bon", "Mauvais"), n, replace = TRUE, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )
}

# ─── Tests : llm_summarize_table (mode dégradé, sans API) ────────────────────

test_that("llm_summarize_table retourne une chaîne sans clé API", {
  # S'assurer qu'il n'y a pas de clé API dans l'environnement de test
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df <- data.frame(
      Modalite  = c("Oui", "Non"),
      Effectif  = c(30L, 20L),
      Frequence = c("60%", "40%")
    )
    result <- llm_summarize_table(df, variable_name = "Accès à l'eau potable")
    expect_type(result, "character")
    expect_true(nchar(trimws(result)) > 10)
  })
})

test_that("llm_summarize_table fonctionne avec un objet analytix", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df  <- .make_test_data()
    res <- desc_categorical(df, sexe)
    result <- llm_summarize_table(res, variable_name = "Sexe")
    expect_type(result, "character")
    expect_true(nchar(trimws(result)) > 0)
  })
})

test_that("llm_summarize_table accepte un data.frame nu", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    stats_df <- data.frame(
      Indicateur = c("Moyenne", "Médiane", "Écart-type", "Min", "Max"),
      Valeur     = c("42.5", "41.0", "12.3", "18", "65"),
      stringsAsFactors = FALSE
    )
    result <- llm_summarize_table(stats_df, variable_name = "Âge")
    expect_type(result, "character")
  })
})

test_that("setup_api_keys écrit les clés sans les charger automatiquement", {
  tmp_env <- tempfile()
  on.exit(unlink(tmp_env))
  withr::local_envvar(GROQ_API_KEY = "")
  setup_api_keys(groq_key = "gsk_test_12345", path = tmp_env)
  expect_true(file.exists(tmp_env))
  content <- readLines(tmp_env)
  expect_true(any(grepl('GROQ_API_KEY="gsk_test_12345"', content, fixed = TRUE)))
  expect_identical(Sys.getenv("GROQ_API_KEY"), "")
  if (.Platform$OS.type != "windows") {
    expect_equal(as.octmode(file.info(tmp_env)$mode), as.octmode("0600"))
  }
})

test_that(".build_prompt contient les règles d'interprétation strictes", {
  prompt <- analytix:::.build_prompt("Stat: val", variable_name = "VarTest")
  expect_true(grepl("RÈGLES FONDAMENTALES D'INTERPRÉTATION", prompt))
  expect_true(grepl("STRUCTURE EN 3 LIGNES", prompt))
  expect_true(grepl("INTERDICTIONS STRICTES", prompt))
})

# ─── Tests : llm_summarize_report ────────────────────────────────────────────

test_that("llm_summarize_report retourne une liste nommée", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df <- .make_test_data()
    results <- desc_auto(df[, c("sexe", "region")], verbose = FALSE)
    syntheses <- llm_summarize_report(results, verbose = FALSE)
    expect_type(syntheses, "list")
    expect_equal(length(syntheses), length(results))
    expect_true(all(nchar(unlist(syntheses)) > 0))
  })
})

# ─── Tests : report_express — structure de sortie ────────────────────────────

test_that("report_express génère un fichier .docx", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df  <- .make_test_data()
    out <- tempfile(fileext = ".docx")
    result_path <- report_express(
      data         = df,
      output       = out,
      title        = "Test Report",
      exclude_vars = "id",
      use_ai       = FALSE,
      verbose      = FALSE
    )
    expect_true(file.exists(out))
    expect_equal(result_path, out)
    expect_gt(file.info(out)$size, 1000)
    unlink(out)
  })
})

test_that("report_magic est un wrapper fonctionnel vers report_express", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df  <- .make_test_data()
    out <- tempfile(fileext = ".docx")
    result_path <- report_magic(
      data    = df,
      output  = out,
      title   = "Test Magic",
      outcome = "outcome"
    )
    expect_true(file.exists(out))
    expect_gt(file.info(out)$size, 1000)
    unlink(out)
  })
})

test_that("report_generate fonctionne avec use_ai = TRUE", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df  <- .make_test_data()
    out <- tempfile(fileext = ".docx")
    result_path <- report_generate(
      data    = df,
      output  = out,
      title   = "Test Gen AI",
      use_ai  = TRUE,
      verbose = FALSE
    )
    expect_true(file.exists(out))
    expect_gt(file.info(out)$size, 1000)
    unlink(out)
  })
})

test_that("report_express fonctionne avec outcome spécifié", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df  <- .make_test_data()
    out <- tempfile(fileext = ".docx")
    expect_no_error({
      report_express(
        data         = df,
        output       = out,
        exclude_vars = "id",
        outcome      = "outcome",
        use_ai       = FALSE,
        verbose      = FALSE
      )
    })
    expect_true(file.exists(out))
    unlink(out)
  })
})

test_that("report_express fonctionne avec outcome = NA (pas de bivariée)", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    df  <- .make_test_data()
    out <- tempfile(fileext = ".docx")
    expect_no_error({
      report_express(
        data         = df,
        output       = out,
        exclude_vars = "id",
        outcome      = NA,
        use_ai       = FALSE,
        verbose      = FALSE
      )
    })
    expect_true(file.exists(out))
    unlink(out)
  })
})

test_that("report_express accepte un chemin CSV en entrée", {
  withr::with_envvar(c(GROQ_API_KEY = "", MISTRAL_API_KEY = ""), {
    csv_path <- system.file("extdata", "sample_data_express.csv", package = "analytix")
    skip_if(!file.exists(csv_path), "sample_data_express.csv non trouvé")
    out <- tempfile(fileext = ".docx")
    expect_no_error({
      report_express(
        data         = csv_path,
        output       = out,
        exclude_vars = "id",
        outcome      = "statut_nutrition",
        use_ai       = FALSE,
        verbose      = FALSE
      )
    })
    expect_true(file.exists(out))
    unlink(out)
  })
})

test_that("report_express retourne une erreur si le data.frame est vide", {
  df_vide <- data.frame(a = integer(0), b = character(0))
  expect_error(
    report_express(df_vide, use_ai = FALSE, verbose = FALSE),
    "vide"
  )
})

test_that("report_express retourne une erreur si le fichier d'entrée n'existe pas", {
  expect_error(
    report_express("fichier_inexistant.xlsx", use_ai = FALSE, verbose = FALSE)
  )
})

# ─── Tests : détection automatique de l'outcome ─────────────────────────────

test_that(".detect_outcome retourne NULL si aucune variable binaire", {
  # Fonctions internes : accès via :::
  df_numeric <- data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20))
  result <- analytix:::.detect_outcome(df_numeric)
  expect_null(result)
})

test_that(".detect_outcome détecte une variable logique en priorité", {
  df <- data.frame(
    age     = 20:39,
    reussi  = sample(c(TRUE, FALSE), 20, replace = TRUE),
    statut  = sample(c("Bon", "Mauvais"), 20, replace = TRUE)
  )
  result <- analytix:::.detect_outcome(df)
  expect_equal(result, "reussi")
})
