test_that("les analyses ne créent aucun fichier de suivi", {
  home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = home))

  result <- desc_numeric(data.frame(score = c(1, 2, 3)), score)

  expect_s3_class(result, "analytix_table")
  expect_false(file.exists(file.path(home, ".analytix_config")))
  expect_false(file.exists(file.path(home, ".analytix_log.json")))
})

test_that("les anciennes surfaces de télémétrie ne sont plus présentes", {
  source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  source_text <- paste(unlist(lapply(source_files, readLines, warn = FALSE)), collapse = "\n")

  expect_false(grepl("\\.track_call\\s*\\(", source_text))
  expect_false(grepl("\\.analytix_send\\s*<-", source_text))
  expect_false(grepl("\\.analytix_log_path\\s*<-", source_text))
  expect_false(grepl("\\.analytix_config_path\\s*<-", source_text))
  expect_false(grepl("httr2::", source_text))
})

test_that("un bug est préparé comme courriel explicite", {
  captured <- new.env(parent = emptyenv())
  captured$subject <- NULL
  captured$body <- NULL

  local_mocked_bindings(
    .analytix_open_feedback = function(subject, body) {
      captured$subject <- subject
      captured$body <- body
      invisible(TRUE)
    },
    .package = "analytix"
  )

  expect_true(analytix_report_issue(
    "Erreur de test",
    fn_name = "desc_numeric",
    context = "Jeu de données synthétique"
  ))
  expect_equal(captured$subject, "[analytix] Bug - desc_numeric")
  expect_match(captured$body, "Erreur de test")
  expect_match(captured$body, "Jeu de données synthétique")
})

test_that("une suggestion est préparée comme courriel explicite", {
  captured <- new.env(parent = emptyenv())
  captured$subject <- NULL
  captured$body <- NULL

  local_mocked_bindings(
    .analytix_open_feedback = function(subject, body) {
      captured$subject <- subject
      captured$body <- body
      invisible(TRUE)
    },
    .package = "analytix"
  )

  expect_true(analytix_feedback("Ajouter un export PDF."))
  expect_identical(captured$subject, "[analytix] Suggestion")
  expect_match(captured$body, "Ajouter un export PDF")
})

test_that("sans clé API, la synthèse reste locale", {
  withr::local_envvar(c(
    GROQ_API_KEY = "",
    MISTRAL_API_KEY = ""
  ))

  result <- llm_summarize_table(data.frame(
    Indicateur = "Moyenne",
    Valeur = "10"
  ))

  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})
