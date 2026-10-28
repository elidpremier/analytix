test_that("analytix_sample_data produit un âge variable et valide n", {
  result <- analytix_sample_data(n = 100, seed = 42)

  expect_gt(length(unique(result$age)), 1)
  expect_gte(min(result$age), 18)
  expect_lte(max(result$age), 95)
  expect_error(analytix_sample_data(n = 1), "`n` doit être")
  expect_error(analytix_sample_data(n = 2.5), "`n` doit être")
})

test_that("prep_recode_odk ne fabrique pas de réponses", {
  expect_warning(
    result <- prep_recode_odk(c("Oui", "Non", NA, "réponse inconnue")),
    "hors vocabulaire"
  )

  expect_identical(
    as.character(result),
    c("Oui", "Non", NA_character_, NA_character_)
  )
})

test_that("les clés API sont écrites avec des permissions privées", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)

  setup_api_keys(
    groq_key = 'key"with\\special',
    path = path
  )

  expect_true(file.exists(path))
  expect_match(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    'GROQ_API_KEY="key\\\\"with\\\\\\\\special"',
    perl = TRUE
  )
  if (.Platform$OS.type != "windows") {
    expect_equal(as.octmode(file.info(path)$mode), as.octmode("0600"))
  }
})

test_that("export_word ne parcourt pas globalenv sans env explicite", {
  expect_error(
    export_word(path = tempfile(fileext = ".docx")),
    "Aucun tableau analytique"
  )
})

test_that("la couche HTTP ne construit pas de commande shell", {
  source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  source_text <- paste(unlist(lapply(source_files, readLines, warn = FALSE)), collapse = "\n")

  expect_false(grepl("\\.http_post[\\s\\S]*system\\s*\\(", source_text, perl = TRUE))
})
