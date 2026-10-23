#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) > 0L) normalizePath(args[[1]], mustWork = TRUE) else getwd()

read_required <- function(path) {
  if (!file.exists(path)) {
    stop("Fichier requis absent : ", path, call. = FALSE)
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

description <- read.dcf(file.path(root, "DESCRIPTION"))
version <- unname(description[1L, "Version"])
if (is.na(version) || !nzchar(version)) {
  stop("La version est absente de DESCRIPTION.", call. = FALSE)
}

readme <- read_required(file.path(root, "README.md"))
news <- read_required(file.path(root, "NEWS.md"))

badge <- sprintf("Version-%s--beta-blue", gsub("\\.", "\\.", version, fixed = FALSE))
if (!grepl(badge, readme, fixed = FALSE)) {
  stop("Le badge README ne correspond pas à la version ", version, ".", call. = FALSE)
}

news_heading <- sprintf("## Version %s", version)
if (!grepl(news_heading, news, fixed = TRUE)) {
  stop("NEWS.md ne contient pas la version courante ", version, ".", call. = FALSE)
}

if (grepl("!\\[Tests\\]\\([^\\n]*[0-9]+[^\\n]*\\)|!\\[Fonctions\\]\\([^\\n]*[0-9]+[^\\n]*\\)",
          readme, ignore.case = TRUE)) {
  stop("README.md contient encore un badge numérique statique de tests ou de fonctions.",
       call. = FALSE)
}

cat("Release cohérente : analytix ", version, "\n", sep = "")
