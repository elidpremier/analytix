#' Clean variable names to comply with Epi Info standards
#'
#' This function transforms variable names to meet Epi Info requirements:
#' lowercase, no accents, underscores as separators, starts with a letter,
#' max length constraint, and unique names.
#'
#' @param x A character vector of variable names OR a data.frame whose column
#'   names should be cleaned
#' @param max_length Maximum length of variable names (default: 64)
#' @param prefix Prefix to add if name doesn't start with a letter (default: "v_")
#' @param sep Separator used by \code{make.unique} for duplicate names (default: "_")
#' @return If \code{x} is a character vector: a cleaned character vector.
#'   If \code{x} is a data.frame: the same data.frame with cleaned column names.
#' @export
#' @examples
#' # On a character vector
#' clean_names(c("Nom Patient", "Âge/Ans", "Température (°C)"))
#' # [1] "nom_patient"     "age_ou_ans"      "temperature_c"
#'
#' # Directly on a data.frame
#' df <- data.frame("Nom Patient" = 1:3, "Âge/Ans" = 20:22)
#' clean_names(df)
clean_names <- function(x,
                        max_length = 64,
                        prefix = "v_",
                        sep = "_") {

  # ---- Internal helper: core cleaning logic ----
  .clean_vector <- function(names_vector, max_length, prefix, sep) {

    # Input validation
    if (!is.character(names_vector)) {
      stop("`names_vector` must be a character vector", call. = FALSE)
    }

    # Handle empty input
    if (length(names_vector) == 0) {
      return(character(0))
    }

    # Cleaning pipeline
    cleaned <- trimws(tolower(names_vector))
    cleaned <- gsub("[/\\\\]", " ou ", cleaned, perl = TRUE)
    cleaned <- iconv(cleaned, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    cleaned <- gsub("[^a-z0-9\\s]", " ", cleaned, perl = TRUE)
    cleaned <- gsub("\\s+", " ", cleaned, perl = TRUE)
    cleaned <- trimws(cleaned)
    cleaned <- gsub(" ", "_", cleaned, fixed = TRUE)
    cleaned <- gsub("^_+|_+$", "", cleaned, perl = TRUE)

    # Ensure names start with a letter
    starts_with_letter <- grepl("^[a-z]", cleaned, perl = TRUE)
    cleaned[!starts_with_letter] <- paste0(prefix, cleaned[!starts_with_letter])

    # Truncate and make unique
    cleaned <- substr(cleaned, 1, max_length)
    cleaned <- make.unique(cleaned, sep = sep)

    return(cleaned)
  }

  # ---- Dispatch based on input type ----

  if (is.data.frame(x)) {
    # Apply to column names and return modified data.frame
    names(x) <- .clean_vector(names(x), max_length, prefix, sep)
    return(x)

  } else if (is.character(x)) {
    # Apply to character vector and return cleaned vector
    return(.clean_vector(x, max_length, prefix, sep))

  } else {
    stop("`x` must be either a character vector or a data.frame", call. = FALSE)
  }
}
