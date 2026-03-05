#' Clean variable names to comply with Epi Info standards
#'
#' This function transforms variable names to meet Epi Info requirements:
#' lowercase, no accents, underscores as separators, starts with a letter,
#' max length constraint, and unique names.
#'
#' @param names_vector A character vector of variable names to clean
#' @param max_length Maximum length of variable names (default: 64)
#' @param prefix Prefix to add if name doesn't start with a letter (default: "v_")
#' @param sep Separator used by \code{make.unique} for duplicate names (default: "_")
#' @return A character vector of cleaned variable names
#' @export
#' @examples
#' clean_names(c("Nom Patient", "Âge/Ans", "Température (°C)"))
#' # [1] "nom_patient"     "age_ou_ans"      "temperature_c"
clean_names <- function(names_vector, 
                                         max_length = 64,
                                         prefix = "v_",
                                         sep = "_") {
  
  # ---- Input validation ----
  if (!is.character(names_vector)) {
    stop("`names_vector` must be a character vector", call. = FALSE)
  }
  
  # Handle empty input
  if (length(names_vector) == 0) {
    return(character(0))
  }
  
  # ---- Cleaning pipeline ----
  
  # 1. Lowercase + trim initial whitespace
  cleaned <- trimws(tolower(names_vector))
  
  # 2. Replace slashes with " ou " (with surrounding spaces for word separation)
  cleaned <- gsub("[/\\\\]", " ou ", cleaned, perl = TRUE)
  
  # 3. Remove accents via ASCII transliteration (sub = "" drops untranslatable chars)
  cleaned <- iconv(cleaned, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  # 4. Replace all non-alphanumeric/non-space chars with space (single regex pass)
  cleaned <- gsub("[^a-z0-9\\s]", " ", cleaned, perl = TRUE)
  
  # 5. Collapse multiple whitespace into single space
  cleaned <- gsub("\\s+", " ", cleaned, perl = TRUE)
  
  # 6. Trim again after substitutions
  cleaned <- trimws(cleaned)
  
  # 7. Replace spaces with underscore (fixed = TRUE for speed on literal match)
  cleaned <- gsub(" ", "_", cleaned, fixed = TRUE)
  
  # 8. Remove leading/trailing underscores that may have resulted
  cleaned <- gsub("^_+|_+$", "", cleaned, perl = TRUE)
  
  # 9. Ensure names start with a letter (vectorized indexing > ifelse for performance)
  starts_with_letter <- grepl("^[a-z]", cleaned, perl = TRUE)
  cleaned[!starts_with_letter] <- paste0(prefix, cleaned[!starts_with_letter])
  
  # 10. Truncate to maximum length
  cleaned <- substr(cleaned, 1, max_length)
  
  # 11. Make duplicate names unique
  cleaned <- make.unique(cleaned, sep = sep)
  
  return(cleaned)
}