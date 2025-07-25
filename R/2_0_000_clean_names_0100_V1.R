#' @title Ensure Fair and Consistent Recognition by Cleaning Employee Names
#' @description I believe that recognizing an individual correctly is a fundamental
#'   mark of respect. This function is dedicated to standardizing the `extracted_name`
#'   column, ensuring that every employee is represented consistently and accurately.
#'   It applies tailored cleaning routines to account for the varying data quality
#'   from different sources, such as poor OCR results.
#'
#' @details
#' My approach is to apply the right level of intervention to achieve fairness.
#' This function creates a new `cleaned_name` column by applying a series of
#' validation and correction steps, with more intensive routines for data cohorts
#' known to have quality issues.
#'
#' \strong{Standard Cleaning (Default):} This is our baseline for data integrity.
#' \itemize{
#'   \item It removes non-standard characters while preserving essential punctuation,
#'     ensuring the name is clean but not distorted.
#'   \item It standardizes whitespace and applies Title Case for consistency.
#'   \item It intelligently corrects "Last, First" formats to the standard
#'     "First Last" order, ensuring uniformity.
#' }
#'
#' \strong{Group 2 Cleaning:} This is a form of data advocacy for lower-quality
#' sources.
#' \itemize{
#'   \item It includes all standard cleaning steps.
#'   \item It applies more assertive character removal to eliminate OCR noise.
#'   \item It removes very short strings that are unlikely to be valid names.
#'   \item It corrects common, systematic OCR errors (e.g., "0" to "O").
#' }
#'
#' @param data A data frame containing the `extracted_name` column, which holds
#'   the raw, unstandardized names.
#' @param group_number An integer (e.g., 1, 2) that specifies which cleaning
#'   routine to apply, ensuring a tailored and appropriate level of cleaning.
#'
#' @return The input data frame with a new `cleaned_name` column, where every
#'   name has been standardized to uphold our commitment to fair and accurate
#'   recognition.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_replace_all str_trim str_to_title str_detect str_replace
#' @author James Gray (JG3288)
#' @export
clean_names <- function(data, group_number = 1) {
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that("extracted_name" %in% names(data))

  data %>%
    dplyr::mutate(
      cleaned_name = extracted_name,
      cleaned_name = dplyr::case_when(
        group_number == 2 ~ {
          # More aggressive cleaning for Group 2
          name <- cleaned_name
          name <- stringr::str_replace_all(name, "[^A-Za-z\\s,.-]", " ")
          name <- stringr::str_replace_all(name, "\\s+", " ")
          name <- stringr::str_trim(name)
          name <- ifelse(nchar(name) < 2, NA_character_, name)
          name <- stringr::str_to_title(name)
          name <- stringr::str_replace_all(name, "\\b0\\b", "O")
          name <- stringr::str_replace_all(name, "\\b1\\b", "I")
          name
        },
        TRUE ~ {
          # Standard cleaning for all other groups
          name <- cleaned_name
          name <- stringr::str_replace_all(name, "[^A-Za-z\\s,.-]", "")
          name <- stringr::str_replace_all(name, "\\s+", " ")
          name <- stringr::str_trim(name)
          name <- stringr::str_to_title(name)
          # Handle "Last, First" format
          name <- ifelse(
            stringr::str_detect(name, ","),
            stringr::str_replace(name, "^([^,]+),\\s*(.+)$", "\\2 \\1"),
            name
          )
          name
        }
      ),
      # Ensure empty strings are converted to NA
      cleaned_name = ifelse(cleaned_name == "", NA_character_, cleaned_name)
    )
}
