#' @title Clean Extracted Employee Names
#' @description This function standardizes the `extracted_name` column by
#'   applying a series of cleaning operations. It can apply different, more
#'   aggressive cleaning rules for specific data groups that are known to have
#'   poorer quality OCR results.
#'
#' @details
#' The function creates a new `cleaned_name` column and performs the following
#' steps, with variations based on the `group_number`:
#'
#' \strong{Standard Cleaning (Default):}
#' \itemize{
#'   \item Removes non-standard characters, leaving only letters, spaces, commas,
#'     dots, and dashes.
#'   \item Collapses multiple whitespace characters into a single space.
#'   \item Trims leading/trailing whitespace.
#'   \item Converts names to Title Case (e.g., "james gray" -> "James Gray").
#'   \item Corrects "Last, First" format to "First Last".
#' }
#'
#' \strong{Group 2 Cleaning:}
#' \itemize{
#'   \item Includes all standard cleaning steps.
#'   \item Applies more aggressive character removal.
#'   \item Removes very short strings (likely OCR errors).
#'   \item Corrects common OCR mistakes (e.g., "0" -> "O", "1" -> "I").
#' }
#'
#' @param data A data frame containing the `extracted_name` column.
#' @param group_number An integer (e.g., 1, 2) specifying which cleaning
#'   routine to use. Defaults to 1.
#'
#' @return The input data frame with a new `cleaned_name` column.
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
