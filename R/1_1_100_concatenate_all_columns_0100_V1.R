# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONCATENATE COLUMNS (VECTORIZED) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Concatenate Text Columns into a Single String
#' @description This function takes a data frame with multiple text columns
#'   (typically named `Col1`, `Col2`, etc.) and collapses them into a single
#'   `concatenated_text` column for each row. This is a crucial pre-processing
#'   step that simplifies pattern matching, as it allows regex to be applied to
#'   the entire text of a line at once.
#'
#' @details
#' The function operates as follows:
#' \enumerate{
#'   \item It identifies all columns that start with "Col".
#'   \item Using `dplyr::rowwise`, it iterates through each row.
#'   \item For each row, it takes all `Col` values, removes any that are `NA`
#'     or empty strings, and then pastes the remaining values together, separated
#'     by a single space.
#'   \item It also adds a unique `row_id` to each row for stable indexing.
#' }
#' This approach is robust to rows with varying numbers of non-empty columns.
#'
#' @param data A data frame or tibble containing the columns to be concatenated.
#'
#' @return The input data frame with two new columns: `concatenated_text` (the
#'   combined string) and `row_id` (a unique integer for each row).
#'
#' @importFrom dplyr rowwise mutate c_across ungroup
#' @importFrom stringr str_c
#' @importFrom tidyselect starts_with
#' @author James Gray (JG3288)
#' @export
concatenate_columns <- function(data) {
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      concatenated_text = {
        # Select columns, remove NAs, and then concatenate
        row_values <- dplyr::c_across(tidyselect::starts_with("Col"))
        row_values <- row_values[!is.na(row_values)]
        stringr::str_c(row_values, collapse = " ")
      },
      # Ensure multiple spaces are collapsed into one
      concatenated_text = gsub("\\s+", " ", concatenated_text)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = seq_len(nrow(.)))

  return(data)
}
