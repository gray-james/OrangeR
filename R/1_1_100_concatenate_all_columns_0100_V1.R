# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONCATENATE COLUMNS (VECTORIZED) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Unify Disparate Text Columns into a Single, Coherent String
#' @description I believe that to understand the whole story, you have to see all
#'   the pieces together. This function performs a critical pre-processing step
#'   by collapsing multiple, fragmented text columns (`Col1`, `Col2`, etc.) into
#'   a single, unified `concatenated_text` column. This act of unification is
#'   essential for enabling accurate and holistic pattern matching across the
#'   entirety of a record.
#'
#' @details
#' My approach is to create a single, coherent narrative for each row, which
#' allows for more reliable data extraction. The function operates as follows:
#' \enumerate{
#'   \item It identifies all fragmented text columns that begin with "Col".
#'   \item It then methodically processes each row, gathering all non-empty
#'     fragments.
#'   \item For each row, it pastes these fragments together into a single,
#'     space-separated string, creating the `concatenated_text` column.
#'   \item It also assigns a unique `row_id`, ensuring that every record has a
#'     stable and auditable identifier throughout the cleaning process.
#' }
#' This process transforms a fragmented view into a holistic one, which is
#' fundamental to achieving data integrity.
#'
#' @param data A data frame containing the disparate text columns that need to
#'   be unified into a single, coherent record.
#'
#' @return The input data frame, now enriched with two new columns:
#'   `concatenated_text`, which provides a unified view of the record, and
#'   `row_id`, which ensures its traceability.
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
