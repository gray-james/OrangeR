# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GROUP SELECT + COMBINATION FUNCTION ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Unify Disparate Data Cohorts into a Single, Cohesive Dataset
#' @description I believe that a holistic view is essential for fair analysis.
#'   This function performs the critical act of unification, taking a list of
#'   data frames—each representing a distinct, processed data cohort—and combining
#'   them into a single, standardized dataset. This step is the foundation for
#'   all subsequent high-level analysis and reporting.
#'
#' @details
#' My approach is to ensure that once data is cleaned, it is brought into a
#' common, auditable schema. This function achieves that through two key actions:
#' \enumerate{
#'   \item It uses `dplyr::bind_rows` to stack the data frames, creating a single,
#'     vertically integrated dataset. Crucially, it uses the `.id` argument to
#'     create a `file_id` column, ensuring that every row retains a transparent
#'     link to its original data cohort.
#'   \item It provides the option to select a specific set of columns, allowing
#'     us to craft a final, analysis-ready schema that is free of intermediate
#'     or raw columns. This ensures a clean, focused, and consistent final output.
#' }
#' Finally, it assigns a `combined_row_id` to provide a unique and stable
#' identifier for every record in the fully unified dataset.
#'
#' @param group_list A named list of data frames, where each data frame represents
#'   a fully processed data cohort. The names of the list elements are used to
#'   preserve the provenance of each record.
#' @param columns_to_keep An optional character vector specifying the exact columns
#'   to be included in the final, standardized output. If `NULL` (the default),
#'   all columns will be retained.
#'
#' @return A single, unified data frame that represents the entire dataset, with
#'   a `file_id` column to ensure traceability and a `combined_row_id` for unique
#'   identification. This is the final, analysis-ready source of truth.
#'
#' @importFrom dplyr bind_rows select all_of
#' @importFrom tidyselect everything
#' @author James Gray (JG3288)
#' @export
combine_and_select_data <- function(group_list, columns_to_keep = NULL) {
  assertthat::assert_that(is.list(group_list))

  # Combine all data frames in the list into a single tibble.
  # The .id argument creates a column from the names of the list elements.
  combined_data <- dplyr::bind_rows(group_list, .id = "file_id")

  # Select specific columns if requested
  if (!is.null(columns_to_keep)) {
    assertthat::assert_that(is.character(columns_to_keep))
    # Ensure all requested columns exist to prevent errors
    columns_to_keep <- intersect(columns_to_keep, names(combined_data))
    combined_data <- combined_data %>%
      dplyr::select(tidyselect::all_of(columns_to_keep))
  }

  # Add a unique row ID to the final combined data
  combined_data$combined_row_id <- seq_len(nrow(combined_data))

  return(combined_data)
}
