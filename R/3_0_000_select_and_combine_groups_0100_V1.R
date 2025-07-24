# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GROUP SELECT + COMBINATION FUNCTION ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Combine and Select Data from Multiple Groups
#' @description This function takes a list of data frames (one for each processed
#'   group) and combines them into a single, unified data frame. It also provides
#'   an option to select a specific set of columns, ensuring the final output
#'   has a consistent, standardized schema.
#'
#' @details
#' The function first uses `dplyr::bind_rows` to stack the data frames from the
#' list. The `.id` argument is used to create a new column (`file_id`) that
#' identifies which group each row originated from.
#'
#' If a character vector of column names is provided to `columns_to_keep`, the
#' function will select only those columns. This is useful for creating a final,
#' clean output without intermediate or raw columns. If `columns_to_keep` is
#' `NULL`, all columns will be retained.
#'
#' Finally, it adds a `combined_row_id` to provide a unique identifier for each
#' row in the final combined dataset.
#'
#' @param group_list A named list of data frames. The names of the list elements
#'   will be used to create the `file_id` column.
#' @param columns_to_keep An optional character vector of column names to retain
#'   in the final combined data frame. If `NULL` (the default), all columns
#'   are kept.
#'
#' @return A single, combined data frame with a `file_id` column indicating the
#'   original group and a unique `combined_row_id`.
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
