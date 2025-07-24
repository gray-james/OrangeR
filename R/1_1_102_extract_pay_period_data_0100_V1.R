#' @title Extract Pay Period Totals
#' @description Extracts total values (e.g., total weekly hours) from timesheet
#'   data. The function can operate in two modes: grouping by a period identifier
#'   to find one total for the entire period, or operating row-by-row.
#'
#' @details
#' This function is designed to find and extract a single total value that
#' applies to a defined period (e.g., a pay week or fortnight).
#'
#' \strong{Grouped Mode} (`use_period_grouping = TRUE`):
#' \itemize{
#'   \item The data is grouped by `employee_period_id` (and optionally a file ID).
#'   \item Within each group, it searches all `concatenated_text` entries for a
#'     match to the `total_pattern`.
#'   \item It handles three cases:
#'     \enumerate{
#'       \item Exactly one total is found: It's assigned to all rows in the group.
#'       \item No total is found: A "No total found" flag is added.
#'       \item Multiple totals are found: A "Multiple totals found" flag is
#'         added, and the total is left as `NA` to signal a data quality issue.
#'     }
#' }
#' \strong{Row-wise Mode} (`use_period_grouping = FALSE`):
#' \itemize{
#'   \item It simply applies the `total_pattern` to each row's
#'     `concatenated_text` individually.
#' }
#'
#' @param data A data frame containing timesheet data. Must include
#'   `concatenated_text` and a `data_quality_flags` list-column.
#' @param config A list containing the configuration for the group, including
#'   `total_type` and `total_pattern`.
#' @param use_period_grouping Logical. If `TRUE` (default), groups data by
#'   `employee_period_id` to find a single total for the period.
#' @param file_id_col Optional. The bare (unquoted) name of a column to add to
#'   the grouping, for cases where `employee_period_id` is not unique across files.
#'
#' @return The input data frame with a new column for the extracted total (e.g.,
#'   `extracted_weekly_total_hours`) and an updated `data_quality_flags` column.
#'
#' @importFrom dplyr group_by mutate ungroup first case_when cur_data
#' @importFrom rlang enquo as_label syms
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @author James Gray (JG3288)
#' @export
extract_period_data <- function(data, config, use_period_grouping = TRUE, file_id_col = NULL) {
  # --- Input Validation ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that("concatenated_text" %in% names(data),
    msg = "Input data frame must contain a 'concatenated_text' column."
  )
  assertthat::assert_that("data_quality_flags" %in% names(data) && is.list(data$data_quality_flags),
    msg = "Input data frame must contain a 'data_quality_flags' list-column."
  )
  assertthat::assert_that(assertthat::is.list(config),
    msg = "config must be a list."
  )
  assertthat::assert_that("total_type" %in% names(config),
    msg = "config must contain 'total_type'."
  )
  assertthat::assert_that("total_pattern" %in% names(config),
    msg = "config must contain 'total_pattern'."
  )
  assertthat::assert_that(assertthat::is.logical(use_period_grouping), length(use_period_grouping) == 1)

  if (use_period_grouping) {
    assertthat::assert_that("employee_period_id" %in% names(data),
      msg = "When use_period_grouping is TRUE, 'employee_period_id' column is required."
    )
    if (!is.null(file_id_col)) {
      assertthat::assert_that(rlang::as_label(rlang::enquo(file_id_col)) %in% names(data),
        msg = paste0("Column ", rlang::as_label(rlang::enquo(file_id_col)), " not found in data.")
      )
    }
  }

  total_col_name <- paste0("extracted_", config$total_type, "_total_hours")

  if (use_period_grouping) {
    grouping_vars <- rlang::syms(c("employee_period_id", rlang::as_label(rlang::enquo(file_id_col))))

    data <- data %>%
      dplyr::group_by(!!!grouping_vars) %>%
      dplyr::mutate({
        # Find all total matches within the entire period
        all_matches <- unlist(stringr::str_extract_all(concatenated_text, config$total_pattern))
        all_matches <- as.numeric(all_matches[!is.na(all_matches)])

        # Determine the single total for the period
        period_total <- if (length(all_matches) == 1) all_matches else NA_real_

        # Create the new total column and data quality flags in one step
        tibble::tibble(
          !!total_col_name := period_total,
          data_quality_flags = dplyr::case_when(
            length(all_matches) > 1 ~
              purrr::map(data_quality_flags, ~ c(., "Multiple totals found for pay period")),
            length(all_matches) == 0 ~
              purrr::map(data_quality_flags, ~ c(., "No total found for pay period")),
            TRUE ~ data_quality_flags
          )
        )
      }) %>%
      dplyr::ungroup()
  } else {
    # Row-wise extraction is simpler
    data <- data %>%
      dplyr::mutate(
        !!total_col_name := as.numeric(stringr::str_extract(concatenated_text, config$total_pattern))
      )
  }

  return(data)
}
