#' @title Ensure Integrity by Extracting and Validating Pay Period Totals
#' @description I believe that for data to be trustworthy, it must be correct at
#'   both the detail and summary level. This function is designed to extract and
#'   validate pay period totals (e.g., total weekly hours), a critical component
#'   for ensuring fair payroll and accurate compliance reporting. It can operate
#'   in two modes to accommodate different data structures.
#'
#' @details
#' My dream is a system that not only extracts data but also intelligently
#' validates it. This function embodies that principle by finding a single,
#' authoritative total for a given pay period.
#'
#' \strong{Grouped Mode (`use_period_grouping = TRUE`):} This is the preferred
#' mode for ensuring data integrity.
#' \itemize{
#'   \item It groups all records by a unique period identifier (`employee_period_id`),
#'     treating the entire period as a single analytical unit.
#'   \item Within each group, it searches for all occurrences of the total value,
#'     as defined by the sanctioned `total_pattern`.
#'   \item It then enforces a "single source of truth" rule:
#'     \enumerate{
#'       \item If exactly one total is found, it is confidently assigned to all
#'         records within the period. This is the ideal outcome.
#'       \item If no total is found, it flags a potential data gap, ensuring that
#'         the omission is not overlooked.
#'       \item If multiple, conflicting totals are found, it flags a significant
#'         data quality issue and withholds the total, preventing the propagation
#'         of erroneous data.
#'     }
#' }
#' \strong{Row-wise Mode (`use_period_grouping = FALSE`):} A simpler mode for
#' when data is not structured by period.
#' \itemize{
#'   \item It applies the `total_pattern` to each row individually, providing a
#'     more direct but less contextually validated extraction.
#' }
#'
#' @param data A data frame containing the timesheet records. It must include
#'   `concatenated_text` and a `data_quality_flags` list-column to track our
#'   validation efforts.
#' @param config A list containing the sanctioned configuration for the data
#'   cohort, including the `total_type` and `total_pattern`.
#' @param use_period_grouping A logical flag. When `TRUE` (the default), it
#'   activates the more robust, period-based validation logic.
#' @param file_id_col An optional, unquoted column name to enhance the uniqueness
#'   of the period grouping, for cases where the `employee_period_id` may not
#'   be unique across different files.
#'
#' @return The input data frame, now enriched with a new column for the validated
#'   period total (e.g., `extracted_weekly_total_hours`) and an updated
#'   `data_quality_flags` column that provides a transparent audit trail of the
#'   validation process.
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
