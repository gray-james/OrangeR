# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 3: DATE SPILLOVER ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Correct Date and Time Spillover Errors in Parsed Data
#' @description This function identifies and corrects a common data extraction
#'   issue where date and time information for an employee incorrectly "spills"
#'   onto the subsequent line. It looks for rows that contain an extracted name
#'   but are missing a date, and then inspects the next row to see if it
#'   contains the missing information.
#'
#' @details
#' A "date spillover" occurs when a row with an employee's name is followed
#' immediately by a row that starts with a date. This function handles this by:
#' \enumerate{
#'   \item Identifying potential spillover rows using `dplyr::lag`.
#'   \item Checking if the next row's text starts with a valid date pattern from
#'     the provided configuration.
#'   \item If a spillover is confirmed, it "pulls up" the date, clock-in/out
#'     times, and work hours from the subsequent row into the current row.
#'   \item It then flags the subsequent row for removal by marking a
#'     `spillover_row` column as `TRUE`.
#' }
#' This vectorized approach using `dplyr` is significantly more efficient than
#' a row-by-row loop for large datasets.
#'
#' @param data A data frame or tibble containing the parsed timesheet data. It
#'   must include the columns `extracted_name`, `extracted_date`, and
#'   `concatenated_text`.
#' @param config A list of configuration settings for the current group,
#'   retrieved from `get_group_config()`. It must contain `date_patterns`,
#'   `time_patterns`, and `work_time_pattern`.
#'
#' @return The input data frame with corrected dates, times, and a new logical
#'   column `spillover_row` indicating which rows should be filtered out later.
#'
#' @importFrom dplyr mutate lag lead if_else case_when
#' @importFrom stringr str_extract str_extract_all str_trim
#' @importFrom purrr map_chr map
#' @author James Gray (JG3288)
#' @export
fix_date_spillovers <- function(data, config) {
  if (nrow(data) < 2) {
    return(data) # Not enough rows for a spillover to occur
  }

  date_pattern <- config$date_patterns[1]
  time_patterns <- config$time_patterns
  work_time_pattern <- config$work_time_pattern

  data <- data %>%
    dplyr::mutate(
      # Identify potential spillover rows: current row has a name but no date,
      # and the next row's text is available.
      is_spillover_candidate = !is.na(extracted_name) &
        is.na(extracted_date) &
        !is.na(dplyr::lead(concatenated_text)),

      # Check if the next row actually starts with a date
      next_row_starts_with_date = dplyr::if_else(
        is_spillover_candidate,
        grepl(paste0("^\\s*", date_pattern), dplyr::lead(concatenated_text)),
        FALSE
      ),

      # --- Data Correction ---
      # If it's a confirmed spillover, pull data up from the next row.

      # 1. Extract and pull up the date
      extracted_date = dplyr::if_else(
        next_row_starts_with_date,
        stringr::str_trim(stringr::str_extract(dplyr::lead(concatenated_text), date_pattern)),
        extracted_date
      ),

      # 2. Extract and pull up clock-in/out times
      temp_times = dplyr::if_else(
        next_row_starts_with_date,
        purrr::map(dplyr::lead(concatenated_text), ~ stringr::str_extract_all(.x, time_patterns) %>% unlist()),
        list(NULL)
      ),
      extracted_clock_in = dplyr::if_else(
        next_row_starts_with_date,
        purrr::map_chr(temp_times, ~ .x[1]),
        extracted_clock_in
      ),
      extracted_clock_out = dplyr::if_else(
        next_row_starts_with_date,
        purrr::map_chr(temp_times, ~ .x[2]),
        extracted_clock_out
      ),

      # 3. Extract and pull up work time
      work_time = dplyr::if_else(
        next_row_starts_with_date,
        stringr::str_extract(dplyr::lead(concatenated_text), work_time_pattern),
        work_time
      ),

      # 4. Flag the row *below* the spillover for later removal
      spillover_row = dplyr::lag(next_row_starts_with_date, default = FALSE)
    ) %>%
    # Clean up temporary columns
    dplyr::select(-is_spillover_candidate, -next_row_starts_with_date, -temp_times)

  return(data)
}
