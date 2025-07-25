# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STEP 3: DATE SPILLOVER ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Restore Integrity by Correcting Date Spillover Errors
#' @description I believe that data, like a contract, must be whole to be valid.
#'   This function addresses a critical data quality issue where date and time
#'   information, fractured during extraction, "spills" onto a subsequent line.
#'   It is designed to methodically identify and correct these errors, ensuring
#'   that every record accurately reflects the intended information.
#'
#' @details
#' A "date spillover" is more than a formatting error; it's a potential source
#' of unfairness, where an employee's record could be misinterpreted. My approach
#' to fixing this is both precise and transparent:
#' \enumerate{
#'   \item It identifies rows that are candidates for spillover: those with an
#'     employee's name but a missing date.
#'   \item It then inspects the subsequent row, using the sanctioned date patterns
#'     from the group's configuration to confirm if it contains the missing data.
#'   \item Upon confirmation, it "pulls up" the fractured data—the date, clock-in/out
#'     times, and work hours—reuniting it with the correct employee record.
#'   \item Finally, it flags the now-redundant subsequent row for removal by marking
#'     a `spillover_row` column as `TRUE`, providing a clear, auditable trail of
#'     the correction.
#' }
#' This vectorized implementation is a core part of my commitment to building
#' not just accurate, but also efficient and scalable data systems.
#'
#' @param data A data frame containing the parsed timesheet data, which may
#'   suffer from spillover-induced integrity issues. It must include the columns
#'   `extracted_name`, `extracted_date`, and `concatenated_text`.
#' @param config A list containing the sanctioned configuration for the current
#'   data cohort, retrieved from `get_group_config()`. It provides the authoritative
#'   patterns for dates, times, and work hours.
#'
#' @return The data frame with its integrity restored. Dates and times are
#'   corrected, and a new logical column, `spillover_row`, is added to transparently
#'   indicate which rows were corrected.
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
