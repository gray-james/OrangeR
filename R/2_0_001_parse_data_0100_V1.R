#' @title Finalize Data Integrity by Parsing and Standardizing Data Types
#' @description I believe that for data to be truly useful, it must speak the
#'   language of computation. This function performs the critical transformation of
#'   raw extracted text into standardized, reliable R data types. It is here that
#'   we instill the final layer of structure, handling the complexities of dates,
#'   times, and durations to ensure the data is ready for rigorous analysis.
#'
#' @details
#' My dream is a system where every data point is not just present, but precise.
#' This function realizes that dream through a series of key transformations:
#' \enumerate{
#'   \item \strong{Date Parsing}: It leverages `lubridate::parse_date_time` to
#'     robustly interpret various date formats found in the `extracted_date` strings,
#'     ensuring consistency regardless of the source format.
#'   \item \strong{Time Parsing}: It creates full `POSIXct` datetime objects by
#'     combining the extracted clock-in/out times with their corresponding dates.
#'     This process intelligently handles both 12-hour and 24-hour time formats.
#'   \item \strong{Overnight Shift Correction}: It fairly and accurately handles
#'     overnight shifts by detecting when a clock-out time is earlier than a
#'     clock-in time and correctly advancing the day.
#'   \item \strong{Duration Calculation}: It computes the precise duration of each
#'     work shift in hours, providing a reliable basis for payroll calculations.
#'   \item \strong{Type Conversion}: It ensures that recorded work hours are
#'     converted to a numeric format for computational use.
#' }
#' As a final integrity check, rows that lack a valid clock-in time after this
#' rigorous parsing are filtered out, as they cannot contribute to a fair analysis.
#'
#' @param data A data frame containing the raw, text-based extracted columns
#'   (e.g., `extracted_date`, `extracted_clock_in`, `extracted_clock_out`).
#'
#' @return A data frame with new, fully standardized columns (`cleaned_date`,
#'   `cleaned_clock_in_time`, `cleaned_clock_out_time`, `cleaned_work_hours`,
#'   `calculated_duration`). The data is now ready for high-integrity analysis.
#'
#' @importFrom dplyr mutate filter case_when
#' @importFrom lubridate parse_date_time days
#' @author James Gray (JG3288)
#' @export
parse_data <- function(data) {
  assertthat::assert_that(is.data.frame(data))

  data %>%
    dplyr::mutate(
      # 1. Parse dates using flexible format orders
      # "aymd" = "04-JAN-2023", "dby" = "04-Jan-2023"
      cleaned_date = lubridate::parse_date_time(
        extracted_date,
        orders = c("dby", "dmy", "ymd", "mdy"),
        quiet = TRUE
      ),

      # 2. Parse times by combining with the cleaned date
      # "I:M:S p" = 12-hour AM/PM, "H:M" = 24-hour
      cleaned_clock_in_time = lubridate::parse_date_time(
        paste(cleaned_date, extracted_clock_in),
        orders = c("Y-m-d I:M:S p", "Y-m-d H:M"),
        quiet = TRUE
      ),
      cleaned_clock_out_time = lubridate::parse_date_time(
        paste(cleaned_date, extracted_clock_out),
        orders = c("Y-m-d I:M:S p", "Y-m-d H:M"),
        quiet = TRUE
      ),

      # 3. Correct for overnight shifts where clock-out is on the next day
      cleaned_clock_out_time = dplyr::case_when(
        !is.na(cleaned_clock_out_time) & !is.na(cleaned_clock_in_time) & cleaned_clock_out_time < cleaned_clock_in_time ~
          cleaned_clock_out_time + lubridate::days(1),
        TRUE ~ cleaned_clock_out_time
      ),

      # 4. Calculate duration in hours
      calculated_duration = ifelse(
        !is.na(cleaned_clock_in_time) & !is.na(cleaned_clock_out_time),
        as.numeric(difftime(cleaned_clock_out_time, cleaned_clock_in_time, units = "hours")),
        NA_real_
      ),

      # 5. Convert work time text to numeric
      cleaned_work_hours = as.numeric(work_time)
    ) %>%
    # Remove rows where a clock-in time could not be parsed, as they are not useful
    dplyr::filter(!is.na(cleaned_clock_in_time))
}
