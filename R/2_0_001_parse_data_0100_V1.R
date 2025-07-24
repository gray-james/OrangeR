#' @title Parse and Finalize Data Types
#' @description This function takes the raw extracted data and converts it into
#'   standard R data types. It handles the parsing of dates and times from text,
#'   converts work hours to numeric, and calculates the duration of work shifts.
#'
#' @details
#' The function performs several key transformations within a `dplyr` pipeline:
#' \enumerate{
#'   \item \strong{Date Parsing}: It uses `lubridate::parse_date_time` to
#'     robustly parse `extracted_date` strings, trying multiple common formats.
#'   \item \strong{Time Parsing}: It combines the `extracted_clock_in` and
#'     `extracted_clock_out` times with the newly parsed date and uses
#'     `lubridate::parse_date_time` to create full `POSIXct` objects. It
#'     handles both 12-hour (AM/PM) and 24-hour time formats.
#'   \item \strong{Next-Day Correction}: It intelligently handles overnight shifts
#'     by checking if the clock-out time is earlier than the clock-in time. If
#'     so, it adds one day to the clock-out time.
#'   \item \strong{Duration Calculation}: It computes the duration of the work
#'     shift in hours by taking the difference between the corrected clock-out
#'     and clock-in times.
#'   \item \strong{Type Conversion}: It converts `work_time` to numeric.
#' }
#' Rows that do not have a valid clock-in time after parsing are filtered out.
#'
#' @param data A data frame containing the raw extracted columns (e.g.,
#'   `extracted_date`, `extracted_clock_in`, `extracted_clock_out`, `work_time`).
#'
#' @return A data frame with new, cleaned columns (`cleaned_date`,
#'   `cleaned_clock_in_time`, `cleaned_clock_out_time`, `cleaned_work_hours`,
#'   `calculated_duration`) and standardized data types.
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
