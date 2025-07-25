# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# generate_period_id ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Impose Logical Structure by Generating Employee Pay Period IDs
#' @description I believe that true data integrity comes from understanding the
#'   underlying structure of the information, even when it is not explicitly
#'   stated. This function is the core of our data segmentation module. It is
#'   designed to intelligently analyze vertically-structured timesheet data and
#'   perform three critical tasks:
#' 1.  **Identify Employee Blocks:** It methodically detects contiguous blocks of
#'     records belonging to a single employee.
#' 2.  **Assign Auditable IDs:** It assigns a consistent and sequential
#'     `employee_period_id` to every record within an identified block.
#' 3.  **Validate Period Integrity:** It cross-references the date range of each
#'     block against the expected `period_days`, flagging any deviations as
#'     potential data quality issues.
#'
#' @section Core Logic:
#' My approach is to treat the data like an archaeological site, using the
#' physical layout of the source file as our ground truth. The function first
#' establishes the authoritative order of records using `sort_order_col`. Within
#' this order, it deduces the start of a new employee's timesheet by detecting
#' a "reset" in the date sequence. This allows us to impose a logical structure
#' that honors the original data's context.
#'
#' @author James Gray (JG3288)
#' @family Data Cleaning Functions
#'
#' @param df A data frame containing the timesheet records. It must have the
#'   `data_quality_flags` list-column initialized to track our validation efforts.
#' @param group_col The bare (unquoted) name of the column identifying the source
#'   file or data group (e.g., `file_id`). This ensures processing is correctly
#'   partitioned.
#' @param date_col The bare (unquoted) name of the column containing the
#'   authoritative timesheet dates (must be of class `Date`).
#' @param period_days An integer specifying the expected number of days in a pay
#'   period (e.g., 14). This serves as the benchmark for our data quality validation.
#' @param sort_order_col The bare (unquoted) name of a numeric column that defines
#'   the true, physical row order before any logical sorting is applied.
#'
#' @return The input data frame, now with two new columns that enforce structure
#'   and transparency:
#'   - `employee_period_id`: An integer ID that ensures all records within a
#'     pay period are logically linked.
#'   - `data_quality_flags`: An updated list-column with appended warnings for
#'     any period that violates our integrity checks, providing a clear audit trail.
#'
#' @importFrom dplyr arrange group_by mutate lag coalesce first if_else ungroup select
#' @importFrom tidyr fill
#' @importFrom purrr map
#' @importFrom assertthat assert_that is.data.frame is.numeric
#' @export
#'
#' @examples
#' # My dream is to bring clarity to even the most unstructured data.
#' # Here, we create sample data that mimics the vertical layout of a PDF.
#' sample_data <- data.frame(
#'   file_id = "file_A.pdf",
#'   line_num = 1:11,
#'   cleaned_date = as.Date(c(
#'     "2024-07-01", "2024-07-02", "2024-07-04", # Employee 1
#'     "2024-07-03", "2024-07-05", "2024-07-06", # Employee 2
#'     "2024-07-20", "2024-07-21", "2024-07-22", # Employee 3 (violates period)
#'     "2024-07-08", # Employee 4
#'     NA
#'   )),
#'   # We must initialize a list-column to hold our data quality findings.
#'   data_quality_flags = I(vector("list", 11))
#' )
#'
#' # This function will now identify the four distinct employee blocks
#' # and flag the third one for having an unusually long date range.
#' result <- generate_period_id(
#'   df = sample_data,
#'   group_col = file_id,
#'   date_col = cleaned_date,
#'   period_days = 14,
#'   sort_order_col = line_num
#' )
#'
#' print(result)
# ------------------------------------------------------------------------------
generate_period_id <- function(df, group_col, date_col, period_days, sort_order_col) {
  # --- Input Validation ---
  # Note: No assertion needed for bare column names, {{}} handles it.
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that("data_quality_flags" %in% names(df) && is.list(df$data_quality_flags))
  assertthat::assert_that(is.numeric(period_days), period_days > 0)

  # --- Main Logic ---
  result_df <- df %>%
    dplyr::arrange({{ group_col }}, {{ sort_order_col }}) %>%
    dplyr::group_by({{ group_col }}) %>%
    # Step 1: Identify the start of a new employee block
    dplyr::mutate(
      is_new_block_start = dplyr::coalesce({{ date_col }} < dplyr::lag({{ date_col }}), TRUE),
      temp_block_id = cumsum(is_new_block_start)
    ) %>%
    # Step 2: Fill the ID across the entire block
    dplyr::group_by({{ group_col }}, temp_block_id) %>%
    tidyr::fill({{ date_col }}, .direction = "downup") %>%
    dplyr::mutate(
      employee_period_id = dplyr::first(temp_block_id)
    ) %>%
    # Step 3: Validate the date range of the block
    dplyr::mutate(
      block_date_range = as.numeric(max({{ date_col }}, na.rm = TRUE) - min({{ date_col }}, na.rm = TRUE)),
      data_quality_flags = dplyr::if_else(
        block_date_range > period_days,
        purrr::map(data_quality_flags, ~ c(., paste("Period length of", block_date_range, "days exceeds expected", period_days, "days."))),
        data_quality_flags
      )
    ) %>%
    # Ungroup and clean up intermediate columns
    dplyr::ungroup() %>%
    dplyr::select(-is_new_block_start, -temp_block_id, -block_date_range) %>%
    dplyr::arrange({{ sort_order_col }})

  return(result_df)
}
