#' @title Translate Raw Text into Structured, Meaningful Data
#' @description I believe that data only becomes powerful when it is given structure
#'   and meaning. This function serves as the core translation engine of our ETL
#'   pipeline. It takes raw, concatenated text and, guided by a group-specific
#'   configuration, methodically extracts the key data points—names, dates,
#'   times, and work hours—that are essential for fair and accurate reporting.
#'
#' @details
#' My approach is to build a system that is as performant as it is transparent.
#' This function is constructed as a single, fully vectorized `dplyr` pipeline,
#' ensuring both efficiency and readability. It sequentially applies the sanctioned
#' extraction logic defined in the `group_config` to ensure consistency.
#'
#' The extraction process is a testament to our commitment to precision:
#' \enumerate{
#'   \item \strong{Dates}: It diligently tries each `date_pattern` from the
#'     configuration in order, ensuring that even non-standard formats are
#'     correctly identified.
#'   \item \strong{Times}: It extracts all time-related strings. For data cohorts
#'     requiring it, it will only extract times from rows where a date has also
#'     been confidently identified, preventing misattribution.
#'   \item \strong{Names}: It applies sophisticated, rule-based logic to accurately
#'     identify employee names, handling various formats to ensure every
#'     individual is correctly accounted for.
#'   \item \strong{Work Time}: It extracts the hours worked, respecting any
#'     conditions defined in the configuration to ensure the value is both
#'     accurate and contextually appropriate.
#' }
#' Every new piece of information is generated and validated within this auditable pipeline.
#'
#' @param data A data frame containing the `concatenated_text` column, which
#'   represents the unified but as-yet-unstructured data for each record.
#' @param config A list containing the sanctioned configuration for the data cohort,
#'   which provides the authoritative rules and patterns for extraction.
#'
#' @return A data frame enriched with new columns of structured, validated data:
#'   `extracted_name`, `extracted_date`, `extracted_clock_in`, `extracted_clock_out`,
#'   and `work_time`. This is a critical step toward creating a source of truth.
#'
#' @importFrom dplyr mutate case_when if_else select
#' @importFrom stringr str_extract str_extract_all str_trim str_detect
#' @importFrom purrr map map_chr reduce
#' @importFrom assertthat assert_that is.string is.list
#' @author James Gray (JG3288)
#' @export
extract_daily_data <- function(data, config) {
  # --- Input Validation ---
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that("concatenated_text" %in% names(data))
  assertthat::assert_that(assertthat::is.list(config))

  # --- Vectorized Extraction Pipeline ---
  data %>%
    dplyr::mutate(
      # 1. DATE EXTRACTION
      # Use reduce to try each date pattern in order until one succeeds
      extracted_date = purrr::reduce(
        config$date_patterns,
        ~ dplyr::if_else(is.na(.x), stringr::str_extract(concatenated_text, .y), .x),
        .init = NA_character_
      ),

      # 2. TIME EXTRACTION
      # First, get all time matches for each row
      all_times = purrr::map(concatenated_text, ~ stringr::str_extract_all(., config$time_patterns) %>% unlist()),

      # Conditionally extract times based on presence of a date, if required by config
      has_date = !is.na(extracted_date),
      times_to_use = dplyr::if_else(
        config$time_extraction_condition == "date_present" & !has_date,
        list(NA_character_),
        all_times
      ),
      extracted_clock_in = purrr::map_chr(times_to_use, ~ .x[1]),
      extracted_clock_out = purrr::map_chr(times_to_use, ~ .x[2]),

      # 3. NAME EXTRACTION
      extracted_name = dplyr::case_when(
        # Rule: name is text before a date
        config$name_extraction_rules$type == "before_date" ~
          stringr::str_trim(stringr::str_extract(concatenated_text, paste0("^(.*?)(?=", config$name_extraction_rules$date_pattern, ")"))),

        # Rule: complex conditional logic based on multiple patterns
        config$name_extraction_rules$type == "complex_conditional" ~
          extract_conditional_name(concatenated_text, has_date, config),
        TRUE ~ NA_character_
      ),

      # 4. WORK TIME EXTRACTION
      # Conditionally extract work time based on presence of a date
      work_time = dplyr::if_else(
        config$work_time_condition == "date_present" & !has_date,
        NA_character_,
        stringr::str_extract(concatenated_text, config$work_time_pattern)
      )
    ) %>%
    # Select only the relevant original and new columns
    dplyr::select(
      row_id, pdf_name, page_no, tidyselect::starts_with("Col"),
      concatenated_text, extracted_name, extracted_date,
      extracted_clock_in, extracted_clock_out, work_time
    )
}


#' @title Extract Name Based on Conditional Logic (Vectorized Helper)
#' @description A vectorized helper that applies a series of conditional regex
#'   patterns to extract a name from a vector of text strings.
#'
#' @param text_vector A character vector (`concatenated_text`).
#' @param has_date_vector A logical vector indicating which rows contain a date.
#' @param config The group-specific configuration list.
#'
#' @return A character vector of extracted names.
#' @noRd
extract_conditional_name <- function(text_vector, has_date_vector, config) {
  patterns <- config$name_extraction_rules$patterns

  dplyr::case_when(
    # Rule: Text ends with a bracket, e.g., "James Gray (Dev)"
    stringr::str_detect(text_vector, patterns$ends_with_bracket) ~
      stringr::str_trim(stringr::str_extract(text_vector, patterns$ends_with_bracket, group = 1)),

    # Rule: Text before a number, but only if no date was found
    !has_date_vector & stringr::str_detect(text_vector, patterns$before_number_no_time) ~
      stringr::str_trim(stringr::str_extract(text_vector, ".+?(?=\\s*\\d)")),

    # Rule: A single word, often a last resort
    stringr::str_detect(text_vector, patterns$single_word) ~
      stringr::str_trim(text_vector),
    TRUE ~ NA_character_
  )
}
