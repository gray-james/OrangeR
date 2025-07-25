# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUALITY ASSURANCE FOR COMBINED DATA ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO : Grouping ffor checks might change - Configure what duplication means in each dataset.

#' @title Orchestrate a Comprehensive Audit of Data Quality and Integrity
#' @description I believe that trust in data is earned through rigorous, transparent
#'   validation. This function serves as the central orchestrator for our entire
#'   data quality assurance process. It systematically executes a suite of
#'   specialized checking functions, each designed to scrutinize a different
#'   facet of the data's integrity.
#'
#' @details
#' My dream is a system where every potential data issue is proactively
#' identified and reported. This function is a key part of that vision, covering
#' several critical areas of validation:
#' \itemize{
#'   \item \strong{Completeness:} Checking for missing data in key fields.
#'   \item \strong{Date & Time Integrity:} Validating the logic and consistency of
#'     all temporal data.
#'   \item \strong{Reasonableness:} Detecting outliers in worked hours that may
#'     indicate errors.
#'   \item \strong{Period Validation:} Ensuring the integrity of fortnightly and
#'     other pay periods.
#'   \item \strong{Identifier Consistency:} Checking for completeness and consistency
#'     in names and IDs.
#' }
#' The results of this comprehensive audit are then compiled and presented in a
#' clear, accessible summary.
#'
#' @param final_data The fully processed and unified data frame, which is now
#'   ready for its final and most thorough validation.
#' @return A list where each element contains the detailed results of a specific
#'   QA check, providing a complete, auditable record of the validation process.
#'
#' @author James Gray (JG3288)
#' @export
perform_quality_assurance <- function(final_data) {
  cli::cli_h1("Running Quality Assurance Checks")

  qa_results <- list()

  # --- Execute All Checks ---
  qa_results$missing_data <- check_missing_data(final_data)
  qa_results$date_anomalies <- check_date_anomalies(final_data)
  qa_results$time_inconsistencies <- check_time_inconsistencies(final_data)
  qa_results$hour_outliers <- check_hour_outliers(final_data)
  qa_results$fortnight_integrity <- check_fortnight_integrity(final_data)
  qa_results$name_id_completeness <- check_name_id_completeness(final_data)
  qa_results$date_interpolation <- check_date_interpolation(final_data)
  qa_results$group_distribution <- check_group_distribution(final_data)
  qa_results$name_consistency <- check_name_consistency(final_data)

  # --- Print Summary ---
  print_qa_summary(qa_results)

  cli::cli_alert_success("Quality Assurance checks complete.")
  return(qa_results)
}

#' @title Check for Missing Data
#' @description Checks for missing values in key columns of the dataset, both
#'   overall and broken down by group.
#' @param data The input data frame.
#' @return A list containing two tibbles: `overall` and `by_group` summaries.
#' @noRd
check_missing_data <- function(data) {
  cli::cli_alert_info("Checking for missing data...")

  key_columns <- c(
    "cleaned_name", "cleaned_date", "extracted_clock_in", "extracted_clock_out",
    "cleaned_work_hours", "master_id", "fortnight_id"
  )
  key_columns <- intersect(key_columns, names(data))

  # Overall missing data
  overall_missing <- data %>%
    dplyr::select(dplyr::all_of(key_columns)) %>%
    dplyr::summarise(dplyr::across(everything(), ~ sum(is.na(.)))) %>%
    tidyr::gather(column, missing_count) %>%
    dplyr::mutate(missing_percentage = round(missing_count / nrow(data) * 100, 2))

  # Missing data by group
  if ("group_id" %in% names(data)) {
    group_missing <- data %>%
      dplyr::group_by(group_id) %>%
      dplyr::select(dplyr::all_of(key_columns)) %>%
      dplyr::summarise(dplyr::across(everything(), ~ sum(is.na(.))), .groups = "drop") %>%
      tidyr::gather(column, missing_count, -group_id) %>%
      dplyr::group_by(group_id) %>%
      dplyr::mutate(group_size = sum(missing_count) / dplyr::n_distinct(column)) %>% # Approximate
      dplyr::ungroup() %>%
      dplyr::mutate(missing_percentage = round(missing_count / group_size * 100, 2))
  } else {
    group_missing <- tibble::tibble() # Empty tibble if no group_id
  }

  return(list(overall = overall_missing, by_group = group_missing))
}

#' @title Check Fortnight Sequence Integrity
#' @description Checks for issues within each identified fortnight, such as the
#'   number of unique dates and the rate of interpolation.
#' @param data The input data frame.
#' @return A tibble summarizing the integrity of each fortnight.
#' @noRd
check_fortnight_integrity <- function(data) {
  cli::cli_alert_info("Checking fortnight sequence integrity...")

  integrity_check <- data %>%
    filter(!is.na(master_id)) %>%
    group_by(pdf_name, master_id, cleaned_name, fortnight_id) %>%
    summarise(
      records_count = n(),
      dates_count = sum(!is.na(cleaned_date)),
      interpolated_dates = sum(date_interpolated, na.rm = TRUE),
      unique_dates = n_distinct(cleaned_date, na.rm = TRUE),
      expected_dates = 14, # Each fortnight should have 14 days
      .groups = "drop"
    ) %>%
    mutate(
      date_coverage = round(unique_dates / expected_dates * 100, 1),
      potential_issues = case_when(
        unique_dates > expected_dates ~ "More dates than expected",
        unique_dates < 7 ~ "Very few dates captured",
        interpolated_dates > 7 ~ "High interpolation",
        TRUE ~ "OK"
      )
    )

  return(integrity_check)
}

#' @title Check Name and ID Completeness
#' @description Checks for records where a name is present but an ID is missing,
#'   or vice-versa.
#' @param data The input data frame.
#' @return A list containing summaries of name/ID completeness.
#' @noRd
check_name_id_completeness <- function(data) {
  cli::cli_alert_info("Checking name-ID completeness...")

  # Check for IDs without names
  ids_without_names <- data %>%
    filter(!is.na(master_id) & is.na(cleaned_name)) %>%
    count(master_id, pdf_name, fortnight_id) %>%
    rename(records_without_name = n)

  # Check for names without IDs
  names_without_ids <- data %>%
    filter(!is.na(cleaned_name) & is.na(master_id)) %>%
    count(cleaned_name, pdf_name, group_id) %>%
    rename(records_without_id = n)

  # Summary statistics
  completeness_summary <- data %>%
    summarise(
      total_records = n(),
      records_with_both = sum(!is.na(cleaned_name) & !is.na(master_id)),
      records_name_only = sum(!is.na(cleaned_name) & is.na(master_id)),
      records_id_only = sum(is.na(cleaned_name) & !is.na(master_id)),
      records_neither = sum(is.na(cleaned_name) & is.na(master_id))
    ) %>%
    mutate(
      completeness_rate = round(records_with_both / total_records * 100, 2)
    )

  return(list(
    ids_without_names = ids_without_names,
    names_without_ids = names_without_ids,
    summary = completeness_summary
  ))
}

#' @title Check Date Interpolation Summary
#' @description Summarizes the extent to which dates were interpolated, both
#'   overall and on a per-file basis.
#' @param data The input data frame.
#' @return A list with `by_file` and `overall` summaries of interpolation.
#' @noRd
check_date_interpolation <- function(data) {
  cli::cli_alert_info("Checking date interpolation summary...")

  if (!"date_interpolated" %in% names(data)) {
    return(list(message = "No date interpolation performed"))
  }

  interpolation_summary <- data %>%
    group_by(group_id, pdf_name) %>%
    summarise(
      total_records = n(),
      interpolated_dates = sum(date_interpolated, na.rm = TRUE),
      interpolation_rate = round(interpolated_dates / total_records * 100, 2),
      .groups = "drop"
    )

  overall_interpolation <- data %>%
    summarise(
      total_records = n(),
      interpolated_dates = sum(date_interpolated, na.rm = TRUE),
      overall_interpolation_rate = round(interpolated_dates / total_records * 100, 2)
    )

  return(list(
    by_file = interpolation_summary,
    overall = overall_interpolation
  ))
}

#' @title Check Group Distribution
#' @description Provides a summary of the data distribution across different groups.
#' @param data The input data frame.
#' @return A tibble summarizing the record distribution.
#' @noRd
check_group_distribution <- function(data) {
  cli::cli_alert_info("Checking group distribution...")

  group_summary <- data %>%
    group_by(group_id) %>%
    summarise(
      records = n(),
      unique_names = n_distinct(cleaned_name, na.rm = TRUE),
      unique_dates = n_distinct(cleaned_date, na.rm = TRUE),
      unique_files = n_distinct(pdf_name, na.rm = TRUE),
      unique_ids = n_distinct(master_id, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percentage_of_total = round(records / sum(records) * 100, 2)
    )

  return(group_summary)
}

#' @title Check Name Consistency
#' @description Analyzes the consistency of cleaned names, looking for potential
#'   OCR errors or other issues.
#' @param data The input data frame.
#' @return A tibble summarizing name consistency metrics.
#' @noRd
check_name_consistency <- function(data) {
  cli::cli_alert_info("Checking name consistency...")

  data %>%
    dplyr::filter(!is.na(cleaned_name)) %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarise(
      total_names = dplyr::n(),
      unique_names = dplyr::n_distinct(cleaned_name),
      avg_name_length = round(mean(nchar(cleaned_name)), 1),
      names_with_numbers = sum(stringr::str_detect(cleaned_name, "\\d")),
      very_short_names = sum(nchar(cleaned_name) < 3),
      very_long_names = sum(nchar(cleaned_name) > 50),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      name_reuse_rate = round((total_names - unique_names) / total_names * 100, 2),
      potential_ocr_errors = names_with_numbers + very_short_names + very_long_names
    )
}

#' @title Print QA Summary to Console
#' @description Formats and prints a summary of the QA results to the console
#'   using the `cli` package for clear and structured output.
#' @param qa_results The list object produced by `perform_quality_assurance`.
#' @return This function is called for its side effect of printing to the console.
#' @noRd
print_qa_summary <- function(qa_results) {
  cli::cli_h2("Data Quality Assurance Summary")

  # --- Missing Data ---
  cli::cli_h3("Overall Missing Data")
  print(qa_results$missing_data$overall)

  # --- Group Distribution ---
  cli::cli_h3("Group Distribution")
  print(qa_results$group_distribution)

  # --- Name-ID Completeness ---
  cli::cli_h3("Name-ID Completeness")
  print(qa_results$name_id_completeness$summary)
  cli::cli_bullets(c(
    "i" = paste("IDs without names:", nrow(qa_results$name_id_completeness$ids_without_names), "issues"),
    "i" = paste("Names without IDs:", nrow(qa_results$name_id_completeness$names_without_ids), "issues")
  ))

  # --- Date Interpolation ---
  if ("overall" %in% names(qa_results$date_interpolation)) {
    cli::cli_h3("Date Interpolation")
    print(qa_results$date_interpolation$overall)
  }

  # --- Fortnight Integrity ---
  integrity_issues <- qa_results$fortnight_integrity %>%
    dplyr::filter(potential_issues != "OK")
  cli::cli_h3("Fortnight Integrity")
  cli::cli_alert_info("{nrow(integrity_issues)} issues found (e.g., periods longer than expected).")


  # --- Standard Anomalies ---
  cli::cli_h3("Standard Anomalies")
  cli::cli_bullets(c(
    "!" = paste("Future dates:", nrow(qa_results$date_anomalies$future_dates)),
    "!" = paste("Time inconsistencies:", nrow(qa_results$time_inconsistencies)),
    "!" = paste("Hour outliers:", nrow(qa_results$hour_outliers))
  ))

  # --- Name Consistency ---
  cli::cli_h3("Name Consistency by Group")
  print(qa_results$name_consistency)
}

# Group 3 specific checks (similar to Group 2 but separate)
check_group3_name_consistency <- function(data) {
  cat("Checking Group 3 name consistency issues...\n")

  # Same logic as Group 2 but separate function for future customization
  name_counts <- data %>%
    filter(!is.na(cleaned_name)) %>%
    count(cleaned_name, sort = TRUE)

  # Flag potential OCR errors (very short names, numbers in names, etc.)
  suspicious_names <- data %>%
    filter(!is.na(cleaned_name)) %>%
    filter(nchar(cleaned_name) < 2 |
      str_detect(cleaned_name, "\\d") |
      str_detect(cleaned_name, "[^A-Za-z\\s]")) %>%
    distinct(cleaned_name)

  # Names that appear only once (might be errors)
  singleton_names <- name_counts %>%
    filter(n == 1) %>%
    pull(cleaned_name)

  issues <- list(
    suspicious_names = suspicious_names$cleaned_name,
    singleton_names = singleton_names,
    name_frequency = name_counts
  )

  return(issues)
}

# Group 3 specific check for time-date relationship
check_group3_time_date_relationship <- function(data) {
  cat("Checking Group 3 time-date relationship compliance...\n")

  # Check if times were extracted only when dates are present
  time_date_violations <- data %>%
    filter(!is.na(extracted_clock_in) | !is.na(extracted_clock_out)) %>% # Has times
    filter(is.na(extracted_date)) %>% # But no date
    select(row_id, concatenated_text, extracted_clock_in, extracted_clock_out, extracted_date)

  # Summary of compliance
  compliance_summary <- data %>%
    summarise(
      total_records = n(),
      records_with_times = sum(!is.na(extracted_clock_in) | !is.na(extracted_clock_out)),
      records_with_dates = sum(!is.na(extracted_date)),
      records_times_and_dates = sum((!is.na(extracted_clock_in) | !is.na(extracted_clock_out)) & !is.na(extracted_date)),
      violations = nrow(time_date_violations)
    ) %>%
    mutate(
      compliance_rate = round(
        ifelse(records_with_times > 0,
          (records_with_times - violations) / records_with_times * 100,
          100
        ), 2
      )
    )

  return(list(
    violations = time_date_violations,
    summary = compliance_summary
  ))
}

# Validate fortnightly totals
validate_fortnightly_totals <- function(data) {
  cat("Validating fortnightly totals...\n")

  # Group by name and fortnight, sum daily hours
  validation <- data %>%
    filter(!is.na(cleaned_name) & !is.na(cleaned_date) & !is.na(cleaned_work_hours)) %>%
    mutate(fortnight = floor_date(cleaned_date, "2 weeks")) %>%
    group_by(cleaned_name, fortnight) %>%
    summarise(
      calculated_fortnightly_total = sum(cleaned_work_hours, na.rm = TRUE),
      reported_fortnightly_total = first(fortnightly_total_hours[!is.na(fortnightly_total_hours)]),
      .groups = "drop"
    ) %>%
    filter(!is.na(reported_fortnightly_total)) %>%
    mutate(
      difference = abs(calculated_fortnightly_total - reported_fortnightly_total),
      significant_difference = difference > 1.0 # More than 1 hour difference
    )

  return(validation)
}

# Enhanced QA summary printing for all groups
print_qa_summary <- function(qa_results, group_number) {
  cat("\n" %R% paste("GROUP", group_number, "QUALITY ASSURANCE SUMMARY") %R% "\n")
  cat("=" %R% 60 %R% "\n")

  # Missing data summary
  cat("Missing Data:\n")
  print(qa_results$missing_data)
  cat("\n")

  # Date anomalies
  cat("Date Anomalies:\n")
  cat("- Future dates:", nrow(qa_results$date_anomalies$future_dates), "\n")
  cat("- Weekend work:", nrow(qa_results$date_anomalies$weekend_work), "\n")
  cat("- Very old dates:", nrow(qa_results$date_anomalies$old_dates), "\n\n")

  # Time inconsistencies
  cat("Time Inconsistencies:", nrow(qa_results$time_inconsistencies), "records\n\n")

  # Hour outliers
  cat("Work Hour Outliers:", nrow(qa_results$hour_outliers), "records\n\n")

  # Group-specific checks
  if (group_number == 2) {
    cat("Group 2 Name Issues:\n")
    cat("- Suspicious names:", length(qa_results$name_issues$suspicious_names), "\n")
    cat("- Singleton names:", length(qa_results$name_issues$singleton_names), "\n\n")

    if ("fortnightly_validation" %in% names(qa_results) && nrow(qa_results$fortnightly_validation) > 0) {
      significant_diffs <- sum(qa_results$fortnightly_validation$significant_difference, na.rm = TRUE)
      cat("Fortnightly Total Validation:\n")
      cat("- Records with significant differences:", significant_diffs, "\n\n")
    }
  } else if (group_number == 3) {
    cat("Group 3 Name Issues:\n")
    cat("- Suspicious names:", length(qa_results$name_issues$suspicious_names), "\n")
    cat("- Singleton names:", length(qa_results$name_issues$singleton_names), "\n\n")

    if ("fortnightly_validation" %in% names(qa_results) && nrow(qa_results$fortnightly_validation) > 0) {
      significant_diffs <- sum(qa_results$fortnightly_validation$significant_difference, na.rm = TRUE)
      cat("Fortnightly Total Validation:\n")
      cat("- Records with significant differences:", significant_diffs, "\n\n")
    }

    if ("time_date_relationship" %in% names(qa_results)) {
      cat("Time-Date Relationship Compliance:\n")
      print(qa_results$time_date_relationship$summary)
      cat("- Violations:", nrow(qa_results$time_date_relationship$violations), "\n\n")
    }
  } else {
    cat("Name Issues:\n")
    cat("- Rare names:", length(qa_results$name_issues$rare_names), "\n")
    cat("- Unusual length names:", length(qa_results$name_issues$unusual_length_names), "\n\n")

    if ("weekly_validation" %in% names(qa_results) && nrow(qa_results$weekly_validation) > 0) {
      significant_diffs <- sum(qa_results$weekly_validation$significant_difference, na.rm = TRUE)
      cat("Weekly Total Validation:\n")
      cat("- Records with significant differences:", significant_diffs, "\n\n")
    }
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UTILITY FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`%R%` <- function(string, times) {
  paste(rep(string, times), collapse = "")
}

check_date_anomalies <- function(data) {
  anomalies <- list()

  future_dates <- data %>%
    filter(!is.na(cleaned_date) & cleaned_date > Sys.Date()) %>%
    select(row_id, cleaned_name, cleaned_date)

  weekend_work <- data %>%
    filter(!is.na(cleaned_date) & weekdays(cleaned_date) %in% c("Saturday", "Sunday")) %>%
    select(row_id, cleaned_name, cleaned_date)

  old_dates <- data %>%
    filter(!is.na(cleaned_date) & cleaned_date < (Sys.Date() - years(2))) %>%
    select(row_id, cleaned_name, cleaned_date)

  anomalies$future_dates <- future_dates
  anomalies$weekend_work <- weekend_work
  anomalies$old_dates <- old_dates

  return(anomalies)
}

check_time_inconsistencies <- function(data) {
  inconsistencies <- data %>%
    filter(!is.na(cleaned_clock_in_time) & !is.na(cleaned_clock_out_time)) %>%
    filter(cleaned_clock_out_time <= cleaned_clock_in_time) %>%
    select(row_id, cleaned_name, extracted_clock_in, extracted_clock_in, calculated_duration)

  return(inconsistencies)
}

check_hour_outliers <- function(data) {
  min_reasonable_hours <- 0.5
  max_reasonable_hours <- 16

  outliers <- data %>%
    filter(!is.na(cleaned_work_hours)) %>%
    filter(cleaned_work_hours < min_reasonable_hours |
      cleaned_work_hours > max_reasonable_hours) %>%
    select(row_id, cleaned_name, cleaned_work_hours, cleaned_date)

  return(outliers)
}

# Analyse the gaps between calculated calculated and pulled work hours
# - Some with no breaks, others with high number pf break
gapanalyis <- function(data) {}

time

aggregated_total_check <- function(data) {}

validate_weekly_totals <- function(data) {
  validation <- data %>%
    filter(!is.na(cleaned_name) & !is.na(cleaned_date) & !is.na(cleaned_work_hours)) %>%
    mutate(week = floor_date(cleaned_date, "week")) %>%
    group_by(cleaned_name, week) %>%
    summarise(
      calculated_weekly_total = sum(cleaned_work_hours, na.rm = TRUE),
      reported_weekly_total = first(weekly_total_hours[!is.na(weekly_total_hours)]),
      .groups = "drop"
    ) %>%
    filter(!is.na(reported_weekly_total)) %>%
    mutate(
      difference = abs(calculated_weekly_total - reported_weekly_total),
      significant_difference = difference > 0.5
    )

  return(validation)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXAMPLE USAGE FOR MULTIPLE GROUPS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example usage:
#
# # Group 1 (original format)
# raw_data_group1 <- read.csv("group1_timesheet.csv", stringsAsFactors = FALSE)
# cleaned_group1 <- clean_timesheet_data(raw_data_group1, group_number = 1)
# qa_results_group1 <- run_quality_checks(cleaned_group1, group_number = 1)
# write.csv(cleaned_group1, "cleaned_group1_timesheet.csv", row.names = FALSE)
#
# # Group 2 (new format with inconsistent names)
# raw_data_group2 <- read.csv("group2_timesheet.csv", stringsAsFactors = FALSE)
# cleaned_group2 <- clean_timesheet_data(raw_data_group2, group_number = 2)
# qa_results_group2 <- run_quality_checks(cleaned_group2, group_number = 2)
# write.csv(cleaned_group2, "cleaned_group2_timesheet.csv", row.names = FALSE)
#
# # Group 3 (to be configured)
# raw_data_group3 <- read.csv("group3_timesheet.csv", stringsAsFactors = FALSE)
# cleaned_group3 <- clean_timesheet_data(raw_data_group3, group_number = 3)
# qa_results_group3 <- run_quality_checks(cleaned_group3, group_number = 3)
# write.csv(cleaned_group3, "cleaned_group3_timesheet.csv", row.names = FALSE)
#
# # Compare final outputs
# final_group1 <- cleaned_group1 %>%
#   select(cleaned_name, cleaned_date, extracted_clock_in, extracted_clock_in,
#          cleaned_work_hours, weekly_total_hours) %>%
#   filter(!is.na(cleaned_name))
#
# final_group2 <- cleaned_group2 %>%
#   select(cleaned_name, cleaned_date, extracted_clock_in, extracted_clock_in,
#          cleaned_work_hours, fortnightly_total_hours) %>%
#   filter(!is.na(cleaned_name))
#
# View(final_group1)
# View(final_group2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TESTING AND VALIDATION FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function to test Group 2 name extraction rules with sample data
test_group2_name_extraction <- function() {
  cat("Testing Group 2 name extraction rules...\n")

  # Sample test cases for Group 2
  test_cases <- c(
    "JOHN DOE)", # Rule a: ends with bracket
    "Jane Smith 7-Nov-21 8:00 17:00 8.5", # Rule b: before date
    "Bob Wilson 45.5", # Rule c: before number, no time
    "Sarah", # Rule d: single word
    "MIKE JONES (TEMP)", # Rule a: ends with bracket
    "7_Nov_21 9:00 18:00 8.0", # No name extractable
    "Training 2.5" # Rule c: before number, no time
  )

  config <- get_group_config(2)

  for (i in seq_along(test_cases)) {
    text <- test_cases[i]
    cat("Test", i, ":", text, "\n")

    # Extract date first (needed for some rules)
    date_found <- str_extract(text, config$date_patterns[1])

    # Extract name using Group 2 logic
    name <- extract_conditional_name(text, date_found, config)

    cat("  Extracted name:", ifelse(is.na(name), "NONE", name), "\n")
    cat("  Extracted date:", ifelse(is.na(date_found), "NONE", date_found), "\n\n")
  }
}

# Function to validate regex patterns work correctly
validate_regex_patterns <- function(group_number) {
  cat("Validating regex patterns for Group", group_number, "...\n")

  config <- get_group_config(group_number)

  # Test date patterns
  cat("Date pattern testing:\n")
  if (group_number == 1) {
    test_dates <- c("MON21/09/06", "TUE22/10/07", "WED23/11/08")
  } else if (group_number == 2) {
    test_dates <- c("7-Nov-21", "15_Dec_22", "3.Jan.23", "28/Feb/24")
  }

  for (date in test_dates) {
    matches <- sapply(config$date_patterns, function(p) !is.na(str_extract(date, p)))
    cat("  Date:", date, "- Matches:", any(matches), "\n")
  }

  # Test time patterns
  cat("\nTime pattern testing:\n")
  if (group_number == 1) {
    test_times <- c("8:00:00 AM", "17:30:00 PM", "9:15:00 AM")
  } else if (group_number == 2) {
    test_times <- c("8:00", "17:30", "23:45", "7:15")
  }

  for (time in test_times) {
    matches <- sapply(config$time_patterns, function(p) !is.na(str_extract(time, p)))
    cat("  Time:", time, "- Matches:", any(matches), "\n")
  }

  cat("\n")
}

# Function to update Group 3 configuration
update_group3_config <- function(name_rules, date_patterns, time_patterns,
                                 work_time_pattern, total_type, total_location) {
  cat("Updating Group 3 configuration...\n")

  # This function allows dynamic updating of Group 3 config
  # You can call this before processing Group 3 data

  # Update the configuration (you would modify the get_group_config function)
  # For now, this is a template for when you define Group 3 requirements

  group3_config <- list(
    name_extraction_rules = name_rules,
    date_patterns = date_patterns,
    time_patterns = time_patterns,
    work_time_pattern = work_time_pattern,
    total_type = total_type,
    total_location = total_location
  )

  cat("Group 3 configuration updated successfully!\n")
  return(group3_config)
}

# Function to create custom configuration for any group
create_custom_config <- function(group_name, config_list) {
  cat("Creating custom configuration for", group_name, "...\n")

  # Validate required fields
  required_fields <- c(
    "name_extraction_rules", "date_patterns", "time_patterns",
    "work_time_pattern", "total_type", "total_location"
  )

  missing_fields <- setdiff(required_fields, names(config_list))
  if (length(missing_fields) > 0) {
    stop("Missing required configuration fields: ", paste(missing_fields, collapse = ", "))
  }

  return(config_list)
}

#' @title Process Multiple Data Cohorts in a Scalable, Auditable Batch
#' @description I believe that our commitment to data quality must be scalable.
#'   This function provides a robust batch processing capability, allowing us to
#'   apply our entire, standardized cleaning and quality assurance pipeline to
#'   multiple, distinct data cohorts in a single, auditable run.
#'
#' @details
#' My approach is to build systems that are not only accurate but also efficient
#' and repeatable. This function orchestrates the end-to-end process for each
#' specified data cohort:
#' \enumerate{
#'   \item It systematically iterates through each file path and its corresponding
#'     group number.
#'   \item For each cohort, it executes the full `clean_timesheet_data` pipeline.
#'   \item It then immediately runs the comprehensive `run_quality_checks` suite.
#'   \item Finally, it saves the cleaned data and compiles the results, providing
#'     a complete record of the entire batch operation.
#' }
#'
#' @param file_paths A character vector of file paths, with each path pointing to
#'   the raw data for a specific cohort.
#' @param group_numbers A numeric vector specifying the corresponding group number
#'   for each file path, ensuring the correct cleaning logic is applied.
#' @param output_dir The sanctioned directory where the final, cleaned data files
#'   for each cohort will be saved.
#'
#' @return A nested list containing the results for each processed group. Each
#'   element includes the cleaned data, the full QA results, and the path to the
#'   final output file, ensuring complete transparency and traceability.
#'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BATCH PROCESSING FUNCTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

process_multiple_groups <- function(file_paths, group_numbers, output_dir = ".") {
  cat("Starting batch processing of multiple timesheet groups...\n")

  if (length(file_paths) != length(group_numbers)) {
    stop("Number of file paths must match number of group numbers")
  }

  results <- list()

  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]
    group_num <- group_numbers[i]

    cat("\nProcessing", file_path, "as Group", group_num, "...\n")

    # Read data
    raw_data <- read.csv(file_path, stringsAsFactors = FALSE)

    # Clean data
    cleaned_data <- clean_timesheet_data(raw_data, group_number = group_num)

    # Run QA
    qa_results <- run_quality_checks(cleaned_data, group_number = group_num)

    # Save results
    output_file <- file.path(output_dir, paste0(
      "cleaned_group", group_num, "_",
      basename(tools::file_path_sans_ext(file_path)), ".csv"
    ))
    write.csv(cleaned_data, output_file, row.names = FALSE)

    results[[paste0("group_", group_num)]] <- list(
      cleaned_data = cleaned_data,
      qa_results = qa_results,
      output_file = output_file
    )

    cat("Group", group_num, "processing completed. Output saved to:", output_file, "\n")
  }

  cat("\nBatch processing completed for all groups!\n")
  return(results)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UPDATED EXAMPLE USAGE FOR LIST-BASED PROCESSING
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Main usage example:
#
# # Load your data as a list of 3 dataframes
# timesheet_data_list <- list(
#   read.csv("group1_timesheet.csv", stringsAsFactors = FALSE),  # Group 1
#   read.csv("group2_timesheet.csv", stringsAsFactors = FALSE),  # Group 2
#   read.csv("group3_timesheet.csv", stringsAsFactors = FALSE)   # Group 3
# )
#
# # Process all groups and get combined result
# final_timesheet_data <- clean_all_timesheet_groups(timesheet_data_list)
#
# # Run comprehensive QA on combined data
# qa_results <- run_combined_quality_checks(final_timesheet_data)
#
# # Save the final combined dataset
# write.csv(final_timesheet_data, "final_combined_timesheet.csv", row.names = FALSE)
#
# # View the structured final data
# final_summary <- final_timesheet_data %>%
#   filter(!is.na(cleaned_name)) %>%
#   select(group_id, path, master_id, fortnight_id, cleaned_name, cleaned_date,
#          extracted_clock_in, extracted_clock_out, cleaned_work_hours, date_interpolated) %>%
#   arrange(group_id, master_id, cleaned_date)
#
# View(final_summary)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TESTING AND VALIDATION FUNCTIONS (UPDATED)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function to test the entire pipeline with sample data
test_full_pipeline <- function() {
  cat("Testing full pipeline with sample data...\n")

  # Create sample data for testing
  sample_data <- create_sample_test_data()

  # Test the full pipeline
  result <- clean_all_timesheet_groups(sample_data)

  # Run QA
  qa <- run_combined_quality_checks(result)

  cat("Pipeline test completed successfully!\n")
  return(list(data = result, qa = qa))
}

create_sample_test_data <- function() {
  # Create sample data for each group
  group1_sample <- data.frame(
    Col1 = c("John Doe", "MON21/09/06", "8:00:00 AM", "17:00:00 PM", "8.5", "40"),
    Col2 = c("", "9:00:00 AM", "18:00:00 PM", "", "", ""),
    Col3 = c("", "", "", "", "", ""),
    pdf_name = "file1.pdf",
    stringsAsFactors = FALSE
  )

  group2_sample <- data.frame(
    Col1 = c("Jane Smith)", "7-Nov-21", "8:00", "17:00", "8.5", "Training 2.5"),
    Col2 = c("", "15:30", "", "", "", ""),
    Col3 = c("", "", "", "", "", ""),
    pdf_name = "file2.pdf",
    stringsAsFactors = FALSE
  )

  group3_sample <- data.frame(
    Col1 = c("Bob Wilson", "2021-11-07", "08:00", "17:00", "8.5", "Weekly: 40"),
    Col2 = c("", "", "", "", "", ""),
    Col3 = c("", "", "", "", "", ""),
    pdf_name = "file3.pdf",
    stringsAsFactors = FALSE
  )

  return(list(group1_sample, group2_sample, group3_sample))
}

# Function to validate fortnight sequence logic
test_fortnight_logic <- function() {
  cat("Testing fortnight sequence logic...\n")

  # Create test data with known fortnight patterns
  test_dates <- seq(from = as.Date("2021-11-01"), to = as.Date("2021-11-28"), by = "day")

  test_data <- data.frame(
    cleaned_name = rep(c("John Doe", "Jane Smith"), each = 14),
    cleaned_date = rep(test_dates, 1),
    pdf_name = "test_file.pdf",
    group_id = 1,
    stringsAsFactors = FALSE
  )

  # Test the fortnight processing
  result <- process_fortnight_sequences(test_data)

  # Validate results
  cat("Fortnight IDs created:", max(result$fortnight_id, na.rm = TRUE), "\n")
  cat("Master IDs created:", max(result$master_id, na.rm = TRUE), "\n")
  cat("Date interpolation performed:", sum(result$date_interpolated, na.rm = TRUE), "dates\n")

  return(result)
}

# Function to check if Group 3 configuration is ready
check_group3_readiness <- function() {
  config <- get_group_config(3)

  if (config$name_extraction_rules$type == "complex_conditional") {
    cat("✓ Group 3 configuration is READY!\n")
    cat("Configuration details:\n")
    cat("- Name extraction: Complex conditional (same as Group 2)\n")
    cat("- Date patterns:", config$date_patterns[1], "\n")
    cat("- Time patterns:", config$time_patterns[1], "\n")
    cat("- Work time condition:", config$work_time_condition, "\n")
    cat("- Time extraction condition:", config$time_extraction_condition, "\n")
    cat("- Total type:", config$total_type, "\n")
    cat("- Total location:", config$total_location, "\n")

    return(TRUE)
  } else {
    cat("Group 3 configuration is NOT ready.\n")
    return(FALSE)
  }
}

# Function to test Group 3 specific functionality
test_group3_features <- function() {
  cat("Testing Group 3 specific features...\n")

  # Test cases for Group 3 time-date relationship
  test_cases <- c(
    "John Doe 7-Nov-21 8:00 17:00 8.5", # Should extract times (has date)
    "Jane Smith 8:00 17:00 8.5", # Should NOT extract times (no date)
    "Bob Wilson) 15:30", # Should NOT extract times (no date)
    "7_Dec_21 9:00 18:00", # Should extract times (has date)
    "Training 45.5" # Should NOT extract times (no date)
  )

  config <- get_group_config(3)

  for (i in seq_along(test_cases)) {
    text <- test_cases[i]
    cat("Test", i, ":", text, "\n")

    # Check if text has date
    has_date <- any(sapply(config$date_patterns, function(dp) str_detect(text, dp)))

    # Extract times only if date present (Group 3 condition)
    times_extracted <- character(0)
    if (has_date) {
      times_extracted <- str_extract_all(text, config$time_patterns[1])[[1]]
    }

    cat("  Has date:", has_date, "\n")
    cat("  Times extracted:", length(times_extracted), ifelse(length(times_extracted) > 0, paste("(", paste(times_extracted, collapse = ", "), ")", sep = ""), ""), "\n\n")
  }
}

# Group 2 specific name consistency checks
check_group2_name_consistency <- function(data) {
  cat("Checking Group 2 name consistency issues...\n")

  # More lenient checks for Group 2's inconsistent names
  name_counts <- data %>%
    filter(!is.na(cleaned_name)) %>%
    count(cleaned_name, sort = TRUE)

  # Flag potential OCR errors (very short names, numbers in names, etc.)
  suspicious_names <- data %>%
    filter(!is.na(cleaned_name)) %>%
    filter(nchar(cleaned_name) < 2 |
      str_detect(cleaned_name, "\\d") |
      str_detect(cleaned_name, "[^A-Za-z\\s]")) %>%
    distinct(cleaned_name)

  # Names that appear only once (might be errors)
  singleton_names <- name_counts %>%
    filter(n == 1) %>%
    pull(cleaned_name)

  issues <- list(
    suspicious_names = suspicious_names$cleaned_name,
    singleton_names = singleton_names,
    name_frequency = name_counts
  )

  return(issues)
}
