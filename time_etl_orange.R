# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MULTI-GROUP TIMESHEET DATA CLEANING PACKAGE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Title: Multi-Group Timesheet Data Cleaning and Quality Assurance System
# Author: James Gray
# Date: 2025-06-05
#
# Purpose:
# Clean and structure messy PDF-extracted timesheet data across different
# format groups with varying extraction patterns, data spillage, and quality issues.
#
# Functionality:
# - Processes three distinct timesheet data groups with different regex patterns
# - Handles multi-column data spillage and date overflow between rows
# - Extracts names, dates, times, work hours, and fortnightly/weekly totals
# - Implements fortnight sequence detection and missing date interpolation for messy data
# - Assigns unique IDs for name-fortnight combinations across files (Spesific to Orange)
# - Combines all groups into unified dataset
# - Comprehensive QA checks
# - Uses hybrid vectorized/loop approach for optimal performance, flexibility and readability
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~=
# TO DO - FUTURE ENHANCEMENTS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# PERFORMANCE & SCALABILITY:
# TODO: Add progress indicators for large dataset processing
# TODO: Implement memory optimization for very large files (chunked processing)
# TODO: Add parallel processing to handle multiple groups simultaneously
# TODO: Implement caching system for intermediate results to avoid reprocessing
# TODO: Date parsing cache (same dates for each pdf file for example)
#
# CONFIGURATION & FLEXIBILITY:
# TODO: Create pattern library with reusable regex patterns across groups and associated parse_data_time order configs
# TODO: Add configurable QA thresholds (outlier detection, validation limits)
# TODO: External config file support (JSON/YAML) for pattern management
# TODO: Implement Coordinate functionality for where regex is the same but location is diff
# TODO: Auto Format (fingerprint) Detector for reading files to make format templates
# TODO: Model selection for approach, and within pdf_data and pdf_text hyperparameter tuning for things like regex selection
# TODO: Consolidate Consequitive Shifts Script but maintain record of shift types
# TODO: Sleepover Split
# TODO; Data Check
#
# ERROR HANDLING & ROBUSTNESS:
# TODO: Implement comprehensive error recovery system with fallback patterns
# TODO: Add graceful degradation when regex patterns fail completely
# TODO: Create recovery strategies for malformed data structures
# TODO: Implement confidence scoring for extracted data quality
# TODO: Backtesting capabilities on raw data to test for accuracy and correctness. Meed to come up with a rigerous way to test %difference between two data sets

#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Multi-Group Timesheet Data Cleaning Script - Orange
# Purpose: Clean and structure messy PDF-extracted timesheet data for 3 different groups
# Author: James Gray
# Date: 2025-06-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD LIBRARIES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(pdftools)
library(stringr)
library(tidyr)
library(dplyr)
library(clipr)
library(lubridate)
library(future)
library(backports)
library(purrr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD SCRIPTS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source_path <- "C:/Users/JG3288/OneDrive - Corporate Network/DevOps/Calculations_Team/OrangeR package/R"

# Might need to Change pattern
included_scripts <- list.files(path = source_path, pattern = "^[0-6]", full.names = TRUE)

included_scripts %>%
  walk(included_scripts, ~ {
    message("Sourcing: ", path_file(.x))
    source(.x, local = FALSE)
  })

names(included_scripts) <- path_file(included_scripts)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MAIN CLEANING FUNCTION ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Main function to process list of 3 dataframes
clean_all_timesheet_groups <- function(data_list) {
  cat("Starting timesheet data cleaning for all groups...\n")

  if (length(data_list) != 3) {
    stop("Expected exactly 3 dataframes in the list")
  }

  # Process each group separately
  cleaned_groups <- list()

  for (group_num in 1:3) {
    cat(paste("PROCESSING GROUP ", group_num))

    cleaned_groups[[group_num]] <- clean_timesheet_data(data_list[[group_num]], group_num)
  }

  # Combine all groups with group identifiers
  cat("COMBINING ALL GROUPS")
  combined_data <- combine_groups(cleaned_groups)

  # FIXME Apply fortnight sequencing and interpolation

  # cat("Applying fortnight sequencing and date interpolation...\n")
  # final_data <- process_fortnight_sequences(combined_data)

  cat("All groups processed and combined successfully!\n")
  return(combined_data)
}

# Individual group cleaning function
clean_timesheet_data <- function(raw_data, group_number = 1) {
  cat("Starting timesheet data cleaning for Group", group_number, "...\n")

  # Get configuration for this group
  config <- get_group_config(group_number)

  # Step 1: Concatenate all columns (same for all groups)
  cat("Step 0: Reading pdf files...\n")
  raw_data <- concatenate_columns(raw_data)


  # Step 1: Concatenate all columns (same for all groups)
  cat("Step 1: Concatenating columns...\n")
  cleaned_data <- concatenate_columns(raw_data)

  # Step 2: Extract structured data using group-specific patterns
  cat("Step 2: Extracting structured data...\n")
  cleaned_data <- extract_timesheet_info_hybrid(cleaned_data, config)

  # FIXME Step 3: Handle date spillover issues (same logic, different patterns)

  # cat("Step 3: Handling date spillovers...\n")
  # cleaned_data <- fix_date_spillovers(cleaned_data, config)

  # FIXME Step 4: Extract totals (group-specific logic)

  # cat("Step 4: Extracting", config$total_type, "totals...\n")
  # cleaned_data <- extract_totals(cleaned_data, raw_data, config)

  # FIXME Step 5: Clean names separately (enhanced for Group 2)
  cat("Step 5: Cleaning names...\n")
  cleaned_data <- clean_names(cleaned_data, group_number)

  # Step 6: Final data type conversions and formatting

  cat("Step 6: Final formatting...\n")
  cleaned_data <- finalize_data_types(cleaned_data, config)

  # FIXME Step 7: Interpolating dates + Fornight Sequences


  # Add group identifier
  cleaned_data$group_id <- group_number

  cat("Data cleaning completed for Group", group_number, "!\n")
  return(cleaned_data)
}
