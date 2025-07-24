# Load all package functions for testing
devtools::load_all()

library(testthat)
library(dplyr)
library(purrr)
library(here)

# Sample Raw Data for Testing
# This data simulates the initial messy format with 'Col' prefixes.
sample_raw_data_group1 <- data.frame(
    Col1 = c("John Doe", "Jane Smith"),
    Col2 = c("2023-10-01", "2023-10-01"),
    Col3 = c("09:00", "08:30"),
    Col4 = c("17:00", "16:30"),
    Col5 = c("8.00", "8.00"),
    stringsAsFactors = FALSE
)

# This data simulates the output after the concatenation step.
sample_concatenated_data <- data.frame(
    concatenated_text = c(
        "John Doe 2023-10-01 09:00 17:00 8.00",
        "Jane Smith 2023-10-01 08:30 16:30 8.00"
    ),
    row_id = 1:2,
    stringsAsFactors = FALSE
)

# Test 1: Initial Data Cleaning and Concatenation
test_that("Data concatenation works correctly", {
    concatenated_data <- concatenate_columns(sample_raw_data_group1)
    expect_true("concatenated_text" %in% names(concatenated_data))
    expect_equal(concatenated_data$concatenated_text[1], "John Doe 2023-10-01 09:00 17:00 8.00")
})

# Test 2: Extraction of Timesheet Information
test_that("Timesheet information is extracted correctly for Group 1", {
    # The config path needs to be relative to the package root.
    config_path <- here::here("R", "config.yaml")
    config_group1 <- get_group_config(1, config_path = config_path)

    extracted_data <- extract_timesheet_info_hybrid(sample_concatenated_data, config_group1)

    expect_true(all(c("name", "date", "start_time", "end_time", "hours") %in% names(extracted_data)))
    expect_equal(extracted_data$name[1], "John Doe")
})

# Test 3: Name Cleaning Functionality
test_that("Names are cleaned correctly", {
    # The clean_names function expects a column named 'name'
    data_with_names <- data.frame(
        name = c("   John Doe  ", "Jane Smith (Contractor)"),
        stringsAsFactors = FALSE
    )

    # The function to be tested is clean_names, but it seems to have an assertion
    # for 'extracted_name'. Let's provide that column instead.
    data_with_extracted_names <- data.frame(
        extracted_name = c("   John Doe  ", "Jane Smith (Contractor)"),
        stringsAsFactors = FALSE
    )

    cleaned_data <- clean_names(data_with_extracted_names, group_number = 1)
    expect_equal(cleaned_data$name[1], "John Doe")
})

# Test 4: Final Data Type Conversion
test_that("Data types are finalized correctly", {
    data_to_finalize <- data.frame(
        date = "2023-10-01",
        hours = "8.00",
        stringsAsFactors = FALSE
    )

    config_path <- here::here("R", "config.yaml")
    config_group1 <- get_group_config(1, config_path = config_path)

    finalized_data <- finalize_data_types(data_to_finalize, config_group1)

    expect_s3_class(finalized_data$date, "Date")
    expect_true(is.numeric(finalized_data$hours))
})

# Test 5: Full ETL pipeline for a single group
test_that("The full ETL process for a single group runs without errors", {
    config_path <- here::here("R", "config.yaml")

    expect_no_error({
        clean_timesheet_data(sample_raw_data_group1, group_number = 1, config_path = config_path)
    })
})

# Test 6: Combining multiple groups
test_that("Multiple groups are combined correctly", {
    cleaned_group1 <- data.frame(name = "John Doe", group_id = 1, stringsAsFactors = FALSE)
    cleaned_group2 <- data.frame(name = "Jane Smith", group_id = 2, stringsAsFactors = FALSE)

    cleaned_list <- list(cleaned_group1, cleaned_group2)

    combined_data <- combine_groups(cleaned_list)

    expect_equal(nrow(combined_data), 2)
    expect_equal(combined_data$group_id, c(1, 2))
})
