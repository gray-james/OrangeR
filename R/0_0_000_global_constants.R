#
#' @title Define Global Constants for the ETL Process
#'
#' @description This script establishes the core constants and configuration
#'   settings required for the time data ETL pipeline. It is the central
#'   location for defining file paths, group identifiers, and initial data
#'   structures. To ensure portability and align with R package best practices,
#'   this script avoids hardcoded absolute paths and `setwd()`.
#'
#' @details
#' The constants are organized into several key sections:
#' \itemize{
#'   \item \strong{Paths}: Defines the input (source) and output (save)
#'     directories for the ETL process. These should be managed via a
#'     configuration file or environment variables in a production setting.
#'   \item \strong{Matter Specific Extract}: Specifies the distinct groups or
#'     clients whose timesheet data is being processed (e.g., "Pinnacle Rhodes").
#'   \item \strong{Initialise Global Variables}: Pre-allocates list structures
#'     to hold data as it moves through the different stages of the ETL pipeline
#'     (raw, extracted, cleaned, QA). This is done for clarity and organization.
#'   \item \strong{Matter Specific Export}: Defines naming conventions and data
#'     structures for the final output files.
#' }
#'
#' @section Path Management:
#' It is highly recommended to use a package like `here` or `config` to manage
#' paths and other environmental settings. For this package, path configuration
#' will be centralized in the `config.yaml` file.
#'
#' @author James Gray (JG3288)
#' @seealso \code{\link{config.yaml}}, \code{here::here()}
#' @export

# The following constants would typically be loaded from a config file
# (e.g., config.yaml) or set as environment variables.

# PATHS ----
# Example using here() package for robust path management
# raw_time_path <- here::here("inst", "extdata", "time_records")
# save_path <- here::here("data-raw")
# source_path <- here::here("R")

# For this refactoring, we will assume these paths are set by a master script
# or a configuration loader at the start of the pipeline.
raw_time_path <- getOption("OrangeR.raw_time_path", "data-raw/timesheets")
save_path <- getOption("OrangeR.save_path", "data-raw/processed")
source_path <- getOption("OrangeR.source_path", "R")


# SCRIPT INCLUSION ----
# Dynamically list scripts to be sourced, based on the file naming convention.
included_scripts <- list.files(
    path = source_path,
    pattern = "^[0-6]",
    full.names = TRUE
)

# MATTER SPECIFIC EXTRACT ----
# Define the distinct groups for which data will be processed.
# This should be loaded from a configuration file.
groups <- getOption("OrangeR.groups", c("Pinnacle Rhodes", "Wentworth Point", "Wolli Creek"))

# INITIALISE GLOBAL VARIABLES ----
# These lists will store data at various stages of the ETL process.
# This approach of using global variables is discouraged in packages.
# A better approach would be to pass data through functions.
# For now, we retain the structure but acknowledge the limitation.
file_list <- list.files(path = raw_time_path, pattern = "\\.pdf$", recursive = TRUE)

raw_folder_groups <- list()
extracted_folder_groups <- list()
cleaned_folder_groups <- list()
QA_folder_groups <- list()

# MATTER SPECIFIC EXPORT ----
base_file_name <- getOption("OrangeR.base_file_name", "ORANGE_OC01-TIME")

# Single File Export Variables
# This is pre-allocating a structure, which is not ideal.
# It's better to create data frames when they are actually needed.
dfs_to_export <- c(combined_time_data = data.frame())
sheet_names <- c("time_transformed")

# Multi File Export Variables
grouped_dfs_to_export <- list()
