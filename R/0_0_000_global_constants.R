#' @title Establish Foundational Constants for a Trusted ETL Process
#'
#' @description This script lays the groundwork for our commitment to data integrity.
#'   It establishes the core constants and configuration settings that govern the
#'   time data ETL pipeline, serving as the central authority for file paths,
#'   group identifiers, and data structures. By centralizing these definitions,
#'   we ensure that the entire process is transparent, auditable, and aligned
#'   with a single standard of truth.
#'
#' @details
#' The constants defined here are the bedrock of a fair and accurate system.
#' They are organized to provide clarity and control over the pipeline:
#' \itemize{
#'   \item \strong{Paths}: Defines the sanctioned input (source) and output (save)
#'     locations. I believe that disciplined path management is the first step
#'     toward a trustworthy data workflow.
#'   \item \strong{Matter Specific Extract}: Specifies the distinct data cohorts
#'     or client groups ("Pinnacle Rhodes," etc.) to be processed. This ensures
#'     that each group's data is handled with appropriate, specific rules.
#'   \item \strong{Initialise Global Variables}: Pre-allocates list structures
#'     to hold data as it is progressively refined through the ETL pipeline
#'     (raw, extracted, cleaned, QA). This architectural choice enhances clarity
#'     and predictability.
#'   \item \strong{Matter Specific Export}: Defines the naming conventions and
#'     schemas for the final, verified datasets, ensuring a consistent and
#'     reliable output.
#' }
#'
#' @section Path Management and Configuration:
#' My dream is a system where configuration is fully externalized, making the
#' pipeline adaptable and robust. In line with this, path management will be
#'   driven by the `config.yaml` file, and I advocate for using packages like
#'   `here` or `config` to manage environmental settings in a production environment.
#'
#' @author James Gray (JG3288)
#' @seealso \code{\link{config.yaml}} for external configuration.
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
