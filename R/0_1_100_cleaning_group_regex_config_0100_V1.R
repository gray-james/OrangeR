# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONFIGURATION SYSTEM FOR DIFFERENT CLEANING GROUPS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
#' @title Retrieve Configuration for a Specific Data Group
#' @description This function acts as a centralized configuration hub, providing
#'   a set of rules and regex patterns tailored for a specific data processing
#'   group. Based on the `group_number`, it returns a list containing all the
#'   necessary parameters for extracting and cleaning data for that group.
#'
#' @details
#' The configuration list is structured to cover various aspects of the ETL
#' process, including rules for name extraction, and patterns for dates, times,
#' and work hours. This function-based approach allows for easy management and
#' extension of cleaning rules for new data sources.
#'
#' The returned list contains the following key elements:
#' \itemize{
#'   \item \strong{name_extraction_rules}: A list defining the logic for
#'     extracting employee names.
#'   \item \strong{date_patterns}: A character vector of regex patterns to
#'     identify dates.
#'   \item \strong{time_patterns}: A character vector of regex patterns to
#'     identify time entries.
#'   \item \strong{work_time_pattern}: A regex pattern to extract worked hours.
#'   \item \strong{work_time_condition}: A condition specifying when to apply
#'     the `work_time_pattern` (e.g., only when a date is present).
#'   \item \strong{total_type}: The type of total to expect (e.g., "weekly",
#'     "fortnightly").
#'   \item \strong{total_location}: A rule indicating where to find the total
#'     value in the data.
#' }
#'
#' @param group_number An integer or character (1, 2, or 3) specifying the
#'   configuration group to retrieve.
#'
#' @return A named list containing the configuration settings for the requested
#'   group. If the group number is not found, it returns `NULL`.
#'
#' @author James Gray (JG3288)
#' @export
#'
#' @examples
#' \dontrun{
#' # Get configuration for Group 1
#' config_g1 <- get_group_config(1)
#'
#' # Get configuration for Group 2
#' config_g2 <- get_group_config("2")
#' }
get_group_config <- function(group_number, config_path = "R/regex_patterns.yaml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required to read configurations. Please install it.", call. = FALSE)
  }
  if (!file.exists(config_path)) {
    stop("Configuration file not found at path: ", config_path, call. = FALSE)
  }

  all_configs <- yaml::read_yaml(config_path)
  group_name <- paste0("group_", group_number, "_config")

  if (!group_name %in% names(all_configs$group_definitions)) {
    warning("No configuration found for group: ", group_number)
    return(NULL)
  }

  group_spec <- all_configs$group_definitions[[group_name]]
  resolved_config <- resolve_pattern_references(group_spec, all_configs$patterns)

  return(resolved_config)
}

#' @title Resolve Pattern References in a Configuration
#' @description A helper function that walks through a group's configuration
#'   specification and replaces pattern references (e.g., `_ref` or `_refs`)
#'   with the actual regex patterns defined in the `patterns` section of the
#'   YAML file.
#'
#' @param group_spec A list containing the configuration for a single group,
#'   which may contain unresolved references.
#' @param all_patterns A list containing all defined regex patterns.
#'
#' @return A list with all `_ref` and `_refs` placeholders replaced by the
#'   actual regex pattern strings.
#' @noRd
resolve_pattern_references <- function(group_spec, all_patterns) {
  # This is a recursive function to handle nested lists
  if (!is.list(group_spec)) {
    return(group_spec)
  }

  resolved_list <- list()
  for (name in names(group_spec)) {
    item <- group_spec[[name]]
    if (grepl("_ref$", name)) {
      # Single reference: "time_pattern_ref" -> "time_pattern"
      new_name <- sub("_ref$", "", name)
      resolved_list[[new_name]] <- get_pattern_from_ref(item, all_patterns)
    } else if (grepl("_refs$", name)) {
      # Multiple references: "date_patterns_refs" -> "date_patterns"
      new_name <- sub("_refs$", "", name)
      resolved_list[[new_name]] <- sapply(item, get_pattern_from_ref, all_patterns, simplify = FALSE, USE.NAMES = FALSE) |> unlist()
    } else if (is.list(item)) {
      # Recursively resolve for nested lists
      resolved_list[[name]] <- resolve_pattern_references(item, all_patterns)
    } else {
      # Keep non-reference items as they are
      resolved_list[[name]] <- item
    }
  }
  return(resolved_list)
}

#' @title Retrieve a Specific Regex Pattern from a Reference
#' @description Given a reference string like "dates.dd_mm_yyyy_slash", this
#'   function navigates the `all_patterns` list to find and return the
#'   corresponding regex string.
#'
#' @param ref_string The reference string (e.g., "category.pattern_name").
#' @param all_patterns The complete list of patterns.
#'
#' @return The regex string for the given reference.
#' @noRd
get_pattern_from_ref <- function(ref_string, all_patterns) {
  path <- strsplit(ref_string, "\\.")[[1]]
  # Navigate the list structure, e.g., all_patterns$dates$dd_mm_yyyy_slash
  pattern_entry <- purrr::pluck(all_patterns, !!!path)

  if (is.null(pattern_entry) || !is.list(pattern_entry) || is.null(pattern_entry$regex)) {
    warning("Could not resolve pattern reference: ", ref_string, call. = FALSE)
    return(ref_string) # Return the reference itself if not found
  }
  return(pattern_entry$regex)
}
