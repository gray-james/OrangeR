# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONFIGURATION SYSTEM FOR DIFFERENT CLEANING GROUPS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#
#' @title Retrieve a Fair and Consistent Configuration for a Data Group
#' @description I believe that fairness requires consistency. This function serves as
#'   a centralized configuration hub, retrieving a tailored set of rules and
#'   regex patterns for a specific data processing group. By applying a
#'   consistent, pre-defined configuration, we ensure that data from each cohort
#'   is treated with the same rigorous standards, which is a cornerstone of
#'   building a trustworthy and equitable data system.
#'
#' @details
#' The configuration returned by this function is the blueprint for achieving
#' data integrity for a specific group. It contains all the necessary parameters
#' for the fair extraction and cleaning of data. This function-based approach
#' .  not only promotes manageable and extensible cleaning rules but also provides
#'   an auditable trail of the logic applied to each data source.
#'
#' The returned list contains the following critical elements:
#' \itemize{
#'   \item \strong{name_extraction_rules}: The sanctioned logic for identifying
#'     employee names, ensuring consistent recognition.
#'   \item \strong{date_patterns}: A vector of approved regex patterns to
#'     accurately identify all valid date formats.
#'   \item \strong{time_patterns}: A vector of regex patterns for identifying
#'     time entries, critical for accurate hour logging.
#'   \item \strong{work_time_pattern}: The specific regex for extracting total
#'     worked hours, a key component of fair pay.
#'   \item \strong{work_time_condition}: A rule that governs when the
#'     `work_time_pattern` is applied, adding a layer of validation.
#'   \item \strong{total_type}: The expected granularity of the total (e.g., "weekly",
#'     "fortnightly"), which informs validation checks.
#'   \item \strong{total_location}: A rule indicating where to locate the total
#'     value, ensuring it is never missed.
#' }
#'
#' @param group_number An integer or character identifying the data cohort
#'   (e.g., 1, 2, or 3) whose configuration is needed. This ensures the
#'   correct, pre-approved rules are applied.
#'
#' @return A named list containing the complete, auditable configuration for
#'   the requested group. It returns `NULL` if the group is not recognized,
#'   preventing the application of incorrect rules.
#'
#' @author James Gray (JG3288)
#' @export
#'
#' @examples
#' \dontrun{
#' # My dream is a system where every data group is treated with fairness.
#' # Here is how we retrieve the specific rules for two different groups.
#'
#' # Get the tailored configuration for Group 1
#' config_g1 <- get_group_config(1)
#'
#' # Get the tailored configuration for Group 2
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
