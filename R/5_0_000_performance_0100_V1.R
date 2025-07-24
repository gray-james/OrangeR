# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PARALLEL PROCESSING IMPLEMENTATION ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Set Up a Parallel Processing Plan
#' @description Initializes a parallel backend for the R session using the
#'   `future` framework. This allows subsequent operations (like `future_lapply`)
#'   to run across multiple cores.
#'
#' @param n_cores The number of cores to use for parallel processing. Defaults
#'   to `future::availableCores() - 2` to leave resources for system operations.
#' @return This function is called for its side effect of setting the future plan.
#' @importFrom future plan multisession availableCores
#' @importFrom cli cli_alert_info
#' @noRd
setup_parallel_plan <- function(n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- future::availableCores(logical = FALSE) - 2
    if (n_cores < 1) n_cores <- 1
  }
  future::plan(future::multisession, workers = n_cores)
  cli::cli_alert_info("Parallel processing initialized with {n_cores} cores.")
}

#' @title Clean Up Parallel Processing Plan
#' @description Resets the `future` plan back to sequential processing. It's
#'   important to call this after parallel operations are complete to release
#'   the worker cores.
#' @noRd
cleanup_parallel_plan <- function() {
  future::plan("sequential")
  cli::cli_alert_info("Parallel processing plan reset to sequential.")
}

#' @title Apply a Function in Parallel to Data Chunks
#' @description A wrapper function that splits a data frame into chunks and
#'   applies a function to each chunk in parallel using `furrr`. This is useful
#'   for processing large datasets without overwhelming memory.
#'
#' @param data The data frame to process.
#' @param fun The function to apply to each chunk of the data.
#' @param ... Additional arguments to pass to `fun`.
#' @param n_cores The number of cores to use.
#'
#' @return A single data frame with the results from all chunks combined.
#' @importFrom furrr future_lapply
#' @noRd
run_in_parallel <- function(data, fun, ..., n_cores = NULL) {
  if (nrow(data) < 1000) {
    # For small datasets, parallel overhead isn't worth it
    return(fun(data, ...))
  }

  if (is.null(n_cores)) {
    n_cores <- future::availableCores(logical = FALSE) - 2
    if (n_cores < 1) n_cores <- 1
  }

  # Split data into chunks for parallel processing
  chunks <- split(data, ceiling(seq_len(nrow(data)) / ceiling(nrow(data) / n_cores)))

  # Process chunks in parallel
  setup_parallel_plan(n_cores)
  on.exit(cleanup_parallel_plan(), add = TRUE)

  results <- furrr::future_lapply(chunks, fun, ..., future.seed = TRUE)
  do.call(rbind, results)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROGRESS MONITORING ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Create a Progress Bar
#' @description Initializes and returns a `progress_bar` object from the `progress`
#'   package, configured with a standard format.
#'
#' @param total The total number of ticks to complete.
#' @param title A title for the progress bar.
#' @return A `progress_bar` object.
#' @importFrom progress progress_bar
#' @noRd
create_progress_monitor <- function(total, title = "Processing") {
  progress::progress_bar$new(
    total = total,
    format = paste0(title, " [:bar] :percent | :current/:total | ETA: :eta"),
    clear = FALSE,
    show_after = 1
  )
}

#' @title Update a Progress Bar
#' @description Ticks a progress bar and optionally displays current memory usage.
#' @param pb The `progress_bar` object to update.
#' @param step The number of ticks to advance the bar.
#' @param show_memory Logical. If `TRUE`, display current memory usage.
#' @noRd
update_progress_monitor <- function(pb, step = 1, show_memory = FALSE) {
  if (show_memory) {
    memory_mb <- round(as.numeric(pryr::mem_used()) / 1024^2, 1)
    pb$tick(step, tokens = list(memory = paste0(memory_mb, "MB")))
  } else {
    pb$tick(step)
  }
}

#' @title Monitor and Report Memory Usage
#' @description Reports the current memory usage and can force garbage collection
#'   if a specified threshold is exceeded.
#'
#' @param operation_name A string describing the operation just completed.
#' @param memory_threshold_mb The memory usage threshold in megabytes that
#'   will trigger a forced garbage collection.
#' @noRd
monitor_memory_usage <- function(operation_name, memory_threshold_mb = 1000) {
  memory_used_mb <- round(as.numeric(pryr::mem_used()) / 1024^2, 1)
  cli::cli_alert_info("Memory after {operation_name}: {memory_used_mb} MB")

  if (memory_used_mb > memory_threshold_mb) {
    cli::cli_alert_warning("High memory usage detected, forcing garbage collection...")
    gc()
  }
}
