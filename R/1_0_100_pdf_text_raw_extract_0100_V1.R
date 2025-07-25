# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ PDF TO DF FUNC----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Initiate the Transformation of Raw PDFs into Transparent Data
#' @description I believe that the journey to data clarity begins with a single,
#'   decisive step: extracting information from opaque sources. This function
#'   orchestrates the foundational 'Extract' step of our ETL pipeline. It is
#'   designed to read multiple PDF files, organized by distinct cohorts (groups),
#'   and transform their raw text into a structured, analysis-ready format.
#'
#' @details
#' My dream is a system where every piece of data, no matter how unstructured,
#' can be brought into a clear and auditable framework. This function is the
#' first realization of that dream. It operates with both efficiency and purpose:
#' \enumerate{
#'   \item It establishes a parallel processing environment, leveraging modern
#'     hardware to handle large volumes of data efficiently.
#'   \item It methodically processes each defined `group`, ensuring that files
#'     are correctly attributed to their specific cohort.
#'   \item For each group, it invokes the `read_pdf` helper function across all
#'     relevant files in parallel, using `furrr::future_lapply` for speed and
#'     scalability.
#'   \item The resulting data is then unified, cleaned of artifacts, and
#'     organized into a list of tibbles, ready for the next stage of validation.
#' }
#' This function is not just reading files; it's laying the first stone in the
#' foundation of a trusted data system.
#'
#' @param file_list A character vector of PDF file names, representing the raw,
#'   unprocessed source material.
#' @param groups A character vector defining the distinct data cohorts to be
#'   processed, ensuring each is handled according to its unique requirements.
#' @param base_path The sanctioned root directory where the source PDF files
#'   reside, providing a single point of reference for data ingestion.
#' @param workers The number of parallel workers dedicated to the extraction
#'   process. Defaults to a resource-conscious setting.
#'
#' @return A named list of tibbles, where each tibble represents a complete and
#'   structured dataset for one group, ready for further transformation and validation.
#'
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_lapply
#' @importFrom dplyr bind_rows mutate across where relocate filter select last
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @importFrom tidyr na_if
#'
#' @author James Gray (JG3288)
#' @export
read_pdf_groups <- function(file_list,
                            groups,
                            base_path,
                            workers = future::availableCores() - 2) {
  extract_groups <- list()

  # Set up parallel processing once, outside the loop
  future::plan(future::multisession, workers = workers)

  on.exit(future::plan(future::sequential)) # Ensure plan is reset on exit

  for (group in groups) {
    pattern <- paste0("^", group)
    group_name <- group

    # Filter files for the current group
    filter_to_group <- file_list[grepl(pattern, file_list)]

    if (length(filter_to_group) == 0) {
      warning("No files found for group: ", group, call. = FALSE)
      next # Skip to the next group
    }

    # Prepend base path to file names for reading
    full_file_paths <- file.path(base_path, filter_to_group)

    # Read files in parallel
    read_files <- furrr::future_lapply(full_file_paths, FUN = read_pdf)

    # Combine and clean the results for the group
    main_tbl <- read_files %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ gsub("\u00A0", " ", ., fixed = TRUE)),
        dplyr::across(dplyr::where(is.character), ~ tidyr::na_if(., ""))
      ) %>%
      dplyr::select(dplyr::where(~ !all(is.na(.)))) %>%
      dplyr::relocate(page_no, .after = "pdf_name") %>%
      dplyr::filter(Col1 != "PDF Error")

    extract_groups[[group_name]] <- main_tbl
  }

  return(extract_groups)
}

#' @title Read and Parse a Single PDF File
#' @description This is a helper function that reads the text content from a
#'   single PDF file, splits it into a structured tibble, and formats it for
#'   downstream processing.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item It uses `pdftools::pdf_text` to extract raw text from each page.
#'   \item It splits the text into individual lines and then into columns based
#'     on whitespace (2 or more spaces).
#'   \item To handle ragged data (rows with varying numbers of columns), it
#'     dynamically determines the maximum number of columns required across all
#'     lines and pads shorter lines with `NA`.
#'   \item It adds metadata, including the source PDF file name and page numbers.
#' }
#' It includes error handling to gracefully manage PDFs that cannot be read.
#'
#' @param file A character string specifying the full path to the PDF file.
#'
#' @return A tibble where each row corresponds to a line from the PDF. The
#'   tibble includes the parsed columns (`Col1`, `Col2`, etc.), the source
#'   `pdf_name`, and the `page_no`.
#'
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_split str_trim str_count
#' @importFrom dplyr bind_cols rename mutate relocate last as_tibble
#' @importFrom magrittr %>%
#' @noRd
#'
read_pdf <- function(file) {
  raw_txt <- tryCatch(
    pdftools::pdf_text(file),
    error = function(e) {
      warning("Failed to read PDF: ", file, "\nError: ", e$message, call. = FALSE)
      return("PDF Error")
    }
  )

  if (identical(raw_txt, "PDF Error")) {
    return(dplyr::tibble(Col1 = "PDF Error", pdf_name = basename(file), page_no = 1))
  }

  # Unlist the text from all pages and create a data frame with page numbers
  pages <- seq_along(raw_txt)
  lines_by_page <- stringr::str_split(raw_txt, "[\\r\\n]+")

  # Create a long-format tibble with one row per line
  split_txt_tbl <- dplyr::tibble(
    page_no = rep(pages, times = sapply(lines_by_page, length)),
    line_text = unlist(lines_by_page)
  ) %>%
    dplyr::mutate(line_text = stringr::str_trim(line_text)) %>%
    dplyr::filter(line_text != "")

  if (nrow(split_txt_tbl) == 0) {
    return(dplyr::tibble(pdf_name = basename(file))) # Return empty tibble with metadata
  }

  # Determine max number of columns needed by finding the line with the most "splits"
  # A split is defined by 2 or more spaces.
  max_cols <- max(stringr::str_count(split_txt_tbl$line_text, "\\s{2,}")) + 1

  # Split each line into the determined number of columns
  main_tbl_matrix <- stringr::str_split(split_txt_tbl$line_text, "\\s{2,}", n = max_cols)

  # Pad rows with fewer than max_cols with NAs
  main_tbl_matrix <- do.call(rbind, lapply(main_tbl_matrix, `length<-`, max_cols))


  # Convert matrix to a tibble and add metadata
  main_tbl <- main_tbl_matrix %>%
    dplyr::as_tibble(.name_repair = ~ paste0("Col", seq_len(ncol(.)))) %>%
    dplyr::bind_cols(page_no = split_txt_tbl$page_no) %>%
    dplyr::mutate(pdf_name = basename(file)) %>%
    dplyr::relocate(pdf_name, page_no)

  return(main_tbl)
}
