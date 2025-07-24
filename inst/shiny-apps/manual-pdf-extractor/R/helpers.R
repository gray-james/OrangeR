# Load necessary libraries
library(dplyr)
library(pdftools)

#' @title Convert Centimeters to Points
#' @description Converts a value in centimeters to typographic points.
#' @param cm A numeric value in centimeters.
#' @return A numeric value in points.
#' @noRd
cm_to_points <- function(cm) {
    return(cm * 28.35)
}

#' @title Create a Grid from PDF Text Data
#' @description This is the core logic for the manual extractor. It takes text
#'   data with x/y coordinates from a single PDF page and arranges it into a
#'   grid-based data frame, attempting to reconstruct the visual layout.
#' @param page_data A data frame from `pdftools::pdf_data()` for a single page.
#' @param file_source The name of the source PDF file.
#' @param page_number The page number.
#' @param cm_interval The grid spacing in cm.
#' @param y_threshold The tolerance for grouping text on the same line.
#' @param num_columns The total number of grid columns to create.
#' @return A data frame representing the grid.
#' @noRd
create_manual_grid <- function(page_data, file_source, page_number, cm_interval = 0.5, y_threshold = 10, num_columns = 50) {
    interval_points <- cm_to_points(cm_interval)
    grid <- list()

    page_data <- page_data %>% arrange(y, x)

    place_in_grid <- function(text_row, grid, interval_points, y_threshold, num_columns) {
        x_pos <- text_row$x
        text <- text_row$text
        y_pos <- text_row$y

        if (length(grid) == 0) {
            new_row <- list(y = y_pos, columns = rep("", num_columns))
            grid <- append(grid, list(new_row))
        }

        row_index <- which(sapply(grid, function(r) {
            if (is.list(r)) {
                return(abs(r$y - y_pos) < y_threshold)
            } else {
                return(FALSE)
            }
        }))

        if (length(row_index) == 0) {
            new_row <- list(y = y_pos, columns = rep("", num_columns))
            grid <- append(grid, list(new_row))
            row_index <- length(grid)
        }

        column_index <- ceiling(x_pos / interval_points)

        if (column_index > 0 && column_index <= num_columns) {
            grid[[row_index]]$columns[column_index] <- text
        }

        return(grid)
    }

    for (i in seq_len(nrow(page_data))) {
        text_row <- page_data[i, ]
        grid <- place_in_grid(text_row, grid, interval_points, y_threshold, num_columns)
    }

    if (length(grid) == 0) {
        return(data.frame(matrix(ncol = num_columns + 2, nrow = 0)))
    }

    grid_df <- do.call(rbind, lapply(grid, function(row) {
        c(file_source, page_number, row$columns)
    }))

    colnames(grid_df) <- c("File Source", "Page Number", paste0("V", seq_len(ncol(grid_df) - 2)))

    return(grid_df)
}

#' @title Process All Pages of a Single PDF
#' @description Iterates through all pages of a PDF, calling `create_manual_grid`
#'   on each one and combining the results.
#' @param file_path The full path to the PDF file.
#' @param file_name The name of the PDF file.
#' @return A single data frame containing the grid data for the entire PDF.
#' @noRd
process_single_pdf <- function(file_path, file_name, cm_interval = 0.5, y_threshold = 10, num_columns = 50) {
    pdf_info <- pdftools::pdf_data(file_path)
    all_pages_data <- list()

    for (page_num in seq_along(pdf_info)) {
        page_data <- pdf_info[[page_num]]
        if (nrow(page_data) > 0) {
            page_grid <- create_manual_grid(page_data, file_name, page_num, cm_interval, y_threshold, num_columns)
            all_pages_data <- append(all_pages_data, list(page_grid))
        }
    }

    if (length(all_pages_data) == 0) {
        return(data.frame(matrix(ncol = num_columns + 2, nrow = 0)))
    }

    do.call(rbind, all_pages_data)
}

#' @title Process Multiple PDF Files
#' @description The main processing wrapper that iterates through a list of PDF
#'   files and processes them.
#' @param pdf_files A vector of file paths.
#' @param pdf_names A vector of file names.
#' @return A single data frame containing the combined grid data from all processed PDFs.
#' @noRd
process_multiple_pdfs <- function(pdf_files, pdf_names, cm_interval = 0.5, y_threshold = 10, num_columns = 50) {
    all_pdfs_data <- list()

    for (i in seq_along(pdf_files)) {
        cat("Processing:", pdf_names[i], "\n")
        pdf_data <- process_single_pdf(pdf_files[i], pdf_names[i], cm_interval, y_threshold, num_columns)
        if (nrow(pdf_data) > 0) {
            all_pdfs_data <- append(all_pdfs_data, list(pdf_data))
        }
    }

    if (length(all_pdfs_data) == 0) {
        return(data.frame())
    }

    do.call(rbind, all_pdfs_data)
}
