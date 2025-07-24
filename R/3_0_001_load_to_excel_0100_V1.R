# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT DATAFRAMES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Export Data Frames to a Styled Excel File
#' @description Exports a list of data frames to a single, styled Excel workbook.
#'   Each data frame is written to a separate sheet. The function includes
#'   automatic versioning to prevent overwriting existing files.
#'
#' @details
#' This function creates a new Excel workbook and iterates through the provided
#' list of data frames (`dfs_to_export`). For each data frame, it calls the
#' `add_styled_worksheet` helper to create a new sheet, write the data, and
#' apply a standard set of professional styles (e.g., table formatting, auto
#' column widths, frozen panes).
#'
#' The output file is automatically versioned (e.g., `_V0001`, `_V0002`) based on
#' files already present in the `save_path`.
#'
#' @param dfs_to_export A named list of data frames to export. The names of the
#'   list elements will become the sheet names in the Excel file.
#' @param base_file_name The base name for the output file (e.g., "TIME_ETL_OUTPUT").
#' @param save_path The directory where the Excel file will be saved.
#' @param table_start_row The row in the Excel sheet where the data table should start.
#'
#' @return Invisibly returns the path to the newly created Excel file.
#' @import openxlsx
#' @author James Gray (JG3288)
#' @export
export_single_file <- function(dfs_to_export, base_file_name, save_path, table_start_row = 1) {
  wb <- openxlsx::createWorkbook()

  for (sheet_name in names(dfs_to_export)) {
    add_styled_worksheet(
      wb,
      df = dfs_to_export[[sheet_name]],
      sheet_name = sheet_name,
      table_start_row = table_start_row
    )
  }

  version <- get_next_version_number(save_path, base_file_name)
  file_name <- file.path(save_path, paste0(base_file_name, "_V", sprintf("%04d", version), ".xlsx"))

  openxlsx::saveWorkbook(wb, file = file_name, overwrite = FALSE)
  return(invisible(file_name))
}

#' @title Export Grouped Data to Multiple Styled Excel Files
#' @description Exports a nested list of data frames to multiple, styled Excel
#'   workbooks, one for each top-level group.
#'
#' @param grouped_dfs_to_export A named, nested list of data frames. The top-level
#'   names represent groups and will be used in the file names.
#' @param base_file_name The base name for the output files.
#' @param folder_save_path The root directory where the Excel files will be saved.
#'
#' @return Invisibly returns a vector of file paths to the newly created Excel files.
#' @export
export_multiple_files <- function(grouped_dfs_to_export, base_file_name, folder_save_path) {
  output_files <- c()
  for (group_name in names(grouped_dfs_to_export)) {
    dfs_to_export <- grouped_dfs_to_export[[group_name]]

    save_path_group <- file.path(folder_save_path, group_name)
    if (!dir.exists(save_path_group)) {
      dir.create(save_path_group, recursive = TRUE)
    }

    wb <- openxlsx::createWorkbook()

    for (sheet_name in names(dfs_to_export)) {
      add_styled_worksheet(wb, dfs_to_export[[sheet_name]], sheet_name)
    }

    version <- get_next_version_number(save_path_group, paste0(base_file_name, "_", group_name))
    file_name <- file.path(save_path_group, paste0(base_file_name, "_", group_name, "_V", sprintf("%04d", version), ".xlsx"))

    openxlsx::saveWorkbook(wb, file = file_name, overwrite = FALSE)
    output_files <- c(output_files, file_name)
  }
  return(invisible(output_files))
}

#' @title Add a Styled Worksheet to an Excel Workbook
#' @description A helper function that adds a single, styled worksheet to an
#'   `openxlsx` workbook object.
#' @noRd
add_styled_worksheet <- function(wb, df, sheet_name, table_start_row = 1) {
  openxlsx::addWorksheet(wb, sheet_name)

  # Define styles
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )
  date_style <- openxlsx::createStyle(
    fontName = "Courier New",
    fontSize = 10,
    numFmt = "ddd dd-mmm-yy",
    textDecoration = "bold"
  )

  # Write the data and create a table
  openxlsx::writeData(wb, sheet = sheet_name, x = df, startRow = table_start_row)
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = df,
    tableName = paste0(make.names(sheet_name), "_tbl"),
    startRow = table_start_row,
    withFilter = TRUE,
    headerStyle = header_style,
    tableStyle = "TableStyleMedium16"
  )

  # Apply custom formatting
  openxlsx::setRowHeights(wb, sheet = sheet_name, rows = table_start_row, heights = 45)

  date_columns <- grep("date", colnames(df), ignore.case = TRUE)
  if (length(date_columns) > 0) {
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = date_style,
      cols = date_columns,
      rows = (table_start_row + 1):(table_start_row + nrow(df)),
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = table_start_row + 1)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
  openxlsx::showGridLines(wb, sheet = sheet_name, showGridLines = FALSE)
}

#' @title Get Next Version Number for a File
#' @description A helper function that scans a directory for files matching a
#'   base name and determines the next sequential version number.
#' @noRd
get_next_version_number <- function(path, base_name) {
  # Escape any special regex characters in the base name
  safe_base_name <- gsub("([\\.\\|\\(\\)\\[\\]\\{\\}\\^\\$\\*\\+\\?])", "\\\\\\1", base_name)

  # List files matching the base name and version pattern
  existing_files <- list.files(
    path = path,
    pattern = paste0("^", safe_base_name, "_V\\d{4}\\.xlsx$"),
    ignore.case = TRUE
  )

  if (length(existing_files) == 0) {
    # If no files exist, start at version 1
    return(1)
  } else {
    # Extract the version numbers from the file names
    versions <- stringr::str_extract(existing_files, "(?<=_V)\\d{4}(?=\\.xlsx)")
    # Convert to numeric, find the max, and add 1 for the new version
    next_version <- max(as.numeric(versions), na.rm = TRUE) + 1
    return(next_version)
  }
}
