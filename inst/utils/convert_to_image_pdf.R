#!/usr/bin/env Rscript

# --- Description ---
# This script converts a directory of standard PDFs into "image-only" PDFs.
# It renders each page as a high-resolution image, applies enhancements, and
# then combines the images back into a new PDF. This is often a pre-processing
# step to improve Optical Character Recognition (OCR) results.

# --- Usage ---
# Rscript convert_to_image_pdf.R --input <path_to_input_dir> --output <path_to_output_dir>

# --- Dependencies ---
suppressPackageStartupMessages({
    library(pdftools)
    library(magick)
    library(fs)
    library(optparse)
})

# --- Main Function ---

#' Convert a single PDF to an image-only PDF
#'
#' @param pdf_path Path to the input PDF.
#' @param output_path Path to save the output image-only PDF.
#' @return Called for its side effect of creating a file.
convert_pdf_to_image_only <- function(pdf_path, output_path) {
    cli::cli_alert_info("Processing: {fs::path_file(pdf_path)}")

    # Sharpening kernels for image enhancement
    sharpen_kernel <- matrix(c(-1, -1, -1, -1, 9, -1, -1, -1, -1), nrow = 3)

    tryCatch(
        {
            # Render each page as a high-density image
            image_list <- pdftools::pdf_convert(
                pdf = pdf_path,
                format = "png",
                dpi = 300,
                verbose = FALSE
            )

            # Read images, enhance them, and join them
            img <- magick::image_read(image_list)
            img_enhanced <- img %>%
                magick::image_contrast(sharpen = 1) %>%
                magick::image_convolve(kernel = sharpen_kernel) %>%
                magick::image_normalize()

            # Write the enhanced images to a new PDF
            magick::image_write(img_enhanced, path = output_path, format = "pdf")

            cli::cli_alert_success("Successfully converted: {fs::path_file(output_path)}")
        },
        error = function(e) {
            cli::cli_alert_danger("Failed to convert {fs::path_file(pdf_path)}: {e$message}")
        }
    )
}

# --- Argument Parsing & Execution ---

option_list <- list(
    make_option(c("-i", "--input"),
        type = "character", default = NULL,
        help = "Input directory containing PDFs to convert.", metavar = "character"
    ),
    make_option(c("-o", "--output"),
        type = "character", default = NULL,
        help = "Output directory to save the converted PDFs.", metavar = "character"
    )
)

opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

if (is.null(opts$input) || is.null(opts$output)) {
    print_help(opt_parser)
    stop("Input and output directories must be specified.", call. = FALSE)
}

# Create output directory if it doesn't exist
if (!fs::dir_exists(opts$output)) {
    fs::dir_create(opts$output)
}

# Find all PDF files in the input directory
pdf_files <- fs::dir_ls(opts$input, regexp = "\\.pdf$", recurse = FALSE)

if (length(pdf_files) == 0) {
    cli::cli_alert_warning("No PDF files found in the input directory.")
} else {
    cli::cli_h1("Starting PDF to Image-Only Conversion")
    for (pdf_file in pdf_files) {
        output_file <- fs::path(opts$output, fs::path_file(pdf_file))
        convert_pdf_to_image_only(pdf_file, output_file)
    }
    cli::cli_h1("Conversion complete.")
}
