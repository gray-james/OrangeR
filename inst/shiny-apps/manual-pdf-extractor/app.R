# Load necessary libraries
library(shiny)
library(DT)

# Source the helper functions from the R subdirectory
source("R/helpers.R")

# --- User Interface ---
ui <- fluidPage(
    titlePanel("PDF to Data Frame Grid Converter"),
    sidebarLayout(
        sidebarPanel(
            fileInput("pdf_files", "Choose PDF File(s)", accept = ".pdf", multiple = TRUE),
            hr(),
            h4("Grid Parameters"),
            sliderInput("y_threshold", "Row Tolerance (Y-Threshold)", min = 1, max = 30, value = 12, step = 1),
            sliderInput("cm_interval", "Column Width (cm)", min = 0.1, max = 2, value = 0.5, step = 0.1),
            numericInput("num_columns", "Number of Columns", value = 50, min = 10, max = 200),
            hr(),
            actionButton("process", "Process PDFs", icon = icon("cogs"), class = "btn-primary")
        ),
        mainPanel(
            DT::dataTableOutput("output_table")
        )
    )
)

# --- Server Logic ---
server <- function(input, output) {
    # Reactive value to store the processed data
    processed_data <- eventReactive(input$process, {
        req(input$pdf_files)

        # Show a notification that processing is starting
        showNotification("Processing PDFs...", type = "message", duration = NULL, id = "processing_note")
        on.exit(removeNotification("processing_note"), add = TRUE)

        process_multiple_pdfs(
            pdf_files = input$pdf_files$datapath,
            pdf_names = input$pdf_files$name,
            cm_interval = input$cm_interval,
            y_threshold = input$y_threshold,
            num_columns = input$num_columns
        )
    })

    # Render the data table
    output$output_table <- DT::renderDataTable({
        DT::datatable(
            processed_data(),
            options = list(
                pageLength = 25,
                lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
                scrollX = TRUE
            ),
            filter = "top",
            class = "cell-border stripe"
        )
    })
}

# --- Run Application ---
shinyApp(ui = ui, server = server)
