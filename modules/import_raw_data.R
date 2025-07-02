# R/raw_data_module.R

# UI du module Raw Data
raw_data_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Raw Data Inputs",
      width = 4,
      fileInput(ns("raw_data_files"), "Upload Raw Data Files (Excel)", multiple = TRUE, accept = c(".csv", ".xlsx")),
      actionButton(ns("load_raw_data"), "Load Raw Data"),
      div(style = "margin-bottom: 50px;"), # Ajoute une marge de 15px pour imiter l'espacement par défaut
      selectInput(ns("primary_mode"), "Primary Mode", choices = c("", "Tracking Mode", "Quantization Mode"), selected = ""),
      selectInput(ns("secondary_mode"), "Secondary Mode", choices = c("", "Light Dark Mode", "Vibration Mode"), selected = "")
    ),
    box(
      title = "Raw Data Preview",
      width = 8,
      uiOutput(ns("raw_data_tabs"))
    )
  )
}

# Server du module Raw Data
raw_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Load Raw Data
    observeEvent(input$load_raw_data, {
      tryCatch({
        req(input$raw_data_files)
        rv$raw_data_list <- lapply(seq_along(input$raw_data_files$datapath), function(i) {
          path <- input$raw_data_files$datapath[i]
          file_name <- input$raw_data_files$name[i]
          df <- if (grepl("\\.csv$", path)) {
            read.csv2(path, sep = ";", dec = ".")
          } else {
            readxl::read_excel(path)
          }
          # Ajouter un attribut pour le nom du fichier
          attr(df, "file_name") <- file_name
          df
        })
        showNotification("Raw data loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Stocker les valeurs des modes dans rv
    observe({
      req(input$primary_mode, input$secondary_mode)
      if (input$primary_mode != "" && input$secondary_mode != "") {
        message("Debug: Updating rv$primary_mode to ", input$primary_mode)
        message("Debug: Updating rv$secondary_mode to ", input$secondary_mode)
        rv$primary_mode <- input$primary_mode
        rv$secondary_mode <- input$secondary_mode
        showNotification("Modes updated successfully!", type = "message")
      }
    })
    
    # Render Raw Data Tabs
    output$raw_data_tabs <- renderUI({
      if (is.null(rv$raw_data_list) || length(rv$raw_data_list) == 0) {
        return(div("No raw data loaded yet. Please upload and load raw data files."))
      }
      
      tabs <- lapply(seq_along(rv$raw_data_list), function(i) {
        file_name <- attr(rv$raw_data_list[[i]], "file_name")
        tabPanel(
          title = paste("File", i),
          DT::dataTableOutput(session$ns(paste0("raw_data_table_", i)))
        )
      })
      do.call(tabsetPanel, c(tabs, list(id = session$ns("raw_data_tabset"))))
    })
    
    # Render Raw Data Tables (one per file)
    observe({
      req(rv$raw_data_list)
      lapply(seq_along(rv$raw_data_list), function(i) {
        output[[paste0("raw_data_table_", i)]] <- DT::renderDataTable({
          DT::datatable(
            rv$raw_data_list[[i]],
            filter = "top",
            options = list(
              pageLength = 10,
              autoWidth = TRUE,
              orderClasses = TRUE,
              scrollX = TRUE
            )
          )
        })
      })
    })
  })
}