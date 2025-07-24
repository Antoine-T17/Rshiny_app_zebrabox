# R/raw_data_module.R

# ---- UI du module Raw Data ----
raw_data_ui <- function(id, primary_choices = c("Tracking Mode", "Quantization Mode"), 
                        secondary_choices = c("Light Dark Mode", "Vibration Mode")) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      box(
        title = "Raw Data Inputs",
        width = 12,
        wellPanel(
          tagAppendAttributes(
            fileInput(ns("raw_data_files"), "Upload Raw Data Files (Excel)", 
                      multiple = TRUE, accept = c(".csv", ".xlsx")),
            `aria-label` = "Upload CSV or Excel files"
          ),
          actionButton(ns("load_raw_data"), "Load Raw Data"),
          selectInput(ns("primary_mode"), "Primary Mode", 
                      choices = c("Select a mode" = "", primary_choices)),
          selectInput(ns("secondary_mode"), "Secondary Mode", 
                      choices = c("Select a mode" = "", secondary_choices))
        )
      )
    ),
    fluidRow(
      box(
        title = "Raw Data Preview",
        width = 12,
        uiOutput(ns("raw_data_tabs"))
      )
    )
  )
}

# ---- Server du module Raw Data ----
# ---- Server du module Raw Data ----
raw_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # ---- Fonction utilitaire pour lire les fichiers ----
    read_file <- function(path, file_name) {
      df <- if (grepl("\\.csv$", path)) {
        read.csv2(path, sep = ";", dec = ".")
      } else {
        readxl::read_excel(path)
      }
      attr(df, "file_name") <- file_name
      df
    }
    
    # ---- Désactiver le bouton si aucun fichier n'est sélectionné ----
    observe({
      shinyjs::toggleState("load_raw_data", !is.null(input$raw_data_files))
    })
    
    # ---- Charger les données brutes ----
    observeEvent(input$load_raw_data, {
      tryCatch({
        req(input$raw_data_files)
        rv$raw_data_list <- Map(read_file, input$raw_data_files$datapath, input$raw_data_files$name)
        showNotification("Raw data loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading files:", e$message), type = "error")
      })
    })
    
    # ---- Mettre à jour les modes ----
    observe({
      req(input$primary_mode, input$secondary_mode)
      # Gestion des NULL pour éviter l'erreur
      current_primary <- rv$primary_mode %||% ""  # Utilise "" si NULL
      current_secondary <- rv$secondary_mode %||% ""  # Utilise "" si NULL
      
      if (input$primary_mode != current_primary || input$secondary_mode != current_secondary) {
        rv$primary_mode <- input$primary_mode
        rv$secondary_mode <- input$secondary_mode
        message("Debug: Updated rv$primary_mode to ", rv$primary_mode)
        message("Debug: Updated rv$secondary_mode to ", rv$secondary_mode)
        showNotification("Modes updated successfully!", type = "message")
      }
    })
    
    # ---- Options communes pour DataTable ----
    dt_options <- list(
      pageLength = 25,
      autoWidth = TRUE,
      orderClasses = TRUE,
      scrollX = TRUE
    )
    
    # ---- Rendu des onglets de données brutes ----
    output$raw_data_tabs <- renderUI({
      req(rv$raw_data_list)
      if (length(rv$raw_data_list) == 0) {
        return(div("No raw data loaded yet. Please upload and load raw data files."))
      }
      
      tabs <- lapply(seq_along(rv$raw_data_list), function(i) {
        file_name <- attr(rv$raw_data_list[[i]], "file_name")
        tabPanel(
          title = file_name,
          DT::dataTableOutput(session$ns(paste0("raw_data_table_", i)))
        )
      })
      do.call(tabsetPanel, c(tabs, list(id = session$ns("raw_data_tabset"))))
    })
    
    # ---- Rendu des tables de données ----
    observe({
      req(rv$raw_data_list)
      lapply(seq_along(rv$raw_data_list), function(i) {
        output[[paste0("raw_data_table_", i)]] <- DT::renderDataTable({
          DT::datatable(rv$raw_data_list[[i]], filter = "top", options = dt_options)
        })
      })
    })
  })
}