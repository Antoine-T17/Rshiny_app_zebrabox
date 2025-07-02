# UI pour Quantization Mode, Light-Dark Mode
processing_qm_ldm_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Match Raw Data to Plate Plans (Quantization Mode, Light-Dark Mode)",
      width = 5,
      h4("Associate Raw Data with Plate Plans"),
      uiOutput(ns("file_plate_selectors")),
      div(style = "margin-bottom: 10px;"),
      actionButton(ns("confirm_mapping"), "Confirm Mapping"),
      div(style = "margin-bottom: 20px;"),
      
      h4("Additional Processing Parameters"),
      fileInput(ns("period_file"), "Upload Period Transitions File (Excel)", accept = c(".xlsx")),
      selectInput(ns("start_column_unit"), "Unit of 'start' Column", choices = c("Hours (h)" = "h", "Minutes (m)" = "m", "Seconds (s)" = "s"), selected = "m"),
      div(style = "margin-bottom: 10px;"),
      actionButton(ns("run_processing"), "Run Full Processing")
    ),
    box(
      title = "Processing Results",
      width = 7,
      tabsetPanel(
        tabPanel("Console Output", 
                 div(
                   style = "background-color: #f5f5f5; border: 1px solid #ccc; padding: 10px; height: 600px; overflow-y: auto; font-family: monospace;",
                   uiOutput(ns("console_output"))
                 )
        ),
        tabPanel("Data with Periods", DT::dataTableOutput(ns("data_with_periods_table"))),
        tabPanel("Period Boundaries", DT::dataTableOutput(ns("period_boundaries_table"))),
        tabPanel("Boundary Associations", DT::dataTableOutput(ns("boundary_associations_table")))
      ),
      downloadButton(ns("download_all_results"), "Download All Results (ZIP)")
    )
  )
}

# Server pour Quantization Mode, Light-Dark Mode
processing_qm_ldm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    console_messages <- reactiveVal(character())
    
    add_console_message <- function(message) {
      current_messages <- console_messages()
      console_messages(c(current_messages, message))
    }
    
    output$file_plate_selectors <- renderUI({
      req(rv$raw_data_list, rv$plate_plan_df_list)
      n_files <- length(rv$raw_data_list)
      n_plates <- length(rv$plate_plan_df_list)
      if (n_files != n_plates) {
        add_console_message(sprintf("Error: Number of raw data files (%d) must match number of plate plans (%d).", n_files, n_plates))
        return(NULL)
      }
      
      file_names <- sapply(rv$raw_data_list, function(df) attr(df, "file_name"))
      plate_ids <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
      
      lapply(seq_along(file_names), function(i) {
        fluidRow(
          column(6, strong(file_names[i])),
          column(6, selectInput(ns(paste0("plate_select_", i)), label = NULL, choices = plate_ids, selected = plate_ids[i]))
        )
      })
    })
    
    observeEvent(input$confirm_mapping, {
      tryCatch({
        req(rv$raw_data_list, rv$plate_plan_df_list)
        file_names <- sapply(rv$raw_data_list, function(df) attr(df, "file_name"))
        plate_ids <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
        
        selected_plates <- sapply(seq_along(file_names), function(i) {
          input[[paste0("plate_select_", i)]]
        })
        
        mapping_df <- data.frame(
          Raw_Data_File = file_names,
          Plate_ID = selected_plates,
          stringsAsFactors = FALSE
        )
        
        if (length(unique(mapping_df$Plate_ID)) != nrow(mapping_df)) {
          stop("Each raw data file must be associated with a unique Plate ID.")
        }
        
        ordered_plate_plans <- list()
        for (i in 1:nrow(mapping_df)) {
          idx <- which(plate_ids == mapping_df$Plate_ID[i])
          if (length(idx) == 0) stop(sprintf("Plate ID %s not found.", mapping_df$Plate_ID[i]))
          ordered_plate_plans[[i]] <- rv$plate_plan_df_list[[idx]]
        }
        
        rv$ordered_plate_plans <- ordered_plate_plans
        rv$mapping <- mapping_df
        
        add_console_message("Mapping confirmed successfully!")
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", e$message))
      })
    })
    
    processing_results <- reactiveVal(NULL)
    
    observeEvent(input$run_processing, {
      tryCatch({
        req(rv$raw_data_list, rv$ordered_plate_plans, input$period_file)
        
        convert_numeric_cols <- function(df, cols) {
          for (col in intersect(names(df), cols)) {
            df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
          }
          df
        }
        
        # Lire le fichier Excel des périodes
        period_df <- readxl::read_excel(input$period_file$datapath)
        req("start" %in% colnames(period_df), "transition" %in% colnames(period_df))
        period_df$start <- as.numeric(period_df$start)
        
        start_unit <- input$start_column_unit
        n_plates <- length(rv$ordered_plate_plans)
        
        add_console_message("Starting data extraction, enrichment, and period assignment...")
        
        extracted_data_list <- rv$raw_data_list
        plate_plans <- rv$ordered_plate_plans
        nplates <- length(plate_plans)
        
        data_with_periods_list <- list()
        period_boundaries_list <- list()
        boundary_associations_list <- list()
        
        potential_numeric_cols <- c("start", "inact", "inadist", "inadur",
                                    "smlct", "smldist", "smldur",
                                    "larct", "lardur", "lardist",
                                    "emptyct", "emptydur",
                                    "totaldist", "totaldur", "totalct")
        
        for (i in seq_len(nplates)) {
          add_console_message(sprintf("-----"))
          add_console_message(sprintf("Processing plate %d", i))
          add_console_message(sprintf("-----"))
          
          current_plan <- plate_plans[[i]]
          current_data <- extracted_data_list[[i]]
          
          required_columns <- c("animal", "condition", "plate_id")
          missing_cols <- setdiff(required_columns, colnames(current_plan))
          if (length(missing_cols) > 0) {
            stop(sprintf("Plate plan %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
          }
          add_console_message(sprintf("Plate plan %d validated.", i))
          
          clean_id <- function(x) sub("_.*$", "", x)
          
          current_data$condition <- sapply(current_data$animal, function(a) {
            vals <- current_plan$condition[clean_id(current_plan$animal) == a]
            if (length(vals) == 0) NA else vals
          })
          add_console_message(sprintf("Conditions assigned for raw data file #%d.", i))
          
          if (!"condition_grouped" %in% names(current_plan)) {
            add_console_message(sprintf("Generating 'condition_grouped' for Plate plan #%d...", i))
            current_plan$condition_grouped <- sapply(current_plan$condition, function(cond) {
              if (is.na(cond)) NA else sub("_.*$", "", cond)
            })
            add_console_message(sprintf("'condition_grouped' generated for Plate plan #%d.", i))
          }
          current_data$condition_grouped <- sapply(current_data$animal, function(a) {
            vals <- current_plan$condition_grouped[clean_id(current_plan$animal) == a]
            if (length(vals) == 0) NA else vals
          })
          
          current_data$plate_id <- current_plan$plate_id[1]
          
          if (!"condition_tagged" %in% names(current_plan)) {
            add_console_message(sprintf("Generating 'condition_tagged' for Plate plan #%d...", i))
            current_plan <- current_plan %>%
              dplyr::group_by(condition_grouped) %>%
              dplyr::mutate(condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", dplyr::row_number()))) %>%
              dplyr::ungroup()
            add_console_message(sprintf("'condition_tagged' generated for Plate plan #%d.", i))
          }
          current_data$condition_tagged <- sapply(current_data$animal, function(a) {
            vals <- current_plan$condition_tagged[clean_id(current_plan$animal) == a]
            if (length(vals) == 0) NA else vals
          })
          add_console_message(sprintf("Grouping, tagging, and plate ID columns appended for raw data file #%d.", i))
          
          # Assignation des périodes à partir du fichier Excel
          add_console_message(sprintf("Assigning periods for Plate %d...", i))
          
          # Convertir les time codes selon l'unité de start
          current_data$start <- as.numeric(current_data$start)
          if (start_unit == "h") current_data$start <- current_data$start * 60
          if (start_unit == "s") current_data$start <- current_data$start / 60
          
          # Trier les périodes par start
          period_df <- period_df[order(period_df$start), ]
          boundaries <- period_df$start
          transitions <- period_df$transition
          
          # Vérifier que les transitions sont bien formées (de la forme "period1-period2")
          transitions_split <- strsplit(transitions, "-")
          if (!all(sapply(transitions_split, length) == 2)) {
            stop("All transitions must be of the form 'period1-period2'.")
          }
          
          # Assigner period_with_numbers
          current_data$period_with_numbers <- NA_character_
          for (j in seq_len(nrow(period_df))) {
            start_time <- period_df$start[j]
            transition <- transitions_split[[j]]
            period_before <- transition[1]
            period_after <- transition[2]
            
            if (j == 1) {
              # Avant le premier time code
              current_data$period_with_numbers[current_data$start < start_time] <- period_before
            }
            
            # Entre ce time code et le suivant (ou jusqu'à la fin si dernier)
            if (j < nrow(period_df)) {
              next_start_time <- period_df$start[j + 1]
              current_data$period_with_numbers[current_data$start >= start_time & current_data$start < next_start_time] <- period_after
            } else {
              current_data$period_with_numbers[current_data$start >= start_time] <- period_after
            }
          }
          
          # Assigner period_without_numbers (spécifique à Light-Dark Mode)
          current_data$period_without_numbers <- dplyr::case_when(
            stringr::str_detect(current_data$period_with_numbers, "^light") ~ "light",
            stringr::str_detect(current_data$period_with_numbers, "^dark") ~ "dark",
            TRUE ~ current_data$period_with_numbers
          )
          
          add_console_message(sprintf("Plate %d – periods assigned and simplified.", i))
          
          data_with_periods_list[[i]] <- current_data
          period_boundaries_list[[i]] <- boundaries
          boundary_associations_list[[i]] <- data.frame(
            boundary_time = boundaries,
            transition = transitions
          )
        }
        
        add_console_message("Data extraction, enrichment, and period assignment completed for all plates!")
        
        # Stocker les résultats
        processing_results(list(
          data_with_periods_df_list = data_with_periods_list,
          period_boundaries_list = period_boundaries_list,
          boundary_associations_list = boundary_associations_list
        ))
        
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", e$message))
      })
    })
    
    output$data_with_periods_table <- DT::renderDataTable({
      req(processing_results())
      if (length(processing_results()$data_with_periods_df_list) == 0) return(NULL)
      DT::datatable(dplyr::bind_rows(processing_results()$data_with_periods_df_list))
    })
    
    output$period_boundaries_table <- DT::renderDataTable({
      req(processing_results())
      if (length(processing_results()$period_boundaries_list) == 0) return(NULL)
      DT::datatable(data.frame(Plate = seq_along(processing_results()$period_boundaries_list),
                               Boundaries = sapply(processing_results()$period_boundaries_list, function(x) paste(x, collapse = ", "))))
    })
    
    output$boundary_associations_table <- DT::renderDataTable({
      req(processing_results())
      if (length(processing_results()$boundary_associations_list) == 0) return(NULL)
      DT::datatable(dplyr::bind_rows(lapply(seq_along(processing_results()$boundary_associations_list), function(i) {
        df <- processing_results()$boundary_associations_list[[i]]
        if (nrow(df) == 0) return(NULL)
        df$Plate <- i
        df
      })))
    })
    
    output$download_all_results <- downloadHandler(
      filename = function() {
        paste("processing_results_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        req(processing_results())
        
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        if (length(processing_results()$data_with_periods_df_list) > 0) {
          write.csv(dplyr::bind_rows(processing_results()$data_with_periods_df_list),
                    file.path(temp_dir, "data_with_periods.csv"), row.names = FALSE)
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "data_with_periods.csv"))
        }
        
        if (length(processing_results()$period_boundaries_list) > 0) {
          write.csv(data.frame(Plate = seq_along(processing_results()$period_boundaries_list),
                               Boundaries = sapply(processing_results()$period_boundaries_list, function(x) paste(x, collapse = ", "))),
                    file.path(temp_dir, "period_boundaries.csv"), row.names = FALSE)
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "period_boundaries.csv"))
        }
        
        if (length(processing_results()$boundary_associations_list) > 0) {
          write.csv(dplyr::bind_rows(lapply(seq_along(processing_results()$boundary_associations_list), function(i) {
            df <- processing_results()$boundary_associations_list[[i]]
            if (nrow(df) == 0) return(NULL)
            df$Plate <- i
            df
          })),
          file.path(temp_dir, "boundary_associations.csv"), row.names = FALSE)
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "boundary_associations.csv"))
        }
        
        zip::zip(file, files_to_zip, mode = "cherry-pick")
      }
    )
    
    output$console_output <- renderUI({
      messages <- console_messages()
      if (length(messages) == 0) return(HTML("No messages yet."))
      HTML(paste(sapply(messages, htmltools::htmlEscape), collapse = "<br>"))
    })
  })
}