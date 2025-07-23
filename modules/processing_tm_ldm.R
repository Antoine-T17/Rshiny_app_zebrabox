# UI for Tracking Mode, Light-Dark Mode
processing_tm_ldm_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Match Raw Data to Plate Plans (Tracking Mode, Light-Dark Mode)",
        width = 12,
        h4("Associate Raw Data with Plate Plans"),
        uiOutput(ns("file_plate_selectors")),
        div(style = "margin-bottom: 10px;"),
        actionButton(ns("confirm_mapping"), "Confirm Mapping"),
        div(style = "margin-bottom: 20px;"),
        
        h4("Additional Processing Parameters"),
        fileInput(ns("period_file"), "Upload Period Transitions File (Excel)", accept = c(".xlsx")),
        fileInput(ns("removal_file"), "Upload Removal Specifications File (Excel)", accept = c(".xlsx")),
        div(style = "margin-bottom: 10px;"),
        actionButton(ns("run_processing"), "Run Full Processing")
      )
    ),
    fluidRow(
      box(
        title = "Processing Results",
        width = 12,
        
        # Clear Console button
        div(style = "margin-bottom: 10px;",
            actionButton(ns("clear_console"), "Clear Console", icon = icon("trash"))),
        
        tabsetPanel(
          tabPanel("Console Output", 
                   div(
                     style = "border: 1px solid #ccc; padding: 10px; height: 800px; overflow-y: auto; font-family: monospace;",
                     uiOutput(ns("console_output"))
                   )
          ),
          tabPanel("Processed Data",
                   uiOutput(ns("tables_with_periods")),
                   div(style = "margin-top:10px;",
                       downloadButton(ns("download_all_results"), "Download all results (.zip)"))),
          tabPanel("Boundary Associations", DT::dataTableOutput(ns("boundary_associations_table")))
        )
      )
    )
  )
}

# Server for Tracking Mode, Light-Dark Mode
processing_tm_ldm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    console_messages <- reactiveVal(character())
    
    # Helper to add messages to console
    log_message <- function(type = "info", msg, plate_id = NULL) {
      prefix <- if (!is.null(plate_id)) sprintf("Plate %d - ", plate_id) else ""
      icon <- switch(type, error = "❌ Error: ", warning = "⚠️ Warning: ", success = "✅ ", progress = "🔄 ", "")
      console_messages(c(console_messages(), paste(prefix, icon, msg)))
    }
    
    # Clear console
    observeEvent(input$clear_console, {
      console_messages(character())
      showNotification("Console cleared", type = "message")
    })
    
    # Render file-plate selectors
    output$file_plate_selectors <- renderUI({
      req(rv$raw_data_list, rv$plate_plan_df_list)
      if (length(rv$raw_data_list) != length(rv$plate_plan_df_list)) {
        log_message("error", sprintf("Number of raw data files (%d) must match number of plate plans (%d).", 
                                     length(rv$raw_data_list), length(rv$plate_plan_df_list)))
        return(NULL)
      }
      
      file_names <- sapply(rv$raw_data_list, \(df) attr(df, "file_name"))
      plate_file_names <- sapply(rv$plate_plan_df_list, \(df) attr(df, "file_name") %||% df$plate_id[1])
      plate_ids <- sapply(rv$plate_plan_df_list, \(df) df$plate_id[1])
      
      lapply(seq_along(file_names), \(i) {
        fluidRow(
          column(6, strong(file_names[i])),
          column(6, selectInput(ns(paste0("plate_select_", i)), label = NULL, 
                                choices = setNames(plate_ids, plate_file_names), selected = plate_ids[i]))
        )
      })
    })
    
    # Confirm mapping
    observeEvent(input$confirm_mapping, {
      tryCatch({
        req(rv$raw_data_list, rv$plate_plan_df_list)
        file_names <- sapply(rv$raw_data_list, \(df) attr(df, "file_name"))
        selected_plates <- sapply(seq_along(file_names), \(i) input[[paste0("plate_select_", i)]])
        
        if (length(unique(selected_plates)) != length(file_names)) {
          stop("Each raw data file must be associated with a unique Plate ID.")
        }
        
        plate_ids <- sapply(rv$plate_plan_df_list, \(df) df$plate_id[1])
        ordered_plate_plans <- rv$plate_plan_df_list[match(selected_plates, plate_ids)]
        
        rv$ordered_plate_plans <- ordered_plate_plans
        rv$mapping <- data.frame(Raw_Data_File = file_names, Plate_ID = selected_plates)
        
        log_message("success", "Mapping confirmed successfully!")
        showNotification("Mapping confirmed successfully!", type = "message")
      }, error = \(e) {
        log_message("error", e$message)
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Run processing
    observeEvent(input$run_processing, {
      tryCatch({
        req(rv$raw_data_list, rv$ordered_plate_plans, input$period_file, input$removal_file)
        
        # Load period transitions
        period_df <- readxl::read_excel(input$period_file$datapath) %>%
          select(start, transition) %>%
          mutate(start = as.numeric(start)) %>%
          arrange(start)
        log_message("success", "Period transitions file loaded successfully.")
        
        # Load removal specs avec force as.character pour robustesse
        removal_df <- readxl::read_excel(input$removal_file$datapath) %>%
          mutate(plate_id = as.numeric(gsub("Plate", "", plate_id)),
                 across(c(remove_time_codes, remove_wells, remove_conditions, remove_periods), 
                        ~ ifelse(is.na(.) | tolower(trimws(as.character(.))) %in% c("", "no"), 
                                 NA_character_, as.character(.))))
        log_message("success", "Removal specifications file loaded successfully.")
        
        log_message("progress", "Starting data extraction, enrichment, and period assignment...")
        
        processed_data_list <- list()
        boundary_associations_list <- list()
        
        # Centralize numeric conversion columns
        numeric_cols <- c("start", "end", "inact", "inadur", "inadist", "smlct", "smldist", "smldur",
                          "larct", "lardur", "lardist", "emptyct", "emptydur", "totaldist", "totaldur", "totalct")
        
        for (plate_index in seq_along(rv$ordered_plate_plans)) {
          log_message("info", "-----", plate_index)
          log_message("progress", sprintf("Processing plate %d", plate_index), plate_index)
          log_message("info", "-----", plate_index)
          
          current_plan <- rv$ordered_plate_plans[[plate_index]]
          current_data <- rv$raw_data_list[[plate_index]] %>%
            mutate(across(any_of(numeric_cols), ~ as.numeric(gsub(",", ".", as.character(.)))))
          
          # Section 1: Column Generation
          log_message("info", "---", plate_index)
          log_message("progress", "Column generation", plate_index)
          log_message("info", "---", plate_index)
          log_message("info", "-", plate_index)
          
          # Step 1: Validate plate plan and generate condition/plate_id
          required_columns <- c("animal", "condition", "plate_id")
          missing_cols <- setdiff(required_columns, colnames(current_plan))
          if (length(missing_cols) > 0) {
            stop(sprintf("Plate %d is missing required columns: %s", plate_index, paste(missing_cols, collapse = ", ")))
          }
          log_message("success", sprintf("Plate plan %d validated.", plate_index), plate_index)
          
          clean_id <- \(x) sub("_.*$", "", x)
          log_message("progress", "Matching animals between current_data and current_plan and generating 'condition' and 'plate_id' column…", plate_index)
          
          current_data <- current_data %>%
            mutate(
              plate_id = current_plan$plate_id[1],
              condition = current_plan$condition[match(clean_id(animal), clean_id(current_plan$animal))]
            )
          if (all(is.na(current_data$condition))) {
            log_message("error", "No conditions could be assigned. Check animal ID matching between raw data and plate plan.", plate_index)
            stop("Condition assignment failed for Plate ", plate_index)
          }
          log_message("success", "Done.", plate_index)
          
          # Step 2: Generate condition_grouped
          log_message("info", "-", plate_index)
          log_message("progress", "Generating 'condition_grouped' column…", plate_index)
          current_data <- current_data %>%
            mutate(condition_grouped = sub("_.*$", "", condition))
          log_message("success", "Done.", plate_index)
          
          # Step 3: Generate condition_tagged
          log_message("info", "-", plate_index)
          log_message("progress", "Generating 'condition_tagged' column…", plate_index)
          current_data <- current_data %>%
            mutate(condition_tagged = case_when(
              condition == "X" ~ "X",
              TRUE ~ paste0(condition_grouped, "_", row_number())
            ))
          log_message("success", "Done.", plate_index)
          
          # Step 4: Assign periods
          log_message("info", "-", plate_index)
          log_message("progress", sprintf("Assigning periods for Plate %d...", plate_index), plate_index)
          if (any(is.na(current_data$start))) {
            log_message("warning", "Some values in 'start' column could not be converted to numeric.", plate_index)
          }
          log_message("info", sprintf("Range of current_data$start (in seconds): [%.2f, %.2f]", min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)), plate_index)
          log_message("info", sprintf("Start time codes (in seconds): %s", paste(period_df$start, collapse = ", ")), plate_index)
          log_message("info", sprintf("Transitions: %s", paste(period_df$transition, collapse = "; ")), plate_index)
          
          assign_periods <- function(data, periods, plate_index = NULL) {
            boundaries <- c(-Inf, periods$start, Inf)
            transitions <- c(periods$transition[1], periods$transition)
            labels <- sapply(seq_along(transitions), \(i) {
              t <- unlist(strsplit(transitions[i], "-"))
              if (i == 1) t[1] else t[2] %||% t[1]
            })
            
            result <- data %>%
              mutate(
                period_with_numbers = labels[cut(start, breaks = boundaries, labels = FALSE)],
                period_without_numbers = case_when(
                  str_detect(period_with_numbers, "^light") ~ "light",
                  str_detect(period_with_numbers, "^dark") ~ "dark",
                  TRUE ~ period_with_numbers
                )
              )
            
            # Logs accentués pour précision, seulement si plate_index est fourni (réduit la complexité)
            if (!is.null(plate_index)) {
              # Log des intervalles
              for (i in seq_along(boundaries)[-length(boundaries)]) {
                if (i == 1) {
                  log_message("info", sprintf("Assigned '%s' to rows where start < %.2f", labels[i], boundaries[i+1]), plate_index)
                } else {
                  log_message("info", sprintf("Assigned '%s' to rows where start >= %.2f and start < %.2f", labels[i], boundaries[i], boundaries[i+1]), plate_index)
                }
              }
              log_message("info", sprintf("Assigned '%s' to rows where start >= %.2f", labels[length(labels)], boundaries[length(boundaries)-1]), plate_index)
              
              # Log du nombre de lignes par période
              period_counts <- table(result$period_with_numbers)
              for (period in names(period_counts)) {
                log_message("info", sprintf("Period '%s' assigned to %d rows", period, period_counts[period]), plate_index)
              }
            }
            
            result
          } # Ne fonctionne pas entièrement
          
          current_data <- assign_periods(current_data, period_df)
          log_message("info", sprintf("Unique periods assigned: %s", paste(unique(current_data$period_with_numbers), collapse = ", ")), plate_index)
          log_message("success", "Done.", plate_index)
          
          # Section 2: Removal Process
          removal_row <- removal_df %>% filter(plate_id == current_data$plate_id[1])
          if (nrow(removal_row) > 0) {
            log_message("info", "---", plate_index)
            log_message("progress", "Starting removal process…", plate_index)
            log_message("info", "---", plate_index)
            
            perform_removals <- function(data, removal_row) {
              # Step 1: Remove time codes
              log_message("info", "-", plate_index)
              log_message("info", sprintf("Range of 'start column': [%.2f, %.2f]", min(data$start, na.rm = TRUE), max(data$start, na.rm = TRUE)), plate_index)
              if (is.na(removal_row$remove_time_codes)) {
                log_message("info", "No time codes to remove (user indicated 'no' or left blank).", plate_index)
              } else {
                values_str <- removal_row$remove_time_codes
                values <- as.numeric(trimws(unlist(strsplit(values_str, ","))))
                valid_values <- intersect(values, data$start)
                invalid <- setdiff(values, data$start)
                if (length(invalid) > 0) {
                  log_message("warning", sprintf("The following time codes do not match any values in start: %s", paste(invalid, collapse = ", ")), plate_index)
                }
                if (length(valid_values) > 0) {
                  log_message("info", sprintf("Removing time codes: %s", paste(valid_values, collapse = ", ")), plate_index)
                  data <- data[!data$start %in% valid_values, ]
                } else if (any(is.na(values))) {
                  log_message("warning", sprintf("Invalid or empty time codes in remove_time_codes: %s", values_str), plate_index)
                }
              }
              log_message("success", "Done.", plate_index)
              
              # Step 2: Remove periods
              log_message("info", "-", plate_index)
              unique_periods <- unique(data$period_with_numbers)
              log_message("info", sprintf("Unique periods assigned: %s", paste(unique_periods, collapse = ", ")), plate_index)
              if (is.na(removal_row$remove_periods)) {
                log_message("info", "No periods to remove (user indicated 'no' or left blank).", plate_index)
              } else {
                values <- trimws(unlist(strsplit(removal_row$remove_periods, ",")))
                valid_values <- intersect(values, data$period_with_numbers)
                invalid <- setdiff(values, data$period_with_numbers)
                if (length(invalid) > 0) {
                  log_message("warning", sprintf("The following periods do not match any values in period_with_numbers: %s", paste(invalid, collapse = ", ")), plate_index)
                }
                if (length(valid_values) > 0) {
                  log_message("info", sprintf("Removing periods: %s", paste(valid_values, collapse = ", ")), plate_index)
                  data <- data[!data$period_with_numbers %in% valid_values, ]
                }
              }
              log_message("success", "Done.", plate_index)
              
              # Step 3: Remove wells
              log_message("info", "-", plate_index)
              if (is.na(removal_row$remove_wells)) {
                log_message("info", "No wells to remove (user indicated 'no' or left blank).", plate_index)
              } else {
                values_str <- removal_row$remove_wells
                values <- trimws(unlist(strsplit(values_str, ",")))
                valid_values <- intersect(values, data$animal)
                invalid <- setdiff(values, data$animal)
                if (length(invalid) > 0) {
                  log_message("warning", sprintf("The following wells do not match any values in animal: %s", paste(invalid, collapse = ", ")), plate_index)
                }
                if (length(valid_values) > 0) {
                  log_message("info", sprintf("Wells to remove: %s", paste(valid_values, collapse = ", ")), plate_index)
                  data <- data[!data$animal %in% valid_values, ]
                } else {
                  log_message("warning", sprintf("Invalid or empty wells in remove_wells: %s", values_str), plate_index)
                }
              }
              log_message("success", "Done.", plate_index)
              
              # Step 4: Remove conditions
              log_message("info", "-", plate_index)
              unique_condition_grouped <- unique(data$condition_grouped)
              log_message("info", sprintf("Range of 'condition_grouped': %s", paste(unique_condition_grouped, collapse = ", ")), plate_index)
              if (is.na(removal_row$remove_conditions)) {
                log_message("info", "No conditions to remove (user indicated 'no' or left blank).", plate_index)
              } else {
                values_str <- removal_row$remove_conditions
                values <- trimws(unlist(strsplit(values_str, ",")))
                if (!"condition" %in% colnames(data) || all(is.na(data$condition))) {
                  log_message("error", "'condition' column is missing or empty. Cannot remove conditions.", plate_index)
                } else {
                  valid_values <- intersect(values, data$condition)
                  invalid <- setdiff(values, data$condition)
                  if (length(invalid) > 0) {
                    log_message("warning", sprintf("The following conditions do not match any values in condition: %s", paste(invalid, collapse = ", ")), plate_index)
                  }
                  if (length(valid_values) > 0) {
                    log_message("info", sprintf("Removing conditions: %s", paste(valid_values, collapse = ", ")), plate_index)
                    data <- data[!data$condition %in% valid_values, ]
                  } else {
                    log_message("warning", sprintf("Invalid or empty conditions in remove_conditions: %s", values_str), plate_index)
                  }
                }
              }
              log_message("success", "Done.", plate_index)
              log_message("info", "---", plate_index)
              
              data
            }
            current_data <- perform_removals(current_data, removal_row)
          }
          
          log_message("success", "Numeric columns converted.", plate_index)
          
          # Section 3: Processing Zones
          log_message("info", "---", plate_index)
          log_message("progress", "Processing zones…", plate_index)
          log_message("info", "---", plate_index)
          
          zones <- unique(current_data$an)
          zone_data <- split(current_data, current_data$an)
          
          if (all(c(0, 2) %in% zones)) {
            zone_data[["1"]] <- zone_data[["0"]]
            num_cols <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur")
            for (col in num_cols) {
              zone_data[["1"]][[col]] <- zone_data[["0"]][[col]] - zone_data[["2"]][[col]]
            }
            log_message("success", "Zone 1 calculated.", plate_index)
          }
          
          processed <- lapply(names(zone_data), \(zn) {
            zd <- zone_data[[zn]] %>%
              mutate(
                totaldist = smldist + lardist,
                totaldur = smldur + lardur,
                totalct = smlct + larct,
                smlspeed = if_else(period > 0, smldist / period, NA_real_),
                larspeed = if_else(period > 0, lardist / period, NA_real_),
                totalspeed = if_else(period > 0, totaldist / period, NA_real_),
                zone = zn
              ) %>%
              select(any_of(c("plate_id", "period", "animal", "condition", "condition_grouped", "condition_tagged",
                              "period_with_numbers", "period_without_numbers", "zone", "start", "end", "inact", 
                              "inadur", "inadist", "emptyct", "emptydur", "smlct", "larct", "totalct", "smldur", 
                              "lardur", "totaldur", "smldist", "lardist", "totaldist", "smlspeed", "larspeed", "totalspeed")))
            log_message("success", sprintf("Zone %s processed.", zn), plate_index)
            zd
          })
          processed_zone <- bind_rows(processed)
          processed_data_list[[plate_index]] <- processed_zone
          boundary_associations_list[[plate_index]] <- data.frame(time_switch = period_df$start, transition = period_df$transition)
        }
        
        log_message("success", "\n Data extraction, enrichment, and period assignment completed for all plates!")
        
        rv$processing_results <- list(processed_data_list = processed_data_list, boundary_associations_list = boundary_associations_list)
        showNotification("Processing completed successfully!", type = "message")
      }, error = \(e) {
        log_message("error", e$message)
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Render tables with periods
    output$tables_with_periods <- renderUI({
      req(rv$processing_results$processed_data_list)
      tabs <- lapply(seq_along(rv$processing_results$processed_data_list), \(i) {
        tabPanel(paste0("Plate ", i), DT::dataTableOutput(ns(paste0("tbl_plate_", i))))
      })
      tabs <- append(tabs, list(tabPanel("All Plates", DT::dataTableOutput(ns("tbl_all_plates")))))
      do.call(tabsetPanel, tabs)
    })
    
    observe({
      req(rv$processing_results$processed_data_list)
      lapply(seq_along(rv$processing_results$processed_data_list), \(i) {
        output[[paste0("tbl_plate_", i)]] <- DT::renderDataTable({
          DT::datatable(rv$processing_results$processed_data_list[[i]], options = list(scrollX = TRUE, pageLength = 100))
        })
      })
      output$tbl_all_plates <- DT::renderDataTable({
        DT::datatable(bind_rows(rv$processing_results$processed_data_list), options = list(scrollX = TRUE, pageLength = 100))
      })
    })
    
    # Boundary associations table
    output$boundary_associations_table <- DT::renderDataTable({
      req(rv$processing_results$boundary_associations_list)
      combined <- bind_rows(rv$processing_results$boundary_associations_list) %>% distinct()
      DT::datatable(combined, options = list(scrollX = TRUE))
    })
    
    # Download handler
    output$download_all_results <- downloadHandler(
      filename = \( ) paste0("processing_results_", Sys.Date(), ".zip"),
      content = \(file) {
        req(rv$processing_results)
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        if (length(rv$processing_results$processed_data_list) > 0) {
          writexl::write_xlsx(bind_rows(rv$processing_results$processed_data_list), file.path(temp_dir, "processed_data.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "processed_data.xlsx"))
        }
        
        if (length(rv$processing_results$boundary_associations_list) > 0) {
          writexl::write_xlsx(bind_rows(rv$processing_results$boundary_associations_list) %>% distinct(), file.path(temp_dir, "boundary_associations.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "boundary_associations.xlsx"))
        }
        
        zip::zip(file, files_to_zip, mode = "cherry-pick")
      }
    )
    
    # Console output
    output$console_output <- renderUI({
      msgs <- console_messages()
      if (length(msgs) == 0) return(HTML("👻 No messages yet."))
      HTML(paste(sapply(msgs, htmltools::htmlEscape), collapse = "<br>"))
    })
  })
}