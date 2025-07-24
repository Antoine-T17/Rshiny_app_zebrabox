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
    
    add_console_message <- function(message) {
      current_messages <- console_messages()
      console_messages(c(current_messages, message))
    }
    
    observeEvent(input$clear_console, {
      console_messages(character())
      showNotification("Console cleared", type = "message")
    })
    
    should_remove <- function(x) {
      !is.na(x) && nzchar(trimws(x))
    }
    
    output$file_plate_selectors <- renderUI({
      req(rv$raw_data_list, rv$plate_plan_df_list)
      n_files <- length(rv$raw_data_list)
      n_plates <- length(rv$plate_plan_df_list)
      if (n_files != n_plates) {
        add_console_message(paste("❌ Error:", sprintf("Number of raw data files (%d) must match number of plate plans (%d).", n_files, n_plates)))
        return(NULL)
      }
      
      file_names <- sapply(rv$raw_data_list, function(df) attr(df, "file_name"))
      plate_file_names <- sapply(rv$plate_plan_df_list, function(df) attr(df, "file_name") %||% df$plate_id[1])
      plate_ids <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
      
      lapply(seq_along(file_names), function(i) {
        fluidRow(
          column(6, strong(file_names[i])),
          column(6, selectInput(ns(paste0("plate_select_", i)), label = NULL, 
                                choices = setNames(plate_ids, plate_file_names), selected = plate_ids[i]))
        )
      })
    })
    
    observeEvent(input$confirm_mapping, {
      tryCatch({
        req(rv$raw_data_list, rv$plate_plan_df_list)
        file_names <- sapply(rv$raw_data_list, function(df) attr(df, "file_name"))
        plate_ids <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
        
        selected_plates <- sapply(seq_along(file_names), function(i) input[[paste0("plate_select_", i)]])
        
        if (length(unique(selected_plates)) != length(file_names)) {
          stop("Each raw data file must be associated with a unique Plate ID.")
        }
        
        ordered_plate_plans <- rv$plate_plan_df_list[match(selected_plates, plate_ids)]
        
        rv$ordered_plate_plans <- ordered_plate_plans
        rv$mapping <- data.frame(Raw_Data_File = file_names, Plate_ID = selected_plates)
        
        add_console_message("✅ Mapping confirmed successfully!")
        showNotification("Mapping confirmed successfully!", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    observeEvent(input$run_processing, {
      tryCatch({
        if (is.null(input$period_file)) stop("Please upload the Period Transitions File (Excel).")
        if (is.null(input$removal_file)) stop("Please upload the Removal Specifications File (Excel).")
        
        req(rv$raw_data_list, rv$ordered_plate_plans, input$period_file, input$removal_file)
        
        convert_numeric_cols <- function(df, cols) {
          for (col in intersect(names(df), cols)) {
            df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
          }
          df
        }
        
        period_df <- readxl::read_excel(input$period_file$datapath) %>%
          mutate(start = as.numeric(start)) %>%
          arrange(start)
        add_console_message("✅ Period transitions file loaded successfully.")
        
        removal_df <- readxl::read_excel(input$removal_file$datapath) %>%
          mutate(plate_id = as.numeric(gsub("Plate", "", plate_id)),
                 across(c(remove_time_codes, remove_wells, remove_conditions, remove_periods), 
                        ~ ifelse(is.na(.) | tolower(trimws(.)) %in% c("", "no"), NA_character_, as.character(.))))
        add_console_message("✅ Removal specifications file loaded successfully.")
        
        add_console_message("🔄 Starting data extraction, enrichment, and period assignment...")
        
        processed_data_list <- list()
        boundary_associations_list <- list()
        
        num_cols_to_convert <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur", "period")
        
        clean_id <- function(x) sub("_.*$", "", x)
        
        for (i in seq_along(rv$ordered_plate_plans)) {
          add_console_message("-----")
          add_console_message(sprintf("Processing plate %d", i))
          add_console_message("-----")
          
          current_plan <- rv$ordered_plate_plans[[i]]
          current_data <- rv$raw_data_list[[i]]
          
          current_data$start <- as.numeric(gsub(",", ".", as.character(current_data$start)))
          if (any(is.na(current_data$start))) add_console_message(sprintf("⚠️ Warning: Plate %d - Some values in 'start' column could not be converted to numeric.", i))
          
          add_console_message("---")
          add_console_message(sprintf("🔄 Plate %d - Column generation", i))
          add_console_message("---")
          
          add_console_message("-")
          required_columns <- c("animal", "condition", "plate_id")
          missing_cols <- setdiff(required_columns, colnames(current_plan))
          if (length(missing_cols) > 0) stop(sprintf("Plate %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
          add_console_message(sprintf("✅ Plate %d - Plate plan validated.", i))
          
          add_console_message(sprintf("🔄 Plate %d - Matching animals and generating columns…", i))
          
          current_data <- current_data %>%
            mutate(
              plate_id = current_plan$plate_id[1],
              condition = current_plan$condition[match(clean_id(animal), clean_id(current_plan$animal))],
              condition_grouped = sub("_.*$", "", condition),
              condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", row_number()))
            )
          
          if (all(is.na(current_data$condition))) {
            add_console_message(sprintf("❌ Error: Plate %d - No conditions assigned. Check animal ID matching.", i))
            stop("Condition assignment failed for Plate ", i)
          }
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          add_console_message("-")
          add_console_message(sprintf("🔄 Plate %d - Assigning periods...", i))
          
          if (any(is.na(current_data$start))) add_console_message(sprintf("⚠️ Warning: Plate %d - Some 'start' values not numeric.", i))
          
          add_console_message(sprintf("Plate %d - Range of start: [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
          add_console_message(sprintf("Plate %d - Start time codes: %s", i, paste(period_df$start, collapse = ", ")))
          add_console_message(sprintf("Plate %d - Transitions: %s", i, paste(period_df$transition, collapse = "; ")))
          
          if (nrow(period_df) == 0) stop("Period transitions file is empty.")
          
          transitions_split <- lapply(strsplit(period_df$transition, "-"), function(parts) if (length(parts) == 1) c(parts, parts) else parts)
          
          current_data$period_with_numbers <- NA_character_
          for (j in seq_len(nrow(period_df))) {
            start_time <- period_df$start[j]
            period_before <- transitions_split[[j]][1]
            period_after <- transitions_split[[j]][2]
            
            add_console_message(sprintf("Plate %d - Transition %d: %s -> %s at %.2f seconds", i, j, period_before, period_after, start_time))
            
            if (j == 1) {
              current_data$period_with_numbers[current_data$start < start_time] <- period_before
              add_console_message(sprintf("Plate %d - Assigned '%s' to start < %.2f", i, period_before, start_time))
            }
            
            if (j < nrow(period_df)) {
              next_start_time <- period_df$start[j + 1]
              current_data$period_with_numbers[current_data$start >= start_time & current_data$start < next_start_time] <- period_after
              add_console_message(sprintf("Plate %d - Assigned '%s' to start >= %.2f and < %.2f", i, period_after, start_time, next_start_time))
            } else {
              current_data$period_with_numbers[current_data$start >= start_time] <- period_after
              add_console_message(sprintf("Plate %d - Assigned '%s' to start >= %.2f", i, period_after, start_time))
            }
          }
          
          current_data$period_without_numbers <- case_when(
            str_detect(current_data$period_with_numbers, "^light") ~ "light",
            str_detect(current_data$period_with_numbers, "^dark") ~ "dark",
            TRUE ~ current_data$period_with_numbers
          )
          
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          plate_id <- current_plan$plate_id[1]
          removal_row <- removal_df[removal_df$plate_id == plate_id, ]
          
          if (nrow(removal_row) > 0) {
            add_console_message("---")
            add_console_message(sprintf("🔄 Plate %d - Starting removal process…", i))
            add_console_message("---")
            
            removal_fields <- list(
              list(field = "remove_time_codes", col = "start", msg = "time codes", numeric = TRUE),
              list(field = "remove_periods", col = "period_with_numbers", msg = "periods", numeric = FALSE),
              list(field = "remove_wells", col = "animal", msg = "wells", numeric = FALSE),
              list(field = "remove_conditions", col = "condition", msg = "conditions", numeric = FALSE)
            )
            
            for (rem in removal_fields) {
              add_console_message("-")
              if (rem$field == "remove_time_codes") add_console_message(sprintf("Plate %d - Range of start: [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
              if (rem$field == "remove_periods") add_console_message(sprintf("Plate %d - Unique periods assigned: %s", i, paste(unique(current_data$period_with_numbers), collapse = ", ")))
              if (rem$field == "remove_conditions") add_console_message(sprintf("Plate %d - Range of 'condition_grouped': %s", i, paste(unique(current_data$condition_grouped), collapse = ", ")))
              
              if (should_remove(removal_row[[rem$field]])) {
                values_str <- removal_row[[rem$field]]
                values_to_remove <- trimws(unlist(strsplit(values_str, ",")))
                if (rem$numeric) values_to_remove <- as.numeric(values_to_remove)
                if (length(values_to_remove) > 0 && !any(is.na(values_to_remove))) {
                  invalid_values <- setdiff(values_to_remove, current_data[[rem$col]])
                  if (length(invalid_values) > 0) {
                    add_console_message(sprintf("⚠️ Warning: Plate %d - The following %s do not match any values in %s: %s", i, rem$msg, rem$col, paste(invalid_values, collapse = ", ")))
                  }
                  values_to_remove <- intersect(values_to_remove, current_data[[rem$col]])
                  if (length(values_to_remove) > 0) {
                    add_console_message(sprintf("Plate %d - Removing %s: %s", i, rem$msg, paste(values_to_remove, collapse = ", ")))
                    current_data <- current_data[!current_data[[rem$col]] %in% values_to_remove, ]
                  }
                } else {
                  add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid or empty %s in %s: %s", i, rem$msg, rem$field, values_str))
                }
              } else {
                add_console_message(sprintf("Plate %d - No %s to remove (user indicated 'no' or left blank).", i, rem$msg))
              }
              add_console_message(sprintf("✅ Plate %d - Done.", i))
            }
            add_console_message("---")
          }
          
          current_data <- convert_numeric_cols(current_data, num_cols_to_convert)
          add_console_message(sprintf("✅ Plate %d - Numeric columns converted.", i))
          
          add_console_message("---")
          add_console_message(sprintf("🔄 Plate %d - Processing zones…", i))
          add_console_message("---")
          
          zones <- unique(current_data$an)
          zone_data <- split(current_data, current_data$an)
          
          if (all(c(0, 2) %in% zones)) {
            zone_data[["1"]] <- zone_data[["0"]]
            num_cols <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur")
            for (col in num_cols) {
              zone_data[["1"]][[col]] <- zone_data[["0"]][[col]] - zone_data[["2"]][[col]]
            }
            add_console_message(sprintf("✅ Plate %d - Zone 1 calculated.", i))
          }
          
          processed_zones <- lapply(names(zone_data), function(zn) {
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
                              "period_with_numbers", "period_without_numbers", "zone", "start", "end", "inact", "inadur", "inadist",
                              "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur", "totaldist",
                              "totaldur", "totalct", "smlspeed", "larspeed", "totalspeed")))
            add_console_message(sprintf("✅ Plate %d - Zone %s processed.", i, zn))
            zd
          })
          
          processed_data_list[[i]] <- bind_rows(processed_zones)
          boundary_associations_list[[i]] <- data.frame(time_switch = period_df$start, transition = period_df$transition)
        }
        
        add_console_message("\n ✅ Data extraction, enrichment, and period assignment completed for all plates!")
        
        rv$processing_results <- list(processed_data_list = processed_data_list, boundary_associations_list = boundary_associations_list)
        showNotification("Processing completed successfully!", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    output$tables_with_periods <- renderUI({
      req(rv$processing_results$processed_data_list)
      tabs <- lapply(seq_along(rv$processing_results$processed_data_list), function(i) {
        tabPanel(paste0("Plate ", i), DT::dataTableOutput(ns(paste0("tbl_plate_", i))))
      })
      tabs <- append(tabs, list(tabPanel("All Plates", DT::dataTableOutput(ns("tbl_all_plates")))))
      do.call(tabsetPanel, tabs)
    })
    
    observe({
      req(rv$processing_results$processed_data_list)
      lapply(seq_along(rv$processing_results$processed_data_list), function(i) {
        output[[paste0("tbl_plate_", i)]] <- DT::renderDataTable({
          DT::datatable(rv$processing_results$processed_data_list[[i]], options = list(scrollX = TRUE, pageLength = 25))
        })
      })
      output$tbl_all_plates <- DT::renderDataTable({
        DT::datatable(bind_rows(rv$processing_results$processed_data_list), options = list(scrollX = TRUE, pageLength = 25))
      })
    })
    
    output$boundary_associations_table <- DT::renderDataTable({
      req(rv$processing_results$boundary_associations_list)
      DT::datatable(bind_rows(rv$processing_results$boundary_associations_list) %>% distinct(), options = list(scrollX = TRUE))
    })
    
    output$download_all_results <- downloadHandler(
      filename = function() paste0("processing_results_", Sys.Date(), ".zip"),
      content = function(file) {
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
    
    output$console_output <- renderUI({
      msgs <- console_messages()
      if (length(msgs) == 0) return(HTML("👻 No messages yet."))
      HTML(paste(sapply(msgs, htmltools::htmlEscape), collapse = "<br>"))
    })
  })
}