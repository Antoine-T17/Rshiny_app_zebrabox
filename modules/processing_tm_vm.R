# UI for Tracking Mode, Rest-Vibration Mode
processing_tm_vm_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Match Raw Data to Plate Plans (Tracking Mode, Rest-Vibration Mode)",
      width = 5,
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
    ),
    box(
      title = "Processing Results",
      width = 7,
      
      # Clear Console button
      div(style = "margin-bottom: 10px;",
          actionButton(ns("clear_console"), "Clear Console", icon = icon("trash"))),
      
      tabsetPanel(
        tabPanel("Console Output", 
                 div(
                   style = "background-color: #f5f5f5; border: 1px solid #ccc; padding: 10px; height: 600px; overflow-y: auto; font-family: monospace;",
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
}

# Server for Tracking Mode, Rest-Vibration Mode
processing_tm_vm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    console_messages <- reactiveVal(character())
    
    add_console_message <- function(message) {
      current_messages <- console_messages()
      console_messages(c(current_messages, message))
    }
    
    # Observer to clear the console when the button is clicked
    observeEvent(input$clear_console, {
      console_messages(character())           # reset to empty character vector
      showNotification("Console cleared", type = "message")
    })
    
    # Helper function to determine if removal should proceed
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
        
        add_console_message("✅ Mapping confirmed successfully!")
        showNotification("Mapping confirmed successfully!", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    observeEvent(input$run_processing, {
      tryCatch({
        # Check for required file uploads
        if (is.null(input$period_file)) {
          stop("Please upload the Period Transitions File (Excel).")
        }
        if (is.null(input$removal_file)) {
          stop("Please upload the Removal Specifications File (Excel).")
        }
        
        req(rv$raw_data_list, rv$ordered_plate_plans, input$period_file, input$removal_file)
        
        convert_numeric_cols <- function(df, cols) {
          for (col in intersect(names(df), cols)) {
            df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
          }
          df
        }
        
        # Read period transitions file
        period_df <- readxl::read_excel(input$period_file$datapath)
        req("start" %in% colnames(period_df), "transition" %in% colnames(period_df))
        period_df$start <- as.numeric(period_df$start)
        add_console_message("✅ Period transitions file loaded successfully.")
        
        # Read removal specifications file
        removal_df <- readxl::read_excel(input$removal_file$datapath)
        req("plate_id" %in% colnames(removal_df))
        for (col in c("remove_time_codes", "remove_wells", "remove_conditions", "remove_periods")) {
          if (!(col %in% colnames(removal_df))) {
            removal_df[[col]] <- NA_character_
          }
        }
        for (col in c("remove_time_codes", "remove_wells", "remove_conditions", "remove_periods")) {
          removal_df[[col]] <- ifelse(
            is.na(removal_df[[col]]) | tolower(trimws(removal_df[[col]])) %in% c("", "no"),
            NA_character_,
            as.character(removal_df[[col]])
          )
        }
        removal_df$plate_id <- as.numeric(gsub("Plate", "", removal_df$plate_id))
        add_console_message("✅ Removal specifications file loaded successfully.")
        
        n_plates <- length(rv$ordered_plate_plans)
        
        add_console_message("🔄 Starting data extraction, enrichment, and period assignment...")
        
        extracted_data_list <- rv$raw_data_list
        plate_plans <- rv$ordered_plate_plans
        nplates <- length(plate_plans)
        
        Processed_Data_list <- list()
        Boundary_Associations_list <- list()
        
        potential_numeric_cols <- c("start", "end", "inact", "inadist", "inadur", "smlct", "smldist", "smldur",
                                    "larct", "lardur", "lardist", "emptyct", "emptydur", "totaldist",
                                    "totaldur", "totalct")
        
        for (i in seq_len(nplates)) {
          add_console_message(sprintf("-----"))
          add_console_message(sprintf("Processing plate %d", i))
          add_console_message(sprintf("-----"))
          
          current_plan <- plate_plans[[i]]
          current_data <- extracted_data_list[[i]]
          
          # Section 1: Column Generation
          add_console_message(sprintf("---"))
          add_console_message(sprintf("🔄 Plate %d - Column generation", i))
          add_console_message(sprintf("---"))
          
          # Step 1: Validate plate plan and generate condition/plate_id
          add_console_message("-")
          required_columns <- c("animal", "condition", "plate_id")
          missing_cols <- setdiff(required_columns, colnames(current_plan))
          if (length(missing_cols) > 0) {
            stop(sprintf("Plate %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
          }
          add_console_message(sprintf("✅ Plate %d - Plate plan %d validated.", i, i))
          
          clean_id <- function(x) sub("_.*$", "", x)
          add_console_message(sprintf("🔄 Plate %d - Matching animals between current_data and current_plan and generating 'condition' and 'plate_id' column…", i))
          
          current_data$condition <- sapply(current_data$animal, function(a) {
            vals <- current_plan$condition[clean_id(current_plan$animal) == a]
            if (length(vals) == 0) NA else vals
          })
          
          if (all(is.na(current_data$condition))) {
            add_console_message(sprintf("❌ Error: Plate %d - No conditions could be assigned. Check animal ID matching between raw data and plate plan.", i))
            stop("Condition assignment failed for Plate ", i)
          }
          
          current_data$plate_id <- current_plan$plate_id[1]
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          # Step 2: Generate condition_grouped
          add_console_message("-")
          add_console_message(sprintf("🔄 Plate %d - Generating 'condition_grouped' column…", i))
          if (!"condition_grouped" %in% names(current_plan)) {
            current_plan$condition_grouped <- sapply(current_plan$condition, function(cond) {
              if (is.na(cond)) NA else sub("_.*$", "", cond)
            })
          }
          current_data$condition_grouped <- sapply(current_data$animal, function(a) {
            vals <- current_plan$condition_grouped[clean_id(current_plan$animal) == a]
            if (length(vals) == 0) NA else vals
          })
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          # Step 3: Generate condition_tagged
          add_console_message("-")
          add_console_message(sprintf("🔄 Plate %d - Generating 'condition_tagged' column…", i))
          if (!"condition_tagged" %in% names(current_plan)) {
            current_plan <- current_plan %>%
              dplyr::group_by(condition_grouped) %>%
              dplyr::mutate(condition_tagged = ifelse(condition == "X", "X", paste0(condition_grouped, "_", dplyr::row_number()))) %>%
              dplyr::ungroup()
          }
          current_data$condition_tagged <- sapply(current_data$animal, function(a) {
            vals <- current_plan$condition_tagged[clean_id(current_plan$animal) == a]
            if (length(vals) == 0) NA else vals
          })
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          # Step 4: Assign periods
          add_console_message("-")
          add_console_message(sprintf("🔄 Plate %d - Assigning periods for Plate %d...", i, i))
          
          current_data$start <- as.numeric(current_data$start)
          period_df$start <- as.numeric(period_df$start)
          if (any(is.na(current_data$start))) {
            add_console_message(sprintf("⚠️ Warning: Plate %d - Some values in 'start' column could not be converted to numeric.", i))
          }
          
          add_console_message(sprintf("Plate %d - Range of current_data$start (in seconds): [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
          add_console_message(sprintf("Plate %d - Start time codes (in seconds): %s", i, paste(period_df$start, collapse = ", ")))
          add_console_message(sprintf("Plate %d - Transitions: %s", i, paste(period_df$transition, collapse = "; ")))
          
          if (nrow(period_df) == 0) {
            stop("The period transitions file is empty.")
          }
          
          period_df <- period_df[order(period_df$start), ]
          boundaries <- period_df$start
          transitions <- period_df$transition
          
          transitions_split <- strsplit(transitions, "-")
          transitions_split <- lapply(transitions_split, function(parts) {
            if (length(parts) == 1) {
              c(parts, parts)
            } else if (length(parts) == 2) {
              parts
            } else {
              stop("Chaque transition doit contenir au plus un tiret (‘-’).")
            }
          })
          
          current_data$period_with_numbers <- NA_character_
          for (j in seq_len(nrow(period_df))) {
            start_time <- period_df$start[j]
            transition <- transitions_split[[j]]
            period_before <- transition[1]
            period_after <- transition[2]
            
            add_console_message(sprintf("Plate %d - Transition %d: %s -> %s at start_time %.2f seconds", i, j, period_before, period_after, start_time))
            
            if (j == 1) {
              current_data$period_with_numbers[current_data$start < start_time] <- period_before
              add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start < %.2f", i, period_before, start_time))
            }
            
            if (j < nrow(period_df)) {
              next_start_time <- period_df$start[j + 1]
              current_data$period_with_numbers[current_data$start >= start_time & current_data$start < next_start_time] <- period_after
              add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start >= %.2f and start < %.2f", i, period_after, start_time, next_start_time))
            } else {
              current_data$period_with_numbers[current_data$start >= start_time] <- period_after
              add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start >= %.2f", i, period_after, start_time))
            }
          }
          
          current_data$period_without_numbers <- dplyr::case_when(
            stringr::str_detect(current_data$period_with_numbers, "^rest") ~ "rest",
            stringr::str_detect(current_data$period_with_numbers, "^vibration") ~ "vibration",
            TRUE ~ current_data$period_with_numbers
          )
          
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          # Section 2: Removal Process
          plate_id <- as.numeric(current_plan$plate_id[1])
          removal_row <- removal_df[removal_df$plate_id == plate_id, ]
          
          if (nrow(removal_row) > 0) {
            add_console_message(sprintf("---"))
            add_console_message(sprintf("🔄 Plate %d - Starting removal process…", i))
            add_console_message(sprintf("---"))
            
            # Step 1: Remove time codes
            add_console_message("-")
            add_console_message(sprintf("Plate %d - Range of 'start column': [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
            
            if (should_remove(removal_row$remove_time_codes)) {
              time_codes_str <- as.character(removal_row$remove_time_codes)
              time_codes_to_remove <- as.numeric(unlist(strsplit(time_codes_str, ",")))
              if (length(time_codes_to_remove) > 0 && !any(is.na(time_codes_to_remove))) {
                invalid_time_codes <- setdiff(time_codes_to_remove, current_data$start)
                if (length(invalid_time_codes) > 0) {
                  add_console_message(sprintf("⚠️ Warning: Plate %d - The following time codes do not match any values in start: %s", i, paste(invalid_time_codes, collapse = ", ")))
                }
                time_codes_to_remove <- intersect(time_codes_to_remove, current_data$start)
                if (length(time_codes_to_remove) > 0) {
                  add_console_message(sprintf("Plate %d - Removing time codes: %s", i, paste(time_codes_to_remove, collapse = ", ")))
                  current_data <- current_data[!current_data$start %in% time_codes_to_remove, ]
                }
              } else {
                add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid or empty time codes in remove_time_codes: %s", i, time_codes_str))
              }
            } else {
              add_console_message(sprintf("Plate %d - No time codes to remove (user indicated 'no' or left blank).", i))
            }
            add_console_message(sprintf("✅ Plate %d - Done.", i))
            
            # Step 2: Remove periods
            add_console_message("-")
            unique_periods <- unique(current_data$period_with_numbers)
            add_console_message(sprintf("Plate %d - Unique periods assigned: %s", i, paste(unique_periods, collapse = ", ")))
            
            if (should_remove(removal_row$remove_periods)) {
              periods_to_remove <- trimws(unlist(strsplit(removal_row$remove_periods, ",")))
              if (length(periods_to_remove) > 0) {
                invalid_periods <- setdiff(periods_to_remove, current_data$period_with_numbers)
                if (length(invalid_periods) > 0) {
                  add_console_message(sprintf("⚠️ Warning: Plate %d - The following periods do not match any values in period_with_numbers: %s", i, paste(invalid_periods, collapse = ", ")))
                }
                periods_to_remove <- intersect(periods_to_remove, current_data$period_with_numbers)
                if (length(periods_to_remove) > 0) {
                  add_console_message(sprintf("Plate %d - Removing periods: %s", i, paste(periods_to_remove, collapse = ", ")))
                  current_data <- current_data[!current_data$period_with_numbers %in% periods_to_remove, ]
                }
              }
            } else {
              add_console_message(sprintf("Plate %d - No periods to remove (user indicated 'no' or left blank).", i))
            }
            add_console_message(sprintf("✅ Plate %d - Done.", i))
            
            # Step 3: Remove wells
            add_console_message("-")
            if (should_remove(removal_row$remove_wells)) {
              wells_str <- as.character(removal_row$remove_wells)
              wells_to_remove <- trimws(unlist(strsplit(wells_str, ",")))
              if (length(wells_to_remove) > 0 && !any(is.na(wells_to_remove))) {
                invalid_wells <- setdiff(wells_to_remove, current_data$animal)
                if (length(invalid_wells) > 0) {
                  add_console_message(sprintf("⚠️ Warning: Plate %d - The following wells do not match any values in animal: %s", i, paste(invalid_wells, collapse = ", ")))
                }
                wells_to_remove <- intersect(wells_to_remove, current_data$animal)
                if (length(wells_to_remove) > 0) {
                  add_console_message(sprintf("Plate %d - Wells to remove: %s", i, paste(wells_to_remove, collapse = ", ")))
                  current_data <- current_data[!current_data$animal %in% wells_to_remove, ]
                }
              } else {
                add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid or empty wells in remove_wells: %s", i, wells_str))
              }
            } else {
              add_console_message(sprintf("Plate %d - No wells to remove (user indicated 'no' or left blank).", i))
            }
            add_console_message(sprintf("✅ Plate %d - Done.", i))
            
            # Step 4: Remove conditions
            add_console_message("-")
            unique_condition_grouped <- unique(current_data$condition_grouped)
            add_console_message(sprintf("Plate %d - Range of 'condition_grouped': %s", i, paste(unique_condition_grouped, collapse = ", ")))
            
            if (should_remove(removal_row$remove_conditions)) {
              conditions_str <- as.character(removal_row$remove_conditions)
              conditions_to_remove <- trimws(unlist(strsplit(conditions_str, ",")))
              if (length(conditions_to_remove) > 0 && !any(is.na(conditions_to_remove))) {
                if (!"condition" %in% colnames(current_data) || all(is.na(current_data$condition))) {
                  add_console_message(sprintf("❌ Error: Plate %d - 'condition' column is missing or empty. Cannot remove conditions.", i))
                } else {
                  invalid_conditions <- setdiff(conditions_to_remove, current_data$condition)
                  if (length(invalid_conditions) > 0) {
                    add_console_message(sprintf("⚠️ Warning: Plate %d - The following conditions do not match any values in condition: %s", i, paste(invalid_conditions, collapse = ", ")))
                  }
                  conditions_to_remove <- intersect(conditions_to_remove, current_data$condition)
                  if (length(conditions_to_remove) > 0) {
                    add_console_message(sprintf("Plate %d - Removing conditions: %s", i, paste(conditions_to_remove, collapse = ", ")))
                    current_data <- current_data[!current_data$condition %in% conditions_to_remove, ]
                  }
                }
              } else {
                add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid or empty conditions in remove_conditions: %s", i, conditions_str))
              }
            } else {
              add_console_message(sprintf("Plate %d - No conditions to remove (user indicated 'no' or left blank).", i))
            }
            add_console_message(sprintf("✅ Plate %d - Done.", i))
            add_console_message("---")
          }
          
          # Convert numeric columns
          num_cols_to_convert <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur",
                                   "larct", "lardur", "lardist", "emptyct", "emptydur", "period")
          current_data <- convert_numeric_cols(current_data, num_cols_to_convert)
          add_console_message(sprintf("✅ Plate %d - Numeric columns converted.", i))
          
          # Section 3: Processing Zones
          add_console_message("---")
          add_console_message(sprintf("🔄 Plate %d - Processing zones…", i))
          add_console_message("---")
          
          zones <- unique(current_data$an)
          zone_data <- list()
          for (z in zones) {
            zone_data[[as.character(z)]] <- current_data %>% filter(an == z)
          }
          
          if (all(c(0, 2) %in% zones)) {
            zone_data[["1"]] <- zone_data[["0"]]
            num_cols <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur")
            for (col in num_cols) {
              zone_data[["1"]][[col]] <- zone_data[["0"]][[col]] - zone_data[["2"]][[col]]
            }
            add_console_message(sprintf("✅ Plate %d - Zone 1 calculated.", i))
          }
          
          # Process zones and calculate new variables
          processed_zones <- list()
          for (zn in names(zone_data)) {
            zd <- zone_data[[zn]]
            calc_cols <- c("smldist", "lardist", "smldur", "lardur", "smlct", "larct", "period")
            zd <- convert_numeric_cols(zd, calc_cols)
            
            zd <- zd %>% mutate(
              totaldist = ifelse(!is.na(smldist) & !is.na(lardist), smldist + lardist, NA),
              totaldur = ifelse(!is.na(smldur) & !is.na(lardur), smldur + lardur, NA),
              totalct = ifelse(!is.na(smlct) & !is.na(larct), smlct + larct, NA)
            )
            
            zd <- zd %>% mutate(
              smlspeed = ifelse(!is.na(smldist) & !is.na(period) & period > 0, smldist / period, NA),
              larspeed = ifelse(!is.na(lardist) & !is.na(period) & period > 0, lardist / period, NA),
              totalspeed = ifelse(!is.na(totaldist) & !is.na(period) & period > 0, totaldist / period, NA)
            )
            
            zd <- zd %>% mutate(zone = zn)
            
            desired_cols <- c("plate_id", "period", "animal", "condition", "condition_grouped", "condition_tagged",
                              "period_with_numbers", "period_without_numbers",
                              "zone", "start", "end", "inact", "inadur", "inadist", "emptyct", "emptydur",
                              "smlct", "larct", "totalct", "smldur", "lardur", "totaldur",
                              "smldist", "lardist", "totaldist", "smlspeed", "larspeed", "totalspeed")
            processed_zones[[zn]] <- zd %>% select(any_of(desired_cols))
            add_console_message(sprintf("✅ Plate %d - Zone %s processed.", i, zn))
          }
          
          zone_combined <- dplyr::bind_rows(processed_zones)
          Processed_Data_list[[i]] <- zone_combined
          Boundary_Associations_list[[i]] <- data.frame(
            time_switch = boundaries,
            transition = transitions
          )
        }
        
        add_console_message("✅ Data extraction, enrichment, and period assignment completed for all plates!")
        
        rv$processing_results <- list(
          Processed_Data_list = Processed_Data_list,
          Boundary_Associations_list = Boundary_Associations_list
        )
        
        add_console_message("✅ Results stored in rv$processing_results.")
        showNotification("Processing completed successfully!", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    output$tables_with_periods <- renderUI({
      req(rv$processing_results)
      dfs <- rv$processing_results$Processed_Data_list
      if (length(dfs) == 0) return(NULL)
      
      tabs <- lapply(seq_along(dfs), function(i) {
        tabPanel(
          paste0("Plate ", i),
          DT::dataTableOutput(ns(paste0("tbl_plate_", i)))
        )
      })
      
      tabs <- append(tabs, list(tabPanel(
        "All Plates",
        DT::dataTableOutput(ns("tbl_all_plates"))
      )))
      
      do.call(tabsetPanel, tabs)
    })
    
    observe({
      req(rv$processing_results)
      dfs <- rv$processing_results$Processed_Data_list
      
      lapply(seq_along(dfs), function(i) {
        local({
          ii <- i
          output[[paste0("tbl_plate_", ii)]] <- DT::renderDataTable({
            DT::datatable(
              dfs[[ii]],
              options = list(
                scrollX = TRUE,
                pageLength = 10
              )
            )
          })
        })
      })
      
      output$tbl_all_plates <- DT::renderDataTable({
        combined_df <- dplyr::bind_rows(dfs)
        DT::datatable(
          combined_df,
          options = list(
            scrollX = TRUE,
            pageLength = 10
          )
        )
      })
    })
    
    output$boundary_associations_table <- DT::renderDataTable({
      req(rv$processing_results)
      dbl <- rv$processing_results$Boundary_Associations_list
      if (length(dbl) == 0) return(NULL)
      
      combined <- dplyr::bind_rows(dbl) %>%
        dplyr::distinct()
      
      DT::datatable(
        combined,
        options = list(scrollX = TRUE)
      )
    })
    
    output$download_all_results <- downloadHandler(
      filename = function() {
        paste0("processing_results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(rv$processing_results)
        
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        if (length(rv$processing_results$Processed_Data_list) > 0) {
          writexl::write_xlsx(
            dplyr::bind_rows(rv$processing_results$Processed_Data_list),
            path = file.path(temp_dir, "processed_data.xlsx")
          )
          files_to_zip <- c(files_to_zip, cooking = file.path(temp_dir, "processed_data.xlsx"))
        }
        
        if (length(rv$processing_results$Boundary_Associations_list) > 0) {
          boundary_df <- dplyr::bind_rows(rv$processing_results$Boundary_Associations_list) %>%
            dplyr::distinct()
          
          writexl::write_xlsx(
            boundary_df,
            path = file.path(temp_dir, "boundary_associations.xlsx")
          )
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "boundary_associations.xlsx"))
        }
        
        zip::zip(file, files_to_zip, mode = "cherry-pick")
      }
    )
    
    output$console_output <- renderUI({
      messages <- console_messages()
      if (length(messages) == 0) return(HTML("👻 No messages yet."))
      HTML(paste(sapply(messages, htmltools::htmlEscape), collapse = "<br>"))
    })
  })
}