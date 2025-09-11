# UI for Tracking Mode, Vibration-Rest Mode
processing_tm_vm_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Match Raw Data to Plate Plans (Tracking Mode, Vibration-Rest Mode)",
        width = 12,
        h4("Associate Raw Data with Plate Plans"),
        uiOutput(ns("file_plate_selectors")),
        actionButton(ns("confirm_mapping"), "Confirm Mapping"),
        
        h4("Additional Processing Parameters"),
        fileInput(ns("period_file"), "Upload Period Transitions File (Excel)", accept = ".xlsx"),
        fileInput(ns("removal_file"), "Upload Removal Specifications File (Excel)", accept = ".xlsx"),
        actionButton(ns("run_processing"), "Run Full Processing")
      )
    ),
    fluidRow(
      box(
        title = "Processing Results",
        width = 12,
        
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
                   downloadButton(ns("download_all_results"), "Download all results (.zip)")),
          tabPanel("Boundary Associations", DT::dataTableOutput(ns("boundary_associations_table")))
        )
      )
    )
  )
}

# Server for Tracking Mode, Vibration-Rest Mode
processing_tm_vm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    console_messages <- reactiveVal(character())
    
    add_console_message <- function(message) {
      console_messages(c(console_messages(), message))
    }
    
    observeEvent(input$clear_console, {
      console_messages(character())
      showNotification("Console cleared", type = "message")
    })
    
    should_remove <- function(x) !is.na(x) && nzchar(trimws(x))
    
    "%||%" <- function(a, b) if (is.null(a)) b else a
    
    output$file_plate_selectors <- renderUI({
      req(rv$raw_data_list, rv$plate_plan_df_list)
      if (length(rv$raw_data_list) != length(rv$plate_plan_df_list)) {
        add_console_message(sprintf("❌ Error: Number of raw data files (%d) must match number of plate plans (%d).", 
                                    length(rv$raw_data_list), length(rv$plate_plan_df_list)))
        return(NULL)
      }
      
      file_names <- sapply(rv$raw_data_list, attr, "file_name")
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
        file_names <- sapply(rv$raw_data_list, attr, "file_name")
        plate_ids <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
        
        selected_plates <- sapply(seq_along(file_names), function(i) input[[paste0("plate_select_", i)]])
        
        mapping_df <- data.frame(Raw_Data_File = file_names, Plate_ID = selected_plates, stringsAsFactors = FALSE)
        
        if (length(unique(mapping_df$Plate_ID)) != nrow(mapping_df)) {
          stop("Each raw data file must be associated with a unique Plate ID.")
        }
        
        rv$ordered_plate_plans <- rv$plate_plan_df_list[match(selected_plates, plate_ids)]
        rv$mapping <- mapping_df
        
        add_console_message("✅ Mapping confirmed successfully!")
        showNotification("Mapping confirmed successfully!", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Helper for generating condition columns
    generate_conditions <- function(current_data, current_plan, i) {
      clean_id <- function(x) sub("_.*$", "", x)
      
      current_data$condition <- sapply(current_data$animal, function(a) {
        vals <- current_plan$condition[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA else vals
      })
      if (all(is.na(current_data$condition))) {
        add_console_message(sprintf("❌ Error: Plate %d - No conditions could be assigned. Check animal ID matching between raw data and plate plan.", i))
        stop("Condition assignment failed for Plate ", i)
      }
      current_data$plate_id <- current_plan$plate_id[1]
      
      if (!"condition_grouped" %in% names(current_plan)) {
        current_plan$condition_grouped <- sapply(current_plan$condition, function(cond) {
          if (is.na(cond)) NA else sub("_.*$", "", cond)
        })
      }
      current_data$condition_grouped <- sapply(current_data$animal, function(a) {
        vals <- current_plan$condition_grouped[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA else vals
      })
      
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
      
      list(data = current_data, plan = current_plan)
    }
    
    # Helper for assigning periods
    assign_periods <- function(current_data, period_df, i) {
      current_data$start <- as.numeric(current_data$start)
      if (any(is.na(current_data$start))) {
        add_console_message(sprintf("⚠️ Warning: Plate %d - Some values in 'start' column could not be converted to numeric.", i))
      }
      
      add_console_message(sprintf("Plate %d - Range of current_data$start (in seconds): [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
      add_console_message(sprintf("Plate %d - Start time codes (in seconds): %s", i, paste(period_df$start, collapse = ", ")))
      add_console_message(sprintf("Plate %d - Transitions: %s", i, paste(period_df$transition, collapse = "; ")))
      
      boundaries <- period_df$start
      transitions <- period_df$transition
      transitions_split <- strsplit(transitions, "-")
      transitions_split <- lapply(transitions_split, function(parts) {
        if (length(parts) == 1) c(parts, parts) else if (length(parts) == 2) parts else stop("Each transition must contain a hyphen.")
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
        stringr::str_detect(current_data$period_with_numbers, "^vibration") ~ "vibration",
        stringr::str_detect(current_data$period_with_numbers, "^rest") ~ "rest",
        TRUE ~ current_data$period_with_numbers
      )
      
      list(data = current_data, boundaries = boundaries, transitions = transitions)
    }
    
    # Helpers for removal processes
    remove_time_codes <- function(current_data, removal_row, i) {
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
      current_data
    }
    
    remove_periods <- function(current_data, removal_row, i) {
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
      current_data
    }
    
    remove_wells <- function(current_data, removal_row, i) {
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
      current_data
    }
    
    remove_conditions <- function(current_data, removal_row, i) {
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
      current_data
    }
    
    # Helper for processing zones
    process_zones <- function(current_data, i) {
      add_console_message("---")
      add_console_message(sprintf("🔄 Plate %d - Processing zones…", i))
      add_console_message("---")
      
      zones <- unique(current_data$an)
      zone_data <- purrr::map(as.character(zones), ~ current_data %>% filter(an == .x))
      names(zone_data) <- as.character(zones)
      
      if (all(c(0, 2) %in% zones)) {
        zone_data[["1"]] <- zone_data[["0"]]
        num_cols <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur")
        for (col in num_cols) {
          zone_data[["1"]][[col]] <- zone_data[["0"]][[col]] - zone_data[["2"]][[col]]
        }
        add_console_message(sprintf("✅ Plate %d - Zone 1 calculated.", i))
      }
      
      processed_zones <- purrr::map(names(zone_data), ~ {
        zd <- zone_data[[.x]] %>% 
          mutate(
            totaldist = smldist + lardist,
            totaldur = smldur + lardur,
            totalct = smlct + larct,
            smlspeed = if_else(period > 0, smldist / period, NA_real_),
            larspeed = if_else(period > 0, lardist / period, NA_real_),
            totalspeed = if_else(period > 0, totaldist / period, NA_real_),
            zone = .x
          ) %>% 
          select(any_of(c("plate_id", "period", "animal", "condition", "condition_grouped", "condition_tagged",
                          "period_with_numbers", "period_without_numbers",
                          "zone", "start", "end", "inact", "inadur", "inadist", "smlct", "smldist", "smldur",
                          "larct", "lardur", "lardist", "emptyct", "emptydur", "totaldist", "totaldur", "totalct",
                          "smlspeed", "larspeed", "totalspeed")))
        add_console_message(sprintf("✅ Plate %d - Zone %s processed.", i, .x))
        zd
      })
      
      dplyr::bind_rows(processed_zones)
    }
    
    observeEvent(input$run_processing, {
      tryCatch({
        if (is.null(input$period_file)) stop("Please upload the Period Transitions File (Excel).")
        if (is.null(input$removal_file)) stop("Please upload the Removal Specifications File (Excel).")
        
        req(rv$raw_data_list, rv$ordered_plate_plans)
        
        convert_numeric_cols <- function(df, cols) {
          for (col in intersect(names(df), cols)) {
            df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
          }
          df
        }
        
        period_df <- readxl::read_excel(input$period_file$datapath)
        req("start" %in% colnames(period_df), "transition" %in% colnames(period_df))
        period_df$start <- as.numeric(period_df$start)
        period_df <- period_df[order(period_df$start), ]
        add_console_message("✅ Period transitions file loaded successfully.")
        
        removal_df <- readxl::read_excel(input$removal_file$datapath)
        req("plate_id" %in% colnames(removal_df))
        for (col in c("remove_time_codes", "remove_wells", "remove_conditions", "remove_periods")) {
          if (!(col %in% colnames(removal_df))) removal_df[[col]] <- NA_character_
          removal_df[[col]] <- ifelse(is.na(removal_df[[col]]) | tolower(trimws(removal_df[[col]])) %in% c("", "no"), NA_character_, as.character(removal_df[[col]]))
        }
        removal_df$plate_id <- as.numeric(gsub("Plate", "", removal_df$plate_id))
        add_console_message("✅ Removal specifications file loaded successfully.")
        
        extracted_data_list <- rv$raw_data_list
        plate_plans <- rv$ordered_plate_plans
        n_plates <- length(plate_plans)
        
        add_console_message("🔄 Starting data extraction, enrichment, and period assignment...")
        
        processed_data_list <- list()
        boundary_associations_list <- list()
        
        num_cols_to_convert <- c("inact", "inadur", "inadist", "smlct", "smldist", "smldur", "larct", "lardur", "lardist", "emptyct", "emptydur", "period")
        
        for (i in seq_len(n_plates)) {
          add_console_message(sprintf("-----"))
          add_console_message(sprintf("Processing plate %d", i))
          add_console_message(sprintf("-----"))
          
          current_plan <- plate_plans[[i]]
          current_data <- extracted_data_list[[i]]
          
          add_console_message(sprintf("---"))
          add_console_message(sprintf("🔄 Plate %d - Column generation", i))
          add_console_message(sprintf("---"))
          
          add_console_message("-")
          required_columns <- c("animal", "condition", "plate_id")
          missing_cols <- setdiff(required_columns, colnames(current_plan))
          if (length(missing_cols) > 0) {
            stop(sprintf("Plate %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
          }
          add_console_message(sprintf("✅ Plate %d - Plate plan %d validated.", i, i))
          
          add_console_message("-")
          add_console_message(sprintf("🔄 Plate %d - Matching animals between current_data and current_plan and generating 'condition' and 'plate_id' column…", i))
          conditions_result <- generate_conditions(current_data, current_plan, i)
          current_data <- conditions_result$data
          current_plan <- conditions_result$plan
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          add_console_message("-")
          add_console_message(sprintf("🔄 Plate %d - Assigning periods for Plate %d...", i, i))
          periods_result <- assign_periods(current_data, period_df, i)
          current_data <- periods_result$data
          boundaries <- periods_result$boundaries
          transitions <- periods_result$transitions
          add_console_message(sprintf("✅ Plate %d - Done.", i))
          
          plate_id <- as.numeric(current_plan$plate_id[1])
          removal_row <- removal_df[removal_df$plate_id == plate_id, ]
          
          if (nrow(removal_row) > 0) {
            add_console_message(sprintf("---"))
            add_console_message(sprintf("🔄 Plate %d - Starting removal process…", i))
            add_console_message(sprintf("---"))
            
            current_data <- remove_time_codes(current_data, removal_row, i)
            current_data <- remove_periods(current_data, removal_row, i)
            current_data <- remove_wells(current_data, removal_row, i)
            current_data <- remove_conditions(current_data, removal_row, i)
            add_console_message("---")
          }
          
          current_data <- convert_numeric_cols(current_data, num_cols_to_convert)
          add_console_message(sprintf("✅ Plate %d - Numeric columns converted.", i))
          
          zone_combined <- process_zones(current_data, i)
          
          processed_data_list[[i]] <- zone_combined
          boundary_associations_list[[i]] <- data.frame(
            plate_id    = as.character(current_plan$plate_id[1]),
            time_switch = boundaries,
            transition  = transitions,
            stringsAsFactors = FALSE
          )
        }
        
        add_console_message("\n ✅ Data extraction, enrichment, and period assignment completed for all plates!")
        
        rv$processing_results <- list(
          processed_data_list = processed_data_list,
          boundary_associations_list = boundary_associations_list
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
      dfs <- rv$processing_results$processed_data_list
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
      dfs <- rv$processing_results$processed_data_list
      
      lapply(seq_along(dfs), function(i) {
        local({
          ii <- i
          output[[paste0("tbl_plate_", ii)]] <- DT::renderDataTable({
            DT::datatable(
              dfs[[ii]],
              options = list(scrollX = TRUE, pageLength = 25)
            )
          })
        })
      })
      
      output$tbl_all_plates <- DT::renderDataTable({
        combined_df <- dplyr::bind_rows(dfs)
        DT::datatable(
          combined_df,
          options = list(scrollX = TRUE, pageLength = 25)
        )
      })
    })
    
    output$boundary_associations_table <- DT::renderDataTable({
      req(rv$processing_results)
      dbl <- rv$processing_results$boundary_associations_list
      if (length(dbl) == 0) return(NULL)
      
      combined <- dplyr::bind_rows(dbl) %>% dplyr::distinct()
      DT::datatable(combined, options = list(scrollX = TRUE))
    })
    
    output$download_all_results <- downloadHandler(
      filename = function() paste0("processing_results_", Sys.Date(), ".zip"),
      content = function(file) {
        req(rv$processing_results)
        
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        if (length(rv$processing_results$processed_data_list) > 0) {
          writexl::write_xlsx(dplyr::bind_rows(rv$processing_results$processed_data_list), file.path(temp_dir, "processed_data.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "processed_data.xlsx"))
        }
        
        if (length(rv$processing_results$boundary_associations_list) > 0) {
          boundary_df <- dplyr::bind_rows(rv$processing_results$boundary_associations_list) %>% dplyr::distinct()
          writexl::write_xlsx(boundary_df, file.path(temp_dir, "boundary_associations.xlsx"))
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