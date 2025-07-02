library(stringr)
library(RColorBrewer)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(zip)
library(DT)
library(writexl)
library(openxlsx)
library(purrr)
library(tidyr)

# Themes
light_theme <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.title = element_text(color = "black", size = 14, hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y = element_text(color = "black", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position = "right",
    legend.text = element_text(color = "black", size = 12, face = "italic"),
    legend.title = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.background = element_rect(fill = "white"),
    plot.caption = element_text(color = "black", size = 8, hjust = 1, margin = margin(t = 10))
  )
}

dark_theme <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.title = element_text(color = "white", size = 14, hjust = 0.5),
    axis.text.y = element_text(color = "white", size = 12),
    axis.text.x = element_text(color = "white", size = 12),
    axis.title.x = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position = "right",
    legend.text = element_text(color = "white", size = 12, face = "italic"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    strip.text.x = element_text(color = "white", size = 12),
    strip.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.border = element_rect(color = "white", fill = NA),
    panel.grid.major = element_line(color = "grey30"),
    panel.grid.minor = element_line(color = "grey30"),
    plot.caption = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
  )
}

# UI for Visualization Module, Rest-Vibration Mode
visualization_tm_vm_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Visualization Inputs",
      width = 4,
      selectInput(ns("plot_type"), "Plot Type",
                  choices = c("boxplot_rest_vibration", "boxplot_cumulate", "boxplot_delta", "lineplot"), 
                  selected = "boxplot_rest_vibration"),
      div(style = "margin-bottom: 20px;"),
      
      # --- BOXPLOT Rest/Vibration ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'boxplot_rest_vibration'", ns("plot_type")),
        actionButton(ns("generate_rest_vibration_dfs"), "Generate Rest/Vibration Datasets"),
        div(style = "margin-bottom: 30px;"),
        # Response Variable juste après Generate
        selectInput(ns("response_var"), "Response Variable",
                    choices = c("", "totaldist", "totaldur", "totalct", "totalspeed",
                                "lardist", "lardur", "larct", "larspeed",
                                "smldist", "smldur", "smlct", "smlspeed",
                                "inadist", "inadur", "inact", "emptydur", "emptyct"),
                    selected = ""),
        selectInput(ns("boxplot_rest_vibration_mode"), "Boxplot Mode",
                    choices = c("separated", "pooled"), selected = "separated")
      ),
      
      # --- BOXPLOT CUMULATE ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'boxplot_cumulate'", ns("plot_type")),
        actionButton(ns("generate_cumulate_dfs"), "Generate Cumulative Datasets"),
        div(style = "margin-bottom: 30px;"),
        selectInput(ns("response_var"), "Response Variable",
                    choices = c("", "totaldist", "totaldur", "totalct", "totalspeed",
                                "lardist", "lardur", "larct", "larspeed",
                                "smldist", "smldur", "smlct", "smlspeed",
                                "inadist", "inadur", "inact", "emptydur", "emptyct"),
                    selected = "")
      ),
      
      # --- BOXPLOT DELTA ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'boxplot_delta'", ns("plot_type")),
        uiOutput(ns("transition_select_ui")),
        textInput(ns("delta_time"), "Delta Time Window (seconds)", value = "60",
                  placeholder = "Enter time window in seconds"),
        actionButton(ns("generate_delta_dfs"), "Generate Delta Datasets"),
        div(style = "margin-bottom: 30px;"),
        # Response Variable juste après Generate
        selectInput(ns("response_var"), "Response Variable",
                    choices = c("", "totaldist", "totaldur", "totalct", "totalspeed",
                                "lardist", "lardur", "larct", "larspeed",
                                "smldist", "smldur", "smlct", "smlspeed",
                                "inadist", "inadur", "inact", "emptydur", "emptyct"),
                    selected = ""),
        selectInput(ns("boxplot_delta_mode"), "Boxplot Mode",
                    choices = c("separated", "pooled"), selected = "separated"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pooled' && input['%s'] == 'boxplot_delta'",
                              ns("boxplot_delta_mode"), ns("plot_type")),
          textInput(ns("boxplot_delta_phase_colors"),
                    "Phase Colors (Before, Switch, After; comma-separated hex)",
                    value = "#FF6F61, #40C4FF, #4CAF50")
        )
      ),
      
      # --- LINEPLOT ---
      conditionalPanel(
        condition = sprintf("input['%s'] == 'lineplot'", ns("plot_type")),
        selectInput(ns("lineplot_replicate_mode"), "Replicate Mode",
                    choices = c("pooled", "separated"), selected = "pooled"),
        selectInput(ns("time_unit_original"), "Original Time Unit",
                    choices = c("seconds", "minutes", "hours", "days"), selected = "seconds"),
        selectInput(ns("time_unit_convert"), "Convert Time Unit?",
                    choices = c("No", "Yes"), selected = "No"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Yes'", ns("time_unit_convert")),
          selectInput(ns("time_unit_target"), "Target Time Unit",
                      choices = c("seconds", "minutes", "hours", "days"), selected = "minutes")
        ),
        uiOutput(ns("aggregation_period_label")),
        actionButton(ns("generate_lineplot_dfs"), "Generate Lineplot Datasets"),
        div(style = "margin-bottom: 30px;"),
        selectInput(ns("response_var"), "Response Variable",
                    choices = c("", "totaldist", "totaldur", "totalct", "totalspeed",
                                "lardist", "lardur", "larct", "larspeed",
                                "smldist", "smldur", "smlct", "smlspeed",
                                "inadist", "inadur", "inact", "emptydur", "emptyct"),
                    selected = "")
      ),
      
      # --- FILL MODE COMMUN AUX BOXPLOTS ---
      conditionalPanel(
        condition = sprintf(
          "input['%s'] == 'boxplot_cumulate' ||
           input['%s'] == 'boxplot_rest_vibration' ||
           input['%s'] == 'boxplot_delta'",
          ns("plot_type"), ns("plot_type"), ns("plot_type")
        ),
        radioButtons(ns("boxplot_fill_mode"), "Boxplot Fill Mode",
                     choices = c("Full" = "full", "Empty" = "empty"),
                     selected = "full", inline = TRUE)
      ),
      
      # --- ORDRE & COULEURS GÉNÉRAUX ---
      textInput(ns("condition_grouped_order"),
                "Condition Order (comma-separated)",
                value = "",
                placeholder = "e.g., cond1, cond2, cond3"),
      textInput(ns("condition_grouped_color"),
                "Condition Colors (comma-separated hex)",
                value = "",
                placeholder = "e.g., #FF0000, #00FF00, #0000FF"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'pooled' && input['%s'] == 'boxplot_rest_vibration'",
                            ns("boxplot_rest_vibration_mode"), ns("plot_type")),
        textInput(ns("boxplot_rest_vibration_periods_colors"),
                  "Rest/Vibration Colors (comma-separated hex)",
                  value = "#890e00,#004089")
      ),
      
      # --- OUTPUT & GÉNÉRATION FINALE ---
      radioButtons(ns("output_mode"), "Output Mode",
                   choices = c("PNG", "HTML"), selected = "HTML", inline = TRUE),
      uiOutput(ns("figure_selector")),
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        actionButton(ns("generate_figure"), "Generate Figure", style = "width: 100%;"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'boxplot_delta'", ns("plot_type")),
          actionButton(ns("generate_delta_tables"),
                       "Generate Delta Percentage Tables",
                       style = "width: 100%;"),
          # Nouveau bouton pour tout générer et zipper
          downloadButton(ns("download_delta_for_type"),
                         "Download All Figures for This Plot Type (.zip)",
                         style = "width: 100%; margin-top: 10px;")
        )
      )
    ),
    
    # --- SORTIE ---
    box(
      title = "Visualization Output",
      width = 8,
      tabsetPanel(
        id = ns("output_tabs"),
        tabPanel("Interactive Figure",
                 uiOutput(ns("figure_plot")),
                 div(style = "margin-top: 10px;"),
                 radioButtons(ns("theme_switch"), "Theme",
                              choices = c("Light", "Dark"),
                              selected = "Light", inline = TRUE),
                 div(style = "margin-top: 10px;"),
                 downloadButton(ns("save_current_figure"),
                                "Save Current Figure"),
                 downloadButton(ns("download_all_for_type"),
                                "Save All Figures as PNG")
        ),
        tabPanel("Datasets",
                 selectInput(ns("dataset_type"), "Dataset Type",
                             choices = c("Boxplot Rest/Vibration",
                                         "Boxplot Cumulative",
                                         "Boxplot Delta",
                                         "Lineplot"),
                             selected = "Boxplot Rest/Vibration"),
                 selectInput(ns("dataset_response_var"), "Response Variable",
                             choices = c("", "totaldist", "totaldur",
                                         "totalct", "totalspeed", "lardist",
                                         "lardur", "larct", "larspeed",
                                         "smldist", "smldur", "smlct",
                                         "smlspeed", "inadist", "inadur",
                                         "inact", "emptydur", "emptyct"),
                             selected = ""),
                 DT::dataTableOutput(ns("dataset_table")),
                 div(style = "margin-top: 10px; margin-bottom: 10px;",
                     downloadButton(ns("download_current_dataset"),
                                    "Download Current Dataset (.xlsx)"),
                     downloadButton(ns("download_all_datasets"),
                                    "Download All Datasets (.zip)")
                 )
        ),
        tabPanel("Console Output",
                 div(style = "background-color: #f5f5f5;
                             border: 1px solid #ccc;
                             padding: 10px;
                             height: 600px;
                             overflow-y: auto;
                             font-family: monospace;",
                     uiOutput(ns("console_output"))
                 )
        ),
        tabPanel("Delta Percentage Tables",
                 value = "delta_percentage_tables",
                 selectInput(ns("delta_table_type"), "Table Type",
                             choices = c("Momentum Comparisons",
                                         "Condition Comparisons"),
                             selected = "Momentum Comparisons"),
                 selectInput(ns("delta_table_var"), "Response Variable",
                             choices = c("totaldist", "totaldur",
                                         "totalct", "totalspeed",
                                         "lardist", "lardur", "larct",
                                         "larspeed", "smldist", "smldur",
                                         "smlct", "smlspeed", "inadist",
                                         "inadur", "inact", "emptydur",
                                         "emptyct"),
                             selected = "totaldist"),
                 DT::dataTableOutput(ns("delta_percentage_table")),
                 div(style = "margin-top: 10px; margin-bottom: 10px;",
                     downloadButton(ns("download_current_delta_table"),
                                    "Download Current Table (.xlsx)"),
                     downloadButton(ns("download_all_delta_tables"),
                                    "Download All Delta Tables (.zip)")
                 )
        )
      )
    )
  )
}

# Server for Visualization Module, Rest-Vibration Mode
visualization_tm_vm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    console_messages <- reactiveVal(character())
    
    add_console_message <- function(msg) {
      console_messages(c(console_messages(), msg))
    }
    
    ensure_directory <- function(path) {
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    }
    
    convert_time <- function(time_val, from_unit, to_unit) {
      if (from_unit == to_unit) return(time_val)
      conversion_factors <- list(seconds = 1, minutes = 60, hours = 3600, days = 86400)
      time_val * conversion_factors[[from_unit]] / conversion_factors[[to_unit]]
    }
    
    compute_wells <- function(df) {
      group_var <- if (input$lineplot_replicate_mode == "pooled") "condition_grouped" else "condition"
      result <- df %>%
        group_by(.data[[group_var]], zone, plate_id) %>%
        summarise(n_wells_plate = n_distinct(animal), .groups = "drop") %>%
        group_by(.data[[group_var]], zone) %>%
        summarise(n_wells = sum(n_wells_plate), .groups = "drop")
      add_console_message(sprintf("Debug: Wells per %s and zone:", group_var))
      for (i in 1:nrow(result)) {
        add_console_message(sprintf("  %s, Zone %s: %d wells", result[[group_var]][i], result$zone[i], result$n_wells[i]))
      }
      result
    }
    
    compute_n_animals <- function(df) {
      df %>%
        # 1) compter les animaux DISTINCTS par plaque
        group_by(condition_grouped, zone, plate_id) %>%
        summarise(
          n_animals_plate = n_distinct(animal),
          .groups = "drop"
        ) %>%
        # 2) sommer ces effectifs de plaque en un total par condition et zone
        group_by(condition_grouped, zone) %>%
        summarise(
          n = sum(n_animals_plate),
          .groups = "drop"
        ) %>%
        # optionnel : placer le label en bas du graphique
        mutate(y = -Inf)
    }
    
    validate_aggregation_period <- function(period, time_unit_original, time_unit_target, convert_time_unit) {
      period_num <- as.numeric(period)
      if (is.na(period_num) || period_num <= 0) {
        add_console_message("Warning: Aggregation period must be a positive number.")
        showModal(modalDialog(
          title = "Warning",
          "Aggregation period must be a positive number. Please adjust and try again.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(FALSE)
      }
      agg_unit <- if (convert_time_unit == "Yes") time_unit_target else time_unit_original
      agg_period_seconds <- convert_time(period_num, agg_unit, "seconds")
      add_console_message(sprintf("Debug: Aggregation period is %s %s (= %s seconds)", period_num, agg_unit, round(agg_period_seconds, 2)))
      if (agg_unit == "minutes" && period_num > 10) {
        add_console_message(sprintf("Warning: Aggregation period (%s %s) seems large for minutes. Consider adjusting it.", period_num, agg_unit))
        showModal(modalDialog(
          title = "Caution",
          sprintf("Aggregation period (%s %s) seems large. Consider adjusting it for better resolution.", period_num, agg_unit),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (agg_unit == "hours" && period_num > 1) {
        add_console_message(sprintf("Warning: Aggregation period (%s %s) seems large for hours. Consider adjusting it.", period_num, agg_unit))
        showModal(modalDialog(
          title = "Caution",
          sprintf("Aggregation period (%s %s) seems large. Consider adjusting it for better resolution.", period_num, agg_unit),
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (agg_unit == "days" && period_num > 0.1) {
        add_console_message(sprintf("Warning: Aggregation period (%s %s) seems large for days. Consider adjusting it.", period_num, agg_unit))
        showModal(modalDialog(
          title = "Caution",
          sprintf("Aggregation period (%s %s) seems large. Consider adjusting it for better resolution.", period_num, agg_unit),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      TRUE
    }
    
    validate_delta_time <- function(delta) {
      delta_num <- as.numeric(delta)
      if (is.na(delta_num) || delta_num <= 0) {
        add_console_message("Warning: Delta time window must be a positive number.")
        showModal(modalDialog(
          title = "Warning",
          "Delta time window must be a positive number. Please adjust and try again.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(FALSE)
      }
      TRUE
    }
    
    output$aggregation_period_label <- renderUI({
      unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
      textInput(ns("aggregation_period"), sprintf("Aggregation Period (in %s)", unit), value = "60")
    })
    
    output$transition_select_ui <- renderUI({
      req(rv$processing_results, rv$processing_results$Boundary_Associations_list)
      boundaries <- bind_rows(rv$processing_results$Boundary_Associations_list) %>% distinct()
      transitions <- boundaries$transition
      if (length(transitions) == 0) {
        add_console_message("⚠️ Aucun transition trouvé.")
        choices <- "No transitions available"
      } else {
        choices <- transitions
      }
      selectInput(
        ns("transition_select"),
        "Select Transition",
        choices = choices,
        selected = choices[1]
      )
    })
    
    validate_transition <- function(transition, boundaries, all_zone_combined) {
      if (transition == "No transitions available") {
        add_console_message("Warning: No valid transitions available.")
        showModal(modalDialog(
          title = "Warning",
          "No valid transitions are available. Please check your data.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(FALSE)
      }
      trans_time <- boundaries$time_switch[boundaries$transition == transition]
      if (length(trans_time) == 0 || !any(abs(all_zone_combined$start - trans_time) < 1)) {
        add_console_message(sprintf("Warning: Transition '%s' has no corresponding timestamp in the data.", transition))
        showModal(modalDialog(
          title = "Invalid Transition",
          sprintf("The selected transition '%s' does not have a corresponding timestamp in the data. Please select another transition.", transition),
          easyClose = TRUE,
          footer = NULL
        ))
        return(FALSE)
      }
      TRUE
    }
    
    observeEvent(input$generate_rest_vibration_dfs, {
      add_console_message(sprintf("Debug: rv$processing_results is %s", if (is.null(rv$processing_results)) "NULL" else "defined"))
      tryCatch({
        if (is.null(rv$processing_results)) {
          add_console_message("Warning: rv$processing_results is NULL. Please run processing first.")
          return()
        }
        if (!"Processed_Data_list" %in% names(rv$processing_results)) {
          add_console_message("Error: Processed_Data_list not found in rv$processing_results.")
          return()
        }
        add_console_message(sprintf("Debug: Processed_Data_list has %d elements", length(rv$processing_results$Processed_Data_list)))
        rv$Processed_Data_list <- map(rv$processing_results$Processed_Data_list, ~ mutate(.x, plate_id = as.character(plate_id)))
        all_zone_combined <- bind_rows(rv$Processed_Data_list)
        rv$all_zone_combined <- all_zone_combined
        add_console_message("Binding processed data into all_zone_combined...")
        required_cols <- c("period_without_numbers", "zone", "condition_tagged", "condition", "condition_grouped",
                           "plate_id", "animal", "start", "totaldist", "totaldur", "totalct", "totalspeed",
                           "lardist", "lardur", "larct", "larspeed", "smldist", "smldur", "smlct", "smlspeed",
                           "inadist", "inadur", "inact", "emptydur", "emptyct")
        if (length(missing_cols <- setdiff(required_cols, colnames(all_zone_combined))) > 0) {
          add_console_message(sprintf("Warning: Missing columns: %s", paste(missing_cols, collapse = ", ")))
        }
        response_vars <- c("totaldist", "totaldur", "totalct", "totalspeed", "lardist", "lardur", "larct",
                           "larspeed", "smldist", "smldur", "smlct", "smlspeed", "inadist", "inadur", "inact",
                           "emptydur", "emptyct")
        unique_periods <- unique(all_zone_combined$period_without_numbers)
        add_console_message(sprintf("Detected periods: %s", paste(unique_periods, collapse = ", ")))
        
        vibration_period <- unique_periods[grepl("vibration", unique_periods, ignore.case = TRUE)]
        rest_period <- unique_periods[grepl("rest", unique_periods, ignore.case = TRUE)]
        
        if (length(vibration_period) == 0 && length(rest_period) == 0) {
          add_console_message("⚠️ Warning: no 'vibration' or 'rest' labels detected – Rest/Vibration plots may be empty.")
        } else {
          if (length(vibration_period) == 0) {
            add_console_message("⚠️ Warning: no 'vibration' period detected; only 'rest' periods will be plotted.")
          }
          if (length(rest_period) == 0) {
            add_console_message("⚠️ Warning: no 'rest' period detected; only 'vibration' periods will be plotted.")
          }
        }
        
        add_console_message(sprintf(
          "Using vibration = %s; rest = %s",
          if (length(vibration_period) > 0) paste(vibration_period, collapse = ", ") else "<none>",
          if (length(rest_period) > 0) paste(rest_period, collapse = ", ") else "<none>"
        ))
        
        calculate_means <- function(var) {
          all_zone_combined %>%
            filter(period_without_numbers %in% c(vibration_period, rest_period)) %>%
            group_by(period_without_numbers, zone, condition_tagged, plate_id) %>%
            summarise(
              plate_id = first(plate_id),
              start = first(start),
              period_with_numbers = first(period_with_numbers),
              condition_grouped = first(condition_grouped),
              condition = first(condition),
              animal = first(animal),
              mean_val = mean(.data[[var]], na.rm = TRUE),
              .groups = "drop"
            )
        }
        rv$all_zone_combined_rest_vibration_boxplots <- setNames(lapply(response_vars, calculate_means), response_vars)
        add_console_message("Processed_data_for_rest_vibration_boxplots created.")
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", if (nzchar(e$message)) e$message else "Unknown error"))
      })
    })
    
    observeEvent(input$generate_cumulate_dfs, {
      add_console_message(sprintf("Debug: rv$processing_results is %s", if (is.null(rv$processing_results)) "NULL" else "defined"))
      tryCatch({
        if (is.null(rv$processing_results)) {
          add_console_message("Warning: rv$processing_results is NULL. Please run processing first.")
          return()
        }
        if (!"Processed_Data_list" %in% names(rv$processing_results)) {
          add_console_message("Error: Processed_Data_list not found in rv$processing_results.")
          return()
        }
        rv$Processed_Data_list <- map(rv$processing_results$Processed_Data_list, ~ mutate(.x, plate_id = as.character(plate_id)))
        all_zone_combined <- bind_rows(rv$Processed_Data_list)
        rv$all_zone_combined <- all_zone_combined
        response_vars <- c("totaldist", "totaldur", "totalct", "totalspeed", "lardist", "lardur", "larct",
                           "larspeed", "smldist", "smldur", "smlct", "smlspeed", "inadist", "inadur", "inact",
                           "emptydur", "emptyct")
        summarize_cum_box <- function(var) {
          all_zone_combined %>%
            group_by(condition_grouped, zone, plate_id, animal) %>%
            summarise(
              cum = sum(.data[[var]], na.rm = TRUE),
              condition_tagged = first(condition_tagged),
              .groups = "drop"
            )
        }
        rv$all_zone_combined_cum_boxplots <- setNames(lapply(response_vars, summarize_cum_box), response_vars)
        add_console_message("Processed_data_for_cumulated_boxplots created.")
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", if (nzchar(e$message)) e$message else "Unknown error"))
      })
    })
    
    observeEvent(input$generate_delta_dfs, {
      add_console_message(sprintf("Debug: rv$processing_results is %s",
                                  if (is.null(rv$processing_results)) "NULL" else "defined"))
      tryCatch({
        if (is.null(rv$processing_results) ||
            !"Processed_Data_list" %in% names(rv$processing_results) ||
            !"Boundary_Associations_list" %in% names(rv$processing_results)) {
          add_console_message("Warning: Please run processing first.")
          return()
        }
        rv$Processed_Data_list <- map(rv$processing_results$Processed_Data_list, ~ mutate(.x, plate_id = as.character(plate_id)))
        all_zone_combined <- bind_rows(rv$Processed_Data_list)
        rv$all_zone_combined <- all_zone_combined
        
        boundaries <- bind_rows(rv$processing_results$Boundary_Associations_list) %>% distinct()
        if (nrow(boundaries) == 0) {
          add_console_message("Warning: No transitions found in Boundary_Associations_list.")
          return()
        }
        
        if (!validate_delta_time(input$delta_time)) return()
        
        if (!validate_transition(input$transition_select, boundaries, all_zone_combined)) return()
        
        response_vars <- c("totaldist","totaldur","totalct","totalspeed",
                           "lardist","lardur","larct","larspeed",
                           "smldist","smldur","smlct","smlspeed",
                           "inadist","inadur","inact","emptydur","emptyct")
        
        summarize_delta_box <- function(var) {
          delta <- as.numeric(input$delta_time)
          selected_transition <- input$transition_select
          trans_time <- boundaries$time_switch[boundaries$transition == selected_transition]
          
          all_zone_combined %>%
            mutate(transition_phase = case_when(
              start >= trans_time - delta & start < trans_time               ~ paste0(selected_transition, "_before"),
              start >= trans_time             & start < trans_time + delta   ~ paste0(selected_transition, "_switch"),
              start >= trans_time + delta     & start < trans_time + 2*delta ~ paste0(selected_transition, "_after"),
              TRUE                                                             ~ NA_character_
            )) %>%
            filter(!is.na(transition_phase)) %>%
            group_by(transition_phase, zone, condition_tagged, plate_id, animal) %>%
            summarise(
              mean_val = mean(.data[[var]], na.rm = TRUE),
              condition_grouped = first(condition_grouped),
              .groups = "drop"
            )
        }
        
        rv$all_zone_combined_delta_boxplots <- setNames(lapply(response_vars, summarize_delta_box), response_vars)
        
        add_console_message("✅ Processed_data_for_delta_boxplots created.")
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", e$message))
      })
    })
    
    observeEvent(input$generate_lineplot_dfs, {
      add_console_message(sprintf("Debug: rv$processing_results is %s", if (is.null(rv$processing_results)) "NULL" else "defined"))
      tryCatch({
        if (is.null(rv$processing_results)) {
          add_console_message("Warning: rv$processing_results is NULL. Please run processing first.")
          return()
        }
        if (!"Processed_Data_list" %in% names(rv$processing_results)) {
          add_console_message("Error: Processed_Data_list not found in rv$processing_results.")
          return()
        }
        rv$Processed_Data_list <- map(rv$processing_results$Processed_Data_list, ~ mutate(.x, plate_id = as.character(plate_id)))
        all_zone_combined <- bind_rows(rv$Processed_Data_list)
        rv$all_zone_combined <- all_zone_combined
        wells_per_condition <- compute_wells(all_zone_combined)
        rv$wells_per_condition <- wells_per_condition
        add_console_message("Well counts per condition and zone computed.")
        target_unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
        if (!validate_aggregation_period(input$aggregation_period, input$time_unit_original, target_unit, input$time_unit_convert)) {
          return()
        }
        response_vars <- c("totaldist", "totaldur", "totalct", "totalspeed", "lardist", "lardur", "larct",
                           "larspeed", "smldist", "smldur", "smlct", "smlspeed", "inadist", "inadur", "inact",
                           "emptydur", "emptyct")
        summarize_line <- function(var) {
          agg_period <- as.numeric(input$aggregation_period)
          agg_unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
          agg_seconds <- convert_time(agg_period, agg_unit, "seconds")
          add_console_message(sprintf("Debug: Aggregation period for %s is %s %s (= %s seconds)", var, agg_period, agg_unit, round(agg_seconds, 2)))
          
          group_var <- if (input$lineplot_replicate_mode == "pooled") "condition_grouped" else "condition"
          
          df <- all_zone_combined %>%
            mutate(start_rounded = floor(start / agg_seconds) * agg_seconds) %>%
            group_by(.data[[group_var]], zone, start_rounded, animal) %>%
            summarise(var_value = sum(.data[[var]], na.rm = TRUE), .groups = "drop")
          add_console_message(sprintf("Debug: After animal-level aggregation, %d rows for %s", nrow(df), var))
          
          df <- df %>%
            group_by(.data[[group_var]], zone, start_rounded) %>%
            summarise(total_val = sum(var_value, na.rm = TRUE), .groups = "drop")
          add_console_message(sprintf("Debug: After summing across animals, %d rows for %s", nrow(df), var))
          
          df <- df %>%
            left_join(wells_per_condition, by = c(group_var, "zone")) %>%
            mutate(val_per_well = total_val / n_wells)
          
          if (input$time_unit_original != agg_unit) {
            add_console_message(sprintf("Debug: Converting time axis from %s to %s for %s", input$time_unit_original, agg_unit, var))
            df <- df %>%
              mutate(start_rounded = convert_time(start_rounded, "seconds", agg_unit))
          }
          
          unique_points <- df %>%
            group_by(.data[[group_var]], zone, start_rounded) %>%
            summarise(count = n(), .groups = "drop")
          if (any(unique_points$count > 1)) {
            add_console_message(sprintf("Warning: Multiple points per time step detected in lineplot dataset for %s:", var))
            for (i in which(unique_points$count > 1)) {
              add_console_message(sprintf("  %s, Zone %s, Time %s: %d points", unique_points[[group_var]][i], unique_points$zone[i], unique_points$start_rounded[i], unique_points$count[i]))
            }
          } else {
            add_console_message(sprintf("Lineplot dataset for %s has one point per time step per %s per zone.", var, group_var))
          }
          df
        }
        rv$all_zone_combined_lineplots <- setNames(lapply(response_vars, summarize_line), response_vars)
        add_console_message("Processed_data_for_lineplots created.")
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", if (nzchar(e$message)) e$message else "Unknown error"))
      })
    })
    
    output$dataset_table <- DT::renderDataTable({
      req(input$dataset_type, input$dataset_response_var)
      df <- switch(input$dataset_type,
                   "Boxplot Rest/Vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$dataset_response_var]],
                   "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
                   "Boxplot Delta" = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
                   "Lineplot" = rv$all_zone_combined_lineplots[[input$dataset_response_var]])
      req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$figure_selector <- renderUI({
      req(input$plot_type, input$response_var)
      df <- switch(input$plot_type,
                   "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$response_var]],
                   "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
                   "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]],
                   "lineplot" = rv$all_zone_combined_lineplots[[input$response_var]])
      req(df)
      zones <- sort(unique(df$zone))
      selectInput(ns("selected_zone"), "Select Zone", choices = setNames(zones, paste("Zone", zones)), selected = zones[1])
    })
    
    generate_plot <- function(df, response_var, plot_type, boxplot_mode, lineplot_replicate_mode, selected_zone, theme_choice, condition_order, condition_colors) {
      sub <- subset(df, zone == selected_zone)
      theme_obj <- if (tolower(theme_choice) == "light") light_theme() else dark_theme()
      edge_col <- if (tolower(theme_choice) == "light") "black" else "white"
      cap_text_boxplot_rest_vibration <- stringr::str_wrap("Each point corresponds to the mean or cumulative value of the response variable for one animal.", width = 60)
      cap_text_boxplot_cumulative <- stringr::str_wrap("Each point corresponds to the cumulative value of the response variable for one animal.", width = 60)
      cap_text_boxplot_delta <- stringr::str_wrap("Each point corresponds to the mean value of the response variable for one animal around a transition.", width = 60)
      cap_text_lineplot <- stringr::str_wrap("Each line represents the normalized response variable over time for a condition.", width = 60)
      
      if (plot_type == "boxplot_rest_vibration") {
        alpha_value <- if (input$boxplot_fill_mode == "full") 0.6 else 1
        dodge_width <- 0.8
        
        if (boxplot_mode == "separated") {
          p <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +
            (if (input$boxplot_fill_mode == "full") {
              geom_boxplot(aes(fill = condition_grouped), width = 0.8, outlier.shape = NA, alpha = alpha_value, color = edge_col)
            } else {
              geom_boxplot(fill = NA, width = 0.8, outlier.shape = NA, alpha = alpha_value, color = edge_col)
            }) +
            geom_jitter(aes(fill = condition_grouped), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                        size = 1.75, alpha = 0.4, shape = 21, color = edge_col) +
            facet_wrap(~period_without_numbers, scales = "free_x") +
            scale_fill_manual(values = condition_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), caption = cap_text_boxplot_rest_vibration) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
          p_html <- ggplot(sub, aes(x = condition_grouped, y = mean_val,
                                    text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal, "<br>Plate ID: ", plate_id,
                                                  "<br>Value: ", sprintf("%.2f", mean_val)))) +
            (if (input$boxplot_fill_mode == "full") {
              geom_boxplot(aes(fill = condition_grouped), position = position_dodge2(width = 0.8, preserve = "single"), width = 0.7, outlier.shape = NA,
                           alpha = alpha_value, color = edge_col)
            } else {
              geom_boxplot(fill = NA, position = position_dodge2(width = 0.8, preserve = "single"), width = 0.7, outlier.shape = NA,
                           alpha = alpha_value, color = edge_col)
            }) +
            geom_jitter(aes(fill = condition_grouped), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                        shape = 21, size = 1.75, alpha = 0.4, color = edge_col) +
            facet_wrap(~period_without_numbers, scales = "free_x", shrink = FALSE) +
            scale_fill_manual(values = condition_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), caption = cap_text_boxplot_rest_vibration) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
        } else {
          sub <- sub %>% tidyr::complete(condition_grouped, period_without_numbers, fill = list(mean_val = NA))
          period_colors <- trimws(unlist(strsplit(input$boxplot_rest_vibration_periods_colors, ",")))
          p <- ggplot(sub, aes(x = condition_grouped, y = mean_val, fill = period_without_numbers)) +
            geom_boxplot(position = position_dodge(width = dodge_width), width = 0.7, outlier.shape = NA,
                         alpha = if (input$boxplot_fill_mode == "full") alpha_value else 0, color = edge_col) +
            geom_jitter(aes(fill = period_without_numbers), position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width),
                        shape = 21, color = edge_col, size = 1.75, alpha = 0.4) +
            scale_fill_manual(values = period_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), fill = "Period", caption = cap_text_boxplot_rest_vibration) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
          p_html <- ggplot(sub, aes(x = condition_grouped, y = mean_val,
                                    text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal, "<br>Plate ID: ", plate_id,
                                                  "<br>Value: ", sprintf("%.2f", mean_val)))) +
            geom_boxplot(aes(fill = period_without_numbers), position = position_dodge(width = dodge_width), width = 0.7, outlier.shape = NA,
                         alpha = if (input$boxplot_fill_mode == "full") alpha_value else 0, color = edge_col) +
            geom_jitter(aes(fill = period_without_numbers), position = position_jitterdodge(jitter.width = 0, dodge.width = dodge_width),
                        shape = 21, size = 1.75, alpha = 0.4, color = edge_col) +
            scale_fill_manual(values = period_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), fill = "Period", caption = cap_text_boxplot_rest_vibration) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
        }
        return(if (input$output_mode == "PNG") p else p_html)
      } else if (plot_type == "boxplot_cumulate") {
        n_animals <- compute_n_animals(sub)
        alpha_value <- if (input$boxplot_fill_mode == "full") 0.6 else 1
        
        p <- ggplot(sub, aes(x = condition_grouped, y = cum)) +
          (if (input$boxplot_fill_mode == "full") {
            geom_boxplot(aes(fill = condition_grouped), varwidth = TRUE, outlier.shape = NA, alpha = alpha_value, color = edge_col)
          } else {
            geom_boxplot(fill = NA, varwidth = TRUE, outlier.shape = NA, alpha = alpha_value, color = edge_col)
          }) +
          geom_jitter(aes(fill = condition_grouped), width = 0.2, size = 1.5, alpha = 0.6, shape = 21, color = edge_col) +
          geom_text(
            data = n_animals,
            aes(x = condition_grouped, y = y, label = paste0("n=", n)),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 3,
            color = edge_col
          ) +
          scale_fill_manual(values = condition_colors) +
          labs(y = sprintf("Cumulative %s (Zone %s)", response_var, selected_zone),
               caption = cap_text_boxplot_cumulative
          ) +
          theme_obj +
          theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 1),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()
          )
        
        p_html <- ggplot(sub, aes(x = condition_grouped, y = cum,
                                  text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal, "<br>Plate ID: ", plate_id,
                                                "<br>Value: ", sprintf("%.2f", cum)))) +
          (if (input$boxplot_fill_mode == "full") {
            geom_boxplot(aes(fill = condition_grouped), varwidth = TRUE, outlier.shape = NA, alpha = alpha_value, color = edge_col)
          } else {
            geom_boxplot(fill = NA, varwidth = TRUE, outlier.shape = NA, alpha = alpha_value, color = edge_col)
          }) +
          geom_jitter(aes(fill = condition_grouped), width = 0.2, size = 1.5, alpha = 0.6, shape = 21, color = edge_col) +
          geom_text(
            data = n_animals,
            aes(x = condition_grouped, y = y, label = paste0("n=", n)),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 3,
            color = edge_col
          ) +
          scale_fill_manual(values = condition_colors) +
          labs(y = sprintf("Cumulative %s (Zone %s)", response_var, selected_zone),
               caption = cap_text_boxplot_cumulative
          ) +
          theme_obj +
          theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 1),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()
          )
        
        return(if (input$output_mode == "PNG") p else p_html)
      } else if (plot_type == "boxplot_delta") {
        alpha_value <- if (input$boxplot_fill_mode == "full") 0.6 else 1
        dodge_width <- 0.8
        selected_transition <- input$transition_select
        sub <- sub %>% filter(grepl(selected_transition, transition_phase))
        # Définir l'ordre des phases
        sub$phase <- factor(sub$transition_phase,
                            levels = paste0(selected_transition, "_", c("before", "switch", "after")),
                            labels = c("Before", "Switch", "After"),
                            ordered = TRUE)
        
        if (boxplot_mode == "separated") {
          p <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +
            (if (input$boxplot_fill_mode == "full") {
              geom_boxplot(aes(fill = condition_grouped), width = 0.8, outlier.shape = NA, alpha = alpha_value, color = edge_col)
            } else {
              geom_boxplot(fill = NA, width = 0.8, outlier.shape = NA, alpha = alpha_value, color = edge_col)
            }) +
            geom_jitter(aes(fill = condition_grouped), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                        size = 1.75, alpha = 0.4, shape = 21, color = edge_col) +
            facet_wrap(~phase, scales = "free_x") +
            scale_fill_manual(values = condition_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), caption = cap_text_boxplot_delta) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
          
          p_html <- ggplot(sub, aes(x = condition_grouped, y = mean_val,
                                    text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal, "<br>Plate ID: ", plate_id,
                                                  "<br>Phase: ", phase, "<br>Value: ", sprintf("%.2f", mean_val)))) +
            (if (input$boxplot_fill_mode == "full") {
              geom_boxplot(aes(fill = condition_grouped), position = position_dodge(width = 0.8), width = 0.7, outlier.shape = NA,
                           alpha = alpha_value, color = edge_col)
            } else {
              geom_boxplot(fill = NA, position = position_dodge(width = 0.8), width = 0.7, outlier.shape = NA,
                           alpha = alpha_value, color = edge_col)
            }) +
            geom_jitter(aes(fill = condition_grouped), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
                        shape = 21, size = 1.75, alpha = 0.4, color = edge_col) +
            facet_wrap(~phase, scales = "free_x", shrink = FALSE) +
            scale_fill_manual(values = condition_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), caption = cap_text_boxplot_delta) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
        } else {
          sub <- sub %>% tidyr::complete(condition_grouped, transition_phase, fill = list(mean_val = NA))
          # Définir l'ordre des phases pour le mode "pooled"
          sub$transition_phase <- factor(sub$transition_phase,
                                         levels = paste0(selected_transition, "_", c("before", "switch", "after")),
                                         labels = c("Before", "Switch", "After"),
                                         ordered = TRUE)
          phase_colors <- trimws(unlist(strsplit(input$boxplot_delta_phase_colors, ",")))
          p <- ggplot(sub, aes(x = condition_grouped, y = mean_val, fill = transition_phase)) +
            geom_boxplot(position = position_dodge(width = dodge_width), width = 0.7, outlier.shape = NA,
                         alpha = if (input$boxplot_fill_mode == "full") alpha_value else 0, color = edge_col) +
            geom_jitter(aes(fill = transition_phase), position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width),
                        shape = 21, color = edge_col, size = 1.75, alpha = 0.4) +
            scale_fill_manual(values = phase_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), fill = "Phase", caption = cap_text_boxplot_delta) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
          p_html <- ggplot(sub, aes(x = condition_grouped, y = mean_val,
                                    text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal, "<br>Plate ID: ", plate_id,
                                                  "<br>Phase: ", transition_phase, "<br>Value: ", sprintf("%.2f", mean_val)))) +
            geom_boxplot(aes(fill = transition_phase), position = position_dodge(width = dodge_width), width = 0.7, outlier.shape = NA,
                         alpha = if (input$boxplot_fill_mode == "full") alpha_value else 0, color = edge_col) +
            geom_jitter(aes(fill = transition_phase), position = position_jitterdodge(jitter.width = 0, dodge.width = dodge_width),
                        shape = 21, size = 1.75, alpha = 0.4, color = edge_col) +
            scale_fill_manual(values = phase_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), fill = "Phase", caption = cap_text_boxplot_delta) +
            theme_obj +
            theme(
              plot.caption.position = "plot",
              plot.caption = element_text(hjust = 1),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank()
            )
        }
        return(if (input$output_mode == "PNG") p else p_html)
      } else if (plot_type == "lineplot") {
        group_var <- if (lineplot_replicate_mode == "pooled") "condition_grouped" else "condition"
        time_unit_label <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
        sub[[group_var]] <- factor(sub[[group_var]], levels = condition_order)
        p <- ggplot(sub, aes(x = start_rounded, y = val_per_well, color = .data[[group_var]], group = .data[[group_var]])) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.75) +
          scale_color_manual(values = condition_colors, breaks = condition_order) +
          labs(x = sprintf("Time (%s)", time_unit_label), y = sprintf("%s (Zone %s)", response_var, selected_zone),
               caption = cap_text_lineplot, color = if (lineplot_replicate_mode == "pooled") "Condition Grouped" else "Condition") +
          theme_obj +
          theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 1, margin = margin(t = 10)),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        p_html <- ggplot(sub, aes(x = start_rounded, y = val_per_well, color = .data[[group_var]], group = .data[[group_var]],
                                  text = paste0("Tag: ", .data[[group_var]], "<br>Time: ", sprintf("%.2f", start_rounded), "<br>Total wells: ", n_wells,
                                                "<br>Value: ", sprintf("%.2f", val_per_well)))) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.75) +
          scale_color_manual(values = condition_colors, breaks = condition_order) +
          labs(x = sprintf("Time (%s)", time_unit_label), y = sprintf("%s (Zone %s)", response_var, selected_zone),
               caption = cap_text_lineplot, color = if (lineplot_replicate_mode == "pooled") "Condition Grouped" else "Condition") +
          theme_obj +
          theme(
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 1, margin = margin(t = 10)),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        return(if (input$output_mode == "PNG") p else p_html)
      }
    }    
    
    observeEvent(input$generate_figure, {
      tryCatch({
        req(input$plot_type, input$selected_zone, input$response_var)
        df <- switch(input$plot_type,
                     "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$response_var]],
                     "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
                     "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]],
                     "lineplot" = rv$all_zone_combined_lineplots[[input$response_var]])
        req(df)
        group_var <- if (input$plot_type == "lineplot" && input$lineplot_replicate_mode == "separated") "condition" else "condition_grouped"
        condition_order <- if (nchar(input$condition_grouped_order) == 0) unique(df[[group_var]]) else trimws(unlist(strsplit(input$condition_grouped_order, ",")))
        condition_colors <- if (nchar(input$condition_grouped_color) == 0) brewer.pal(n = length(unique(df[[group_var]])), name = "Set1") else trimws(unlist(strsplit(input$condition_grouped_color, ",")))
        df[[group_var]] <- factor(df[[group_var]], levels = condition_order)
        if (input$plot_type == "boxplot_rest_vibration") {
          df$period_without_numbers <- factor(df$period_without_numbers, levels = c("rest", "vibration"), labels = c("Rest period", "Vibration period"))
        }
        add_console_message(sprintf("Generating %s figure for %s (Zone %s, %s theme)...", input$plot_type, input$response_var, input$selected_zone, input$theme_switch))
        p <- generate_plot(
          df = df,
          response_var = input$response_var,
          plot_type = input$plot_type,
          boxplot_mode = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else "separated",
          lineplot_replicate_mode = input$lineplot_replicate_mode,
          selected_zone = input$selected_zone,
          theme_choice = input$theme_switch,
          condition_order = condition_order,
          condition_colors = condition_colors
        )
        figure_key <- paste(
          input$response_var,
          input$selected_zone,
          tolower(input$theme_switch),
          if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode,
          input$boxplot_fill_mode,
          input$output_mode,
          if (input$plot_type == "boxplot_delta") input$transition_select else "",
          sep = "_"
        )
        rv$generated_figures[[figure_key]] <- list(
          plot = p,
          var = input$response_var,
          zone = input$selected_zone,
          theme = tolower(input$theme_switch),
          mode = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode,
          fill_mode = input$boxplot_fill_mode,
          output_mode = input$output_mode,
          transition = if (input$plot_type == "boxplot_delta") input$transition_select else NULL
        )
        rv$plot <- if (input$output_mode == "HTML") {
          if ((input$plot_type == "boxplot_rest_vibration" && input$boxplot_rest_vibration_mode == "pooled") || (input$plot_type == "boxplot_delta" && input$boxplot_delta_mode == "pooled")) {
            ggplotly(p, tooltip = "text") %>% layout(boxmode = "group")
          } else {
            ggplotly(p, tooltip = "text")
          }
        } else {
          p
        }
        add_console_message(sprintf("%s figure for %s (Zone %s, %s theme, %s fill) generated.", input$plot_type, input$response_var, input$selected_zone, tolower(input$theme_switch), input$boxplot_fill_mode))
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", e$message))
      })
    })
    
    observeEvent(input$theme_switch, {
      tryCatch({
        req(input$plot_type, input$selected_zone, input$response_var, rv$plot)
        df <- switch(input$plot_type,
                     "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$response_var]],
                     "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
                     "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]],
                     "lineplot" = rv$all_zone_combined_lineplots[[input$response_var]])
        req(df)
        group_var <- if (input$plot_type == "lineplot" && input$lineplot_replicate_mode == "separated") "condition" else "condition_grouped"
        condition_order <- if (nchar(input$condition_grouped_order) == 0) unique(df[[group_var]]) else trimws(unlist(strsplit(input$condition_grouped_order, ",")))
        condition_colors <- if (nchar(input$condition_grouped_color) == 0) brewer.pal(n = length(unique(df[[group_var]])), name = "Set1") else trimws(unlist(strsplit(input$condition_grouped_color, ",")))
        df[[group_var]] <- factor(df[[group_var]], levels = condition_order)
        if (input$plot_type == "boxplot_rest_vibration") {
          df$period_without_numbers <- factor(df$period_without_numbers, levels = c("rest", "vibration"), labels = c("Rest period", "Vibration period"))
        }
        p <- generate_plot(
          df = df,
          response_var = input$response_var,
          plot_type = input$plot_type,
          boxplot_mode = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else "separated",
          lineplot_replicate_mode = input$lineplot_replicate_mode,
          selected_zone = input$selected_zone,
          theme_choice = input$theme_switch,
          condition_order = condition_order,
          condition_colors = condition_colors
        )
        figure_key <- paste(
          input$response_var,
          input$selected_zone,
          tolower(input$theme_switch),
          if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode,
          input$boxplot_fill_mode,
          input$output_mode,
          if (input$plot_type == "boxplot_delta") input$transition_select else "",
          sep = "_"
        )
        rv$generated_figures[[figure_key]] <- list(
          plot = p,
          var = input$response_var,
          zone = input$selected_zone,
          theme = tolower(input$theme_switch),
          mode = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode,
          fill_mode = input$boxplot_fill_mode,
          output_mode = input$output_mode,
          transition = if (input$plot_type == "boxplot_delta") input$transition_select else NULL
        )
        rv$plot <- if (input$output_mode == "HTML") {
          if ((input$plot_type == "boxplot_rest_vibration" && input$boxplot_rest_vibration_mode == "pooled") || (input$plot_type == "boxplot_delta" && input$boxplot_delta_mode == "pooled")) {
            ggplotly(p, tooltip = "text") %>% layout(boxmode = "group")
          } else {
            ggplotly(p, tooltip = "text")
          }
        } else {
          p
        }
        add_console_message(sprintf("Theme switched to %s for %s (Zone %s, %s fill).", tolower(input$theme_switch), input$response_var, input$selected_zone, input$boxplot_fill_mode))
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", e$message))
      })
    })
    
    observeEvent(input$boxplot_fill_mode, {
      tryCatch({
        req(input$plot_type, input$selected_zone, input$response_var, rv$plot)
        if (!input$plot_type %in% c("boxplot_rest_vibration", "boxplot_cumulate", "boxplot_delta")) {
          return()
        }
        df <- switch(input$plot_type,
                     "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$response_var]],
                     "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
                     "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]])
        req(df)
        group_var <- if (input$plot_type == "lineplot" && input$lineplot_replicate_mode == "separated") "condition" else "condition_grouped"
        condition_order <- if (nchar(input$condition_grouped_order) == 0) unique(df[[group_var]]) else trimws(unlist(strsplit(input$condition_grouped_order, ",")))
        condition_colors <- if (nchar(input$condition_grouped_color) == 0) brewer.pal(n = length(unique(df[[group_var]])), name = "Set1") else trimws(unlist(strsplit(input$condition_grouped_color, ",")))
        df[[group_var]] <- factor(df[[group_var]], levels = condition_order)
        if (input$plot_type == "boxplot_rest_vibration") {
          df$period_without_numbers <- factor(df$period_without_numbers, levels = c("rest", "vibration"), labels = c("Rest period", "Vibration period"))
        }
        p <- generate_plot(
          df = df,
          response_var = input$response_var,
          plot_type = input$plot_type,
          boxplot_mode = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else "separated",
          lineplot_replicate_mode = input$lineplot_replicate_mode,
          selected_zone = input$selected_zone,
          theme_choice = input$theme_switch,
          condition_order = condition_order,
          condition_colors = condition_colors
        )
        figure_key <- paste(
          input$response_var,
          input$selected_zone,
          tolower(input$theme_switch),
          if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode,
          input$boxplot_fill_mode,
          input$output_mode,
          if (input$plot_type == "boxplot_delta") input$transition_select else "",
          sep = "_"
        )
        rv$generated_figures[[figure_key]] <- list(
          plot = p,
          var = input$response_var,
          zone = input$selected_zone,
          theme = tolower(input$theme_switch),
          mode = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode,
          fill_mode = input$boxplot_fill_mode,
          output_mode = input$output_mode,
          transition = if (input$plot_type == "boxplot_delta") input$transition_select else NULL
        )
        rv$plot <- if (input$output_mode == "HTML") {
          if ((input$plot_type == "boxplot_rest_vibration" && input$boxplot_rest_vibration_mode == "pooled") || (input$plot_type == "boxplot_delta" && input$boxplot_delta_mode == "pooled")) {
            ggplotly(p, tooltip = "text") %>% layout(boxmode = "group")
          } else {
            ggplotly(p, tooltip = "text")
          }
        } else {
          p
        }
        add_console_message(sprintf("Fill mode switched to %s for %s (Zone %s, %s theme).", input$boxplot_fill_mode, input$response_var, input$selected_zone, tolower(input$theme_switch)))
      }, error = function(e) {
        add_console_message(sprintf("Error: %s", e$message))
      })
    })
    
    # Auto-update du plot quand on change output_mode, boxplot_mode ou response_var
    observeEvent(
      list(
        input$output_mode,
        input$boxplot_rest_vibration_mode,
        input$boxplot_delta_mode,
        input$response_var
      ),
      {
        tryCatch({
          req(input$plot_type, input$selected_zone, input$response_var)
          # 1) sélectionner le bon df
          df <- switch(
            input$plot_type,
            "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$response_var]],
            "boxplot_cumulate"   = rv$all_zone_combined_cum_boxplots[[input$response_var]],
            "boxplot_delta"      = rv$all_zone_combined_delta_boxplots[[input$response_var]],
            "lineplot"           = rv$all_zone_combined_lineplots[[input$response_var]]
          )
          req(df)
          # 2) recalculer condition_order & condition_colors
          group_var <- if (input$plot_type=="lineplot" && input$lineplot_replicate_mode=="separated")
            "condition" else "condition_grouped"
          condition_order <- if (nzchar(input$condition_grouped_order))
            trimws(strsplit(input$condition_grouped_order, ",")[[1]])
          else unique(df[[group_var]])
          condition_colors <- if (nzchar(input$condition_grouped_color))
            trimws(strsplit(input$condition_grouped_color, ",")[[1]])
          else RColorBrewer::brewer.pal(length(unique(df[[group_var]])), "Set1")
          df[[group_var]] <- factor(df[[group_var]], levels = condition_order)
          if (input$plot_type=="boxplot_rest_vibration") {
            df$period_without_numbers <- factor(
              df$period_without_numbers,
              levels = c("rest","vibration"),
              labels = c("Rest period","Vibration period")
            )
          }
          # 3) générer le plot ggplot
          p <- generate_plot(
            df                      = df,
            response_var            = input$response_var,
            plot_type               = input$plot_type,
            boxplot_mode            = if (input$plot_type=="boxplot_rest_vibration")
              input$boxplot_rest_vibration_mode
            else if (input$plot_type=="boxplot_delta")
              input$boxplot_delta_mode
            else "separated",
            lineplot_replicate_mode = input$lineplot_replicate_mode,
            selected_zone           = input$selected_zone,
            theme_choice            = input$theme_switch,
            condition_order         = condition_order,
            condition_colors        = condition_colors
          )
          # 4) stocker dans rv$plot en PNG ou HTML
          rv$plot <- if (input$output_mode=="HTML") {
            if ((input$plot_type=="boxplot_rest_vibration" && input$boxplot_rest_vibration_mode=="pooled") ||
                (input$plot_type=="boxplot_delta"      && input$boxplot_delta_mode=="pooled")) {
              plotly::ggplotly(p, tooltip="text") %>% plotly::layout(boxmode="group")
            } else {
              plotly::ggplotly(p, tooltip="text")
            }
          } else {
            p
          }
          add_console_message(sprintf(
            "Auto-update: response_var=%s, boxplot_mode=%s, output_mode=%s — plot mis à jour",
            input$response_var,
            if (input$plot_type=="boxplot_rest_vibration")
              input$boxplot_rest_vibration_mode else input$boxplot_delta_mode,
            input$output_mode
          ))
        }, error = function(e) {
          add_console_message(sprintf("Erreur auto-update: %s", e$message))
        })
      }
    )
    
    
    observeEvent(input$plot_type, {
      if (input$plot_type == "boxplot_delta") {
        showTab(inputId = "output_tabs", target = "delta_percentage_tables", session = session)
      } else {
        hideTab(inputId = "output_tabs", target = "delta_percentage_tables", session = session)
      }
    })
    
    observeEvent(input$output_mode, {
      tryCatch({
        req(input$plot_type, input$selected_zone, input$response_var, rv$generated_figures)
        # Récupération du dataframe
        df <- switch(input$plot_type,
                     "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$response_var]],
                     "boxplot_cumulate"    = rv$all_zone_combined_cum_boxplots[[input$response_var]],
                     "boxplot_delta"       = rv$all_zone_combined_delta_boxplots[[input$response_var]],
                     "lineplot"            = rv$all_zone_combined_lineplots[[input$response_var]])
        req(df)
        # Calcul des paramètres condition_order et condition_colors
        group_var <- if (input$plot_type == "lineplot" && input$lineplot_replicate_mode == "separated") "condition" else "condition_grouped"
        condition_order  <- if (nzchar(input$condition_grouped_order)) trimws(strsplit(input$condition_grouped_order, ",")[[1]]) else unique(df[[group_var]])
        condition_colors <- if (nzchar(input$condition_grouped_color)) trimws(strsplit(input$condition_grouped_color, ",")[[1]]) else brewer.pal(length(unique(df[[group_var]])), "Set1")
        df[[group_var]] <- factor(df[[group_var]], levels = condition_order)
        if (input$plot_type == "boxplot_rest_vibration") {
          df$period_without_numbers <- factor(df$period_without_numbers, levels = c("rest", "vibration"), labels = c("Rest period", "Vibration period"))
        }
        # Génération du ggplot brut
        p <- generate_plot(
          df                      = df,
          response_var            = input$response_var,
          plot_type               = input$plot_type,
          boxplot_mode            = if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode
          else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode
          else "separated",
          lineplot_replicate_mode = input$lineplot_replicate_mode,
          selected_zone           = input$selected_zone,
          theme_choice            = input$theme_switch,
          condition_order         = condition_order,
          condition_colors        = condition_colors
        )
        # Mise à jour de rv$plot en fonction du mode
        rv$plot <- if (input$output_mode == "HTML") {
          if ((input$plot_type == "boxplot_rest_vibration" && input$boxplot_rest_vibration_mode == "pooled") ||
              (input$plot_type == "boxplot_delta"       && input$boxplot_delta_mode == "pooled")) {
            ggplotly(p, tooltip = "text") %>% layout(boxmode = "group")
          } else {
            ggplotly(p, tooltip = "text")
          }
        } else {
          p
        }
        add_console_message(sprintf("Output mode changé en %s : graphique régénéré.", input$output_mode))
      }, error = function(e) {
        add_console_message(sprintf("Erreur lors du changement de output_mode : %s", e$message))
      })
    })
    
    observeEvent(input$generate_delta_tables, {
      add_console_message("-----")
      add_console_message("🔄 Starting generation of delta percentage tables...")
      tryCatch({
        req(rv$all_zone_combined_delta_boxplots, input$transition_select)
        
        excel_output_dir <- file.path(tempdir(), "excel")
        ensure_directory(excel_output_dir)
        
        percentage_diff_results_momentum <- list()
        for (var in names(rv$all_zone_combined_delta_boxplots)) {
          add_console_message(sprintf("📊 Calculating momentum percentage differences for %s...", var))
          boxplot_data <- rv$all_zone_combined_delta_boxplots[[var]]
          
          results_momentum <- boxplot_data %>%
            mutate(momentum = sub(".*_(before|switch|after)$", "\\1", transition_phase)) %>%
            group_by(condition_grouped, zone) %>%
            nest() %>%
            mutate(
              comparison_results = map(data, function(df) {
                momentum_pairs <- list(
                  c("before", "switch"),
                  c("switch", "after"),
                  c("before", "after")
                )
                map_dfr(momentum_pairs, function(pair) {
                  m1 <- filter(df, momentum == pair[1])
                  m2 <- filter(df, momentum == pair[2])
                  if (nrow(m1) > 0 && nrow(m2) > 0) {
                    tibble(
                      momentum_comparison = paste(pair[1], pair[2], sep = "-"),
                      mean_value_1 = round(mean(m1$mean_val, na.rm = TRUE), 2),
                      mean_value_2 = round(mean(m2$mean_val, na.rm = TRUE), 2),
                      median_value_1 = round(median(m1$mean_val, na.rm = TRUE), 2),
                      median_value_2 = round(median(m2$mean_val, na.rm = TRUE), 2),
                      mean_diff_pct = round((mean_value_2 - mean_value_1) / abs(mean_value_1) * 100, 2),
                      median_diff_pct = round((median_value_2 - median_value_1) / abs(median_value_1) * 100, 2)
                    )
                  } else {
                    tibble()
                  }
                })
              })
            ) %>%
            unnest(comparison_results) %>%
            select(-data)
          
          percentage_diff_results_momentum[[var]] <- results_momentum
        }
        
        rv$percentage_diff_results_momentum <- percentage_diff_results_momentum
        
        wb_momentum <- createWorkbook()
        for (var in names(percentage_diff_results_momentum)) {
          addWorksheet(wb_momentum, var)
          writeData(wb_momentum, var, percentage_diff_results_momentum[[var]])
          
          df_out <- percentage_diff_results_momentum[[var]]
          n <- nrow(df_out)
          if (n > 0) {
            mean_col <- which(names(df_out) == "mean_diff_pct")
            median_col <- which(names(df_out) == "median_diff_pct")
            rows <- 2:(n + 1)
            
            if (length(mean_col)) {
              conditionalFormatting(wb_momentum, var, cols = mean_col, rows = rows,
                                    rule = ">0", style = createStyle(bgFill = "#b9ffb2"))
              conditionalFormatting(wb_momentum, var, cols = mean_col, rows = rows,
                                    rule = "<0", style = createStyle(bgFill = "#ffb2b2"))
            }
            if (length(median_col)) {
              conditionalFormatting(wb_momentum, var, cols = median_col, rows = rows,
                                    rule = ">0", style = createStyle(bgFill = "#b9ffb2"))
              conditionalFormatting(wb_momentum, var, cols = median_col, rows = rows,
                                    rule = "<0", style = createStyle(bgFill = "#ffb2b2"))
            }
          }
        }
        rv$delta_momentum_excel <- file.path(excel_output_dir, "delta_percentage_differences_momentum.xlsx")
        saveWorkbook(wb_momentum, rv$delta_momentum_excel, overwrite = TRUE)
        add_console_message(sprintf("🎉 Momentum delta pairwise differences saved to: %s", rv$delta_momentum_excel))
        
        percentage_diff_results_condition <- list()
        for (var in names(rv$all_zone_combined_delta_boxplots)) {
          add_console_message(sprintf("📊 Calculating condition percentage differences for %s...", var))
          boxplot_data <- rv$all_zone_combined_delta_boxplots[[var]]
          
          results_condition <- boxplot_data %>%
            mutate(momentum = sub(".*_(before|switch|after)$", "\\1", transition_phase)) %>%
            group_by(momentum, zone) %>%
            nest() %>%
            mutate(
              comparison_results = map(data, function(df) {
                condition_pairs <- combn(unique(df$condition_grouped), 2, simplify = FALSE)
                map_dfr(condition_pairs, function(pair) {
                  c1 <- filter(df, condition_grouped == pair[1])
                  c2 <- filter(df, condition_grouped == pair[2])
                  if (nrow(c1) > 0 && nrow(c2) > 0) {
                    tibble(
                      condition_comparison = paste(pair[1], pair[2], sep = "-"),
                      mean_value_1 = round(mean(c1$mean_val, na.rm = TRUE), 2),
                      mean_value_2 = round(mean(c2$mean_val, na.rm = TRUE), 2),
                      median_value_1 = round(median(c1$mean_val, na.rm = TRUE), 2),
                      median_value_2 = round(median(c2$mean_val, na.rm = TRUE), 2),
                      mean_diff_pct = round((mean_value_2 - mean_value_1) / abs(mean_value_1) * 100, 2),
                      median_diff_pct = round((median_value_2 - median_value_1) / abs(median_value_1) * 100, 2)
                    )
                  } else {
                    tibble()
                  }
                })
              })
            ) %>%
            unnest(comparison_results) %>%
            select(-data)
          
          percentage_diff_results_condition[[var]] <- results_condition
        }
        
        rv$percentage_diff_results_condition <- percentage_diff_results_condition
        
        wb_condition <- createWorkbook()
        for (var in names(percentage_diff_results_condition)) {
          addWorksheet(wb_condition, var)
          writeData(wb_condition, var, percentage_diff_results_condition[[var]])
          
          df_out <- percentage_diff_results_condition[[var]]
          n <- nrow(df_out)
          if (n > 0) {
            mean_col <- which(names(df_out) == "mean_diff_pct")
            median_col <- which(names(df_out) == "median_diff_pct")
            rows <- 2:(n + 1)
            
            if (length(mean_col)) {
              conditionalFormatting(wb_condition, var, cols = mean_col, rows = rows,
                                    rule = ">0", style = createStyle(bgFill = "#b9ffb2"))
              conditionalFormatting(wb_condition, var, cols = mean_col, rows = rows,
                                    rule = "<0", style = createStyle(bgFill = "#ffb2b2"))
            }
            if (length(median_col)) {
              conditionalFormatting(wb_condition, var, cols = median_col, rows = rows,
                                    rule = ">0", style = createStyle(bgFill = "#b9ffb2"))
              conditionalFormatting(wb_condition, var, cols = median_col, rows = rows,
                                    rule = "<0", style = createStyle(bgFill = "#ffb2b2"))
            }
          }
        }
        rv$delta_condition_excel <- file.path(excel_output_dir, "delta_percentage_differences_condition.xlsx")
        saveWorkbook(wb_condition, rv$delta_condition_excel, overwrite = TRUE)
        add_console_message(sprintf("🎉 Condition delta pairwise differences saved to: %s", rv$delta_condition_excel))
        
        add_console_message("\n🎉 Delta percentage tables generation completed!\n")
      }, error = function(e) {
        add_console_message(sprintf("Error in delta percentage tables generation: %s", e$message))
      })
    })
    
    output$delta_percentage_table <- DT::renderDataTable({
      req(input$delta_table_type, input$delta_table_var)
      table_data <- if (input$delta_table_type == "Momentum Comparisons") {
        rv$percentage_diff_results_momentum[[input$delta_table_var]]
      } else {
        rv$percentage_diff_results_condition[[input$delta_table_var]]
      }
      req(table_data)
      DT::datatable(
        table_data,
        options = list(pageLength = 10, scrollX = TRUE),
        class = "display"
      ) %>%
        DT::formatStyle(
          columns = c("mean_diff_pct", "median_diff_pct"),
          backgroundColor = DT::styleInterval(
            cuts   = 0,
            values = c("#ffb2b2", "#b9ffb2")
          )
        )
    })
    
    output$figure_plot <- renderUI({
      req(rv$plot)
      if (input$output_mode == "HTML") {
        plotlyOutput(ns("plotly_plot"), height = "600px")
      } else {
        plotOutput(ns("static_plot"), height = "600px")
      }
    })
    
    output$plotly_plot <- renderPlotly({
      req(rv$plot, input$output_mode == "HTML")
      rv$plot
    })
    
    output$static_plot <- renderPlot({
      req(rv$plot, input$output_mode == "PNG")
      rv$plot
    })
    
    output$console_output <- renderUI({
      HTML(paste(console_messages(), collapse = "<br>"))
    })
    
    observeEvent(input$download_all_for_type, {
      # Reconstruire la liste des fichiers PNG à générer, exactement comme dans votre handler
      tmp_dir <- file.path(tempdir(), paste0("all_figs_", input$plot_type))
      dfs_list <- switch(input$plot_type,
                         "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots,
                         "boxplot_cumulate"   = rv$all_zone_combined_cum_boxplots,
                         "boxplot_delta"      = rv$all_zone_combined_delta_boxplots,
                         "lineplot"           = rv$all_zone_combined_lineplots)
      for (var in names(dfs_list)) {
        df_var <- dfs_list[[var]]
        for (z in sort(unique(df_var$zone))) {
          fname <- sprintf("%s_%s_zone%s_%s_%s.png",
                           input$plot_type, var, z,
                           tolower(input$theme_switch),
                           if (input$plot_type=="lineplot") input$lineplot_replicate_mode else input$boxplot_fill_mode)
          # Ici on loggue le chemin complet
          add_console_message(sprintf("✔️ PNG généré : %s/%s", tmp_dir, fname))
        }
      }
    })
    
    
    output$save_current_figure <- downloadHandler(
      filename = function() {
        var <- input$response_var
        zone <- input$selected_zone
        theme <- tolower(input$theme_switch)
        mode <- if (input$plot_type == "boxplot_rest_vibration") input$boxplot_rest_vibration_mode else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode else if (input$plot_type == "boxplot_cumulate") "separated" else input$lineplot_replicate_mode
        fill_mode <- input$boxplot_fill_mode
        transition <- if (input$plot_type == "boxplot_delta") input$transition_select else ""
        sprintf("%s_%s_zone%s_%s_%s_%s%s.%s",
                input$plot_type, var, zone, theme, mode, fill_mode,
                if (nchar(transition) > 0) paste0("_", transition) else "",
                if (input$output_mode == "HTML") "html" else "png")
      },
      content = function(file) {
        req(rv$plot)
        if (input$output_mode == "HTML") {
          saveWidget(rv$plot, file, selfcontained = TRUE)
        } else {
          ggsave(file, plot = rv$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$download_current_dataset <- downloadHandler(
      filename = function() {
        sprintf("%s_dataset_%s.xlsx", input$dataset_type, input$dataset_response_var)
      },
      content = function(file) {
        df <- switch(input$dataset_type,
                     "Boxplot Rest/Vibration" = rv$all_zone_combined_rest_vibration_boxplots[[input$dataset_response_var]],
                     "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
                     "Boxplot Delta" = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
                     "Lineplot" = rv$all_zone_combined_lineplots[[input$dataset_response_var]])
        req(df)
        write_xlsx(df, file)
      }
    )
    
    output$download_all_datasets <- downloadHandler(
      filename = function() {
        sprintf("all_datasets_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        temp_dir <- tempdir()
        dataset_dir <- file.path(temp_dir, "datasets")
        ensure_directory(dataset_dir)
        files <- c()
        
        dataset_types <- list(
          "Boxplot Rest/Vibration" = rv$all_zone_combined_rest_vibration_boxplots,
          "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots,
          "Boxplot Delta" = rv$all_zone_combined_delta_boxplots,
          "Lineplot" = rv$all_zone_combined_lineplots
        )
        
        for (type in names(dataset_types)) {
          datasets <- dataset_types[[type]]
          for (var in names(datasets)) {
            df <- datasets[[var]]
            if (!is.null(df)) {
              filename <- sprintf("%s_dataset_%s.xlsx", type, var)
              filepath <- file.path(dataset_dir, filename)
              write_xlsx(df, filepath)
              files <- c(files, filepath)
            }
          }
        }
        
        zip::zip(file, files = files, root = temp_dir)
      },
      contentType = "application/zip"
    )
    
    output$download_current_delta_table <- downloadHandler(
      filename = function() {
        sprintf("%s_%s.xlsx", input$delta_table_type, input$delta_table_var)
      },
      content = function(file) {
        if (input$delta_table_type == "Momentum Comparisons") {
          file.copy(rv$delta_momentum_excel, file)
        } else {
          file.copy(rv$delta_condition_excel, file)
        }
      }
    )
    
    output$download_all_delta_tables <- downloadHandler(
      filename = function() {
        sprintf("all_delta_tables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        temp_dir <- tempdir()
        files <- c(rv$delta_momentum_excel, rv$delta_condition_excel)
        zip::zip(file, files = files, root = temp_dir)
      },
      contentType = "application/zip"
    )
    
    output$download_all_for_type <- downloadHandler(
      filename = function() {
        sprintf("%s_all_figures_%s.zip", input$plot_type, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(zipfile) {
        tmp_dir <- file.path(tempdir(), paste0("all_figs_", input$plot_type))
        ensure_directory(tmp_dir)
        dfs_list <- switch(
          input$plot_type,
          "boxplot_rest_vibration" = rv$all_zone_combined_rest_vibration_boxplots,
          "boxplot_cumulate"   = rv$all_zone_combined_cum_boxplots,
          "boxplot_delta"      = rv$all_zone_combined_delta_boxplots,
          "lineplot"           = rv$all_zone_combined_lineplots
        )
        condition_order <- if (nchar(input$condition_grouped_order)) trimws(strsplit(input$condition_grouped_order, ",")[[1]]) else unique(dfs_list[[1]]$condition_grouped)
        condition_colors <- if (nchar(input$condition_grouped_color)) trimws(strsplit(input$condition_grouped_color, ",")[[1]]) else brewer.pal(length(condition_order), "Set1")
        
        for (var in names(dfs_list)) {
          df_var <- dfs_list[[var]]
          for (z in sort(unique(df_var$zone))) {
            mode <- switch(input$plot_type,
                           "boxplot_rest_vibration" = input$boxplot_rest_vibration_mode,
                           "boxplot_cumulate"   = "separated",
                           "boxplot_delta"      = input$boxplot_delta_mode,
                           "lineplot"           = input$lineplot_replicate_mode)
            fname <- sprintf("%s_%s_zone%s_%s_%s_%s.png",
                             input$plot_type, var, z, tolower(input$theme_switch), mode, input$boxplot_fill_mode)
            path_out <- file.path(tmp_dir, fname)
            p <- generate_plot(
              df = df_var, response_var = var, plot_type = input$plot_type,
              boxplot_mode = mode, lineplot_replicate_mode = input$lineplot_replicate_mode,
              selected_zone = z, theme_choice = input$theme_switch,
              condition_order = condition_order, condition_colors = condition_colors
            )
            ggsave(path_out, plot = p, width = 10, height = 6, dpi = 300)
            add_console_message(sprintf("✔️ PNG généré : %s", path_out))
          }
        }
        
        # Utiliser zip::zip avec chemins complets
        files <- list.files(tmp_dir, full.names = TRUE)
        zip::zip(zipfile, files = files, root = tmp_dir)
      },
      contentType = "application/zip"
    )
  })
}