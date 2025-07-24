# R/plate_plan_module.R

library(shiny)
library(shinyjs)
library(DT)
library(readxl)
library(ggplot2)
library(plotly)
library(scales)
library(openxlsx)
library(zip)

# ---- UI du module Plate Plan ----
plate_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      box(
        title = "Plate Plan Inputs",
        width = 12,
        wellPanel(
          selectInput(ns("create_plate_plan"), "Create New Plate Plan?", 
                      choices = c("Select an option" = "", "Yes" = "yes", "No" = "no")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'yes'", ns("create_plate_plan")),
            numericInput(ns("plate_number"), "Number of Plates", value = 1, min = 1),
            selectInput(ns("plate_type"), "Plate Type (wells)", 
                        choices = c("Select a type" = "", "12", "24", "48", "96")),
            numericInput(ns("conditions_number"), "Number of Conditions", value = 1, min = 1),
            textInput(ns("conditions_name"), "Condition Names (semicolon-separated, e.g., pH 8,1;CT)", value = ""),
            numericInput(ns("replicates_number"), "Replicates per Condition", value = 1, min = 1),
            numericInput(ns("units_per_replicate"), "Units per Replicate", value = 1, min = 1),
            selectInput(ns("keep_border_wells"), "Include Border Wells?", 
                        choices = c("Select an option" = "", "Yes" = "yes", "No" = "no")),
            numericInput(ns("seed_value"), "Seed for Randomization", value = 42),
            textInput(ns("plate_plan_name_xlsx"), "Excel File Name Base", value = "plate_plan")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'no'", ns("create_plate_plan")),
            tagAppendAttributes(
              fileInput(ns("plate_plan_files"), "Upload Plate Plan Files (Excel)", 
                        multiple = TRUE, accept = c(".csv", ".xlsx")),
              `aria-label` = "Upload CSV or Excel files"
            )
          ),
          actionButton(ns("generate_plate_plan"), "Generate/Load Plate Plan")
        )
      )
    ),
    fluidRow(
      box(
        title = "Plate Plan Preview",
        width = 12,
        uiOutput(ns("plate_plan_tabs")),
        selectInput(ns("download_plate_id"), "Select Plate ID to Download", choices = NULL),
        downloadButton(ns("download_plate_plan"), "Download plate plan (.xlsx)"),
        downloadButton(ns("download_all_plate_plans"), "Download all plate plans (.zip)")
      )
    )
  )
}

# ---- Server du module Plate Plan ----
plate_plan_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # ---- Fonctions utilitaires ----
    detect_plate_type <- function(df) {
      wells <- sub("_plate_.*", "", df$animal)
      rows <- unique(sub("([A-Z]).*", "\\1", wells))
      cols <- unique(as.integer(sub("[A-Z](\\d{2})", "\\1", wells)))
      total <- length(rows) * length(cols)
      if (total %in% c(12, 24, 48, 96)) return(total) else return(NA)
    }
    
    read_file <- function(path, file_name) {
      df <- if (grepl("\\.csv$", path)) {
        read.csv2(path, sep = ";", dec = ".")
      } else {
        readxl::read_excel(path)
      }
      attr(df, "file_name") <- file_name
      df
    }
    
    # ---- Validation et activation du bouton ----
    observe({
      valid <- input$create_plate_plan != "" && 
        (input$create_plate_plan == "no" || 
           (input$plate_type != "" && input$keep_border_wells != "" && 
              !is.na(input$plate_number) && input$plate_number > 0 &&
              !is.na(input$conditions_number) && input$conditions_number > 0 &&
              !is.na(input$replicates_number) && input$replicates_number > 0 &&
              !is.na(input$units_per_replicate) && input$units_per_replicate > 0 &&
              !is.na(input$seed_value) && input$seed_value != 0 &&
              input$conditions_name != ""))
      shinyjs::toggleState("generate_plate_plan", valid)
    })
    
    # ---- Génération ou chargement des plans ----
    observeEvent(input$generate_plate_plan, {
      tryCatch({
        req(input$create_plate_plan)
        
        if (input$create_plate_plan == "yes") {
          # Parse condition names
          condition_names <- trimws(unlist(strsplit(input$conditions_name, ";")))
          if (length(condition_names) != input$conditions_number || any(condition_names == "")) {
            stop("Invalid condition names: must match number of conditions and not be empty.")
          }
          
          inputs <- list(
            create_plate_plan = input$create_plate_plan,
            plate_type = input$plate_type,
            conditions_number = input$conditions_number,
            conditions_name = condition_names,
            replicates_number = input$replicates_number,
            units_per_replicate = input$units_per_replicate,
            plate_number = input$plate_number,
            keep_border_wells = input$keep_border_wells,
            seed_value = input$seed_value,
            plate_plan_name_xlsx = input$plate_plan_name_xlsx,
            plate_plan_files = input$plate_plan_files
          )
          
          rv$plate_plan_df_list <- generate_plate_plan_shiny(inputs, write_files = FALSE)
          rv$plate_plan_type <- rep(as.integer(input$plate_type), length(rv$plate_plan_df_list))
          showNotification("Plate plan generated successfully!", type = "message")
        } else {
          req(input$plate_plan_files)
          plate_plan_list <- Map(read_file, input$plate_plan_files$datapath, input$plate_plan_files$name)
          
          # Détection des types et ajout de plate_id si absent
          rv$plate_plan_type <- lapply(plate_plan_list, detect_plate_type)
          for (i in seq_along(plate_plan_list)) {
            if (!"plate_id" %in% colnames(plate_plan_list[[i]])) {
              plate_plan_list[[i]]$plate_id <- paste0("plate_", i)
            }
          }
          
          rv$plate_plan_df_list <- plate_plan_list
          showNotification("Plate plans loaded successfully!", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        message(paste("Error in generate_plate_plan_shiny:", e$message))
      })
    })
    
    # ---- Mise à jour des choix de téléchargement ----
    observe({
      if (!is.null(rv$plate_plan_df_list) && length(rv$plate_plan_df_list) > 0) {
        plate_ids <- sapply(seq_along(rv$plate_plan_df_list), function(i) {
          file_name <- attr(rv$plate_plan_df_list[[i]], "file_name")
          if (!is.null(file_name)) file_name else rv$plate_plan_df_list[[i]]$plate_id[1]
        })
        updateSelectInput(session, "download_plate_id", choices = plate_ids, selected = plate_ids[1])
      } else {
        updateSelectInput(session, "download_plate_id", choices = NULL, selected = NULL)
      }
    })
    
    # ---- Options communes pour DataTable ----
    dt_options <- list(
      pageLength = 25,
      autoWidth = TRUE,
      orderClasses = TRUE
    )
    
    # ---- Rendu des onglets ----
    output$plate_plan_tabs <- renderUI({
      req(rv$plate_plan_df_list)
      if (length(rv$plate_plan_df_list) == 0) {
        return(div("No plate plans generated yet. Please generate or load plate plans."))
      }
      
      tabs <- lapply(seq_along(rv$plate_plan_df_list), function(i) {
        file_name <- attr(rv$plate_plan_df_list[[i]], "file_name")
        title <- if (!is.null(file_name)) file_name else rv$plate_plan_df_list[[i]]$plate_id[1]
        tabPanel(
          title = title,
          tabsetPanel(
            tabPanel("Tableau", DT::dataTableOutput(session$ns(paste0("plate_plan_table_", i)))),
            tabPanel("Figure", plotly::plotlyOutput(session$ns(paste0("plate_plan_figure_", i))))
          )
        )
      })
      do.call(tabsetPanel, tabs)
    })
    
    # ---- Rendu des tables ----
    observe({
      req(rv$plate_plan_df_list)
      lapply(seq_along(rv$plate_plan_df_list), function(i) {
        output[[paste0("plate_plan_table_", i)]] <- DT::renderDataTable({
          DT::datatable(rv$plate_plan_df_list[[i]], filter = "top", options = dt_options)
        })
      })
    })
    
    # ---- Fonctions utilitaires pour figures ----
    extract_row_col <- function(df) {
      df$Row <- sub("([A-Z])\\d{2}_plate_\\d", "\\1", df$animal)
      df$Column <- as.integer(sub("[A-Z](\\d{2})_plate_\\d", "\\1", df$animal))
      df$well_id <- sub("_plate_.*", "", df$animal)
      df
    }
    
    get_conditions <- function(df) {
      if ("Condition_Base" %in% colnames(df)) {
        df$Condition <- df$Condition_Base
        df$Condition_Base <- NULL
      } else {
        df$Condition <- ifelse(is.na(df$condition) | df$condition == "", "X", sub("_\\d+$", "", df$condition))
      }
      df
    }
    
    generate_colors <- function(conditions) {
      colors <- scales::hue_pal()(length(conditions))
      setNames(c(colors, "#FFFFFF"), c(conditions, "X"))
    }
    
    # ---- Rendu des figures ----
    observe({
      req(rv$plate_plan_df_list)
      lapply(seq_along(rv$plate_plan_df_list), function(i) {
        output[[paste0("plate_plan_figure_", i)]] <- plotly::renderPlotly({
          df <- rv$plate_plan_df_list[[i]]
          
          # Vérifier que la colonne condition existe
          if (!"condition" %in% colnames(df) && !"Condition_Base" %in% colnames(df)) {
            return(plotly::plot_ly() %>% 
                     plotly::layout(title = paste("Plate", i, "Layout"), 
                                    annotations = list(text = "No valid conditions found in data", 
                                                       showarrow = FALSE)))
          }
          
          df <- extract_row_col(df)
          df <- get_conditions(df)
          
          conditions <- unique(df$Condition[df$Condition != "X"])
          if (length(conditions) == 0) {
            return(plotly::plot_ly() %>% 
                     plotly::layout(title = paste("Plate", i, "Layout"), 
                                    annotations = list(text = "No valid conditions found", showarrow = FALSE)))
          }
          
          plate_type <- as.character(rv$plate_plan_type[[i]])
          if (is.na(plate_type)) {
            return(plotly::plot_ly() %>% 
                     plotly::layout(title = paste("Plate", i, "- Unknown format")))
          }
          
          well_configs <- list(
            "12" = list(rows = LETTERS[1:3], cols = 1:4),
            "24" = list(rows = LETTERS[1:4], cols = 1:6),
            "48" = list(rows = LETTERS[1:6], cols = 1:8),
            "96" = list(rows = LETTERS[1:8], cols = 1:12)
          )
          config <- well_configs[[plate_type]]
          
          grid <- expand.grid(Row = config$rows, Column = config$cols)
          grid$Row <- factor(grid$Row, levels = rev(config$rows))
          grid$Column <- factor(grid$Column, levels = as.character(1:length(config$cols)))
          df$Column <- factor(df$Column, levels = as.character(1:length(config$cols)))
          df <- merge(grid, df, by = c("Row", "Column"), all.x = TRUE)
          df$Condition <- ifelse(is.na(df$Condition), "X", df$Condition)
          
          colors <- generate_colors(conditions)
          
          p <- ggplot(df, aes(x = Column, y = Row, fill = Condition)) +
            geom_tile(color = "black") +
            geom_text(aes(label = well_id), size = 2, color = "black") +
            scale_fill_manual(values = colors) +
            scale_x_discrete(breaks = as.character(1:length(config$cols)), labels = as.character(1:length(config$cols))) +
            labs(title = paste("Plate", i, "Layout"), x = "Column", y = "Row") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 0), panel.grid = element_blank())
          
          aspect_ratio <- length(config$rows) / length(config$cols)
          fig <- plotly::ggplotly(p, width = 600, height = 600 * aspect_ratio) %>%
            plotly::layout(margin = list(l = 50, r = 50, b = 50, t = 50))
          
          fig
        })
      })
    })
    
    # ---- Téléchargement individuel ----
    output$download_plate_plan <- downloadHandler(
      filename = function() {
        sprintf("%s_%s.xlsx", input$plate_plan_name_xlsx, input$download_plate_id)
      },
      content = function(file) {
        req(input$download_plate_id)
        idx <- which(sapply(rv$plate_plan_df_list, function(df) df$plate_id[1]) == input$download_plate_id)
        openxlsx::write.xlsx(rv$plate_plan_df_list[[idx]], file, rowNames = FALSE)
        showNotification("Plate plan downloaded!", type = "message")
      }
    )
    
    # ---- Téléchargement ZIP ----
    output$download_all_plate_plans <- downloadHandler(
      filename = function() {
        paste0(input$plate_plan_name_xlsx, "_all.zip")
      },
      content = function(file) {
        temp_dir <- tempdir()
        xlsx_files <- sapply(seq_along(rv$plate_plan_df_list), function(i) {
          path <- file.path(temp_dir, sprintf("%s_plate_%d.xlsx", input$plate_plan_name_xlsx, i))
          openxlsx::write.xlsx(rv$plate_plan_df_list[[i]], path, rowNames = FALSE)
          path
        })
        zip::zip(file, files = xlsx_files, mode = "cherry-pick")
        showNotification("All plate plans downloaded as ZIP!", type = "message")
      }
    )
  })
}
# ---- Fonction utilitaire pour générer les plans de plaque ----
generate_plate_plan_shiny <- function(inputs, plan_dir = "inputs/plate_plans", write_files = FALSE) {
  if (write_files && !dir.exists(plan_dir)) dir.create(plan_dir, recursive = TRUE)
  
  if (inputs$create_plate_plan == "yes") {
    plate_type <- as.integer(inputs$plate_type)
    cond_n <- inputs$conditions_number
    cond_names <- inputs$conditions_name
    repl_n <- inputs$replicates_number
    units_n <- inputs$units_per_replicate
    plate_number <- inputs$plate_number
    border_pref <- inputs$keep_border_wells
    seed_val <- inputs$seed_value
    base_xlsx <- inputs$plate_plan_name_xlsx
    
    # ---- Configurations des plaques ----
    well_configs <- list(
      "12" = list(rows = LETTERS[1:3], cols = 1:4, border = c("A01","A02","A03","A04","B01","B04","C01","C02","C03","C04")),
      "24" = list(rows = LETTERS[1:4], cols = 1:6, border = c("A01","A02","A03","A04","A05","A06","B01","B06","C01","C06","D01","D02","D03","D04","D05","D06")),
      "48" = list(rows = LETTERS[1:6], cols = 1:8, border = c("A01","A02","A03","A04","A05","A06","A07","A08","B01","B08","C01","C08","D01","D08","E01","E08","F01","F02","F03","F04","F05","F06","F07","F08")),
      "96" = list(rows = LETTERS[1:8], cols = 1:12, border = c(paste0("A", sprintf("%02d", 1:12)), paste0(rep(c("B","C","D","E","F","G"), each = 2), sprintf("%02d", c(1,12))), paste0("H", sprintf("%02d", 1:12))))
    )
    config <- well_configs[[as.character(plate_type)]]
    wells_template <- with(expand.grid(Row = config$rows, Column = config$cols), paste0(Row, sprintf("%02d", Column)))
    available_wells <- if (border_pref %in% c("no", "n")) setdiff(wells_template, config$border) else wells_template
    available_total <- length(available_wells) * plate_number
    
    total_units <- cond_n * repl_n * units_n
    if (total_units > available_total) stop("Total units exceed available wells.")
    
    set.seed(seed_val)
    
    # ---- Répartition des conditions ----
    cond_counts <- rep(repl_n * units_n, length(cond_names))
    names(cond_counts) <- cond_names
    cond_by_plate <- lapply(cond_names, function(cond) {
      total_c <- cond_counts[cond]
      base <- floor(total_c / plate_number)
      rem <- total_c %% plate_number
      counts <- rep(base, plate_number)
      if (rem > 0) counts[1:rem] <- counts[1:rem] + 1
      counts
    })
    names(cond_by_plate) <- cond_names
    
    plate_plan_list <- list()
    for (i in 1:plate_number) {
      this_labels <- unlist(lapply(names(cond_by_plate), function(cond) {
        total_u <- cond_by_plate[[cond]][i]
        base <- floor(total_u / repl_n)
        rem <- total_u %% repl_n
        rep_counts <- rep(base, repl_n)
        if (rem > 0) rep_counts[1:rem] <- rep_counts[1:rem] + 1
        unlist(mapply(function(r, cnt) rep(paste0(cond, "_", r), cnt), 1:repl_n, rep_counts, SIMPLIFY = FALSE))
      }))
      
      plate_assign <- rep("X", length(wells_template))
      avail_idx <- if (border_pref %in% c("no", "n")) which(!wells_template %in% config$border) else seq_along(wells_template)
      choice_idx <- sample(avail_idx, length(this_labels))
      plate_assign[choice_idx] <- this_labels
      
      df <- data.frame(
        animal = paste0(wells_template, "_plate_", i),
        condition = plate_assign,
        plate_id = paste0("plate_", i),
        stringsAsFactors = FALSE
      )
      
      attr(df, "file_name") <- sprintf("%s_plate_%d.xlsx", base_xlsx, i)
      
      if (write_files) {
        xlsx_path <- file.path(plan_dir, sprintf("%s_plate_%d.xlsx", base_xlsx, i))
        openxlsx::write.xlsx(df, xlsx_path, rowNames = FALSE)
      }
      
      plate_plan_list[[i]] <- df
    }
    return(plate_plan_list)
  } else {
    req(inputs$plate_plan_files)
    Map(read_file, inputs$plate_plan_files$datapath, inputs$plate_plan_files$name)
  }
}