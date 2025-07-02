# Plate Plan Application
# Ce fichier contient l'UI, le serveur, et la fonction utilitaire pour générer les plans de plaque

# UI du module Plate Plan
plate_plan_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Plate Plan Inputs",
      width = 4,
      selectInput(ns("create_plate_plan"), "Create New Plate Plan?", choices = c("", "yes", "no"), selected = ""),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'yes'", ns("create_plate_plan")),
        numericInput(ns("plate_number"), "Number of Plates", value = NULL, min = 1),
        selectInput(ns("plate_type"), "Plate Type (wells)", choices = c("", 12, 24, 48, 96), selected = ""),
        numericInput(ns("conditions_number"), "Number of Conditions", value = NULL, min = 1),
        textInput(ns("conditions_name"), "Condition Names (semicolon-separated, e.g., pH 8,1;CT)", value = ""),
        numericInput(ns("replicates_number"), "Replicates per Condition", value = NULL, min = 1),
        numericInput(ns("units_per_replicate"), "Units per Replicate", value = NULL, min = 1),
        selectInput(ns("keep_border_wells"), "Include Border Wells?", choices = c("", "yes", "no"), selected = ""),
        numericInput(ns("seed_value"), "Seed for Randomization", value = NULL),
        textInput(ns("plate_plan_name_xlsx"), "Excel File Name Base", value = "")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'no'", ns("create_plate_plan")),
        fileInput(ns("plate_plan_files"), "Upload Plate Plan Files (Excel)", multiple = TRUE, accept = c(".csv", ".xlsx"))
      ),
      actionButton(ns("generate_plate_plan"), "Generate/Load Plate Plan")
    ),
    box(
      title = "Plate Plan Preview",
      width = 8,
      uiOutput(ns("plate_plan_tabs")),
      selectInput(ns("download_plate_id"), "Select Plate ID to Download", choices = NULL),
      downloadButton(ns("download_plate_plan"), "Download plate plan (.xlsx)"),
      downloadButton(ns("download_all_plate_plans"), "Download all plate plans (.zip)")
    )
  )
}

# Server du module Plate Plan
plate_plan_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Generate or Load Plate Plan
    observeEvent(input$generate_plate_plan, {
      tryCatch({
        if (input$create_plate_plan == "") {
          stop("Please select 'yes' or 'no' for Create New Plate Plan.")
        }
        
        if (input$create_plate_plan == "yes") {
          if (input$plate_type == "") {
            stop("Please select a Plate Type.")
          }
          if (input$keep_border_wells == "") {
            stop("Please select whether to include Border Wells.")
          }
          if (is.na(input$plate_number) || input$plate_number <= 0) {
            stop("Number of Plates must be a positive integer.")
          }
          if (is.na(input$conditions_number) || input$conditions_number <= 0) {
            stop("Number of Conditions must be a positive integer.")
          }
          if (is.na(input$replicates_number) || input$replicates_number <= 0) {
            stop("Replicates per Condition must be a positive integer.")
          }
          if (is.na(input$units_per_replicate) || input$units_per_replicate <= 0) {
            stop("Units per Replicate must be a positive integer.")
          }
          if (is.na(input$seed_value) || input$seed_value == 0) {
            stop("Seed for Randomization must be a non-zero integer.")
          }
          
          # Parse condition names with semicolon separator
          cleaned_conditions_name <- gsub("^\"|\"$", "", input$conditions_name)
          if (cleaned_conditions_name == "") {
            stop("Condition names cannot be empty.")
          }
          condition_names <- trimws(unlist(strsplit(cleaned_conditions_name, ";")))
          
          # Validation des noms des conditions
          if (length(condition_names) != input$conditions_number) {
            stop(sprintf("Number of condition names (%d) must match Number of Conditions (%d).", 
                         length(condition_names), input$conditions_number))
          }
          if (any(condition_names == "")) {
            stop("One or more condition names are empty or invalid.")
          }
          
          # Créer la liste inputs avec condition_names (liste parsée)
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
          
          rv$plate_plan_df_list <- generate_plate_plan_shiny(inputs)
          rv$plate_plan_type <- rep(as.integer(input$plate_type), length(rv$plate_plan_df_list))
          showNotification("Plate plan generated successfully!", type = "message")
        } else {
          req(input$plate_plan_files)
          plate_plan_list <- lapply(input$plate_plan_files$datapath, function(path) {
            if (grepl("\\.csv$", path)) {
              read.csv2(path, sep = ";", dec = ".")
            } else {
              readxl::read_excel(path)
            }
          })
          
          # Détection du format de plaque
          plate_types <- lapply(plate_plan_list, function(df) {
            wells <- sub("_plate_.*", "", df$animal)
            rows <- unique(sub("([A-Z]).*", "\\1", wells))
            cols <- unique(as.integer(sub("[A-Z](\\d{2})", "\\1", wells)))
            nr <- length(rows)
            nc <- length(cols)
            total <- nr * nc
            if (total == 12) return(12)
            else if (total == 24) return(24)
            else if (total == 48) return(48)
            else if (total == 96) return(96)
            else return(NA)
          })
          
          # Ajout de plate_id si absent
          for (i in seq_along(plate_plan_list)) {
            if (!"plate_id" %in% colnames(plate_plan_list[[i]])) {
              plate_plan_list[[i]]$plate_id <- paste0("plate_", i)
            }
          }
          
          rv$plate_plan_df_list <- plate_plan_list
          rv$plate_plan_type <- plate_types
          showNotification("Plate plans loaded successfully!", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        print(paste("Error in generate_plate_plan_shiny:", e$message))
      })
    })
    
    # Update Download Plate Plan Choices
    observe({
      if (!is.null(rv$plate_plan_df_list) && length(rv$plate_plan_df_list) > 0) {
        plate_ids <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
        updateSelectInput(session, "download_plate_id", choices = plate_ids, selected = plate_ids[1])
      } else {
        updateSelectInput(session, "download_plate_id", choices = NULL, selected = NULL)
      }
    })
    
    # Render Plate Plan Tabs
    output$plate_plan_tabs <- renderUI({
      print("Rendering plate_plan_tabs")
      if (is.null(rv$plate_plan_df_list) || length(rv$plate_plan_df_list) == 0) {
        print("No plate plans available to render")
        return(div("No plate plans generated yet. Please generate or load plate plans."))
      }
      print("Generating tabs for plates")
      tabs <- lapply(seq_along(rv$plate_plan_df_list), function(i) {
        print(paste("Creating tab for Plate", i))
        tabPanel(
          title = paste("Plate", i),
          tabsetPanel(
            tabPanel("Tableau", DT::dataTableOutput(session$ns(paste0("plate_plan_table_", i)))),
            tabPanel("Figure", plotly::plotlyOutput(session$ns(paste0("plate_plan_figure_", i))))
          )
        )
      })
      do.call(tabsetPanel, tabs)
    })
    
    # Render Plate Plan Tables (one per plate)
    observe({
      req(rv$plate_plan_df_list)
      print("Rendering plate plan tables")
      lapply(seq_along(rv$plate_plan_df_list), function(i) {
        print(paste("Rendering table for Plate", i))
        output[[paste0("plate_plan_table_", i)]] <- DT::renderDataTable({
          DT::datatable(
            rv$plate_plan_df_list[[i]],
            filter = "top",
            options = list(
              pageLength = 10,
              autoWidth = TRUE,
              orderClasses = TRUE
            )
          )
        })
      })
    })
    
    # Render Plate Plan Figures (one per plate)
    observe({
      req(rv$plate_plan_df_list)
      lapply(seq_along(rv$plate_plan_df_list), function(i) {
        output[[paste0("plate_plan_figure_", i)]] <- plotly::renderPlotly({
          df <- rv$plate_plan_df_list[[i]]
          
          # Vérifier que la colonne condition existe et contient des valeurs
          if (!"condition" %in% colnames(df) || all(is.na(df$condition) | df$condition == "")) {
            return(plotly::plot_ly() %>% 
                     plotly::layout(title = paste("Plate", i, "Layout"), 
                                    annotations = list(text = "No valid conditions found in data", 
                                                       showarrow = FALSE)))
          }
          
          # Extraire Row et Column à partir de animal
          df$Row <- sub("([A-Z])\\d{2}_plate_\\d", "\\1", df$animal)
          df$Column <- as.integer(sub("[A-Z](\\d{2})_plate_\\d", "\\1", df$animal))
          
          # Ajouter la colonne well_id pour les identifiants des puits (A01, C12, etc.)
          df$well_id <- sub("_plate_.*", "", df$animal)
          
          # Extraire Condition_Base (en gérant les cas où condition est NA ou vide)
          df$Condition_Base <- ifelse(is.na(df$condition) | df$condition == "", 
                                      "X", 
                                      sub("_\\d+$", "", df$condition))
          
          # Récupérer le type de plaque détecté
          plate_type <- as.character(rv$plate_plan_type[[i]])
          if (is.na(plate_type)) {
            return(plotly::plot_ly() %>% 
                     plotly::layout(
                       title = paste("Plate", i, "- format non reconnu"),
                       annotations = list(
                         text = "Impossible de détecter le format de cette plaque",
                         showarrow = FALSE
                       )
                     ))
          }
          
          # Définir les configurations des plaques
          well_configs <- list(
            "12" = list(rows = LETTERS[1:3], cols = 1:4),
            "24" = list(rows = LETTERS[1:4], cols = 1:6),
            "48" = list(rows = LETTERS[1:6], cols = 1:8),
            "96" = list(rows = LETTERS[1:8], cols = 1:12)
          )
          config <- well_configs[[as.character(plate_type)]]
          
          # Déterminer le nombre maximum de colonnes
          max_cols <- length(config$cols)
          
          # Convertir Column en facteur pour avoir des entiers sur l'axe des abscisses
          df$Column <- factor(df$Column, levels = as.character(1:max_cols))
          
          # Créer la grille et fusionner
          grid <- expand.grid(Row = config$rows, Column = config$cols)
          grid$Row <- factor(grid$Row, levels = rev(config$rows))
          grid$Column <- factor(grid$Column, levels = as.character(1:max_cols))
          df <- merge(grid, df, by = c("Row", "Column"), all.x = TRUE)
          df$Condition_Base <- ifelse(is.na(df$Condition_Base), "X", df$Condition_Base)
          
          # Obtenir les conditions uniques (exclure "X")
          conditions <- unique(df$Condition_Base[df$Condition_Base != "X"])
          
          # Gérer le cas où aucune condition valide n'est trouvée
          if (length(conditions) == 0) {
            return(plotly::plot_ly() %>% 
                     plotly::layout(title = paste("Plate", i, "Layout"), 
                                    annotations = list(text = "No valid conditions found in data", 
                                                       showarrow = FALSE)))
          }
          
          # Générer la palette de couleurs
          colors <- scales::hue_pal()(length(conditions))
          colors <- setNames(c(colors, "#FFFFFF"), c(conditions, "X"))
          
          # Créer la figure
          p <- ggplot(df, aes(x = Column, y = Row, fill = Condition_Base)) +
            geom_tile(color = "black") +
            geom_text(aes(label = well_id), size = 2, color = "black") +
            scale_fill_manual(values = colors) +
            scale_x_discrete(breaks = as.character(1:max_cols), labels = as.character(1:max_cols)) +
            labs(title = paste("Plate", i, "Layout"), x = "Column", y = "Row") +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 0),
              panel.grid = element_blank()
            )
          
          # Calculer le ratio d'aspect (hauteur/largeur)
          aspect_ratio <- length(config$rows) / length(config$cols)
          
          # Convertir en Plotly
          fig <- plotly::ggplotly(
            p,
            width = 600,
            height = 600 * aspect_ratio
          )
          
          # Ajuster les marges
          fig <- plotly::layout(
            fig,
            margin = list(l = 50, r = 50, b = 50, t = 50)
          )
          
          fig
        })
      })
    })
    
    # Download Individual Plate Plan
    output$download_plate_plan <- downloadHandler(
      filename = function() {
        sprintf("%s_plate_%s.xlsx", input$plate_plan_name_xlsx, input$download_plate_id)
      },
      content = function(file) {
        req(input$download_plate_id)
        plate_id <- input$download_plate_id
        df <- rv$plate_plan_df_list[[which(sapply(rv$plate_plan_df_list, function(df) df$plate_id[1]) == plate_id)]]
        openxlsx::write.xlsx(df, file, rowNames = FALSE)
      }
    )
    
    # Download All Plate Plans as ZIP
    output$download_all_plate_plans <- downloadHandler(
      filename = function() {
        paste0(input$plate_plan_name_xlsx, "_all_plates.zip")
      },
      content = function(file) {
        temp_dir <- tempdir()
        xlsx_files <- character()
        for (i in seq_along(rv$plate_plan_df_list)) {
          xlsx_file <- file.path(temp_dir, sprintf("%s_plate_%d.xlsx", input$plate_plan_name_xlsx, i))
          openxlsx::write.xlsx(rv$plate_plan_df_list[[i]], xlsx_file, rowNames = FALSE)
          xlsx_files <- c(xlsx_files, xlsx_file)
        }
        zip::zip(file, files = xlsx_files, mode = "cherry-pick")
        showNotification("All plate plans downloaded as a ZIP file!", type = "message")
      }
    )
  })
}

# Fonction utilitaire pour générer les plans de plaque
generate_plate_plan_shiny <- function(inputs, plan_dir = "inputs/plate_plans") {
  if (!dir.exists(plan_dir)) dir.create(plan_dir, recursive = TRUE)
  
  if (inputs$create_plate_plan == "yes") {
    plate_type <- as.integer(inputs$plate_type)
    cond_n <- as.integer(inputs$conditions_number)
    
    # Gérer inputs$conditions_name (liste ou chaîne)
    if (is.character(inputs$conditions_name) && length(inputs$conditions_name) == 1) {
      cond_names <- trimws(unlist(strsplit(inputs$conditions_name, ";")))
    } else {
      cond_names <- inputs$conditions_name
    }
    
    repl_n <- as.integer(inputs$replicates_number)
    units_n <- as.integer(inputs$units_per_replicate)
    plate_number <- as.integer(inputs$plate_number)
    border_pref <- inputs$keep_border_wells
    seed_val <- as.integer(inputs$seed_value)
    base_xlsx <- inputs$plate_plan_name_xlsx
    
    # Valider le nombre de conditions
    if (length(cond_names) != cond_n) {
      stop(sprintf("Number of condition names (%d) must match conditions_number (%d).", 
                   length(cond_names), cond_n))
    }
    
    # Valider que les noms ne sont pas vides
    if (any(cond_names == "")) {
      stop("One or more condition names are empty or invalid.")
    }
    
    # Define plate grid (supports 12, 24, 48, and 96 wells)
    rows <- LETTERS[1:ifelse(plate_type == 12, 3,
                             ifelse(plate_type == 24, 4,
                                    ifelse(plate_type == 48, 6, 8)))]
    cols <- 1:ifelse(plate_type == 12, 4,
                     ifelse(plate_type == 24, 6,
                            ifelse(plate_type == 48, 8, 12)))
    wells_template <- with(expand.grid(Row = rows, Column = cols),
                           paste0(Row, sprintf("%02d", Column)))
    total_wells <- length(wells_template)
    
    # Explicit border wells for each plate format
    if (plate_type == 12) {
      explicit_border <- c(
        "A01","A02","A03","A04",
        "B01","B04",
        "C01","C02","C03","C04"
      )
    } else if (plate_type == 24) {
      explicit_border <- c(
        "A01","A02","A03","A04","A05","A06",
        "B01","B06",
        "C01","C06",
        "D01","D02","D03","D04","D05","D06"
      )
    } else if (plate_type == 48) {
      explicit_border <- c(
        "A01","A02","A03","A04","A05","A06","A07","A08",
        "B01","B08",
        "C01","C08",
        "D01","D08",
        "E01","E08",
        "F01","F02","F03","F04","F05","F06","F07","F08"
      )
    } else if (plate_type == 96) {
      explicit_border <- c(
        paste0("A", sprintf("%02d", 1:12)),
        paste0(rep(c("B","C","D","E","F","G"), each = 2),
               sprintf("%02d", c(1,12))),
        paste0("H", sprintf("%02d", 1:12))
      )
    }
    
    if (border_pref %in% c("no","n")) {
      available_wells <- setdiff(wells_template, explicit_border)
    } else {
      available_wells <- wells_template
    }
    available_total <- length(available_wells) * plate_number
    
    total_units <- cond_n * repl_n * units_n
    message(sprintf("total_units = %d ; available_total = %d", total_units, available_total))
    if (total_units > available_total) stop("Total units exceed available wells.")
    
    set.seed(seed_val)
    
    # Répartition équilibrée des conditions sur les plaques
    cond_counts <- rep(repl_n * units_n, length(cond_names))
    names(cond_counts) <- cond_names
    cond_by_plate <- lapply(cond_names, function(cond) {
      total_c <- cond_counts[cond]
      base <- floor(total_c / plate_number)
      rem  <- total_c %% plate_number
      counts <- rep(base, plate_number)
      if (rem > 0) counts[seq_len(rem)] <- counts[seq_len(rem)] + 1
      counts
    })
    names(cond_by_plate) <- cond_names
    
    plate_plan_list <- list()
    for (i in 1:plate_number) {
      # Générer les étiquettes pour cette plaque
      this_labels <- unlist(lapply(names(cond_by_plate), function(cond) {
        total_u <- cond_by_plate[[cond]][i]
        base <- floor(total_u / repl_n)
        rem  <- total_u %% repl_n
        rep_counts <- rep(base, repl_n)
        if (rem > 0) rep_counts[seq_len(rem)] <- rep_counts[seq_len(rem)] + 1
        unlist(mapply(function(r, cnt) 
          rep(paste0(cond, "_", r), cnt),
          seq_len(repl_n), rep_counts,
          SIMPLIFY = FALSE))
      }))
      
      # Initialiser avec "X"
      plate_assign <- rep("X", total_wells)
      
      # Assigner les conditions uniquement aux indices disponibles
      avail_idx <- if (border_pref %in% c("no", "n")) {
        seq_along(wells_template)[!wells_template %in% explicit_border]
      } else {
        seq_along(wells_template)
      }
      choice_idx <- sample(avail_idx, length(this_labels))
      plate_assign[choice_idx] <- this_labels
      
      # Vérifier que les puits de bordure sont bien "X"
      if (border_pref %in% c("no", "n")) {
        border_indices <- which(wells_template %in% explicit_border)
        if (any(plate_assign[border_indices] != "X")) {
          stop("Border wells contain conditions on plate ", i, "!")
        }
      }
      
      df <- data.frame(
        animal = paste0(wells_template, "_plate_", i),
        condition = plate_assign,
        plate_id = i,
        stringsAsFactors = FALSE
      )
      
      # Écrire le fichier Excel
      xlsx_path <- file.path(plan_dir, sprintf("%s_plate_%d.xlsx", base_xlsx, i))
      openxlsx::write.xlsx(df, file = xlsx_path, rowNames = FALSE)
      
      plate_plan_list[[i]] <- df
    }
    return(plate_plan_list)
  } else {
    req(inputs$plate_plan_files)
    plate_plan_list <- lapply(inputs$plate_plan_files$datapath, function(path) {
      if (grepl("\\.csv$", path)) {
        read.csv2(path, sep = ";", dec = ".")
      } else {
        readxl::read_excel(path)
      }
    })
    return(plate_plan_list)
  }
}