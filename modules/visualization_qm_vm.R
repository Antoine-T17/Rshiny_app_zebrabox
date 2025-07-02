# R/visualization_module.R

# Themes
light_theme <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.title       = element_text(color = "black", size = 14, hjust = 0.5),
    axis.text.y      = element_text(color = "black", size = 12),
    axis.text.x      = element_text(color = "black", size = 12),
    axis.title.x     = element_text(color = "black", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y     = element_text(color = "black", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position  = "right",
    legend.text      = element_text(color = "black", size = 12, face = "italic"),
    legend.title     = element_blank(),
    strip.text.x     = element_text(size = 12),
    strip.background = element_rect(fill = "white"),
    plot.caption     = element_text(color = "black", size = 8, hjust = 1, margin = margin(t = 10))
  )
}

dark_theme <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.title         = element_text(color = "white", size = 14, hjust = 0.5),
    axis.text.y        = element_text(color = "white", size = 12),
    axis.text.x        = element_text(color = "white", size = 12),
    axis.title.x       = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y       = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position    = "right",
    legend.text        = element_text(color = "white", size = 12, face = "italic"),
    legend.title       = element_blank(),
    legend.background  = element_rect(fill = "black"),
    legend.key         = element_rect(fill = "black"),
    strip.text.x       = element_text(color = "white", size = 12),
    strip.background   = element_rect(fill = "black", color = "white"),
    plot.background    = element_rect(fill = "black"),
    panel.background   = element_rect(fill = "black"),
    panel.border       = element_rect(color = "white", fill = NA),
    panel.grid.major   = element_line(color = "grey30"),
    panel.grid.minor   = element_line(color = "grey30"),
    plot.caption       = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
  )
}

# UI du module Visualization
visualization_qm_vm_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Visualization Inputs",
      width = 4,
      actionButton(ns("generate_dfs"), "Generate all df for visualization"),
      div(style = "margin-bottom: 20px;"),
      selectInput(ns("response_var"), "Response Variable", choices = c("totaldist", "totaldur", "totalct"), selected = "totaldist"),
      selectInput(ns("plot_type"), "Plot Type", choices = c("boxplot_light_dark"), selected = "boxplot_light_dark"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'boxplot_light_dark'", ns("plot_type")),
        selectInput(ns("boxplot_light_dark_mode"), "Boxplot Mode", choices = c("separated", "pooled"), selected = "separated")
      ),
      textInput(ns("condition_grouped_order"), "Condition Order (comma-separated)", value = ""),
      textInput(ns("condition_grouped_color"), "Condition Colors (comma-separated hex)", value = ""),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'pooled'", ns("boxplot_light_dark_mode")),
        textInput(ns("boxplot_light_dark_periods_colors"), "Light/Dark Colors (comma-separated hex)", value = "#FF9999,#9999FF")
      ),
      radioButtons(ns("output_mode"), "Output Mode", choices = c("PNG", "HTML"), selected = "HTML", inline = TRUE),
      actionButton(ns("generate_figure"), "Generate Figure")
    ),
    box(
      title = "Visualization Output",
      width = 8,
      tabsetPanel(
        tabPanel("Interactive Figure", 
                 plotlyOutput(ns("figure_plot"), height = "500px"),
                 div(style = "margin-top: 10px;"),
                 downloadButton(ns("save_current_figure"), "Save Current Figure"),
                 downloadButton(ns("save_all_figures"), "Save All Figures as PNG")
        ),
        tabPanel("Console Output", 
                 div(
                   style = "background-color: #f5f5f5; border: 1px solid #ccc; padding: 10px; height: 200px; overflow-y: auto; font-family: monospace;",
                   uiOutput(ns("console_output"))
                 )
        )
      )
    )
  )
}

# Server du module Visualization
visualization_qm_vm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    console_messages <- reactiveVal(character())
    
    add_console_message <- function(message) {
      current_messages <- console_messages()
      console_messages(c(current_messages, message))
      shiny::invalidateLater(100, session)
    }
    
    # Fonction pour créer les répertoires
    ensure_directory <- function(dir_path) {
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
    }
    
    # Générer les datasets pour les visualisations
    observeEvent(input$generate_dfs, {
      tryCatch({
        req(rv$processing_results)
        all_zone_combined <- dplyr::bind_rows(rv$processing_results$Processed_Data_list)
        rv$all_zone_combined_df <- all_zone_combined
        
        response_vars <- c("totaldist", "totaldur", "totalct")
        
        # Déterminer les périodes light et dark
        light_period <- unique(all_zone_combined$period_without_numbers)[grepl("light", unique(all_zone_combined$period_without_numbers))]
        dark_period <- unique(all_zone_combined$period_without_numbers)[grepl("dark", unique(all_zone_combined$period_without_numbers))]
        
        add_console_message("🔄 Generating light/dark boxplot dataset...")
        
        calculate_means <- function(var) {
          all_zone_combined %>% 
            filter(period_without_numbers %in% c(light_period, dark_period)) %>%
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
        
        rv$all_zone_combined_light_dark_boxplots <- setNames(lapply(response_vars, calculate_means), response_vars)
        add_console_message("✔️ Processed_data_for_light_dark_boxplots created.")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
      })
    })
    
    # Générer la figure
    observeEvent(input$generate_figure, {
      tryCatch({
        req(rv$all_zone_combined_light_dark_boxplots)
        df <- rv$all_zone_combined_light_dark_boxplots[[input$response_var]]
        condition_order <- trimws(unlist(strsplit(input$condition_grouped_order, ",")))
        condition_colors <- trimws(unlist(strsplit(input$condition_grouped_color, ",")))
        
        df$condition_grouped <- factor(df$condition_grouped, levels = condition_order)
        df$period_without_numbers <- factor(df$period_without_numbers, levels = c("light", "dark"), labels = c("Light period", "Dark period"))
        
        add_console_message(sprintf("🔄 Generating %s figure for %s...", input$plot_type, input$response_var))
        
        cap_text <- str_wrap("Each point corresponds to the mean of the response variable for one animal according to the selected period.", width = 60)
        
        for (z in sort(unique(df$zone))) {
          sub <- subset(df, zone == z)
          for (th in c("light", "dark")) {
            theme_obj <- if (th == "light") light_theme() else dark_theme()
            edge_col <- if (th == "light") "black" else "white"
            
            if (input$plot_type == "boxplot_light_dark") {
              if (input$boxplot_light_dark_mode == "separated") {
                p <- ggplot(sub, aes(
                  x = condition_grouped,
                  y = mean_val,
                  fill = condition_grouped,
                  text = paste0(
                    "Tag: ", condition_tagged,
                    "<br>Well: ", animal,
                    "<br>Plate ID: ", plate_id,
                    "<br>Value: ", sprintf("%.2f", mean_val)
                  )
                )) +
                  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = edge_col) +
                  geom_jitter(aes(color = condition_tagged),
                              position = position_jitterdodge(0.2, 0.2),
                              size = 1.75, alpha = 0.4, color = edge_col) +
                  facet_wrap(~period_without_numbers, scales = "free_x") +
                  scale_fill_manual(values = condition_colors) +
                  labs(
                    x = "Condition",
                    y = sprintf("%s (Zone %s)", input$response_var, z),
                    caption = cap_text
                  ) +
                  theme_obj +
                  theme(
                    plot.caption.position = "plot",
                    plot.caption = element_text(hjust = 1),
                    legend.position = "none"
                  )
              } else {
                period_colors <- trimws(unlist(strsplit(input$boxplot_light_dark_periods_colors, ",")))
                p <- ggplot(sub, aes(
                  x = condition_grouped,
                  y = mean_val,
                  fill = period_without_numbers,
                  text = paste0(
                    "Tag: ", condition_tagged,
                    "<br>Well: ", animal,
                    "<br>Plate ID: ", plate_id,
                    "<br>Value: ", sprintf("%.2f", mean_val)
                  )
                )) +
                  geom_boxplot(
                    position = position_dodge(width = 0.8),
                    outlier.shape = NA,
                    alpha = 0.6,
                    width = 0.7,
                    color = edge_col
                  ) +
                  geom_jitter(
                    aes(fill = period_without_numbers),
                    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
                    shape = 21,
                    color = edge_col,
                    size = 1.75,
                    alpha = 0.4
                  ) +
                  scale_fill_manual(values = period_colors) +
                  labs(
                    x = "Condition",
                    y = sprintf("%s (Zone %s)", input$response_var, z),
                    fill = "Period",
                    caption = cap_text
                  ) +
                  theme_obj +
                  theme(
                    plot.caption.position = "plot",
                    plot.caption = element_text(hjust = 1),
                    legend.position = "right"
                  )
              }
              
              if (input$output_mode == "HTML") {
                rv$plot <- ggplotly(p, tooltip = "text") %>% layout(boxmode = "group")
              } else {
                rv$plot <- p
              }
              
              # Stocker la figure pour un téléchargement groupé
              figure_key <- paste(input$response_var, z, th, input$boxplot_light_dark_mode, input$output_mode, sep = "_")
              rv$generated_figures[[figure_key]] <- list(
                plot = p,
                var = input$response_var,
                zone = z,
                theme = th,
                mode = input$boxplot_light_dark_mode,
                output_mode = input$output_mode
              )
              
              add_console_message(sprintf("✔️ %s figure for %s (Zone %s, %s theme) generated.", input$plot_type, input$response_var, z, th))
            }
          }
        }
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
      })
    })
    
    # Afficher la figure
    output$figure_plot <- renderPlotly({
      req(rv$plot)
      if (input$output_mode == "HTML") {
        rv$plot
      } else {
        ggplotly(rv$plot, tooltip = "text") %>% layout(boxmode = "group")
      }
    })
    
    # Sauvegarder la figure actuelle
    output$save_current_figure <- downloadHandler(
      filename = function() {
        if (input$output_mode == "HTML") {
          paste0(input$response_var, "_boxplot_zone_", rv$generated_figures[[length(rv$generated_figures)]]$zone, "_", rv$generated_figures[[length(rv$generated_figures)]]$theme, ".html")
        } else {
          paste0(input$response_var, "_boxplot_zone_", rv$generated_figures[[length(rv$generated_figures)]]$zone, "_", rv$generated_figures[[length(rv$generated_figures)]]$theme, ".png")
        }
      },
      content = function(file) {
        req(rv$plot)
        if (input$output_mode == "HTML") {
          htmlwidgets::saveWidget(rv$plot, file, selfcontained = TRUE)
        } else {
          output_base_dir <- "outputs"
          dir_path <- file.path(output_base_dir, "figures", "boxplots_light_dark", "png", paste0(tolower(rv$generated_figures[[length(rv$generated_figures)]]$theme), "_theme"))
          ensure_directory(dir_path)
          ggsave(file, plot = rv$plot, width = 8, height = 6, dpi = 300)
        }
      }
    )
    
    # Sauvegarder toutes les figures en PNG
    output$save_all_figures <- downloadHandler(
      filename = function() {
        paste0("all_figures_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(rv$generated_figures)
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        output_base_dir <- "outputs"
        for (fig_key in names(rv$generated_figures)) {
          fig <- rv$generated_figures[[fig_key]]
          dir_path <- file.path(output_base_dir, "figures", "boxplots_light_dark", "png", paste0(tolower(fig$theme), "_theme"))
          ensure_directory(dir_path)
          fname <- sprintf("boxplot_%s_zone_%s_%s.png", fig$var, fig$zone, fig$theme)
          file_path <- file.path(temp_dir, fname)
          ggsave(file_path, plot = fig$plot, width = 8, height = 6, dpi = 300)
          files_to_zip <- c(files_to_zip, file_path)
        }
        
        zip::zip(file, files_to_zip, mode = "cherry-pick")
      }
    )
    
    # Afficher la console
    output$console_output <- renderUI({
      messages <- console_messages()
      if (length(messages) == 0) return(HTML("No messages yet."))
      HTML(paste(sapply(messages, htmltools::htmlEscape), collapse = "<br>"))
    })
  })
}