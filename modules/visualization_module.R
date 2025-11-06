# ======================================================================
# modules/visualization_module.R
# Generic visualization module (TM/QM × LDM/VM) driven by config
# ======================================================================
# ---- Local themes -----------------------------------------------------
light_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace% ggplot2::theme(
    plot.title = ggplot2::element_text(color = "black", size = 14, hjust = .5),
    axis.text = ggplot2::element_text(color = "black", size = 12),
    axis.title.x = ggplot2::element_text(color = "black", size = 12, margin = ggplot2::margin(t = 5, r = 15)),
    axis.title.y = ggplot2::element_text(color = "black", size = 12, angle = 90, margin = ggplot2::margin(r = 10)),
    legend.position = "right",
    legend.text = ggplot2::element_text(color = "black", size = 12, face = "italic"),
    legend.title = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 12),
    strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
    panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
    plot.caption = ggplot2::element_text(color = "black", size = 8, hjust = 1, margin = ggplot2::margin(t = 10)),
    panel.border = ggplot2::element_rect(color = "black", fill = NA)
  )
}
dark_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace% ggplot2::theme(
    plot.title = ggplot2::element_text(color = "white", size = 14, hjust = .5),
    axis.text = ggplot2::element_text(color = "white", size = 12),
    axis.title.x = ggplot2::element_text(color = "white", size = 12, margin = ggplot2::margin(t = 5, r = 15)),
    axis.title.y = ggplot2::element_text(color = "white", size = 12, angle = 90, margin = ggplot2::margin(r = 10)),
    legend.position = "right",
    legend.text = ggplot2::element_text(color = "white", size = 12, face = "italic"),
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(fill = "black"),
    legend.key = ggplot2::element_rect(fill = "black"),
    strip.text.x = ggplot2::element_text(color = "white", size = 12),
    strip.background = ggplot2::element_rect(fill = "black", color = "white"),
    plot.background = ggplot2::element_rect(fill = "black"),
    panel.background = ggplot2::element_rect(fill = "black", colour = "white"),
    panel.border = ggplot2::element_rect(color = "white", fill = NA),
    panel.grid.major = ggplot2::element_line(color = "grey30"),
    panel.grid.minor = ggplot2::element_line(color = "grey30"),
    plot.caption = ggplot2::element_text(color = "white", size = 8, hjust = 1, margin = ggplot2::margin(t = 10))
  )
}

# ---- UI ---------------------------------------------------------------
visualization_module_ui <- function(id, config) {
  ns <- shiny::NS(id)
  cfg <- if (is.function(config)) config() else config
  cond <- function(x, y) paste0("input['", ns(x), "'] == '", y, "'")
  
  shiny::fluidRow(
    shinydashboard::box(
      title = paste("Visualization Inputs —", cfg$ui_title), width = 4,
      shiny::selectInput(ns("plot_type"), "Plot Type",
                         c("boxplot_periods","boxplot_cumulate","boxplot_delta","lineplot"),
                         selected = "boxplot_periods"),
      shiny::div(style = "margin-bottom:20px;"),
      shiny::selectInput(ns("response_var"), "Response Variable", choices = "", selected = ""),
      
      # --- Periods boxplots ---
      shiny::conditionalPanel(
        condition = cond("plot_type","boxplot_periods"),
        shiny::actionButton(ns("generate_periods_dfs"),
                            paste0("Generate ", cfg$period_ui_name, " Datasets")),
        shiny::div(style = "margin-bottom:16px;"),
        shiny::radioButtons(ns("boxplot_periods_mode"), "Boxplot Mode",
                            c("separated","pooled"), "separated", inline = TRUE),
        shiny::conditionalPanel(
          condition = paste(cond("boxplot_periods_mode","pooled"),
                            "&&", cond("plot_type","boxplot_periods")),
          shiny::textInput(
            ns("boxplot_periods_colors"),
            paste0(cfg$period_ui_name, " Colors (comma-separated hex)"),
            value = cfg$period_default_colors
          )
        )
      ),
      
      # --- Cumulative (no mode buttons) ---
      shiny::conditionalPanel(
        condition = cond("plot_type","boxplot_cumulate"),
        shiny::actionButton(ns("generate_cumulate_dfs"), "Generate Cumulative Datasets"),
        shiny::div(style = "margin-bottom:16px;")
      ),
      
      # --- Delta (around transitions) ---
      shiny::conditionalPanel(
        condition = cond("plot_type","boxplot_delta"),
        shiny::uiOutput(ns("transition_select_ui")),
        
        # NOUVEAU : Slider pour Delta Time
        shiny::sliderInput(
          ns("delta_time"),
          "Delta Time Window (seconds)",
          min = 5, max = 500, value = 60, step = 5,
          width = "100%"
        ),
        
        shiny::actionButton(ns("generate_delta_dfs"), "Generate Delta Datasets"),
        shiny::div(style = "margin-bottom:16px;"),
        shiny::radioButtons(ns("boxplot_delta_mode"), "Boxplot Mode",
                            c("separated","pooled"), "separated", inline = TRUE),
        shiny::conditionalPanel(
          condition = paste(cond("boxplot_delta_mode","pooled"),
                            "&&", cond("plot_type","boxplot_delta")),
          shiny::textInput(
            ns("boxplot_delta_phase_colors"),
            "Phase Colors (Before, Switch, After; comma-separated hex)",
            value = "#FF6F61, #40C4FF, #4CAF50"
          )
        )
      ),
      
      # --- Lineplot (pooled only) ---
      shiny::conditionalPanel(
        condition = cond("plot_type","lineplot"),
        shiny::radioButtons(ns("time_unit_convert"), "Convert Time Unit?",
                            c("Yes","No"), "No", inline = TRUE),
        shiny::conditionalPanel(
          condition = cond("time_unit_convert","Yes"),
          shiny::selectInput(ns("time_unit_original"), "Original Time Unit",
                             c("seconds","minutes","hours","days"), "seconds"),
          shiny::selectInput(ns("time_unit_target"), "Target Time Unit",
                             c("seconds","minutes","hours","days"), "minutes")
        ),
        shiny::conditionalPanel(
          condition = cond("time_unit_convert","No"),
          shiny::selectInput(ns("time_unit_original"), "Time Unit",
                             c("seconds","minutes","hours","days"), "seconds")
        ),
        shiny::uiOutput(ns("aggregation_period_label")),
        shiny::radioButtons(ns("lineplot_error_mode"), "Error Representation",
                            c("Error Bars" = "error_bar", "95% CI" = "ci95"),
                            "error_bar", inline = TRUE),
        shiny::actionButton(ns("generate_lineplot_dfs"), "Generate Lineplot Datasets"),
        shiny::div(style = "margin-bottom:16px;")
      ),
      
      # --- Global ordering & colors ---
      shiny::textInput(ns("condition_grouped_order"), "Condition Order (comma-separated)",
                       value = "", placeholder = "e.g., cond1, cond2, cond3"),
      shiny::textInput(ns("condition_grouped_color"), "Condition Colors (comma-separated hex)",
                       value = "", placeholder = "e.g., #FF0000, #00FF00, #0000FF"),
      
      # --- Output mode & figure builder ---
      shiny::radioButtons(ns("output_mode"), "Output Mode", c("PNG","HTML"), "HTML", inline = TRUE),
      shiny::uiOutput(ns("figure_selector")),
      shiny::div(
        style = "display:flex; flex-direction:column; gap:10px;",
        shiny::actionButton(ns("generate_figure"), "Generate Figure", style = "width:100%;"),
        shiny::conditionalPanel(
          condition = cond("plot_type","boxplot_delta"),
          shiny::actionButton(ns("generate_delta_tables"), "Generate Delta Percentage Tables", style = "width:100%;")
        )
      )
    ),
    
    shinydashboard::box(
      title = "Visualization Output", width = 8,
      shiny::div(style = "margin-bottom:10px;",
                 shiny::actionButton(ns("clear_console"), "Clear Console", icon = shiny::icon("trash"))),
      shiny::tabsetPanel(
        id = ns("output_tabs"),
        shiny::tabPanel("Interactive Figure",
                        shiny::uiOutput(ns("figure_plot")),
                        shiny::div(style="margin-top:10px;"),
                        shiny::radioButtons(ns("theme_switch"), "Theme", c("Light","Dark"), "Light", inline = TRUE),
                        shiny::div(style="margin-top:10px;"),
                        shiny::downloadButton(ns("save_current_figure"), "Save Current Figure"),
                        shiny::downloadButton(ns("download_plot_script"), "Download R Script (.R)"),
                        
                        # NOUVEAU : Delta Time Explorer
                        shiny::conditionalPanel(
                          condition = cond("plot_type", "boxplot_delta"),
                          shiny::div(
                            style = "margin-top: 25px; border-top: 1px solid #ccc; padding-top: 15px;",
                            shiny::h5("Delta Time Explorer (Live Preview)",
                                      class = "delta-time-title",
                                      style = "text-align: center; margin-bottom: 10px;"),
                            plotly::plotlyOutput(ns("delta_time_explorer"), height = "300px"),
                            shiny::helpText(
                              "The delta time window defines three phases: Before [t-Δ,t), Switch [t,t+Δ), After [t+Δ,t+2Δ)",
                              class = "delta-time-help",
                              style = "text-align: center; font-size: 11px; margin-top: 8px;"
                            )
                          )
                        )
        ),
        shiny::tabPanel("Datasets",
                        shiny::selectInput(ns("dataset_type"), "Dataset Type",
                                           c("Boxplot Periods","Boxplot Cumulative","Boxplot Delta","Lineplot"),
                                           selected = "Boxplot Periods"),
                        shiny::selectInput(ns("dataset_response_var"), "Response Variable", choices = "", selected = ""),
                        DT::dataTableOutput(ns("dataset_table")),
                        shiny::div(style="margin-top:10px; margin-bottom:10px;",
                                   shiny::downloadButton(ns("download_current_dataset"), "Download Current Dataset (.xlsx)"))),
        shiny::tabPanel("Console Output",
                        shiny::div(
                          style = "height: 600px; overflow-y: auto; padding: 10px; border-radius: 6px;",
                          class = "console-container",
                          verbatimTextOutput(ns("console_output"), placeholder = TRUE)
                        )
        ),
        shiny::tabPanel("Delta Percentage Tables", value = "delta_percentage_tables",
                        shiny::selectInput(ns("delta_table_type"), "Table Type",
                                           c("Momentum Comparisons","Condition Comparisons"), "Momentum Comparisons"),
                        shiny::selectInput(ns("delta_table_var"), "Response Variable", choices = "", selected = ""),
                        DT::dataTableOutput(ns("delta_percentage_table")),
                        shiny::div(style="margin-top:10px; margin-bottom:10px;",
                                   shiny::downloadButton(ns("download_current_delta_table"), "Download Current Table (.xlsx)"),
                                   shiny::downloadButton(ns("download_all_delta_tables"), "Download All Delta Tables (.zip)")))
      )
    )
  )
}

# ---- Server -----------------------------------------------------------
visualization_module_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    cfg <- shiny::reactive({
      if (is.function(config)) tryCatch(config(), error = function(e) NULL) else config
    })
    EXPECTED_VARS <- shiny::reactive({
      cfg()$expected_vars
    })
    
    console_messages <- shiny::reactiveVal("👋 Ready.")
    log <- function(...) console_messages(c(console_messages(), paste(...)))
    
    rv$hidden_traces <- integer(0)
    
    convert_time <- function(x, from, to) {
      if (from == to) return(x)
      f <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)
      x * f[[from]] / f[[to]]
    }
    ensure_colors <- function(n, cols = character(0)) {
      cols <- cols[!is.na(cols) & nzchar(trimws(cols))]
      if (length(cols) == 0) {
        base <- tryCatch(RColorBrewer::brewer.pal(min(8, max(3, n)), "Set1"),
                         error = function(e) grDevices::rainbow(min(8, max(3, n))))
        return(grDevices::colorRampPalette(base)(n))
      }
      cols <- trimws(cols)
      if (length(cols) < n) cols <- rep(cols, length.out = n)
      if (length(cols) > n) cols <- cols[seq_len(n)]
      cols
    }
    
    # ---- ggplotly conversion (register events; match legend behaviour) ----
    to_widget <- function(p, pooled = FALSE, df = NULL) {
      safe_rows <- tryCatch({ is.data.frame(df) && nrow(df) > 0 }, error = function(e) FALSE)
      if (!isTRUE(safe_rows)) {
        empty <- ggplot2::ggplot() + ggplot2::geom_blank() + light_theme()
        w <- plotly::ggplotly(empty, source = ns("html"))
        plotly::event_register(w, "plotly_restyle")
        plotly::event_register(w, "plotly_legendclick")
        plotly::event_register(w, "plotly_legenddoubleclick")
        w <- plotly::layout(w, legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers"))
        return(w)
      }
      out <- tryCatch({
        w <- plotly::ggplotly(p, tooltip = "text", source = ns("html"))
        if (isTRUE(pooled)) w <- plotly::layout(w, boxmode = "group")
        plotly::event_register(w, "plotly_restyle")
        plotly::event_register(w, "plotly_legendclick")
        plotly::event_register(w, "plotly_legenddoubleclick")
        w <- plotly::layout(w, legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers"))
        w
      }, error = function(e) {
        msg <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = paste0("ggplotly failed:\n", e$message)) +
          ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + light_theme()
        w <- plotly::ggplotly(msg, source = ns("html"))
        plotly::event_register(w, "plotly_restyle")
        plotly::event_register(w, "plotly_legendclick")
        plotly::event_register(w, "plotly_legenddoubleclick")
        w <- plotly::layout(w, legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers"))
        w
      })
      out
    }
    
    get_boundaries_list <- function() {
      if (is.null(rv$processing_results)) return(NULL)
      k <- names(rv$processing_results)
      k <- k[tolower(k) == "boundary_associations_list"]
      if (length(k)) rv$processing_results[[k[1]]] else NULL
    }
    
    update_response_choices <- function(vars, read_current = TRUE) {
      if (is.null(vars) || !length(vars)) return()
      rv_sel <- ""; ds_sel <- ""; dt_sel <- vars[1]
      if (isTRUE(read_current)) {
        if (!is.null(input$response_var) && isolate(input$response_var) %in% vars) rv_sel <- isolate(input$response_var)
        if (!is.null(input$dataset_response_var) && isolate(input$dataset_response_var) %in% vars) ds_sel <- isolate(input$dataset_response_var)
        if (!is.null(input$delta_table_var) && isolate(input$delta_table_var) %in% vars) dt_sel <- isolate(input$delta_table_var)
      }
      shiny::updateSelectInput(session, "response_var", choices = c("", vars), selected = rv_sel)
      shiny::updateSelectInput(session, "dataset_response_var", choices = c("", vars), selected = ds_sel)
      shiny::updateSelectInput(session, "delta_table_var", choices = c("", vars), selected = dt_sel)
    }
    
    shiny::observeEvent(cfg(), {
      vars <- EXPECTED_VARS(); if (is.null(vars)) return()
      update_response_choices(vars, read_current = FALSE)
    }, ignoreInit = FALSE)
    
    shiny::observeEvent(
      list(input$generate_periods_dfs,
           input$generate_cumulate_dfs,
           input$generate_delta_dfs,
           input$generate_lineplot_dfs),
      {
        vars <- EXPECTED_VARS(); if (is.null(vars)) return()
        update_response_choices(vars, read_current = TRUE)
      },
      ignoreInit = TRUE
    )
    
    # ==================================================================
    # DATASET BUILDERS
    # ==================================================================
    prepare_all_zone <- function() {
      shiny::req(rv$processing_results,
                 "processed_data_list" %in% names(rv$processing_results))
      
      mode <- cfg()$mode %||% "unknown"          # "tm_ldm", "qm_vm", …
      is_qm <- grepl("^qm", mode)                # TRUE pour qm_ldm / qm_vm
      
      rv$processed_data_list <- purrr::map(
        rv$processing_results$processed_data_list,
        ~ {
          # toujours présent
          .x <- dplyr::mutate(.x, plate_id = as.character(plate_id))
          
          ## -------------------------------------------------
          ## 1. Colonnes communes (toujours créées)
          ## -------------------------------------------------
          if (all(c("smldist","lardist","inadist") %in% names(.x))) {
            .x <- .x %>% dplyr::mutate(
              totaldist = smldist + lardist + inadist,
              totaldur  = smldur  + lardur  + inadur,
              totalct   = smlct   + larct   + inact,
              totalspeed = totaldist / pmax(totaldur, 1),
              smlspeed   = smldist   / pmax(smldur, 1),
              larspeed   = lardist   / pmax(lardur, 1),
              inaspeed   = inadist   / pmax(inadur, 1)
            )
          }
          
          ## -------------------------------------------------
          ## 2. Colonnes spécifiques QM (frect, fredur, …)
          ## -------------------------------------------------
          if (is_qm && all(c("frect","midct","burct","zerct") %in% names(.x))) {
            .x <- .x %>% dplyr::mutate(
              totaldist = frect + midct + burct + zerct,          # ou la vraie somme que tu veux
              totaldur  = fredur + middur + burdur + zerdur,
              totalct   = 0,   # pas de "count" en QM, ou garde la vraie variable
              totalspeed = totaldist / pmax(totaldur, 1)
              # les "smlspeed" etc. ne sont pas pertinentes en QM → on ne les crée pas
            )
          }
          
          .x
        }
      )
      
      rv$all_zone_combined <- dplyr::bind_rows(rv$processed_data_list)
      rv$all_zone_combined
    }
    
    observe({
      if (!is.null(rv$all_zone_combined)) {
        cat("=== prepare_all_zone OK ===\n")
        cat("mode :", cfg()$mode, "\n")
        cat("colonnes :", paste(names(rv$all_zone_combined), collapse=", "), "\n")
        cat("lignes  :", nrow(rv$all_zone_combined), "\n")
      }
    })
    
    build_periods_df <- function(az, v, cfg) {
      periods <- unique(az$period_without_numbers)
      keys <- cfg$period_keys
      labels <- cfg$period_labels %||% keys
      
      if (is.null(keys) || length(keys) == 0) {
        stop("Configuration error: 'period_keys' is empty.")
      }
      
      # Recherche flexible : on garde seulement les clés présentes dans les données
      found_keys <- character(0)
      found_labels <- character(0)
      
      for (i in seq_along(keys)) {
        p_match <- grep(keys[i], periods, ignore.case = TRUE, value = TRUE)
        if (length(p_match) > 0) {
          found_keys <- c(found_keys, p_match[1])  # on prend le premier match
          found_labels <- c(found_labels, labels[i])
        }
      }
      
      # Si aucune période trouvée → erreur claire
      if (length(found_keys) == 0) {
        stop(sprintf("No periods matching any of %s found in data. Available: %s",
                     paste("'", keys, "'", collapse=", "),
                     paste("'", periods, "'", collapse=", ")))
      }
      
      # Si une seule période → on continue, pas de problème
      message_log <- if (length(found_keys) == 1) {
        sprintf("Only one period found: '%s'. Proceeding with single-period boxplot.", found_labels[1])
      } else {
        sprintf("Found periods: %s", paste(found_labels, collapse = " and "))
      }
      # (optionnel) log(message_log)  # si tu veux voir dans la console
      
      out <- az %>%
        dplyr::filter(period_without_numbers %in% found_keys) %>%
        dplyr::group_by(period_without_numbers, zone, condition_tagged, plate_id) %>%
        dplyr::summarise(
          plate_id = dplyr::first(as.character(plate_id)),
          start = dplyr::first(start),
          period_with_numbers = dplyr::first(period_with_numbers),
          condition_grouped = dplyr::first(condition_grouped),
          condition = dplyr::first(condition),
          animal = dplyr::first(animal),
          mean_val = mean(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          period_without_numbers = factor(
            period_without_numbers,
            levels = found_keys,
            labels = found_labels
          )
        ) %>%
        dplyr::select(dplyr::any_of(c(
          "zone", "condition_grouped", "condition_tagged", "condition",
          "start", "plate_id", "animal",
          "period_without_numbers", "period_with_numbers", "mean_val"
        )))
      
      out
    }
    
    build_cumulate_df <- function(az, v) {
      az %>%
        dplyr::group_by(condition_grouped, zone, plate_id, animal) %>%
        dplyr::summarise(
          cum = sum(.data[[v]], na.rm = TRUE),
          condition_tagged = dplyr::first(condition_tagged),
          .groups = "drop"
        ) %>%
        dplyr::mutate(plate_id = as.character(plate_id)) %>%
        dplyr::select(dplyr::any_of(c(
          "zone", "condition_grouped", "condition_tagged",
          "plate_id", "animal", "cum"
        )))
    }
    
    build_delta_split <- function(az, vars, transition, delta_sec, round_to = NULL) {
      shiny::req("boundary_associations_list" %in% names(rv$processing_results))
      b <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
      if (!nrow(b)) return(NULL)
      
      b_clean <- b %>%
        dplyr::mutate(plate_id = as.character(plate_id)) %>%
        dplyr::arrange(transition, plate_id, time_switch) %>%
        dplyr::group_by(transition, plate_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::filter(transition == !!transition) %>%
        dplyr::select(plate_id, time_switch)
      
      joined <- az %>%
        dplyr::mutate(
          plate_id = as.character(plate_id),
          start_for_cut = if (!is.null(round_to)) floor(start / round_to) * round_to else start
        ) %>%
        dplyr::inner_join(b_clean, by = "plate_id", relationship = "many-to-many") %>%
        dplyr::mutate(
          phase_raw = dplyr::case_when(
            start_for_cut >= time_switch - delta_sec & start_for_cut < time_switch ~ "before",
            start_for_cut >= time_switch & start_for_cut < time_switch + delta_sec ~ "switch",
            start_for_cut >= time_switch + delta_sec & start_for_cut < time_switch + 2*delta_sec ~ "after",
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(phase_raw)) %>%
        dplyr::mutate(
          transition_phase = paste0(transition, "_", phase_raw),
          period_without_numbers = dplyr::recode(phase_raw,
                                                 before = "Before", switch = "Switch", after = "After"),
          period_with_numbers = paste0(transition, "_", period_without_numbers)
        )
      
      if (!nrow(joined)) return(NULL)
      
      phased_long <- tidyr::pivot_longer(
        joined, cols = tidyselect::all_of(vars),
        names_to = "variable", values_to = "value"
      ) %>%
        dplyr::group_by(
          transition_phase, period_without_numbers, period_with_numbers,
          zone, condition_tagged, plate_id, animal, variable
        ) %>%
        dplyr::summarise(
          mean_val = mean(value, na.rm = TRUE),
          condition_grouped = dplyr::first(condition_grouped),
          start = dplyr::first(start),
          .groups = "drop"
        ) %>%
        dplyr::select(dplyr::any_of(c(
          "zone","condition_grouped","condition_tagged",
          "plate_id","animal","mean_val",
          "period_without_numbers","period_with_numbers",
          "transition_phase","variable","start"
        )))
      
      split(phased_long, phased_long$variable)
    }
    
    compute_wells <- function(df, gvar) {
      df %>%
        dplyr::group_by(.data[[gvar]], zone, plate_id) %>%
        dplyr::summarise(n_wells_plate = dplyr::n_distinct(animal), .groups = "drop") %>%
        dplyr::group_by(.data[[gvar]], zone) %>%
        dplyr::summarise(n_wells = sum(n_wells_plate), .groups = "drop")
    }
    
    build_lineplot_df <- function(az, v, agg_period, unit_from, unit_to, convert) {
      agg_unit <- if (identical(convert, "Yes")) unit_to else unit_from
      agg_s <- convert_time(as.numeric(agg_period), agg_unit, "seconds")
      gvar <- "condition_grouped"
      
      per_well <- az %>%
        dplyr::mutate(
          plate_id = as.character(plate_id),
          start_rounded = floor(start / agg_s) * agg_s
        ) %>%
        dplyr::group_by(.data[[gvar]], zone, start_rounded, plate_id, animal) %>%
        dplyr::summarise(
          var_value_per_well = sum(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        )
      
      summary_df <- per_well %>%
        dplyr::group_by(.data[[gvar]], zone, start_rounded) %>%
        dplyr::summarise(
          total_val = sum(var_value_per_well, na.rm = TRUE),
          mean_per_well = mean(var_value_per_well, na.rm = TRUE),
          sd_per_well = stats::sd(var_value_per_well, na.rm = TRUE),
          n_wells = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(se_per_well = sd_per_well / sqrt(pmax(n_wells, 1)))
      
      out <- per_well %>%
        dplyr::left_join(summary_df, by = c(gvar, "zone", "start_rounded")) %>%
        dplyr::mutate(
          start_rounded = if (!identical(unit_from, agg_unit))
            convert_time(start_rounded, "seconds", agg_unit) else start_rounded
        ) %>%
        dplyr::rename(val_per_well = mean_per_well) %>%
        dplyr::select(dplyr::any_of(c(
          "zone", gvar, "start_rounded",
          "plate_id", "animal",
          "total_val", "n_wells", "val_per_well", "se_per_well", "sd_per_well"
        )))
      
      out
    }
    
    # ==================================================================
    # UI bits
    # ==================================================================
    shiny::observeEvent(input$clear_console, {
      console_messages("👋 Ready."); notify("Console cleared.", type = "message")
    })
    
    output$aggregation_period_label <- shiny::renderUI({
      unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
      shiny::textInput(ns("aggregation_period"), sprintf("Aggregation Period (in %s)", unit), value = "60")
    })
    
    output$transition_select_ui <- shiny::renderUI({
      shiny::req(rv$processing_results, get_boundaries_list())
      b <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
      tr <- unique(b$transition)
      if (!length(tr)) return(shiny::div("No transitions available. Please run processing first."))
      shiny::selectInput(ns("transition_select"), "Select Transition", choices = tr, selected = tr[1])
    })
    
    output$figure_selector <- shiny::renderUI({
      shiny::req(input$plot_type, input$response_var)
      df <- switch(
        input$plot_type,
        "boxplot_periods" = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
        "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
        "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]],
        "lineplot" = rv$all_zone_combined_lineplots[[input$response_var]]
      )
      if (is.null(df)) return(shiny::helpText("Generate datasets first (click the button above)."))
      zones <- sort(unique(df$zone))
      choices <- stats::setNames(as.character(zones), paste("Zone", zones))
      shiny::selectInput(ns("selected_zone"), "Select Zone", choices = choices, selected = choices[1])
    })
    
    # ==================================================================
    # Delta Time Explorer (Live Preview)
    # ==================================================================
    output$delta_time_explorer <- plotly::renderPlotly({
      # --- guards ---
      if (is.null(input$transition_select) || input$transition_select == "") return(plotly::plotly_empty())
      if (is.null(rv$all_zone_combined) || nrow(rv$all_zone_combined) == 0)   return(plotly::plotly_empty())
      if (is.null(get_boundaries_list()))                                      return(plotly::plotly_empty())
      if (is.null(input$response_var) || input$response_var == "")             return(plotly::plotly_empty())
      
      # --- transition center (one per plate, first occurrence) ---
      b <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
      ts_info <- b %>%
        dplyr::filter(transition == input$transition_select) %>%
        dplyr::group_by(plate_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
      if (!nrow(ts_info)) return(plotly::plotly_empty())
      t_center <- mean(ts_info$time_switch, na.rm = TRUE)
      
      # --- delta ---
      delta_sec <- suppressWarnings(as.numeric(input$delta_time))
      if (is.na(delta_sec) || delta_sec <= 0) delta_sec <- 60
      
      # windows
      t1 <- t_center - delta_sec      # Before start
      t2 <- t_center                  # Switch start
      t3 <- t_center + delta_sec      # After start
      t4 <- t_center + 2 * delta_sec  # After end
      
      # --- data (mean over time × condition) ---
      az <- rv$all_zone_combined
      v  <- input$response_var
      if (!v %in% names(az)) v <- names(az)[sapply(az, is.numeric)][1]
      
      line_df <- az %>%
        dplyr::mutate(time_sec = floor(start)) %>%
        dplyr::group_by(time_sec, condition_grouped) %>%
        dplyr::summarise(mean_val = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
      if (!nrow(line_df)) return(plotly::plotly_empty())
      
      # y-range (finite bounds for bands)
      yr   <- range(line_df$mean_val, na.rm = TRUE)
      pad  <- diff(yr) * 0.04
      ymin <- yr[1] - pad
      ymax <- yr[2] + pad
      
      # --- base ggplot (just the lines; keep it simple) ---
      p <- ggplot2::ggplot(line_df, ggplot2::aes(x = time_sec, y = mean_val, colour = condition_grouped, group = condition_grouped)) +
        ggplot2::geom_line(linewidth = 0.7, alpha = 0.9) +
        ggplot2::geom_point(size = 1.8, alpha = 0.75) +
        ggplot2::labs(x = "Time (s)", y = paste("Mean", v), colour = "Condition") +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(legend.position = "right")
      
      # --- convert to plotly ---
      wp <- plotly::ggplotly(p, tooltip = "none", height = 270) %>%
        plotly::layout(
          xaxis = list(title = "Time (s)", showgrid = FALSE),
          yaxis = list(title = paste("Mean", v), showgrid = FALSE),
          margin = list(l = 50, r = 20, t = 10, b = 50),
          hovermode = "x unified"
        )
      
      # --- add bands as shapes (layer BELOW traces) ---
      band_shapes <- list(
        list(type = "rect", xref = "x", yref = "y", x0 = t1, x1 = t2, y0 = ymin, y1 = ymax,
             fillcolor = "rgba(255,152,0,0.18)", line = list(width = 0), layer = "below"),
        list(type = "rect", xref = "x", yref = "y", x0 = t2, x1 = t3, y0 = ymin, y1 = ymax,
             fillcolor = "rgba(244,67,54,0.16)", line = list(width = 0), layer = "below"),
        list(type = "rect", xref = "x", yref = "y", x0 = t3, x1 = t4, y0 = ymin, y1 = ymax,
             fillcolor = "rgba(76,175,80,0.18)", line = list(width = 0), layer = "below")
      )
      wp <- plotly::layout(wp, shapes = band_shapes)
      
      # --- add vertical dashed lines as extra traces (guaranteed visible) ---
      add_vseg <- function(fig, x, y0, y1, dash = "dot") {
        plotly::add_segments(fig,
                             x = x, xend = x, y = y0, yend = y1,
                             inherit = FALSE,
                             line = list(width = 1.1, dash = dash),
                             showlegend = FALSE,
                             hoverinfo = "skip"
        )
      }
      wp <- add_vseg(wp, t1, ymin, ymax, "dot")
      wp <- add_vseg(wp, t2, ymin, ymax, "dot")
      wp <- add_vseg(wp, t3, ymin, ymax, "dot")
      wp <- add_vseg(wp, t4, ymin, ymax, "dot")
      
      # --- dynamic letters B / S / A centered between the lines ---
      annotations <- list(
        list(x = (t1 + t2)/2, y = 0.96, xref = "x", yref = "paper", text = "B",
             showarrow = FALSE, font = list(size = 14)),
        list(x = (t2 + t3)/2, y = 0.96, xref = "x", yref = "paper", text = "S",
             showarrow = FALSE, font = list(size = 14)),
        list(x = (t3 + t4)/2, y = 0.96, xref = "x", yref = "paper", text = "A",
             showarrow = FALSE, font = list(size = 14))
      )
      wp <- plotly::layout(wp, annotations = annotations)
      
      # --- done ---
      wp
    })
    
    
    
    # Update Delta Time Explorer reactively
    shiny::observeEvent(list(input$delta_time, input$transition_select, input$response_var), {
      if (input$plot_type == "boxplot_delta") {
        shiny::req(rv$all_zone_combined, get_boundaries_list())
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # ==================================================================
    # Dataset generators
    # ==================================================================
    shiny::observeEvent(input$generate_periods_dfs, {
      tryCatch({
        az <- prepare_all_zone()
        current_cfg <- cfg()
        rv$all_zone_combined_light_dark_boxplots <- stats::setNames(
          lapply(EXPECTED_VARS(), function(v) build_periods_df(az, v, current_cfg)),
          EXPECTED_VARS()
        )
        log("✅ Periods datasets created.")
      }, error = function(e) log(paste("❌ Periods dataset generation failed:", e$message)))
    })
    
    shiny::observeEvent(input$generate_cumulate_dfs, {
      tryCatch({
        az <- prepare_all_zone()
        rv$all_zone_combined_cum_boxplots <- stats::setNames(
          lapply(EXPECTED_VARS(), function(v) build_cumulate_df(az, v)),
          EXPECTED_VARS()
        )
        log("✅ Cumulative datasets created.")
      }, error = function(e) log(paste("❌ Cumulative generation failed:", e$message)))
    })
    
    validate_num_pos <- function(x, msg) {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v) || v <= 0) { shiny::showModal(shiny::modalDialog(title = "Warning", msg, easyClose = TRUE)); return(FALSE) }
      TRUE
    }

    shiny::observeEvent(input$generate_delta_dfs, {
      tryCatch({
        shiny::req(validate_num_pos(input$delta_time, "Delta time window must be a positive number."))
        az <- prepare_all_zone()
        b <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
        delta_sec <- as.numeric(input$delta_time)
        split_list <- build_delta_split(az, EXPECTED_VARS(), input$transition_select, delta_sec)
        rv$all_zone_combined_delta_boxplots <- split_list
        log(sprintf("✅ Delta datasets created for transition '%s' (±%ss).", input$transition_select, delta_sec))
      }, error = function(e) log(paste("❌ Delta generation failed:", e$message)))
    })
    
    shiny::observeEvent(input$generate_lineplot_dfs, {
      tryCatch({
        shiny::req(validate_num_pos(input$aggregation_period, "Aggregation period must be a positive number."))
        az <- prepare_all_zone()
        rv$all_zone_combined_lineplots <- stats::setNames(
          lapply(EXPECTED_VARS(), function(v) {
            build_lineplot_df(
              az = az,
              v = v,
              agg_period= input$aggregation_period,
              unit_from = input$time_unit_original,
              unit_to = input$time_unit_target,
              convert = input$time_unit_convert
            )
          }),
          EXPECTED_VARS()
        )
        log("✅ Lineplot datasets created (normalized per well, pooled).")
      }, error = function(e) log(paste("❌ Lineplot generation failed:", e$message)))
    })
    
    # ==================================================================
    # Plot factory (reste inchangé)
    # ==================================================================

    order_and_colors <- function(df) {
      gvar <- "condition_grouped"
      present <- unique(df[[gvar]])
      # ordre demandé par l'utilisateur (séparé par des virgules)
      ord_in <- if (nzchar(input$condition_grouped_order))
        trimws(strsplit(input$condition_grouped_order, ",")[[1]]) else present
      # garde uniquement les niveaux présents + ajoute les restants à la fin
      ord <- unique(c(intersect(ord_in, present), setdiff(present, ord_in)))
      
      raw_cols <- if (nzchar(input$condition_grouped_color))
        trimws(strsplit(input$condition_grouped_color, ",")[[1]]) else character(0)
      
      cols <- ensure_colors(length(ord), raw_cols)
      names(cols) <- ord  # <- important : couleurs nommées par niveau
      
      list(order = ord, colors = cols)
    }
    
    
    generate_plot <- function(df, response_var, plot_type, boxplot_mode,
                              selected_zone, theme_choice, condition_order, condition_colors) {
      
      # --- sous-ensemble zone + thème ---
      sub <- subset(df, zone == selected_zone)
      sub <- droplevels(sub)
      
      theme_is_light <- tolower(theme_choice) == "light"
      theme_obj <- if (theme_is_light) light_theme() else dark_theme()
      edge_col  <- if (theme_is_light) "black" else "white"
      
      # --- ordre & couleurs nommées (condition_grouped) ---
      gvar <- "condition_grouped"
      present_levels <- unique(sub[[gvar]])
      ord <- intersect(condition_order, present_levels)
      if (!length(ord)) ord <- present_levels
      # complète si ordre partiel
      ord <- unique(c(ord, setdiff(present_levels, ord)))
      sub[[gvar]] <- factor(sub[[gvar]], levels = ord)
      
      # couleurs : nommées selon les niveaux (sécurise le mapping)
      cols_named <- condition_colors
      if (is.null(names(cols_named)) ||
          length(cols_named) != length(ord) ||
          any(!(ord %in% names(cols_named)))) {
        cols_named <- ensure_colors(length(ord), cols_named)
        names(cols_named) <- ord
      }
      
      # =====================================================================
      # BOX PLOT — PERIODS
      # =====================================================================
      if (plot_type == "boxplot_periods") {
        
        if (identical(boxplot_mode, "separated")) {
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
            ggplot2::geom_boxplot(
              ggplot2::aes(group = .data[[gvar]], fill = .data[[gvar]]),
              colour = edge_col
            ) +
            ggforce::geom_sina(
              ggplot2::aes(
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", .data[[gvar]],
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.6, alpha = 0.55, colour = edge_col
            ) +
            ggplot2::facet_wrap(~period_without_numbers, scales = "free_x") +
            ggplot2::scale_fill_manual(values = cols_named, limits = ord) +
            ggplot2::labs(
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              caption = "Each point corresponds to the mean value for one animal."
            ) +
            theme_obj +
            ggplot2::theme(
              legend.position = "none",
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              axis.title.x = ggplot2::element_blank(),
              plot.caption.position = "plot",
              plot.caption = ggplot2::element_text(hjust = 1)
            )
          return(gg)
          
        } else {
          # pooled : couleurs de périodes
          per_cols_in <- trimws(strsplit(input$boxplot_periods_colors, ",")[[1]])
          per_levels  <- unique(sub$period_without_numbers)
          per_cols    <- ensure_colors(length(per_levels), per_cols_in)
          names(per_cols) <- per_levels
          dodge_w <- 0.75
          
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
            ggplot2::geom_boxplot(
              ggplot2::aes(
                group = interaction(.data[[gvar]], period_without_numbers),
                fill = period_without_numbers
              ),
              colour = edge_col,
              dodge.width = dodge_w
            ) +
            ggforce::geom_sina(
              ggplot2::aes(
                x = .data[[gvar]],
                y = mean_val,
                colour = period_without_numbers,
                group = interaction(.data[[gvar]], period_without_numbers),
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", .data[[gvar]],
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.8, alpha = 0.55,
              position = ggplot2::position_dodge(width = dodge_w)
            ) +
            ggplot2::scale_fill_manual(values = per_cols, breaks = per_levels, limits = per_levels, name = "Period") +
            ggplot2::scale_colour_manual(values = rep(edge_col, length(per_levels)), guide = "none") +
            ggplot2::labs(
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              caption = "Each point corresponds to the mean value for one animal."
            ) +
            theme_obj +
            ggplot2::theme(
              legend.position = "right",
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              axis.title.x = ggplot2::element_blank(),
              plot.caption.position = "plot",
              plot.caption = ggplot2::element_text(hjust = 1)
            )
          return(gg)
        }
      }
      
      # =====================================================================
      # BOX PLOT — CUMULATE
      # =====================================================================
      if (plot_type == "boxplot_cumulate") {
        
        n_animals <- sub %>%
          dplyr::group_by(.data[[gvar]], zone, plate_id) %>%
          dplyr::summarise(n_animals_plate = dplyr::n_distinct(animal), .groups = "drop") %>%
          dplyr::group_by(.data[[gvar]], zone) %>%
          dplyr::summarise(n = sum(n_animals_plate), .groups = "drop") %>%
          dplyr::mutate(y = -Inf)
        
        gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = cum)) +
          ggplot2::geom_boxplot(
            ggplot2::aes(group = .data[[gvar]], fill = .data[[gvar]]),
            colour = edge_col
          ) +
          ggforce::geom_sina(
            ggplot2::aes(
              text = paste0(
                "Animal: ", animal,
                "<br>Plate: ", plate_id,
                "<br>Condition: ", .data[[gvar]],
                "<br>Value: ", sprintf("%.2f", cum)
              )
            ),
            maxwidth = 0.25, size = 1.4, alpha = 0.6, colour = edge_col
          ) +
          ggplot2::geom_text(
            data = n_animals,
            ggplot2::aes(x = .data[[gvar]], y = y, label = paste0("n=", n)),
            inherit.aes = FALSE, vjust = -0.5, size = 3, colour = edge_col
          ) +
          ggplot2::scale_fill_manual(values = cols_named, limits = ord) +
          ggplot2::labs(
            y = sprintf("Cumulative %s (Zone %s)", response_var, selected_zone),
            caption = "Each point corresponds to the cumulative value for one animal."
          ) +
          theme_obj +
          ggplot2::theme(
            legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            axis.title.x = ggplot2::element_blank(),
            plot.caption.position = "plot",
            plot.caption = ggplot2::element_text(hjust = 1)
          )
        return(gg)
      }
      
      # =====================================================================
      # BOX PLOT — DELTA (Before / Switch / After)
      # =====================================================================
      if (plot_type == "boxplot_delta") {
        
        tr <- input$transition_select
        sub <- dplyr::filter(sub, grepl(paste0("^", tr, "_"), transition_phase))
        sub$phase <- gsub(paste0("^", tr, "_"), "", sub$transition_phase)
        sub$phase <- dplyr::recode(sub$phase,
                                   before = "Before",
                                   switch = "Switch",
                                   after  = "After",
                                   .default = NA_character_)
        present_phase <- intersect(c("Before","Switch","After"), unique(sub$phase))
        sub$phase <- factor(sub$phase, levels = present_phase, ordered = TRUE)
        
        if (!length(present_phase) || nrow(sub) == 0) {
          return(ggplot2::ggplot() + ggplot2::geom_blank() + theme_obj)
        }
        
        if (identical(boxplot_mode, "separated")) {
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
            ggplot2::geom_boxplot(
              ggplot2::aes(group = .data[[gvar]], fill = .data[[gvar]]),
              colour = edge_col
            ) +
            ggforce::geom_sina(
              ggplot2::aes(
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", .data[[gvar]],
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.8, alpha = 0.55, colour = edge_col
            ) +
            ggplot2::facet_wrap(~phase, scales = "free_x") +
            ggplot2::scale_fill_manual(values = cols_named, limits = ord) +
            ggplot2::labs(
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              caption = "Each point is the mean per animal in the Before / Switch / After windows."
            ) +
            theme_obj +
            ggplot2::theme(
              legend.position = "none",
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              axis.title.x = ggplot2::element_blank(),
              plot.caption.position = "plot",
              plot.caption = ggplot2::element_text(hjust = 1)
            )
          return(gg)
          
        } else {
          # pooled : couleurs de phase
          phase_cols_in <- trimws(strsplit(input$boxplot_delta_phase_colors, ",")[[1]])
          phase_cols    <- ensure_colors(length(present_phase), phase_cols_in)
          names(phase_cols) <- present_phase
          dodge_w <- 0.75
          
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
            ggplot2::geom_boxplot(
              ggplot2::aes(group = interaction(.data[[gvar]], phase), fill = phase),
              colour = edge_col, dodge.width = dodge_w
            ) +
            ggforce::geom_sina(
              ggplot2::aes(
                x = .data[[gvar]], y = mean_val,
                colour = phase, group = interaction(.data[[gvar]], phase),
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", .data[[gvar]],
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.8, alpha = 0.55,
              position = ggplot2::position_dodge(width = dodge_w)
            ) +
            ggplot2::scale_fill_manual(values = phase_cols, breaks = present_phase, limits = present_phase, name = "Phase") +
            ggplot2::scale_colour_manual(values = rep(edge_col, length(present_phase)), guide = "none") +
            ggplot2::labs(
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              fill = "Phase",
              caption = "Each point is the mean per animal in the Before / Switch / After windows."
            ) +
            theme_obj +
            ggplot2::theme(
              legend.position = "right",
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              axis.title.x = ggplot2::element_blank(),
              plot.caption.position = "plot",
              plot.caption = ggplot2::element_text(hjust = 1)
            )
          return(gg)
        }
      }
      
      # =====================================================================
      # LINE PLOT — (val_per_well, SE/CI selon input$lineplot_error_mode)
      # =====================================================================
      if (plot_type == "lineplot") {
        
        gg <- ggplot2::ggplot(
          sub,
          ggplot2::aes(x = start_rounded, y = val_per_well, colour = .data[[gvar]], group = .data[[gvar]])
        )
        
        # Barres d'erreur / CI 95% si dispo
        if ("se_per_well" %in% names(sub)) {
          if (identical(input$lineplot_error_mode, "error_bar")) {
            gg <- gg +
              ggplot2::geom_errorbar(
                ggplot2::aes(ymin = val_per_well - se_per_well, ymax = val_per_well + se_per_well),
                width = 0.15, linewidth = 0.5, alpha = 0.7
              )
          } else if (identical(input$lineplot_error_mode, "ci95")) {
            gg <- gg +
              ggplot2::geom_ribbon(
                ggplot2::aes(
                  x = start_rounded,
                  ymin = val_per_well - 1.96 * se_per_well,
                  ymax = val_per_well + 1.96 * se_per_well,
                  fill = .data[[gvar]]
                ),
                alpha = 0.2, color = NA, inherit.aes = FALSE
              ) +
              ggplot2::scale_fill_manual(values = cols_named, limits = ord)
          }
        }
        
        gg <- gg +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(
            ggplot2::aes(
              text = paste0(
                "<br>Condition: ", .data[[gvar]],
                "<br>Time: ", start_rounded,
                "<br>Value: ", sprintf("%.2f", val_per_well)
              )
            ),
            size = 1.8, alpha = 0.55
          ) +
          ggplot2::scale_colour_manual(values = cols_named, limits = ord) +
          ggplot2::labs(
            x = sprintf("Time (%s)", if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original),
            y = sprintf("%s (Zone %s)", response_var, selected_zone),
            caption = "Each line is the normalized response per condition over time.",
            colour = "Condition Grouped"
          ) +
          theme_obj +
          ggplot2::theme(
            plot.caption.position = "plot",
            plot.caption = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(t = 10)),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
        return(gg)
      }
      
      # fallback
      ggplot2::ggplot() + ggplot2::geom_blank() + theme_obj
    }
    
    
    generate_r_script <- function(df, response_var, plot_type, boxplot_mode,
                                  selected_zone, theme_choice, condition_order, condition_colors,
                                  extra_params = list()) {
      sub <- subset(df, zone == selected_zone)
      sub <- droplevels(sub)
      theme_is_light <- tolower(theme_choice) == "light"
      edge_col <- if (theme_is_light) "black" else "white"
      
      script <- c(
        "# ======================================================================",
        "# REPRODUCTION SCRIPT - Generated by Shiny App",
        paste("# Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste("# Response Variable:", response_var),
        paste("# Zone:", selected_zone),
        paste("# Plot Type:", plot_type),
        paste("# Mode:", boxplot_mode %||% "pooled"),
        paste("# Theme:", theme_choice),
        "# ======================================================================\n",
        "library(ggplot2)",
        "library(ggforce)",
        "library(dplyr)",
        "library(readxl)\n"
      )
      
      dataset_type <- switch(plot_type,
                             "boxplot_periods" = "Boxplot Periods",
                             "boxplot_cumulate" = "Boxplot Cumulative",
                             "boxplot_delta" = "Boxplot Delta",
                             "lineplot" = "Lineplot",
                             "Unknown")
      xlsx_name <- paste0(dataset_type, "_dataset_", response_var, ".xlsx")
      
      script <- c(script,
                  "# ---- Load Data ----",
                  paste0("df <- readxl::read_excel('", xlsx_name, "')"),
                  paste0("sub <- subset(df, zone == ", selected_zone, ")"),
                  "sub <- droplevels(sub)\n"
      )
      
      if (length(condition_colors) > 0) {
        col_str <- paste0('"', condition_colors, '"', collapse = ", ")
        script <- c(script,
                    "# ---- Condition Colors ----",
                    paste0("condition_colors <- c(", col_str, ")"),
                    "names(condition_colors) <- sort(unique(sub$condition_grouped))\n"
        )
      }
      
      script <- c(script,
                  "# ---- Minimal Theme ----",
                  "theme_clean <- function() {",
                  " theme_bw() +",
                  " theme(",
                  " plot.title = element_text(size = 14, hjust = 0.5),",
                  paste0(" axis.text = element_text(color = '", edge_col, "', size = 12),"),
                  paste0(" axis.title = element_text(color = '", edge_col, "', size = 12),"),
                  " axis.title.x = element_text(margin = margin(t = 5, r = 15)),",
                  " axis.title.y = element_text(angle = 90, margin = margin(r = 10)),",
                  " legend.position = 'right',",
                  " legend.text = element_text(size = 12, face = 'italic'),",
                  " strip.text = element_text(size = 12),",
                  paste0(" strip.background = element_rect(fill = '", if(theme_is_light) "white" else "black", "', colour = '", edge_col, "'),"),
                  paste0(" panel.background = element_rect(fill = '", if(theme_is_light) "white" else "black", "', colour = '", edge_col, "'),"),
                  paste0(" panel.border = element_rect(color = '", edge_col, "', fill = NA),"),
                  " plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10))",
                  " )",
                  "}",
                  "theme_obj <- theme_clean()\n"
      )
      
      if (plot_type == "boxplot_periods") {
        if ("period_without_numbers" %in% names(sub)) {
          levels_present <- unique(sub$period_without_numbers)
          if (all(c("Light", "Dark") %in% levels_present)) {
            script <- c(script,
                        "# ---- Force Facet Order: Light to Dark ----",
                        "sub$period_without_numbers <- factor(sub$period_without_numbers, levels = c('Light', 'Dark'))\n"
            )
          }
        }
        
        if (boxplot_mode == "separated") {
          script <- c(script,
                      "# ---- Boxplot: Separated by Period ----",
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      paste0(" geom_boxplot(aes(fill = condition_grouped), colour = '", edge_col, "') +"),
                      paste0(" geom_sina(aes(text = paste0(",
                             "'Animal: ', animal, '\\n',",
                             "'Plate: ', plate_id, '\\n',",
                             "'Condition: ', condition_grouped, '\\n',",
                             "'Value: ', sprintf('%.2f', mean_val)",
                             ")), maxwidth = 0.25, size = 1.6, alpha = 0.55, colour = '", edge_col, "') +"),
                      " facet_wrap(~period_without_numbers, scales = 'free_x') +",
                      " scale_fill_manual(values = condition_colors) +",
                      paste0(" labs(y = '", response_var, " (Zone ", selected_zone, ")',"),
                      " caption = 'Each point = mean value per animal') +",
                      " theme_obj +",
                      " theme(legend.position = 'none',",
                      " axis.text.x = element_text(angle = 45, hjust = 1),",
                      " axis.title.x = element_blank(),",
                      " plot.caption = element_text(hjust = 1))"
          )
        } else {
          per_cols <- trimws(strsplit(extra_params$period_colors %||% "", ",")[[1]])
          per_cols <- ensure_colors(2, per_cols)
          col_str <- paste0('"', per_cols, '"', collapse = ", ")
          
          script <- c(script,
                      "# ---- Boxplot: Pooled (Light/Dark) ----",
                      paste0("period_colors <- c(", col_str, ")"),
                      "names(period_colors) <- c('Light', 'Dark')",
                      "dodge_w <- 0.75",
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      " geom_boxplot(",
                      " aes(group = interaction(condition_grouped, period_without_numbers), fill = period_without_numbers),",
                      paste0(" colour = '", edge_col, "', position = position_dodge(width = dodge_w)"),
                      " ) +",
                      paste0(" geom_sina(aes(colour = period_without_numbers, text = paste0(",
                             "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', mean_val)",
                             ")), maxwidth = 0.25, size = 1.8, alpha = 0.55, position = position_dodge(width = dodge_w)) +"),
                      " scale_fill_manual(values = period_colors, name = 'Period') +",
                      paste0(" scale_colour_manual(values = rep('", edge_col, "', 2), guide = 'none') +"),
                      paste0(" labs(y = '", response_var, " (Zone ", selected_zone, ")',"),
                      " caption = 'Each point = mean value per animal') +",
                      " theme_obj +",
                      " theme(legend.position = 'right',",
                      " axis.text.x = element_text(angle = 45, hjust = 1),",
                      " axis.title.x = element_blank())"
          )
        }
      }
      
      else if (plot_type == "boxplot_cumulate") {
        script <- c(script,
                    "# ---- Boxplot: Cumulative ----",
                    "gg <- ggplot(sub, aes(x = condition_grouped, y = cum)) +",
                    paste0(" geom_boxplot(aes(fill = condition_grouped), colour = '", edge_col, "') +"),
                    paste0(" geom_sina(aes(text = paste0(",
                           "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', cum)",
                           ")), maxwidth = 0.25, size = 1.4, alpha = 0.6, colour = '", edge_col, "') +"),
                    " scale_fill_manual(values = condition_colors) +",
                    paste0(" labs(y = 'Cumulative ", response_var, " (Zone ", selected_zone, ")',"),
                    " caption = 'Each point = cumulative value per animal') +",
                    " theme_obj +",
                    " theme(legend.position = 'none',",
                    " axis.text.x = element_text(angle = 45, hjust = 1),",
                    " axis.title.x = element_blank(),",
                    " plot.caption = element_text(hjust = 1))"
        )
      }
      
      else if (plot_type == "boxplot_delta") {
        transition <- extra_params$transition %||% "Unknown"
        script <- c(script,
                    paste0("# ---- Boxplot: Delta around '", transition, "' (Before/After only) ----"))
        
        phase_cols <- trimws(strsplit(extra_params$phase_colors %||% "", ",")[[1]])
        if (length(phase_cols) < 2) phase_cols <- c("#FF6F61", "#4CAF50")
        col_str <- paste0('"', phase_cols[1:2], '"', collapse = ", ")
        
        script <- c(script,
                    "sub$phase <- gsub(paste0('^', '" , transition , "', '_'), '', sub$transition_phase)",
                    "sub$phase <- dplyr::recode(sub$phase, before = 'Before', after = 'After')",
                    "present <- intersect(c('Before','After'), unique(sub$phase))",
                    "sub$phase <- factor(sub$phase, levels = present, ordered = TRUE)")
        
        if (identical(boxplot_mode, "separated")) {
          script <- c(script,
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      " geom_boxplot(aes(fill = condition_grouped), colour = '", edge_col, "') +",
                      " geom_sina(aes(text = paste0(",
                      " 'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', mean_val)",
                      " )), maxwidth = 0.25, size = 1.8, alpha = 0.55, colour = '", edge_col, "') +",
                      " facet_wrap(~phase, scales = 'free_x') +",
                      " scale_fill_manual(values = condition_colors) +",
                      " labs(y = '", response_var, " (Zone ", selected_zone, ")',",
                      " caption = 'Each point = mean value around transition (Before/After)') +",
                      " theme_obj +",
                      " theme(legend.position = 'none',",
                      " axis.text.x = element_text(angle = 45, hjust = 1),",
                      " axis.title.x = element_blank(),",
                      " plot.caption = element_text(hjust = 1))"
          )
        } else {
          script <- c(script,
                      paste0("phase_colors <- c(", col_str, ")"),
                      "names(phase_colors) <- c('Before','After')",
                      "dodge_w <- 0.75",
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      " geom_boxplot(aes(group = interaction(condition_grouped, phase), fill = phase),",
                      " colour = '", edge_col, "', position = position_dodge(width = dodge_w)) +",
                      " geom_sina(aes(colour = phase, text = paste0(",
                      " 'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', mean_val)",
                      " )), maxwidth = 0.25, size = 1.8, alpha = 0.55, position = position_dodge(width = dodge_w)) +",
                      " scale_fill_manual(values = phase_colors, name = 'Phase') +",
                      " scale_colour_manual(values = rep('", edge_col, "', 2), guide = 'none') +",
                      " labs(y = '", response_var, " (Zone ", selected_zone, ")',",
                      " caption = 'Each point = mean value around transition (Before/After)') +",
                      " theme_obj +",
                      " theme(legend.position = 'right',",
                      " axis.text.x = element_text(angle = 45, hjust = 1),",
                      " axis.title.x = element_blank())"
          )
        }
      }
      
      else if (plot_type == "lineplot") {
        time_unit <- extra_params$time_unit %||% "seconds"
        error_mode <- extra_params$error_mode %||% "error_bar"
        
        script <- c(script,
                    paste0("# ---- Lineplot: Over Time (", time_unit, ") ----"),
                    "gg <- ggplot(sub, aes(x = start_rounded, y = val_per_well, colour = condition_grouped, group = condition_grouped)) +"
        )
        
        if (error_mode == "error_bar" && "se_per_well" %in% names(sub)) {
          script <- c(script,
                      " geom_errorbar(aes(ymin = val_per_well - se_per_well, ymax = val_per_well + se_per_well),",
                      " width = 0.15, linewidth = 0.5, alpha = 0.7) +"
          )
        } else if (error_mode == "ci95" && "se_per_well" %in% names(sub)) {
          script <- c(script,
                      " geom_ribbon(aes(ymin = val_per_well - 1.96 * se_per_well, ymax = val_per_well + 1.96 * se_per_well, fill = condition_grouped),",
                      " alpha = 0.2, color = NA) +",
                      " scale_fill_manual(values = condition_colors) +"
          )
        }
        
        script <- c(script,
                    " geom_line(linewidth = 0.8) +",
                    paste0(" geom_point(aes(text = paste0(",
                           "Condition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', val_per_well)",
                           ")), size = 1.8, alpha = 0.55) +"),
                    " scale_colour_manual(values = condition_colors) +",
                    paste0(" labs(x = 'Time (", time_unit, ")',"),
                    paste0(" y = '", response_var, " (Zone ", selected_zone, ")',"),
                    " caption = 'Each line = normalized response per condition') +",
                    " theme_obj +",
                    " theme(axis.text.x = element_text(angle = 45, hjust = 1),",
                    " plot.caption = element_text(hjust = 1, margin = margin(t = 10)))"
        )
      }
      
      script <- c(script,
                  "\n# ---- Display ----",
                  "print(gg)\n",
                  "# ---- Optional: Interactive ----",
                  "# library(plotly)",
                  "# plotly::ggplotly(gg)"
      )
      
      paste(script, collapse = "\n")
    }
    
    output$figure_plot <- shiny::renderUI({
      shiny::req(rv$plot)
      mode_out <- isolate(input$output_mode); if (is.null(mode_out)) mode_out <- "PNG"
      if (toupper(mode_out) == "HTML") plotly::plotlyOutput(ns("plotly_plot"), height = "600px")
      else shiny::plotOutput(ns("static_plot"), height = "600px")
    })
    output$plotly_plot <- plotly::renderPlotly({ shiny::req(rv$plot); rv$plot })
    output$static_plot <- shiny::renderPlot({ shiny::req(rv$plot); rv$plot })
    output$console_output <- renderPrint({
      msgs <- console_messages()
      if (length(msgs) == 0 || all(msgs == "👋 Ready.")) {
        cat("👋 Ready.")
      } else {
        cat(paste(msgs, collapse = "\n"))
      }
    })
    
    shiny::observeEvent(plotly::event_data("plotly_restyle", source = ns("html")), {
      ed <- plotly::event_data("plotly_restyle", source = ns("html")); if (is.null(ed)) return()
      if (!is.null(ed[["visible"]]) && ed[["visible"]][[1]] == "legendonly") {
        rv$hidden_traces <- sort(unique(c(rv$hidden_traces, ed$traces)))
      }
      if (!is.null(ed[["visible"]]) && ed[["visible"]][[1]] == TRUE) {
        rv$hidden_traces <- setdiff(rv$hidden_traces, ed$traces)
      }
    }, ignoreInit = TRUE)
    
    save_current_state <- function(p, sub = NULL) {
      mode_out <- isolate(input$output_mode); if (is.null(mode_out) || !nzchar(mode_out)) mode_out <- "PNG"
      if (toupper(mode_out) == "HTML") {
        pooled <- (isolate(input$plot_type) == "boxplot_periods" && isolate(input$boxplot_periods_mode) == "pooled") ||
          (isolate(input$plot_type) == "boxplot_delta" && isolate(input$boxplot_delta_mode) == "pooled")
        rv$plot <- to_widget(p, pooled = pooled, df = sub)
        if (isolate(input$plot_type) == "lineplot" && length(rv$hidden_traces)) {
          try({
            plotly::plotlyProxy(ns("plotly_plot"), session) %>%
              plotly::plotlyProxyInvoke("restyle", list(visible = "legendonly"), as.list(rv$hidden_traces))
          }, silent = TRUE)
        }
      } else {
        rv$plot <- if (inherits(p, "ggplot")) p else (ggplot2::ggplot() + ggplot2::geom_blank())
      }
    }
    
    make_plot <- function(log_it = FALSE) {
      df <- switch(
        input$plot_type,
        "boxplot_periods" = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
        "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
        "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]],
        "lineplot" = rv$all_zone_combined_lineplots[[input$response_var]]
      )
      if (is.null(df) || is.null(input$response_var) || input$response_var == "") {
        if (log_it) log("⚠️ Select a response variable and generate datasets first."); return(invisible(NULL))
      }
      selected_zone <- input$selected_zone
      if (is.null(selected_zone) || !length(selected_zone)) {
        zones <- sort(unique(df$zone)); if (!length(zones)) { if (log_it) log("⚠️ No zones available in current dataset."); return(invisible(NULL)) }
        selected_zone <- as.character(zones[1])
      }
      oc <- order_and_colors(df)
      boxplot_mode <- switch(input$plot_type,
                             "boxplot_periods" = input$boxplot_periods_mode,
                             "boxplot_cumulate" = "separated",
                             "boxplot_delta" = input$boxplot_delta_mode, NULL)
      p <- generate_plot(df, input$response_var, input$plot_type, boxplot_mode,
                         selected_zone, input$theme_switch, oc$order, oc$colors)
      sub <- tryCatch({ subset(df, zone == selected_zone) }, error = function(e) NULL)
      if (!is.null(sub)) sub <- droplevels(sub)
      save_current_state(p, sub = sub)
      if (log_it) log("🖼 Figure generated.")
      invisible(TRUE)
    }
    
    shiny::observeEvent(input$generate_figure, { make_plot(log_it = TRUE) })
    shiny::observeEvent(list(
      input$theme_switch, input$output_mode,
      input$boxplot_periods_mode, input$boxplot_delta_mode,
      input$response_var, input$selected_zone,
      input$lineplot_error_mode, input$time_unit_convert, input$time_unit_target, input$time_unit_original
    ), {
      df_exists <- switch(
        isolate(input$plot_type),
        "boxplot_periods" = !is.null(rv$all_zone_combined_light_dark_boxplots[[isolate(input$response_var)]]),
        "boxplot_cumulate" = !is.null(rv$all_zone_combined_cum_boxplots[[isolate(input$response_var)]]),
        "boxplot_delta" = !is.null(rv$all_zone_combined_delta_boxplots[[isolate(input$response_var)]]),
        "lineplot" = !is.null(rv$all_zone_combined_lineplots[[isolate(input$response_var)]]),
        FALSE
      )
      if (isTRUE(df_exists)) make_plot(log_it = FALSE)
    }, ignoreInit = TRUE)
    
    output$download_plot_script <- shiny::downloadHandler(
      filename = function() {
        var <- input$response_var
        zone <- input$selected_zone
        mode <- if (input$plot_type %in% c("boxplot_periods", "boxplot_delta")) {
          input[[paste0("boxplot_", sub("boxplot_", "", input$plot_type), "_mode")]] %||% "separated"
        } else "pooled"
        transition <- if (input$plot_type == "boxplot_delta") paste0("_", input$transition_select) else ""
        sprintf("%s_%s_zone%s_%s%s.R", input$plot_type, var, zone, mode, transition)
      },
      content = function(file) {
        df <- switch(
          input$plot_type,
          "boxplot_periods" = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
          "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
          "boxplot_delta" = rv$all_zone_combined_delta_boxplots[[input$response_var]],
          "lineplot" = rv$all_zone_combined_lineplots[[input$response_var]]
        )
        shiny::req(df, input$response_var, input$selected_zone)
        
        oc <- order_and_colors(df)
        boxplot_mode <- switch(input$plot_type,
                               "boxplot_periods" = input$boxplot_periods_mode,
                               "boxplot_delta" = input$boxplot_delta_mode,
                               "separated")
        
        extra <- list()
        if (input$plot_type == "boxplot_periods" && boxplot_mode == "pooled")
          extra$period_colors <- input$boxplot_periods_colors
        if (input$plot_type == "boxplot_delta" && boxplot_mode == "pooled")
          extra$phase_colors <- input$boxplot_delta_phase_colors
        if (input$plot_type == "boxplot_delta")
          extra$transition <- input$transition_select
        if (input$plot_type == "lineplot") {
          unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
          extra$time_unit <- unit
          extra$error_mode <- input$lineplot_error_mode
        }
        
        script_content <- generate_r_script(
          df = df,
          response_var = input$response_var,
          plot_type = input$plot_type,
          boxplot_mode = boxplot_mode,
          selected_zone = input$selected_zone,
          theme_choice = input$theme_switch,
          condition_order = oc$order,
          condition_colors = oc$colors,
          extra_params = extra
        )
        
        writeLines(script_content, file)
      }
    )
    
    output$dataset_table <- DT::renderDataTable({
      shiny::req(input$dataset_type, input$dataset_response_var)
      dtype <- input$dataset_type
      df <- if (dtype == "Boxplot Periods") {
        rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]]
      } else if (dtype == "Boxplot Cumulative") {
        rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]]
      } else if (dtype == "Boxplot Delta") {
        rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]]
      } else {
        rv$all_zone_combined_lineplots[[input$dataset_response_var]]
      }
      shiny::req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$save_current_figure <- shiny::downloadHandler(
      filename = function() {
        var <- input$response_var; zone <- input$selected_zone; theme <- tolower(input$theme_switch)
        mode <- if (input$plot_type == "boxplot_periods") input$boxplot_periods_mode
        else if (input$plot_type == "boxplot_delta") input$boxplot_delta_mode
        else if (input$plot_type == "boxplot_cumulate") "cumulative"
        else if (input$plot_type == "lineplot") "lineplot"
        else "unknown"
        transition <- if (input$plot_type == "boxplot_delta") paste0("_", input$transition_select) else ""
        sprintf("%s_%s_zone%s_%s%s_%s.png", input$plot_type, var, zone, mode, transition, theme)
      },
      content = function(file) {
        shiny::req(rv$plot)
        if (inherits(rv$plot, "plotly")) {
          plotly::save_image(rv$plot, file, width = 1200, height = 800)
        } else {
          ggplot2::ggsave(file, plot = rv$plot, device = "png", width = 12, height = 8, dpi = 150)
        }
      }
    )
    
    # ==================================================================
    # Delta Percentage Tables
    # ==================================================================
    compute_delta_percentages <- function(df, var) {
      shiny::req(df, var %in% names(df))
      tr <- input$transition_select
      df <- dplyr::filter(df, grepl(paste0("^", tr, "_"), transition_phase))
      df$phase <- gsub(paste0("^", tr, "_"), "", df$transition_phase)
      df$phase <- dplyr::recode(df$phase,
                                before = "Before",
                                switch = "Switch",
                                after = "After")
      present <- intersect(c("Before","Switch","After"), unique(df$phase))
      if (length(present) < 2) return(NULL)
      
      df <- df %>% dplyr::filter(phase %in% present)
      
      if (input$delta_table_type == "Momentum Comparisons") {
        # Before → Switch, Switch → After
        comps <- list(
          "Before_to_Switch" = c("Before","Switch"),
          "Switch_to_After" = c("Switch","After")
        )
        out <- purrr::map_dfr(names(comps), function(name) {
          p1 <- comps[[name]][1]; p2 <- comps[[name]][2]
          df12 <- df %>% dplyr::filter(phase %in% c(p1,p2))
          if (!nrow(df12)) return(NULL)
          df12 %>%
            dplyr::group_by(condition_grouped, zone, plate_id, animal) %>%
            dplyr::filter(n() == 2) %>%
            dplyr::summarise(
              val1 = mean_val[phase == p1],
              val2 = mean_val[phase == p2],
              pct_change = ((val2 - val1) / pmax(val1, 1e-6)) * 100,
              .groups = "drop"
            ) %>%
            dplyr::mutate(comparison = name)
        })
      } else {
        # Condition vs Condition within each phase
        out <- df %>%
          dplyr::group_by(phase, zone) %>%
          dplyr::do({
            d <- .
            conds <- unique(d$condition_grouped)
            if (length(conds) < 2) return(NULL)
            combos <- utils::combn(conds, 2, simplify = FALSE)
            purrr::map_dfr(combos, function(pair) {
              c1 <- pair[1]; c2 <- pair[2]
              d12 <- d %>% dplyr::filter(condition_grouped %in% pair)
              if (!nrow(d12)) return(NULL)
              d12 %>%
                dplyr::group_by(plate_id, animal) %>%
                dplyr::filter(n() == 2) %>%
                dplyr::summarise(
                  val1 = mean_val[condition_grouped == c1],
                  val2 = mean_val[condition_grouped == c2],
                  pct_change = ((val2 - val1) / pmax(val1, 1e-6)) * 100,
                  .groups = "drop"
                ) %>%
                dplyr::mutate(
                  comparison = paste0(c1, "_vs_", c2),
                  phase = unique(d$phase)
                )
            })
          }) %>%
          dplyr::ungroup()
      }
      out
    }
    
    output$delta_percentage_table <- DT::renderDataTable({
      shiny::req(
        input$delta_table_var,
        rv$all_zone_combined_delta_boxplots[[input$delta_table_var]]
      )
      df <- rv$all_zone_combined_delta_boxplots[[input$delta_table_var]]
      tab <- compute_delta_percentages(df, input$delta_table_var)
      if (is.null(tab)) {
        DT::datatable(data.frame(Message = "Not enough data for percentage calculations."), 
                      options = list(dom = "t"))
      } else {
        DT::datatable(tab, options = list(pageLength = 15, scrollX = TRUE))
      }
    })
    
    output$download_current_delta_table <- shiny::downloadHandler(
      filename = function() {
        sprintf("delta_pct_%s_%s_%s.xlsx",
                input$delta_table_type, input$delta_table_var, input$transition_select)
      },
      content = function(file) {
        shiny::req(
          input$delta_table_var,
          rv$all_zone_combined_delta_boxplots[[input$delta_table_var]]
        )
        df <- rv$all_zone_combined_delta_boxplots[[input$delta_table_var]]
        tab <- compute_delta_percentages(df, input$delta_table_var)
        if (!is.null(tab)) {
          openxlsx::write.xlsx(tab, file)
        }
      }
    )
    
    output$download_all_delta_tables <- shiny::downloadHandler(
      filename = function() {
        sprintf("all_delta_tables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        shiny::req(rv$all_zone_combined_delta_boxplots, input$transition_select)
        tmp <- tempdir()
        files <- c()
        for (v in names(rv$all_zone_combined_delta_boxplots)) {
          df <- rv$all_zone_combined_delta_boxplots[[v]]
          tab <- compute_delta_percentages(df, v)
          if (!is.null(tab)) {
            fn <- file.path(tmp, sprintf("delta_pct_%s_%s_%s.xlsx", input$delta_table_type, v, input$transition_select))
            openxlsx::write.xlsx(tab, fn)
            files <- c(files, fn)
          }
        }
        if (length(files)) {
          zip::zip(file, files, mode = "cherry-pick")
        }
      },
      contentType = "application/zip"
    )
    
    # ==================================================================
    # Dataset Download (all types)
    # ==================================================================
    output$download_current_dataset <- shiny::downloadHandler(
      filename = function() {
        dtype <- switch(input$dataset_type,
                        "boxplot Periods" = "periods",
                        "boxplot Cumulative" = "cumulative",
                        "boxplot Delta" = "delta",
                        "lineplot" = "lineplot")
        sprintf("%s_dataset_%s.xlsx", dtype, input$dataset_response_var)
      },
      content = function(file) {
        df <- switch(
          input$dataset_type,
          "Boxplot Periods" = rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]],
          "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
          "Boxplot Delta" = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
          "Lineplot" = rv$all_zone_combined_lineplots[[input$dataset_response_var]]
        )
        shiny::req(df)
        openxlsx::write.xlsx(df, file)
      }
    )
    
    # ==================================================================
    # Final reactive updates
    # ==================================================================
    shiny::observeEvent(input$generate_delta_tables, {
      shiny::req(rv$all_zone_combined_delta_boxplots, input$delta_table_var)
      log("Delta percentage tables ready for viewing/download.")
    })
    
    # Auto-select first zone when datasets are generated
    shiny::observeEvent(rv$all_zone_combined_light_dark_boxplots, {
      if (is.null(input$selected_zone) || input$selected_zone == "") {
        zones <- sort(unique(rv$all_zone_combined_light_dark_boxplots[[1]]$zone))
        if (length(zones)) shiny::updateSelectInput(session, "selected_zone", selected = as.character(zones[1]))
      }
    }, ignoreNULL = TRUE)
    
    # Clean up on module destroy
    onStop(function() {
      rv$plot <- NULL
      rv$hidden_traces <- integer(0)
    })
  })
}

# ======================================================================
# End of visualization_module.R
# ======================================================================