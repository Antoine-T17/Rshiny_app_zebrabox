# ======================================================================
# modules/visualization_module.R
# Generic visualization module (TM/QM × LDM/VM) driven by config
# Centralized dataset builders, ggplotly-only HTML, UI buttons + fixes
# ======================================================================

# ---- Local themes -----------------------------------------------------
light_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace% ggplot2::theme(
    plot.title   = ggplot2::element_text(color = "black", size = 14, hjust = .5),
    axis.text    = ggplot2::element_text(color = "black", size = 12),
    axis.title.x = ggplot2::element_text(color = "black", size = 12, margin = ggplot2::margin(t = 5, r = 15)),
    axis.title.y = ggplot2::element_text(color = "black", size = 12, angle = 90, margin = ggplot2::margin(r = 10)),
    legend.position = "right",
    legend.text  = ggplot2::element_text(color = "black", size = 12, face = "italic"),
    legend.title = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 12),
    strip.background = ggplot2::element_rect(fill = "white", colour = "black"),   # keep facet box in HTML
    panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
    plot.caption = ggplot2::element_text(color = "black", size = 8, hjust = 1, margin = ggplot2::margin(t = 10)),
    panel.border = ggplot2::element_rect(color = "black", fill = NA)              # keep panel border in HTML
  )
}

dark_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace% ggplot2::theme(
    plot.title   = ggplot2::element_text(color = "white", size = 14, hjust = .5),
    axis.text    = ggplot2::element_text(color = "white", size = 12),
    axis.title.x = ggplot2::element_text(color = "white", size = 12, margin = ggplot2::margin(t = 5, r = 15)),
    axis.title.y = ggplot2::element_text(color = "white", size = 12, angle = 90, margin = ggplot2::margin(r = 10)),
    legend.position = "right",
    legend.text  = ggplot2::element_text(color = "white", size = 12, face = "italic"),
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(fill = "black"),
    legend.key        = ggplot2::element_rect(fill = "black"),
    strip.text.x      = ggplot2::element_text(color = "white", size = 12),
    strip.background  = ggplot2::element_rect(fill = "black", color = "white"),
    plot.background   = ggplot2::element_rect(fill = "black"),
    panel.background  = ggplot2::element_rect(fill = "black", colour = "white"),
    panel.border      = ggplot2::element_rect(color = "white", fill = NA),
    panel.grid.major  = ggplot2::element_line(color = "grey30"),
    panel.grid.minor  = ggplot2::element_line(color = "grey30"),
    plot.caption      = ggplot2::element_text(color = "white", size = 8, hjust = 1, margin = ggplot2::margin(t = 10))
  )
}

# ---- UI ---------------------------------------------------------------
visualization_module_ui <- function(id, config) {
  ns   <- shiny::NS(id)
  cfg  <- if (is.function(config)) config() else config
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
        shiny::textInput(ns("delta_time"), "Delta Time Window (seconds)", value = "60"),
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
                        shiny::downloadButton(ns("download_plot_script"), "Download R Script (.R)")),
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
    ns  <- session$ns
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
        if (!is.null(input$response_var) && isolate(input$response_var) %in% vars)            rv_sel <- isolate(input$response_var)
        if (!is.null(input$dataset_response_var) && isolate(input$dataset_response_var) %in% vars) ds_sel <- isolate(input$dataset_response_var)
        if (!is.null(input$delta_table_var) && isolate(input$delta_table_var) %in% vars)      dt_sel <- isolate(input$delta_table_var)
      }
      shiny::updateSelectInput(session, "response_var",         choices = c("", vars), selected = rv_sel)
      shiny::updateSelectInput(session, "dataset_response_var", choices = c("", vars), selected = ds_sel)
      shiny::updateSelectInput(session, "delta_table_var",      choices = c("", vars), selected = dt_sel)
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
      shiny::req(rv$processing_results, "processed_data_list" %in% names(rv$processing_results))
      rv$processed_data_list <- purrr::map(
        rv$processing_results$processed_data_list,
        ~ dplyr::mutate(.x,
                        plate_id   = as.character(plate_id),
                        totaldist  = smldist + lardist + inadist,
                        totaldur   = smldur  + lardur  + inadur,
                        totalct    = smlct   + larct   + inact,
                        totalspeed = totaldist / pmax(totaldur, 1),
                        smlspeed   = smldist / pmax(smldur, 1),
                        larspeed   = lardist / pmax(lardur, 1),
                        inaspeed   = inadist / pmax(inadur, 1)
        )
      )
      rv$all_zone_combined <- dplyr::bind_rows(rv$processed_data_list)
      rv$all_zone_combined
    }
    
    build_periods_df <- function(az, v, cfg) {
      periods <- unique(az$period_without_numbers)
      keys    <- cfg$period_keys
      labels  <- cfg$period_labels %||% keys
      
      if (is.null(keys) || length(keys) < 2) {
        stop("Configuration error: 'period_keys' must contain at least 2 values.")
      }
      
      p1 <- grep(keys[1], periods, ignore.case = TRUE, value = TRUE)
      p2 <- grep(keys[2], periods, ignore.case = TRUE, value = TRUE)
      
      if (length(p1) == 0 || length(p2) == 0) {
        stop(sprintf("Could not find periods matching '%s' or '%s' in data.", keys[1], keys[2]))
      }
      
      out <- az %>%
        dplyr::filter(period_without_numbers %in% c(p1, p2)) %>%
        dplyr::group_by(period_without_numbers, zone, condition_tagged, plate_id) %>%
        dplyr::summarise(
          plate_id            = dplyr::first(as.character(plate_id)),
          start               = dplyr::first(start),
          period_with_numbers = dplyr::first(period_with_numbers),
          condition_grouped   = dplyr::first(condition_grouped),
          condition           = dplyr::first(condition),
          animal              = dplyr::first(animal),
          mean_val            = mean(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          period_without_numbers = factor(
            period_without_numbers,
            levels = c(p1, p2),
            labels = labels
          )
        ) %>%
        # ---- ordre des colonnes
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
        # ---- ordre des colonnes
        dplyr::select(dplyr::any_of(c(
          "zone", "condition_grouped", "condition_tagged",
          "plate_id", "animal", "cum"
        )))
    }
    
    
    build_delta_split <- function(az, vars, transition, delta_sec) {
      shiny::req("boundary_associations_list" %in% names(rv$processing_results))
      b <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
      if (!nrow(b)) return(NULL)
      
      b_clean <- b %>%
        dplyr::mutate(plate_id = as.character(plate_id)) %>%
        dplyr::arrange(transition, plate_id, time_switch) %>%
        dplyr::group_by(transition, plate_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
      
      bd_sel <- b_clean %>%
        dplyr::filter(transition == transition) %>%
        dplyr::select(plate_id, time_switch)
      
      joined <- az %>%
        dplyr::mutate(plate_id = as.character(plate_id)) %>%
        dplyr::inner_join(bd_sel, by = "plate_id", relationship = "many-to-many") %>%
        dplyr::mutate(
          phase_raw = dplyr::case_when(
            start >= time_switch - delta_sec & start <  time_switch               ~ "before",
            start >= time_switch              & start <  time_switch + delta_sec  ~ "switch",
            start >= time_switch + delta_sec  & start <  time_switch + 2*delta_sec ~ "after",
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(phase_raw)) %>%
        dplyr::mutate(transition_phase = paste0(transition, "_", phase_raw))
      
      if (!nrow(joined)) return(NULL)
      
      phased_long <- tidyr::pivot_longer(
        joined, cols = tidyselect::all_of(vars),
        names_to = "variable", values_to = "value"
      ) %>%
        dplyr::group_by(transition_phase, zone, condition_tagged, plate_id, animal, variable) %>%
        dplyr::summarise(
          mean_val = mean(value, na.rm = TRUE),
          condition_grouped = dplyr::first(condition_grouped),
          .groups = "drop"
        ) %>%
        # ---- ordre des colonnes
        dplyr::select(dplyr::any_of(c(
          "zone", "condition_grouped", "condition_tagged",
          "plate_id", "animal", "mean_val", "transition_phase", "variable"
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
      agg_s    <- convert_time(as.numeric(agg_period), agg_unit, "seconds")
      gvar     <- "condition_grouped"  # pooled only
      
      # 1) Valeur par animal (par puits) après binning temporel
      per_well <- az %>%
        dplyr::mutate(
          plate_id      = as.character(plate_id),
          start_rounded = floor(start / agg_s) * agg_s
        ) %>%
        dplyr::group_by(.data[[gvar]], zone, start_rounded, plate_id, animal) %>%
        dplyr::summarise(
          var_value_per_well = sum(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        )
      
      # 2) Résumé par condition × zone × temps
      summary_df <- per_well %>%
        dplyr::group_by(.data[[gvar]], zone, start_rounded) %>%
        dplyr::summarise(
          total_val     = sum(var_value_per_well, na.rm = TRUE),
          mean_per_well = mean(var_value_per_well, na.rm = TRUE),
          sd_per_well   = stats::sd(var_value_per_well, na.rm = TRUE),
          n_wells       = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(se_per_well = sd_per_well / sqrt(pmax(n_wells, 1)))
      
      # 3) Joindre le résumé à chaque ligne (on garde animal & plate_id)
      out <- per_well %>%
        dplyr::left_join(summary_df, by = c(gvar, "zone", "start_rounded")) %>%
        dplyr::mutate(
          start_rounded = if (!identical(unit_from, agg_unit))
            convert_time(start_rounded, "seconds", agg_unit) else start_rounded
        ) %>%
        dplyr::rename(val_per_well = mean_per_well) %>%
        # ---- ordre des colonnes
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
      console_messages("👻 No messages yet."); shiny::showNotification("Console cleared!", type = "message")
    })
    
    output$aggregation_period_label <- shiny::renderUI({
      unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
      shiny::textInput(ns("aggregation_period"), sprintf("Aggregation Period (in %s)", unit), value = "60")
    })
    
    output$transition_select_ui <- shiny::renderUI({
      shiny::req(rv$processing_results, get_boundaries_list())
      b  <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
      tr <- unique(b$transition)
      if (!length(tr)) return(shiny::div("No transitions available. Please run processing first."))
      shiny::selectInput(ns("transition_select"), "Select Transition", choices = tr, selected = tr[1])
    })
    
    output$figure_selector <- shiny::renderUI({
      shiny::req(input$plot_type, input$response_var)
      df <- switch(
        input$plot_type,
        "boxplot_periods"   = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
        "boxplot_cumulate"  = rv$all_zone_combined_cum_boxplots[[input$response_var]],
        "boxplot_delta"     = rv$all_zone_combined_delta_boxplots[[input$response_var]],
        "lineplot"          = rv$all_zone_combined_lineplots[[input$response_var]]
      )
      if (is.null(df)) return(shiny::helpText("Generate datasets first (click the button above)."))
      zones <- sort(unique(df$zone))
      choices <- stats::setNames(as.character(zones), paste("Zone", zones))
      shiny::selectInput(ns("selected_zone"), "Select Zone", choices = choices, selected = choices[1])
    })
    
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
        log(paste0("✅ Periods datasets created."))
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
    validate_transition <- function(transition, boundaries, all_zone_combined) {
      if (is.null(boundaries) || !nrow(boundaries)) return(FALSE)
      tt <- boundaries$time_switch[boundaries$transition == transition]
      if (!length(tt) || !any(abs(all_zone_combined$start - tt) < 1)) {
        shiny::showModal(shiny::modalDialog(
          title = "❌ Invalid Transition",
          sprintf("The selected transition '%s' has no corresponding timestamp in the data.", transition),
          easyClose = TRUE
        ))
        return(FALSE)
      }
      TRUE
    }
    
    shiny::observeEvent(input$generate_delta_dfs, {
      tryCatch({
        shiny::req(validate_num_pos(input$delta_time, "Delta time window must be a positive number."))
        az <- prepare_all_zone()
        b  <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
        shiny::req(validate_transition(input$transition_select, b, az))
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
              v  = v,
              agg_period= input$aggregation_period,
              unit_from = input$time_unit_original,
              unit_to   = input$time_unit_target,
              convert   = input$time_unit_convert
            )
          }),
          EXPECTED_VARS()
        )
        log("✅ Lineplot datasets created (normalized per well, pooled).")
      }, error = function(e) log(paste("❌ Lineplot generation failed:", e$message)))
    })
    
    # ==================================================================
    # Plot factory
    # ==================================================================
    order_and_colors <- function(df) {
      gvar <- "condition_grouped"
      ord  <- if (nzchar(input$condition_grouped_order))
        trimws(strsplit(input$condition_grouped_order, ",")[[1]]) else unique(df[[gvar]])
      raw_cols <- if (nzchar(input$condition_grouped_color))
        trimws(strsplit(input$condition_grouped_color, ",")[[1]]) else character(0)
      n_levels <- length(unique(df[[gvar]]))
      cols <- ensure_colors(n_levels, raw_cols)
      list(order = ord, colors = cols)
    }
    
    generate_plot <- function(df, response_var, plot_type, boxplot_mode,
                              selected_zone, theme_choice, condition_order, condition_colors) {
      
      sub <- subset(df, zone == selected_zone)
      sub <- droplevels(sub)
      theme_is_light <- tolower(theme_choice) == "light"
      theme_obj <- if (theme_is_light) light_theme() else dark_theme()
      edge_col  <- if (theme_is_light) "black" else "white"
      
      # ---- boxplot_periods ----
      if (plot_type == "boxplot_periods") {
        if (boxplot_mode == "separated") {
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = condition_grouped, y = mean_val)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = condition_grouped, fill = condition_grouped), colour = edge_col) +
            ggforce::geom_sina(
              ggplot2::aes(
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", condition_grouped,
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.6, alpha = 0.55, colour = edge_col
            ) +
            ggplot2::facet_wrap(~period_without_numbers, scales = "free_x") +
            ggplot2::scale_fill_manual(values = condition_colors) +
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
          
        } else {  # pooled
          per_cols <- trimws(strsplit(input$boxplot_periods_colors, ",")[[1]])
          per_cols <- ensure_colors(length(unique(sub$period_without_numbers)), per_cols)
          dodge_w  <- 0.75
          
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = condition_grouped, y = mean_val)) +
            ggplot2::geom_boxplot(
              ggplot2::aes(
                group = interaction(condition_grouped, period_without_numbers),
                fill  = period_without_numbers
              ),
              colour = edge_col,
              dodge.width = dodge_w
            ) +
            ggforce::geom_sina(
              ggplot2::aes(
                x = condition_grouped,
                y = mean_val,
                colour = period_without_numbers,
                group = interaction(condition_grouped, period_without_numbers),
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", condition_grouped,
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.8, alpha = 0.55,
              position = ggplot2::position_dodge(width = dodge_w)
            ) +
            ggplot2::scale_fill_manual(values = per_cols, name = "Period") +
            ggplot2::scale_colour_manual(values = rep(edge_col, length(per_cols)), guide = "none") +
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
      
      # ---- boxplot_cumulate ----
      if (plot_type == "boxplot_cumulate") {
        n_animals <- sub %>%
          dplyr::group_by(condition_grouped, zone, plate_id) %>%
          dplyr::summarise(n_animals_plate = dplyr::n_distinct(animal), .groups = "drop") %>%
          dplyr::group_by(condition_grouped, zone) %>%
          dplyr::summarise(n = sum(n_animals_plate), .groups = "drop") %>%
          dplyr::mutate(y = -Inf)
        
        gg <- ggplot2::ggplot(sub, ggplot2::aes(x = condition_grouped, y = cum)) +
          ggplot2::geom_boxplot(ggplot2::aes(group = condition_grouped, fill = condition_grouped), colour = edge_col,) +
          ggforce::geom_sina(
            ggplot2::aes(
              text = paste0(
                "Animal: ", animal,
                "<br>Plate: ", plate_id,
                "<br>Condition: ", condition_grouped,
                "<br>Value: ", sprintf("%.2f", cum)
              )
            ),
            maxwidth = 0.25, size = 1.4, alpha = 0.6, colour = edge_col
          ) +
          ggplot2::geom_text(
            data = n_animals,
            ggplot2::aes(x = condition_grouped, y = y, label = paste0("n=", n)),
            inherit.aes = FALSE, vjust = -0.5, size = 3, colour = edge_col
          ) +
          ggplot2::scale_fill_manual(values = condition_colors) +
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
      
      # ---- boxplot_delta ----
      if (plot_type == "boxplot_delta") {
        tr <- input$transition_select
        sub <- dplyr::filter(sub, grepl(tr, transition_phase))
        sub$phase <- factor(
          sub$transition_phase,
          levels = paste0(tr, "_", c("before", "switch", "after")),
          labels = c("Before", "Switch", "After"),
          ordered = TRUE
        )
        
        if (boxplot_mode == "separated") {
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = condition_grouped, y = mean_val)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = condition_grouped, fill = condition_grouped), colour = edge_col,) +
            ggforce::geom_sina(
              ggplot2::aes(
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", condition_grouped,
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.8, alpha = 0.55, colour = edge_col
            ) +
            ggplot2::facet_wrap(~phase, scales = "free_x") +
            ggplot2::scale_fill_manual(values = condition_colors) +
            ggplot2::labs(
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              caption = "Each point is the mean for one animal around a transition."
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
          
        } else {  # pooled
          phase_cols <- trimws(strsplit(input$boxplot_delta_phase_colors, ",")[[1]])
          phase_cols <- ensure_colors(3, phase_cols)
          sub$transition_phase <- factor(
            sub$transition_phase,
            levels = paste0(tr, "_", c("before", "switch", "after")),
            labels = c("Before", "Switch", "After"),
            ordered = TRUE
          )
          dodge_w <- 0.75
          
          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = condition_grouped, y = mean_val)) +
            ggplot2::geom_boxplot(
              ggplot2::aes(
                group = interaction(condition_grouped, transition_phase),
                fill  = transition_phase
              ),
              colour = edge_col,
              dodge.width = dodge_w
            ) +
            ggforce::geom_sina(
              ggplot2::aes(
                x = condition_grouped,
                y = mean_val,
                colour = transition_phase,
                group = interaction(condition_grouped, transition_phase),
                text = paste0(
                  "Animal: ", animal,
                  "<br>Plate: ", plate_id,
                  "<br>Condition: ", condition_grouped,
                  "<br>Value: ", sprintf("%.2f", mean_val)
                )
              ),
              maxwidth = 0.25, size = 1.8, alpha = 0.55,
              position = ggplot2::position_dodge(width = dodge_w)
            ) +
            ggplot2::scale_fill_manual(values = phase_cols, name = "Phase") +
            ggplot2::scale_colour_manual(values = rep(edge_col, length(phase_cols)), guide = "none") +
            ggplot2::labs(
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              fill = "Phase",
              caption = "Each point is the mean for one animal around a transition."
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
      
      # ---- lineplot (pooled only) ----
      if (plot_type == "lineplot") {
        gvar <- "condition_grouped"
        time_label <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
        
        present_levels <- unique(sub[[gvar]])
        ord <- intersect(condition_order, present_levels)
        if (!length(ord)) ord <- present_levels
        sub[[gvar]] <- factor(sub[[gvar]], levels = ord)
        
        gg <- ggplot2::ggplot(
          sub,
          ggplot2::aes(x = start_rounded, y = val_per_well, colour = .data[[gvar]], group = .data[[gvar]])
        )
        
        if ("se_per_well" %in% names(sub)) {
          if (input$lineplot_error_mode == "error_bar") {
            gg <- gg +
              ggplot2::geom_errorbar(
                ggplot2::aes(ymin = val_per_well - se_per_well, ymax = val_per_well + se_per_well, colour = .data[[gvar]]),
                width = 0.15, linewidth = 0.5, alpha = 0.7
              )
          } else if (input$lineplot_error_mode == "ci95") {
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
              ggplot2::scale_fill_manual(values = condition_colors, breaks = ord)
          }
        }
        
        gg <- gg +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(
            ggplot2::aes(
              text = paste0(
                "Animal: ", animal,
                "<br>Plate: ", plate_id,
                "<br>Condition: ", condition_grouped,
                "<br>Value: ", sprintf("%.2f", val_per_well)
              )
            ), size = 1.8, alpha = 0.55
          ) +
          ggplot2::scale_colour_manual(values = condition_colors, breaks = ord) +
          ggplot2::labs(
            x = sprintf("Time (%s)", time_label),
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
      
      # Fallback
      ggplot2::ggplot() + ggplot2::geom_blank()
    }
    
    # ==================================================================
    # R script
    # ==================================================================
    
    generate_r_script <- function(df, response_var, plot_type, boxplot_mode,
                                  selected_zone, theme_choice, condition_order, condition_colors,
                                  extra_params = list()) {
      sub <- subset(df, zone == selected_zone)
      sub <- droplevels(sub)
      theme_is_light <- tolower(theme_choice) == "light"
      edge_col <- if (theme_is_light) "black" else "white"
      
      # === EN-TÊTE ===
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
      
      # === FICHIER XLSX SUGGÉRÉ ===
      dataset_type <- switch(plot_type,
                             "boxplot_periods" = "Boxplot Periods",
                             "boxplot_cumulate" = "Boxplot Cumulative",
                             "boxplot_delta" = "Boxplot Delta",
                             "lineplot" = "Lineplot",
                             "Unknown")
      xlsx_name <- paste0(dataset_type, "_dataset_", response_var, ".xlsx")
      
      script <- c(script,
                  "# ---- Load Data ----",
                  paste0("df <- readxl::read_excel('", xlsx_name, "')  # Remplacez par votre chemin"),
                  paste0("sub <- subset(df, zone == ", selected_zone, ")"),
                  "sub <- droplevels(sub)\n"
      )
      
      # === COULEURS DES CONDITIONS ===
      if (length(condition_colors) > 0) {
        col_str <- paste0('"', condition_colors, '"', collapse = ", ")
        script <- c(script,
                    "# ---- Condition Colors ----",
                    paste0("condition_colors <- c(", col_str, ")"),
                    "names(condition_colors) <- sort(unique(sub$condition_grouped))\n"
        )
      }
      
      # === THÈME MINIMAL ===
      script <- c(script,
                  "# ---- Minimal Theme ----",
                  "theme_clean <- function() {",
                  "  theme_bw() +",
                  "  theme(",
                  "    plot.title = element_text(size = 14, hjust = 0.5),",
                  paste0("    axis.text = element_text(color = '", edge_col, "', size = 12),"),
                  paste0("    axis.title = element_text(color = '", edge_col, "', size = 12),"),
                  "    axis.title.x = element_text(margin = margin(t = 5, r = 15)),",
                  "    axis.title.y = element_text(angle = 90, margin = margin(r = 10)),",
                  "    legend.position = 'right',",
                  "    legend.text = element_text(size = 12, face = 'italic'),",
                  "    strip.text = element_text(size = 12),",
                  paste0("    strip.background = element_rect(fill = '", if(theme_is_light) "white" else "black", "', colour = '", edge_col, "'),"),
                  paste0("    panel.background = element_rect(fill = '", if(theme_is_light) "white" else "black", "', colour = '", edge_col, "'),"),
                  paste0("    panel.border = element_rect(color = '", edge_col, "', fill = NA),"),
                  "    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10))",
                  "  )",
                  "}",
                  "theme_obj <- theme_clean()\n"
      )
      
      # === PLOT : boxplot_periods ===
      if (plot_type == "boxplot_periods") {
        # Force Light → Dark
        if ("period_without_numbers" %in% names(sub)) {
          levels_present <- unique(sub$period_without_numbers)
          if (all(c("Light", "Dark") %in% levels_present)) {
            script <- c(script,
                        "# ---- Force Facet Order: Light → Dark ----",
                        "sub$period_without_numbers <- factor(sub$period_without_numbers, levels = c('Light', 'Dark'))\n"
            )
          }
        }
        
        if (boxplot_mode == "separated") {
          script <- c(script,
                      "# ---- Boxplot: Separated by Period ----",
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      paste0("  geom_boxplot(aes(fill = condition_grouped), colour = '", edge_col, "') +"),
                      paste0("  geom_sina(aes(text = paste0(",
                             "'Animal: ', animal, '\\n',",
                             "'Plate: ', plate_id, '\\n',",
                             "'Condition: ', condition_grouped, '\\n',",
                             "'Value: ', sprintf('%.2f', mean_val)",
                             ")), maxwidth = 0.25, size = 1.6, alpha = 0.55, colour = '", edge_col, "') +"),
                      "  facet_wrap(~period_without_numbers, scales = 'free_x') +",
                      "  scale_fill_manual(values = condition_colors) +",
                      paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")',"),
                      "       caption = 'Each point = mean value per animal') +",
                      "  theme_obj +",
                      "  theme(legend.position = 'none',",
                      "        axis.text.x = element_text(angle = 45, hjust = 1),",
                      "        axis.title.x = element_blank(),",
                      "        plot.caption = element_text(hjust = 1))"
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
                      "  geom_boxplot(",
                      "    aes(group = interaction(condition_grouped, period_without_numbers), fill = period_without_numbers),",
                      paste0("    colour = '", edge_col, "', position = position_dodge(width = dodge_w)"),
                      "  ) +",
                      paste0("  geom_sina(aes(colour = period_without_numbers, text = paste0(",
                             "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', mean_val)",
                             ")), maxwidth = 0.25, size = 1.8, alpha = 0.55, position = position_dodge(width = dodge_w)) +"),
                      "  scale_fill_manual(values = period_colors, name = 'Period') +",
                      paste0("  scale_colour_manual(values = rep('", edge_col, "', 2), guide = 'none') +"),
                      paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")',"),
                      "       caption = 'Each point = mean value per animal') +",
                      "  theme_obj +",
                      "  theme(legend.position = 'right',",
                      "        axis.text.x = element_text(angle = 45, hjust = 1),",
                      "        axis.title.x = element_blank())"
          )
        }
      }
      
      # === PLOT : boxplot_cumulate ===
      else if (plot_type == "boxplot_cumulate") {
        script <- c(script,
                    "# ---- Boxplot: Cumulative ----",
                    "gg <- ggplot(sub, aes(x = condition_grouped, y = cum)) +",
                    paste0("  geom_boxplot(aes(fill = condition_grouped), colour = '", edge_col, "') +"),
                    paste0("  geom_sina(aes(text = paste0(",
                           "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', cum)",
                           ")), maxwidth = 0.25, size = 1.4, alpha = 0.6, colour = '", edge_col, "') +"),
                    "  scale_fill_manual(values = condition_colors) +",
                    paste0("  labs(y = 'Cumulative ", response_var, " (Zone ", selected_zone, ")',"),
                    "       caption = 'Each point = cumulative value per animal') +",
                    "  theme_obj +",
                    "  theme(legend.position = 'none',",
                    "        axis.text.x = element_text(angle = 45, hjust = 1),",
                    "        axis.title.x = element_blank(),",
                    "        plot.caption = element_text(hjust = 1))"
        )
      }
      
      # === PLOT : boxplot_delta ===
      else if (plot_type == "boxplot_delta") {
        transition <- extra_params$transition %||% "Unknown"
        script <- c(script,
                    paste0("# ---- Boxplot: Delta around '", transition, "' ----")
        )
        
        if (boxplot_mode == "separated") {
          script <- c(script,
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      paste0("  geom_boxplot(aes(fill = condition_grouped), colour = '", edge_col, "') +"),
                      paste0("  geom_sina(aes(text = paste0(",
                             "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', mean_val)",
                             ")), maxwidth = 0.25, size = 1.8, alpha = 0.55, colour = '", edge_col, "') +"),
                      "  facet_wrap(~transition_phase, scales = 'free_x') +",
                      "  scale_fill_manual(values = condition_colors) +",
                      paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")',"),
                      "       caption = 'Each point = mean value around transition') +",
                      "  theme_obj +",
                      "  theme(legend.position = 'none',",
                      "        axis.text.x = element_text(angle = 45, hjust = 1),",
                      "        axis.title.x = element_blank(),",
                      "        plot.caption = element_text(hjust = 1))"
          )
        } else {
          phase_cols <- trimws(strsplit(extra_params$phase_colors %||% "", ",")[[1]])
          phase_cols <- ensure_colors(3, phase_cols)
          col_str <- paste0('"', phase_cols, '"', collapse = ", ")
          
          script <- c(script,
                      paste0("phase_colors <- c(", col_str, ")"),
                      "names(phase_colors) <- c('Before', 'Switch', 'After')",
                      "dodge_w <- 0.75",
                      "gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
                      "  geom_boxplot(",
                      "    aes(group = interaction(condition_grouped, transition_phase), fill = transition_phase),",
                      paste0("    colour = '", edge_col, "', position = position_dodge(width = dodge_w)"),
                      "  ) +",
                      paste0("  geom_sina(aes(colour = transition_phase, text = paste0(",
                             "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', mean_val)",
                             ")), maxwidth = 0.25, size = 1.8, alpha = 0.55, position = position_dodge(width = dodge_w)) +"),
                      "  scale_fill_manual(values = phase_colors, name = 'Phase') +",
                      paste0("  scale_colour_manual(values = rep('", edge_col, "', 3), guide = 'none') +"),
                      paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")',"),
                      "       caption = 'Each point = mean value around transition') +",
                      "  theme_obj +",
                      "  theme(legend.position = 'right',",
                      "        axis.text.x = element_text(angle = 45, hjust = 1),",
                      "        axis.title.x = element_blank())"
          )
        }
      }
      
      # === PLOT : lineplot ===
      else if (plot_type == "lineplot") {
        time_unit <- extra_params$time_unit %||% "seconds"
        error_mode <- extra_params$error_mode %||% "error_bar"
        
        script <- c(script,
                    paste0("# ---- Lineplot: Over Time (", time_unit, ") ----"),
                    "gg <- ggplot(sub, aes(x = start_rounded, y = val_per_well, colour = condition_grouped, group = condition_grouped)) +"
        )
        
        if (error_mode == "error_bar" && "se_per_well" %in% names(sub)) {
          script <- c(script,
                      "  geom_errorbar(aes(ymin = val_per_well - se_per_well, ymax = val_per_well + se_per_well),",
                      "                width = 0.15, linewidth = 0.5, alpha = 0.7) +"
          )
        } else if (error_mode == "ci95" && "se_per_well" %in% names(sub)) {
          script <- c(script,
                      "  geom_ribbon(aes(ymin = val_per_well - 1.96 * se_per_well, ymax = val_per_well + 1.96 * se_per_well, fill = condition_grouped),",
                      "              alpha = 0.2, color = NA) +",
                      "  scale_fill_manual(values = condition_colors) +"
          )
        }
        
        script <- c(script,
                    "  geom_line(linewidth = 0.8) +",
                    paste0("  geom_point(aes(text = paste0(",
                           "'Animal: ', animal, '\\nPlate: ', plate_id, '\\nCondition: ', condition_grouped, '\\nValue: ', sprintf('%.2f', val_per_well)",
                           ")), size = 1.8, alpha = 0.55) +"),
                    "  scale_colour_manual(values = condition_colors) +",
                    paste0("  labs(x = 'Time (", time_unit, ")',"),
                    paste0("  y = '", response_var, " (Zone ", selected_zone, ")',"),
                    "       caption = 'Each line = normalized response per condition') +",
                    "  theme_obj +",
                    "  theme(axis.text.x = element_text(angle = 45, hjust = 1),",
                    "        plot.caption = element_text(hjust = 1, margin = margin(t = 10)))"
        )
      }
      
      # === FIN ===
      script <- c(script,
                  "\n# ---- Display ----",
                  "print(gg)\n",
                  "# ---- Optional: Interactive ----",
                  "# library(plotly)",
                  "# plotly::ggplotly(gg)"
      )
      
      paste(script, collapse = "\n")
    }
    
    # ==================================================================
    # Outputs & orchestration (+ preserve legend selection in HTML)
    # ==================================================================
    output$figure_plot <- shiny::renderUI({
      shiny::req(rv$plot)
      mode_out <- isolate(input$output_mode); if (is.null(mode_out)) mode_out <- "PNG"
      if (toupper(mode_out) == "HTML") plotly::plotlyOutput(ns("plotly_plot"), height = "600px")
      else                              shiny::plotOutput(ns("static_plot"), height = "600px")
    })
    output$plotly_plot <- plotly::renderPlotly({ shiny::req(rv$plot); rv$plot })
    output$static_plot <- shiny::renderPlot({   shiny::req(rv$plot); rv$plot })
    output$console_output <- renderPrint({
      msgs <- console_messages()
      if (length(msgs) == 0 || all(msgs == "Ready.")) {
        cat("No messages yet.")
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
          (isolate(input$plot_type) == "boxplot_delta"    && isolate(input$boxplot_delta_mode)    == "pooled")
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
        "boxplot_periods"   = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
        "boxplot_cumulate"  = rv$all_zone_combined_cum_boxplots[[input$response_var]],
        "boxplot_delta"     = rv$all_zone_combined_delta_boxplots[[input$response_var]],
        "lineplot"          = rv$all_zone_combined_lineplots[[input$response_var]]
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
                             "boxplot_periods"  = input$boxplot_periods_mode,
                             "boxplot_cumulate" = "separated",
                             "boxplot_delta"    = input$boxplot_delta_mode, NULL)
      p <- generate_plot(df, input$response_var, input$plot_type, boxplot_mode,
                         selected_zone, input$theme_switch, oc$order, oc$colors)
      sub <- tryCatch({ subset(df, zone == selected_zone) }, error = function(e) NULL)
      if (!is.null(sub)) sub <- droplevels(sub)
      save_current_state(p, sub = sub)
      if (log_it) log("🖼️ Figure generated.")
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
        "boxplot_periods"   = !is.null(rv$all_zone_combined_light_dark_boxplots[[isolate(input$response_var)]]),
        "boxplot_cumulate"  = !is.null(rv$all_zone_combined_cum_boxplots[[isolate(input$response_var)]]),
        "boxplot_delta"     = !is.null(rv$all_zone_combined_delta_boxplots[[isolate(input$response_var)]]),
        "lineplot"          = !is.null(rv$all_zone_combined_lineplots[[isolate(input$response_var)]]),
        FALSE
      )
      if (isTRUE(df_exists)) make_plot(log_it = FALSE)
    }, ignoreInit = TRUE)
    
    
    # ==================================================================
    # Download R scripts
    # ==================================================================
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
          "boxplot_periods"   = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
          "boxplot_cumulate"  = rv$all_zone_combined_cum_boxplots[[input$response_var]],
          "boxplot_delta"     = rv$all_zone_combined_delta_boxplots[[input$response_var]],
          "lineplot"          = rv$all_zone_combined_lineplots[[input$response_var]]
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
    
    # ==================================================================
    # Data tables & downloads
    # ==================================================================
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
        else if (input$plot_type == "boxplot_delta")  input$boxplot_delta_mode
        else if (input$plot_type == "boxplot_cumulate") "separated"
        else "pooled"
        transition <- if (input$plot_type == "boxplot_delta") paste0("_", input$transition_select) else ""
        ext <- if (toupper(isolate(input$output_mode %||% "PNG")) == "HTML") "html" else "png"
        sprintf("%s_%s_zone%s_%s_%s_%s%s.%s",
                input$plot_type, var, zone, theme, mode, transition, ext)
      },
      content = function(file) {
        shiny::req(rv$plot)
        if (toupper(isolate(input$output_mode %||% "PNG")) == "HTML") {
          htmlwidgets::saveWidget(rv$plot, file, selfcontained = TRUE)
        } else {
          ggplot2::ggsave(file, plot = rv$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$delta_percentage_table <- DT::renderDataTable({
      shiny::req(input$delta_table_type, input$delta_table_var)
      df <- if (input$delta_table_type == "Momentum Comparisons")
        rv$percentage_diff_results_momentum[[input$delta_table_var]]
      else
        rv$percentage_diff_results_condition[[input$delta_table_var]]
      shiny::req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), class = "display") %>%
        DT::formatStyle(
          columns = c("mean_diff_pct", "median_diff_pct"),
          backgroundColor = DT::styleInterval(0, c("#ffb2b2", "#b9ffb2"))
        )
    })
    
    output$download_current_dataset <- shiny::downloadHandler(
      filename = function() sprintf("%s_dataset_%s.xlsx", input$dataset_type, input$dataset_response_var),
      content  = function(file) {
        df <- switch(input$dataset_type,
                     "Boxplot Periods"    = rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]],
                     "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
                     "Boxplot Delta"      = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
                     "Lineplot"           = rv$all_zone_combined_lineplots[[input$dataset_response_var]]
        )
        shiny::req(df); writexl::write_xlsx(df, file)
      }
    )
    
    output$download_current_delta_table <- shiny::downloadHandler(
      filename = function() sprintf("%s_%s.xlsx", input$delta_table_type, input$delta_table_var),
      content  = function(file) {
        if (input$delta_table_type == "Momentum Comparisons") file.copy(rv$delta_momentum_excel,  file)
        else                                                  file.copy(rv$delta_condition_excel, file)
      }
    )
    
    output$download_all_delta_tables <- shiny::downloadHandler(
      filename = function() sprintf("all_delta_tables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content  = function(file) {
        td <- tempdir(); files <- c(rv$delta_momentum_excel, rv$delta_condition_excel)
        zip::zip(file, files = files, root = td)
      },
      contentType = "application/zip"
    )
  })
}
