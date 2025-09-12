# ======================================================================
# 1) Themes
# ======================================================================

light_theme <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.title   = element_text(color = "black", size = 14, hjust = .5),
    axis.text    = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y = element_text(color = "black", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position = "right",
    legend.text  = element_text(color = "black", size = 12, face = "italic"),
    legend.title = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.background = element_rect(fill = "white"),
    plot.caption = element_text(color = "black", size = 8, hjust = 1, margin = margin(t = 10))
  )
}

dark_theme <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(
    plot.title   = element_text(color = "white", size = 14, hjust = .5),
    axis.text    = element_text(color = "white", size = 12),
    axis.title.x = element_text(color = "white", size = 12, margin = margin(t = 5, r = 15)),
    axis.title.y = element_text(color = "white", size = 12, angle = 90, margin = margin(r = 10)),
    legend.position = "right",
    legend.text  = element_text(color = "white", size = 12, face = "italic"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "black"),
    legend.key        = element_rect(fill = "black"),
    strip.text.x      = element_text(color = "white", size = 12),
    strip.background  = element_rect(fill = "black", color = "white"),
    plot.background   = element_rect(fill = "black"),
    panel.background  = element_rect(fill = "black"),
    panel.border      = element_rect(color = "white", fill = NA),
    panel.grid.major  = element_line(color = "grey30"),
    panel.grid.minor  = element_line(color = "grey30"),
    plot.caption      = element_text(color = "white", size = 8, hjust = 1, margin = margin(t = 10))
  )
}

# ======================================================================
# 2) UI
# ======================================================================

visualization_qm_ldm_ui <- function(id) {
  ns <- NS(id)
  cond <- function(x, y) paste0("input['", ns(x), "'] == '", y, "'")
  
  fluidRow(
    # ---- Left: Inputs -------------------------------------------------
    box(
      title = "Visualization Inputs", width = 4,
      
      selectInput(ns("plot_type"), "Plot Type",
                  c("boxplot_light_dark","boxplot_cumulate","boxplot_delta","lineplot"),
                  selected = "boxplot_light_dark"),
      div(style = "margin-bottom:20px;"),
      
      # One unified response variable selector for all plot types
      selectInput(ns("response_var"), "Response Variable", choices = "", selected = ""),
      
      # --- LIGHT/DARK datasets
      conditionalPanel(
        condition = cond("plot_type","boxplot_light_dark"),
        actionButton(ns("generate_light_dark_dfs"), "Generate Light/Dark Datasets"),
        div(style = "margin-bottom:30px;"),
        selectInput(ns("boxplot_light_dark_mode"), "Boxplot Mode", c("separated","pooled"), "separated"),
        conditionalPanel(
          condition = paste(cond("boxplot_light_dark_mode","pooled"),
                            "&&", cond("plot_type","boxplot_light_dark")),
          textInput(ns("boxplot_light_dark_periods_colors"),
                    "Light/Dark Colors (comma-separated hex)", value = "#FFC300,#B3AAAA")
        )
      ),
      
      # --- CUMULATIVE datasets
      conditionalPanel(
        condition = cond("plot_type","boxplot_cumulate"),
        actionButton(ns("generate_cumulate_dfs"), "Generate Cumulative Datasets"),
        div(style = "margin-bottom:30px;")
      ),
      
      # --- DELTA datasets
      conditionalPanel(
        condition = cond("plot_type","boxplot_delta"),
        uiOutput(ns("transition_select_ui")),
        textInput(ns("delta_time"), "Delta Time Window (seconds)", value = "60"),
        actionButton(ns("generate_delta_dfs"), "Generate Delta Datasets"),
        div(style = "margin-bottom:30px;"),
        selectInput(ns("boxplot_delta_mode"), "Boxplot Mode", c("separated","pooled"), "separated"),
        conditionalPanel(
          condition = paste(cond("boxplot_delta_mode","pooled"),
                            "&&", cond("plot_type","boxplot_delta")),
          textInput(ns("boxplot_delta_phase_colors"),
                    "Phase Colors (Before, Switch, After; comma-separated hex)",
                    value = "#FF6F61, #40C4FF, #4CAF50")
        )
      ),
      
      # --- LINEPLOT datasets
      conditionalPanel(
        condition = cond("plot_type","lineplot"),
        selectInput(ns("time_unit_original"), "Original Time Unit",
                    c("seconds","minutes","hours","days"), "seconds"),
        selectInput(ns("time_unit_convert"), "Convert Time Unit?", c("No","Yes"), "No"),
        conditionalPanel(
          condition = cond("time_unit_convert","Yes"),
          selectInput(ns("time_unit_target"), "Target Time Unit",
                      c("seconds","minutes","hours","days"), "minutes")
        ),
        uiOutput(ns("aggregation_period_label")),
        selectInput(ns("lineplot_replicate_mode"), "Lineplot Mode", c("pooled","separated"), "pooled"),
        actionButton(ns("generate_lineplot_dfs"), "Generate Lineplot Datasets"),
        div(style = "margin-bottom:30px;")
      ),
      
      # --- Boxplots fill mode
      conditionalPanel(
        condition = paste(cond("plot_type","boxplot_cumulate"), "||",
                          cond("plot_type","boxplot_light_dark"), "||",
                          cond("plot_type","boxplot_delta")),
        radioButtons(ns("boxplot_fill_mode"), "Boxplot Fill Mode",
                     c("Full"="full","Empty"="empty"), "full", inline = TRUE)
      ),
      
      # --- Global ordering & colors
      textInput(ns("condition_grouped_order"), "Condition Order (comma-separated)",
                value = "", placeholder = "e.g., cond1, cond2, cond3"),
      textInput(ns("condition_grouped_color"), "Condition Colors (comma-separated hex)",
                value = "", placeholder = "e.g., #FF0000, #00FF00, #0000FF"),
      
      # --- Output mode & figure builder
      radioButtons(ns("output_mode"), "Output Mode", c("PNG","HTML"), "HTML", inline = TRUE),
      uiOutput(ns("figure_selector")),
      div(
        style = "display:flex; flex-direction:column; gap:10px;",
        actionButton(ns("generate_figure"), "Generate Figure", style = "width:100%;"),
        conditionalPanel(
          condition = cond("plot_type","boxplot_delta"),
          actionButton(ns("generate_delta_tables"), "Generate Delta Percentage Tables", style = "width:100%;")
        )
      )
    ),
    
    # ---- Right: Output panel -----------------------------------------
    box(
      title = "Visualization Output", width = 8,
      div(style = "margin-bottom:10px;",
          actionButton(ns("clear_console"), "Clear Console", icon = icon("trash"))),
      tabsetPanel(
        id = ns("output_tabs"),
        
        tabPanel("Interactive Figure",
                 uiOutput(ns("figure_plot")),
                 div(style="margin-top:10px;"),
                 radioButtons(ns("theme_switch"), "Theme", c("Light","Dark"), "Light", inline = TRUE),
                 div(style="margin-top:10px;"),
                 downloadButton(ns("save_current_figure"), "Save Current Figure")),
        
        tabPanel("Datasets",
                 selectInput(ns("dataset_type"), "Dataset Type",
                             c("Boxplot Light/Dark","Boxplot Cumulative","Boxplot Delta","Lineplot"),
                             selected = "Boxplot Light/Dark"),
                 selectInput(ns("dataset_response_var"), "Response Variable", choices = "", selected = ""),
                 DT::dataTableOutput(ns("dataset_table")),
                 div(style="margin-top:10px; margin-bottom:10px;",
                     downloadButton(ns("download_current_dataset"), "Download Current Dataset (.xlsx)"))),
        
        tabPanel("Console Output",
                 div(style="background-color:#f5f5f5;border:1px solid #ccc;padding:10px;height:600px;overflow-y:auto;font-family:monospace;",
                     uiOutput(ns("console_output")))),
        
        tabPanel("Delta Percentage Tables", value = "delta_percentage_tables",
                 selectInput(ns("delta_table_type"), "Table Type",
                             c("Momentum Comparisons","Condition Comparisons"), "Momentum Comparisons"),
                 selectInput(ns("delta_table_var"), "Response Variable", choices = "", selected = ""),
                 DT::dataTableOutput(ns("delta_percentage_table")),
                 div(style="margin-top:10px; margin-bottom:10px;",
                     downloadButton(ns("download_current_delta_table"), "Download Current Table (.xlsx)"),
                     downloadButton(ns("download_all_delta_tables"), "Download All Delta Tables (.zip)")))
      )
    )
  )
}

# ======================================================================
# 3) Server
# ======================================================================

visualization_qm_ldm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Expected QM/LDM variables (single source of truth) ----------
    QMLDM_EXPECTED <- c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg")
    
    # ---- Console helpers ---------------------------------------------
    console_messages <- reactiveVal("👋 Ready.")
    log <- function(...) console_messages(c(console_messages(), paste(...)))
    ensure_directory <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE)
    
    # ==================================================================
    # 3.1) Small helpers (shared)
    # ==================================================================
    
    get_boundaries_list <- function() {
      if (is.null(rv$processing_results)) return(NULL)
      k <- names(rv$processing_results)
      k <- k[tolower(k) == "boundary_associations_list"]
      if (length(k)) rv$processing_results[[k[1]]] else NULL
    }
    
    convert_time <- function(x, from, to) {
      if (from == to) return(x)
      f <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)
      x * f[[from]] / f[[to]]
    }
    
    get_df <- function() switch(
      input$plot_type,
      "boxplot_light_dark" = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
      "boxplot_cumulate"   = rv$all_zone_combined_cum_boxplots[[input$response_var]],
      "boxplot_delta"      = rv$all_zone_combined_delta_boxplots[[input$response_var]],
      "lineplot"           = rv$all_zone_combined_lineplots[[input$response_var]]
    )
    
    group_var <- function() {
      if (input$plot_type == "lineplot" && input$lineplot_replicate_mode == "separated")
        "condition" else "condition_grouped"
    }
    
    order_and_colors <- function(df) {
      gvar <- group_var()
      ord  <- if (nzchar(input$condition_grouped_order))
        trimws(strsplit(input$condition_grouped_order, ",")[[1]])
      else unique(df[[gvar]])
      cols <- if (nzchar(input$condition_grouped_color))
        trimws(strsplit(input$condition_grouped_color, ",")[[1]])
      else RColorBrewer::brewer.pal(length(unique(df[[gvar]])), "Set1")
      list(order = ord, colors = cols)
    }
    
    as_widget <- function(p) {
      if (input$output_mode != "HTML") return(p)
      pooled <- (input$plot_type == "boxplot_light_dark" && input$boxplot_light_dark_mode == "pooled") ||
        (input$plot_type == "boxplot_delta"      && input$boxplot_delta_mode      == "pooled")
      w <- plotly::ggplotly(p, tooltip = "text")
      if (pooled) w <- plotly::layout(w, boxmode = "group")
      w
    }
    
    save_current_state <- function(p) {
      mode <- if (input$plot_type == "boxplot_light_dark") input$boxplot_light_dark_mode
      else if (input$plot_type == "boxplot_delta")  input$boxplot_delta_mode
      else if (input$plot_type == "boxplot_cumulate") "separated"
      else input$lineplot_replicate_mode
      key <- paste(input$response_var, input$selected_zone, tolower(input$theme_switch),
                   mode, input$boxplot_fill_mode, input$output_mode,
                   if (input$plot_type == "boxplot_delta") input$transition_select else "", sep = "_")
      rv$generated_figures[[key]] <- list(plot = p)
      rv$plot <- as_widget(p)
    }
    
    validate_num_pos <- function(x, msg) {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v) || v <= 0) {
        showModal(modalDialog(title = "Warning", msg, easyClose = TRUE))
        return(FALSE)
      }
      TRUE
    }
    
    validate_transition <- function(transition, boundaries, all_zone_combined) {
      if (is.null(boundaries) || !nrow(boundaries)) return(FALSE)
      tt <- boundaries$time_switch[boundaries$transition == transition]
      if (!length(tt) || !any(abs(all_zone_combined$start - tt) < 1)) {
        showModal(modalDialog(
          title = "Invalid Transition",
          sprintf("The selected transition '%s' has no corresponding timestamp in the data.", transition),
          easyClose = TRUE
        ))
        return(FALSE)
      }
      TRUE
    }
    
    # Safe and unified menu updater for response variables
    update_response_choices <- function(vars) {
      safe_sel <- function(x, vars, default = "")
        if (!is.null(x) && length(x) == 1 && isTRUE(x %in% vars)) x else default
      rv_sel <- safe_sel(isolate(input$response_var), vars, "")
      ds_sel <- safe_sel(isolate(input$dataset_response_var), vars, "")
      dt_def <- if ("totaldist" %in% vars) "totaldist" else vars[1]
      dt_sel <- safe_sel(isolate(input$delta_table_var), vars, dt_def)
      updateSelectInput(session, "response_var",         choices = c("", vars), selected = rv_sel)
      updateSelectInput(session, "dataset_response_var", choices = c("", vars), selected = ds_sel)
      updateSelectInput(session, "delta_table_var",      choices = c("", vars), selected = dt_sel)
    }
    
    # ==================================================================
    # 3.2) UI-Reactive bits
    # ==================================================================
    
    observeEvent(input$clear_console, {
      console_messages("👻 No messages yet.")
      showNotification("Console cleared!", type = "message")
    })
    
    output$aggregation_period_label <- renderUI({
      unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
      textInput(ns("aggregation_period"), sprintf("Aggregation Period (in %s)", unit), value = "60")
    })
    
    output$transition_select_ui <- renderUI({
      req(rv$processing_results, get_boundaries_list())
      b <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
      tr <- unique(b$transition)
      if (!length(tr)) return(div("No transitions available. Please run processing first."))
      selectInput(ns("transition_select"), "Select Transition", choices = tr, selected = tr[1])
    })
    
    # ==================================================================
    # 3.3) Data preparation
    # ==================================================================
    
    prepare_all_zone <- function() {
      req(rv$processing_results, "processed_data_list" %in% names(rv$processing_results))
      rv$processed_data_list <- purrr::map(
        rv$processing_results$processed_data_list,
        ~ dplyr::mutate(.x, plate_id = as.character(plate_id))
      )
      rv$all_zone_combined <- dplyr::bind_rows(rv$processed_data_list)
      rv$all_zone_combined
    }
    
    # Initialize menus once UI is fully flushed
    session$onFlushed(function() update_response_choices(QMLDM_EXPECTED), once = TRUE)
    # Re-sync menus after dataset generation
    observeEvent(
      list(input$generate_light_dark_dfs, input$generate_cumulate_dfs,
           input$generate_delta_dfs,       input$generate_lineplot_dfs),
      { update_response_choices(QMLDM_EXPECTED) },
      ignoreInit = TRUE
    )
    
    # ==================================================================
    # 3.4) Dataset generators
    # ==================================================================
    
    # -- Light/Dark
    observeEvent(input$generate_light_dark_dfs, {
      tryCatch({
        az <- prepare_all_zone()
        periods      <- unique(az$period_without_numbers)
        light_period <- grep("light", periods, ignore.case = TRUE, value = TRUE)
        dark_period  <- grep("dark",  periods, ignore.case = TRUE, value = TRUE)
        
        calc <- function(v) az %>%
          dplyr::filter(period_without_numbers %in% c(light_period, dark_period)) %>%
          dplyr::group_by(period_without_numbers, zone, condition_tagged, plate_id) %>%
          dplyr::summarise(
            plate_id            = dplyr::first(plate_id),
            start               = dplyr::first(start),
            period_with_numbers = dplyr::first(period_with_numbers),
            condition_grouped   = dplyr::first(condition_grouped),
            condition           = dplyr::first(condition),
            animal              = dplyr::first(animal),
            mean_val            = mean(.data[[v]], na.rm = TRUE),
            .groups = "drop"
          )
        
        rv$all_zone_combined_light_dark_boxplots <- setNames(lapply(QMLDM_EXPECTED, calc), QMLDM_EXPECTED)
        log("✅ Light/Dark datasets created.")
      }, error = function(e) log(paste("❌ Light/Dark generation failed:", e$message)))
    })
    
    # -- Cumulative
    observeEvent(input$generate_cumulate_dfs, {
      tryCatch({
        az <- prepare_all_zone()
        calc <- function(v) az %>%
          dplyr::group_by(condition_grouped, zone, plate_id, animal) %>%
          dplyr::summarise(cum = sum(.data[[v]], na.rm = TRUE),
                           condition_tagged = dplyr::first(condition_tagged), .groups = "drop")
        rv$all_zone_combined_cum_boxplots <- setNames(lapply(QMLDM_EXPECTED, calc), QMLDM_EXPECTED)
        log("✅ Cumulative datasets created.")
      }, error = function(e) log(paste("❌ Cumulative generation failed:", e$message)))
    })
    
    # -- Delta (around transitions)
    observeEvent(input$generate_delta_dfs, {
      tryCatch({
        req("boundary_associations_list" %in% names(rv$processing_results))
        az <- prepare_all_zone()
        b  <- dplyr::bind_rows(get_boundaries_list()) %>% dplyr::distinct()
        if (!nrow(b)) return(log("⚠️ No transitions found."))
        if (!validate_num_pos(input$delta_time, "Delta time window must be a positive number.")) return()
        if (!validate_transition(input$transition_select, b, az)) return()
        
        b_clean <- b %>%
          dplyr::mutate(plate_id = as.character(plate_id)) %>%
          dplyr::distinct(transition, plate_id, time_switch, .keep_all = TRUE)
        
        delta <- as.numeric(input$delta_time)
        tr    <- input$transition_select
        bd_sel <- b_clean %>% dplyr::filter(transition == tr) %>% dplyr::select(plate_id, time_switch)
        
        joined <- az %>%
          dplyr::mutate(plate_id = as.character(plate_id)) %>%
          dplyr::inner_join(bd_sel, by = "plate_id") %>%
          dplyr::mutate(
            phase_raw = dplyr::case_when(
              start >= time_switch - delta & start <  time_switch            ~ "before",
              start >= time_switch          & start <  time_switch + delta   ~ "switch",
              start >= time_switch + delta  & start <  time_switch + 2*delta ~ "after",
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::filter(!is.na(phase_raw)) %>%
          dplyr::mutate(transition_phase = paste0(tr, "_", phase_raw))
        
        if (!nrow(joined)) return(log("⚠️ No data in requested delta windows."))
        
        phased_long <- tidyr::pivot_longer(
          joined, cols = tidyselect::all_of(QMLDM_EXPECTED),
          names_to = "variable", values_to = "value"
        ) %>%
          dplyr::group_by(transition_phase, zone, condition_tagged, plate_id, animal, variable) %>%
          dplyr::summarise(
            mean_val = mean(value, na.rm = TRUE),
            condition_grouped = dplyr::first(condition_grouped),
            .groups = "drop"
          )
        
        rv$all_zone_combined_delta_boxplots <- split(phased_long, phased_long$variable)
        log(sprintf("✅ Delta datasets created for transition '%s' (±%ss).", tr, delta))
      }, error = function(e) log(paste("❌ Delta generation failed:", e$message)))
    })
    
    # -- Lineplot (normalized per well)
    observeEvent(input$generate_lineplot_dfs, {
      tryCatch({
        az <- prepare_all_zone()
        compute_wells <- function(df) {
          gv <- if (input$lineplot_replicate_mode == "pooled") "condition_grouped" else "condition"
          df %>%
            dplyr::group_by(.data[[gv]], zone, plate_id) %>%
            dplyr::summarise(n_wells_plate = dplyr::n_distinct(animal), .groups = "drop") %>%
            dplyr::group_by(.data[[gv]], zone) %>%
            dplyr::summarise(n_wells = sum(n_wells_plate), .groups = "drop")
        }
        wells <- compute_wells(az)
        rv$wells_per_condition <- wells
        
        if (!validate_num_pos(input$aggregation_period, "Aggregation period must be a positive number.")) return()
        
        calc <- function(v) {
          agg_unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
          agg_s    <- convert_time(as.numeric(input$aggregation_period), agg_unit, "seconds")
          gv <- if (input$lineplot_replicate_mode == "pooled") "condition_grouped" else "condition"
          az %>%
            dplyr::mutate(start_rounded = floor(start / agg_s) * agg_s) %>%
            dplyr::group_by(.data[[gv]], zone, start_rounded, animal) %>%
            dplyr::summarise(var_value = sum(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
            dplyr::group_by(.data[[gv]], zone, start_rounded) %>%
            dplyr::summarise(total_val = sum(var_value, na.rm = TRUE), .groups = "drop") %>%
            dplyr::left_join(wells, by = c(gv, "zone")) %>%
            dplyr::mutate(
              val_per_well = total_val / n_wells,
              start_rounded = if (input$time_unit_original != agg_unit)
                convert_time(start_rounded, "seconds", agg_unit) else start_rounded
            )
        }
        
        rv$all_zone_combined_lineplots <- setNames(lapply(QMLDM_EXPECTED, calc), QMLDM_EXPECTED)
        log("✅ Lineplot datasets created (normalized per well).")
      }, error = function(e) log(paste("❌ Lineplot generation failed:", e$message)))
    })
    
    # ==================================================================
    # 3.5) Data Table Outputs
    # ==================================================================
    
    output$dataset_table <- DT::renderDataTable({
      req(input$dataset_type, input$dataset_response_var)
      df <- switch(input$dataset_type,
                   "Boxplot Light/Dark" = rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]],
                   "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
                   "Boxplot Delta"      = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
                   "Lineplot"           = rv$all_zone_combined_lineplots[[input$dataset_response_var]]
      )
      req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$figure_selector <- renderUI({
      req(input$plot_type, input$response_var)
      df <- get_df()
      if (is.null(df)) return(helpText("Generate datasets first (click the button above)."))
      zones <- sort(unique(df$zone))
      choices <- setNames(as.character(zones), paste("Zone", zones))
      selectInput(ns("selected_zone"), "Select Zone", choices = choices, selected = choices[1])
    })
    
    # ==================================================================
    # 3.6) Plot factory + build orchestration
    # ==================================================================
    
    generate_plot <- function(df, response_var, plot_type, boxplot_mode, lineplot_mode,
                              selected_zone, theme_choice, condition_order, condition_colors) {
      sub <- subset(df, zone == selected_zone)
      theme_obj <- if (tolower(theme_choice) == "light") light_theme() else dark_theme()
      edge_col  <- if (tolower(theme_choice) == "light") "black" else "white"
      alpha_val <- if (input$boxplot_fill_mode == "full") 0.6 else 1
      
      cap_box_ld <- stringr::str_wrap("Each point corresponds to the mean value for one animal.", 60)
      cap_box_cu <- stringr::str_wrap("Each point corresponds to the cumulative value for one animal.", 60)
      cap_box_de <- stringr::str_wrap("Each point is the mean for one animal around a transition.", 60)
      cap_line   <- stringr::str_wrap("Each line is the normalized response per condition over time.", 60)
      
      box_layer <- function(mapping) {
        if (input$boxplot_fill_mode == "full") {
          geom_boxplot(mapping = mapping, width = 0.7, outlier.shape = NA,
                       alpha = alpha_val, color = edge_col)
        } else {
          geom_boxplot(mapping = modifyList(mapping, aes(fill = NULL)), width = 0.7,
                       outlier.shape = NA, fill = NA, alpha = alpha_val, color = edge_col)
        }
      }
      
      if (plot_type == "boxplot_light_dark") {
        sub$period_without_numbers <- factor(
          sub$period_without_numbers, levels = c("light","dark"),
          labels = c("Light period","Dark period")
        )
        
        if (boxplot_mode == "separated") {
          gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +
            box_layer(aes(group = condition_grouped, fill = condition_grouped)) +
            geom_jitter(
              aes(fill = condition_grouped,
                  text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal,
                                "<br>Plate ID: ", plate_id, "<br>Value: ", sprintf("%.2f", mean_val))),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
              size = 1.75, alpha = 0.4, shape = 21, color = edge_col
            ) +
            facet_wrap(~period_without_numbers, scales = "free_x") +
            scale_fill_manual(values = condition_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), caption = cap_box_ld) +
            theme_obj +
            theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(), plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 1))
          return(gg)
        } else {
          # Pooled
          sub <- tidyr::complete(sub, condition_grouped, period_without_numbers, fill = list(mean_val = NA))
          per_cols <- trimws(strsplit(input$boxplot_light_dark_periods_colors, ",")[[1]])
          
          gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +
            box_layer(aes(group = interaction(condition_grouped, period_without_numbers),
                          fill  = period_without_numbers)) +
            geom_jitter(
              aes(fill = period_without_numbers,
                  text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal,
                                "<br>Plate ID: ", plate_id, "<br>Value: ", sprintf("%.2f", mean_val))),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
              shape = 21, color = edge_col, size = 1.75, alpha = 0.4
            ) +
            scale_fill_manual(values = per_cols) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), fill = "Period", caption = cap_box_ld) +
            theme_obj +
            theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(), plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 1))
          return(gg)
        }
      }
      
      if (plot_type == "boxplot_cumulate") {
        n_animals <- sub %>%
          dplyr::group_by(condition_grouped, zone, plate_id) %>%
          dplyr::summarise(n_animals_plate = dplyr::n_distinct(animal), .groups = "drop") %>%
          dplyr::group_by(condition_grouped, zone) %>%
          dplyr::summarise(n = sum(n_animals_plate), .groups = "drop") %>%
          dplyr::mutate(y = -Inf)
        
        gg <- ggplot(sub, aes(x = condition_grouped, y = cum)) +
          box_layer(aes(group = condition_grouped, fill = condition_grouped)) +
          geom_jitter(
            aes(fill = condition_grouped,
                text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal,
                              "<br>Plate ID: ", plate_id, "<br>Value: ", sprintf("%.2f", cum))),
            width = 0.2, size = 1.5, alpha = 0.6, shape = 21, color = edge_col
          ) +
          geom_text(data = n_animals, aes(x = condition_grouped, y = y, label = paste0("n=", n)),
                    inherit.aes = FALSE, vjust = -0.5, size = 3, color = edge_col) +
          scale_fill_manual(values = condition_colors) +
          labs(y = sprintf("Cumulative %s (Zone %s)", response_var, selected_zone), caption = cap_box_cu) +
          theme_obj +
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title.x = element_blank(), plot.caption.position = "plot",
                plot.caption = element_text(hjust = 1))
        return(gg)
      }
      
      if (plot_type == "boxplot_delta") {
        tr <- input$transition_select
        sub <- dplyr::filter(sub, grepl(tr, transition_phase))
        sub$phase <- factor(
          sub$transition_phase,
          levels = paste0(tr, "_", c("before","switch","after")),
          labels = c("Before","Switch","After"),
          ordered = TRUE
        )
        
        if (boxplot_mode == "separated") {
          gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +
            box_layer(aes(group = condition_grouped, fill = condition_grouped)) +
            geom_jitter(
              aes(fill = condition_grouped,
                  text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal,
                                "<br>Plate ID: ", plate_id, "<br>Phase: ", phase,
                                "<br>Value: ", sprintf("%.2f", mean_val))),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2),
              size = 1.75, alpha = 0.4, shape = 21, color = edge_col
            ) +
            facet_wrap(~phase, scales = "free_x") +
            scale_fill_manual(values = condition_colors) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), caption = cap_box_de) +
            theme_obj +
            theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(), plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 1))
          return(gg)
        } else {
          sub <- tidyr::complete(sub, condition_grouped, transition_phase, fill = list(mean_val = NA))
          sub$transition_phase <- factor(
            sub$transition_phase,
            levels = paste0(tr, "_", c("before","switch","after")),
            labels = c("Before","Switch","After"),
            ordered = TRUE
          )
          phase_cols <- trimws(strsplit(input$boxplot_delta_phase_colors, ",")[[1]])
          
          gg <- ggplot(sub, aes(x = condition_grouped, y = mean_val)) +
            box_layer(aes(group = interaction(condition_grouped, transition_phase),
                          fill = transition_phase)) +
            geom_jitter(
              aes(fill = transition_phase,
                  text = paste0("Tag: ", condition_tagged, "<br>Well: ", animal,
                                "<br>Plate ID: ", plate_id, "<br>Phase: ", transition_phase,
                                "<br>Value: ", sprintf("%.2f", mean_val))),
              position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
              shape = 21, color = edge_col, size = 1.75, alpha = 0.4
            ) +
            scale_fill_manual(values = phase_cols) +
            labs(y = sprintf("%s (Zone %s)", response_var, selected_zone), fill = "Phase", caption = cap_box_de) +
            theme_obj +
            theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(), plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 1))
          return(gg)
        }
      }
      
      if (plot_type == "lineplot") {
        gvar <- if (lineplot_mode == "pooled") "condition_grouped" else "condition"
        time_label <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
        sub[[gvar]] <- factor(sub[[gvar]], levels = condition_order)
        
        gg <- ggplot(sub, aes(x = start_rounded, y = val_per_well,
                              color = .data[[gvar]], group = .data[[gvar]],
                              text = paste0("Tag: ", .data[[gvar]], "<br>Time: ", sprintf("%.2f", start_rounded),
                                            "<br>Total wells: ", n_wells, "<br>Value: ", sprintf("%.2f", val_per_well)))) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.75) +
          scale_color_manual(values = condition_colors, breaks = condition_order) +
          labs(x = sprintf("Time (%s)", time_label),
               y = sprintf("%s (Zone %s)", response_var, selected_zone),
               caption = cap_line,
               color = if (lineplot_mode == "pooled") "Condition Grouped" else "Condition") +
          theme_obj +
          theme(plot.caption.position = "plot",
                plot.caption = element_text(hjust = 1, margin = margin(t = 10)),
                axis.text.x = element_text(angle = 45, hjust = 1))
        return(gg)
      }
    }
    
    # Rebuild when output mode changes (to keep PNG/HTML in sync)
    observeEvent(input$output_mode, {
      req(input$plot_type, input$selected_zone, input$response_var)
      df <- get_df(); req(df)
      oc <- order_and_colors(df)
      boxmode <- if (input$plot_type == "boxplot_light_dark") input$boxplot_light_dark_mode
      else if (input$plot_type == "boxplot_delta")  input$boxplot_delta_mode
      else "separated"
      p <- generate_plot(df, input$response_var, input$plot_type, boxmode,
                         input$lineplot_replicate_mode, input$selected_zone, input$theme_switch,
                         oc$order, oc$colors)
      save_current_state(p)
      log(sprintf("🔁 Output mode switched to %s.", input$output_mode))
    })
    
    # One entry point to build a figure
    make_plot <- function(log_it = FALSE) {
      df <- get_df()
      if (is.null(df) || is.null(input$response_var) || input$response_var == "") {
        if (log_it) log("⚠️ Select a response variable and generate datasets first.")
        return(invisible(NULL))
      }
      selected_zone <- input$selected_zone
      if (is.null(selected_zone) || !length(selected_zone)) {
        zones <- sort(unique(df$zone))
        if (!length(zones)) {
          if (log_it) log("⚠️ No zones available in current dataset.")
          return(invisible(NULL))
        }
        selected_zone <- as.character(zones[1])
      }
      oc <- order_and_colors(df)
      p <- generate_plot(
        df = df,
        response_var = input$response_var,
        plot_type = input$plot_type,
        boxplot_mode = if (input$plot_type == "boxplot_light_dark") input$boxplot_light_dark_mode
        else if (input$plot_type == "boxplot_delta")  input$boxplot_delta_mode
        else "separated",
        lineplot_mode = input$lineplot_replicate_mode,
        selected_zone = selected_zone,
        theme_choice = input$theme_switch,
        condition_order = oc$order,
        condition_colors = oc$colors
      )
      save_current_state(p)
      if (log_it) log("🖼️ Figure generated.")
      invisible(TRUE)
    }
    
    observeEvent(input$generate_figure, { make_plot(log_it = TRUE) })
    
    # Auto-refresh (no noisy logging)
    observeEvent(list(
      input$theme_switch, input$boxplot_fill_mode, input$output_mode,
      input$boxplot_light_dark_mode, input$boxplot_delta_mode,
      input$response_var, input$selected_zone, input$lineplot_replicate_mode
    ), {
      if (!is.null(get_df())) make_plot(log_it = FALSE)
    }, ignoreInit = TRUE)
    
    # ==================================================================
    # 3.7) Outputs (figure & console)
    # ==================================================================
    
    output$figure_plot <- renderUI({
      req(rv$plot)
      if (input$output_mode == "HTML") plotlyOutput(ns("plotly_plot"), height = "600px")
      else                              plotOutput(ns("static_plot"), height = "600px")
    })
    output$plotly_plot <- renderPlotly({ req(rv$plot, input$output_mode == "HTML"); rv$plot })
    output$static_plot <- renderPlot({   req(rv$plot, input$output_mode == "PNG");  rv$plot })
    output$console_output <- renderUI({ HTML(paste(console_messages(), collapse = "<br>")) })
    
    # ==================================================================
    # 3.8) Downloads (figure)
    # ==================================================================
    
    output$save_current_figure <- downloadHandler(
      filename = function() {
        var <- input$response_var; zone <- input$selected_zone; theme <- tolower(input$theme_switch)
        mode <- if (input$plot_type == "boxplot_light_dark") input$boxplot_light_dark_mode
        else if (input$plot_type == "boxplot_delta")  input$boxplot_delta_mode
        else if (input$plot_type == "boxplot_cumulate") "separated"
        else input$lineplot_replicate_mode
        fill_mode <- input$boxplot_fill_mode
        transition <- if (input$plot_type == "boxplot_delta") paste0("_", input$transition_select) else ""
        ext <- if (input$output_mode == "HTML") "html" else "png"
        sprintf("%s_%s_zone%s_%s_%s_%s%s.%s",
                input$plot_type, var, zone, theme, mode, fill_mode, transition, ext)
      },
      content = function(file) {
        req(rv$plot)
        if (input$output_mode == "HTML") {
          htmlwidgets::saveWidget(rv$plot, file, selfcontained = TRUE)
        } else {
          ggplot2::ggsave(file, plot = rv$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    # ==================================================================
    # 3.9) Delta percentage tables (build + render + download)
    # ==================================================================
    
    observeEvent(input$generate_delta_tables, {
      log("🔄 Generating delta percentage tables…")
      tryCatch({
        req(rv$all_zone_combined_delta_boxplots)
        
        build_momentum <- function(df) {
          df %>%
            dplyr::mutate(momentum = sub(".*_(before|switch|after)$", "\\1", transition_phase)) %>%
            dplyr::group_by(condition_grouped, zone) %>%
            tidyr::nest() %>%
            dplyr::mutate(comparison_results = purrr::map(data, function(d) {
              pairs <- list(c("before", "switch"), c("switch", "after"), c("before", "after"))
              purrr::map_dfr(pairs, function(p) {
                m1 <- dplyr::filter(d, momentum == p[1]); m2 <- dplyr::filter(d, momentum == p[2])
                if (nrow(m1) * nrow(m2) == 0) return(tibble())
                tibble(
                  momentum_comparison = paste(p, collapse = "-"),
                  mean_value_1   = round(mean(m1$mean_val, na.rm = TRUE), 2),
                  mean_value_2   = round(mean(m2$mean_val, na.rm = TRUE), 2),
                  median_value_1 = round(median(m1$mean_val, na.rm = TRUE), 2),
                  median_value_2 = round(median(m2$mean_val, na.rm = TRUE), 2),
                  mean_diff_pct   = round((mean_value_2 - mean_value_1) / abs(mean_value_1) * 100, 2),
                  median_diff_pct = round((median_value_2 - median_value_1) / abs(median_value_1) * 100, 2)
                )
              })
            })) %>%
            tidyr::unnest(comparison_results) %>%
            dplyr::select(-data)
        }
        
        build_condition <- function(df) {
          df %>%
            dplyr::mutate(momentum = sub(".*_(before|switch|after)$", "\\1", transition_phase)) %>%
            dplyr::group_by(momentum, zone) %>%
            tidyr::nest() %>%
            dplyr::mutate(comparison_results = purrr::map(data, function(d) {
              pairs <- combn(unique(d$condition_grouped), 2, simplify = FALSE)
              purrr::map_dfr(pairs, function(p) {
                c1 <- dplyr::filter(d, condition_grouped == p[1])
                c2 <- dplyr::filter(d, condition_grouped == p[2])
                if (nrow(c1) * nrow(c2) == 0) return(tibble())
                tibble(
                  condition_comparison = paste(p, collapse = "-"),
                  mean_value_1   = round(mean(c1$mean_val, na.rm = TRUE), 2),
                  mean_value_2   = round(mean(c2$mean_val, na.rm = TRUE), 2),
                  median_value_1 = round(median(c1$mean_val, na.rm = TRUE), 2),
                  median_value_2 = round(median(c2$mean_val, na.rm = TRUE), 2),
                  mean_diff_pct   = round((mean_value_2 - mean_value_1) / abs(mean_value_1) * 100, 2),
                  median_diff_pct = round((median_value_2 - median_value_1) / abs(median_value_1) * 100, 2)
                )
              })
            })) %>%
            tidyr::unnest(comparison_results) %>%
            dplyr::select(-data)
        }
        
        rv$percentage_diff_results_momentum  <- purrr::imap(rv$all_zone_combined_delta_boxplots, ~build_momentum(.x))
        rv$percentage_diff_results_condition <- purrr::imap(rv$all_zone_combined_delta_boxplots, ~build_condition(.x))
        
        # Save two Excel files (one per family)
        out_dir <- file.path(tempdir(), "excel"); ensure_directory(out_dir)
        
        wb1 <- openxlsx::createWorkbook()
        for (nm in names(rv$percentage_diff_results_momentum)) {
          openxlsx::addWorksheet(wb1, nm)
          openxlsx::writeData(wb1, nm, rv$percentage_diff_results_momentum[[nm]])
        }
        rv$delta_momentum_excel <- file.path(out_dir, "delta_percentage_differences_momentum.xlsx")
        openxlsx::saveWorkbook(wb1, rv$delta_momentum_excel, overwrite = TRUE)
        
        wb2 <- openxlsx::createWorkbook()
        for (nm in names(rv$percentage_diff_results_condition)) {
          openxlsx::addWorksheet(wb2, nm)
          openxlsx::writeData(wb2, nm, rv$percentage_diff_results_condition[[nm]])
        }
        rv$delta_condition_excel <- file.path(out_dir, "delta_percentage_differences_condition.xlsx")
        openxlsx::saveWorkbook(wb2, rv$delta_condition_excel, overwrite = TRUE)
        
        log("🎉 Delta percentage tables generated (and saved).")
      }, error = function(e) log(paste("❌ Delta tables generation failed:", e$message)))
    })
    
    output$delta_percentage_table <- DT::renderDataTable({
      req(input$delta_table_type, input$delta_table_var)
      df <- if (input$delta_table_type == "Momentum Comparisons")
        rv$percentage_diff_results_momentum[[input$delta_table_var]]
      else
        rv$percentage_diff_results_condition[[input$delta_table_var]]
      req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), class = "display") %>%
        DT::formatStyle(
          columns = c("mean_diff_pct", "median_diff_pct"),
          backgroundColor = DT::styleInterval(0, c("#ffb2b2", "#b9ffb2"))
        )
    })
    
    # ==================================================================
    # 3.10) Downloads (datasets & tables)
    # ==================================================================
    
    output$download_current_dataset <- downloadHandler(
      filename = function() sprintf("%s_dataset_%s.xlsx", input$dataset_type, input$dataset_response_var),
      content  = function(file) {
        df <- switch(input$dataset_type,
                     "Boxplot Light/Dark" = rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]],
                     "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
                     "Boxplot Delta"      = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
                     "Lineplot"           = rv$all_zone_combined_lineplots[[input$dataset_response_var]]
        )
        req(df)
        writexl::write_xlsx(df, file)
      }
    )
    
    output$download_current_delta_table <- downloadHandler(
      filename = function() sprintf("%s_%s.xlsx", input$delta_table_type, input$delta_table_var),
      content  = function(file) {
        if (input$delta_table_type == "Momentum Comparisons") {
          file.copy(rv$delta_momentum_excel,  file)
        } else {
          file.copy(rv$delta_condition_excel, file)
        }
      }
    )
    
    output$download_all_delta_tables <- downloadHandler(
      filename = function() sprintf("all_delta_tables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content  = function(file) {
        td <- tempdir()
        files <- c(rv$delta_momentum_excel, rv$delta_condition_excel)
        zip::zip(file, files = files, root = td)
      },
      contentType = "application/zip"
    )
  })
}
