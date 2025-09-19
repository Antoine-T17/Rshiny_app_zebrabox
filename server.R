# ======================================================================
# server.R
# Server logic (runs once per user session)
# ======================================================================

server <- function(input, output, session) {
  
  # ------------------------------------------------------------------
  # Reactive Values (per-session global store)
  # ------------------------------------------------------------------
  rv <- shiny::reactiveValues(
    plate_plan_df_list                   = NULL,
    raw_data_list                        = NULL,
    enriched_data_list                   = NULL,
    zone_calculated_list                 = NULL,
    all_zone_combined_df                 = NULL,
    all_zone_combined_lineplots          = NULL,
    all_zone_combined_light_dark_boxplots = NULL,
    all_zone_combined_cum_boxplots       = NULL,
    all_zone_combined_delta_boxplots     = NULL,
    plot                                 = NULL,
    primary_mode                         = NULL,
    secondary_mode                       = NULL,
    generated_figures                    = list(),
    # track started visualization servers by mode (named logical)
    visualization_started                = list()
  )
  
  # ------------------------------------------------------------------
  # Always-Loaded Modules (created once per session)
  # ------------------------------------------------------------------
  plate_plan_server("plate_plan", rv)
  raw_data_server("raw_data", rv)
  
  # ------------------------------------------------------------------
  # Helper Reactive: Build Mode Identifier
  # ------------------------------------------------------------------
  get_mode <- shiny::reactive({
    # treat NULL or empty strings as "no selection"
    is_missing <- function(x) is.null(x) || (is.character(x) && nzchar(trimws(x)) == FALSE)
    if (is_missing(rv$primary_mode) || is_missing(rv$secondary_mode)) return(NULL)
    
    primary   <- ifelse(rv$primary_mode == "Tracking Mode", "tm", "qm")
    secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
    paste(primary, secondary, sep = "_")
  })
  
  # ------------------------------------------------------------------
  # Create the processing module ONCE and pass a function that returns
  # the current config at runtime (so module can be instantiated early
  # even when modes are not set).
  # ------------------------------------------------------------------
  processing_module_server(
    "processing",
    rv,
    config = function() {
      m <- get_mode()
      if (is.null(m)) stop("Processing mode not set. Please select primary + secondary mode in Raw Data tab.")
      get_processing_config(m)
    }
  )
  
  # ------------------------------------------------------------------
  # Dynamic UI: Processing
  # ------------------------------------------------------------------
  output$processing_ui <- shiny::renderUI({
    tryCatch({
      if (is.null(rv$primary_mode) || is.null(rv$secondary_mode)) return(waiting_message_ui())
      mode <- get_mode()
      shiny::req(mode)
      cfg <- get_processing_config(mode)
      processing_module_ui("processing", cfg)
    }, error = function(e) {
      shiny::wellPanel(shiny::h4("Processing UI error"), shiny::pre(e$message))
    })
  })
  
  # ------------------------------------------------------------------
  # Dynamic UI: Visualization
  # ------------------------------------------------------------------
  output$visualization_ui <- shiny::renderUI({
    tryCatch({
      if (is.null(rv$primary_mode) || is.null(rv$secondary_mode)) return(waiting_message_ui())
      mode <- get_mode()
      shiny::req(mode)
      switch(mode,
             "tm_ldm" = visualization_tm_ldm_ui("visualization"),
             "tm_vm"  = visualization_tm_vm_ui("visualization"),
             "qm_ldm" = visualization_qm_ldm_ui("visualization"),
             "qm_vm"  = visualization_qm_vm_ui("visualization"),
             visualization_tm_ldm_ui("visualization")
      )
    }, error = function(e) {
      shiny::wellPanel(shiny::h4("Visualization UI error"), shiny::pre(e$message))
    })
  })
  
  # ------------------------------------------------------------------
  # Start corresponding visualization server ONCE per mode
  # ------------------------------------------------------------------
  shiny::observeEvent(get_mode(), {
    mode <- get_mode()
    if (is.null(mode)) return(NULL)
    
    # start only if not started already for this mode
    if (is.null(rv$visualization_started[[mode]]) || !isTRUE(rv$visualization_started[[mode]])) {
      switch(mode,
             "tm_ldm" = visualization_tm_ldm_server("visualization", rv),
             "tm_vm"  = visualization_tm_vm_server("visualization", rv),
             "qm_ldm" = visualization_qm_ldm_server("visualization", rv),
             "qm_vm"  = visualization_qm_vm_server("visualization", rv),
             visualization_tm_ldm_server("visualization", rv)
      )
      rv$visualization_started[[mode]] <- TRUE
    }
  }, ignoreNULL = TRUE)
  
  # ------------------------------------------------------------------
  # Exit Button
  # ------------------------------------------------------------------
  shiny::observeEvent(input$exit_app, {
    shiny::stopApp()
  })
  
  # ------------------------------------------------------------------
  # Reset Button (Full Reset of data/state). IMPORTANT:
  # - Clear data state so UI returns to initial state.
  # - Do NOT recreate module servers here (that would cause duplicate observers).
  # ------------------------------------------------------------------
  shiny::observeEvent(input$reset_app, {
    # Clear data-related reactive values
    rv$plate_plan_df_list                   <- NULL
    rv$raw_data_list                        <- NULL
    rv$enriched_data_list                   <- NULL
    rv$zone_calculated_list                 <- NULL
    rv$all_zone_combined_df                 <- NULL
    rv$all_zone_combined_lineplots          <- NULL
    rv$all_zone_combined_light_dark_boxplots <- NULL
    rv$all_zone_combined_cum_boxplots       <- NULL
    rv$all_zone_combined_delta_boxplots     <- NULL
    rv$plot                                 <- NULL
    rv$primary_mode                         <- NULL
    rv$secondary_mode                       <- NULL
    rv$generated_figures                    <- list()
    rv$processing_results <- NULL
    rv$ordered_plate_plans <- NULL
    rv$mapping <- NULL
    
    # Note: do NOT reset rv$visualization_started (keep modules created once per session).
    # Reset inputs in the UI (module namespaces)
    shinyjs::reset("plate_plan-plate_plan_files")
    shinyjs::reset("raw_data-raw_data_files")
    
    shiny::updateSelectInput(session, "raw_data-primary_mode", selected = "")
    shiny::updateSelectInput(session, "raw_data-secondary_mode", selected = "")
    
    shiny::updateSelectInput(session, "plate_plan-create_plate_plan", selected = "")
    shiny::updateSelectInput(session, "plate_plan-plate_type", selected = "")
    shiny::updateSelectInput(session, "plate_plan-keep_border_wells", selected = "")
    
    shiny::updateNumericInput(session, "plate_plan-plate_number", value = 1)
    shiny::updateNumericInput(session, "plate_plan-conditions_number", value = 1)
    shiny::updateNumericInput(session, "plate_plan-replicates_number", value = 1)
    shiny::updateNumericInput(session, "plate_plan-units_per_replicate", value = 1)
    shiny::updateNumericInput(session, "plate_plan-seed_value", value = 42)
    
    shiny::updateTextInput(session, "plate_plan-conditions_name", value = "")
    shiny::updateTextInput(session, "plate_plan-plate_plan_name_xlsx", value = "plate_plan")
    
    shiny::showNotification("Application fully reset — please reload your plate plans, raw data and modes.", type = "message")
  })
}
