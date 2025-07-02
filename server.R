# server.R

server <- function(input, output, session) {
  # Valeurs réactives globales
  rv <- reactiveValues(
    plate_plan_df_list               = NULL,
    raw_data_list                    = NULL,
    enriched_data_list               = NULL,
    zone_calculated_list             = NULL,
    all_zone_combined_df             = NULL,
    all_zone_combined_lineplots      = NULL,
    all_zone_combined_light_dark_boxplots = NULL,
    all_zone_combined_cum_boxplots   = NULL,
    all_zone_combined_delta_boxplots = NULL,
    plot                             = NULL,
    primary_mode                     = NULL,
    secondary_mode                   = NULL,
    generated_figures                = list(),
    processing_server_called         = FALSE,
    visualization_server_called      = FALSE
  )
  
  # Modules toujours appelés
  plate_plan_server("plate_plan", rv)
  raw_data_server("raw_data",     rv)
  
  # UI dynamiques processing
  output$processing_ui <- renderUI({
    req(rv$primary_mode, rv$secondary_mode)
    primary   <- ifelse(rv$primary_mode   == "Tracking Mode", "tm", "qm")
    secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
    mode      <- paste(primary, secondary, sep = "_")
    switch(
      mode,
      tm_ldm = processing_tm_ldm_ui("processing"),
      tm_vm  = processing_tm_vm_ui("processing"),
      qm_ldm = processing_qm_ldm_ui("processing"),
      qm_vm  = processing_qm_vm_ui("processing"),
      processing_tm_ldm_ui("processing")
    )
  })
  
  # UI dynamiques visualization
  output$visualization_ui <- renderUI({
    req(rv$primary_mode, rv$secondary_mode)
    primary   <- ifelse(rv$primary_mode   == "Tracking Mode", "tm", "qm")
    secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
    mode      <- paste(primary, secondary, sep = "_")
    switch(
      mode,
      tm_ldm = visualization_tm_ldm_ui("visualization"),
      tm_vm  = visualization_tm_vm_ui("visualization"),
      qm_ldm = visualization_qm_ldm_ui("visualization"),
      qm_vm  = visualization_qm_vm_ui("visualization"),
      visualization_tm_ldm_ui("visualization")
    )
  })
  
  # Forcer le rafraîchissement de l'UI si modes changent
  rv$ui_refresh <- reactiveVal(0)
  observe({
    req(rv$primary_mode, rv$secondary_mode)
    rv$ui_refresh(isolate(rv$ui_refresh() + 1))
  })
  
  # Serveurs dynamiques processing
  observe({
    req(rv$primary_mode, rv$secondary_mode)
    if (!rv$processing_server_called) {
      primary   <- ifelse(rv$primary_mode   == "Tracking Mode", "tm", "qm")
      secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
      mode      <- paste(primary, secondary, sep = "_")
      switch(
        mode,
        tm_ldm = processing_tm_ldm_server("processing", rv),
        tm_vm  = processing_tm_vm_server("processing", rv),
        qm_ldm = processing_qm_ldm_server("processing", rv),
        qm_vm  = processing_qm_vm_server("processing", rv),
        processing_tm_ldm_server("processing", rv)
      )
      rv$processing_server_called <- TRUE
    }
  })
  
  # Serveurs dynamiques visualization
  observe({
    req(rv$primary_mode, rv$secondary_mode)
    if (!rv$visualization_server_called) {
      primary   <- ifelse(rv$primary_mode   == "Tracking Mode", "tm", "qm")
      secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
      mode      <- paste(primary, secondary, sep = "_")
      switch(
        mode,
        tm_ldm = visualization_tm_ldm_server("visualization", rv),
        tm_vm  = visualization_tm_vm_server("visualization", rv),
        qm_ldm = visualization_qm_ldm_server("visualization", rv),
        qm_vm  = visualization_qm_vm_server("visualization", rv),
        visualization_tm_ldm_server("visualization", rv)
      )
      rv$visualization_server_called <- TRUE
    }
  })
  
  observeEvent(input$exit_app, {
    stopApp()
  })
  
  # Debug modes
  observe({ message("primary_mode → ",   rv$primary_mode) })
  observe({ message("secondary_mode → ", rv$secondary_mode) })
}