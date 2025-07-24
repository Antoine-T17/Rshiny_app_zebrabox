library(shiny)
library(shinydashboard)
library(shinyjs)

server <- function(input, output, session) {
  # Valeurs réactives globales (inchangé, mais trié pour clarté)
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
  
  # Helper pour le message d'attente statique (factorisé pour réutilisation)
  waiting_message_ui <- function() {
    fluidRow(
      column(width = 12, align = "center",
             h3("Still waiting for primary and secondary modes...", style = "color: #2196F3; font-style: italic;"),
             icon("coffee", class = "fa-3x", style = "margin-top: 1em;"),
             div(style = "margin-bottom: 20px;"),  # Espace ajouté entre l'icône et le texte
             p("Go fill them in the Raw Data Tab", style = "font-size: 1.2em;")
      )
    )
  }
  
  # Modules toujours appelés
  plate_plan_server("plate_plan", rv)
  raw_data_server("raw_data", rv)
  
  # Helper réactive pour calculer le mode une seule fois
  get_mode <- reactive({
    req(rv$primary_mode, rv$secondary_mode)
    primary <- ifelse(rv$primary_mode == "Tracking Mode", "tm", "qm")
    secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
    paste(primary, secondary, sep = "_")
  })
  
  # UI dynamiques processing
  output$processing_ui <- renderUI({
    if (is.null(rv$primary_mode) || is.null(rv$secondary_mode)) {
      waiting_message_ui()  # Affiche le message fun
    } else {
      mode <- get_mode()  # Utilise ton helper get_mode() existant
      switch(mode,
             "tm_ldm" = processing_tm_ldm_ui("processing"),
             "tm_vm"  = processing_tm_vm_ui("processing"),
             "qm_ldm" = processing_qm_ldm_ui("processing"),
             "qm_vm"  = processing_qm_vm_ui("processing"),
             processing_tm_ldm_ui("processing")  # Default
      )
    }
  })
  
  # UI dynamiques visualization
  output$visualization_ui <- renderUI({
    if (is.null(rv$primary_mode) || is.null(rv$secondary_mode)) {
      waiting_message_ui()  # Affiche le message fun
    } else {
      mode <- get_mode()  # Utilise ton helper get_mode() existant
      switch(mode,
             "tm_ldm" = visualization_tm_ldm_ui("visualization"),
             "tm_vm"  = visualization_tm_vm_ui("visualization"),
             "qm_ldm" = visualization_qm_ldm_ui("visualization"),
             "qm_vm"  = visualization_qm_vm_ui("visualization"),
             visualization_tm_ldm_ui("visualization")  # Default
      )
    }
  })
  
  # Serveurs dynamiques (appelés une fois via observeEvent sur mode)
  observeEvent(get_mode(), {
    mode <- get_mode()
    if (!rv$processing_server_called) {
      switch(mode,
             "tm_ldm" = processing_tm_ldm_server("processing", rv),
             "tm_vm"  = processing_tm_vm_server("processing", rv),
             "qm_ldm" = processing_qm_ldm_server("processing", rv),
             "qm_vm"  = processing_qm_vm_server("processing", rv),
             processing_tm_ldm_server("processing", rv)  # Default
      )
      rv$processing_server_called <- TRUE
    }
    
    if (!rv$visualization_server_called) {
      switch(mode,
             "tm_ldm" = visualization_tm_ldm_server("visualization", rv),
             "tm_vm"  = visualization_tm_vm_server("visualization", rv),
             "qm_ldm" = visualization_qm_ldm_server("visualization", rv),
             "qm_vm"  = visualization_qm_vm_server("visualization", rv),
             visualization_tm_ldm_server("visualization", rv)  # Default
      )
      rv$visualization_server_called <- TRUE
    }
  })
  
  # Gestion du bouton Exit
  observeEvent(input$exit_app, {
    stopApp()
  })
  
  # Debug modes (conditionné pour dev only)
  debug_mode <- TRUE  # Set to FALSE in prod
  if (debug_mode) {
    observe({ message("primary_mode → ", rv$primary_mode) })
    observe({ message("secondary_mode → ", rv$secondary_mode) })
  }
}