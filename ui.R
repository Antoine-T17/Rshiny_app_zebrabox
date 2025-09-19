# ======================================================================
# ui.R
# ======================================================================

# ======================================================================
# 1) Base Theme (Light)
# ======================================================================
base_theme <- fresh::create_theme(
  theme = "default",
  fresh::bs_vars_global(
    body_bg   = "#FFF",  # Default light background
    text_color = "#000"  # Default text color
  ),
  fresh::bs_vars_wells(
    bg     = "#F8F9FA",  # Well panel background
    border = "#DEE2E6"   # Well panel border
  ),
  fresh::bs_vars_button(
    default_bg     = "#2196F3", # Button background
    default_color  = "#FFF",    # Button text color
    default_border = "#2196F3"  # Button border
  )
)

# ======================================================================
# 2) Custom CSS and JavaScript for Theme Toggle
# ======================================================================
header_styles <- shiny::tags$head(
  # -------------------- CSS --------------------
  shiny::tags$style(shiny::HTML('
    /* Toggle switch (light/dark mode) */
    .switch { position: relative; display: inline-block; width: 50px; height: 28px; }
    .switch input { opacity: 0; width: 0; height: 0; } /* hide default checkbox */
    .slider { 
      position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0; 
      background-color: #ccc; transition: .4s; border-radius: 24px; 
    }
    .slider:before { 
      position: absolute; content: ""; height: 20px; width: 20px; left: 4px; bottom: 4px; 
      background-color: white; transition: .4s; border-radius: 50%; 
    }
    input:checked + .slider { background-color: #2196F3; }
    input:checked + .slider:before { transform: translateX(22px); }

    /* Exit button (round red button) */
    .exit-button-circle {
      background-color: #a31f15;
      color: #fff;
      border: none;
      border-radius: 50%;
      width: 35px;
      height: 35px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 18px;
      cursor: pointer;
      margin: 0 !important;
      padding: 0 !important;
    }
    .exit-button-circle:hover {
      background-color: #861810;
    }

    /* ================== LIGHT THEME ================== */
    .main-header { background-color: #FFF !important; color: #000 !important; }  /* header bar */
    .main-sidebar { background-color: #F8F9FA !important; color: #000 !important; } /* sidebar */
    .main-sidebar .sidebar-menu li a { color: #000 !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; }
    .main-sidebar .sidebar-menu li.active a { color: #000 !important; background-color: #E0E0E0 !important; } /* selected menu item */
    .main-sidebar .sidebar-menu li a:hover { background-color: #2196F3 !important; color: #FFF !important; transform: translateY(-3px) !important; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15) !important; } /* hover effect */
    .content-wrapper { background-color: #FFF !important; } /* main background */
    
    /* Box (card) styling */
    .box { 
      border-radius: 15px !important; background-color: #FFF !important; 
      border: 2px solid #DEE2E6 !important; 
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1) !important; 
      transition: transform 0.3s ease, box-shadow 0.3s ease !important; 
      padding: 15px !important; 
    }
    .box:hover { transform: translateY(-5px) !important; box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important; }
    .box:hover i { transform: scale(1.2) !important; transition: transform 0.5s ease !important; }
    .box i { transition: transform 0.5s ease !important; }
    .box .details { display: none; font-size: 0.95em; color: #555; margin-top: 1em; }
    .box:hover .details { display: block; }
    
    /* Tabs inside boxes */
    .box .nav-tabs { background-color: transparent !important; border-bottom: 2px solid #DEE2E6 !important; border-radius: 10px 10px 0 0 !important; }
    .box .nav-tabs li a { color: #000 !important; background-color: transparent !important; border: none !important; padding: 10px 20px !important; border-radius: 10px 10px 0 0 !important; }
    .box .nav-tabs li a:hover, .box .nav-tabs li.active a { background-color: #2196F3 !important; color: #FFF !important; border-bottom: none !important; }
    .box .tab-content { background-color: #F8F9FA !important; border-radius: 0 0 15px 15px !important; padding: 15px !important; }
    
    /* Well panels */
    .well { background-color: #F8F9FA !important; border: 1px solid #DEE2E6 !important; }
    
    /* ================== DARK THEME ================== */
    [data-theme="dark"] .main-header { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a { color: #FFF !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li.active a { color: #FFF !important; background-color: #1a2226 !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a:hover { background-color: #2196F3 !important; color: #FFF !important; transform: translateY(-3px) !important; box-shadow: 0 4px 8px rgba(0,0,0,0.4) !important; }
    [data-theme="dark"] .content-wrapper { background-color: #2E2E2E !important; }
    
    /* Boxes */
    [data-theme="dark"] .box { background-color: #2E2E2E !important; border: 2px solid #444 !important; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4) !important; padding: 15px !important; }
    [data-theme="dark"] .box:hover { transform: translateY(-5px) !important; box-shadow: 0 8px 20px rgba(0, 0, 0, 0.6) !important; }
    [data-theme="dark"] .box:hover i { transform: scale(1.2) !important; transition: transform 0.5s ease !important; }
    [data-theme="dark"] .box i { transition: transform 0.5s ease !important; }
    [data-theme="dark"] .box .details { display: none; font-size: 0.95em; color: #BBB; margin-top: 1em; }
    [data-theme="dark"] .box:hover .details { display: block; }
    
    /* Tabs inside dark boxes */
    [data-theme="dark"] .box .nav-tabs { border-bottom-color: #444 !important; }
    [data-theme="dark"] .box .nav.nav-tabs li a { color: #FFF !important; }
    [data-theme="dark"] .box .nav.nav-tabs li a:hover, 
    [data-theme="dark"] .box .nav.nav-tabs li.active a { background-color: #2196F3 !important; color: #FFF !important; border-bottom: none !important; }
    [data-theme="dark"] .box .tab-content { background-color: #2E2E2E !important; }
    
    /* Console / pre output */
    [data-theme="dark"] pre, 
    [data-theme="dark"] .shiny-output-error, 
    [data-theme="dark"] .shiny-text-output {
      background-color: #2E2E2E !important;
      color: #FFF !important;
      border: 1px solid #444 !important;
      padding: 10px;
      border-radius: 5px;
    }
    
    /* Well panels in dark */
    [data-theme="dark"] .well { background-color: #444 !important; border-color: #666 !important; color: #FFF !important; }
    
    /* Global stronger text color */
    [data-theme="dark"] body,
    [data-theme="dark"] .content-wrapper,
    [data-theme="dark"] .main-sidebar,
    [data-theme="dark"] .box,
    [data-theme="dark"] .well,
    [data-theme="dark"] .nav > li > a,
    [data-theme="dark"] .tab-content,
    [data-theme="dark"] .box-header,
    [data-theme="dark"] .box-body {
      color: #FFF !important;
    }
    
    /* DataTables in dark */
    [data-theme="dark"] .dataTables_wrapper { background-color: #2E2E2E !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper table { background-color: #2E2E2E !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTable th, 
    [data-theme="dark"] .dataTables_wrapper .dataTable td { color: #FFF !important; border-color: #444 !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_filter input, 
    [data-theme="dark"] .dataTables_wrapper .dataTables_length select { background-color: #444 !important; color: #FFF !important; border: 1px solid #666 !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button { background-color: #444 !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background-color: #2196F3 !important; color: #FFF !important; }

  ')),
  
  # -------------------- JavaScript --------------------
  shiny::tags$script(shiny::HTML('
    function toggleTheme() {
      var body = document.body;
      var isDark = body.getAttribute("data-theme") === "dark";
      body.setAttribute("data-theme", isDark ? "light" : "dark");
      if (window.Shiny) {
        Shiny.setInputValue("themeChanged", !isDark);
      }
    }
  '))
)

# ======================================================================
# 3) Dashboard Header
# ======================================================================
header_items <- shinydashboard::dashboardHeader(
  title = "Zebrabox Treatment",
  shiny::tags$li(
    shiny::div(
      style = "display: flex; align-items: center; gap: 15px; margin-top: 6px; margin-right: 12px;",
      # Theme toggle switch
      shiny::tags$label(
        class = "switch",
        shiny::tags$input(type = "checkbox", id = "themeToggle", onclick = "toggleTheme()"),
        shiny::tags$span(class = "slider"),
        shiny::tags$span(style = "margin-left: 8px;", shiny::icon("sun", class = "fa-lg"))
      ),
      # Help icon
      shiny::a(
        href   = "https://antoine-t17.github.io/website_personal/posts/rshiny_app_zebrabox/",
        target = "_blank",
        shiny::icon("question-circle", class = "fa-2x"),
        style  = "color: #2196F3; text-decoration: none;"
      ),
      # Reset button
      shiny::actionButton(
        inputId = "reset_app",
        label   = NULL,
        icon    = shiny::icon("refresh"),
        class   = "exit-button-circle",
        style   = "background-color:#2196F3;"  # blue refresh
      ),
      # Exit button
      shiny::actionButton(
        inputId = "exit_app",
        label   = NULL,
        icon    = shiny::icon("power-off"),
        class   = "exit-button-circle"
      )
    ),
    class = "dropdown"
  )
)

# ======================================================================
# 4) Sidebar
# ======================================================================
sidebar_content <- shinydashboard::dashboardSidebar(
  shinyjs::useShinyjs(),
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Welcome",       tabName = "welcome",       icon = shiny::icon("home")),
    shinydashboard::menuItem("Plate Plan",    tabName = "plate_plan",    icon = shiny::icon("table")),
    shinydashboard::menuItem("Raw Data",      tabName = "raw_data",      icon = shiny::icon("file-import")),
    shinydashboard::menuItem("Processing",    tabName = "processing",    icon = shiny::icon("gears")),
    shinydashboard::menuItem("Visualization", tabName = "visualization", icon = shiny::icon("chart-line"))
  )
)

# ======================================================================
# 5) Welcome Tab Content
# ======================================================================
welcome_content <- shinydashboard::tabItem(
  tabName = "welcome",
  shiny::fluidRow(
    shiny::column(
      width = 12, align = "center",
      shiny::h1("Welcome to Zebrabox Experiment Pipeline", class = "welcome-title"),
      shiny::p(
        "This application allows you to generate randomized plate layouts, 
        import and process raw data, and visualize results in multiple modes.",
        style = "font-size: 1.4em; line-height: 1.6; margin-bottom: 2em; text-align: center; font-style: italic;"
      )
    )
  ),
  # Feature overview boxes
  shiny::fluidRow(
    shiny::column(width = 3, shinydashboard::box(
      width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("table", class = "fa-3x")),
      shiny::h4(shiny::strong("PLATE PLAN"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Import or generate randomized plate layouts to minimize edge effects from well positioning."),
      shiny::div(class = "details", shiny::p("Randomization helps avoid camera overestimation due to reflections in border wells. Exclude borders if needed, create multiple plates with equitable condition distribution, and preview as tables or figures."))
    )),
    shiny::column(width = 3, shinydashboard::box(
      width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("file-import", class = "fa-3x")),
      shiny::h4(shiny::strong("RAW DATA"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Import Zebrabox outputs (.xlsx). Choose mode: Tracking or Quantization, Light/Dark or Vibration/Rest."),
      shiny::div(class = "details",
                 shiny::p("Tracking follows exact movements of organisms (e.g., one per well, multi-animal support). Quantization measures pixel changes for activity (e.g., Δ pixels above threshold). Light/Dark tests light responses; Vibration/Rest assesses sound/vibration reactions."),
                 shiny::p("For details, see the ", shiny::tags$a(href = "https://www.viewpoint.fr/upload/productBrochurePdf/catalogueaqua-compressed-6373a62793a3e038651827.pdf", "Zebrabox brochure", target = "_blank"), " or visit the ", tags$a(href = "https://www.viewpoint.fr/product/zebrafish/fish-behavior-monitoring/zebrabox", "official website", target = "_blank"), "."))
    )),
    shiny::column(width = 3, shinydashboard::box(
      width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("gears", class = "fa-3x")),
      shiny::h4(shiny::strong("PROCESSING"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Confirm plate assignments with raw data, then upload Transition and Removal .xlsx templates for event timing and exclusions."),
      shiny::div(class = "details", shiny::p("Transition captures time codes (e.g., light/vibration events); Removal excludes times, wells, periods, or conditions. Output cleaned datasets as .xlsx, with console history."))
    )),
    shiny::column(width = 3, shinydashboard::box(
      width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("chart-line", class = "fa-3x")),
      shiny::h4(shiny::strong("VISUALIZATION"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Generate dataframes from processed data and create customizable charts with themes, labels, and colors."),
      shiny::div(class = "details", shiny::p("Export as .png or interactive .html for detailed hovering and exploration."))
    ))
  ),
  shiny::fluidRow(
    shiny::column(width = 12, align = "center",
                  shiny::p("Use the left-hand menu to navigate.", style = "font-size: 1.2em; margin-top: 2em; font-style: italic;")
    )
  )
)

# ======================================================================
# 6) Dashboard Body
# ======================================================================
dashboard_body <- shinydashboard::dashboardBody(
  fresh::use_theme(base_theme),
  header_styles,
  shinydashboard::tabItems(
    welcome_content,
    shinydashboard::tabItem(tabName = "plate_plan", plate_plan_ui("plate_plan")),
    shinydashboard::tabItem(tabName = "raw_data", raw_data_ui("raw_data")),
    shinydashboard::tabItem(tabName = "processing", shiny::uiOutput("processing_ui")),
    shinydashboard::tabItem(tabName = "visualization", shiny::uiOutput("visualization_ui"))
  )
)

# ======================================================================
# 7) Assemble Full Dashboard
# ======================================================================
shinydashboard::dashboardPage(
  header  = header_items,
  sidebar = sidebar_content,
  body    = dashboard_body,
  skin    = "black"
)

