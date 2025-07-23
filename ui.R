library(shiny)
library(shinydashboard)
library(shinyjs)
library(fresh)

# ── Définition du thème de base (clair) ─────────────────────────────────────
base_theme <- create_theme(
  theme = "default",
  bs_vars_global(
    body_bg = "#FFF",           # Fond clair par défaut
    text_color = "#000"         # Texte noir
  ),
  bs_vars_wells(
    bg = "#F8F9FA",             # Fond des panneaux
    border = "#DEE2E6"          # Bordure des panneaux
  ),
  bs_vars_button(
    default_bg = "#2196F3",     # Couleur des boutons
    default_color = "#FFF",     # Texte blanc
    default_border = "#2196F3"  # Bordure bleue
  )
)

# ── Styles CSS personnalisés et JavaScript pour le toggle ───────────────────
header_styles <- tags$head(
  tags$style(HTML('
    .switch { position: relative; display: inline-block; width: 50px; height: 28px; }
    .switch input { opacity: 0; width: 0; height: 0; }
    .slider { position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0; background-color: #ccc; transition: .4s; border-radius: 24px; }
    .slider:before { position: absolute; content: ""; height: 20px; width: 20px; left: 4px; bottom: 4px; background-color: white; transition: .4s; border-radius: 50%; }
    input:checked + .slider { background-color: #2196F3; }
    input:checked + .slider:before { transform: translateX(22px); }
    .exit-button { background-color: #a31f15; color: #fff; border: none; margin: 8px 10px 0 0; padding: 6px 12px; font-weight: bold; border-radius: 10px; }

    /* Thème clair (par défaut) */
    .main-header { background-color: #FFF !important; color: #000 !important; }
    .main-sidebar { background-color: #F8F9FA !important; color: #000 !important; }
    .main-sidebar .sidebar-menu li a { color: #000 !important; }
    .main-sidebar .sidebar-menu li.active a { color: #000 !important; background-color: #E0E0E0 !important; }
    .main-sidebar .sidebar-menu li a:hover { background-color: #2196F3 !important; color: #FFF !important; }
    .content-wrapper { background-color: #FFF !important; }
    .box { border-radius: 15px !important; background-color: #FFF !important; border: 2px solid #DEE2E6 !important; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1) !important; }
    .box .nav-tabs { background-color: transparent !important; border-bottom: 2px solid #DEE2E6 !important; border-radius: 10px 10px 0 0 !important; }
    .box .nav-tabs li a { color: #000 !important; background-color: transparent !important; border: none !important; padding: 10px 20px !important; border-radius: 10px 10px 0 0 !important; }
    .box .nav-tabs li a:hover, .box .nav-tabs li.active a { background-color: #2196F3 !important; color: #FFF !important; border-bottom: none !important; }
    .box .tab-content { background-color: #F8F9FA !important; border-radius: 0 0 15px 15px !important; padding: 15px !important; }
    .well { background-color: #F8F9FA !important; border: 1px solid #DEE2E6 !important; }

    /* Thème sombre */
    [data-theme="dark"] .main-header { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a { color: #FFF !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li.active a { color: #FFF !important; background-color: #1a2226 !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a:hover { background-color: #2196F3 !important; color: #FFF !important; }
    [data-theme="dark"] .content-wrapper { background-color: #2E2E2E !important; }
    [data-theme="dark"] .box { background-color: #2E2E2E !important; border: 2px solid #444 !important; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4) !important; }
    [data-theme="dark"] .box .nav-tabs { border-bottom-color: #444 !important; }
    [data-theme="dark"] .box .nav.nav-tabs li a { color: #FFF !important; }
    [data-theme="dark"] .box .nav.nav-tabs li a:hover, [data-theme="dark"] .box .nav.nav-tabs li.active a { background-color: #2196F3 !important; color: #FFF !important; border-bottom: none !important; }
    [data-theme="dark"] .box .tab-content { background-color: #2E2E2E !important; }
    [data-theme="dark"] .box > .box-header { color: #FFF !important; border-bottom: 1px solid #444 !important; }
    [data-theme="dark"] { color: #FFF !important; }
    [data-theme="dark"] .well { background-color: #444 !important; border-color: #666 !important; color: #FFF !important; }

    /* Styles spécifiques pour DT::datatable en thème sombre */
    [data-theme="dark"] .dataTables_wrapper { background-color: #2E2E2E !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper table { background-color: #2E2E2E !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTable th, [data-theme="dark"] .dataTables_wrapper .dataTable td { color: #FFF !important; border-color: #444 !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_filter input, [data-theme="dark"] .dataTables_wrapper .dataTables_length select { background-color: #444 !important; color: #FFF !important; border: 1px solid #666 !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button { background-color: #444 !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background-color: #2196F3 !important; color: #FFF !important; }

    /* Styles spécifiques pour plotly en thème sombre */
    [data-theme="dark"] .plotly .plotly .js-plotly-plot .plot-container { background-color: #2E2E2E !important; color: #FFF !important; }
    [data-theme="dark"] .plotly .plotly .js-plotly-plot .plot-container .svg-container { background-color: #2E2E2E !important; }
    [data-theme="dark"] .plotly .plotly .js-plotly-plot .plot-container text { fill: #FFF !important; }
    [data-theme="dark"] .plotly .plotly .js-plotly-plot .plot-container .cartesianlayer .trace { color: #FFF !important; }
  ')),
  tags$script(HTML('
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

# ─ Dashboard Header avec toggle ────────────────────────────────────────────
header_items <- dashboardHeader(
  title = "Zebrabox Treatment",
  tags$li(
    div(
      style = "margin-top: 11px; margin-right: 10px; display: inline-flex; align-items: center;",
      tags$label(
        class = "switch",
        tags$input(
          type = "checkbox",
          id = "themeToggle",
          onclick = "toggleTheme()"
        ),
        tags$span(class = "slider"),
        tags$span(
          style = "margin-left: 10px;",
          icon("sun", class = "fa-lg")
        )
      )
    ),
    class = "dropdown"
  ),
  tags$li(
    actionButton(
      inputId = "exit_app",
      label = "Exit",
      icon = icon("power-off"),
      class = "exit-button"
    ),
    class = "dropdown"
  )
)

# ── Dashboard Sidebar ───────────────────────────────────────────────────────
sidebar_content <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),
    menuItem("Plate Plan", tabName = "plate_plan", icon = icon("table")),
    menuItem("Raw Data", tabName = "raw_data", icon = icon("file-import")),
    menuItem("Processing", tabName = "processing", icon = icon("gears")),
    menuItem("Visualization", tabName = "visualization", icon = icon("chart-line"))
  )
)

# ── Welcome Tab Content ─────────────────────────────────────────────────────
welcome_content <- tabItem(
  tabName = "welcome",
  h2(
    "Welcome to Zebrabox Experiment Pipeline",
    style = "font-size: 2em; margin-bottom: 0.8em;"
  ),
  p(
    "This application lets you create randomize assignment conditions plate plans, import and process raw data, and visualize results in various modes.",
    style = "font-size: 1.2em; line-height: 1.6; margin-bottom: 1.2em;"
  ),
  tags$ul(
    style = "font-size: 1.1em; line-height: 1.6; margin-left: 1.5em; margin-bottom: 1.5em;",
    tags$li(strong("Plate Plan:"), " Create or upload .xlsx plate layouts, preview them, and download as .xlsx."),
    tags$li(strong("Raw Data:"), " Import and view raw Zebrabox outputs (.xlsx)."),
    tags$li(
      style = "margin-left: 1em;",
      tags$u("Primary mode:"), " Tracking or Quantization"
    ),
    tags$li(
      style = "margin-left: 1em;",
      tags$u("Secondary mode:"), " Light/Dark tests or Vibration tests"
    ),
    tags$li(strong("Processing:"), " Match raw data with plate plans, upload two Excel templates, view history in Console Output, and download cleaned datasets as .xlsx in Processed Data."),
    tags$li(strong("Visualization:"), " Generate customizable charts (light/dark theme), labels, and colors. Export as .png or interactive .html.")
  ),
  p(
    "Use the menu on the left to navigate.",
    style = "font-size: 1.1em; margin-top: 1.5em;"
  )
)

# ── Dashboard Body ──────────────────────────────────────────────────────────
dashboard_body <- dashboardBody(
  use_theme(base_theme), # Appliquer le thème de base (clair)
  header_styles,
  tabItems(
    welcome_content,
    tabItem(tabName = "plate_plan", plate_plan_ui("plate_plan")),
    tabItem(tabName = "raw_data", raw_data_ui("raw_data")),
    tabItem(tabName = "processing", uiOutput("processing_ui")),
    tabItem(tabName = "visualization", uiOutput("visualization_ui"))
  )
)

# ── Assemble Dashboard ──────────────────────────────────────────────────────
dashboardPage(
  header = header_items,
  sidebar = sidebar_content,
  body = dashboard_body,
  skin = "black"
)