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
    default_color = "#FFF",     # Texte blancb
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
    .main-sidebar .sidebar-menu li a { color: #000 !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; }
    .main-sidebar .sidebar-menu li.active a { color: #000 !important; background-color: #E0E0E0 !important; }
    .main-sidebar .sidebar-menu li a:hover { background-color: #2196F3 !important; color: #FFF !important; transform: translateY(-3px) !important; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15) !important; }
    .content-wrapper { background-color: #FFF !important; }
    .box { border-radius: 15px !important; background-color: #FFF !important; border: 2px solid #DEE2E6 !important; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1) !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; padding: 15px !important; }
    .box:hover { transform: translateY(-5px) !important; box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important; }
    .box:hover i { transform: scale(1.2) !important; transition: transform 0.5s ease !important; } /* Grossissement au hover */
    .box i { transition: transform 0.5s ease !important; } /* Transition pour retour à la taille initiale */
    .box .details { display: none; font-size: 0.95em; color: #555; margin-top: 1em; } /* Détails cachés par défaut */
    .box:hover .details { display: block; } /* Affichage au hover */
    .box .nav-tabs { background-color: transparent !important; border-bottom: 2px solid #DEE2E6 !important; border-radius: 10px 10px 0 0 !important; }
    .box .nav-tabs li a { color: #000 !important; background-color: transparent !important; border: none !important; padding: 10px 20px !important; border-radius: 10px 10px 0 0 !important; }
    .box .nav-tabs li a:hover, .box .nav-tabs li.active a { background-color: #2196F3 !important; color: #FFF !important; border-bottom: none !important; }
    .box .tab-content { background-color: #F8F9FA !important; border-radius: 0 0 15px 15px !important; padding: 15px !important; }
    .well { background-color: #F8F9FA !important; border: 1px solid #DEE2E6 !important; }

    /* Thème sombre */
    [data-theme="dark"] .main-header { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a { color: #FFF !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li.active a { color: #FFF !important; background-color: #1a2226 !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a:hover { background-color: #2196F3 !important; color: #FFF !important; transform: translateY(-3px) !important; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.4) !important; }
    [data-theme="dark"] .content-wrapper { background-color: #2E2E2E !important; }
    [data-theme="dark"] .box { background-color: #2E2E2E !important; border: 2px solid #444 !important; box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4) !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; padding: 15px !important; }
    [data-theme="dark"] .box:hover { transform: translateY(-5px) !important; box-shadow: 0 8px 20px rgba(0, 0, 0, 0.6) !important; }
    [data-theme="dark"] .box:hover i { transform: scale(1.2) !important; transition: transform 0.5s ease !important; }
    [data-theme="dark"] .box i { transition: transform 0.5s ease !important; }
    [data-theme="dark"] .box .details { display: none; font-size: 0.95em; color: #BBB; margin-top: 1em; }
    [data-theme="dark"] .box:hover .details { display: block; }
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
  fluidRow(
    column(width = 12, align = "center",
           h1("Welcome to Zebrabox Experiment Pipeline", class = "welcome-title"),
           p(
             "This application lets you create randomize assignment conditions plate plans, import and process raw data, and visualize results in various modes.",
             style = "font-size: 1.4em; line-height: 1.6; margin-bottom: 2em; text-align: center; font-style: italic;"
           )
    )
  ),
  fluidRow(
    column(width = 3,
           box(
             width = NULL, status = "primary",
             div(style = "text-align: center; margin-bottom: 1em;", icon("table", class = "fa-3x")),
             h4(strong("PLATE PLAN"), style = "text-align: center; text-transform: uppercase; margin-bottom: 1.5em;"),
             p("Import or generate randomized .xlsx plate layouts to minimize edge effects from well positioning.", style = "text-align: justify; font-size: 1em; line-height: 1.5;"),
             div(class = "details",
                 p("Randomization helps avoid camera overestimation due to reflections in border wells. Exclude borders if needed, create multiple plates with equitable condition distribution, and preview as tables or figures.")
             )
           )
    ),
    column(width = 3,
           box(
             width = NULL, status = "primary",
             div(style = "text-align: center; margin-bottom: 1em;", icon("file-import", class = "fa-3x")),
             h4(strong("RAW DATA"), style = "text-align: center; text-transform: uppercase; margin-bottom: 1.5em;"),
             p("Import Zebrabox outputs in .xlsx (convert from .xls). Select primary mode (Tracking or Quantization) and secondary mode (Light/Dark or Vibration/Rest).", style = "text-align: justify; font-size: 1em; line-height: 1.5;"),
             div(class = "details",
                 p("Tracking follows exact movements of organisms (e.g., one per well, multi-animal support). Quantization measures pixel changes for activity (e.g., Δ pixels above threshold). Light/Dark tests light responses; Vibration/Rest assesses sound/vibration reactions."),
                 p("For details, see the ", tags$a(href = "https://www.viewpoint.fr/upload/productBrochurePdf/catalogueaqua-compressed-6373a62793a3e038651827.pdf", "Zebrabox brochure", target = "_blank"), " or visit the ", tags$a(href = "https://www.viewpoint.fr/product/zebrafish/fish-behavior-monitoring/zebrabox", "official website", target = "_blank"), ".")
             )
           )
    ),
    column(width = 3,
           box(
             width = NULL, status = "primary",
             div(style = "text-align: center; margin-bottom: 1em;", icon("gears", class = "fa-3x")),
             h4(strong("PROCESSING"), style = "text-align: center; text-transform: uppercase; margin-bottom: 1.5em;"),
             p("Confirm plate assignments with raw data, then upload Transition and Removal .xlsx templates for event timing and exclusions.", style = "text-align: justify; font-size: 1em; line-height: 1.5;"),
             div(class = "details",
                 p("Transition captures time codes (e.g., light/vibration events); Removal excludes times, wells, periods, or conditions. Output cleaned datasets as .xlsx, with console history.")
             )
           )
    ),
    column(width = 3,
           box(
             width = NULL, status = "primary",
             div(style = "text-align: center; margin-bottom: 1em;", icon("chart-line", class = "fa-3x")),
             h4(strong("VISUALIZATION"), style = "text-align: center; text-transform: uppercase; margin-bottom: 1.5em;"),
             p("Generate dataframes from processed data and create customizable charts with themes, labels, and colors.", style = "text-align: justify; font-size: 1em; line-height: 1.5;"),
             div(class = "details",
                 p("Export as .png or interactive .html for detailed hovering and exploration.")
             )
           )
    )
  ),
  fluidRow(
    column(width = 12, align = "center",
           p("Use the menu on the left to navigate.", style = "font-size: 1.2em; margin-top: 2em; font-style: italic;")
    )
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