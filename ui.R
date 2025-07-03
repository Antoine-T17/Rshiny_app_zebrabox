# ui.R

# ── Consolidated CSS and JavaScript ───────────────────────────────────────────
header_styles <- tags$head(
  tags$style(HTML('
    .switch {
      position: relative;
      display: inline-block;
      width: 50px;
      height: 28px;
    }
    .switch input {
      opacity: 0;
      width: 0;
      height: 0;
    }
    .slider {
      position: absolute;
      cursor: pointer;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background-color: #ccc;
      transition: .4s;
      border-radius: 24px;
    }
    .slider:before {
      position: absolute;
      content: "";
      height: 20px;
      width: 20px;
      left: 4px;
      bottom: 4px;
      background-color: white;
      transition: .4s;
      border-radius: 50%;
    }
    input:checked + .slider {
      background-color: #2196F3;
    }
    input:checked + .slider:before {
      transform: translateX(22px);
    }
    .exit-button {
      background-color: #a31f15;
      color: #fff;
      border: none;
      margin-top: 8px;
      margin-right: 10px;
      padding: 6px 12px;
      font-weight: bold;
      border-radius: 10px;
    }
    .theme-toggle {
      margin-top: 11px;
      margin-right: 10px;
      display: inline-flex;
      align-items: center;
    }
  ')),
  tags$script(HTML('
    function toggleTheme() {
      var body = document.body;
      var isDark = body.getAttribute("data-bs-theme") === "dark";
      body.setAttribute("data-bs-theme", isDark ? "light" : "dark");
      var sidebar = document.getElementsByClassName("sidebar")[0];
      if (sidebar) {
        sidebar.style.backgroundColor = isDark ? "#222d32" : "#343a40";
}
}
'))
)

# ── Dashboard Header Components ─────────────────────────────────────────────
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
        tags$span(class = "slider")
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
  skin = "red"
)

