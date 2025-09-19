# ======================================================================
# global.R
# Global setup and utilities (runs once per R session)
# ======================================================================

# ----------------------------------------------------------------------
# Utility: Install & Load Required Packages
# ----------------------------------------------------------------------
load_or_install <- function(pkgs) {
  installed <- rownames(installed.packages())
  for (pkg in pkgs) {
    if (! pkg %in% installed) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------------------------------------------------
# Required Packages
# ----------------------------------------------------------------------
required_pkgs <- c(
  "shiny", "shinyjs", "shinydashboard",
  "bslib", "sass", "readxl", "openxlsx",
  "dplyr", "ggplot2", "plotly", "htmlwidgets", "DT",
  "zip", "scales", "rhandsontable", "shinyjqui",
  "stringr", "RColorBrewer", "writexl", "purrr", "tidyr", "fmsb"
)
load_or_install(required_pkgs)

# ----------------------------------------------------------------------
# Global Utility Functions
# ----------------------------------------------------------------------

# Convert selected columns to numeric (with locale-safe decimal replacement)
convert_numeric_cols <- function(df, cols) {
  for (col in intersect(names(df), cols)) {
    df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  }
  df
}

# UI placeholder shown until modes are defined
waiting_message_ui <- function() {
  shiny::fluidRow(
    shiny::column(
      width = 12, align = "center",
      shiny::h3(
        "Still waiting for primary and secondary modes...",
        style = "color: #2196F3; font-style: italic;"
      ),
      shiny::icon("coffee", class = "fa-3x", style = "margin-top: 1em;"),
      shiny::div(style = "margin-bottom: 20px;"),  # spacing
      shiny::p("Go fill them in the Raw Data Tab", style = "font-size: 1.2em;")
    )
  )
}

# File Reading Utility
read_file <- function(path, file_name) {
  df <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    utils::read.csv2(path, sep = ";", dec = ".")
  } else {
    readxl::read_excel(path)
  }
  attr(df, "file_name") <- file_name
  df
}

# ======================================================================
# Processing mode configuration (used by the generic processing module)
# ======================================================================

processing_config <- list(
  tm_ldm = list(
    ui_title = "Tracking Mode, Light-Dark Mode",
    # maps "lightXX" -> "light", "darkYY" -> "dark", otherwise unchanged
    period_map = function(x) {
      dplyr::case_when(
        stringr::str_detect(x, "^light") ~ "light",
        stringr::str_detect(x, "^dark")  ~ "dark",
        TRUE ~ x
      )
    }
  ),
  tm_vm = list(
    ui_title = "Tracking Mode, Vibration-Rest Mode",
    # maps "vibrationXX" -> "vibration", "restYY" -> "rest", otherwise unchanged
    period_map = function(x) {
      dplyr::case_when(
        stringr::str_detect(x, "^vibration") ~ "vibration",
        stringr::str_detect(x, "^rest")      ~ "rest",
        TRUE ~ x
      )
    }
  )
)

# Small helper to fetch config safely
# global.R (append)

# Return a configuration list used by the generic processing module
get_processing_config <- function(mode) {
  # helper period mapping functions
  ldm_period_map <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, "^light") ~ "light",
      stringr::str_detect(x, "^dark")  ~ "dark",
      TRUE ~ x
    )
  }
  
  vm_period_map <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, "^vibration") ~ "vibration",
      stringr::str_detect(x, "^rest")      ~ "rest",
      TRUE ~ x
    )
  }
  
  # filter function for QM modes (keep only rows with "quantauc" in datatype)
  qm_filter_fn <- function(df) {
    if ("datatype" %in% colnames(df) && any(stringr::str_detect(df$datatype, "quantauc"))) {
      dplyr::filter(df, stringr::str_detect(datatype, "quantauc"))
    } else {
      df
    }
  }
  
  switch(mode,
         "tm_ldm" = list(
           ui_title        = "Tracking Mode, Light-Dark Mode",
           period_map      = ldm_period_map,
           convert_cols    = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur","period"),
           zone_num_cols   = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur"),
           filter_fn       = NULL
         ),
         "tm_vm" = list(
           ui_title        = "Tracking Mode, Vibration-Rest Mode",
           period_map      = vm_period_map,
           convert_cols    = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur","period"),
           zone_num_cols   = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur"),
           filter_fn       = NULL
         ),
         "qm_ldm" = list(
           ui_title        = "Quantization Mode, Light-Dark Mode",
           period_map      = ldm_period_map,
           convert_cols    = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg","period"),
           zone_num_cols   = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg"),
           filter_fn       = qm_filter_fn
         ),
         "qm_vm" = list(
           ui_title        = "Quantization Mode, Vibration-Rest Mode",
           period_map      = vm_period_map,
           convert_cols    = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg","period"),
           zone_num_cols   = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg"),
           filter_fn       = qm_filter_fn
         ),
         # default fallback
         list(
           ui_title        = "Tracking Mode, Light-Dark Mode",
           period_map      = ldm_period_map,
           convert_cols    = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur","period"),
           zone_num_cols   = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur"),
           filter_fn       = NULL
         )
  )
}

# ----------------------------------------------------------------------
# Options
# ----------------------------------------------------------------------
# Allow large Excel uploads (500 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

# ----------------------------------------------------------------------
# Auto-load All Modules
# ----------------------------------------------------------------------
module_files <- list.files(
  path       = "modules",
  pattern    = "\\.R$",
  full.names = TRUE,
  recursive  = TRUE
)
module_files <- sort(module_files)
invisible(lapply(module_files, source))
