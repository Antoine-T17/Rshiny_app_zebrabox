# global.R

# 1) Fonction utilitaire pour installer/charger les paquets
load_or_install <- function(pkgs) {
  installed <- rownames(installed.packages())
  for (pkg in pkgs) {
    if (! pkg %in% installed) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# 2) Liste des paquets dont vous avez besoin
required_pkgs <- c(
  "shiny", "shinyjs", "shinydashboard",
  "bslib", "sass", "readxl", "openxlsx",
  "dplyr", "ggplot2", "plotly", "htmlwidgets", "DT",
  "zip", "scales", "rhandsontable", "shinyjqui",
  "stringr", "RColorBrewer", "writexl", "purrr", "tidyr"
)

# Installe & charge tous les paquets
load_or_install(required_pkgs)

# 3) Fonctions utilitaires globales
convert_numeric_cols <- function(df, cols) {
  for (col in intersect(names(df), cols)) {
    df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  }
  df
}

# Autorise de gros imports Excel
options(shiny.maxRequestSize = 500 * 1024^2)

# 4) Chargement automatique de tous les modules
module_files <- list.files(
  path       = "modules",
  pattern    = "\\.R$",
  full.names = TRUE,
  recursive  = TRUE
)
module_files <- sort(module_files)
invisible(lapply(module_files, source))
