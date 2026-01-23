# app.R

# ---- renv bootstrap (must be first) ----
if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  
  options(renv.consent = TRUE)   
  renv::load(project = ".")     
  
  st <- tryCatch(renv::status(project = "."), error = function(e) NULL)
  if (!is.null(st) && isFALSE(st$synchronized)) {
    message("renv: dependencies refresh (first launch)…")
    tryCatch(
      renv::restore(project = ".", prompt = FALSE),
      error = function(e) {
        stop(
          "renv::restore() has failed.\n",
          "Frequent cause: missing system dependencies (Rtools under Windows, libs curl/xml, etc.).\n",
          "Details: ", conditionMessage(e)
        )
      }
    )
  }
}

source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shiny::shinyApp(ui = ui, server = server)
