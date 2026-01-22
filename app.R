# app.R

# ---- renv bootstrap (must be first) ----
if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  renv::activate()
  
  st <- tryCatch(renv::status(), error = function(e) NULL)
  if (!is.null(st) && isFALSE(st$synchronized)) {
    stop(
      "Dépendances renv non installées / non synchronisées.\n",
      "Dans le dossier du projet, exécute : renv::restore()\n",
      "Puis relance l'application."
    )
  }
}

source("global.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shiny::shinyApp(ui = ui, server = server)
