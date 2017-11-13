help_cBioPortal<- c("Studies" = "Studies.md", "Clinical" = "Clinical.md", "Profiles" = "Profiles.md",
                    "Mutation" = "Mutation.md")

help_enrichment <- c("Circomics" = "Circomics.md", "Classifier" = "Classifier.md", "Networking" = "Reactome.Rmd")

help_data <- c("Datasets" = "manage.md","View" = "view.md", "Plots" = "visualize.md",
               "Regroup" = "pivotr.md", "Statistics" = "explore.md", "Transform" = "transform.md",
               "Combine" = "combine.md", "Report" = "report.md", "Code" = "code.md")

output$help_cBioPortal <- reactive(append_help("help_cBioPortal", file.path(getOption("radiant.path.bioCancer"),"app/tools/help"), Rmd = TRUE))
output$help_enrichment <- reactive(append_help("help_enrichment", file.path(getOption("radiant.path.bioCancer"),"app/tools/help"), Rmd = TRUE))

## needed to reread new helps version
output$help_data <- reactive(append_help("help_data", file.path(getOption("radiant.path.bioCancer"),"app/tools/help/"), Rmd = TRUE))

observeEvent(input$help_cBioPortal_all, {help_switch(input$help_cBioPortal_all, "help_cBioPortal")})
observeEvent(input$help_cBioPortal_none,{help_switch(input$help_cBioPortal_none, "help_cBioPortal", help_on = FALSE)})

observeEvent(input$help_enrichment_all, {help_switch(input$help_enrichment_all, "help_enrichment")})
observeEvent(input$help_enrichment_none,{help_switch(input$help_enrichment_none, "help_enrichment", help_on = FALSE)})

## needed to redefine help_data_panel avec re-defining help_data
help_data_panel <-
  wellPanel(
    HTML("<label>Workspace: <i id='help_data_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
         <i id='help_data_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_data", NULL, help_data, selected = state_group("help_data"), inline = TRUE)
    )

help_cBioPortal_panel <- tagList(
  wellPanel(
    HTML("<label>cBioPortal Menu: <i id='help_cBioPortal_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
         <i id='help_cBioPortal_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_cBioPortal", NULL, help_cBioPortal,
                       selected = state_group("help_cBioPortal"), inline = TRUE)
    )
  )


help_enrichment_panel <- tagList(
  wellPanel(
    HTML("<label>Enrichment Menu: <i id='help_enrichment_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
         <i id='help_enrichment_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_enrichment", NULL, help_enrichment,
                       selected = state_group("help_enrichment"), inline = TRUE)
    )
  )

output$help_about <- renderUI({
  file.path(getOption("radiant.path.bioCancer"),"app/tools/help/about.md") %>% inclMD %>% HTML
})


stop_radiant <- function(rmd = FALSE) {
  ## quit R, unless you are running an interactive session
  if (interactive()) {
    ## flush input and r_data into Rgui or Rstudio
    isolate({
      toList(input) %>%
      {.$nav_radiant <- r_data$nav_radiant; .} %>%
        assign("r_state", ., envir = .GlobalEnv)

      assign("r_data", toList(r_data), envir = .GlobalEnv)

      stop_message <- "\nStopped bioCancer. State available as r_state and r_data.\n"
      lib <- if ("radiant" %in% installed.packages()) "radiant" else "radiant.data"

      if (!is_empty(input$rmd_report)) {
        lib <- "radiant"
        rmd_report <- paste0("---
                             title: \"Radiant report\"
                             author: \"\"
                             date: \"`r Sys.Date()`\"
                             output:
                             html_document:
                             highlight: textmate
                             theme: spacelab
                             df_print: paged
                             code_download: true
                             code_folding: hide
                             toc: yes
                             ---

                             ```{r setup, include = FALSE}
                             knitr::opts_chunk$set(comment = NA, echo = TRUE, cache = FALSE, dpi = 96, message = FALSE, warning = FALSE)
                             library(", lib, ")
                             load(\"~/radiant.sessions/r_data.rda\")
                             ```

                             <style type='text/css'> .table { width: auto; } ul, ol { padding-left: 18px; }</style>\n\n") %>%

          paste0(input$rmd_report) %>%
          gsub("\\\\\\\\","\\\\",.) %>%
          cleanout(.)
      }
      ## removing r_environment and r_sessions
      if (exists("r_sessions")) rm(r_sessions, envir = .GlobalEnv)
      unlink("~/r_figures/", recursive = TRUE)
      sshhr(try(rm(help_menu, make_url_patterns, import_fs, init_data, envir = .GlobalEnv), silent = TRUE))
      message(stop_message)

      if (rstudioapi::isAvailable() && !is_empty(input$rmd_report) && rmd) {
        path <- file.path(normalizePath("~"), "radiant.sessions")
        save(list = "r_data", envir = .GlobalEnv, file = file.path(path, "r_data.rda"))
        stopApp(rstudioapi::insertText(rmd_report))
      } else {
        stopApp()
      }
    })
  } else {
    stopApp()
    q("no")
  }
}
