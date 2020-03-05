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


stop_radiant <- function() {
  ## quit R, unless you are running an interactive session
  if (interactive()) {
    ## flush input and r_data into Rgui or Rstudio
    isolate({
      LiveInputs <- toList(input)
      r_state[names(LiveInputs)] <- LiveInputs
      r_state$nav_radiant <- r_info[["nav_radiant"]]
      assign("r_state", r_state, envir = .GlobalEnv)
      ## convert environment to a list and then back to an environment
      ## again to remove active bindings https://github.com/rstudio/shiny/issues/1905
      ## using an environment so you can "attach" and access data easily
      rem_non_active() ## keep only the active bindings (i.e., data, datalist, etc.)
      assign("r_data", list2env(mget(ls(r_data), r_data)), envir = .GlobalEnv)
      assign("r_info", toList(r_info), envir = .GlobalEnv)
      ## removing r_sessions and functions defined in global.R
      if (exists("r_sessions")) rm(r_sessions, envir = .GlobalEnv)
      unlink("~/r_figures/", recursive = TRUE)
      sshhr(try(rm(help_menu, make_url_patterns, import_fs, init_data, navbar_proj, knit_print.data.frame, withMathJax, envir = .GlobalEnv), silent = TRUE))
      message("\nStopped bioCancer. State information is available in the r_state and r_info lists and the r_data environment. Use attach(r_data) to access data loaded into bioCancer.\n")
      stopApp()
    })
  } else {
    stopApp()
    q("no")
  }
}
