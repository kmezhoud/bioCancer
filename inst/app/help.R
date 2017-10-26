help_bioCancer <- c("Studies" = "Studies.md", "Clinical" = "Clinical.md", "Profiles" = "Profiles.md")

output$help_bioCancer <- reactive(append_help("help_bioCancer", file.path(getOption("radiant.path.bioCancer"),"app/tools/help"), Rmd = TRUE))

observeEvent(input$help_bioCancer_all, {help_switch(input$help_bioCancer_all, "help_bioCancer")})
observeEvent(input$help_bioCancer_none,{help_switch(input$help_bioCancer_none, "help_bioCancer", help_on = FALSE)})

help_bioCancer_panel <- tagList(
  wellPanel(
    HTML("<label>bioCancer Menu: <i id='help_bioCancer_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
         <i id='help_bioCancer_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_bioCancer", NULL, help_bioCancer,
                       selected = state_group("help_bioCancer"), inline = TRUE)
    )
  )

output$help_about <- renderUI({
  file.path(getOption("radiant.path.bioCancer"),"app/tools/help/about.md") %>% inclMD %>% HTML
})
