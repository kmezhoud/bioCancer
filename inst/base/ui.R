help_menu <-
  tagList(
    navbarMenu(title = "", id = "Help", icon = icon("question-circle"),
      tabPanel("Help", uiOutput("help_base"), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://kmezhoud.github.io/bioCancer/", target = "_blank",
               list(icon("globe"), "bioCancer docs"))),
      tabPanel(tags$a("", href = "https://github.com/kmezhoud/CancerPortal/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    js_head
  )

## ui for base radiant
shinyUI(

  do.call(navbarPage, c("bioCancer", nav_ui, shared_ui, help_menu))

)

