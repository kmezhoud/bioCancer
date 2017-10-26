output$Tools <- renderUI({
  tagList(
  #includeCSS(file.path(getOption("radiant.path.data"),"app/www/style.css")),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
      conditionalPanel("input.tabs_Tools == 'Transform'",
                       ## needed to modify radiant helps
                       help_modal("Transform","transform_help",author = "Karim Mezhoud",
                                  inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/transform.md"))),
                       uiOutput("ui_Transform")),
      conditionalPanel("input.tabs_Tools == 'Combine'",
                       ## needed to modify radiant helps
                       help_modal("Combine","combine_help",author = "Karim Mezhoud",
                                  inclMD(file.path(getOption("radiant.path.bioCancer"),"app/tools/help/combine.md"))),
                       uiOutput("ui_Combine"))
      #uiOutput("ui_Transform")
      )
      ),
    mainPanel(
      tabsetPanel(id = "tabs_Tools",

          tabPanel("Transform",
                   htmlOutput("transform_data"),
                   verbatimTextOutput("transform_summary"),
                   uiOutput("ui_tr_log")

        ),
        tabPanel("Combine",
                 htmlOutput("cmb_data1"),
                 htmlOutput("cmb_data2"),
                 htmlOutput("cmb_possible"),
                 htmlOutput("cmb_data")
        )#,
        #tabPanel("Tools")
     )
    )
  )
  )
})
