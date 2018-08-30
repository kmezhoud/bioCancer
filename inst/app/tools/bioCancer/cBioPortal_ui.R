output$cBioPortal <- renderUI({
  tagList(
    includeCSS(file.path(getOption("radiant.path.bioCancer"),"app/www/style.css")),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          conditionalPanel("input.tabs_cbioportal == 'Studies'",
                           uiOutput("Welcome"),
                           uiOutput("ui_Studies")),
          conditionalPanel("input.tabs_cbioportal != 'Studies'",
                           selectizeInput(
                             'StudiesID', 'Select a study', choices = Studies, selected = "gbm_tcga_pub" ,multiple = FALSE
                           ),
                           uiOutput("ui_Cases"),
                           conditionalPanel("input.tabs_cbioportal != 'Clinical'",
                                            uiOutput("ui_GenProfs"),
                                            uiOutput("ui_GeneList"),
                                            uiOutput("ui_loadGeneList")

                           )
          ),

          conditionalPanel("input.tabs_cbioportal == 'Clinical'", uiOutput("ui_ClinicalData")),
          conditionalPanel("input.tabs_cbioportal == 'ProfData'", uiOutput("ui_ProfData")),
          conditionalPanel("input.tabs_cbioportal == 'Mutation'", uiOutput("ui_MutData"))

        )
      ),
      mainPanel(
        conditionalPanel("input.overview_id == true",
                         uiOutput("pipeline"),
                         imageOutput("overview")
        ),

       # tags$hr(),

        tabsetPanel(id = "tabs_cbioportal",

                    tabPanel("Studies",
                             downloadLink("dl_Studies_tab", "", class = "fa fa-download alignright"),
                             DT::dataTableOutput(outputId = "StudiesTable")
                    ),
                    tabPanel("Clinical",
                             downloadLink("dl_Clinical_tab", "", class = "fa fa-download alignright"),
                             DT::dataTableOutput(outputId="ClinicalDataTable")

                    ),
                    tabPanel("ProfData",
                             downloadLink("dl_ProfData_tab", "", class = "fa fa-download alignright"),
                             DT::dataTableOutput(outputId ="ProfDataTable")

                    ),
                    tabPanel("Mutation",
                             downloadLink("dl_MutData_tab", "", class = "fa fa-download alignright"),
                             DT::dataTableOutput(outputId ="MutDataTable")
                    )
        )
      )
    )
  )
})
