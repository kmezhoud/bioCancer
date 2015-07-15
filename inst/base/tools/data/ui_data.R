#######################################
# Shiny interface for data functions
#######################################

output$ui_filter_error <- renderUI({
  if (is_empty(r_data$filter_error))
    return()
  helpText(r_data$filter_error)
})

# data ui and tabs
output$ui_data <- renderUI({
  tagList(#includeCSS(file.path(r_path,"base/www/style.css")),
    #includeScript(file.path(r_path,"base/www/js/returnTextAreaBinding.js")),
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        wellPanel(
          uiOutput("ui_datasets"),

          conditionalPanel(
            "input.tabs_data != 'Manage'",
            checkboxInput(
              'show_filter', 'Filter (e.g., price > 5000)', value = state_init("show_filter",FALSE)
            ),
            conditionalPanel(
              "input.show_filter == true",
              returnTextAreaInput(
                "data_filter", label = "", value = state_init("data_filter")
              ),
              uiOutput("ui_filter_error")
            )
          ),

          ################# Include selectize prompt Studies, Clinical data and Profile data
          selectizeInput(
            'StudiesID', 'Studies List', choices = NULL, multiple = FALSE
          ),
          uiOutput("ui_Cases"),
          uiOutput("ui_GenProfs"),
          uiOutput("ui_GeneList")
          #selectizeInput('GeneListID', 'Gene List', choices=NULL, multiple = FALSE)

          ##################

        ),
        conditionalPanel("input.tabs_data == 'Clinical'", uiOutput("ui_ClinicalData")),
        conditionalPanel("input.tabs_data == 'ProfData'", uiOutput("ui_ProfData")),
        conditionalPanel("input.tabs_data == 'MutData'", uiOutput("ui_MutData")),
        conditionalPanel("input.tabs_data == 'Circomics'", uiOutput("ui_Circomics")),
        conditionalPanel("input.tabs_data=='Network'",
                         wellPanel(
                           uiOutput("ui_NetworkSlider"),
                           uiOutput("ui_Provider")
                         )),
        conditionalPanel("input.tabs_data=='Classifier'",

                         uiOutput("ui_Classifier")


        ),
        conditionalPanel("input.tabs_data == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel("input.tabs_data == 'View'",uiOutput("ui_View")),
        # conditionalPanel("input.tabs_data == 'View_old'",uiOutput("ui_View_old")),
        conditionalPanel("input.tabs_data == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel("input.tabs_data == 'Pivot'",uiOutput("ui_Pivot")),
        conditionalPanel("input.tabs_data == 'Explore'", uiOutput("ui_Explore")),
        conditionalPanel("input.tabs_data == 'Transform'", uiOutput("ui_Transform")),
        conditionalPanel("input.tabs_data == 'Combine'", uiOutput("ui_Combine"))),
      # conditionalPanel("input.tabs_data == 'Simulate'", uiOutput("ui_Simulater"))),
      mainPanel(
        tabsetPanel(
          id = "tabs_data",
          tabPanel("Studies", DT::dataTableOutput(outputId = "StudiesTable")),
          tabPanel("Clinical", DT::dataTableOutput(outputId="ClinicalDataTable")),
          tabPanel("ProfData", DT::dataTableOutput(outputId ="ProfDataTable")),
          tabPanel("MutData", DT::dataTableOutput(outputId ="MutDataTable")),
          tabPanel("Circomics",
                   conditionalPanel("input.WheelID =='init'",
                                    h3("Available Profiles data in select Studies", align="center"),
                                    DT::dataTableOutput(outputId ="CircosInit")),


                   conditionalPanel(
                     "input.WheelID == 'Zoom'",
                     h3("Profiles Data: CNA, Exp, RPPA, miRNA: (Up, Down)"),
                     coffeewheelOutput('getCoffeeWheel', width = 600, height = 600),
                     h3("Correlation of silencing gene by Methylation: (0:1)"),
                     coffeewheelOutput('getCoffeeWheel_Met', width = 600, height = 600),
                     h3("Mutation Frequency: (Min,Max)"),
                     coffeewheelOutput('getCoffeeWheel_Mut', width = 600, height = 600)
                     # uiOutput("dataDescriptionHTML")
                   )
                   # conditionalPanel("input.WheelID == 'Zoom'", uiOutput("dataDescriptionHTML"))

                   #              conditionalPanel(
                   #               "input.WheelID == 'Static'",
                   #               metabologramOutput('metabologram')
                   #             )


          ),

          tabPanel("Network",
                   h3("Simple Network"),
                   networkD3::simpleNetworkOutput("simpleNetwork"),
                   h3("Forced Network"),
                   networkD3::forceNetworkOutput("forceNetwork")
          ),
          tabPanel("Classifier",
                   conditionalPanel("input.ClassID =='Samples'",
                                    h5("Enter sampling size smaller than in Studies"),
                                    tableOutput("viewTableCases"),
                                    verbatimTextOutput("SampleSize"),
                                    verbatimTextOutput("ClassifierThreshold"),
                                    #selectizeInput('StudiesIDClassifier', 'Studies Classification', choices=NULL, multiple = TRUE),
                                    h3("Filter only Studies that have mRNA data", align="center"),
                                    fluidRow(
                                      column(6,
                                             uiOutput("list_Cases")),
                                      column(6,
                                             uiOutput("list_GenProfs")
                                      )
                                    ),
                                    textOutput("PrintCases")


                   ),
                   conditionalPanel("input.ClassID =='Classifier'",
                                    tableOutput("viewTablegetGenesClassifier")),

                   conditionalPanel("input.ClassID=='Plot'",
                                    h4("Which Disease are involved your Genes list", align='center'),
                                    plotOutput("Plot_enricher"),
                                    h4("Diseases Studies Genes associations", align='center'),
                                    plotOutput("compareClusterDO"),
                                    h4("Pathway cluster Enrichment", align='center'),
                                    plotOutput("compareClusterPathway"),
                                    h4("Gene Ontholgy Studies associations", align='center'),
                                    plotOutput("compareClusterGO"),
                                    h4("KEGG Pathway Enrichment", align='center'),
                                    plotOutput("compareClusterKEGG")
                   )
          ),

          ##########


          tabPanel(
            "Manage", htmlOutput("htmlDataExample"),
            conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),
            conditionalPanel(
              "input.man_add_descr == true", uiOutput("dataDescriptionMD")
            )
          ),

          tabPanel("View", DT::dataTableOutput("dataviewer")),
          tabPanel("Visualize",plotOutput("visualize", width = "100%", height = "100%")),
          tabPanel("Pivot", rpivotTable::rpivotTableOutput("pivotData")),
          tabPanel("Explore", verbatimTextOutput("expl_summary"), plotOutput("expl_plots", width = "100%", height = "100%")),
          tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary")),
          tabPanel("Combine", htmlOutput("cmb_data1"), htmlOutput("cmb_data2"),
                   htmlOutput("cmb_possible"), htmlOutput("cmb_data"))
          # tabPanel("Simulate", verbatimTextOutput("sim_summary"),
          #          plotOutput("sim_plots", width = "100%", height = "100%"))
          # tabPanel("Generate", HTML("<h3>Generate input data for simulation and prediction</h3>")),
          # , selected = ifelse(is_empty(r_url$tab), "Manage", r_url$tab)
        )
      )
    ))
})
