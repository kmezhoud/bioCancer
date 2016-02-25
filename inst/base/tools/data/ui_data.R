#######################################
# Shiny interface for data functions
#######################################
## show error message from filter dialog
output$ui_filter_error <- renderUI({
  if (is_empty(r_data$filter_error))
    return()
  helpText(r_data$filter_error)
})

## Welcome message
output$Welcome <- renderUI({
  wellPanel(
    h5("Welcome to bioCancer!", align="center"),
    HTML("Help is available on each page by clicking the <i title='Help' class='fa fa-question'></i> icon on the bottom left of your screen.")
  )
})

## data ui and tabs
output$ui_data <- renderUI({

  tagList(
    includeCSS(file.path(r_path,"base/www/style.css")),
    # includeScript(file.path(r_path,"base/www/js/returnTextAreaBinding.js")),
    # includeScript(file.path(r_path,"base/www/js/returnTextInputBinding.js")),

    sidebarLayout(
      sidebarPanel(

        #### Include selectize prompt Studies, Clinical data and Profile data
        conditionalPanel("input.tabs_data== 'Portal'",
                         conditionalPanel("input.tabs_portal=='Studies'",
                                          #h5("Welcome to bioCancer!", align="center"),
                                          uiOutput("Welcome")
                                          #p("Documentation and tutorials are available in each panel <i title='Help' class='fa fa-question'></i>  or 'Help' menu.", align ="center")
                         ),
                         selectizeInput(
                           'StudiesID', 'Studies', choices = NULL, multiple = FALSE
                         ),
                         uiOutput("ui_Cases"),
                         uiOutput("ui_GenProfs"),
                         uiOutput("ui_GeneList")
                         #selectizeInput('GeneListID', 'Gene List', choices=NULL, multiple = FALSE)
        ),

        conditionalPanel("input.tabs_data=='Portal'",
                         conditionalPanel("input.tabs_portal=='Studies'", uiOutput("ui_Studies")),
                         conditionalPanel("input.tabs_portal == 'Clinical'", uiOutput("ui_ClinicalData")),
                         conditionalPanel("input.tabs_portal == 'Profiles'", uiOutput("ui_ProfData")),
                         conditionalPanel("input.tabs_portal == 'Mutation'", uiOutput("ui_MutData"))
        ),
        conditionalPanel("input.tabs_data== 'Enrich'",
                         conditionalPanel("input.tabs_Enrich == 'Circomics'",
                                          uiOutput("ui_Circomics")),
                         #                          conditionalPanel("input.tabs_Enrich=='Network'",
                         #                                           wellPanel(
                         #                                             uiOutput("ui_NetworkSlider"),
                         #                                             uiOutput("ui_Provider")
                         #                                           )),

                         conditionalPanel("input.tabs_Enrich=='Classifier'",
                                          uiOutput("ui_Classifier")
                         ),

                         conditionalPanel("input.tabs_Enrich=='Reactome'",
                                          uiOutput("ui_Reactome")
                         ),
                         conditionalPanel("input.tabs_Enrich=='Network'",
                                          uiOutput("ui_Network")
                         )),

         conditionalPanel("input.tabs_data== 'Handle'",
#
#                          conditionalPanel("input.tabs_Handle=='Manage'",
#                                           uiOutput("Welcome")
#
#                          ),


                         uiOutput("ui_datasets"),

                         conditionalPanel(
                           "input.tabs_Handle != 'Manage'",
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
                         )
        ),

        conditionalPanel("input.tabs_data == 'Handle'",

                         conditionalPanel("input.tabs_Handle == 'Manage'", uiOutput("ui_Manage")),
                         conditionalPanel("input.tabs_Handle == 'View'", uiOutput("ui_View")),
                         conditionalPanel("input.tabs_Handle == 'Visualize'", uiOutput("ui_Visualize")),
                         conditionalPanel("input.tabs_Handle == 'Pivot'",uiOutput("ui_Pivotr")),
                         conditionalPanel("input.tabs_Handle == 'Explore'",uiOutput("ui_Explore")),
                         conditionalPanel("input.tabs_Handle == 'Transform'", uiOutput("ui_Transform")),
                         conditionalPanel("input.tabs_Handle == 'Combine'", uiOutput("ui_Combine"))

        )

      ),
      mainPanel(
        tabsetPanel(
          id = "tabs_data",

          tabPanel("Portal", uiOutput("Portal")),
          tabPanel("Enrich", uiOutput("Enrich")),
          tabPanel("Handle", uiOutput("Handle"))




        )
      )
    ))
})

output$Portal <- renderUI({
  tabsetPanel(id = "tabs_portal",
              tabPanel("Studies",
                       downloadLink("dl_Studies_tab", "", class = "fa fa-download alignright"),
                       DT::dataTableOutput(outputId = "StudiesTable")),
              tabPanel("Clinical",
                       downloadLink("dl_Clinical_tab", "", class = "fa fa-download alignright"),
                       DT::dataTableOutput(outputId="ClinicalDataTable")),
              tabPanel("Profiles",
                       downloadLink("dl_ProfData_tab", "", class = "fa fa-download alignright"),
                       DT::dataTableOutput(outputId ="ProfDataTable")),
              tabPanel("Mutation",
                       downloadLink("dl_MutData_tab", "", class = "fa fa-download alignright"),
                       DT::dataTableOutput(outputId ="MutDataTable"))
  )
})


output$Enrich <- renderUI({

  tabsetPanel(id = "tabs_Enrich",

              tabPanel("Circomics",
#                        if('CNA' %in% input$CircosDimensionID ){
#                          plot_downloader("SaveMetabologram_CNA", pre = "")
#                        },
                       if('CNA' %in% input$CircosDimensionID ){
                         coffeewheelOutput('getCoffeeWheel_CNA', width = 600, height = 600)
                       },
                       if('Methylation' %in% input$CircosDimensionID ){
                         plot_downloader("SaveMetabologram_Met", pre = "")
                       },
                       if('Methylation' %in% input$CircosDimensionID ){
                         #  h3("Correlation of silencing gene by Methylation: (0:1)")
                         coffeewheelOutput('getCoffeeWheel_Met', width = 600, height = 600)
                       },
#                        if('mRNA' %in% input$CircosDimensionID ){
#                          plot_downloader("SaveMetabologram_mRNA", pre = "")
#                        },
                       if('mRNA' %in% input$CircosDimensionID ){
                         # h3("Gene Expression")
                         coffeewheelOutput('getCoffeeWheel_mRNA', width = 600, height = 600)
                       },
#                        if('Mutation' %in% input$CircosDimensionID ){
#                          plot_downloader("SaveMetabologram_Mut", pre = "")
#                        },
                       if('Mutation' %in% input$CircosDimensionID ){
                         # h3("Mutation Frequency: (Min,Max)")
                         coffeewheelOutput('getCoffeeWheel_Mut', width = 600, height = 600)
                       },
#                        if('miRNA' %in% input$CircosDimensionID ){
#                          plot_downloader("SaveMetabologram_miRNA", pre = "")
#                        },

                       if('miRNA' %in% input$CircosDimensionID ){
                         #h3("Protein phosphorylation:")
                         coffeewheelOutput('getCoffeeWheel_miRNA', width = 600, height = 600)
                       },
#                        if('RPPA' %in% input$CircosDimensionID ){
#                          plot_downloader("SaveMetabologram_RPPA", pre = "")
#                        },

                       if('RPPA' %in% input$CircosDimensionID ){
                         #h3("Protein phosphorylation:")
                         coffeewheelOutput('getCoffeeWheel_RPPA', width = 600, height = 600)
                       },
                       if('All' %in% input$CircosDimensionID ){
                         plot_downloader("SaveMetabologram_All", pre = "")
                       },
                       if('All' %in% input$CircosDimensionID ){
                         #h3("Profiles Data: CNA, Exp, RPPA, miRNA: (Up, Down)")
                         coffeewheelOutput('getCoffeeWheel_All', width = 800, height = 800)

                       },
                       conditionalPanel(condition = "input.getlistProfDataCircosID ==true",
                                        h3("Loaded Profiles Data", align="center"),
                                        verbatimTextOutput("StrListProfDataCircos")
                       ),

                       #                        conditionalPanel("input.WheelID =='init'",
                       #                                         h3("Available Profiles data in select Studies", align="center"),
                       #                                         DT::dataTableOutput(outputId ="CircosInit")),
                       #
                       #
                       #                        conditionalPanel("input.WheelID == 'Zoom'",
                       #                                         h3("Profiles Data: CNA, Exp, RPPA, miRNA: (Up, Down)"),
                       #
                       #                                         coffeewheelOutput('getCoffeeWheel_All', width = 600, height = 600),
                       #
                       #                                         h3("Correlation of silencing gene by Methylation: (0:1)"),
                       #                                         coffeewheelOutput('getCoffeeWheel_Met', width = 600, height = 600),
                       #
                       #                                         h3("Mutation Frequency: (Min,Max)"),
                       #                                         coffeewheelOutput('getCoffeeWheel_Mut', width = 600, height = 600)
                       #                                         # uiOutput("dataDescriptionHTML")
                       #                        )
                       # conditionalPanel("input.WheelID == 'Zoom'", uiOutput("dataDescriptionHTML"))

                       #                                     conditionalPanel(
                       #                                      "input.WheelID == 'Static'",
                       #                                      metabologramOutput('metabologram')
                       #                                    )
                       conditionalPanel(condition = "input.ViewProfDataCircosID==true",
                                        h3("Available Profiles data in select Studies", align="center"),
                                        DT::dataTableOutput(outputId ="CircosAvailability")
                       ),
                       conditionalPanel("input.getlistProfDataCircosID==false",
                                        verbatimTextOutput("CircomicsHowto")
                       )

              ),

              #               tabPanel("Network",
              #                        h3("Simple Network"),
              #                        networkD3::simpleNetworkOutput("simpleNetwork"),
              #                        h3("Forced Network"),
              #                        networkD3::forceNetworkOutput("forceNetwork")
              #               ),
              tabPanel("Classifier",
                       conditionalPanel("input.ClassID=='None'",
                                        verbatimTextOutput("ClassifierHowto")
                       ),
                       conditionalPanel("input.ClassID =='Samples'",
                                        h4("Enter sampling size smaller than in Case"),
                                        #tableOutput("viewTableCases"),
                                        DT::dataTableOutput("viewTableCases"),
                                        #verbatimTextOutput("SampleSize"),
                                        #verbatimTextOutput("ClassifierThreshold"),
                                        #selectizeInput('StudiesIDClassifier', 'Studies Classification', choices=NULL, multiple = TRUE),
                                        h4("Filtering Studies with mRNA data", align="center"),
                                        fluidRow(
                                          column(6,
                                                 uiOutput("list_Cases")),
                                          column(6,
                                                 uiOutput("list_GenProfs")
                                          )
                                        )
                                        # textOutput("PrintCases")


                       ),
                       conditionalPanel("input.ClassID =='Classifier'",
                                        downloadLink("dl_GenesClassDetails_tab", "", class = "fa fa-download alignright"),
                                        #tableOutput("viewTablegetGenesClassifier")),
                                        DT::dataTableOutput("getGenesClassifier"),

                                        # conditionalPanel("input.ClassID=='Plot'",

                                        conditionalPanel("input.ClusterPlotsID=='GeneList/Diseases'",
                                                         h4("Which Disease are involving your Genes list", align='center'),
                                                         plot_downloader("Plot_enrich", pre = ""),
                                                         plotOutput("Plot_enricher")
                                        ),
                                        conditionalPanel("input.ClusterPlotsID=='Disease Onthology'",
                                                         h4("Diseases Studies Genes associations", align='center'),
                                                         plot_downloader("compareClusterDO", pre=""),
                                                         plotOutput("compareClusterDO")
                                        ),
                                        conditionalPanel("input.ClusterPlotsID=='Reactome'",
                                                         h4("Reactome Pathway cluster Enrichment", align='center'),
                                                         plot_downloader("compareClusterReactome", pre=""),
                                                         plotOutput("compareClusterReactome")
                                        ),
                                        conditionalPanel("input.ClusterPlotsID=='GO'",
                                                         h4("Gene Ontholgy Studies associations", align='center'),
                                                         plot_downloader("compareClusterGO", pre=""),
                                                         plotOutput("compareClusterGO")
                                        ),
                                        conditionalPanel("input.ClusterPlotsID=='KEGG'",
                                                         h4("KEGG Pathway Enrichment", align='center'),
                                                         plot_downloader("compareClusterKEGG", pre=""),
                                                         plotOutput("compareClusterKEGG")
                                        ),
                                        conditionalPanel("input.ClusterPlotsID=='Cellular Component'",
                                                         h4("Cellular Component Enrichment", align='center'),
                                                         plot_downloader("compareClusterCC", pre=""),
                                                         plotOutput("compareClusterCC")
                                        )
                                        )
                       #)
              ),
              tabPanel("Reactome",
                       conditionalPanel(condition = "input.ReacRunId == false",
                                        verbatimTextOutput("ReactomeHowto")
                       ),
                       conditionalPanel(condition = "input.ReacRunId== true",
                                         #plot_downloader("ld_diagrammeR_plot", pre=""),
                                         downloadButton('Save_diagrammeR_plot', 'HTML'),
                                         #actionLink("ReactomeFI_save_plot", "", class = "fa fa-download alignright", onclick = "window.print();"),
                                         grVizOutput('diagrammeR'),
                                         conditionalPanel(condition= "input.TypeGeneSetID == 'Pathway'||
                                                    input.TypeGeneSetID == 'BP'||
                                                    input.TypeGeneSetID == 'CC'||
                                                    input.TypeGeneSetID == 'MF'",
                                                          #tableOutput("GeneSetLegend")
                                                          downloadLink("dl_GeneSet_Legend", "", class = "fa fa-download alignright"),
                                                          DT::dataTableOutput("GeneSet_Legend")
                                         )


                                         #                         conditionalPanel(condition= "input.NodeAttri_ReactomeID == 'FreqInt./GeneSet'",
                                         #                                          #tableOutput("GeneSetLegend")
                                         #                                          DT::dataTableOutput("GeneSet_Legend")
                                         #                         )

                       ),
                       conditionalPanel(condition= "input.ReacLegendId == true",
                                        h4("Legend", align="center"),
                                        imageOutput("ReactomeLegend")
                       ),

                       conditionalPanel(condition = "input.ViewProfDataReactomeID==true",
                                        h3("Available Profiles data in select Studies", align="center"),
                                        DT::dataTableOutput(outputId ="ReactomeAvailability")
                       ),
                       conditionalPanel( condition = "input.NetworkRunId== true",
                                         visNetwork::visNetworkOutput("network",height = "600px")
                       ),
                       conditionalPanel(condition = "input.getlistProfDataID ==true",
                                        h3("Loaded Profiles Data", align="center"),
                                        verbatimTextOutput("StrListProfData")
                                        #htmlOutput("StrListProfData")


                       )

                       #                    conditionalPanel(condition = "input.getlistProfDataID == 'Availability'",
                       #                                     h3("Available Profiles data in select Studies", align="center"),
                       #                                     DT::dataTableOutput(outputId ="ReactomeView")),
                       #
                       #                    conditionalPanel(condition = "input.getlistProfDataID == 'Load'",
                       #                                     h3("Load Profiles Data", align="center"),
                       #                                     verbatimTextOutput("StrListProfData")
                       #                   )


              )
# tabPanel("Network",
#          conditionalPanel( condition = "input.NetworkRunId== true",
#          visNetworkOutput("network",height = "600px")
#          )
# )

  )


})

output$Handle <- renderUI({
  tabsetPanel(id = "tabs_Handle",
              tabPanel(
                "Manage", htmlOutput("htmlDataExample"),
                conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),

                conditionalPanel("input.man_add_descr == true", uiOutput("dataDescriptionMD"))),
              tabPanel("View",
                       downloadLink("dl_view_tab", "", class = "fa fa-download alignright"),
                       DT::dataTableOutput("dataviewer")),

              # tabPanel("View", DT::dataTableOutput("dataviewer"), verbatimTextOutput("tbl_state")),
              tabPanel("Visualize",
                       plot_downloader(".visualize", width = viz_plot_width(), height = viz_plot_height(), pre = ""),
                       plotOutput("visualize", width = "100%", height = "100%")),
              tabPanel("Pivot",
                       conditionalPanel("input.pvt_tab == true",
                                        downloadLink("dl_pivot_tab", "", class = "fa fa-download alignright"),
                                        DT::dataTableOutput("pivotr")
                       ),
                       conditionalPanel("input.pvt_chi2 == true", htmlOutput("pivotr_chi2")),
                       conditionalPanel("input.pvt_plot == true",
                                        HTML("<br><br>"),
                                        plot_downloader("pivot", width = pvt_plot_width(), height = pvt_plot_height()),
                                        plotOutput("plot_pivot", width = "100%", height = "100%")
                       )
              ),
              tabPanel("Explore",
                       downloadLink("dl_explore_tab", "", class = "fa fa-download alignright"),
                       DT::dataTableOutput("explorer")),
              tabPanel("Transform",
                       htmlOutput("transform_data"),
                       verbatimTextOutput("transform_summary"),
                       uiOutput("ui_tr_log")),
              tabPanel("Combine", htmlOutput("cmb_data1"), htmlOutput("cmb_data2"),
                       htmlOutput("cmb_possible"), htmlOutput("cmb_data"))


  )
})
