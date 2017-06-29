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
    HTML("Help is available on each page by clicking the <i title='Help' class='fa fa-question'></i> icon on the bottom left of your screen."),
    checkboxInput(
      'overview_id', 'Pipeline Overview', value = FALSE
    )
  )
})

output$pipeline <- renderUI({
  tagList(
  h4("Schematic view of bioCancer pipeline:"),
  h5(" The pipeline consists of three major panels:"),
  h5("1- Portal loads own gene list and explore profiles data of cancer studies."),
  h5("2- Processing exchanges data with user computer, Portal section, and Circomics tab.
     It provides useful tools for pre-processing, processing, and plotting graphs."),
  h5("3- Enrichment integrates methods for classification and clustering and displays the results as interactive graphs.")
  )
})

output$overview <- renderImage({

  list(src = file.path(r_path,"base/tools/help/figures/overview_methods.png"),
       contentType = 'image/png',
       width = 600,
       height = 500,
       deleteFile = FALSE,
       alt = "This is alternate text"
       )}, deleteFile = FALSE
)
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
        conditionalPanel("input.tabs_data == 'Enrichment'",
                         conditionalPanel("input.tabs_Enrichment == 'Circomics'",
                                          uiOutput("ui_Circomics")),
                         #                          conditionalPanel("input.tabs_Enrichment=='Network'",
                         #                                           wellPanel(
                         #                                             uiOutput("ui_NetworkSlider"),
                         #                                             uiOutput("ui_Provider")
                         #                                           )),

                         conditionalPanel("input.tabs_Enrichment =='Classifier'",
                                          uiOutput("ui_Classifier")
                         ),

                         conditionalPanel("input.tabs_Enrichment =='Networking'",
                                          uiOutput("ui_Reactome")
                         )
                         # conditionalPanel("input.tabs_Enrichment=='Network'",
                         #                  uiOutput("ui_Network")
                         # )
                         ),

        conditionalPanel("input.tabs_data== 'Processing'",
                         #
                         #                          conditionalPanel("input.tabs_Processing=='Manage'",
                         #                                           uiOutput("Welcome")
                         #
                         #                          ),


                         uiOutput("ui_datasets"),

                         conditionalPanel(
                           "input.tabs_Processing != 'Manage'",
                           checkboxInput(
                             'show_filter', 'Filter (e.g., CNA == -1)', value = state_init("show_filter",FALSE)
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

        conditionalPanel("input.tabs_data == 'Processing'",

                         conditionalPanel("input.tabs_Processing == 'Manage'", uiOutput("ui_Manage")),
                         conditionalPanel("input.tabs_Processing == 'View'", uiOutput("ui_View")),
                         conditionalPanel("input.tabs_Processing == 'Visualize'", uiOutput("ui_Visualize")),
                         conditionalPanel("input.tabs_Processing == 'Pivot'",uiOutput("ui_Pivotr")),
                         conditionalPanel("input.tabs_Processing == 'Explore'",uiOutput("ui_Explore")),
                         conditionalPanel("input.tabs_Processing == 'Transform'", uiOutput("ui_Transform")),
                         conditionalPanel("input.tabs_Processing == 'Combine'", uiOutput("ui_Combine"))

        )

      ),
      mainPanel(
        conditionalPanel("input.overview_id == true",
                         uiOutput("pipeline"),
                         imageOutput("overview")
        ),

        tags$hr(),

        tabsetPanel(
          id = "tabs_data",

          tabPanel("Portal", uiOutput("Portal")),
          tabPanel("Processing", uiOutput("Processing")),
          tabPanel("Enrichment", uiOutput("Enrichment"))




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

output$Enrichment <- renderUI({

  tabsetPanel(id = "tabs_Enrichment",
              tabPanel("Circomics",
                       tags$hr(),
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

                         tagList(

                           div(class="row",
                               div(class="col-xs-6",
                                   DT::dataTableOutput(outputId = "Sequenced_SampleSize")),
                               div(class="col-xs-6",
                                   DT::dataTableOutput(outputId = "FreqMutSummary"))
                           ),


                         h3(paste0("Mutation Percentage: (Min = ", min(r_data$Freq_DfMutData, na.rm = TRUE) ,
                                   "%, Max = ", max(r_data$Freq_DfMutData, na.rm = TRUE)  ,"%)", sep=""),  align="center"),

                         coffeewheelOutput('getCoffeeWheel_Mut', width = 600, height = 600)
                         )
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
                       conditionalPanel(condition = "input.loadListProfDataCircosId ==true",
                                        h3("Loaded Profiles Data", align="center"),
                                        verbatimTextOutput("StrListProfDataCircos")
                       ),
                       conditionalPanel(condition = "input.ViewProfDataCircosID==true",
                                        h3("Available Profiles data in select Studies", align="center"),
                                        DT::dataTableOutput(outputId ="CircosAvailability")
                       ),
                       conditionalPanel("input.loadListProfDataCircosId==false",
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
                       #conditionalPanel("input.ClassID=='None'",
                       conditionalPanel("input.runSamplingBox == false",
                                        verbatimTextOutput("ClassifierHowto")
                       ),
                       #conditionalPanel("input.ClassID =='Samples' && input.runClassificationBox == false",
                       conditionalPanel("input.runSamplingBox == true && input.runClassificationBox == false",
                                        h4("Enter sampling size smaller than in Cases"),
                                        #tableOutput("viewTableCases"),
                                        DT::dataTableOutput("viewTableCases")

                                        # h4("Filtering Studies with mRNA data", align="center"),
                                        # fluidRow(
                                        #   column(6,
                                        #          uiOutput("list_Cases")),
                                        #   column(6,
                                        #          uiOutput("list_GenProfs")
                                        #   )
                                        # )

                                        # chooserInput("mychooser", "Available frobs", "Selected frobs",
                                        #              row.names(USArrests), c(), size = 10, multiple = TRUE
                                        # ),
                                        #  verbatimTextOutput("selection")
                                        # textOutput("PrintCases")


                       ),
                       #conditionalPanel("input.ClassID == 'Classifier'",
                       conditionalPanel("input.runClassificationBox == true",
                                        downloadLink("dl_GenesClassDetails_tab", "", class = "fa fa-download alignright"),
                                        #tableOutput("viewTablegetGenesClassifier")),
                                        DT::dataTableOutput("getGenesClassifier"),

                                        # conditionalPanel("input.ClassID=='Plot'",

                                        conditionalPanel("input.ClusterPlotsID=='GeneList/Diseases'",
                                                         h4("Which Diseases are involving your Genes list", align='center'),
                                                         plot_downloader("Plot_enrich", pre = ""),
                                                         plotOutput("Plot_enricher")
                                        ),
                                        conditionalPanel("input.ClusterPlotsID=='Disease Ontology'",
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
                                                         h4("Cancer / Gene Ontolgy associations", align='center'),
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
              tabPanel("Networking",
                       conditionalPanel(condition = "input.ReacRunId == false",
                                        verbatimTextOutput("ReactomeHowto")
                       ),
                       conditionalPanel(condition = "input.ReacRunId== true",
                                        #plot_downloader("ld_diagrammeR_plot", pre=""),
                                        downloadButton('Save_diagrammeR_plot', 'HTML'),
                                        #actionLink("ReactomeFI_save_plot", "", class = "fa fa-download alignright", onclick = "window.print();"),
                                        DiagrammeR::grVizOutput('diagrammeR'),
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
                       conditionalPanel(condition = "input.getlistProfDataIDReactome ==true",
                                        h3("Loaded Profiles Data", align="center"),
                                        #uiOutput("StrListProfDataCircos")
                                        verbatimTextOutput("StrListProfDataReactome")
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

output$Processing <- renderUI({
  tabsetPanel(id = "tabs_Processing",
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
