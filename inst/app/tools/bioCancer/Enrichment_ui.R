output$coffeewheels <- renderUI({

  tagList(
    #                        if('CNA' %in% input$CircosDimensionID ){
    #                          plot_downloader("SaveMetabologram_CNA", pre = "")},
    #
    if('CNA' %in% input$CircosDimensionID ){
      tagList(
        div(class="row",
            div(class="col-xs-6",
                DT::dataTableOutput(outputId = "Sequenced_SampleSize")),
            div(class="col-xs-6",
                DT::dataTableOutput(outputId = "CNA_Max"))
        ),
        tagList(
          actionButton("saveCircosCNA", "Save as png", style='padding:4px; font-size:80%'),
          column(12, align="center",
                 coffeewheelOutput('getCoffeeWheel_CNA', width = 600, height = 600)
          ))
      )
    },
    # if('Methylation' %in% input$CircosDimensionID ){
    #   plot_downloader("SaveMetabologram_Met", pre = "")
    # },
    if('Methylation' %in% input$CircosDimensionID ){
      tagList(
        div(class="row",
            div(class="col-xs-6",
                DT::dataTableOutput(outputId = "Sequenced_SampleSize")),
            div(class="col-xs-6",
                DT::dataTableOutput(outputId = "Methylation_mean"))
        ),
        tagList(
          actionButton("saveCircosMet", "Save as png", style='padding:4px; font-size:80%'),
          #  h3("Correlation of silencing gene by Methylation: (0:1)")
          column(12, align="center",
                 coffeewheelOutput('getCoffeeWheel_Met', width = 600, height = 600)
          ))
      )
    },
    #                        if('mRNA' %in% input$CircosDimensionID ){
    #                          plot_downloader("SaveMetabologram_mRNA", pre = "")
    #                        },
    if('mRNA' %in% input$CircosDimensionID ){
      tagList(
        div(class="row",
            div(class="col-xs-6",
                DT::dataTableOutput(outputId = "Sequenced_SampleSize")),
            div(class="col-xs-6",
                DT::dataTableOutput(outputId = "mRNA_mean"))
        ),
        tagList(
          actionButton("saveCircosMRNA", "Save as png", style='padding:4px; font-size:80%'),
          #h3("Gene Expression"),
          column(12, align="center",
                 coffeewheelOutput('getCoffeeWheel_mRNA', width = 600, height = 600)
          ))
      )
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
        tagList(
          actionButton("saveCircosMut", "Save as png", style='padding:4px; font-size:80%'),

          column(12, align="center",
                 coffeewheelOutput('getCoffeeWheel_Mut', width = 600, height = 600)
          ))
      )
    },
    #                        if('miRNA' %in% input$CircosDimensionID ){
    #                          plot_downloader("SaveMetabologram_miRNA", pre = "")
    #                        },

    if('miRNA' %in% input$CircosDimensionID ){
      tagList(
        actionButton("saveCircosMiRNA", "Save as png", style='padding:4px; font-size:80%'),
        #h3("Protein phosphorylation:")
        column(12, align="center",
               coffeewheelOutput('getCoffeeWheel_miRNA', width = 600, height = 600)
        ))
    },
    #                        if('RPPA' %in% input$CircosDimensionID ){
    #                          plot_downloader("SaveMetabologram_RPPA", pre = "")
    #                        },

    if('RPPA' %in% input$CircosDimensionID ){
      tagList(
        actionButton("saveCircosRPPA", "Save as png", style='padding:4px; font-size:80%'),
        #h3("Protein phosphorylation:")
        column(12, align="center",
               coffeewheelOutput('getCoffeeWheel_RPPA', width = 600, height = 600)
        )
      )
    },
    # if('All' %in% input$CircosDimensionID ){
    #   plot_downloader("SaveMetabologram_All", pre = "")
    # },
    if('All' %in% input$CircosDimensionID ){
      tagList(
        actionButton("saveCircosAll", "Save as png", style='padding:4px; font-size:80%'),
        #h3("Profiles Data: CNA, Exp, RPPA, miRNA: (Up, Down)")
        column(12, align="center",
               #downloadLink("dl_metabologram_All"),
               #plot_downloader("dl_metabologram_All", pre = ""),
               coffeewheelOutput('getCoffeeWheel_All', width = 800, height = 800)
        )
      )
    }
  )
})

output$Enrichment <- renderUI({
  tagList(
    includeCSS(file.path(getOption("radiant.path.bioCancer"),"app/www/switchButton.css")),
    includeCSS(file.path(getOption("radiant.path.bioCancer"),"app/www/style.css")),
    sidebarLayout(
      sidebarPanel(

        selectizeInput("GeneListIDEnrichment", "Gene List:", input$GeneListID),

        conditionalPanel("input.tabs_Enrichment == 'Circomics'",
                         uiOutput("ui_Circomics")),
        conditionalPanel("input.tabs_Enrichment == 'Classifier'",
                         uiOutput("ui_Classifier")),
        conditionalPanel("input.tabs_Enrichment == 'Networking'",
                         uiOutput("ui_Reactome"))

      ),
      mainPanel(
        tabsetPanel(id = "tabs_Enrichment",

                    tabPanel("Circomics",
                            # tags$hr(),

                             uiOutput('coffeewheels'),
                             conditionalPanel("input.CircosDimensionID == 'All'",
                                              column(12, align="center",

                                                     metabologramOutput('metabologram_All'))
                             ),

                             conditionalPanel(condition = "input.loadListProfDataCircosId == true",# && input.CircosDimensionID == null ",
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
                             conditionalPanel(condition = "input.plotDiagrammeRSwithButtonId == false",
                                              verbatimTextOutput("ReactomeHowto")
                             ),
                             conditionalPanel(condition = "input.plotDiagrammeRSwithButtonId== true",
                                              #plot_downloader("ld_diagrammeR_plot", pre=""),
                                              downloadButton('Save_diagrammeR_plot', 'HTML',style='padding:4px; font-size:80%'),
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
                             conditionalPanel( condition = "input.plotVisNetworkSwithButtonId == true",
                                               visNetwork::visNetworkOutput("network",height = "600px"),
                                               actionButton('saveVisNetworkWidget',  'HTML',style='padding:4px; font-size:80%')
                             ),
                             conditionalPanel(condition = "input.getlistProfDataIDReactome ==true",
                                              h3("Loaded Profiles Data", align="center"),
                                              #uiOutput("StrListProfDataCircos")
                                              verbatimTextOutput("StrListProfDataReactome")
                                              #htmlOutput("StrListProfData")


                             )
                             #
                             #          #                    conditionalPanel(condition = "input.getlistProfDataID == 'Availability'",
                             #          #                                     h3("Available Profiles data in select Studies", align="center"),
                             #          #                                     DT::dataTableOutput(outputId ="ReactomeView")),
                             #          #
                             #          #                    conditionalPanel(condition = "input.getlistProfDataID == 'Load'",
                             #          #                                     h3("Load Profiles Data", align="center"),
                             #          #                                     verbatimTextOutput("StrListProfData")
                             #          #                   )
                             #
                             #
                    )
                    # # tabPanel("Network",
                    # #          conditionalPanel( condition = "input.NetworkRunId== true",
                    # #          visNetworkOutput("network",height = "600px")
                    # #          )
                    # # )

        )
      )
    )
  )
})
