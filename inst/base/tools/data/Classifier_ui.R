updateSelectizeInput(session, 'ClusterPlotsID',
                     choices=c("None","GeneList/Diseases","Disease Ontology","Pathway","GO","KEGG"),
                     selected = "None")

updateSelectizeInput(session, 'ClassID',
                     choices= c("None"="None", "Samples"="Samples" ,"Classifier" = "Classifier"),
                     selected = "None")

output$ui_Classifier <- renderUI({

  updateSelectizeInput(session, 'StudiesIDClassifier', choices = Studies[,1],
                       selected = c("brca_tcga","gbm_tcga","lihc_tcga","lusc_tcga"))

  wellPanel(

    #conditionalPanel("input.tabs_Enrichment == 'Classifier'",

                     selectizeInput('StudiesIDClassifier', 'Studies  to Classify', choices=NULL, multiple = TRUE),

                     # selectizeInput(inputId = "ClassID", label="Sampling",
                     #                choices= c("None"="None","Samples"="Samples"), #,"Classifier" = "Classifier"
                     #                multiple=FALSE
                     # ),
                     div(class="col-xs-6",
                         conditionalPanel("input.StudiesIDClassifier != null",
                         checkboxInput('runSamplingBox', 'Sampling')
                     )),
                     tags$hr(),
                     #conditionalPanel( "input.ClassID == 'Samples'",
                     conditionalPanel( "input.runSamplingBox == true ",
                                       div(class="col-xs-6",
                                           numericInput("SampleSizeClassifierID",
                                                        "Samples",
                                                        "50",min = 0, max = 200 , step = 10)),
                                       div(class="col-xs-6",
                                           numericInput("ClassifierThresholdID",
                                                        "PostProb",
                                                        "0.95",min = 0.9, max = 1 , step = 0.01)),

                                       conditionalPanel("input.runSamplingBox == true",
                                       div(class="col-xs-12",
                                            checkboxInput('runClassificationBox', 'Classification')
                                           )
                                       )

                     ),
                     br(),

  #  ),
    #conditionalPanel("input.ClassID=='Classifier'",
    conditionalPanel("input.runClassificationBox == true",

                     selectizeInput('ClusterPlotsID', 'Biological terms clusters',
                                    choices=c("None","GeneList/Diseases","Disease Ontology","Reactome","GO","KEGG", "Cellular Component"),
                                    multiple= FALSE)

    ),
    br(),
    help_modal_km('Classification','ClassifierHelp',inclMD(file.path(r_path,"base/tools/help/Classifier.md")))
  )
})



# observe({
#   if(not_pressed(input$runClassificationButton)) return()
#   isolate({
#     shiny::withProgress(message = 'running geNetClassifier... ', value = 0.1, {
#       Sys.sleep(0.25)
#       getListProfData(panel='Circomics',input$GeneListID)
#     })
#
#   })
# })
