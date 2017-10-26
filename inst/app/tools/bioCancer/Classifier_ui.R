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

    div(class="row",
        div(class="col-xs-8",
            conditionalPanel("input.StudiesIDClassifier != null  && input.runSamplingBox==false",
                             h5('Sampling')),
            conditionalPanel("input.StudiesIDClassifier != null && input.runSamplingBox==true",
                             h5('Sampling', style = "color:#428bca")
            )
        ),
        div(class="col-xs-4",
            conditionalPanel("input.StudiesIDClassifier != null",
                             switchButton(inputId = "runSamplingBox",
                                          value = FALSE, col = "GB", type = "OO"))
        )
    ),
   # tags$hr(),
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
                                       #div(class="col-xs-12",
                                      #     checkboxInput('runClassificationBox', 'Classification')
                                      # ),
                                       
                                       div(class="row",
                                           div(class="col-xs-8",
                                               conditionalPanel("input.runSamplingBox == true  && input.runClassificationBox==false",
                                                                h5('Run classification')),
                                               conditionalPanel("input.StudiesIDClassifier != null && input.runClassificationBox==true",
                                                                h5('Run classification', style = "color:#428bca")
                                               )
                                           ),
                                           div(class="col-xs-4",
                                               conditionalPanel("input.runSamplingBox == true",
                                                                switchButton(inputId = "runClassificationBox",
                                                                             value = FALSE, col = "GB", type = "OO"))
                                           )
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
    help_and_report(modal_title = "Classifier", fun_name = "ClassifierHelp",
                    author = "Karim Mezhoud",
                    help_file = inclRmd(file.path(
                      getOption("radiant.path.bioCancer"),"app/tools/help/Classifier.md")))
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
