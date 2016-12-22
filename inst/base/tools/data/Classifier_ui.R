updateSelectizeInput(session, 'ClusterPlotsID',
                     choices=c("None","GeneList/Diseases","Disease Ontology","Pathway","GO","KEGG"),
                     selected = "None")

updateSelectizeInput(session, 'ClassID',
                     choices= c("None"="None", "Samples"="Samples" ,"Classifier" = "Classifier"),
                     selected = "None")

output$ui_Classifier <- renderUI({

  updateSelectizeInput(session, 'StudiesIDClassifier', choices = Studies[,1], selected = c("brca_tcga","gbm_tcga","lihc_tcga","lusc_tcga"))

  wellPanel(

    conditionalPanel("input.tabs_Enrichment == 'Classifier'",

                     selectizeInput('StudiesIDClassifier', 'Studies  to Classify', choices=NULL, multiple = TRUE),

                     selectizeInput(inputId = "ClassID", label="Processing",
                                    choices= c("None"="None","Samples"="Samples" ,"Classifier" = "Classifier"),
                                    multiple=FALSE
                     ),

                     div(class="col-xs-6",
                         numericInput("SampleSizeClassifierID",
                                      "Samples",
                                      "50",min = 0, max = 200 , step = 10)),
                     div(class="col-xs-6",
                         numericInput("ClassifierThresholdID",
                                      "PostProb",
                                      "0.95",min = 0.9, max = 1 , step = 0.01))

    ),
    conditionalPanel("input.ClassID=='Classifier'",
                     selectizeInput('ClusterPlotsID', 'Biological terms clusters',
                                    choices=c("None","GeneList/Diseases","Disease Ontology","Reactome","GO","KEGG", "Cellular Component"),
                                    multiple= FALSE)
    ),


    help_modal_km('Classification','ClassifierHelp',inclMD(file.path(r_path,"base/tools/help/Classifier.md")))
  )
})

