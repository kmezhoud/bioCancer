updateSelectizeInput(session, 'ClusterPlotsID',
                     choices=c("None","GeneList/Diseases","Disease Onthology","Pathway","GO","KEGG"),
                     selected = "None")

updateSelectizeInput(session, 'ClassID',
                     choices= c("None"="None", "Samples"="Samples" ,"Classifier" = "Classifier"),
                     selected = "None")

output$ui_Classifier <- renderUI({
  withProgress(message = 'Loading Studies with mRNA Data. Waiting...', value = 0.2, {
    Sys.sleep(0.25)
#   ## remove Studies without mrna in genetic profiles
#   listCases <- lapply(Studies[,1], function(x)getCaseLists(cgds,x)[,1])
#   names(listCases) <- Studies[,1]
#   listCases <- lapply(listCases, function(x) x[grep("mrna", x)])
#   listCases <- listCases[lapply(listCases,length)>0]
})
  updateSelectizeInput(session, 'StudiesIDClassifier', choices = Studies[,1], selected = c("brca_tcga","gbm_tcga","lihc_tcga","lusc_tcga"))



#   checked_Studies <- input$StudiesIDClassifier
#   listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,1])
#   updateSelectizeInput(session, 'CasesIDClassifier', choices = listCases)

  wellPanel(

    conditionalPanel("input.tabs_Analysis == 'Classifier'",

                     selectizeInput('StudiesIDClassifier', 'Studies  to Classify', choices=NULL, multiple = TRUE),

                     div(class="col-xs-6",
                         numericInput("SampleSizeClassifierID",
                                      "Samples",
                                      "50",min = 0, max = 200 , step = 10)),
                     div(class="col-xs-6",
                         numericInput("ClassifierThresholdID",
                                      "PostProb",
                                      "0.95",min = 0.9, max = 1 , step = 0.01)),

#                      radioButtons(inputId = "ClassID", label = "Processing",
#                                   c("Samples"="Samples" ,"Classifier" = "Classifier", "Plot"="Plot"),
#                                   selected = "Samples", inline = TRUE),

                     selectizeInput(inputId = "ClassID", label="Processing",
                                    choices= c("None"="None","Samples"="Samples" ,"Classifier" = "Classifier"),
                                    multiple=FALSE
                                    )



                     #selectizeInput('CasesIDClassifier', 'Cases', choices=NULL, multiple = TRUE),
                     #uiOutput("list_Cases")



    ),
    conditionalPanel("input.ClassID=='Classifier'",
                     selectizeInput('ClusterPlotsID', 'Plot Clusters',
                                    choices=c("None","GeneList/Diseases","Disease Onthology","Pathway","GO","KEGG"),
                                    multiple= FALSE)
                     )



  )
})

