
output$ui_FIsFilter <- renderUI({
  FIs_Att <- c("activat","inhibit","predicted","catalyze","reaction","phosphoryl","regulat","express",
               "complex","regulat","binding","compound","input","dissociation","indirect","ubiquitinated" )


  selectizeInput("FIs_AttId", label = "FIs Attributes:", choices = FIs_Att,
                 selected = state_multiple("FIs_AttId",FIs_Att, "activat"), multiple = TRUE)

})

output$ui_UseLinker <- renderUI({
  UseLinkers <- c("FALSE", "TRUE")


  selectizeInput("UseLinkerId", label = "Use Linkers:", choices = UseLinkers,
                 selected = "FALSE", multiple = FALSE)

})


output$ui_ReacLayout <- renderUI({
  Layouts <- c("dot","neato","twopi","circo")


  selectizeInput("ReacLayoutId", label = "Layouts:", choices = Layouts,
                 selected = "dot", multiple = FALSE)

})

# if(!exists('input.ClassID', envir = .GlobalEnv)){
#   output$ui_ClassEnrich <- renderUI({
#     ClassEnrich <- c("Freq. Interaction")
#     selectizeInput("ClassEnrichID", label= "Classifier:", choices= ClassEnrich,
#                    selected= "Freq. Interaction", multiple=FALSE)
#
#
#   })
# }else if(exists('input.ClassID', envir = .GlobalEnv)) {
output$ui_ClassEnrich <- renderUI({
  ClassEnrich <- c("Freq. Interaction", "Cancer/mRNA")
  selectizeInput("ClassEnrichID", label= "Classifier:", choices= ClassEnrich,
                 selected= "Freq. Interaction", multiple=FALSE)


})
#}


output$ui_Reactome <- renderUI({
  tagList(
    conditionalPanel(condition = "input.ReacRunId==true",
                     actionButton("ReacGeneListId", "load Reactome Genes")
    ),
    h4("Edges Attributes:"),
    wellPanel(
      uiOutput("ui_FIsFilter"),
      uiOutput("ui_UseLinker"),
      uiOutput("ui_ReacLayout")

    ),
    ## Attributes Nodes from geNetClassifier (Only if Class is pressed)
    conditionalPanel(condition = "input.ClassID == 'Classifier'",
                     h4("Node Attributes:"),
                     wellPanel(
                       uiOutput("ui_ClassEnrich")
                     )

    ),
    checkboxInput("ReacRunId", "Run", value = FALSE)
  )
})


## load genelist from clipBoard
observe({
  # 'reading' data from clipboard
  if (not_pressed(input$ReacGeneListId)) return()
  isolate({
    r_data[['genelist']] <- c(r_data[['genelist']], 'Reactome_GeneList') %>% unique

  })
})



