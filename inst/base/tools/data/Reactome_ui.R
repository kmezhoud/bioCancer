
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


output$ui_NodeAttri_ReactomeFI <- renderUI({
  ReactomeEnrich <- c("None","Freq. Interaction")
  selectizeInput("NodeAttri_ReactomeID", label= "From ReactomeFI:", choices= ReactomeEnrich,
                 selected= "None", multiple=FALSE)
})

output$ui_NodeAttri_Classifier <- renderUI({
  ClassEnrich <- c("None","mRNA","Studies", "mRNA/Studies")
  selectizeInput("NodeAttri_ClassifierID", label= "From Classifier:", choices= ClassEnrich,
                 selected= "None", multiple=FALSE)
})

output$ui_NodeAttri_ProfData <- renderUI({

  ProfDataEnrich <- c("None","CNA","mRNA", "Methylation","Mutation" )
  selectizeInput("NodeAttri_ProfDataID", label= "Select Profiles Data:", choices= ProfDataEnrich,
                 selected= "None", multiple=FALSE)
})
output$ui_Freq_MutSlider <- renderUI({

  sliderInput("FreqMutSliderID", "Mutation Percentage", 25, min = 1,
              max = 100, step = 1)

})
# output$ui_Freq_MutSlider2 <- renderUI({
#
#   sliderInput("FreqMutSliderID2", "Mutation Percentage", 25, min = 1,
#               max = 100, step = 1)
#
# })

output$ui_Reactome <- renderUI({
  updateSelectizeInput(session, 'StudiesIDReactome', choices = Studies[,1], selected = c("brca_tcga","gbm_tcga","lihc_tcga","lusc_tcga"))

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
    # conditionalPanel(condition = "input.ClassID == 'Samples'",
    h4("Node Attributes:"),
    wellPanel(
      uiOutput("ui_NodeAttri_ReactomeFI"),
      #conditionalPanel(condition = "input.ClassID=='Classifier'",
      uiOutput("ui_NodeAttri_Classifier"),
      #),
      #  conditionalPanel(condition = "input.WheelID=='Zoom'",
      wellPanel(
        selectizeInput('StudiesIDReactome', 'From Which Studies', choices=NULL, multiple = TRUE),
        with(tags, table(
          tr(
            td(checkboxInput("ViewProfDataReactomeID", "Availability", value = FALSE)),
            td(
              checkboxInput("getlistProfDataID", "Load", value = FALSE)
              #           radioButtons(inputId = "getlistProfDataID", label = "Profile Data",
              #                        c("Availability"="Availability" ,"Load"="Load"),
              #                        selected = "", inline = TRUE),
              #
            )
          )
        )),
        conditionalPanel(condition= "input.getlistProfDataID==true",
                         uiOutput("ui_NodeAttri_ProfData")
        ),


        #),
        #                 )
        conditionalPanel(condition ="input.NodeAttri_ProfDataID =='Mutation'",
                         uiOutput("ui_Freq_MutSlider")
        )
      )
      #conditionalPanel(condition ="input.NodeAttri_ClassID =='All'",
      #                uiOutput("ui_Freq_MutSlider2")
      #),


    ),

    #     radioButtons(inputId = "ClassID2", label = "Processing",
    #                  c("Samples"="Samples" ,"Classifier" = "Classifier", "Plot"="Plot"),
    #                  selected = "Samples", inline = TRUE),

    checkboxInput("ReacRunId", "Run", value = FALSE)
  )
})


## View Available Profile Data

output$ReactomeAvailability <- DT::renderDataTable({
  withProgress(message = 'Loading Data...', value = 0.1, {
    Sys.sleep(0.25)
    dat <- checkDimensions(panel = "Reactome")
    ## remove rownames to column
    dat <- dat %>% add_rownames("Samples")
    # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
    action = DT::dataTableAjax(session, dat, rownames = FALSE)

    DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                  rownames = FALSE, style = "bootstrap", escape = FALSE,
                  # class = "compact",
                  options = list(
                    ajax = list(url = action),
                    search = list(regex = TRUE),
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    autoWidth = TRUE,
                    processing = FALSE,
                    pageLength = 14,
                    lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                  )
    )%>%  formatStyle(names(dat),
                      color = styleEqual("No", 'red'))#, backgroundColor = 'white', fontWeight = 'bold'




  })
})

## print Structure of Profiles data

output$StrListProfData <- renderPrint({
#   if(is.null(input$getlistProfDataID)){
#     return()
#   }else{
    withProgress(message = 'loading Profiles Data... ', value = 0.1, {
      Sys.sleep(0.25)
    getListProfData(panel='Reactome')
    })
  cat("STUDIES:\n", names(ListMutData_bkp), "\n")
  cat("PROFILES DATA:\n",names(ListProfData_bkp) ,"and Mutation", sep = " " )
    #str(r_data$ListProfData)
    #str(r_data$ListMutData)
  #}
})


## load genelist from clipBoard
observe({
  # 'reading' data from clipboard
  if (not_pressed(input$ReacGeneListId)) return()
  isolate({
    r_data[['genelist']] <- c(r_data[['genelist']], 'Reactome_GeneList') %>% unique

  })
})



