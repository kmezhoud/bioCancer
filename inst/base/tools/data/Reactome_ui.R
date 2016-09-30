
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
  ReactomeEnrich <- c("None","Freq. Interaction", "GeneSet", "FreqInt./GeneSet")
  selectizeInput("NodeAttri_ReactomeID", label= "From ReactomeFI:", choices= ReactomeEnrich,
                 selected= "None", multiple=FALSE)
})

output$ui_AnnoGeneSet_ReactomeFI <- renderUI({
  type <- c("None","Pathway", "BP","CC","MF")
  selectizeInput("TypeGeneSetID", label="Type of enrichment:", choices=type,
                 selected ="None", multiple=FALSE
  )
})

output$ui_GeneSetFDR <- renderUI({
  #if(is.null(r_data$MinGeneSetFDR)){
  sliderInput("GeneSetFDRID", "FDR of enrichment", 0.025, min =0.0005,
              max=0.05, step=0.0005 )
  #}else{
  #sliderInput("GeneSetFDRID", "FDR", 0.005, min =r_data$MinGeneSetFDR,
  #            max=0.05, step=0.0005 )
  #}
})

output$ui_NodeAttri_Classifier <- renderUI({
  ClassEnrich <- c("None","mRNA","Studies", "mRNA/Studies")
  selectizeInput("NodeAttri_ClassifierID", label= "From Classifier:", choices= ClassEnrich,
                 selected= "None", multiple=FALSE)
})

output$ui_NodeAttri_ProfData <- renderUI({

  Dimension <- c("None","CNA","Met_HM27", "Met_HM450","Mutation" )
  selectizeInput("NodeAttri_ProfDataID", label= "Select Profiles Data:", choices= Dimension,
                 selected= "None", multiple=TRUE)
})

output$ui_Freq_MutSlider <- renderUI({
  #div(style="height: 27px;",
  sliderInput("FreqMutSliderID", "Mutation Percentage", 25, min = 1,
              max = 100, step = 1)
  #)

})

output$ui_MetSliderHM450 <- renderUI({
  sliderInput("ThresholdMetHM450ID", "Silencing gene rate HM450", 0.8, min =0,
              max=1, step=0.05 )
})

output$ui_MetSliderHM27 <- renderUI({
  sliderInput("ThresholdMetHM27ID", "Silencing gene rate HM27", 0.8, min =0,
              max=1, step=0.05 )
})



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

      conditionalPanel("input.NodeAttri_ReactomeID =='GeneSet' ||
                       input.NodeAttri_ReactomeID =='FreqInt./GeneSet'",
                       uiOutput("ui_AnnoGeneSet_ReactomeFI"),
                       uiOutput("ui_GeneSetFDR")
      ),


      #conditionalPanel(condition = "input.ClassID=='Classifier'",
      uiOutput("ui_NodeAttri_Classifier"),
      #),
      #  conditionalPanel(condition = "input.WheelID=='Zoom'",
      wellPanel(
        selectizeInput('StudiesIDReactome', 'From Which Studies', choices=NULL, multiple = TRUE),

        div(class="row",
            div(class="col-xs-6",
                checkboxInput("ViewProfDataReactomeID", "Availability", value = FALSE)),
            div(class="col-xs-6",
                checkboxInput("getlistProfDataID", "Load", value = FALSE))
        ),


        #           radioButtons(inputId = "getlistProfDataID", label = "Profile Data",
        #                        c("Availability"="Availability" ,"Load"="Load"),
        #                        selected = "", inline = TRUE),



        conditionalPanel(condition= "input.getlistProfDataID==true",
                         uiOutput("ui_NodeAttri_ProfData")
        ),


        #),
        #                 )
        conditionalPanel(condition ="input.getlistProfDataID==true",
                         uiOutput("ui_Freq_MutSlider"),
                         uiOutput("ui_MetSliderHM450"),
                         uiOutput("ui_MetSliderHM27")
                         #                       div(class="col-xs-6",
                         #                            numericInput("ThresholdMetHM450ID",
                         #                                         "HM450",
                         #                                         "0.8",min = 0.8, max = 1 , step = 0.05)),
                         #                        div(class="col-xs-6",
                         #                            numericInput("ThresholdMetHM27ID",
                         #                                         "HM27",
                         #                                         "0.8",min = 0.8, max = 1 , step = 0.05))
        )
      ),
      #         conditionalPanel(condition= "input.NodeAttri_ProfDataID=='Met_HM450'",
      #                           uiOutput("ui_MetSliderHM450")
      #         ),
      #         conditionalPanel(condition= "input.NodeAttri_ProfDataID %in%'Met_HM27'",
      #                          uiOutput("ui_MetSliderHM27")
      #       )
      #conditionalPanel(condition ="input.NodeAttri_ClassID =='All'",
      #                uiOutput("ui_Freq_MutSlider2")
      #),
      div(class="row",
          div(class="col-xs-4",
              checkboxInput("ReacRunId", "Plot" ,value = FALSE)),
          div(class="col-xs-4",
              checkboxInput("ReacLegendId", "Legend", value=FALSE)
          )
      )

    ),

    tagList(
      h4("Dynamic Network:"),
      wellPanel(
        uiOutput("ui_visPhysic"),
        div(class="row",
            div(class="col-xs-4",
                checkboxInput("NetworkRunId", "Plot" ,value = FALSE)),
            div(class="col-xs-4"
                #checkboxInput("ReacLegendId", "Legend", value=FALSE)
            )
        )
      )
    ),
    help_modal_km('Reactome','ReactomeHelp',inclMD(file.path(r_path,"base/tools/help/Reactome.md")))

    # with(tags, table(
    #   tr(
    #     td(checkboxInput("ReacRunId", "Run" ,value = FALSE)),
    #     td(h4("", align="center")),
    #     td(checkboxInput("ReacLegendId", "Legend", value=FALSE))
    #   ))),
    #checkboxGroupInput(inputId="ReacRunId", label="", choices=c("Run","Legend"), inline = TRUE)
  )
})


## View Available Profile Data

output$ReactomeAvailability <- DT::renderDataTable({
  withProgress(message = 'Loading Data...', value = 0.1, {
    Sys.sleep(0.25)
    dat <- checkDimensions(panel = "Reactome", StudyID= input$StudiesIDReactome)
    ## remove rownames to column
    dat <- dat %>% tibble::rownames_to_column("Samples")
    # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
    displayTable(dat) %>%  DT::formatStyle(names(dat),
                          color = DT::styleEqual("No", 'red'))#, backgroundColor = 'white', fontWeight = 'bold'

  })
})

## print Structure of Profiles data

output$StrListProfData <- renderPrint({
  #   if(is.null(input$getlistProfDataID)){
  #     return()
  #   }else{
  withProgress(message = 'loading Profiles Data... ', value = 0.1, {
    Sys.sleep(0.25)
    getListProfData(panel='Reactome', input$GeneListID)
  })
  cat("STUDIES:\n", names(r_data$ListMutData), "\n")
  cat("PROFILES DATA:\n",names(r_data$ListProfData) ,"and Mutation", sep = " " )
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



