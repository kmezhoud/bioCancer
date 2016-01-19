output$LegendCircos <- renderPlot({
  my.colors = colorRampPalette(c("blue3","cyan","white","yellow", "red","black"))
  z=matrix(1:100,nrow=1)
  x=1
  y=seq(3,2345,len=100) # supposing 3 and 2345 are the range of your data
  image(x,y,z,col=my.colors(100),axes=FALSE,xlab="",ylab="")
  #axis(2)
  mtext(text=c("Min","Low","middle","High","Max","NA"), line=1.8, las=2, side=4, at=c(50,500,900, 1400,1900,2300),adj = 1)
  mtext(text=c("Down","Low","0","High","Up","NA"), line=0.5, las=2, side=2, outer = FALSE, at=c(50,500,900, 1400,1900,2300),adj = 1)
})



output$ui_Circomics <- renderUI({

  ## get Studies for Circomics
  updateSelectizeInput(session, 'StudiesIDCircos', choices = Studies[,1], selected = c("luad_tcga_pub","blca_tcga_pub"))
  #,"prad_tcga_pub","ucec_tcga_pub"

  output$ui_CircosDimension <- renderUI({

    Dimension <- c("CNA","Methylation", "mRNA","Mutation","miRNA","RPPA", "All" )
    selectizeInput("CircosDimensionID", label= "Select Dimension:", choices= Dimension,
                   selected= "", multiple=TRUE)
  })

#   output$ui_SaveCircos <- renderUI({
#     Dimension <- c("","CNA","Methylation", "mRNA","Mutation","miRNA","RPPA", "All" )
#     selectizeInput("SaveCircosID", label= "Save Dimension:", choices= Dimension,
#                    selected= "", multiple=FALSE)
#
#   })
#
#   if('All' %in% input$SaveCircosID ){
#
#     metabologramOutput('metabologram_All')
#
#   }

  output$StrListProfDataCircos <- renderPrint({
    #   if(is.null(input$getlistProfDataID)){
    #     return()
    #   }else{
    withProgress(message = 'loading Profiles Data... ', value = 0.1, {
      Sys.sleep(0.25)
      getListProfData(panel='Circomics')
    })
    cat("STUDIES:\n", names(r_data$ListMutData), "\n")
    cat("PROFILES DATA:\n",names(r_data$ListProfData) ,"and Mutation", sep = " " )
    #str(r_data$ListProfData)
    #str(r_data$ListMutData)
    #}
  })

  conditionalPanel("input.tabs_Enrich == 'Circomics'",

                   selectizeInput('StudiesIDCircos', 'Studies in Wheel', choices=NULL, multiple = TRUE),
                   div(class="row",
                       div(class="col-xs-6",
                           checkboxInput("ViewProfDataCircosID", "Availability", value = FALSE)),
                       div(class="col-xs-6",
                           checkboxInput("getlistProfDataCircosID", "Load", value = FALSE))
                   ),

                   conditionalPanel("input.getlistProfDataCircosID==true",
                                    actionButton('loadListProfData', 'Load Profiles to Handle Panel', style="float:center"),
                                    uiOutput("ui_CircosDimension")
                                    #uiOutput("ui_SaveCircos")
                   ),

#                    conditionalPanel("input.CircosDimensionID == 'All'",
#                                     #plot_downloader("SaveMetabologram_All", pre = ""),
#                                     downloadButton('Save_Metabologram_All', 'All')
#
#                    ),

                   checkboxInput("CircosLegendID", "Legend", value=FALSE),
                   #                    radioButtons(inputId = "WheelID", label = "Wheel Style:",
                   #                                 c("Summary"="init" ,"Zoom" = "Zoom",  "Static" = "Static"),
                   #                                 selected = "", inline = TRUE),

                   #                    radioButtons(inputId = "saveWheelID", label = "Save Wheel:",
                   #                                 c("SVG" = "SVG", "Gif" = "Gif"),
                   #                                 selected = "SVG", inline = TRUE),
                   #
                   #                    conditionalPanel(condition = "input.saveWheelID == 'SVG'",
                   #                                     downloadButton('SaveSVG', 'SVG')),
                   #
                   #                    conditionalPanel(condition = "input.saveWheelID == 'Gif'",
                   #                                     downloadButton("SaveGif")),

                     conditionalPanel("input.CircosLegendID==true",
                                      wellPanel(
                                      plotOutput("LegendCircos")
                     )
                     #                    h4("Wheel Legend"),
                     #                    p(span("Black", style="color:black"),": Non available data."),
                     #                    p(span("White", style="color:black"),": Non significant rate."),
                     #                    p(span("Cyan", style="color:deepskyblue"),": Middle Negative significant rate."),
                     #                    p(span("Blue", style="color:blue"),": Negative significant rate."),
                     #                    p(span("Yellow", style="color:gold"),": Middle Positive significant rate."),
                     #                    p(span("Red", style="color:red"),": Positive significant rate.")
                   ),

                   help_modal_km('Circomics','CircomicsHelp',inclMD(file.path(r_path,"base/tools/help/Circomics.md")))
  )

})

## Load Profile data in datasets
observe({
  if (not_pressed(input$loadListProfData)) return()
  isolate({

    loadInDatasets(fname="xCNA", header=TRUE)
    loadInDatasets(fname="xmRNA", header=TRUE)
    loadInDatasets(fname="xMetHM450", header=TRUE)
    loadInDatasets(fname="xMetHM27", header=TRUE)
    loadInDatasets(fname="xMut", header=TRUE)
    loadInDatasets(fname="xFreqMut", header=TRUE)
    loadInDatasets(fname="xmiRNA", header=TRUE)
    loadInDatasets(fname="xRPPA", header=TRUE)

    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist,
                      selected = "")

  })
})

# observe({
#
#   if (not_pressed(input$SaveSVG)) return()
#   isolate({
#     library(metabologram)
#     CoffeewheelTreeData <- reStrDimension(r_data$ListProfData)
#     metabologram(CoffeewheelTreeData, width=500, height=500, main="metabologram", showLegend = TRUE, fontSize = 12, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")
#
#   })
# })


#
# output$SaveGif = downloadHandler(
#   filename = function(){'random.png'},
#   content  = function(file){
#     CoffeewheelTreeData <- reStrDimension(r_data$ListProfData)
#     ggsave(file,metabologram(CoffeewheelTreeData, width=500, height=500, main="metabologram", showLegend = TRUE, fontSize = 12, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend"))
#     #file.rename('random.gif', file)
# }
#     )



#   observe({
#
#     if (not_pressed(input$SaveTIFF)) return()
#     isolate({
#
#       coffeewheel(CoffeewheelTreeData, width=500, height=500, main="CoffeeWheel", partitionAttribute="value")
#
#     })
#   })

