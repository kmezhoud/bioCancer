

output$ui_Circomics <- renderUI({

  ## get Studies for Circomics
  updateSelectizeInput(session, 'StudiesIDCircos', choices = Studies[,1], selected = c("luad_tcga_pub","blca_tcga_pub"))
#,"prad_tcga_pub","ucec_tcga_pub"

  wellPanel(
  conditionalPanel("input.tabs_data == 'Circomics'",

                   selectizeInput('StudiesIDCircos', 'Studies in Wheel', choices=NULL, multiple = TRUE),

                   radioButtons(inputId = "WheelID", label = "Wheel Style:",
                                c("Init"="init" ,"Zoom" = "Zoom",  "Static" = "Static"),
                                selected = "init", inline = TRUE),

                   radioButtons(inputId = "saveWheelID", label = "Save Wheel:",
                                c("SVG" = "SVG", "Gif" = "Gif"),
                                selected = "SVG", inline = TRUE),

                   conditionalPanel(condition = "input.saveWheelID == 'SVG'",
                                    downloadButton('SaveSVG', 'SVG')),

                   conditionalPanel(condition = "input.saveWheelID == 'Gif'",
                                    downloadButton("SaveGif")),
                   wellPanel(
                   h4("Wheel Legend"),
                   p(span("Black", style="color:black"),": Non available data."),
                   p(span("White", style="color:black"),": Non significant rate."),
                   p(span("Cyan", style="color:deepskyblue"),": Middle Negative significant rate."),
                   p(span("Blue", style="color:blue"),": Negative significant rate."),
                   p(span("Yellow", style="color:gold"),": Middle Positive significant rate."),
                   p(span("Red", style="color:red"),": Positive significant rate.")
)

                  )
)
})



observe({

  if (not_pressed(input$SaveSVG)) return()
  isolate({
    library(metabologram)
    CoffeewheelTreeData <- reStrDimension(r_data$ListProfData)
    metabologram(CoffeewheelTreeData, width=500, height=500, main="metabologram", showLegend = TRUE, fontSize = 12, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")

  })
})



output$SaveGif = downloadHandler(
  filename = function(){'random.png'},
  content  = function(file){
    CoffeewheelTreeData <- reStrDimension(r_data$ListProfData)
    ggsave(file,metabologram(CoffeewheelTreeData, width=500, height=500, main="metabologram", showLegend = TRUE, fontSize = 12, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend"))
    #file.rename('random.gif', file)
}
    )



#   observe({
#
#     if (not_pressed(input$SaveTIFF)) return()
#     isolate({
#
#       coffeewheel(CoffeewheelTreeData, width=500, height=500, main="CoffeeWheel", partitionAttribute="value")
#
#     })
#   })

