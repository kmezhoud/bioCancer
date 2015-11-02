# #### UI ####
#
# output$ui_NetworkSlider <- renderUI({
#
#   sliderInput("opacity", "Opacity (not for Sankey)", 1, min = 0.1,
#               max = 1, step = .1)
#
# })
#
#
# ### example 1 Provider
# # output$ui_Provider <- renderUI({
# # psicquic <- PSICQUIC()
# # providers <- providers(psicquic)
# #
# #
# #    selectInput("ProviderID", "Select Data Base:", choices  = providers,
# #                selected = "Reactome-FIs", multiple = TRUE,
# #                selectize = FALSE, size = min(10, length(providers)))
# #
# # })
#
# ### Example 2 Provider
# output$ui_Provider <- renderUI({
# psicquic <- PSICQUIC::PSICQUIC()
# providers <- PSICQUIC::providers(psicquic)
# updateSelectizeInput(session, 'ProviderID', choices = providers, selected = NULL)
#
#                     conditionalPanel("input.tabs_data == 'Analysis'",
#                     conditionalPanel("input.tabs_Analysis == 'Network'",
#                    selectizeInput('ProviderID', 'Select Data Bases', choices=NULL, multiple = TRUE)
#                   ))
# })
#
# output$ui_saveSimpleNetwork <- renderUI({
#
#   actionButton("saveHTML")
# })
#
#
# observe({
#   if (is.null(input$saveHTML) ) return()
#   isolate({
#     ## simple plot
#     networkData <- data.frame(tbl[,"A.name"], tbl[,"B.name"] )
#     networkD3::simpleNetwork(networkData)
#     # save network
#     library(magrittr)
#     networkD3::simpleNetwork(networkData) %>%   saveNetwork(file = 'Net1.html')
#     #
#     #       updateRadioButtons(session = session, inputId = "saveAs",
#     #                          label = "Save data:",
#     #                          c("rda" = "rda", "csv" = "csv", "clipboard" = "clipboard",
#     #                            "state" = "state"), selected = "rda", inline = TRUE)
#   })
# })
