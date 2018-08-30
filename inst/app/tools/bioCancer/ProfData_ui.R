
# output$ui_fileUploadProfData <- renderUI({
#
#   if (is.null(input$loadGeneListID)) return()
#   if (input$loadGeneListID == "csv") {
#     fileInput('uploadfile', '', multiple=TRUE,
#               accept = c('text/csv','text/comma-separated-values',
#                          'text/tab-separated-values',
#                          'text/plain','.csv','.tsv'))
#   } else if (input$loadGeneListID == "rda") {
#     fileInput('uploadfile', '', multiple=TRUE,
#               accept = c(".rda",".rds",".rdata"))
#   }
# })

output$ui_clipboard_load_ProfData <- renderUI({
  if (getOption("radiant.local")) {
    actionButton('loadClipProfData', 'Paste data')
  } else {
    tagList(tags$textarea(class="form-control",
                          id="load_cdata", rows="5"
    ),
    actionButton('loadClipProfData', 'Paste data'))
  }
})


output$ui_ProfData <- renderUI({
  list(

    # wellPanel(
    #   conditionalPanel("input.GeneListID == 'DNA_damage_Response' && input.loadClipProf_GeneList%2 == 1",
    #                    p("Gene List is empty!",align="center",style = "color:red")
    #   ),
    #
    #   radioButtons(inputId = "loadGeneListID_ProfData", label = "Load Gene List:",
    #                c( "examples" = "ExampleGeneList_ProfData",  "clipboard" = "clipboard_GeneList_ProfData"),
    #                selected = state_multiple("loadGeneListID_ProfData", "DNA_damage_Response"), inline = TRUE),
    #
    #   conditionalPanel(condition = "input.loadGeneListID_ProfData == 'clipboard_GeneList_ProfData'",
    #                    actionButton('loadClipProf_GeneList', 'Paste Gene List')
    #                    #uiOutput("ui_clipboard_load_ProfData")
    #   ),
    #   conditionalPanel(condition = "input.loadGeneListID_ProfData == 'ExampleGeneList_ProfData'",
    #                    actionButton('loadExampleGeneList_ProfData', 'Load examples')
    #   )
    #
    # ),

    wellPanel(
      checkboxInput(inputId = "ProfData", "Export for Processing" ,value = FALSE),
      # radioButtons(inputId = "ProfData", label = "Load ProfData to Datasets:",
      #             c("ProfData"="ProfData"), selected = "ProfData", inline =TRUE),
      conditionalPanel(condition = "input.ProfData == true",
                       actionButton('loadProfData', 'Export to Datasets', icon('arrow-up')))
    ),
    #help_modal_km('Profiles Data','ProfilesHelp',inclMD(file.path(r_path,"base/tools/help/Profiles.md")))
    help_and_report(modal_title = "Profiles Data", fun_name = "ProfilesHelp",
                    author = "Karim Mezhoud",
                    help_file = inclRmd(file.path(
                      getOption("radiant.path.bioCancer"),"app/tools/help/Profiles.md")))
  )
})
####################

# # loading all examples files (linked to helpfiles)
# observe({
#   if (not_pressed(input$loadExampleGeneList_ProfData)) return()
#   isolate({
#
#     # loading gene list
#     data_path <- file.path( system.file(package = "bioCancer"),"extdata/GeneList")
#     examples <- list.files(data_path)
#
#     for (ex in examples) loadUserData(ex, file.path(data_path,ex), 'txt')
#
#     # sorting files alphabetically
#     r_data[['genelist']] <- sort(r_data[['genelist']])
#
#     updateSelectInput(session, "GeneListID", label = "Gene List Examples:",
#                       choices = r_data$genelist,
#                       selected = r_data$genelist[1])
#   })
#   #  A <<- r_data$genelist
# })
#
# ## load genelist from clipBoard
# observe({
#   # 'reading' data from clipboard
#   if (not_pressed(input$loadClipProf_GeneList)) return()
#   isolate({
#     loadClipboard_GeneList(tab=input$loadClipProf_GeneList)
#     updateRadioButtons(session = session, inputId = "GeneListID",
#                        label = "Paste Genes:",
#                        c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
#                        #selected = "Genes",
#                        inline = TRUE)
#     updateSelectInput(session, "GeneListID", label = "Pasted Genes:",
#                       choices = r_data$genelist)
#   })
# })

## Load Profile data in datasets
observe({
  if (not_pressed(input$loadProfData)) return()
  isolate({

    loadInDatasets(fname="ProfData", header=TRUE)

    # sorting files alphabetically
    r_info[['datasetlist']] <- sort(r_info[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_info$datasetlist,
                      selected = "ProfData")

  })
})
