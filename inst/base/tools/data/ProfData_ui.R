
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
  if (r_local) {
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

    wellPanel(

      radioButtons(inputId = "loadGeneListID_ProfData", label = "Load Gene List:",
                   c( "examples" = "ExampleGeneList_ProfData",  "clipboard" = "clipboard_GeneList_ProfData"),
                   selected = "Genes", inline = TRUE),

      conditionalPanel(condition = "input.loadGeneListID_ProfData == 'clipboard_GeneList_ProfData'",
                       actionButton('loadClipProf_GeneList', 'Paste Gene List')
                       #uiOutput("ui_clipboard_load_ProfData")
      ),
      conditionalPanel(condition = "input.loadGeneListID_ProfData == 'ExampleGeneList_ProfData'",
                       actionButton('loadExampleGeneList_ProfData', 'Load examples')
      )
#       conditionalPanel(condition = "input.loadGeneListID == 'state_Prof'",
#                        fileInput('uploadstate_Prof', 'Load previous app state_Prof:',  accept = ".rda"),
#                        uiOutput("refreshOnUpload")
#       )

  ),

  wellPanel(
    checkboxInput(inputId = "ProfData", "Load Profile to Datasets" ,value = FALSE),
   # radioButtons(inputId = "ProfData", label = "Load ProfData to Datasets:",
    #             c("ProfData"="ProfData"), selected = "ProfData", inline =TRUE),
    conditionalPanel(condition = "input.ProfData == true",
                     actionButton('loadProfData', 'Load Profiles Table'))
  ),
    help_modal_km('Profiles Data','ProfilesHelp',inclMD(file.path(r_path,"base/tools/help/Profiles.md")))
  )
})
####################

# loading all examples files (linked to helpfiles)
observe({
  if (not_pressed(input$loadExampleGeneList_ProfData)) return()
  isolate({

    # loading data bundled with Radiant
    data_path <- file.path(r_path,"base/data/GeneList")
    examples <- list.files(data_path)

    for (ex in examples) loadUserData(ex, file.path(data_path,ex), 'txt')

    # sorting files alphabetically
    r_data[['genelist']] <- sort(r_data[['genelist']])

    updateSelectInput(session, "GeneListID", label = "Gene List Examples:",
                      choices = r_data$genelist,
                      selected = r_data$genelist[1])
  })
#  A <<- r_data$genelist
})

## load genelist from clipBoard
observe({
  # 'reading' data from clipboard
  if (not_pressed(input$loadClipProf_GeneList)) return()
  isolate({
    loadClipboard_GeneList()
    updateRadioButtons(session = session, inputId = "GeneListID",
                       label = "Paste Genes:",
                       c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
                       selected = "Genes", inline = TRUE)
    updateSelectInput(session, "GeneListID", label = "Pasted Genes:",
                      choices = r_data$genelist, selected = "Genes")
  })
})

## Load Profile data in datasets
observe({
  if (not_pressed(input$loadProfData)) return()
  isolate({

    loadInDatasets(fname="ProfData", header=TRUE)

    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist,
                      selected = "ProfData")

  })
})
