

# output$ui_clipboard_load_MutData <- renderUI({
#   if (r_local) {
#     actionButton('loadClipMutData', 'Paste data')
#   } else {
#     tagList(tags$textarea(class="form-control",
#                           id="load_cdata", rows="5"
#     ),
#     actionButton('loadClipMutData', 'Paste data'))
#   }
# })


output$ui_Mut_vars <- renderUI({

  if(input$GeneListID != "Genes"){
    GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  } else{
    GeneList <- r_data$Genes
  }


   dat <- getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)
  ## change rownames in the first column
  dat <- as.data.frame(dat %>% add_rownames("Patients"))

  Mut_vars <- names(dat)
  selectInput("ui_Mut_vars", "Select variables to show:", choices  = Mut_vars,
              selected = state_multiple("Mut_vars",Mut_vars, Mut_vars), multiple = TRUE,
              selectize = FALSE, size = min(6, length(Mut_vars)))
})

output$ui_MutData <- renderUI({

  list(
    wellPanel(

      radioButtons(inputId = "loadGeneListID", label = "Load Gene List:",
                   c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
                   selected = "Genes", inline = TRUE),

      conditionalPanel(condition = "input.loadGeneListID == 'clipboard_GeneList'",
                       actionButton('loadClip_GeneList', 'Paste Gene List')
                       #uiOutput("ui_clipboard_load_MutData")
      ),
      conditionalPanel(condition = "input.loadGeneListID == 'ExampleGeneList'",
                       actionButton('loadExampleGeneList', 'Load examples')
      )
      #       conditionalPanel(condition = "input.loadGeneListID == 'state_Prof'",
      #                        fileInput('uploadstate_Prof', 'Load previous app state_Prof:',  accept = ".rda"),
      #                        uiOutput("refreshOnUpload")
      #       )

    ),
    wellPanel(
      if (length(grep("mutation", input$GenProfID))!=0){
        uiOutput("ui_Mut_vars")
      }
    ),

    wellPanel(

      radioButtons(inputId = "MutData", label = "Load Mutations to Datasets:",
                   c("MutData"="MutData"), selected = FALSE, inline =TRUE),
      conditionalPanel(condition = "input.MutData == 'MutData'",
                       actionButton('loadMutData', 'Load MutData')

      )
    ),
    #   wellPanel(
    #     radioButtons(inputId = "saveAs", label = "Save data:",
    #                  c("rda" = "rda", "csv" = "csv", "clipboard" = "clipboard",
    #                    "state_Prof" = "state_Prof"), selected = "rda", inline = TRUE),
    #
    #     conditionalPanel(condition = "input.saveAs == 'clipboard'",
    #                      uiOutput("ui_clipboard_save")
    #     ),
    #     conditionalPanel(condition = "input.saveAs != 'clipboard' &&
    #                      input.saveAs != 'state_Prof'",
    #                      downloadButton('downloadData', 'Save')
    #     ),
    #     conditionalPanel(condition = "input.saveAs == 'state_Prof'",
    #                      HTML("<label>Save current app state_Prof:</label><br/>"),
    #                      downloadButton('downloadstate_Prof', 'Save')
    #     )
    #   ),
    #   wellPanel(
    #     checkboxInput('man_show_remove', 'Remove data from memory', FALSE),
    #     conditionalPanel(condition = "input.man_show_remove == true",
    #                      uiOutput("uiRemoveDataset"),
    #                      actionButton('removeDataButton', 'Remove data')
    #     )
    #   ),
    #   help_modal('Manage','manageHelp',inclMD(file.path(r_path,"base/tools/help/manage.md")))
    fileInput('file1', 'Choose txt File',
              accept=c('text',
                       'text,text/plain'))
  )
})
####################

# loading all examples files (linked to helpfiles)
observe({
  if (not_pressed(input$loadExampleGeneList)) return()
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

})

## load genelist from clipBoard
observe({
  # 'reading' data from clipboard
  if (not_pressed(input$loadClip_GeneList)) return()
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


## Load Mutation data in datasets
observe({
  if (not_pressed(input$loadMutData)) return()
  isolate({

    loadInDatasets(fname="MutData", header=TRUE)

    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist,
                      selected = "MutData")

  })
})
