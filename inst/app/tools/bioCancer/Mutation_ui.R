output$ui_Mut_vars <- renderUI({

  shiny::withProgress(message = 'loading Variables of Mutation Data from cgdsr server...', value = 1, {

  GeneList <- whichGeneList(input$GeneListID)

  dat <- cBioPortalData::getDataByGenes(api = cgds,
                        studyId = input$StudiesID,
                        genes = GeneList,
                        by = "hugoGeneSymbol",
                        molecularProfileIds = input$GenProfID
  ) %>% .[[1]] |>
    select(-c(uniqueSampleKey, uniquePatientKey, molecularProfileId, sampleId, studyId))

  ## avoid error when geneList is empty
  if(dim(dat)[1]==0){

    #dat <- as.data.frame("Gene List is empty. copy and paste genes from text file (Gene/line)")

    selectInput("Mut_varsID", "Select variables to show:",
                choices  = "Patients",
                #selected = state_multiple("Mut_vars",Mut_vars, Mut_vars),
                multiple = FALSE,
                selectize = FALSE,
                size = min(2)
    )
  }else{

    Mut_vars <- names(dat)

    selectInput("Mut_varsID", "Select variables to show:",
                choices  = Mut_vars,
                selected = state_multiple("Mut_vars",Mut_vars, Mut_vars),
                multiple = TRUE,
                selectize = FALSE,
                size = min(6, length(Mut_vars)))
  }
  })
})

output$ui_MutData <- renderUI({

  list(
    # wellPanel(
    #
    #   conditionalPanel("input.GeneListID == 'DNA_damage_Response' && input.loadClipMut_GeneList%2 == 1 ",
    #                    p("Gene List is empty!", align="center",style = "color:red")
    #   ),
    #
    #   radioButtons(inputId = "loadGeneListID_Mut", label = "Load Gene List:",
    #                c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
    #                selected = "Genes", inline = TRUE),
    #
    #   conditionalPanel(condition = "input.loadGeneListID_Mut == 'clipboard_GeneList'",
    #                    actionButton('loadClipMut_GeneList', 'Paste Gene List')
    #                    #uiOutput("ui_clipboard_load_MutData")
    #   ),
    #   conditionalPanel(condition = "input.loadGeneListID_Mut == 'ExampleGeneList'",
    #                    actionButton('loadExampleGeneList_MutData', 'Load examples')
    #   )
    #
    # ),
    wellPanel(
      if (length(grep("mutation", input$GenProfID))!=0){
        uiOutput("ui_Mut_vars")
      }
    ),

    wellPanel(
      checkboxInput(inputId = "MutData", "Export for Processing:" ,value = FALSE),
      #radioButtons(inputId = "MutData", label = "Load Mutations to Datasets:",
      #            c("MutData"="MutData"), selected = FALSE, inline =TRUE),
      conditionalPanel(condition = "input.MutData == true",
                       actionButton('loadMutData', 'Export to Datasets', icon('arrow-up'))

      )
    ),
    #help_modal_km('Mutation Data','MutationHelp',inclMD(file.path(r_path,"base/tools/help/Mutation.md")))
    help_and_report(modal_title = "Mutation Data", fun_name = "MutationHelp",
                    author = "Karim Mezhoud",
                    help_file = inclRmd(file.path(
                      getOption("radiant.path.bioCancer"),"app/tools/help/Mutation.md")))
  )
})
####################

# # loading all examples files (linked to helpfiles)
# observe({
#   if (not_pressed(input$loadExampleGeneList_MutData)) return()
#   isolate({
#
#     # loading data bundled with bioCancer
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
#
# })
#
# ## load genelist from clipBoard
# observe({
#   # 'reading' data from clipboard
#   if (not_pressed(input$loadClipMut_GeneList)) return()
#   isolate({
#     loadClipboard_GeneList(tab = input$loadClipMut_GeneList)
#     updateRadioButtons(session = session, inputId = "GeneListID",
#                        label = "Paste Genes:",
#                        c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
#                        #selected = "Genes",
#                        inline = TRUE)
#     updateSelectInput(session, "GeneListID", label = "Pasted Genes:",
#                       choices = r_data$genelist)
#   })
# })


## Load Mutation data in datasets
observe({
  if (not_pressed(input$loadMutData)) return()
  isolate({

    loadInDatasets(fname="MutData", header=TRUE)

    # sorting files alphabetically
    r_info[['datasetlist']] <- sort(r_info[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_info$datasetlist,
                      selected = "MutData")

  })
})
