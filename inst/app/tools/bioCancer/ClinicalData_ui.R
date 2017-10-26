output$ui_clipboard_load_Clinical <- renderUI({
  if (getOption("radiant.local")) { ## running local or on a server
    actionButton('loadClipDataClinical', 'Paste data')
  } else {
    tagList(tags$textarea(class="form-control",
                          id="load_cdata", rows="5"
    ),
    actionButton('loadClipDataClinical', 'Paste data'))
  }
})

output$ui_Clinical_vars <- renderUI({
  shiny::withProgress(message = 'loading Clinical Data from cgdsr server...', value = 0.1, {
    Sys.sleep(0.25)
  ##### get Clinical Data for selected Case
  dat <- cgdsr::getClinicalData(cgds, input$CasesID)
  ## change rownames in the first column
  #dat <- dat %>% dplyr::add_rownames("Patients")
  dat <- dat %>% tibble::rownames_to_column("Patients")
  r_data[['ClinicalData']] <- dat

  Clinical_vars <- names(dat)
  r_data[['Clinical_vars']] <- Clinical_vars

  selectInput("Clinical_varsID", "Select variables to show:", choices  = Clinical_vars,
               multiple = TRUE, selected = state_multiple("Clinical_vars",Clinical_vars, Clinical_vars),
              selectize = FALSE, size = min(8, length(Clinical_vars)))
  })
})


output$ui_ClinicalData <- renderUI({

  list(
    wellPanel(
      uiOutput("ui_Clinical_vars")
    ),

    wellPanel(
      radioButtons(inputId = "ClinicalDataID", label = "Export for Processing:",
                   c("Load ClinicalData"="ClinicalData","clipboard" = "clipboard_Clin"),
                   selected = "ClinicalData", inline = TRUE),


      conditionalPanel(condition = "input.ClinicalDataID == 'ClinicalData'",
                       actionButton('loadClinicalData', 'Export to Datasets',
                                    icon('arrow-up'), style="float:center")

      ),
      conditionalPanel(condition = "input.ClinicalDataID == 'clipboard_Clin'",
                       uiOutput("ui_clipboard_load_Clinical")
      )
    ),

    #help_modal_km('Clinical Data','ClinicalHelp',inclMD(file.path(r_path,"base/tools/help/Clinical.md")))
    help_and_report(modal_title = "Clinical Data", fun_name = "ClinicalHelp",
                    author = "Karim Mezhoud",
                    help_file = inclRmd(file.path(
                      getOption("radiant.path.bioCancer"),"app/tools/help/Clinical.md")))
  )
})


observe({
  # 'reading' data from clipboard
  if (not_pressed(input$loadClipDataClinical)) return()
  isolate({
    loadClipboardData()
    updateRadioButtons(session = session, inputId = "ClinicalDataID",
                       label = "Export Clincial Data to Datasets:",
                       c("Load ClinicalData" = "ClinicalData","clipboard" = "clipboard_Clin"),
                       selected = "ClinicalData", inline = TRUE)

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist, selected = "xls_data")
  })
})

## Load Clinical data in datasets
observe({
  if (not_pressed(input$loadClinicalData)) return()
  isolate({

    loadInDatasets(fname="ClinicalData", header=TRUE)

    # sorting files alphabetically
    r_data[['datasetlist']] <- sort(r_data[['datasetlist']])

    updateSelectInput(session, "dataset", label = "Datasets:",
                      choices = r_data$datasetlist,
                      selected = "ClinicalData")

  })
})
