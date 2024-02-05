# for cBioPortal

cgds <- cBioPortalData::cBioPortal(
        hostname = "www.cbioportal.org",
        protocol = "https",
        api = "/api/v2/api-docs"
)
#Studies<- getCancerStudies.CGDS(cgds)
Studies <- cBioPortalData::getStudies(cgds)[c("name", "description", "studyId")]


#updateSelectizeInput(session, 'StudiesID', choices = Studies[,1], selected = "gbm_tcga_pub")

## get Cases in side bar panel
output$ui_Cases <- renderUI({
  shiny::withProgress(message = 'loading Cases from cBioPortal server...', value = 1, {
    #Sys.sleep(0.25)
  #CaseLists <- getCaseLists.CGDS(cgds,input$StudiesID)[,1]
  CaseLists <- cBioPortalData::sampleLists(cgds, studyId = input$StudiesID) |>
               pull(sampleListId)

  selectInput("CasesID", "Cases for selected study",
              choices= CaseLists,
              selected = CaseLists[2]
  )
  })
})

## get Genetic Profiles in side bar panel
output$ui_GenProfs <- renderUI({
  shiny::withProgress(message = 'loading Genetic Profiles from cBioPortal server...', value = 1, {
    #Sys.sleep(0.25)

  #GeneticProfiles <- getGeneticProfiles.CGDS(cgds,input$StudiesID)[,1]
  GeneticProfiles <- cBioPortalData::molecularProfiles(api = cgds, studyId = input$StudiesID) |>
                     pull(molecularProfileId)
  selectInput("GenProfID", "Genetic Profiles",
              choices = GeneticProfiles,
              selected = GeneticProfiles[3]
  )

  })
})

## get Gene List in side bar panel

output$ui_GeneList <- renderUI({
  shiny::withProgress(message = 'loading default Gene List ...', value = 1, {
    #Sys.sleep(0.25)
  selectInput("GeneListID", "Gene List:", r_data$genelist)
  })
})


output$ui_loadGeneList <- renderUI({
  wellPanel(
    conditionalPanel("input.GeneListID == 'DNA_damage_Response' && input.loadClip_GeneList%2 == 1",
                     p("Gene List is empty!",align="center",style = "color:red")
    ),

    radioButtons(inputId = "loadGeneListID", label = "Load Gene List:",
                 c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
                 selected = state_multiple("loadGeneListID", "DNA_damage_Response"), inline = TRUE),

    conditionalPanel(condition = "input.loadGeneListID == 'clipboard_GeneList'",
                     actionButton('loadClip_GeneList', 'Paste Gene List')
                     #uiOutput("ui_clipboard_load_ProfData")
    ),
    conditionalPanel(condition = "input.loadGeneListID == 'ExampleGeneList'",
                     actionButton('loadExampleGeneList', 'Load examples')
    )

  )


})

# loading all examples files (linked to helpfiles)
observe({
  if (not_pressed(input$loadExampleGeneList)) return()
  isolate({

    # loading gene list
    data_path <- file.path( system.file(package = "bioCancer"),"extdata/GeneList")
    examples <- list.files(data_path)

    for (ex in examples) loadUserData(ex, file.path(data_path,ex), 'txt')

    # sorting files alphabetically
    r_data[['genelist']] <- sort(r_data[['genelist']])

    updateSelectInput(session, "GeneListID", label = "Gene List:",
                      choices = r_data$genelist,
                      selected = r_data$genelist[1])
  })
  #  A <<- r_data$genelist
})

## load genelist from clipBoard
observe({
  # 'reading' data from clipboard
  if (not_pressed(input$loadClip_GeneList)) return()
  isolate({
    loadClipboard_GeneList(tab=input$loadClip_GeneList)
    updateRadioButtons(session = session, inputId = "GeneListID",
                       label = "Paste Genes:",
                       c( "examples" = "ExampleGeneList",  "clipboard" = "clipboard_GeneList"),
                       #selected = "Genes",
                       inline = TRUE)
    updateSelectInput(session, "GeneListID", label = "Gene List:",
                      choices = r_data$genelist)
  })
})
