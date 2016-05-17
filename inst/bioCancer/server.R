## SERVER FOR bioCancer PACKAGE
shinyServer(function(input, output, session) {

  # for cgdsr

  cgds <- cgdsr::CGDS("http://www.cbioportal.org/public-portal/")
  Studies<- cgdsr::getCancerStudies(cgds)
  updateSelectizeInput(session, 'StudiesID', choices = Studies[,1], selected = "gbm_tcga_pub")

  ## get Cases in side bar panel
  output$ui_Cases <- renderUI({
    selectInput("CasesID", "Cases for selected study",cgdsr::getCaseLists(cgds,input$StudiesID)[,1] )
  })

  ## get Genetic Profiles in side bar panel
  output$ui_GenProfs <- renderUI({
    selectInput("GenProfID", "Genetic Profiles",cgdsr::getGeneticProfiles(cgds,input$StudiesID)[,1] )
  })


  ## source shared functions
  for (file in list.files(paste0(system.file(package ="bioCancer"),"/Rbis"),
                          pattern="\\.(r|R)$",
                          full.names = TRUE)) {

    source(file, encoding = r_encoding, local = TRUE)
  }
  source(file.path(r_path,"base/init.R"), encoding = r_encoding, local = TRUE)
  source(file.path(r_path,"base/bioCancerInit.R"), encoding = r_encoding, local = TRUE)



  ## get Gene List in side bar panel

  output$ui_GeneList <- renderUI({
    selectInput("GeneListID", "Gene List:", r_data$genelist)
  })

  # if (!"package:radiant" %in% search()) {
  if (r_path == "..") {
    for (file in list.files("../Rbis",
                            pattern="\\.(r|R)$",
                            full.names = TRUE)) {

      source(file, encoding = r_encoding, local = TRUE)
    }
  } else {
    copy_all(bioCancer)
    set_class <- set_class
  }
  #   } else {
  #     copy_from(radiant, state_init, state_single, state_multiple)
  #   }

  ## source for Documents
  for (file in list.files("../../R",
                          pattern="\\.(r|R)$",
                          full.names = TRUE)) {

    source(file, encoding = r_encoding, local = TRUE)
  }

  # source data & app tools from base
  for (file in list.files(c(file.path(r_path,"base/tools/app"),
                            file.path(r_path,"base/tools/data")
                            ),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = r_encoding, local = TRUE)

  # source analysis tools from quant app
  for (file in list.files(file.path(r_path,"quant/tools/analysis"),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = r_encoding, local = TRUE)

  # source additional analysis tools for app
  for (file in list.files(c("tools/analysis"),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = r_encoding, local = TRUE)



  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
