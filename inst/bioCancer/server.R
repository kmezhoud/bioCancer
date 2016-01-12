## SERVER FOR bioCancer PACKAGE
shinyServer(function(input, output, session) {
  #library(networkD3)

  library(shinythemes)
  library(cgdsr)
  library(magrittr)
  library(coffeewheel)
  library(htmlwidgets)
  #library(metabologram)
  library(DiagrammeR)
  #library(dplyr)

  library(geNetClassifier)
  library(Biobase)
  library(clusterProfiler)
  library(DOSE)
  library(reactome.db)
  library(ReactomePA)
  library("org.Hs.eg.db")
  library(AnnotationFuncs)
  #### masked
  library(plyr)
  library(DT)
  library(XML)
  library(RCurl)

  ### package in NAMESSPACE not imported
  library(gridExtra)
  library(psych)
  library(broom)
  #library(radiant)
  library(car)
  library(scales)

  #   library(grDevices)
  #library(S4Vectors)
  ####masked package
  #library(lubridate)
  #remove(list = conflicts(detail = TRUE)$.GlobalEnv)
  ##################
  # for cgdsr

  cgds <- cgdsr::CGDS("http://www.cbioportal.org/public-portal/")
  Studies<- cgdsr::getCancerStudies(cgds)
  updateSelectizeInput(session, 'StudiesID', choices = Studies[,1], selected = "gbm_tcga_pub")

  ####### Gene List
  ## get gene list path
  #listfiles <- list.files(file.path(r_path,"base/data/GeneList"), full.names = TRUE)

  ## load Gene list in list
  #GeneLists <- lapply(listfiles, function(x) t(unique(read.table(x))))
  #GeneLists <- t(unique(read.table(listfiles[5])))
  ## rename gene lists
  #names(GeneLists)<- basename(listfiles)


  #GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID, sep=""))))
  #GeneList <- t(unique(read.table(paste0(getwd(),"/inst/base/data/GeneList/102.txt", sep=""))))

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
    bioCancer::copy_all(bioCancer)
    set_class <- bioCancer::set_class
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
