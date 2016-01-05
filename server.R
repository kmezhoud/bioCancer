## SERVER FOR SHINY APP DEPLOY
## rsync -avz -e "ssh" bioCancer kmezhoud@46.101.227.144:/srv/shiny-server/
##
## ssh kmezhoud@46.101.227.144
## pass
## sudo R
## .libPaths(.libPaths()[3])
##  install.packages("devtools", lib="/usr/lib/R/library")
##  install_github("armish/coffeewheel", lib="/usr/lib/R/library")
## source("http://bioconductor.org/biocLite.R")
## biocLite("geNetClassifier, lib="/usr/lib/R/library")
## install.packages("AnnotationFuncs", repos = "http://www.bioconductor.org/packages/3.2/bioc", lib="/usr/local/share/R/bioconductor")

shinyServer(function(input, output, session) {

#   #library(networkD3)
#

  library(shinythemes)
  library(cgdsr)
  library(magrittr)
  library(coffeewheel)
  library(htmlwidgets)
 # library(metabologram)
  library(DiagrammeR)
  library(dplyr)
  library(geNetClassifier)
  library(RCurl)
  library(clusterProfiler)
  library(AnnotationFuncs)
#### masked
  library(plyr)

##library(DT)
library(Biobase)
library(DOSE)
library("org.Hs.eg.db")
library(XML)
  #
### package in NAMESSPACE not imported
  library(gridExtra)
  library(psych)
  library(broom)

  library(car)
  library(scales)
  library(ggplot2)
  library(covr)
  #library(hwriter)


  #   library(grDevices)
  #library(S4Vectors)
  ####masked package
  #library(lubridate)
  #remove(list = conflicts(detail = TRUE)$.GlobalEnv)



#   if (file.exists("inst/base") && file.exists("inst/bioCancer")){
#     #source("inst/bioCancer/global.R", encoding = r_encoding, local = TRUE)
#     setwd("inst/bioCancer")
#   }
  #source("../base/global.R", encoding = "UTF-8", local = TRUE)


  source("inst/base/init.R", encoding = r_encoding, local = TRUE)
  source("inst/base/bioCancerInit.R", encoding = r_encoding, local = TRUE)

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
    selectInput("CasesID", "Cases for selected study",getCaseLists(cgds,input$StudiesID)[,1] )
  })

  ## get Genetic Profiles in side bar panel
  output$ui_GenProfs <- renderUI({
    selectInput("GenProfID", "Genetic Profiles",getGeneticProfiles(cgds,input$StudiesID)[,1] )
  })


  ## source shared functions




  ## get Gene List in side bar panel

  output$ui_GeneList <- renderUI({
    selectInput("GeneListID", "Gene List:", r_data$genelist)
  })

 # if (!"package:bioCancer" %in% search()) {
  if (r_path == "inst") {
    for (file in list.files("inst/Rbis",
                            pattern="\\.(r|R)$",
                            full.names = TRUE)) {

      source(file, encoding = r_encoding, local = TRUE)
    }
  }
#  else {
#     radiant::copy_all(radiant)
#     set_class <- radiant::set_class
#   }
#     } else {
#       copy_from(bioCancer, state_init, state_single, state_multiple)
#     }

  ## source for Documents
#   for (file in list.files("R",
#                           pattern="\\.(r|R)$",
#                           full.names = TRUE)) {
#
#     source(file, encoding = r_encoding, local = TRUE)
#   }

  ## source data & analysis tools
  for (file in list.files(c("inst/base/tools/app","inst/base/tools/data","inst/base/tools/help"),
                          pattern="\\.(r|R)$",
                          full.names = TRUE)) {

    source(file, encoding = r_encoding, local = TRUE)
  }


  # source analysis tools from quant app
  for (file in list.files(c("inst/quant/tools/analysis", "inst/quant/tools/help"),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = r_encoding, local = TRUE)

  # source additional analysis tools for marketing app
  for (file in list.files(c("inst/bioCancer/tools/analysis","inst/bioCancer/tools/help"),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = r_encoding, local = TRUE)


  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
