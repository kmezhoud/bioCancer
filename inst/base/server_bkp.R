# shinyServer(function(input, output, session) {
#
#   cgds <- CGDS("http://www.cbioportal.org/public-portal/")
#   Studies<- getCancerStudies(cgds)
#
#   ## sidebar menu of Studies
#   updateSelectizeInput(session, 'StudiesID', choices = Studies[,1], selected = "gbm_tcga_pub")
#
#   ## get Cases in side bar panel
#   output$ui_Cases <- renderUI({
#     selectInput("CasesID", "Cases for selected study",getCaseLists(cgds,input$StudiesID)[,1] )
#   })
#
#   ## get Genetic Profiles in side bar panel
#   output$ui_GenProfs <- renderUI({
#     selectInput("GenProfID", "Genetic Profiles",getGeneticProfiles(cgds,input$StudiesID)[,1] )
#   })
#
#   ## source shared functions
#   source("global.R", encoding = "UTF-8", local = TRUE)
# 	source("init.R", encoding = r_encoding, local = TRUE)
# 	source("bioCancerInit.R", encoding = r_encoding, local = TRUE)
#
#
#
#   ## get Gene List in side bar panel
#
#   output$ui_GeneList <- renderUI({
#     selectInput("GeneListID", "Gene List:", r_data$genelist)
#   })
#
#  	if (!"package:bioCancer" %in% search()) {
#  		if (r_path == "..") {
#       for (file in list.files("../../R",
# 		      pattern="\\.(r|R)$",
# 		      full.names = TRUE)) {
#
# 		  	source(file, encoding = r_encoding, local = TRUE)
# 		  }
# 		} else {
# 			bioCancer::copy_all(bioCancer)
#       set_class <- bioCancer::set_class
# 		}
# 	} else {
#
# 	  copy_from(bioCancer, state_init, state_single, state_multiple)
#
# 	}
#
# 	## source data & analysis tools
#   for (file in list.files(c("tools/app","tools/data"),
#       pattern="\\.(r|R)$",
#       full.names = TRUE)) {
#
#   	source(file, encoding = r_encoding, local = TRUE)
#   }
#
#   ## save state on refresh or browser close
#   saveStateOnRefresh(session)
# })
