shinyServer(function(input, output, session) {


  ##################
  # for cgdsr
  library(cgdsr)
  cgds <- CGDS("http://www.cbioportal.org/public-portal/")
  Studies<- getCancerStudies(cgds)
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
#GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/73.txt", sep=""))))

  ## get Cases in side bar panel
  output$ui_Cases <- renderUI({
    selectInput("CasesID", "Cases for selected study",getCaseLists(cgds,input$StudiesID)[,1] )
  })

  ## get Genetic Profiles in side bar panel
  output$ui_GenProfs <- renderUI({
    selectInput("GenProfID", "Genetic Profiles",getGeneticProfiles(cgds,input$StudiesID)[,1] )
  })

  # source shared functions
	source("init.R", local = TRUE)
	source("radiant.R", local = TRUE)


  ##################

 #for(i in 1:length(GeneLists)){
  r_data[["DNA_damage_Response"]] <- c("CHEK1", "CHEK2", "RAD51", "BRCA1", "BRCA2", "MLH1", "MSH2","ATM", "ATR", "MDC1", "PARP1", "FANCF")
  r_data$genelist <- c("DNA_damage_Response")
  #r_data[[names(GeneLists)[i]]] <- c(r_data$genelists ,r_data$genelist)
#}


  ## get Gene List in side bar panel

  output$ui_GeneList <- renderUI({
    selectInput("GeneListID", "Gene List:", r_data$genelist)
  })

  ##################

	# for shiny-server
 	if (!"package:radiant" %in% search()) {
 		if (r_path == "..") {
      for (file in list.files("../../R",
		      pattern="\\.(r|R)$",
		      full.names = TRUE)) {

		  	source(file, local = TRUE)
		  }
		} else {
			radiant::copy_all(radiant)
      set_class <- radiant::set_class
		}
	} else {
	  copy_from(radiant, state_init, state_single, state_multiple)
	}

	# source data & analysis tools
  for (file in list.files(c("tools/app","tools/data"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, local = TRUE)
  }

  # save state on refresh or browser close
  saveStateOnRefresh(session)
})
