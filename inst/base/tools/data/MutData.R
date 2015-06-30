output$MutDataTable <- DT::renderDataTable({
  ## check if GenProf is mutation

  if (length(grep("mutation", input$GenProfID))==0){

    dat <- as.data.frame("Please select mutations from Genetic Profiles")

  }else if(input$GeneListID != "Genes"){

    GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))

  #GeneList <- (r_data[[input$GeneListID]])

  ##### Get Mutation Data for selected Case and Genetic Profile
  #dat <- getProfileData(cgds, GeneList, input$GenProfID,input$CasesID)
  dat <- getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)
  ## change rownames in the first column
  dat <- as.data.frame(dat %>% add_rownames("Patients"))
  dat <- dat[input$ui_Mut_vars]
  }
#   if(is.numeric(dat[1,1])){
#     dat <- round(dat, digits = 3)
#   }
  ####

  # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  #DT::datatable(dat, filter = "top", rownames = FALSE, server = TRUE,
  DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                              rownames = FALSE, style = "bootstrap", escape = FALSE,
                # class = "compact",
                options = list(
                  ajax = list(url = action),
                  search = list(regex = TRUE),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  autoWidth = TRUE,
                  processing = FALSE,
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                )
  )

})
