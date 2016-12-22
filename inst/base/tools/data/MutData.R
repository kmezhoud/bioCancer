output$MutDataTable <- DT::renderDataTable({
  ## check if GenProf is mutation

  if (length(grep("mutation", input$GenProfID))==0){

    dat <- as.data.frame("Please select mutations from Genetic Profiles")

  }else{
    GeneList <- whichGeneList(input$GeneListID)

    ##### Get Mutation Data for selected Case and Genetic Profile
    if(length(GeneList)>500){
      dat <- getMegaProfData(GeneList,input$GenProfID,input$CasesID, Class="MutData")
    } else if (inherits(try(dat <- cgdsr::getMutationData(cgds,input$CasesID, input$GenProfID, GeneList), silent=FALSE),"try-error")){

      dat <- as.data.frame("There are some Gene Symbols not supported by cbioportal.
                           Or bioCancer is not connected to cgdsr server (check connection).")
    }else{
      dat <- cgdsr::getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)
      if(dim(dat)[1]==0){
        ## avoide error when GeneList is empty
        ## Error..No.cancer.study..cancer_study_id...or.genetic.profile..genetic_profile_id..or.case.list.or..case_list..case.set..case_set_id..provid
        dat <- as.data.frame("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.")
      }else{
        req(input$ui_Mut_vars)
        # dat <- cgdsr::getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)
        ## change rownames in the first column
        dat <- as.data.frame(dat %>% tibble::rownames_to_column("Patients"))
        dat <- dat[input$ui_Mut_vars]
        r_data[['MutData']] <- dat
      }
    }
  }
  displayTable(dat)%>% DT::formatStyle(names(dat),
                                       color = DT::styleEqual("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.",
                                                              'red'))#, backgroundColor = 'white', fontWeight = 'bold'
})


output$dl_MutData_tab <- shiny::downloadHandler(
  filename = function() { paste0("MutData_tab.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(r_data$MutData, vars = input$ui_Mut_vars, filt = data_filter,
            rows = NULL, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)
