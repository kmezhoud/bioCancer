output$MutDataTable <- DT::renderDataTable({
  ## check if GenProf is mutation

  if (length(grep("mutation", input$GenProfID))==0){

    dat <- as.data.frame("Please select mutations from Genetic Profiles")
  }else{

    GeneList <- whichGeneList(input$GeneListID)

    ##### Get Mutation Data for selected Case and Genetic Profile
    if(length(GeneList)>500){
      dat <- getMegaProfData(GeneList,input$GenProfID,input$CasesID, Class="MutData")
    } else{
      if (inherits(try(dat <- cgdsr::getMutationData(cgds,input$CasesID, input$GenProfID, GeneList), silent=FALSE),"try-error")){

        stop("There are some Gene Symbols not supported by cbioportal server")

      }else{
        dat <- cgdsr::getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)
      }
    }
    ## change rownames in the first column
    dat <- as.data.frame(dat %>% tibble::rownames_to_column("Patients"))
    dat <- dat[input$ui_Mut_vars]
    r_data[['MutData']] <- dat

    displayTable(dat)
  }
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
