output$ProfDataTable <- DT::renderDataTable({

GeneList <- whichGeneList(input$GeneListID)

  ##### Get Profile Data for selected Case and Genetic Profile
  if(length(GeneList)>500){
    dat <- getMegaProfData(GeneList,input$GenProfID,input$CasesID, Class="ProfData")
  } else{
    dat <- cgdsr::getProfileData(cgds,GeneList, input$GenProfID,input$CasesID)
  }

  if(is.numeric(dat[2,2])){
  dat <- round(dat, digits = 3)
  }
  dat <- dat %>% dplyr::add_rownames("Patients")
  r_data[['ProfData']] <- dat

    displayTable(dat)
})

output$dl_ProfData_tab <- shiny::downloadHandler(
  filename = function() { paste0("ProfData_tab.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(r_data$ProfData, vars = NULL, filt = data_filter,
            rows = NULL, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)
