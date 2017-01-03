output$ProfDataTable <- DT::renderDataTable({

  GeneList <- whichGeneList(input$GeneListID)

  if (ncol(GeneList)==0){
    dat <- as.data.frame("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.")
  }else{
    if(length(GeneList)>500){

      shiny::withProgress(message = 'loading MegaProfData...', value = 0.1, {
        Sys.sleep(0.25)

        ##### Get Profile Data for selected Case and Genetic Profile
        dat <- getMegaProfData(GeneList,input$GenProfID,input$CasesID, Class="ProfData")
      })
    } else if (inherits(try( dat <- cgdsr::getProfileData(cgds,GeneList, input$GenProfID,input$CasesID),
                             silent=FALSE),"try-error")){
      dat <- as.data.frame("There are some Gene Symbols not supported by cbioportal.
                           Or bioCancer is not connected to cgdsr server (check connection).")
    }else{
      dat <- cgdsr::getProfileData(cgds,GeneList, input$GenProfID,input$CasesID)
      if(all(dim(dat)==c(0,1))== TRUE){
        ## avoide error when GeneList is empty
        ## Error..No.cancer.study..cancer_study_id...or.genetic.profile..genetic_profile_id..or.case.list.or..case_list..case.set..case_set_id..provid
        dat <- as.data.frame("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.")
      }else{
        #dat <- cgdsr::getProfileData(cgds,GeneList, input$GenProfID,input$CasesID)
        ## remove empty row
        dat <-  dat[which(apply(!(apply(dat,1,is.na) ),2,sum)!=0 ),]

        if(is.numeric(dat[2,2])){
          dat <- round(dat, digits = 3)
        }
        dat <- dat %>% tibble::rownames_to_column("Patients")
        r_data[['ProfData']] <- dat
      }
    }

    displayTable(dat)%>% DT::formatStyle(names(dat),
                                         color = DT::styleEqual("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.",
                                                                'red'))#, backgroundColor = 'white', fontWeight = 'bold'

  }

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
