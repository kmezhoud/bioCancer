output$MutDataTable <- DT::renderDataTable({
  req(input$StudiesID)
   req(input$Mut_varsID)
    ##  needed to make silence the error
    if (inherits(try( dat <- r_info$MutData[input$Mut_varsID], silent=TRUE),"try-error")){

      dat <- as.data.frame(paste0("There is no data with this genetic profile: ", input$GenProfID ," for selected Gene list."))
    }else{
      dat <- r_info$MutData[input$Mut_varsID]
    }
    displayTable(dat) %>%
      DT::formatStyle(names(dat),
                      color = DT::styleEqual(paste0("There is no ", input$GenProfID ," for selected Gene list."),
                                             'red'))#, backgroundColor = 'white', fontWeight = 'bold'

})


# output$MutDataTable <- DT::renderDataTable({
#   ## check if GenProf is mutation
#
#   # if (length(grep("mutation", input$GenProfID))==0){
#   #
#   #   dat <- as.data.frame("Please select mutations from Genetic Profiles")
#   #
#   # }else{
#     GeneList <- whichGeneList(input$GeneListID)
#
#     ##### Get Mutation Data for selected Case and Genetic Profile
#      if (inherits(try(dat <- cBioPortalData::getDataByGenes(api = cgds,
#                                                   studyId = input$StudiesID,
#                                                   genes = GeneList,
#                                                   by = "hugoGeneSymbol",
#                                                   molecularProfileIds = input$GenProfID
#                             ), silent=FALSE),"try-error")){
#
#       dat <- as.data.frame("There are some Gene Symbols not supported by cbioportal.
#                            Or the gene list is empty.
#                            Or bioCancer is not connected to cBioPortal server (check connection).")
#     }else{
#       shiny::withProgress(message = 'loading Mutation Data from cBioPortal server...', value = 1, {
#
#         dat <- cBioPortalData::getDataByGenes(api = cgds,
#                               studyId = input$StudiesID,
#                               genes = GeneList,
#                               by = "hugoGeneSymbol",
#                               molecularProfileIds = input$GenProfID) #%>%
#         #.[[1]] |>
#           #select(-c(uniqueSampleKey, uniquePatientKey, molecularProfileId, patientId, studyId))
#
#
#         if(dim(dat)[1]==0){
#           ## avoide error when GeneList is empty
#           ## Error..No.cancer.study..cancer_study_id...or.genetic.profile..genetic_profile_id..or.case.list.or..case_list..case.set..case_set_id..provid
#           dat <- as.data.frame("Gene List is empty.
#                                Or The Gene Symbols are not supported by cbioportal.
#                                Copy and paste genes from text file (Gene/line) or use gene list from examples.")
#         }else{
#           req(input$Mut_varsID)
#
#           dat <- dat[input$Mut_varsID]
#           r_data[['MutData']] <- dat
#         }
#       })
#     }
#   #}
#   displayTable(dat)%>% DT::formatStyle(names(dat),
#                                        color = DT::styleEqual("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.",
#                                                               'red'))#, backgroundColor = 'white', fontWeight = 'bold'
# })


output$dl_MutData_tab <- shiny::downloadHandler(
  filename = function() { paste0("MutData_tab.csv") },
  content = function(file) {
    #data_filter <- if (input$show_filter) input$data_filter else ""
    get_data(r_data$MutData[input$MutDataTable_rows_all,], vars = input$Mut_varsID,
            rows = NULL, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)


observeEvent(input$MutationHelp_report, {

  cmd <- paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0(" get_data(MutData[input$MutDataTable_rows_all,], vars = input$Mut_varsID,
                       na.rm = FALSE)"),
                "\n",
                "\n```\n"
  )
  update_report_fun(cmd)
})
