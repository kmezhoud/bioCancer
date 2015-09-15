output$ProfDataTable <- DT::renderDataTable({

#   #if (not_available(GeneList)) return("Load Gene List")
#   output$GeneListFromFile <- renderTable({
#
#     inFile <- input$file1
#
#     if (is.null(inFile))  return(NULL)
#
#     GeneList <- unique(read.table(inFile$datapath))
#     GeneList <- t(GeneList)
#   })

#   loadClipboardGeneList <- function(objname = "xls_data", ret = "", header = TRUE, sep = "\t") {
#
#     if (Sys.info()["sysname"] == "Windows") {
#       dat <- try(read.table("clipboard", header = header, sep = sep), silent = TRUE)
#     } else if (Sys.info()["sysname"] == "Darwin") {
#       dat <- try(read.table(pipe("pbpaste"), header = header, sep = sep), silent = TRUE)
#     } else {
#       dat <- try(read.table(text = input$load_cdata, header = header, sep = sep), silent = TRUE)
#     }
#
#     if (is(dat, 'try-error')) {
#       if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
#       upload_error_handler(objname,ret)
#     } else {
#       ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
#
#       #r_data[[objname]] <- data.frame(dat, check.names = FALSE)
#       #r_data[[paste0(objname,"description")]] <- ret
#       GeneList <- data.frame(dat, check.names = FALSE)
#     }
#     #r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
#   }
 if(input$GeneListID != "Genes"){
  GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
 }else{
  GeneList <- r_data$Genes
}
  #GeneList <- (r_data[[input$GeneListID]])
  #x <<- GeneList

  ##### Get Profile Data for selected Case and Genetic Profile
  if(length(GeneList)>500){
    dat <- getMegaProfData(GeneList,input$GenProfID,input$CasesID, Class="ProfData")
  } else{
    dat <- getProfileData(cgds,GeneList, input$GenProfID,input$CasesID)
  }


  #dat <- getProfileData(cgds, GeneList, input$GenProfID,input$CasesID)


  if(is.numeric(dat[2,2])){
  dat <- round(dat, digits = 3)
  }
  dat <- dat %>% add_rownames("Patients")
  r_data[['ProfData']] <- dat
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

output$dl_ProfData_tab <- shiny::downloadHandler(
  filename = function() { paste0("ProfData_tab.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(r_data$ProfData, vars = NULL, filt = data_filter,
            rows = NULL, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)
