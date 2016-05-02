output$StudiesTable <- DT::renderDataTable({
  dat <- Studies
  # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  #DT::datatable(dat, filter = "top", rownames =FALSE, server = TRUE,
  DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                rownames = FALSE, style = "bootstrap", escape = FALSE,
                # class = "compact",
                options = list(
                  ajax = list(url = action),
                  search = list(search = "",regex = TRUE),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  autoWidth = FALSE,
                  processing = FALSE,
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                )
  )

})

output$dl_Studies_tab <- shiny::downloadHandler(
  filename = function() { paste0("Studies_tab.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(Studies, vars = NULL, filt = data_filter,
            rows = NULL,
            na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)



output$ClinicalDataTable <- DT::renderDataTable({


  #if(is.null(r_data$Clinical_vars)) return()
  ##### get Clinical Data for selected Case
  #dat <- cgdsr::getClinicalData(cgds, input$CasesID)
  ## change rownames in the first column
  #dat <- dat %>% add_rownames("Patients")

  #dat <- r_data[['ClinicalData']]

  ##  needed to make silence the error
  if (inherits(try( dat <- r_data$ClinicalData[input$ui_Clinical_vars], silent=TRUE),"try-error")){
    dat <- r_data$ClinicalData
  }else{
    dat <- r_data$ClinicalData[input$ui_Clinical_vars]
  }
  # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  #DT::datatable(dat, filter = "top", rownames =FALSE, server = TRUE,
  DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                rownames = FALSE, style = "bootstrap", escape = FALSE,
                class = "compact",
                options = list(
                  ajax = list(url = action),
                  search = list(regex = TRUE),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  autoWidth = FALSE,
                  processing = FALSE,
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                )
  )
})



output$dl_Clinical_tab <- shiny::downloadHandler(

  filename = function() { paste0("Clinical_tab.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(r_data$ClinicalData, vars = input$Clinical_vars, filt = data_filter,
            #rows = r_data$ClinicalData[grepl(paste0(data_filter, collapse="|"),r_data$ClinicalData),],
            na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)
