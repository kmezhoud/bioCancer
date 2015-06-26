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
                  search = list(regex = TRUE),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  autoWidth = FALSE,
                  processing = FALSE,
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                )
  )

})





output$ClinicalDataTable <- DT::renderDataTable({

  #if (not_available(input$view_vars)) return()

  ##### get Clinical Data for selected Case
  dat <- getClinicalData(cgds, input$CasesID)
  ## change rownames in the first column
  dat <- dat %>% add_rownames("Patients")
  ####

  # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  #DT::datatable(dat, filter = "top", rownames =FALSE, server = TRUE,
  DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                              rownames = FALSE, style = "bootstrap", escape = FALSE,
                # class = "compact",
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
