output$StudiesTable <- DT::renderDataTable({

  displayTable(Studies)

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

  ##  needed to make silence the error
  if (inherits(try( dat <- r_data$ClinicalData[input$ui_Clinical_vars], silent=TRUE),"try-error")){
    dat <- r_data$ClinicalData
  }else{
    dat <- r_data$ClinicalData[input$ui_Clinical_vars]
  }
  displayTable(dat)
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
