output$ClinicalDataTable <- DT::renderDataTable({

  ##  needed to make silence the error
  if (inherits(try( dat <- r_data$ClinicalData[input$Clinical_varsID], silent=TRUE),"try-error")){
    dat <- r_data$ClinicalData
  }else{
    dat <- r_data$ClinicalData[input$Clinical_varsID]
  }
  displayTable(dat)
})



output$dl_Clinical_tab <- shiny::downloadHandler(

  filename = function() { paste0("Clinical_tab.csv") },
  content = function(file) {
   # data_filter <- if (input$show_filter) input$data_filter else ""
    get_data(r_data$ClinicalData[input$ClinicalDataTable_rows_all,], vars = input$Clinical_varsID,
            #rows = r_data$ClinicalData[grepl(paste0(data_filter, collapse="|"),r_data$ClinicalData),],
            na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)


observeEvent(input$ClinicalHelp_report, {

  cmd <- paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                 paste0(" get_data(r_data$ClinicalData[input$ClinicalDataTable_rows_all,], vars = input$Clinical_varsID,
                        na.rm = FALSE)"),
                 "\n",
                "\n```\n"
)
  update_report_fun(cmd)
})
