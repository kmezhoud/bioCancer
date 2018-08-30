## Welcome message
output$Welcome <- renderUI({
  wellPanel(
    h5("Welcome to bioCancer!", align="center"),
    HTML("Help is available on each page by clicking the <i title='Help' class='fa fa-question'></i> icon on the bottom left of your screen."),
    checkboxInput(
      'overview_id', 'Pipeline Overview', value = FALSE
    )
  )
})

output$pipeline <- renderUI({
  tagList(
    h4("Schematic view of bioCancer pipeline:"),
    h5(" The pipeline consists of three major panels:"),
    h5("1- Portal loads own gene list and explore profiles data of cancer studies."),
    h5("2- Processing exchanges data with user computer, Portal section, and Circomics tab.
       It provides useful tools for pre-processing, processing, and plotting graphs."),
    h5("3- Enrichment integrates methods for classification and clustering and displays the results as interactive graphs.")
    )
})

output$overview <- renderImage({

  list(src = file.path( getOption("radiant.path.bioCancer"),"app/tools/help/figures/overview_methods.png"),
       contentType = 'image/png',
       width = 600,
       height = 500,
       deleteFile = FALSE,
       alt = "This is alternate text"
  )}, deleteFile = FALSE
)


output$dl_Studies_tab <- shiny::downloadHandler(
  filename = function() { paste0("Studies_tab.csv") },
  content = function(file) {
    # append _rows_all to StudiesTable ID of the "Studies" dataframe
    get_data(Studies[input$StudiesTable_rows_all,], vars = NULL,
            rows = NULL,
            na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)

output$ui_Studies <- renderUI({
  #help_modal_km('Workflow Overview','StudiesHelp',inclMD(file.path(r_path,"base/tools/help/Studies.md")))
  help_and_report(modal_title = "Workflow Overview", fun_name = "Studies",
                  author = "Karim Mezhoud",
                  help_file = inclRmd(file.path(
                    getOption("radiant.path.bioCancer"),"app/tools/help/Studies.md")))

})

output$StudiesTable <- DT::renderDataTable({
  shiny::withProgress(message = 'loading Studies from cgdsr server...', value = 1, {

  displayTable(Studies)
  })

})
