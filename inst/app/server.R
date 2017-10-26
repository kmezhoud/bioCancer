shinyServer(function(input, output, session) {

    ##  trace error
   #options(warn=2, shiny.error= recover)


  ## connect to cgdsr server
  source(file.path(getOption("radiant.path.bioCancer"),"app/tools/bioCancer/cBioPortal.R"),
         encoding = getOption("radiant.encoding"), local = TRUE)


                      ############################################
                      #    SOURCING FROM  radiant.data PACKAGE   #
                      ############################################

  ## source shared functions
  source(file.path(getOption("radiant.path.data"),"app/init.R"),
         encoding = getOption("radiant.encoding"), local = TRUE)

  source(file.path(getOption("radiant.path.data"),"app/radiant.R"),
         encoding = getOption("radiant.encoding"), local = TRUE)
  #source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## source data & app tools from radiant.data
  for (file in list.files(c(file.path(getOption("radiant.path.data"),"app/tools/app"),
                            file.path(getOption("radiant.path.data"),"app/tools/data")
  ),
  pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)


  ## help ui
  source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)

  output$help_bioCancer_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_bioCancer_panel,
        uiOutput("help_text"),
        width = 3
      ),

      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_bioCancer"),
        htmlOutput("help_data")
      )
    )
  })
                  ############################################
                  #          SETTING NEW  ui_data            #
                  ############################################


  ## Sourcing global state from bioCancer
  source(file.path(getOption("radiant.path.bioCancer"), "app/global.R"),
         encoding = getOption("radiant.encoding"), local = TRUE)

  ## source tools from bioCancer app
  for (file in list.files(c(file.path(getOption("radiant.path.bioCancer"),"app/tools/bioCancer"),
                          file.path(getOption("radiant.path.bioCancer"),"app/tools/help")
                          ),
                          pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)


  # If bioCancer is not loaded
  if (!"package:bioCancer" %in% search() &&
      getOption("radiant.path.bioCancer") == "..") {
    ## for shiny-server and development
    for (file in list.files("../../R", pattern="\\.(r|R)$", full.names = TRUE))
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  }else {
    ## for use with launcher
    radiant.data::copy_all(bioCancer)
  }

  ## add Gene List
  #r_data[["DNA_damage_Response"]] <- as.data.frame("CHEK1", "CHEK2", "RAD51", "BRCA1", "BRCA2", "MLH1", "MSH2","ATM", "ATR", "MDC1", "PARP1", "FANCF")
  data_path <- file.path( system.file(package = "bioCancer"),"extdata/GeneList")
  r_data[["DNA_damage_Response"]] <- loadUserData("DNA_damage_Response", file.path(data_path,"DNA_damage_Response"), 'txt')
  r_data$genelist <- "DNA_damage_Response"

  ## save state on refresh or browser close
  saveStateOnRefresh(session)

  ## close browser when radiant.dose is stopped
  ## close Rstudio when radiant.dose os stopped
  # session$onSessionEnded(function() {
  #    stopApp()
  #    q("no")
  # })
})
