#' Launch bioCancer with default browser
#'
#' @return  web page of bioCancer Shiny App
#' @export
#' @usage bioCancer()
#'
#' @examples
#' ShinyApp <-  1
#' \dontrun{
#' bioCancer::bioCancer()
#' }
#'
#' @name bioCancer
#' @docType package
#' @import ggplot2
#' @import DiagrammeR
#' @importFrom knitr knit2html
#' @importFrom pryr where
#' @importFrom magrittr %<>% %T>% %$% set_rownames set_colnames set_names divide_by add extract2
#' @importFrom lubridate is.Date is.POSIXt now year month wday week hour minute second ymd mdy dmy ymd_hms hms hm as.duration parse_date_time
#' @importFrom broom tidy glance
#' @importFrom tidyr gather_ gather separate
#' @importFrom gridExtra arrangeGrob
#' @importFrom markdown markdownToHTML
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom readr read_delim write_csv
#' @import shiny
#' @importFrom DT datatable dataTableOutput renderDataTable styleColorBar
#' @importFrom DT styleInterval JS formatStyle formatPercentage
#' @import plyr
#' @import ggdendro
#' @import data.tree
#' @importFrom stringr str_match
#' @importFrom psych KMO corr.test cortest.bartlett fa.sort principal skew
#'@importFrom yaml yaml.load
#'@importFrom AlgDesign optFederov
#'@importFrom MASS isoMDS
#'@importFrom wordcloud textplot
#'@importFrom car leveragePlots recode vif
#'@importFrom curl curl
#'@import GPArotation
#'@import scales
#'@import htmlwidgets
#'@importFrom htmlwidgets shinyRenderWidget
#'@import covr
#'@importFrom jsonlite fromJSON
#'@import stats
#'

### dont import function from DT : conflicts with shiny
#@importFrom DT datatable dataTableOutput renderDataTable styleColorBar
#@importFrom DT styleInterval JS formatStyle formatPercentage
# @importFrom shiny actionButton actionLink addResourcePath browserViewer checkboxGroupInput checkboxInput conditionalPanel dateInput downloadButton downloadHandler downloadLink eventReactive fileInput
# @importFrom shiny h4 h5 headerPanel HTML htmlOutput icon imageOutput img includeCSS includeHTML includeMarkdown includeScript includeText incProgress inputPanel isolate mainPanel navbarMenu
# @importFrom shiny navbarPage navlistPanel observe observeEvent outputOptions paneViewer plotOutput plotPNG Progress reactive reactivePlot reactiveTable reactiveText
# @importFrom shiny reactiveUI reactiveValues renderPlot renderPrint renderText renderUI runApp selectInput selectizeInput setProgress shinyApp shinyServer shinyUI sidebarLayout sidebarPanel
# @importFrom shiny sliderInput stopApp submitButton tabPanel tabsetPanel tag tagList textInput textOutput titlePanel uiOutput updateCheckboxInput updateDateInput updateNavbarPage updateNumericInput updateRadioButtons
# @importFrom shiny  updateSelectInput updateSelectizeInput updateTabsetPanel updateTextInput validate validateCssUnit verbatimTextOutput verticalLayout wellPanel withMathJax withProgress
# @importFrom shiny pageWithSliderbar radioButton session viewer

bioCancer <- function(){
  if ("package:bioCancer" %in% search()){
    runApp(paste0(system.file(package = "bioCancer", "/bioCancer", sep="")), launch.browser = TRUE)
  }else{
    stop("Install and load bioCancer package before to run it.")
  }
}
