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
#'@import shiny
#'@import plyr
#'@import ggdendro
#'@import data.tree
#'@importFrom stringr str_match
#'@importFrom psych KMO corr.test cortest.bartlett fa.sort principal skew
#'@importFrom yaml yaml.load
#'@importFrom AlgDesign optFederov
#'@importFrom MASS isoMDS
#'@importFrom wordcloud textplot
#'@importFrom car leveragePlots recode vif
#'@importFrom curl curl
#'@import GPArotation
#'@importFrom DT datatable dataTableOutput renderDataTable styleColorBar
#'@importFrom DT styleInterval JS formatStyle formatPercentage
#'@import scales
#'@import htmlwidgets
#'@importFrom htmlwidgets shinyRenderWidget
#'@import covr
#'@importFrom jsonlite fromJSON
#'@import stats
#'

NULL

bioCancer <- function(){
  if ("package:bioCancer" %in% search()){
    runApp(paste0(system.file(package = "bioCancer", "/bioCancer", sep="")), launch.browser = TRUE)
  }else{
    stop("Install and load bioCancer package before to run it.")
  }
}
