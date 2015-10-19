
#' Launch bioCancer with default browser
#'
#' @return  web page of bioCancer Shiny App
#' @export
#'
#' @examples
#' \dontrun{
#' bioCancer()
#' }
#'@import shiny
#'@importFrom shinyAce aceEditor
#'@importFrom shinyAce updateAceEditor
#'@import radiant
#'@import dplyr
#'@import plyr
#'@import ggplot2
#'@import ggdendro
#'@import data.tree
#'@import lubridate
#'@importFrom stringr str_match
#'@import magrittr
#'@importFrom MathJaxR withMathJaxR
#'@import psych
#'@import tidyr
#'@importFrom yaml yaml.load
#'@importFrom AlgDesign optFederov
#'@importFrom MASS isoMDS
#'@importFrom broom glance
#'@importFrom broom tidy
#'@importFrom wordcloud textplot
#'@importFrom markdown markdownToHTML
#'@importFrom car leveragePlots
#'@importFrom car recode
#'@importFrom car vif
#'@importFrom curl curl
#'
#'
bioCancer <- function(){
  if (!"package:bioCancer" %in% search())
    if (!require(bioCancer)) stop("Calling bioCancer start function but bioCancer is not installed.")
  runApp(paste0(system.file(package = "bioCancer", "/bioCancer", sep="")), launch.browser = TRUE)
}
