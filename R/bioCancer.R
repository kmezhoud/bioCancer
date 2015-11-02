
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
<<<<<<< HEAD
#'@import dplyr
#'@importFrom plyr adply
=======
#'@import radiant
#'@import dplyr
#'@import plyr
>>>>>>> bbe4d9cf03dcd14dc250f9c2de17d39c14fb1c1b
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
#'@import gridExtra
#'@import GPArotation
#'@import knitr
#'@import pryr
#'@import DT
#'@import readr
#'@import scales
#'@import htmlwidgets
#'@import car
<<<<<<< HEAD
#'@import scales
=======
>>>>>>> bbe4d9cf03dcd14dc250f9c2de17d39c14fb1c1b
bioCancer <- function(){
  if ("package:bioCancer" %in% search()){
    runApp(paste0(system.file(package = "bioCancer", "/bioCancer", sep="")), launch.browser = TRUE)
  }else{
    stop("Calling bioCancer start function but bioCancer is not installed.")
  }
}
