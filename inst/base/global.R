# path to use for local and server use
r_path <- ifelse(file.exists("../base") && file.exists("../quant"), "..",
                 system.file(package = "radiant"))
if (r_path == "") r_path <- ".."  # if radiant is not installed revert to local inst

# reactive programming in Shiny requires (some) use of global variables
# currently these are r_env, r_data, r_state, r_local, r_path, r_sessions, r_ssuid

## print options
options("width"=200)
options("scipen"=100)

## pkgs used
pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "wordcloud",
               "AlgDesign", "knitr", "lubridate", "ggplot2", "ggdendro",
               "pryr", "shiny", "magrittr", "tidyr", "dplyr", "broom",
               "htmlwidgets")
pkgs_gh <- c("shinyAce","rpivotTable","DT")
# pkgs_gh <- c("shinyAce","rpivotTable")
pkgs <- c(pkgs_cran, pkgs_gh)
rm(pkgs_cran,pkgs_gh)

## list of function arguments
expl_functions <- list("n" = "length", "mean" = "mean_rm", "median" = "median_rm",
                       "min" = "min_rm", "max" = "max_rm", "25%" = "p25",
                       "75%" = "p75", "sd" = "sd_rm", "se" = "serr",
                       "cv" = "cv", "skew" = "skew", "kurtosis" = "kurtosi",
                       "# missing" = "nmissing")

#<<<<<<< HEAD
# from: http://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
# hasIP <- function() {
#   if (.Platform$OS.type == "windows") {
#     ip <- system("ipconfig", intern = TRUE)
#   } else {
#     ip <- system("ifconfig", intern = TRUE)
#   }
#
#   validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
#   any(grep(validIP, ip))
# }
#
# withMathJaxIP <- function(...) {
#   if(hasIP()) {
#     withMathJax(...)
#   } else {
#     tagList(...)
#   }
# }

# withMathJaxIP <- withMathJax
# withMathJaxIP <- function(...)  tagList(...)

# if(havingIP()) {
#   withMathJaxIP <- withMathJax
# } else {
#   withMathJaxIP <- function(...) tagList(...)
# }
# if(havingIP()) withMathJaxIP(.) else . }

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(echo=FALSE, comment=NA, cache=FALSE, message=FALSE,
                      warning=FALSE, fig.path = "~/radiant_figures/")
#>>>>>>> upstream/master

## using DT rather than Shiny versions of datatable
renderDataTable <- DT::renderDataTable
dataTableOutput <- DT::dataTableOutput
datatable       <- DT::datatable

## running local or on a server
if (Sys.getenv('SHINY_PORT') == "") {

  r_local <- TRUE
#<<<<<<< HEAD
#  options(shiny.maxRequestSize=-1) # no limit to filesize locally
#=======
  options(shiny.maxRequestSize = -1) ## no limit to filesize locally
#>>>>>>> upstream/master

  ## if radiant package was not loaded load dependencies
  if (!"package:radiant" %in% search())
    sapply(pkgs, require, character.only=TRUE)

} else {
  r_local <- FALSE
#<<<<<<< HEAD
  #options(shiny.maxRequestSize=5*1024^2) # limit upload filesize on server (5MB)
  #sapply(pkgs, require, character.only=TRUE)
#=======
  options(shiny.maxRequestSize = 5 * 1024^2)   ## limit upload filesize on server (5MB)
  sapply(pkgs, require, character.only = TRUE)
#>>>>>>> upstream/master
}

## environment to hold session information
r_sessions <- new.env(parent = emptyenv())

## create directory to hold session files
if (!r_local)
  "~/r_sessions/" %>% { if (!file.exists(.)) dir.create(., recursive = TRUE) }

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(r_path,"base/tools/help/figures/"))
addResourcePath("imgs", file.path(r_path,"base/www/imgs/"))
addResourcePath("js", file.path(r_path,"base/www/js/"))

#<<<<<<< HEAD
### options used for debugging
# options(shiny.trace = TRUE)
# options(shiny.error=recover)

### options used for debugging when warnings are given
# options(warn=0)
#=======
## using local mathjax if available
if ("MathJaxR" %in% installed.packages()[,"Package"]) {
  addResourcePath("MathJax", file.path(system.file(package = "MathJaxR"), "MathJax/"))
  withMathJax <- MathJaxR::withMathJaxR
}

# if (r_local) {
#   addResourcePath("MathJax", file.path(system.file(package = "MathJaxR"), "MathJax/"))
#   withMathJax <- MathJaxR::withMathJaxR
# }

## options used for debugging
# options(shiny.trace = TRUE)
# options(shiny.error=recover)
#>>>>>>> upstream/master
# options(warn=2)
# options(warn=0)

# Windows or Mac
# if (.Platform$OS.type == 'windows') {
#   Sys.setlocale(category = 'LC_ALL','English_United States.1252')
# } else {
#   Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
# }
