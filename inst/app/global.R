############################################
#    SOURCING FROM  radiant.data PACKAGE   #
############################################

## Must be in global.R before sourcing from bioCancer
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"),
       encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)
## sourcing functions from radiant.data radiant.R file
#source(file.path(getOption("radiant.path.bioCancer"), "app/radiant.R"),
#      encoding = getOption("radiant.encoding"), local = TRUE)


## Setting bioCancer package path
options(radiant.path.bioCancer = system.file(package = "bioCancer"))


## option to change theme
options(radiant.nav_ui =
          list(windowTitle = "bioCancer" ,theme= shinythemes::shinytheme("cerulean") , id = "nav_radiant",
               inverse = TRUE, collapsible = TRUE, tabPanel("Workspace", withMathJax(), uiOutput("ui_data"))))



## change help menu function
## function to generate help, must be in global because used in ui.R
help_menu <- function(hlp) {
  tagList(
    navbarMenu("", icon = icon("question-circle"),
               tabPanel("Help", uiOutput(hlp), icon = icon("question")),
               #tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
               tabPanel("About", uiOutput("help_about"), icon = icon("info")),
               #tabPanel(tags$a("", href = "http://kmezhoud.github.io/bioCancer/", target = "_blank",
                #               list(icon("globe"), "Radiant docs"))),
               tabPanel(tags$a("", href = "https://github.com/kmezhoud/bioCancer/issues", target = "_blank",
                               list(icon("github"), "Report issue")))
    ),
    tags$head(
      tags$script(src = "js/session.js"),
      tags$script(src = "js/returnTextAreaBinding.js"),
      tags$script(src = "js/returnTextInputBinding.js"),
      tags$script(src = "js/video_reset.js"),
      tags$script(src = "js/message-handler.js"),
      tags$script(src = "js/run_return.js"),
      # tags$script(src = "js/draggable_modal.js"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )
}
## needed to change author in the helps of the Menu help
options(radiant.help.cc = "&copy; Karim Mezhoud (2020) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'
        target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a></br>")


## set path for package
ifelse (grepl("bioCancer", getwd()) && file.exists("../../inst") , "..",
        system.file(package = "bioCancer")) %>%
  options(radiant.path.bioCancer = .)

## loading urls and ui
source(file.path(getOption("radiant.path.bioCancer"), "app/init.R"),
       encoding = getOption("radiant.encoding"), local = TRUE)

## loading functions no longer used by radiant.data
source(file.path(getOption("radiant.path.bioCancer"), "app/radiant_old.R"),
       encoding = getOption("radiant.encoding"), local = TRUE)


## needed to change figures in helps
addResourcePath("figures", file.path(getOption("radiant.path.bioCancer"), "app/tools/help/figures/"))
addResourcePath("imgs", file.path(getOption("radiant.path.bioCancer"), "app/www/logo/"))
#addResourcePath("js", file.path(getOption("radiant.path.bioCancer"), "app/www/js/"))
#addResourcePath("js", file.path(getOption("radiant.path.bioCancer"), "htmlwidgets/"))
#addResourcePath("js", file.path(getOption("radiant.path.bioCancer"), "htmlwidgets/tooltipster/"))

## set example data
options(radiant.example.data = "bioCancer")




## cleanup the global environment if stop button is pressed in Rstudio
## based on barbara's reply to
## https://community.rstudio.com/t/rstudio-viewer-window-not-closed-on-shiny-stopapp/4158/7?u=vnijs
onStop(function() {
  ## don't run if the stop button was pressed in Radiant
  if (!exists("r_data")) {
    unlink("~/r_figures/", recursive = TRUE)
    clean_up_list <- c(
      "r_sessions", "help_menu", "make_url_patterns", "import_fs",
      "init_data", "navbar_proj", "knit_print.data.frame", "withMathJax"
    )
    suppressWarnings(
      suppressMessages({
        res <- try(sapply(clean_up_list, function(x) if (exists(x, envir = .GlobalEnv)) rm(list = x, envir = .GlobalEnv)), silent = TRUE)
        rm(res)
      })
    )
    message("Stopped bioCancer\n")
    stopApp()
  }
})
