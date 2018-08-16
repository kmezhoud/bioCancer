######### ## functions that are not longer used by radiant.data ###########


## check if a button was pressed
pressed <- function(x) !is.null(x) && (is.list(x) || x > 0)

## check if a button was NOT pressed
not_pressed <- function(x) !pressed(x)



plot_downloader <- function(plot_name,
                            width = plot_width,
                            height = plot_height,
                            pre = ".plot_",
                            po = "dl_",
                            fname = plot_name) {
  ## link and output name
  lnm <- paste0(po, plot_name)
  ## create an output
  output[[lnm]] <- downloadHandler(
    filename = function() {
      paste0(fname, ".png")
    },
    content = function(file) {
      ## download graphs in higher resolution than shown in GUI (504 dpi)
      pr <- 5
      ## fix for https://github.com/radiant-rstats/radiant/issues/20
      w <- if (any(c("reactiveExpr", "function") %in% class(width))) width() * pr else width * pr
      h <- if (any(c("reactiveExpr", "function") %in% class(height))) height() * pr else height * pr
      plot <- try(get(paste0(pre, plot_name))(), silent = TRUE)
      if (is(plot, "try-error") || is.character(plot) || is.null(plot)) {
        plot <- ggplot() + labs(title = "Plot not available")
        pr <- 1
        w <- h <- 500
      }
      png(file = file, width = w, height = h, res = 96 * pr)
      print(plot)
      dev.off()
    }
  )
  downloadLink(lnm, "", class = "fa fa-download alignright")
}

## Avoid `XQuartz` requirement in radiant.data
## request XQuartz
## see https://github.com/tidyverse/ggplot2/issues/2655
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])) {
  options(bitmapType = "cairo")
}

  #############################################
  #  FUNCTIONS can not load from radiant.data #
  #############################################

#
# if (getOption("radiant.shinyFiles", FALSE)) {
#   download_link <- function(id) {
#     uiOutput(paste0("ui_", id))
#   }
#
#   download_button <- function(id, ...) {
#     uiOutput(paste0("ui_", id))
#   }
#
#   download_handler <- function(
#     id, label = "", fun = id, fn, type = "csv", caption = "Save to csv",
#     class = "", ic = "download", btn = "link", ...
#   ) {
#     ## create observer
#     shinyFiles::shinyFileSave(input, id, roots = sf_volumes, session = session)
#
#     ## create renderUI
#     if (btn == "link") {
#       output[[paste0("ui_", id)]] <- renderUI({
#         if (is.function(fn)) fn <- fn()
#         if (is.function(type)) type <- type()
#         shinyFiles::shinySaveLink(
#           id, label, caption, filename = fn, filetype = type,
#           class = "alignright", icon = icon(ic)
#         )
#       })
#     } else {
#       output[[paste0("ui_", id)]] <- renderUI({
#         if (is.function(fn)) fn <- fn()
#         if (is.function(type)) type <- type()
#         shinyFiles::shinySaveButton(
#           id, label, caption, filename = fn, filetype = type,
#           class = class, icon = icon("download")
#         )
#       })
#     }
#
#     observeEvent(input[[id]], {
#       if (is.integer(input[[id]])) return()
#       path <-  shinyFiles::parseSavePath(sf_volumes, input[[id]])
#       if (!inherits(path, "try-error") && !is_empty(path$datapath)) {
#         fun(path$datapath, ...)
#       }
#     })
#   }
# } else {
#   download_link <- function(id) {
#     downloadLink(id, "", class = "fa fa-download alignright")
#   }
#   download_button <- function(id, label = "Save", ic = "download", class = "", ...) {
#     downloadButton(id, label, class = class)
#   }
#   download_handler <- function(
#     id, label = "", fun = id, fn, type = "csv", caption = "Save to csv",
#     class = "", ic = "download", btn = "link", ...
#   ) {
#     output[[id]] <- downloadHandler(
#       filename = function() {
#         if (is.function(fn)) fn <- fn()
#         if (is.function(type)) type <- type()
#         paste0(fn, ".", type)
#       },
#       content = function(path) { fun(path, ...) }
#     )
#   }
# }
