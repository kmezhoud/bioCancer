#' Capture html output widget as .png in R
#'
#' @return  3 files .html, .js and .png
#'
#' @usage widgetThumbnail(p, thumbName, width = 1024, height = 1024)
#' @param p is the html widget
#' @param thumbName is the name of the new png file
#' @param width 1024
#' @param height 1024
#'
#' @examples
#' How <- "runManually"
#' \dontrun{
#' # Load package
#' library(networkD3)
#' library(htmlwidgets)
#' # Create fake data
#' src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
#' target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
#' networkData <- data.frame(src, target)
#' # Plot
#' plot = simpleNetwork(networkData)
#' # Save html as png
#' widgetThumbnail(p = plot, thumbName = "plot", width = 1024, height = 1024)
#' }
#' @export
#'
widgetThumbnail <- function(p, thumbName, width = 1024, height = 1024){
  phantom <- findPhantom()

  success <- FALSE
  if(phantom == "") {
    message("** phantomjs dependency could not be found - thumbnail cannot be generated.
            Please visit this page to install phantomjs on your system: http://phantomjs.org/download.html)")
  } else {
    res <- try({
      ff <- paste0(thumbName, ".html")
      ffjs <- paste0(thumbName, ".js")

      # don't want any padding
      p$sizingPolicy$padding <- 0
      suppressMessages(htmlwidgets::saveWidget(p, ff, selfcontained = FALSE))

      js <- paste0("var page = require('webpage').create();
                   page.viewportSize = { width: ", width,", height: ", height," };
                   page.clipRect = { top: 0, left: 0, width: ", width,", height: ", height," };
                   page.open('", ff, "', function(status) {
                   console.log(\"Status: \" + status);
                   if(status === \"success\") {
                   page.render('", thumbName, ".png', {format: 'png', quality: '100'});
                   }
                   phantom.exit();
    });")
      cat(js, file = ffjs)
      ## this line generate png file from JS
      base::system2(phantom,ffjs )

      ## delete files .html; .js, plot_files and .png
      #unlink(paste0(thumbName, '.html', sep=""), recursive = FALSE)
      unlink(paste0(thumbName, '.js', sep=""), recursive = FALSE)
      unlink(paste0(thumbName, '_files', sep=""), recursive = TRUE)
    })
    if(!inherits(res, "try-error")) {
      success <- TRUE
    }
    if(!file.exists(paste0(thumbName, ".png"))) {
      success <- FALSE
    }
  }

  if(!success) {
    message("** could not create htmlwidget thumbnail... creating an empty thumbnail...")
  }
  }

#' Check if PhantomJS is installed. Similar to webshot
#' @return Logic object
#' @usage findPhantom()
#' @examples
#' How <- "runManually"
#' \dontrun{
#' findPhantom()
#' }
#' @export
findPhantom <- function(){

  phantom <- Sys.which("phantomjs")

  if(Sys.which("phantomjs") == "") {
    if(identical(.Platform$OS.type, "windows")) {
      phantom <- Sys.which(file.path(Sys.getenv("APPDATA"), "npm", "phantomjs.cmd"))
    }
  }

  phantom

}

