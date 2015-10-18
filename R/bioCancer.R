
#' Launch bioCancer with default browser
#'
#' @return  web page of bioCancer Shiny App
#' @export
#'
#' @examples
#' \dontrun{
#' bioCancer()
#' }
bioCancer <- function(){
  if (!"package:bioCancer" %in% search())
    if (!require(bioCancer)) stop("Calling bioCancer start function but bioCancer is not installed.")
  runApp(paste0(system.file(package = "bioCancer", "/bioCancer", sep="")), launch.browser = TRUE)
}
