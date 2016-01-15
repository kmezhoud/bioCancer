
#' Circular plot of hierarchital data of genetic profile.
#' @return  A circular layout with genetic profile.
#'
#' @param treeData A hierarchical tree data as in example
#' @param width 600
#' @param height 600
#' @param main Title
#' @param showLegend FALSE
#' @param legenBreacks NULL
#' @param legendColors NULL
#' @param fontSize 12
#' @param legendText Legend
#'
#' @seealso https://github.com/armish/metabologram
#' @usage metabologram(treeData = sampleMetabologramData, width=600, height=600, main=title, showLegend = TRUE, fontSize = 10, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")
#' @examples
#' \dontrun{
#' library(bioCancer)
#' bioCancer::metabologram(treeData = sampleMetabologramData, width=600, height=600, main=title, showLegend = TRUE, fontSize = 10, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")
#' }
#'
#' @import htmlwidgets
#'
#' @export
metabologram <- function(treeData,
                         width=600,
                         height=600,
                         main="",
                         showLegend=FALSE,
                         legendBreaks=NULL,
                         legendColors=NULL,
                         fontSize=12,
                         legendText="Legend") {
  x <- list(
    treeData = treeData,
    showLegend = showLegend,
    legendBreaks = legendBreaks,
    legendColors = legendColors,
    legendText = legendText,
    fontSize = fontSize,
    main = main
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'metabologram',
    x,
    width = width,
    height = height,
    package = 'bioCancer'
  )
}

# #' Widget output function for use in Shiny
# #'
# #'@return  A circular plot with genetic profile in Shiny App.
# #'
# #' @param output id
# #' @param width 600
# #' @param height 600
# #' @usage metabologram(treeData = sampleMetabologramData)
# #' @examples
# #' \dontrun{
# #' library(bioCancer)
# #' bioCancer::metabologram(treeData = sampleMetabologramData)
# #' }
# #' @export
# metabologramOutput <- function(outputId, width = 600, height = 500) {
#   shinyWidgetOutput(outputId, 'metabologram', width, height, package = 'bioCancer')
# }
#
# #' Widget render function for use in Shiny
# #'@return  A circular plot with genetic profile in Shiny App.
# #'
# #' @param expr
# #' @param env parent.frame()
# #' @param quoted FALSE
# #' @usage metabologram(treeData = sampleMetabologramData)
# #' @examples
# #' \dontrun{
# #' library(bioCancer)
# #' bioCancer::metabologram(treeData = sampleMetabologramData)
# #' }
# #' @export
# renderMetabologram <- function(expr, env = parent.frame(), quoted = FALSE) {
#   if (!quoted) { expr <- substitute(expr) } # force quoted
#   shinyRenderWidget(expr, metabologramOutput, env, quoted = TRUE)
# }
#
# #' Sample data for wheel initialization
# #' @metabologram(treeData = sampleMetabologramData)
# #' @examples
# #' \dontrun{
# #' library(bioCancer)
# #' bioCancer::metabologram(treeData = sampleMetabologramData)
# #' }
# #' @export
# sampleMetabologramData <- list(
#   list(
#     name="Metabolites",
#     colour="#FFDDDD",
#     children=list(
#       list(name="M_1", colour="#0000FF"),
#       list(name="M_5", colour="#F3F3FF"),
#       list(name="M_9", colour="#FFF3F3"),
#       list(name="M_b", colour="#FFDDDD"),
#       list(name="M_f", colour="#FF8585")
#     )
#   ),
#   list(
#     name="Genes",
#     colour="#FF6E6E",
#     children=list(
#       list(name="G_1", colour="#B1B1FF"),
#       list(name="G_3", colour="#DDDDFF"),
#       list(name="G_5", colour="#F3F3FF"),
#       list(name="G_7", colour="#FF6E6E"),
#       list(name="G_9", colour="#8585FF"),
#       list(name="G_b", colour="#FF1616"),
#       list(name="G_d", colour="#FF6E6E"),
#       list(name="G_f", colour="#FF0000")
#     )
#   )
# )
#
# #' Sample breaks data
# #' @usage metabologram(treeData = sampleMetabologramData)
# #' @export
# sampleMetabologramBreaks <- c(
#   -3.00, -2.75, -2.50, -2.25, -2.00, -1.75, -1.50, -1.25, -1.00,
#   -0.75, -0.50, -0.25, 0.00,  0.25,  0.50,  0.75,  1.00,
#   1.25,  1.50,  1.75,  2.00,  2.25,  2.50,  2.75, 3.00
# )
#
# #' Sample colors data
# #' @usage metabologram(treeData = sampleMetabologramData)
# #' @export
# sampleMetabologramColors <- c(
#   "#0000FF", "#1616FF", "#2C2CFF", "#4242FF", "#5858FF", "#6E6EFF", "#8585FF",
#   "#9B9BFF", "#B1B1FF", "#C7C7FF", "#DDDDFF", "#F3F3FF", "#FFF3F3", "#FFDDDD",
#   "#FFC7C7", "#FFB1B1", "#FF9B9B", "#FF8585", "#FF6E6E", "#FF5858", "#FF4242",
#   "#FF2C2C", "#FF1616", "#FF0000"
# )
