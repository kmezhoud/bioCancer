#' This is an htmlwidgets-based visualization tool for hierarchical data.
#' It is zoomable, meaning that you can interact with the hierarchy and zoom in/out accordingly.
#'
#' @return  A circular layout with genetic profile.
#'
#' @usage coffeewheel(treeData, width=600, height=600, main="", partitionAttribute="value")
#' @param treeData A hierarchical tree data as in example
#' @param width 600
#' @param height 600
#' @param main Title
#' @param partitionAttribute "value"
#'
#' @examples
#' How <- "runManually"
#' \dontrun{
#'  coffeewheel(treeData = sampleWheelData)
#'  }
#' @export
#'
coffeewheel <- function(treeData, width=600, height=600, main="", partitionAttribute="value"){
  x <- list(
    treeData = treeData,
    main = main,
    partitionAttribute = partitionAttribute
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'coffeewheel',
    x,
    width = width,
    height = height,
    package = 'bioCancer'
  )
}

#' Widget output function for use in Shiny
#'@return  A circular layout with genetic profile in Shiny App.
#'
#' @usage coffeewheelOutput(outputId, width=700, height=700)
#' @param outputId id
#' @param width 700
#' @param height 700
#'
#' @examples
#' How <- "runManually"
#' \dontrun{
#' coffeewheel(treeData = sampleWheelData)
#' }
#' @export
#'
coffeewheelOutput <- function(outputId, width=700, height=700) {
  htmlwidgets::shinyWidgetOutput(outputId, 'coffeewheel', width, height, package = 'bioCancer')
}

#' Widget render function for use in Shiny
#'@return  A circular layout with genetic profile in Shiny App.
#'
#' @usage renderCoffeewheel(expr, env = parent.frame(), quoted = FALSE)
#' @param expr id
#' @param env parent.frame
#' @param quoted FALSE
#'
#' @examples
#' How <- "runManually"
#' \dontrun{
#' coffeewheel(treeData = sampleWheelData)
#' }
#' @export
#'
renderCoffeewheel <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, coffeewheelOutput, env, quoted = TRUE)
}

#' @title  Sample data for wheel initialization
#' 
#' @description Example of data structure
#'
#' @format lists of lists hierarchical data
#'  @examples
#' \dontrun{
#' data(sampleWheelData)
#'}
# sampleWheelData <- list(
#   list(
#     name="R",
#     children=list(
#       list(name="R_1", colour="#110000"),
#       list(name="R_3", colour="#330000"),
#       list(name="R_5", colour="#550000"),
#       list(name="R_7", colour="#770000"),
#       list(name="R_9", colour="#990000"),
#       list(name="R_b", colour="#bb0000"),
#       list(name="R_d", colour="#dd0000"),
#       list(name="R_f", colour="#ff0000")
#     )
#   ),
#   list(
#     name="G",
#     children=list(
#       list(name="G_1", colour="#001100"),
#       list(name="G_3", colour="#003300"),
#       list(name="G_5", colour="#005500"),
#       list(name="G_7", colour="#007700"),
#       list(name="G_9", colour="#009900"),
#       list(name="G_b", colour="#00bb00"),
#       list(name="G_d", colour="#00dd00"),
#       list(name="G_f", colour="#00ff00")
#     )
#   ),
#   list(
#     name="B",
#     children=list(
#       list(name="B_1", colour="#000011"),
#       list(name="B_3", colour="#000033"),
#       list(name="B_5", colour="#000055"),
#       list(name="B_7", colour="#000077"),
#       list(name="B_9", colour="#000099"),
#       list(name="B_b", colour="#0000bb"),
#       list(name="B_d", colour="#0000dd"),
#       list(name="B_f", colour="#0000ff")
#     )
#   )
# )
