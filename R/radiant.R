#' Return message when the filter formula is not correct (mRNA > 500)
#'
#' @return  text message
#' @usage returnTextAreaInput(inputId,
#'                           label= NULL,
#'                           rows = 2,
#'                          placeholder = NULL,
#'                          resize= "vertical",
#'                          value = "")
#'
#' @param inputId The ID of the object
#' @param label Text describes the box area
#' @param rows  Number of rows
#' @param placeholder  Error message if needed 
#' @param resize orientation of text
#' @param value default text in the area box
#' 
#' @examples
#' ShinyApp <-  1
#' \dontrun{
#' returnTextAreaInput(inputId = "data-filter",
#'                     label = "Error message",
#'                     rows =  2,
#'                     placeholder = "Provide a filter (e.g., Genes == 'ATM') and press return",
#'                     resize = "vertical",
#'                     value="")
#' }
returnTextAreaInput <- function(inputId,
                                label = NULL,
                                rows = 2,
                                placeholder = NULL,
                                resize = "vertical",
                                value = "") {
  tagList(
    tags$label(label, `for` = inputId), br(),
    tags$textarea(
      value,
      id = inputId,
      type = "text",
      rows = rows,
      placeholder = placeholder,
      resize = resize,
      class = "returnTextArea form-control"
    )
  )
}
