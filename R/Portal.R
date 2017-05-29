#' Display dataframe in table using DT package
#'
#' @usage displayTable(df)
#' @param df a dataframe
#'
#' @return A table
#'
#' @examples
#' session <- NULL
#' cgds <- CGDS("http://www.cbioportal.org/public-portal/")
#' Studies<- getCancerStudies(cgds)
#' \dontrun{
#' displayTable(Studies)
#' }
#'@export
displayTable <- function(df){
  # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
  action = DT::dataTableAjax(session, df, rownames = FALSE)

  #DT::datatable(dat, filter = "top", rownames =FALSE, server = TRUE,
  table <- DT::datatable(df, filter = list(position = "top", clear = FALSE, plain = TRUE),
                rownames = FALSE, style = "bootstrap", escape = FALSE,
                # class = "compact",
                options = list(
                  ajax = list(url = action),
                  search = list(search = "",regex = TRUE),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  autoWidth = FALSE,
                  processing = FALSE,
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                )
  )
  return(table)
}

#' Verify which gene list is selected
#'
#' @usage whichGeneList(geneListLabel)
#'
#' @param geneListLabel The label of GeneList. There are three cases:
#'        "Genes" user gene list,
#'        "Reactome_GeneList" GeneList plus genes from reactomeFI
#'       "file name" from Examples
#'
#' @return Gene List label
#'
#' @examples
#' How <- "runManually"
#' \dontrun{
#' whichGeneList("102")
#'}
#' @export
whichGeneList <- function(geneListLabel){
  if(geneListLabel == "Genes"){
    GeneList <- r_data$Genes
  }else if(geneListLabel == "Reactome_GeneList"){
    GeneList <- t(r_data$Reactome_GeneList)
  }else if(r_path == "inst"){
    ## For server
    GeneList <- t(unique(read.table(paste0(r_path,"/base/data/GeneList/",geneListLabel,".txt" ,sep=""))))
  } else{
    ## For R package
    GeneList <- t(unique(read.table(paste0(path.package('bioCancer'),"/base/data/GeneList/",geneListLabel,".txt" ,sep=""))))
  }
  return(GeneList)

}
