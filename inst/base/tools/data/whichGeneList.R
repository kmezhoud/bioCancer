#' @return  Verify which gene list is selected
#' @export
#'
#' @examples
#' \dontrun{
#'}
#'
whichGeneList <- function(){
  if(input$GeneListID == "Genes"){
    GeneList <- r_data$Genes
  }else if(input$GeneListID == "Reactome_GeneList"){
    GeneList <- t(r_data$Reactome_GeneList)
  }else{
    ## For server
    #GeneList <- t(unique(read.table(paste0(r_path,"/base/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
    ## For R package
    GeneList <- t(unique(read.table(paste0(.libPaths(),"/bioCancer/base/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  }
  return(GeneList)

}
