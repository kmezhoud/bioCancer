#' @return  Verify which gene list is selected
#' @export
#'
#' @examples
#' library(bioCancer)
#' bioCancer()
#'
#'
whichGeneList <- function(){
  if(input$GeneListID == "Genes"){
    GeneList <- r_data$Genes
  }else if(input$GeneListID == "Reactome_GeneList"){
    GeneList <- t(r_data$Reactome_GeneList)
  }else{
    #GeneList <- t(unique(read.table(paste0(r_path,"/base/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
    GeneList <- t(unique(read.table(paste0(system.file(package = "bioCancer"),"/base/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  }
  return(GeneList)

}
