#' Launch bioCancer with default browser
#'
#' @return  web page of bioCancer Shiny App
#' @usage bioCancer()
#' @description The Main function to run bioCancer App
#'
#' @examples
#' ShinyApp <-  1
#' \dontrun{
#' bioCancer()
#' }
#' @name bioCancer
#' @docType package
#' @import cBioPortalData
#' @import radiant.data
#' @importFrom utils capture.output read.table tail stack
#' @importFrom geNetClassifier calculateGenesRanking
#' @importFrom geNetClassifier genesDetails
#' @importFrom Biobase ExpressionSet
#' @importFrom Biobase exprs
#' @importFrom AnnotationDbi mget dbmeta dbconn
#' @importFrom org.Hs.eg.db org.Hs.egSYMBOL2EG
#' @import DOSE
#' @importFrom clusterProfiler enricher
#' @importFrom clusterProfiler compareCluster
#' @importFrom clusterProfiler enrichGO
#' @importFrom clusterProfiler enrichKEGG
#' @import reactome.db
#' @import ReactomePA
#' @import DiagrammeR
#' @importFrom DiagrammeR grViz
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR grVizOutput
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xmlValue
#' @importFrom visNetwork renderVisNetwork visNetwork visNodes visEdges visOptions
#' @importFrom visNetwork visHierarchicalLayout visExport visLegend visPhysics visNetworkOutput visExport
#' @importFrom dplyr select
#' @importFrom tidyr spread
#' @importFrom methods   new
#' @import AlgDesign GO.db R.methodsS3 import org.Bt.eg.db shinythemes
#' @export

bioCancer <- function(){
  if ("package:bioCancer" %in% search()){
    shiny::runApp(system.file( "app", package = "bioCancer"), launch.browser = TRUE)
    #shiny::runApp(system.file("app", package = "bioCancer"), launch.browser = TRUE)
  }else{
    stop("Install and load bioCancer package before to run it.")
  }
}

#' Default dataset of bioCancer
#'
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
"epiGenomics"

#' Example of Copy Number Alteration (CNA) dataset
#'
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
"user_CNA"

#' Example of Methylation HM27 dataset
#'
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
"user_MetHM27"

#' Example of Methylation HM450 dataset
#'
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
"user_MetHM450"

#' Example of Mutation dataset
#'
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
"user_Mut"

#' Example of mRNA expression dataset
#'
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
"user_mRNA"
