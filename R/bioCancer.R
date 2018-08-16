#' Launch bioCancer with default browser
#'
#' @return  web page of bioCancer Shiny App
#' @usage bioCancer()
#'
#' @examples
#' ShinyApp <-  1
#' \dontrun{
#' bioCancer()
#' }
#'
#' @name bioCancer
#' @docType package
#' @import radiant.data
#' @importFrom utils capture.output read.table tail
#' @importFrom geNetClassifier calculateGenesRanking
#' @importFrom geNetClassifier genesDetails
#' @importFrom Biobase ExpressionSet
#' @importFrom Biobase exprs
#' @importFrom AnnotationFuncs translate
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
