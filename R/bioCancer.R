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
#' @importFrom dplyr add_rownames combine collect combine collapse collect full_join
#' @importFrom dplyr inner_join left_join mutate n_distinct rename right_join
#' @importFrom dplyr select select_ slice slice_
#' @importFrom plyr ldply ddply adply arrange desc
#' @import ggplot2
#' @import DiagrammeR
#' @importFrom knitr knit2html
#' @importFrom pryr where
#' @importFrom magrittr %<>% %T>% %$% set_rownames set_colnames set_names divide_by add extract2
#' @importFrom lubridate is.Date is.POSIXt now year week hour minute second ymd mdy dmy ymd_hms hms hm as.duration parse_date_time
#' @importFrom broom tidy glance
#' @importFrom tidyr gather_ gather separate
#' @importFrom gridExtra arrangeGrob
#' @importFrom markdown markdownToHTML
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom readr read_delim write_csv
#' @import shiny
#' @importFrom DT datatable styleColorBar
#' @importFrom DT styleInterval JS formatStyle formatPercentage
#' @import ggdendro
#' @import data.tree
#' @importFrom stringr str_match
#' @importFrom psych KMO corr.test cortest.bartlett fa.sort principal skew
#' @importFrom yaml yaml.load
#' @importFrom AlgDesign optFederov
#' @importFrom MASS isoMDS
#' @importFrom wordcloud textplot
#' @importFrom car leveragePlots recode vif
#' @importFrom curl curl
#' @import GPArotation
#' @import scales
#' @import htmlwidgets
#' @importFrom htmlwidgets shinyRenderWidget
#' @import covr
#' @importFrom jsonlite fromJSON
#' @import stats
#' @importFrom utils capture.output read.table tail
#' @importFrom geNetClassifier calculateGenesRanking
#' @importFrom geNetClassifier genesDetails
#' @importFrom Biobase ExpressionSet
#' @importFrom Biobase exprs
#' @importFrom AnnotationFuncs translate
#' @importFrom org.Hs.eg.db org.Hs.egSYMBOL2EG
#' @import DOSE
#' @importFrom clusterProfiler enricher
#' @importFrom clusterProfiler plot
#' @importFrom clusterProfiler compareCluster
#' @importFrom clusterProfiler enrichGO
#' @importFrom clusterProfiler enrichKEGG
#' @import reactome.db
#' @import ReactomePA
#' @importFrom magrittr %>%
#' @import DiagrammeR
#' @importFrom DiagrammeR grViz
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR grVizOutput
#' @importFrom RCurl basicTextGatherer
#' @importFrom RCurl curlPerform
#' @importFrom XML xmlInternalTreeParse
#' @importFrom visNetwork renderVisNetwork visNetwork visNodes visEdges visOptions
#' @importFrom visNetwork visHierarchicalLayout visExport visLegend visPhysics visNetworkOutput visExport
#' @import radiant.data
#' @export

bioCancer <- function(){
  if ("package:bioCancer" %in% search()){
    shiny::runApp(system.file( "bioCancer", package = "bioCancer"), launch.browser = TRUE)
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
