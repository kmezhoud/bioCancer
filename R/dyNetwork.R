#' get Edges dataframe for Gene/Disease association from geNetClassifier
#'
#' @return A data frame with egdes attributes
#' @export
#'
#' @usage Edges_Diseases_obj(genesclassdetails)
#' @param genesclassdetails  a dataframe from geNetClassifier
#'
#' @examples
#' GenesClassDetails <- structure(list(Genes = c("FANCF", "MLH1", "MSH2", "ATR", "PARP1",
#' "CHEK2", "RAD51"), ranking = c(1L, 1L, 1L, 2L, 3L, 1L, 2L), class = c("brca_tcga",
#' "gbm_tcga", "lihc_tcga", "lihc_tcga", "lihc_tcga", "lusc_tcga",
#' "lusc_tcga"), postProb = c(1, 0.99, 1, 0.99, 0.99, 1,
#' 0.98), exprsMeanDiff = c(180, 256, -373, -268,
#' -1482, 258, 143), exprsUpDw = c("UP", "UP", "DOWN",
#' "DOWN", "DOWN", "UP", "UP")), .Names = c("Genes", "ranking",
#' "class", "postProb", "exprsMeanDiff", "exprsUpDw"),
#' class = "data.frame", row.names = c(NA,-7L))
#'
#' Ed_Diseases_obj <- Edges_Diseases_obj(genesclassdetails=GenesClassDetails)
#'
Edges_Diseases_obj <- function(genesclassdetails){

  Ed_Diseases_obj <- data.frame(from = genesclassdetails$class,
                                to= genesclassdetails$Genes,
                                title="GDA",
                                arrows="to",
                                width= 0.3,
                                dashes=TRUE,
                                smooth=FALSE,
                                shadow=FALSE
                                #length= NULL
  )
  return(Ed_Diseases_obj)
}


#' Attributes shape to Nodes
#'
#' @return A data frame with egdes attributes
#' @export
#' @usage attriShape2Node(gene, genelist)
#' @param gene symbol "TP53"
#' @param genelist a vector of gene symbol
#'
#' @examples
#' GeneList <- c("DKK3" , "NBN"  , "MYO6" , "TP53" , "PML"  , "IFI16" ,"BRCA1")
#' NodeShape <- attriShape2Gene("DKK3", GeneList)
#'
attriShape2Node <- function(gene, genelist){

  if(gene %in% genelist){
    paste0("circle")
  }else{
    paste0("box")
  }

}

#' Attributes size to Nodes depending on number of interaction
#'
#' @return A data frame with nodes size attributes
#' @export
#'
#' @usage Node_df_FreqIn(genelist, freqIn)
#' @param genelist a vector of genes
#' @param freqIn dataframe with Node interaction frequencies
#'
#' @examples
#' Node_df_FreqIn
#' \dontrun{
#' r_data <- new.env()
#' r_data[["FreqIn"]] <- structure(list(Genes = c("ATM", "ATR", "BRCA1", "BRCA2", "CHEK1",
#' "CHEK2", "FANCF", "MDC1", "RAD51"), FreqSum = c(0.04, 0.05, 0.05,
#'  0.03, 0.05, 0.04, 0.03, 0.03, 0.02)), .Names = c("Genes", "FreqSum"),
#'  class = "data.frame", row.names = c(NA, -9L))
#' GeneList <- whichGeneList("DNA_damage_Response")
#' node_df <- Node_df_FreqIn(GeneList, r_data$FreqIn)
#'}
Node_df_FreqIn <- function(genelist, freqIn){

  FreqIn <- freqIn
  #FreqIn[["shape"]]<- unname(sapply(FreqIn$Genes,  function(x) attriShape2Node(x, genelist)))
  FreqIn$FreqSum  <- freqIn$FreqSum/10
  #names(FreqIn) <- c("id", "value","shape")
  names(FreqIn) <- c("id", "value")
  #FreqIn[["label"]] <- FreqIn$id
  GeneFreq <- FreqIn[,2]
  return(GeneFreq)

}

#' Attributes color and shape to Nodes of Diseases
#'
#' @return A data frame with nodes Shapes and colors
#' @export
#'
#' @usage Node_Diseases_obj(genesclassdetails)
#' @param genesclassdetails  a dataframe from geNetClassifier function
#'
#' @examples
#' GenesClassDetails <- structure(list(Genes = c("FANCF", "MLH1", "MSH2", "ATR", "PARP1",
#' "CHEK2", "RAD51"), ranking = c(1L, 1L, 1L, 2L, 3L, 1L, 2L), class = c("brca_tcga",
#' "gbm_tcga", "lihc_tcga", "lihc_tcga", "lihc_tcga", "lusc_tcga",
#' "lusc_tcga"), postProb = c(1, 0.99, 1, 0.99, 0.99, 1,
#' 0.98), exprsMeanDiff = c(180, 256, -373, -268,
#' -1482, 258, 143), exprsUpDw = c("UP", "UP", "DOWN",
#' "DOWN", "DOWN", "UP", "UP")), .Names = c("Genes", "ranking",
#' "class", "postProb", "exprsMeanDiff", "exprsUpDw"),
#' class = "data.frame", row.names = c(NA,-7L))
#' Node_Diseases_df <- Node_Diseases_obj(genesclassdetails= GenesClassDetails)
#'
Node_Diseases_obj <- function(genesclassdetails){
  if(is.null(genesclassdetails)){
    msgNoClassifier <- paste("Gene Classes Details is not found, please run gene Classifier before...")
    stop(msgNoClassifier)
  }else{
    V <- as.numeric(factor(genesclassdetails[,3]))
    set.seed(17)
    C <- sample(colors(),length(unique(genesclassdetails[,3])))
    C <- gsub('[0-9]+', '', C)
    Node_Diseases_df <- data.frame(id= unique(genesclassdetails[,3]),
                                   shape= "ellipse",
                                   label=unique(genesclassdetails[,3]),
                                   color=unique(C[V]),
                                   value="0.5"
    )
    return(Node_Diseases_df)

  }
}
