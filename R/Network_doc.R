#' get Edges dataframe for visNetwork function
#' @usage Edges_df()
#'
#' @return A data frame with egdes attributes
#' @export
#'
#' @examples
#' example <- "run manually"
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' Edges_df()
#' }
#'
#'
#' @importFrom RCurl basicTextGatherer
#' @importFrom XML xmlInternalTreeParse
#' @importFrom visNetwork renderVisNetwork visNetwork visNodes visEdges visOptions
#' @importFrom visNetwork visHierarchicalLayout visExport visLegend visPhysics visNetworkOutput visExport
#'
#'
Edges_df <- function(){

  if(is.null(ReactomeFI)){
    #shiny::withProgress(message = 'Loading ReactomeFI...', value = 0.1, {
      Sys.sleep(0.25)

      if("package:bioCancer" %in% search()) {
        r_data[['ReactomeFI']]  <- read.delim(paste0(system.file(package = "bioCancer"), "/extdata/FIsInGene_121514_with_annotations.txt", sep=""))
      }else{
        r_data[['ReactomeFI']]  <- read.delim(file.path(paste(r_path,"/extdata/FIsInGene_121514_with_annotations.txt", sep="")))
      }

   # })
  }

  #GeneList <- c("DKK3", "NBN", "MYO6", "TP53","PML", "IFI16", "BRCA1")
  GeneList <- whichGeneList()

  ## Edges Attributes
  #shiny::withProgress(message = 'load FI for GeneList...', value = 0.1, {
    Sys.sleep(0.25)
    fis <- getReactomeFI(2014,genes=GeneList, use.linkers = FALSE) # input$UseLinkerNetId
#})
  #shiny::withProgress(message = 'load gene relationships...', value = 0.1, {
    Sys.sleep(0.25)

    names(fis) <- c("Gene1", "Gene2")
    Edges_obj1 <- merge(ReactomeFI, fis, by=c("Gene1","Gene2"))
    names(fis) <- c("Gene2", "Gene1")
    Edges_obj2 <- merge(ReactomeFI, fis, by=c("Gene1","Gene2"))
    Edges_obj <- rbind(Edges_obj1,Edges_obj2)

    #     > head(Edges_obj)
    #     from to                                    Annotation Direction Score
    #     1  EGR1  TP53 expression regulated by; expression regulates       <->  1.00
    #     2  EGR1   UBB                          expression regulates        ->  1.00
    #     3  MYO6   UBB                                     predicted         -  0.61
    #     4   PML  TP53       complex; expression regulated by; input        <-  1.00
    #     5  TP53   UBB                  catalyze; complex; predicted        ->  1.00
    #     6 BRCA1  EGR1                       expression regulated by        <-  1.00

    ## Filter Annotation interaction
    #  Edges_obj <- Edges_obj[- grep(c("predic",activat), Edges_obj$Annotation),]
    #  Edges_obj <- Edges_obj[!grepl("predict|activat|binding|complex|indirect",Edges_obj$Annotation),]

    Edges_obj <- Edges_obj[grepl(paste0(c("activat","predict"), collapse="|"),Edges_obj$Annotation),] #input$FIs_AttNetworkId

    ## skip infinity loop when load Reactome_Genelist
    if(is.null(r_data$Reactome_GeneList)){
      r_data[['Reactome_GeneList']] <- union(Edges_obj$Gene1, Edges_obj$Gene2)
    }else if (all(r_data$length(Reactome_GeneList) == length(union(Edges_obj$Gene1, Edges_obj$Gene2)))
              && all(r_data$Reactome_GeneList == union(Edges_obj$Gene1, Edges_obj$Gene2))
    ){

    }else{
      r_data[['Reactome_GeneList']] <- union(Edges_obj$Gene1, Edges_obj$Gene2)
    }
    ## Get interaction Frequency in dataframe FreqIn

    FreqIn <- rbind(t(t(table(as.character(Edges_obj$Gene2)))), t(t(table(as.character(Edges_obj$Gene1)))))
    colnames(FreqIn) <- "Freq"
    FreqIn <- as.data.frame(FreqIn) %>% add_rownames("Genes")
    r_data[['FreqIn']] <- plyr::ddply(FreqIn,~Genes,summarise,FreqSum=sum(Freq))


    rownames(Edges_obj) <- NULL

    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<\\->","from;to", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("\\|\\->","from;to", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<\\-\\|","to;from", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("->","to", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<-","from", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("\\|\\-","from", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("\\-\\|","to", x)))
    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("-","none", x)))

    colnames(Edges_obj)<- c("from", "to","title","arrows" ,"width")

    #Edges_obj <- Edges_obj[1:150,]
  #})
  ## add 3 column used with geneset enrichment
  Edges_obj <- cbind(Edges_obj,
                     dashes=FALSE,
                     smooth=FALSE,
                     shadow=FALSE
                     #length= NULL
  )
  return(Edges_obj)
}


#' get Edges dataframe for Gene/Disease association from geNetClassifier
#'
#' @return A data frame with egdes attributes
#' @export
#'
#' @usage Edges_Diseases_obj(genesclassdetails)
#' @param genesclassdetails  a dataframe from geNetClassifier
#'
#' @examples
#' example <- "run Manually"
#' \dontrun{
#' load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' Ed_Diseases_obj <- Edges_Diseases_obj(genesclassdetails=GenesClassDetails)
#' }
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

  if(gene %in% GeneList){
    paste0("circle")
  }else{
    paste0("box")
  }

}

#' Data frame with Nodes id
#'
#' @return A data frame with nodes id
#' @export
#'
#' @usage Node_df(genelist, freqIn)
#' @param genelist a vector of genes
#' @param freqIn dataframe with Node interaction frequencies
#'
#' @examples
#'
#' example <- "run Manually"
#' \dontrun{
#' load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' node_df <- Node_df(GeneList, FreqIn )
#'}
#'
Node_df <- function(genelist, freqIn){
  #FreqIn <- r_data$FreqIn
  FreqIn[["shape"]]<- unname(sapply(FreqIn$Genes,  function(x) attriShape2Node(x, GeneList)))
  FreqIn$FreqSum  <- FreqIn$FreqSum / 10
  names(FreqIn) <- c("id","FreqSum","shape")
  FreqIn[["label"]] <- FreqIn$id
  GeneFreq <- FreqIn[,c(1,3,4)]
  #GeneFreq <- data.frame(lapply(GeneFreq[,names(GeneFreq)] , as.factor))

  #if(input$NodeAttri_ClassifierID == 'mRNA'|| input$NodeAttri_ClassifierID == 'mRNA/Studies'){
    colorsVector <- sapply(GenesClassDetails$exprsMeanDiff, function(x) as.character(attriColorVector(x,GenesClassDetails$exprsMeanDiff ,colors=c("blue","white","red"), feet=1)))
    colors_df <- data.frame(id = GenesClassDetails$Genes,color = colorsVector)
    colors_df <- data.frame(lapply(colors_df, as.character), stringsAsFactors=FALSE)
    merge1 <- dplyr::inner_join(GeneFreq, colors_df, by="id")
    merge1 <- data.frame(merge1, value= "1")
    diff1 <- dplyr::anti_join(GeneFreq, colors_df, by="id")
    diff1 <- data.frame (diff1, color= "lightgrey", value= "1")
    GeneFreq <- rbind(merge1, diff1)
 # }else{
  #  GeneFreq <- cbind(GeneFreq,color="lightgrey", value="1")
  #}
  return(GeneFreq)
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
#' example <- "run Manually"
#' \dontrun{
#' load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' GeneFreq <- Node_df_FreqIn(GeneList, FreqIn)
#'}
Node_df_FreqIn <- function(genelist, freqIn){

  #FreqIn <- r_data$FreqIn
  #FreqIn[["shape"]]<- unname(sapply(FreqIn$Genes,  function(x) attriShape2Node(x, GeneList)))
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
#' example <- "run manually"
#' \dontrun{
#' load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' Node_Diseases_df <- Node_Diseases_obj(genesclassdetails= GenesClassDetails)
#'}
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


#' Gene Set enrichment using Reactome FI methods
#'
#' @return A data frame with Gene Set enrichment: BP,MF, CC, Pathway, pval, FDR
#' @export
#' @usage getAnnoGeneSet_df(genelist, type)
#' @param genelist vector of genes
#' @param type "BP", "pathway","CC", "MF"
#'
#' @examples
#' example <- "run manually"
#' \dontrun{
#' load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' getAnnoGeneSet_df(GeneList, "BP")
#'}
getAnnoGeneSet_df <- function(genelist,type){
  # type = c("Pathway", "BP", "CC", "MF")
  # type <- input$TypeGeneSetID
  #type <- match.arg(type)

  ## Query GeneSet Annotation
  AnnoGeneSet <- queryAnnotateGeneSet(2014, t(genelist) ,type)

  ## Filter significant annotation using FDR
  AnnoGeneSet <- AnnoGeneSet[AnnoGeneSet$fdr < 0.95,]  #input$GeneSetFDRID

  r_data[['AnnoGeneSet']] <- AnnoGeneSet

  if(nrow(AnnoGeneSet)== 0){
    GeneSet_df <- data.frame(title = "")
  } else{

    #r_data[['MinGeneSetFDR']] <- min(AnnoGeneSet$fdr, na.rm = TRUE)

    ## Split hits to a list
    key0 <- strsplit(AnnoGeneSet$hits, ",")

    ## from Martin Morgan  http://stackoverflow.com/questions/12837462/how-to-subset-data-with-advance-string-matching

    Index_Gene <- data.frame(index = rep(seq_along(key0), sapply(key0, length)),ID = unlist(key0))

    ## Maybe useful add GeneList with index and genes
    #ref_GeneSet  <-  cbind(GeneSet = AnnoGeneSet_hits[subset[,1],2],subset)

    GeneSet_df <- data.frame(from = Index_Gene[,1],
                             to = Index_Gene[,2],
                             title= "GeneSet",
                             arrows= "to",
                             width = "0.3",
                             dashes = TRUE,
                             smooth = FALSE,
                             shadow = FALSE
                             #length = NULL
    )

  }
  return(GeneSet_df)
}
