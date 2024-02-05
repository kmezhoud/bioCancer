#' Attribute color to a vector of numeric values
#'
#' @usage attriColorVector(Value, vector, colors=c(a,b,c),feet)
#'
#' @param Value numeric
#' @param vector A vector of numeric data
#' @param colors  3 colors
#' @param feet An interval between two numeric value needed to change the color
#'
#' @return A vetor of colors
#'
#' @examples
#' cgds <- cBioPortal(
#' hostname = "www.cbioportal.org",
#' protocol = "https",
#' api = "/api/v2/api-docs"
#' )
#' \dontrun{
#' getDataByGenes( api =  cgds,
#' studyId = "gbm_tcga_pub",
#' genes = c("NF1", "TP53", "ABL1"),
#' by = "hugoGeneSymbol",
#' molecularProfileIds = "gbm_tcga_pub_mrna"
#' )
#'}
#' @export
#' @importFrom grDevices colorRampPalette colors
#'
attriColorVector <- function(Value, vector, colors=c(a,b,c),feet){

  vector <- round(vector, digits = 0)
  Value <- round(Value, digits = 0)
  Max <- max(vector, na.rm=TRUE)
  Min <- min(vector, na.rm=TRUE)
  #   }
  my.colors <- grDevices::colorRampPalette(colors)
  #generates Max-Min colors from the color ramp

  color.df <- data.frame(COLOR_VALUE=seq(Min,Max,feet), color.name=my.colors(length(seq(Min,Max,feet))))
  colorRef <- color.df[which(color.df[,1]==Value),2]
  #colorRef <- paste0(colorRef, collapse =",")
  return(colorRef)
}


### Add Studies to Network

# Genes ranking     class postProb exprsMeanDiff exprsUpDw
# 1 FANCF       1 brca_tcga  1.00000      179.9226        UP
# 2  MLH1       1  gbm_tcga  0.99703      256.3173        UP

#' get object for grViz. Link Studies to genes
#' @usage Studies_obj(df)
#' @param df data frame with gene classes
#'
#' @return grViz object. a data frame with Study attributes
#' @export
#'
#' @examples
#' Studies_obj(data.frame("col1", "col2", "col3", "col4", "col5", "col6"))
#' \dontrun{
#' Genes ranking     class postProb exprsMeanDiff exprsUpDw
#' 1 FANCF       1 brca_tcga  1.00000      179.9226        UP
#' 2  MLH1       1  gbm_tcga  0.99703      256.3173        UP
#' }
Studies_obj <- function(df= df){

  if(is.null(df)){
    msgNoClassifier <- paste("Gene Classes Details is not found, please run gene Classifier before...")
    stop(msgNoClassifier)
  }else{
    names(df) <- c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")

    df$Gene2 <- "->"
    df$Annotation <- "[arrowhead = None,"
    df$arrowsize <- "color= Gray,alpha=80,"
    df$Score <- "penwidth= 0.2]"

    V <- as.numeric(factor(df$Direction))
    set.seed(17)
    C <- sample(colors(),length(table(df$Direction)))
    dfbis <- data.frame("Gene1"=df$Direction,
                        "Gene2"="[shape=egg,",
                        "Direction" = "style = filled,",
                        "Annotation"= "fillcolor =",
                        "arrowsize"= C[V],
                        "Score"="]"
    )
    df <- rbind(df, dfbis)
    #GenesClassDetails$class <- paste("penwidth=3,color =", C[V],"," ,sep=" ")

    return(df)
  }
}

#' Atribute mutation frequency to nodes
#' @usage Mutation_obj(list,FreqMutThreshold, geneListLabel)
#'
#' @param list A list of data frame with mutation data. Each data frame to study
#' @param FreqMutThreshold threshold Rate of cases (patients) having mutation (0-1).
#' @param geneListLabel file name of geneList examples: "73"
#'
#' @return A dat frame with mutation frequency. Ech column corresponds to a study.
#' @export
#'
#' @examples
#' cgds <- cBioPortal(
#' hostname = "www.cbioportal.org",
#' protocol = "https",
#' api = "/api/v2/api-docs"
#' )
#' \dontrun{
#' getDataByGenes( api =  cgds,
#' studyId = "gbm_tcga_pub",
#' genes = c("NF1", "TP53", "ABL1"),
#' by = "hugoGeneSymbol",
#' molecularProfileIds = "gbm_tcga_pub_mrna"
#' )
#'}
#'
Mutation_obj <- function(list, FreqMutThreshold, geneListLabel){

  df <- getFreqMutData(list = list, geneListLabel)

  if(is.null(df)){
    msgNoFreqMut <- paste("Mutation frequency is not found, please run gene Circomics before...")
    stop(msgNoFreqMut)
  }else{

    df <- df[apply(df, 1, function(x) !all(is.na(x))),]
    c1 <- apply(df,1, function(x)max(x, na.rm=TRUE))
    c2 <- colnames(df)[apply(df,1, function(x)which.max(x))]
    c  <- cbind.data.frame(c2,round(c1, digits=2))

    c <- c %>% tibble::rownames_to_column("Genes")
    colnames(c) <- c("Genes", "Disease", "Percentage")

    V <- as.numeric(factor(c$Disease))
    set.seed(17)
    C <- sample(colors(),length(table(c$Disease)))

    Mut <- cbind.data.frame(c,arrowsize=C[V])
    #BRCA1[shape = box, style= filled, fillcolor="#0007CD", color=red, penwidth=3, peripheries=2 ]
    #names(df) <- c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")
    Mut$Gene1 <- paste(Mut$Genes,"[", sep=" ")
    Mut$Gene2 <- "shape="
    Mut$Direction <- sapply(Mut$Percentage,function(x)if(x < FreqMutThreshold){
      paste("circle","," ,sep="")
    }else{
      paste("diamond",",",sep="")})
    Mut$Annotation <- "color="
    Mut$Score <- ",fontsize=10]"
    Mut <- Mut[c("Gene1","Gene2","Direction","Annotation","arrowsize","Score")]
    return(Mut)
  }
}

### Attributes for Nodes
#' Attribute shape to nodes
#'
#' @usage attriShape2Gene(gene, genelist)
#' @param gene Gene symbol
#' @param genelist Gene list
#'
#' @return A character "BRCA1[shape = 'circle', "
#' @export
#'
#' @examples
#' how <- "runManually"
#' \dontrun{
#' GeneList <- whichGeneList("73")
#' attriShape2Gene("P53", GeneList)
#' attriShape2Gene("GML",GeneList)
#'}
#'
attriShape2Gene <- function(gene, genelist){

  if(gene %in% genelist){
    paste0(gene, "[shape = 'circle',", sep=" ")
  }else{
    paste0(gene, "[shape = 'box',", sep=" ")
  }

}


#' Attribute interaction frequency to node size
#' @usage Node_obj_FreqIn(geneList)
#' @param geneList A list of gene symbol
#'
#' @return A data frame with node attributes
#' @export
#'
#' @examples
#' r_data <- new.env()
#' r_data[["FreqIn"]] <- structure(list(Genes = c("ATM", "ATR", "BRCA1", "BRCA2", "CHEK1",
#' "CHEK2", "FANCF", "MDC1", "RAD51"), FreqSum = c(0.04, 0.05, 0.05,
#'  0.03, 0.05, 0.04, 0.03, 0.03, 0.02)), .Names = c("Genes", "FreqSum"),
#'  class = "data.frame", row.names = c(NA, -9L))
#'  \dontrun{
#' GeneList <- whichGeneList("DNA_damage_Response")
#' nodeObj <- Node_obj_FreqIn(GeneList)
#'}
#'
Node_obj_FreqIn <- function(geneList){

  FreqIn <- r_data$FreqIn
  FreqIn$Genes<- unname(sapply(FreqIn$Genes,  function(x) attriShape2Gene(x, geneList)))
  FreqIn$FreqSum  <- FreqIn$FreqSum / 10
  FreqIn$FreqSum <- paste0("fixedsize = TRUE, width =",FreqIn$FreqSum,", alpha_fillcolor =",FreqIn$FreqSum,",")
  FreqIn <- cbind(FreqIn, Direction="peripheries=1,")
  FreqIn <- cbind(FreqIn, Annotation="style = filled,")
  FreqIn <- cbind(FreqIn, Arrowsize="alpha_fillcolor = 1,")
  FreqIn <- cbind(FreqIn, Score="fontsize=10]")
  names(FreqIn) <- c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")

  GeneAttri <- FreqIn
  return(GeneAttri)

}

#' Atrribute genes expression to color nodes
#'
#' @usage Node_obj_mRNA_Classifier(geneList,genesclassdetails)
#' @param geneList A gene list.
#' @param genesclassdetails A dataframe with genes classes and genes expression.
#'
#' @return A data frame with node color attributes
#' @export
#'
#' @examples
#' r_data <- new.env()
#' input <- NULL
#'
#' r_data[["FreqIn"]] <- structure(list(Genes = c("ATM", "ATR", "BRCA1", "BRCA2", "CHEK1",
#' "CHEK2", "FANCF", "MDC1", "RAD51"), FreqSum = c(0.04, 0.05, 0.05,
#'  0.03, 0.05, 0.04, 0.03, 0.03, 0.02)), .Names = c("Genes", "FreqSum"),
#'  class = "data.frame", row.names = c(NA, -9L))
#'
#' GenesClassDetails <- structure(list(Genes = c("FANCF", "MLH1", "MSH2", "ATR", "PARP1",
#' "CHEK2", "RAD51"), ranking = c(1L, 1L, 1L, 2L, 3L, 1L, 2L), class = c("brca_tcga",
#' "gbm_tcga", "lihc_tcga", "lihc_tcga", "lihc_tcga", "lusc_tcga",
#' "lusc_tcga"), postProb = c(1, 0.99, 1, 0.99, 0.99, 1,
#' 0.98), exprsMeanDiff = c(180, 256, -373, -268,
#' -1482, 258, 143), exprsUpDw = c("UP", "UP", "DOWN",
#' "DOWN", "DOWN", "UP", "UP")), .Names = c("Genes", "ranking",
#' "class", "postProb", "exprsMeanDiff", "exprsUpDw"),
#' class = "data.frame", row.names = c(NA,-7L))
#' \dontrun{
#' GeneList <- whichGeneList("DNA_damage_Response")
#' nodeObj <- Node_obj_mRNA_Classifier(GeneList, GenesClassDetails)
#'}
Node_obj_mRNA_Classifier <- function(geneList,genesclassdetails){
  if(is.null(genesclassdetails)){
    msgNoClassifier <- paste("Gene Classes Details is not found, please run gene Classifier before...")
    stop(msgNoClassifier)
  }else{
    GenesClassDetails <- merge(genesclassdetails, r_data$FreqIn, by="Genes")
    GenesClassDetails <- GenesClassDetails[,!(names(GenesClassDetails) %in% "exprsUpDw")]
    GenesClassDetails$FreqSum  <- GenesClassDetails$FreqSum / 10

    GenesClassDetails$Genes <- unname(sapply(GenesClassDetails$Genes,  function(x) attriShape2Gene(x, geneList)))

    ###GenesClassDetails$ranking <- paste("peripheries=",GenesClassDetails$ranking,"," ,sep=" ")
    GenesClassDetails$ranking <- paste("peripheries=","1","," ,sep=" ")
    V <- as.numeric(factor(GenesClassDetails$class))
    set.seed(17)
    C <- sample(colors(),length(table(GenesClassDetails$class)))

    if(is.null(input$NodeAttri_ProfDataID)){
      GenesClassDetails$class <- paste("penwidth=3,color =", C[V],"," ,sep=" ")
    }else{
      GenesClassDetails$class <- paste("penwidth=3,color =", "white","," ,sep=" ")
    }
    GenesClassDetails$postProb <- "style = filled, fillcolor ='"

    GenesClassDetails$exprsMeanDiff <- sapply(GenesClassDetails$exprsMeanDiff, function(x) as.character(attriColorVector(x,GenesClassDetails$exprsMeanDiff ,colors=c("blue","white","red"), feet=1)))

    GenesClassDetails$FreqSum <- paste0("',fixedsize = TRUE, width =",GenesClassDetails$FreqSum,", alpha_fillcolor =",GenesClassDetails$FreqSum,"]")

    # rename column to rbind with edge dataframe
    names(GenesClassDetails) <- c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")
    #GenesClassDetails_bkp <<- GenesClassDetails
    GeneAttri <- GenesClassDetails
    return(GeneAttri)
  }
}

#' Attribute CNA data to node border
#' @usage Node_obj_CNA_ProfData(list)
#' @param list A list of data frame with CNA data. Each data frame corresponds to a study.
#'
#' @return A data frame with node border attributes
#' @export
#'
#' @examples
#' cgds <- cBioPortal(
#' hostname = "www.cbioportal.org",
#' protocol = "https",
#' api = "/api/v2/api-docs"
#' )
#' \dontrun{
#' getDataByGenes( api =  cgds,
#' studyId = "gbm_tcga_pub",
#' genes = c("NF1", "TP53", "ABL1"),
#' by = "hugoGeneSymbol",
#' molecularProfileIds = "gbm_tcga_pub_mrna"
#' )
#'}
Node_obj_CNA_ProfData <- function(list){

  ListDf <-lapply(list, function(x) apply(x, 2, function(y) as.data.frame(table(y[order(y)]))))
  ListDf2 <-   lapply(ListDf, function(x) lapply(x, function(y) y[,1][which(y[,2]== max(y[,2]))]))
  ListDf3 <- plyr::ldply(ListDf2, data.frame)
  MostFreqCNA_Df <- plyr::ldply(apply(ListDf3,2,function(x) names(which(max(table(x))==table(x))))[-1],data.frame)

  #MostFreqCNA_Df$arrowsize <- paste(MostFreqCNA_Df[,1], MostFreqCNA_Df[,2], sep=":")
  MostFreqCNA_Df[,2] <- gsub("-1", "1, style=dashed", MostFreqCNA_Df[,2] )
  MostFreqCNA_Df[,2] <- gsub("-2", "2, style=dashed", MostFreqCNA_Df[,2] )
  MostFreqCNA_Df[,2] <- gsub("0", "0.5", MostFreqCNA_Df[,2] )
  MostFreqCNA_Df[,2] <- gsub("1", " 1, penwidth=2 ", MostFreqCNA_Df[,2] )
  MostFreqCNA_Df[,2] <- gsub("2", " 2 ", MostFreqCNA_Df[,2])
  MostFreqCNA_Df$arrowsize <- MostFreqCNA_Df[,2]
  MostFreqCNA_Df$Gene1 <- MostFreqCNA_Df$.id
  MostFreqCNA_Df$Gene2 <- "["
  MostFreqCNA_Df$Direction <- "pripheries"
  MostFreqCNA_Df$Annotation <- "="
  MostFreqCNA_Df$Score <- "]"
  MostFreqCNA_Df <- MostFreqCNA_Df[c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")]

  return(MostFreqCNA_Df)
}

#' Attribute gene Methylation to Nodes
#' @usage Node_obj_Met_ProfData(list, type, threshold)
#'
#' @param list a list of data frame with methylation data
#' @param type HM450 or HM27
#' @param threshold the Rate cases (patients) that have a silencing genes by methylation
#'
#' @return a data frame with node shape attributes
#' @export
#'
#' @examples
#' cgds <- cBioPortal(
#' hostname = "www.cbioportal.org",
#' protocol = "https",
#' api = "/api/v2/api-docs"
#' )
#' \dontrun{
#' getDataByGenes( api =  cgds,
#' studyId = "gbm_tcga_pub",
#' genes = c("NF1", "TP53", "ABL1"),
#' by = "hugoGeneSymbol",
#' molecularProfileIds = "gbm_tcga_pub_mrna"
#' )
#'}
#'
Node_obj_Met_ProfData <- function(list, type, threshold){
  #dfMeansOrCNA<-apply(df,2,function(x) mean(x, na.rm=TRUE))
  #dfMeansOrCNA <- round(dfMeansOrCNA, digits = 0)

  Met_Obj <- lapply(list, function(x) apply(x,2,function(y) mean(y, na.rm=TRUE)))
  Met_Obj <- lapply(Met_Obj, function(x) round(x, digits = 2))
  Met_Obj <- plyr::ldply(Met_Obj)[,-1]

  Met_Obj <- plyr::ldply(Met_Obj,function(x) (max(x, na.rm = TRUE)))

  if(type == "HM450"){
    Met_Obj <- subset(Met_Obj, V1 > threshold)
  } else if( type == "HM27"){
    Met_Obj <- subset(Met_Obj, V1 > threshold)
  }

  if(nrow(Met_Obj)== 0){

  }else{
    Met_Obj$Gene1 <- Met_Obj$.id
    Met_Obj$Gene2 <- "["
    Met_Obj$Direction <- "shape"
    Met_Obj$Annotation <- "="
    Met_Obj$arrowsize <- "invtriangle,"
    Met_Obj$Score <- "fixedsize=true]"
    Met_Obj <- Met_Obj[c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")]

    #lapply(ListProfData_bkp$Met_HM450, function(x) attriColorGene(x))
    return(Met_Obj)
  }
}

