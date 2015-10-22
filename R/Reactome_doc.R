# output$ReactomeHowto <- renderPrint({
#   cat("1 - Select Gene list \n
#        2 - Set Edges attributes \n
#        3 - Set Nodes attributes from Classifier panel. \n
#            This step needs to classiy genes by studies (mRNA, Studies) \n
#        4 - Set Nodes attributes from Profiles data (CNA, Methylation, Mutation, miRNA, RPPA)
#       ")
# })

####### Attribute Color to Value in Vector
#' Attribute colorto a vector of numeric values
#'
#' @param Value numeric
#' @param vector A vector of numeric data
#' @param colors  3 colors
#' @param feet An interval between two numeric value needed to change the color
#'
#' @return A vetor of colors
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' sapply(GenesClassDetails$exprsMeanDiff, function(x) as.character(attriColorVector(x,GenesClassDetails$exprsMeanDiff ,colors=c("blue","white","red"), feet=1)))
#' }
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


#' get Edges object for grVis function
#'
#' @return A data frame with egdes attributes
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Ed_obj <- Edges_obj()
#' }
#'
#' @importFrom RCurl basicTextGatherer
#' @importFrom XML xmlInternalTreeParse
#'
#'
Edges_obj <- function(){

  #if(!'ReactomeFI' %in% r_data){
  if(is.null(ReactomeFI)){
    # withProgress(message = 'Loading ReactomeFI...', value = 0.1, {
    #  Sys.sleep(0.25)

    #r_data[['ReactomeFI']] <- read.csv("https://raw.githubusercontent.com/kmezhoud/ReactomeFI/master/FIsInGene_121514_with_annotations.txt", header=TRUE, sep="\t")
    #r_data[['ReactomeFI']]  <- read.delim("inst/extdata/FIsInGene_121514_with_annotations.txt")
    ReactomeFI  <- read.delim(paste0(system.file(package = "bioCancer"), "/extdata/FIsInGene_121514_with_annotations.txt", sep=""))

    #})
  }

  #GeneList <- c("DKK3", "NBN", "MYO6", "TP53","PML", "IFI16", "BRCA1")
  GeneList <- whichGeneList()
  #GeneList <- c("SPRY2","FOXO1","FOXO3")
  ## Edges Attributes
  # withProgress(message = 'load FI for GeneList...', value = 0.1, {
  # Sys.sleep(0.25)
  fis <- getReactomeFI(2014,genes=GeneList, use.linkers = FALSE)
  #})
  # withProgress(message = 'load gene relationships...', value = 0.1, {
  #  Sys.sleep(0.25)

  names(fis) <- c("Gene1", "Gene2")
  Edges_obj1 <- merge(ReactomeFI, fis, by=c("Gene1","Gene2"))
  names(fis) <- c("Gene2", "Gene1")
  Edges_obj2 <- merge(ReactomeFI, fis, by=c("Gene1","Gene2"))
  Edges_obj <- rbind(Edges_obj1,Edges_obj2)

  ## Filter Annotation interaction
  #  Edges_obj <- Edges_obj[- grep(c("predic",activat), Edges_obj$Annotation),]
  #  Edges_obj <- Edges_obj[!grepl("predict|activat|binding|complex|indirect",Edges_obj$Annotation),]
  Edges_obj <- Edges_obj[grepl(paste0("activat", collapse="|"),Edges_obj$Annotation),]

  Reactome_GeneList <- union(Edges_obj$Gene1, Edges_obj$Gene2)

  ## Get interaction Frequency in dataframe FreqIn
  FreqIn <- rbind(t(t(table(as.character(Edges_obj$Gene2)))), t(t(table(as.character(Edges_obj$Gene1)))))
  colnames(FreqIn) <- "Freq"
  FreqIn <- as.data.frame(FreqIn) %>% add_rownames("Genes")
  FreqIn <- plyr::ddply(FreqIn,~Genes,summarise,FreqSum=sum(Freq))

  rownames(Edges_obj) <- NULL

  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<\\->","dir=both,arrowhead = tee,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("\\|\\->","dir=both,arrowtail = tee,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<\\-\\|","dir=both,arrowhead = tee,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("->","dir = forward,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<-","dir = back, arrowtail = normal,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("\\|\\-","dir= back, arrowtail = tee,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("\\-\\|","dir = forward, arrowhead = tee,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("-","dir = none,", x)))
  ## Egdes relationships
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*complex.*","arrowhead=diamond,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*catalyze.*","arrowhead=curve,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*reaction.*","arrowhead=curve,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*phosphoryl.*","arrowhead=dot,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*activat.*","arrowhead=normal,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*inhibit.*","arrowhead=tee,", x)))
  # Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*expression.*","arrowhead=normal,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*express.*","arrowhead=normal,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*regulat.*","arrowhead=normal,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*binding.*","dir = none,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*input.*","dir = none,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*dissociation.*"," arrowhead= inv", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*compound.*","dir = none,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*predicted.*","style = dashed,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*indirect.*","style = dashed,", x)))
  Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub(".*ubiquitinated.*","style = dashed,", x)))

  Edges_obj[,5] <-  paste("penwidth=", Edges_obj[,5],"]", sep=" ")
  #Edges_obj$int <- "->"
  Edges_obj[,1] <- paste(Edges_obj[,1], "->", sep=" ")
  Edges_obj[,2] <- paste(Edges_obj[,2],"[", sep=" ")
  Edges_obj$arrowsize <- "arrowsize=0.5,"
  #Edges_obj$croch2 <- "]"

  Edges_obj<- Edges_obj[c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")]
  #Edges_obj <- Edges_obj[1:150,]
  #})
  # Edges_objbkp <<- Edges_obj
  return(Edges_obj)
}

### Add Studies to Network

# Genes ranking     class postProb exprsMeanDiff exprsUpDw
# 1 FANCF       1 brca_tcga  1.00000      179.9226        UP
# 2  MLH1       1  gbm_tcga  0.99703      256.3173        UP

#' get object for grViz. Link Studies to genes
#'
#' @param df data frame with gene classes
#'
#' @return grViz object. a data frame with Study attributes
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Sd_obj <- Studies_obj(GenesClassDetails)
#' }
Studies_obj <- function(df= df){
  #df <- GenesClassDetails_bkp
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

### Mutation Attribution
#' Atribute mutation frequency to nodes
#'
#' @param list A list of data frame with mutation data. Each data frame to study
#' @param threshold Rate of cases (patients) having mutation (0-1).
#'
#' @return A dat frame with mutation frequency. Ech column corresponds to a study.
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Mut_obj <- Mutation_obj(ListMutData_bkp, 0.2)
#' }
Mutation_obj <- function(list,threshold){

  df <- getFreqMutData(list = ListMutData_bkp)

  if(is.null(df)){
    msgNoFreqMut <- paste("Mutation frequency is not found, please run gene Circomics before...")
    stop(msgNoFreqMut)
  }else{

    df <- df[apply(df, 1, function(x) !all(is.na(x))),]
    c1 <- apply(df,1, function(x)max(x, na.rm=TRUE))
    c2 <- colnames(df)[apply(df,1, function(x)which.max(x))]
    c  <- cbind.data.frame(c2,round(c1, digits=2))

    c <- c %>% add_rownames("Genes")
    colnames(c) <- c("Genes", "Disease", "Percentage")

    V <- as.numeric(factor(c$Disease))
    set.seed(17)
    C <- sample(colors(),length(table(c$Disease)))

    Mut <- cbind.data.frame(c,arrowsize=C[V])
    #BRCA1[shape = box, style= filled, fillcolor="#0007CD", color=red, penwidth=3, peripheries=2 ]
    #names(df) <- c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")
    Mut$Gene1 <- paste(Mut$Genes,"[", sep=" ")
    Mut$Gene2 <- "shape="
    Mut$Direction <- sapply(Mut$Percentage,function(x)if(x < threshold){
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
#' @param gene Gene symbol
#' @param genelist Gene list
#'
#' @return A character "BRCA1[shape = 'circle', "
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Shape_object <- attriShape2Gene("BRCA1", GeneList)
#' }
attriShape2Gene <- function(gene, genelist){

  if(gene %in% genelist){
    paste0(gene, "[shape = 'circle',", sep=" ")
  }else{
    paste0(gene, "[shape = 'box',", sep=" ")
  }

}


#' Attribute interaction number to node size
#'
#' @param GeneList A list of gene symbol
#'
#' @return A data frame with node attributes
#' @export
#'
#' @examples
#' \dontrun{
#'load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Node_obj_FreqIn(GeneList)
#' }
#'
Node_obj_FreqIn <- function(GeneList){

  FreqIn$Genes<- unname(sapply(FreqIn$Genes,  function(x) attriShape2Gene(x, GeneList)))
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

#' Atrribute genes exprssion to color nodes
#'
#' @param GeneList A gene list.
#' @param GenesClassDetails A dataframe with genes classes and genes expression.
#'
#' @return A data frame with node color attributes
#' @export
#'
#' @examples
#' \dontrun{
#'load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Node_obj <- Node_obj_mRNA_Classifier(GeneList, GenesClassDetails)
#' }
Node_obj_mRNA_Classifier <- function(GeneList,GenesClassDetails= df){
  if(is.null(GenesClassDetails)){
    msgNoClassifier <- paste("Gene Classes Details is not found, please run gene Classifier before...")
    stop(msgNoClassifier)
  }else{
    GenesClassDetails <- merge(GenesClassDetails, FreqIn, by="Genes")
    GenesClassDetails <- GenesClassDetails[,!(names(GenesClassDetails) %in% "exprsUpDw")]
    GenesClassDetails$FreqSum  <- GenesClassDetails$FreqSum / 10

    ## geneDiseaseClass_obj
    # GenesClassDetails_ls <- lapply(r_data$GenesClassDetails, function(x) x %>% add_rownames("Genes")
    #GenesClassDetails_df <- ldply(GenesClassDetails_ls)
    #GenesClassDetails_df <- GenesClassDetails_df[,-1]
    #GenesClassDetails <- r_data$GenesClassDetails
    ## identify Gene List
    #   if(input$GeneListID == "Genes"){
    #     GeneList <- r_data$Genes
    #   }else if(input$GeneListID =="Reactome_GeneList"){
    #     GeneList <- r_data$Reactome_GeneList
    #     print(GeneList)
    #   }else{
    #     GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
    #   }

    GenesClassDetails$Genes <- unname(sapply(GenesClassDetails$Genes,  function(x) attriShape2Gene(x, GeneList)))

    ###GenesClassDetails$ranking <- paste("peripheries=",GenesClassDetails$ranking,"," ,sep=" ")
    GenesClassDetails$ranking <- paste("peripheries=","1","," ,sep=" ")

    V <- as.numeric(factor(GenesClassDetails$class))
    set.seed(17)
    C <- sample(colors(),length(table(GenesClassDetails$class)))

    # if(is.null(input$NodeAttri_ProfDataID)){
    GenesClassDetails$class <- paste("penwidth=3,color =", C[V],"," ,sep=" ")
    #     }else{
    #       GenesClassDetails$class <- paste("penwidth=3,color =", "white","," ,sep=" ")
    #     }
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
#'
#' @param list A list of data frame with CNA data. Each data frame corresponds to a study.
#'
#' @return A data frame with node border attributes
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' CNA_obj <- Node_obj_CNA_ProfData(ListCNAData_bkp)
#' }
Node_obj_CNA_ProfData <- function(list){

  ListDf <-lapply(list, function(x) apply(x, 2, function(y) as.data.frame(table(y[order(y)]))))
  ListDf2 <-   lapply(ListDf, function(x) lapply(x, function(y) y[,1][which(y[,2]== max(y[,2]))]))
  ListDf3 <- ldply(ListDf2, data.frame)
  MostFreqCNA_Df <- ldply(apply(ListDf3,2,function(x) names(which(max(table(x))==table(x))))[-1],data.frame)

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

#' Title
#'
#' @param list a list of data frame with methylation data
#' @param type HM450 or HM27
#' @param Threshold the Rate cases (patients) that have a silencing genes by methylation
#'
#' @return a data frame with node shape attributes
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' Met_obj <- Node_obj_Met_ProfData(ListMetData_bkp$HM450,type="HM450",0.8)
#' }
Node_obj_Met_ProfData <- function(list, type, Threshold){
  #dfMeansOrCNA<-apply(df,2,function(x) mean(x, na.rm=TRUE))
  #dfMeansOrCNA <- round(dfMeansOrCNA, digits = 0)

  Met_Obj <- lapply(list, function(x) apply(x,2,function(y) mean(y, na.rm=TRUE)))
  Met_Obj <- lapply(Met_Obj, function(x) round(x, digits = 2))
  Met_Obj <- ldply(Met_Obj)[,-1]

  Met_Obj <- ldply(Met_Obj,function(x) (max(x, na.rm = TRUE)))

  if(type == "HM450"){
    Met_Obj <- subset(Met_Obj, V1 > Threshold)
  } else if( type == "HM27"){
    Met_Obj <- subset(Met_Obj, V1 > Threshold)
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

#' get graph object for grViz
#'
#' @param NodeAttri_Reactome  Node attribute from Reactome database ('Freq. Interaction')
#' @param NodeAttri_Classifier Node attributes from geNetClassifier ('mRNA','Studies','mRNA/Studies')
#' @param NodeAttri_ProfData Node attributes from Profiles Data ('Mutation', 'CNA', 'Met_HM450', 'Met_HM27')
#'
#' @return Object graph for grViz with Nodes and Edges attributes
#' @export
#'
#' @examples
#' \dontrun{
#' load(paste(path.package("bioCancer"),"/data/ListProfData.RData", sep=""))
#' gr_obj <- graph_obj('Freq. Interaction', 'mRNA', 'Met_HM450')
#' }
#'
#' @importFrom RCurl basicTextGatherer
#'
graph_obj <- function(NodeAttri_Reactome,NodeAttri_Classifier,NodeAttri_ProfData){

  GeneList <- whichGeneList()

  Edges_obj <- Edges_obj( )

  if(NodeAttri_Reactome == 'Freq. Interaction'){
    ## Nodes Attributes
    GeneAttri_df <- Node_obj_FreqIn(GeneList)
    #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
    Edges_obj<- rbind(Edges_obj, GeneAttri_df)
  }
  if(NodeAttri_Classifier == 'mRNA'){
    ## Nodes Attributes
    GeneAttri_df1 <- Node_obj_mRNA_Classifier(GeneList, GenesClassDetails)
    #GeneAttri_df2 <- Node_obj_FreqIn(GeneList)
    #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
    #GeneAttri_df <- rbind(GeneAttri_df1, GeneAttri_df2)
    #GeneAttri_bkp <<- GeneAttri_df
    Edges_obj <- rbind(Edges_obj, GeneAttri_df1)
    #Edges_obj_bkp <<- Edges_obj

  }
  if (NodeAttri_Classifier =='Studies'){

    Disease_Net <- Studies_obj(df=GenesClassDetails)
    Edges_obj<- rbind(Edges_obj, Disease_Net)

  }

  if (NodeAttri_Classifier == 'mRNA/Studies'){

    ## Nodes Attributes
    GeneAttri_mRNA <- Node_obj_mRNA_Classifier(GeneList, GenesClassDetails)
    #GeneAttri_FreqIn <- Node_obj_FreqIn(GeneList)
    Studies_Net <- Studies_obj(df= GenesClassDetails)
    #FreqMut_obj <- Mutation_obj()

    #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
    GeneAttri_df <- rbind(GeneAttri_mRNA, Studies_Net)
    # GeneAttri_df <- rbind(GeneAttri_df,Studies_Net)
    #GeneAttri_df <- rbind(GeneAttri_df, FreqMut_obj)

    #GeneAttri_bkp <<- GeneAttri_df
    Edges_obj <- rbind(Edges_obj, GeneAttri_df)
    #Edges_obj_bkp <<- Edges_obj
  }

  if(NodeAttri_ProfData == 'Mutation'){

    FreqMut_obj <- Mutation_obj(ListMutData_bkp, threshold = 0.2)
    Edges_obj <- rbind(Edges_obj, FreqMut_obj)

  }
  if(NodeAttri_ProfData == 'CNA'){

    CNA_obj <- Node_obj_CNA_ProfData(list=ListProfData_bkp$CNA)
    Edges_obj <- rbind(Edges_obj, CNA_obj)

  }

  if(NodeAttri_ProfData== 'Met_HM450'){

    Met_obj <- Node_obj_Met_ProfData(list= ListProfData_bkp$Met_HM450, type ='HM450', Threshold = 0.8)
    Edges_obj <- rbind(Edges_obj, Met_obj)

  }

  if(NodeAttri_ProfData=='Met_HM27' ){

    Met_obj <- Node_obj_Met_ProfData(list= ListProfData_bkp$Met_HM27, type='HM27', Threshold = 0.8)
    Edges_obj <- rbind(Edges_obj, Met_obj)

  }


  ## convert Dataframe to graph object
  #cap <- capture.output(print(Edges_obj, row.names = FALSE)[-1])
  cap <- apply(Edges_obj, 1, function(x) paste(x, sep="\t", collapse=" "))
  ca <- paste(cap,"", collapse=";")
  obj <- paste0("\n","digraph{","\n", ca, "\n","}", sep="")

  return(obj)

}



#' Plot network with nodes and edges attributes
#'
#' @param NodeAttri_Reactome  Node attribute from Reactome database ('Freq. Interaction')
#' @param NodeAttri_Classifier Node attributes from geNetClassifier ('mRNA','Studies','mRNA/Studies')
#' @param NodeAttri_ProfData Node attributes from Profiles Data ('Mutation', 'CNA', 'Met_HM450', 'Met_HM27')
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' diagrammeR('Freq. Interaction', 'mRNA', 'Met_HM450')
#' }
#' @import DiagrammeR
#' @importFrom DiagrammeR grViz
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR grVizOutput
diagrammeR <- function(NodeAttri_Reactome,NodeAttri_Classifier,NodeAttri_ProfData){
  DiagrammeR::grViz(
    #   digraph{
    # ## Edge Atrributes
    #
    #     BRCA1  -> IFI16   [arrowhead = normal, color= LightGray, alpha=30, penwidth= 0.2] ;
    #     BRCA1  ->   NBN [arrowhead = normal] ;
    #     BRCA1  ->  TP53   [color= LightGray] ;
    #     IFI16  ->  TP53 [arrowtail = normal] ;
    #     PML  ->  TP53 [arrowtail = normal];
    #
    #
    # ## Node Attributes
    #
    #     BRCA1[shape = box, style= filled, fillcolor="#0007CD", color=red, penwidth=3, peripheries=2 ]
    #
    # },
    graph_obj(NodeAttri_Reactome,NodeAttri_Classifier,NodeAttri_ProfData),
    engine =  'neato',   #dot, neato|twopi|circo|
    width = 1200
  )
}


# ld_diagrammeR_plot<- function(){
#   grViz(
#     graph_obj(),
#     engine =  input$ReacLayoutId,   #dot, neato|twopi|circo|
#     width = 1200
#   )
# }
#
# output$ReactomeLegend <- renderImage({
#   # When input$n is 3, filename is ./images/image3.jpeg
#   filename <- paste(r_path,"/extdata/imgs/ReactomeLegend.png", sep="")
#
#   # Return a list containing the filename and alt text
#   list(src = filename,
#        contentType = 'image/png',
#        width = 500,
#        height = 300,
#        alt = paste("Image number"))
#
# }, deleteFile = FALSE)
