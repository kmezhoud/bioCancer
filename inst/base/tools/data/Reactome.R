
####### Attribute Color to Value in Vector
attriColorVector <- function(Value, vector, colors=c(a,b,c),feet){

  vector <- round(vector, digits = 0)
  Value <- round(Value, digits = 0)
  Max <- max(vector, na.rm=TRUE)
  Min <- min(vector, na.rm=TRUE)
  #   }
  my.colors <- colorRampPalette(colors)
  #generates Max-Min colors from the color ramp

  color.df <- data.frame(COLOR_VALUE=seq(Min,Max,feet), color.name=my.colors(length(seq(Min,Max,feet))))
  colorRef <- color.df[which(color.df[,1]==Value),2]
  #colorRef <- paste0(colorRef, collapse =",")
  return(colorRef)
}

graph_obj <- function(){
  #if(!'ReactomeFI' %in% r_data){
  if(!exists("ReactomeFI", r_data)){
    withProgress(message = 'Loading ReactomeFI...', value = 0.1, {
      Sys.sleep(0.25)

      #r_data[['ReactomeFI']] <- read.csv("https://raw.githubusercontent.com/kmezhoud/ReactomeFI/master/FIsInGene_121514_with_annotations.txt", header=TRUE, sep="\t")
      r_data[['ReactomeFI']]  <- read.delim(paste0(getwd(),"/FIsInGene_121514_with_annotations.txt", sep=""))
    })
  }

  #GeneList <- c("DKK3", "NBN", "MYO6", "TP53","PML", "IFI16", "BRCA1")
  if(input$GeneListID == "Genes"){
    GeneList <- r_data$Genes
  }else if(input$GeneListID == "Reactome_GeneList"){
    GeneList <- r_data$Reactome_GeneList
  }else{
    GeneList <- as.character(t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep="")))))
  }
  #GeneList <- c("SPRY2","FOXO1","FOXO3")
  ## Edges Attributes
  withProgress(message = 'load FI for GeneList...', value = 0.1, {
    Sys.sleep(0.25)
    fis <- getReactomeFI(2014,genes=GeneList, use.linkers = input$UseLinkerId)
  })
  withProgress(message = 'load gene relationships...', value = 0.1, {
    Sys.sleep(0.25)

    names(fis) <- c("Gene1", "Gene2")
    subset1 <- merge(r_data$ReactomeFI, fis, by=c("Gene1","Gene2"))
    names(fis) <- c("Gene2", "Gene1")
    subset2 <- merge(r_data$ReactomeFI, fis, by=c("Gene1","Gene2"))
    subset <- rbind(subset1,subset2)

    ## Filter Annotation interaction
    #  subset <- subset[- grep(c("predic",activat), subset$Annotation),]
    #  subset <- subset[!grepl("predict|activat|binding|complex|indirect",subset$Annotation),]
    subset <- subset[grepl(paste0(input$FIs_AttId, collapse="|"),subset$Annotation),]
    subsetbkp <<- subset
    r_data[['Reactome_GeneList']] <- union(subset$Gene1, subset$Gene2)

    ## Get interaction Frequency in dataframe FreqIn
    FreqIn <- rbind(t(t(table(as.character(subset$Gene2)))), t(t(table(as.character(subset$Gene1)))))
    colnames(FreqIn) <- "Freq"
    FreqIn <- as.data.frame(FreqIn) %>% add_rownames("Genes")
    r_data[['FreqIn']] <- ddply(FreqIn,~Genes,summarise,FreqSum=sum(Freq))

    rownames(subset) <- NULL

    subset <- as.data.frame(lapply(subset, function(x) gsub("<\\->","dir=both,arrowhead = tee,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("\\|\\->","dir=both,arrowtail = tee,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("<\\-\\|","dir=both,arrowhead = tee,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("->","dir = forward,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("<-","dir = back, arrowtail = normal,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("\\|\\-","dir= back, arrowtail = tee,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("\\-\\|","dir = forward, arrowhead = tee,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub("-","dir = none,", x)))
    ## Egdes relationships
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*complex.*","arrowhead=diamond,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*catalyze.*","arrowhead=curve,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*reaction.*","arrowhead=curve,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*phosphoryl.*","arrowhead=dot,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*activat.*","arrowhead=normal,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*inhibit.*","arrowhead=tee,", x)))
    # subset <- as.data.frame(lapply(subset, function(x) gsub(".*expression.*","arrowhead=normal,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*express.*","arrowhead=normal,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*regulat.*","arrowhead=normal,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*binding.*","dir = none,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*input.*","dir = none,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*dissociation.*"," arrowhead= inv", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*compound.*","dir = none,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*predicted.*","style = dashed,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*indirect.*","style = dashed,", x)))
    subset <- as.data.frame(lapply(subset, function(x) gsub(".*ubiquitinated.*","style = dashed,", x)))

    subset[,5] <-  paste("penwidth=", subset[,5],"]", sep=" ")
    #subset$int <- "->"
    subset[,1] <- paste(subset[,1], "->", sep=" ")
    subset[,2] <- paste(subset[,2],"[", sep=" ")
    subset$arrowsize <- "arrowsize=0.5,"
    #subset$croch2 <- "]"

    subset <- subset[c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")]
    #subset <- subset[1:150,]


    ## Verify radiobutton and Use Node attributes from geNetClassifier
    if(input$NodeAttriID == 'Freq. Interaction'){
      ## Nodes Attributes
      GeneAttri_df <- Node_obj_FreqIn(GeneList)
      #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
      subset<- rbind(subset, GeneAttri_df)
    }else if(input$NodeAttriID == 'Cancer/mRNA'){

      if (inherits(try(GeneAttri_df <- Node_obj_Classifier(GeneList, r_data$GenesClassDetails) , silent=FALSE),"try-error"))
      {
      msgNoClassifier <- paste("Gene Classes Details is not found, please run gene Classifier before...")
      #tkmessageBox(message=msgNoClassifier , icon="warning")
      stop(msgNoClassifier)
    } else{
      ## Nodes Attributes
      GeneAttri_df1 <- Node_obj_Classifier(GeneList, r_data$GenesClassDetails)
      GeneAttri_df2 <- Node_obj_FreqIn(GeneList)
      #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
      GeneAttri_df <- rbind(GeneAttri_df1, GeneAttri_df2)
      GeneAttri_bkp <<- GeneAttri_df
      subset <- rbind(subset, GeneAttri_df)
      subset_bkp <<- subset
    }

}

  ## convert Dataframe to graph object
  #cap <- capture.output(print(subset, row.names = FALSE)[-1])
  cap <- apply(subset, 1, function(x) paste(x, sep="\t", collapse=" "))
  ca <- paste(cap,"", collapse=";")
  obj <- paste0("\n","digraph{","\n", ca, "\n","}", sep="")
  return(obj)
})
  }




### Attributes for Nodes
attriShape2Gene <- function(gene, genelist){

  if(gene %in% genelist){
    paste0(gene, "[shape = 'circle',", sep=" ")
  }else{
    paste0(gene, "[shape = 'box',", sep=" ")
  }

}


Node_obj_FreqIn <- function(GeneList){

  FreqIn <- r_data$FreqIn
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

Node_obj_Classifier <- function(GeneList,GenesClassDetails= r_data$GenesClassDetails){

  GenesClassDetails <- merge(GenesClassDetails, r_data$FreqIn, by="Genes")
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

  GenesClassDetails$ranking <- paste("peripheries=",GenesClassDetails$ranking,"," ,sep=" ")
  print("ok peripheries=")
  V <- as.numeric(GenesClassDetails$class)
  C <- sample(colors(),length(table(GenesClassDetails$class)))
  GenesClassDetails$class <- paste("penwidth=3,color =", C[V],"," ,sep=" ")

  GenesClassDetails$postProb <- "style = filled, fillcolor ='"

  GenesClassDetails$exprsMeanDiff <- sapply(GenesClassDetails$exprsMeanDiff, function(x) as.character(attriColorVector(x,GenesClassDetails$exprsMeanDiff ,colors=c("blue","white","red"), feet=1)))

  GenesClassDetails$FreqSum <- paste0("',fixedsize = TRUE, width =",GenesClassDetails$FreqSum,", alpha_fillcolor =",GenesClassDetails$FreqSum,"]")

  # rename column to rbind with edge dataframe
  names(GenesClassDetails) <- c("Gene1", "Gene2","Direction","Annotation","arrowsize" ,"Score")
  GenesClassDetails_bkp <<- GenesClassDetails
  GeneAttri <- GenesClassDetails
  return(GeneAttri)
}




output$diagrammeR <- renderGrViz({
  grViz(
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
    graph_obj(),
    engine =  input$ReacLayoutId,   #dot, neato|twopi|circo|
    width = 1200
  )
})


