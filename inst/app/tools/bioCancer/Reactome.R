output$ReactomeHowto <- renderPrint({
  cat("
      1 - Select Gene list from `Portal/Profiles`
      2 - Set Edges attributes
      3 - Set GeneSet enrichment from ReactomeFI
      4 - Set Nodes attributes from Classifier panel
      This step needs to classiy genes by studies (mRNA, Studies)
      5 - Set Nodes attributes from Profiles data (CNA, Methylation, Mutation, miRNA, RPPA)
      6 - Run
      ")
})

#' get Edges object for grVis function
#'
#' @usage Edges_obj()
#'
#' @return A data frame with egdes attributes
#'
#' @examples
#'example <- "runManually"
#' \dontrun{
#'  string1 <- "https://wiki.ubuntu.com/kmezhoud/bioCancer?"
#'  string2 <- "action=AttachFile&do=get&target=ListProfData.RData"
#'  link <- curl::curl(paste0(string1,string2, sep=""))
#'  load(link)
#' ##load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' ReactomeFI <- readRDS(paste0(system.file(package = "bioCancer"),"/extdata/DisGeNet.RDS"))
#' Ed_obj <- Edges_obj()
#'}
#'
Edges_obj <- reactive({

  #if(!'ReactomeFI' %in% r_data){
  if(is.null(r_data$ReactomeFI)){
    shiny::withProgress(message = 'Loading ReactomeFI...', value = 1, {

      #r_data[['ReactomeFI']] <- read.csv("https://raw.githubusercontent.com/kmezhoud/ReactomeFI/master/FIsInGene_121514_with_annotations.txt", header=TRUE, sep="\t")
      #r_data[['ReactomeFI']]  <- read.delim("inst/extdata/FIsInGene_121514_with_annotations.txt")

      #download.file("http://reactomews.oicr.on.ca:8080/caBigR3WebApp2014/FIsInGene_121514_with_annotations.txt.zip", tmp <- tempfile())
      #xx = read.delim(unzip(tmp))

      if ("package:bioCancer" %in% search()) {
        r_data[['ReactomeFI']]  <- readRDS(paste0(system.file(package = "bioCancer"),
                                                  "/extdata/ReactomeFI2015.RDS", sep=""))
      }else{
        r_data[['ReactomeFI']]  <- readRDS(file.path(paste(getOption("radiant.path.bioCancer"),
                                                           "/extdata/ReactomeFI2015.RDS", sep="")))
      }

    })
  }

  #GeneList <- c("DKK3", "NBN", "MYO6", "TP53","PML", "IFI16", "BRCA1")
  GeneList <- whichGeneList(input$GeneListID)
  #GeneList <- c("SPRY2","FOXO1","FOXO3")
  ## Edges Attributes
  shiny::withProgress(message = 'load FI for GeneList...', value = 1, {

    fis <- getReactomeFI(2014,genes=GeneList, use.linkers = input$UseLinkerId)
  })
  shiny::withProgress(message = 'load gene relationships...', value = 1, {

    names(fis) <- c("Gene1", "Gene2")
    Edges_obj1 <- merge(r_data$ReactomeFI, fis, by=c("Gene1","Gene2"))
    names(fis) <- c("Gene2", "Gene1")
    Edges_obj2 <- merge(r_data$ReactomeFI, fis, by=c("Gene1","Gene2"))
    Edges_obj <- rbind(Edges_obj1,Edges_obj2)

    #     > head(Edges_obj)
    #     Gene1 Gene2                                    Annotation Direction Score
    #     1  EGR1  TP53 expression regulated by; expression regulates       <->  1.00
    #     2  EGR1   UBB                          expression regulates        ->  1.00
    #     3  MYO6   UBB                                     predicted         -  0.61
    #     4   PML  TP53       complex; expression regulated by; input        <-  1.00
    #     5  TP53   UBB                  catalyze; complex; predicted        ->  1.00
    #     6 BRCA1  EGR1                       expression regulated by        <-  1.00

    ## Filter Annotation interaction
    #  Edges_obj <- Edges_obj[- grep(c("predic",activat), Edges_obj$Annotation),]
    #  Edges_obj <- Edges_obj[!grepl("predict|activat|binding|complex|indirect",Edges_obj$Annotation),]
    Edges_obj <- Edges_obj[grepl(paste0(input$FIs_AttId, collapse="|"),Edges_obj$Annotation),]

    ## skip infinity loop when load Reactome_Genelist
    if(is.null(r_data$Reactome_GeneList)){
      r_data[['Reactome_GeneList']] <- union(Edges_obj$Gene1, Edges_obj$Gene2)
    }else if (all(length(r_data$Reactome_GeneList) == length(union(Edges_obj$Gene1, Edges_obj$Gene2)))
              && all(r_data$Reactome_GeneList == union(Edges_obj$Gene1, Edges_obj$Gene2))
    ){

    }else{
      r_data[['Reactome_GeneList']] <- union(Edges_obj$Gene1, Edges_obj$Gene2)
    }
    ## Get interaction Frequency in dataframe FreqIn

    FreqIn <- rbind(t(t(table(as.character(Edges_obj$Gene2)))), t(t(table(as.character(Edges_obj$Gene1)))))
    colnames(FreqIn) <- "Freq"
    #FreqIn <- as.data.frame(FreqIn) %>% tibble::rownames_to_column("Genes")
    rnames <- rownames(FreqIn)
    rownames(FreqIn) <- NULL
    FreqIn <- as.data.frame(FreqIn)
    FreqIn <- cbind("Genes"= rnames, FreqIn)

    r_data[['FreqIn']] <- plyr::ddply(FreqIn,~Genes,dplyr::summarise,FreqSum=sum(Freq))

    rownames(Edges_obj) <- NULL

    Edges_obj <- as.data.frame(lapply(Edges_obj, function(x) gsub("<\\->","dir=both,arrowhead = normal,", x)))
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
  })
  #Edges_objbkp <<- Edges_obj

  ## Attribute number of interaction to size of nodes
  GeneAttri_df <- Node_obj_FreqIn(GeneList)
  #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
  Edges_obj<- rbind(Edges_obj, GeneAttri_df)

  return(Edges_obj)
})


getAnnoGeneSet_obj <- function(genelist,type, fdr){
  # type = c("Pathway", "BP", "CC", "MF")
  # type <- input$TypeGeneSetID
  #type <- match.arg(type)
  if (inherits(try(
    AnnoGeneSet <- queryAnnotateGeneSet(2014, t(genelist) ,type),   #Legend_GeneSet <-
    silent=TRUE),"try-error") ){
    GeneSet_obj <- data.frame(Gene1 = "",
                              Gene2 = "",
                              Direction = "",
                              Annotation = "",
                              arrowsize = "",
                              Score = ""
    )
    r_data[['AnnoGeneSet']] <- as.data.frame("Select type of enrichment.")

    GeneSet_obj <- NULL
  }else{
    ## Query GeneSet Annotation
    AnnoGeneSet <- queryAnnotateGeneSet(2014, t(genelist) ,type)
    #AnnoGeneSet_bkp <<- AnnoGeneSet
    if(nrow(AnnoGeneSet)== 0){
      GeneSet_obj <- data.frame(Gene1 = "",
                                Gene2 = "",
                                Direction = "",
                                Annotation = "",
                                arrowsize = "",
                                Score = "")
      r_data[['AnnoGeneSet']] <- as.data.frame("No GeneSet found.")
      GeneSet_obj <- NULL
    }else{

      ## Filter significant annotation using FDR
      AnnoGeneSet <- AnnoGeneSet[AnnoGeneSet$fdr < fdr,]
      #AnnoGeneSetbkp <<- AnnoGeneSet

      if(nrow(AnnoGeneSet)== 0){
        GeneSet_obj <- data.frame(Gene1 = "",
                                  Gene2 = "",
                                  Direction = "",
                                  Annotation = "",
                                  arrowsize = "",
                                  Score = "")
        r_data[['AnnoGeneSet']] <- as.data.frame("No significant GeneSet found.")
        GeneSet_obj <- NULL
      }else{

        r_data[['AnnoGeneSet']] <- AnnoGeneSet

        #r_data[['MinGeneSetFDR']] <- min(AnnoGeneSet$fdr, na.rm = TRUE)

        ## Split hits to a list
        key0 <- strsplit(AnnoGeneSet$hits, ",")

        ## from Martin Morgan  http://stackoverflow.com/questions/12837462/how-to-subset-data-with-advance-string-matching

        Index_Gene <- data.frame(index = rep(seq_along(key0), sapply(key0, length)),ID = unlist(key0))
        #Index_Genebkp <<- Index_Gene
        ## Maybe useful add GeneList with index and genes
        #ref_GeneSet  <-  cbind(GeneSet = AnnoGeneSet_hits[subset[,1],2],subset)

        GeneSet_obj <- data.frame(Gene1 = paste(input$TypeGeneSetID,Index_Gene[,1], sep=""),
                                  Gene2 = "->",
                                  Direction = Index_Gene[,2],
                                  Annotation = "[arrowhead=none,",
                                  arrowsize = "color=Gray,",
                                  Score = "penwidth=0.2]"
        )

      }
    }
  }
  return(GeneSet_obj)
}

output$dl_GeneSet_Legend <- shiny::downloadHandler(
  filename = function() { paste0("GeneSet_Legend.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    get_data(r_data$GeneSet_Legend, vars = NULL, filt = data_filter,
            rows = NULL, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)

output$GeneSet_Legend <- DT::renderDataTable({

  if(nrow(r_data$AnnoGeneSet)==1){ # 1 corresponds to r_data[['AnnoGeneSet']] <- as.data.frame("No GeneSet found.")
    dat  <- as.data.frame('There is no significant enrichment found. Change FDR.')
    colnames(dat) <- 'There is no significant enrichment found. Change FDR.'
  }else{
    ## Attribute index to pathway
    if (inherits(try(
      Legend_GeneSet <- cbind(Node = paste(input$TypeGeneSetID,
                                           seq_len(nrow(r_data$AnnoGeneSet)),
                                           sep=""),
                              r_data$AnnoGeneSet[,names(r_data$AnnoGeneSet) != "hits"]),
      silent=TRUE),"try-error")){

      dat <- as.data.frame("Select one type of enrichment.")

    }else{

      Legend_GeneSet <- cbind(Node = paste(input$TypeGeneSetID,
                                           seq_len(nrow(r_data$AnnoGeneSet)),
                                           sep=""),
                              r_data$AnnoGeneSet[,names(r_data$AnnoGeneSet) != "hits"])


      #Legend_GeneSet_bkp <<- Legend_GeneSet
      Legend_GeneSet[,4:7] <- round(Legend_GeneSet[,4:7], digits=2)
      colnames(Legend_GeneSet)[c(3,4,6)] <- c("nhit","nGenes","pval")
      dat <- Legend_GeneSet[,c(1,2,3,4,6,7)]
    }
  }
  r_data[['GeneSet_Legend']] <- dat

  displayTable(dat)
})


#' get graph object for grViz
#' @usage graph_obj(NodeAttri_Reactome,NodeAttri_Classifier,NodeAttri_ProfData)
#'
#' @param NodeAttri_Reactome  Node attribute from Reactome database ('Freq. Interaction')
#' @param NodeAttri_Classifier Node attributes from geNetClassifier ('mRNA','Studies','mRNA/Studies')
#' @param NodeAttri_ProfData Node attributes from Profiles Data ('Mutation', 'CNA', 'Met_HM450', 'Met_HM27')
#'
#' @return Object graph for grViz with Nodes and Edges attributes
#'
#' @examples
#'example <- "runManually"
#' \dontrun{
#'  string1 <- "https://wiki.ubuntu.com/kmezhoud/bioCancer?"
#'  string2 <- "action=AttachFile&do=get&target=ListProfData.RData"
#'  link <- curl::curl(paste0(string1,string2, sep=""))
#'  load(link)
#' ##load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' ReactomeFI <- readRDS(paste0(system.file(package = "bioCancer"),"/extdata/DisGeNet.RDS"))
#' gr_obj <- graph_obj('Freq.Interaction', 'mRNA', 'Met_HM450')
#'}
#'
#' @importFrom RCurl basicTextGatherer
#'
graph_obj <- reactive({

  GeneList <- whichGeneList(input$GeneListID)

  Edges_obj <- Edges_obj()

  # if(input$NodeAttri_ReactomeID == 'Freq. Interaction'){
  #   ## Nodes Attributes
  #   GeneAttri_df <- Node_obj_FreqIn(GeneList)
  #   #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
  #   Edges_obj<- rbind(Edges_obj, GeneAttri_df)
  # }
  #if(input$NodeAttri_ReactomeID == 'FreqInt./GeneSet'){
  ## Nodes Attributes
  GeneFreqIn_df <- Node_obj_FreqIn(GeneList)
  if(input$TypeGeneSetID =="None"){

  }else if(input$TypeGeneSetID =="Pathway" ||
           input$TypeGeneSetID =="BP" ||
           input$TypeGeneSetID =="CC" ||
           input$TypeGeneSetID =="MF"
  ){
    GeneSetAnno_df <- getAnnoGeneSet_obj(GeneList,input$TypeGeneSetID,input$GeneSetFDRID)

    GeneAttri_df <- rbind(GeneFreqIn_df,GeneSetAnno_df)
    #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
    Edges_obj<- rbind(Edges_obj, GeneAttri_df)
  }

  # }

  #if(input$NodeAttri_ReactomeID == 'GeneSet'){

  # if(input$TypeGeneSetID =="None"){
  #
  # }else if(input$TypeGeneSetID =="Pathway" ||
  #          input$TypeGeneSetID =="BP" ||
  #          input$TypeGeneSetID =="CC" ||
  #          input$TypeGeneSetID =="MF"
  # ){
  #   GeneSetAnno_df <- getAnnoGeneSet_obj(GeneList,input$TypeGeneSetID,input$GeneSetFDRID) #input$TypeGeneSetID
  #   Edges_obj <- rbind(Edges_obj, GeneSetAnno_df)
  #   # Edges_obj_bkp <<- Edges_obj
  # }
  # #  }

  if(exists("r_data") && !is.null(r_data[['GenesClassDetails']])){
    if(input$NodeAttri_ClassifierID == 'mRNA'){

      ## Nodes Attributes
      GeneAttri_df1 <- Node_obj_mRNA_Classifier(GeneList, r_data$GenesClassDetails)
      #GeneAttri_df2 <- Node_obj_FreqIn(GeneList)
      #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
      #GeneAttri_df <- rbind(GeneAttri_df1, GeneAttri_df2)
      #GeneAttri_bkp <<- GeneAttri_df
      Edges_obj <- rbind(Edges_obj, GeneAttri_df1)
      #Edges_obj_bkp <<- Edges_obj
    }

    if (input$NodeAttri_ClassifierID =='Studies'){
      Disease_Net <- Studies_obj(df=r_data$GenesClassDetails)
      Edges_obj<- rbind(Edges_obj, Disease_Net)

    }

    if (input$NodeAttri_ClassifierID == 'mRNA/Studies'){

      ## Nodes Attributes
      GeneAttri_mRNA <- Node_obj_mRNA_Classifier(GeneList, r_data$GenesClassDetails)
      #GeneAttri_FreqIn <- Node_obj_FreqIn(GeneList)
      Studies_Net <- Studies_obj(df=r_data$GenesClassDetails)
      #FreqMut_obj <- Mutation_obj()

      #BRCA1[shape = box, style= filled, fillcolor="blue", color=red, penwidth=3, peripheries=2 ]
      GeneAttri_df <- rbind(GeneAttri_mRNA, Studies_Net)
      # GeneAttri_df <- rbind(GeneAttri_df,Studies_Net)
      #GeneAttri_df <- rbind(GeneAttri_df, FreqMut_obj)

      #GeneAttri_bkp <<- GeneAttri_df
      Edges_obj <- rbind(Edges_obj, GeneAttri_df)
      #Edges_obj_bkp <<- Edges_obj
    }
  }
  if('Mutation' %in% input$NodeAttri_ProfDataID ){

    FreqMut_obj <- Mutation_obj(list = r_data$ListMutData, FreqMutThreshold=input$FreqMutSliderID, geneListLabel = input$GeneListID)
    Edges_obj <- rbind(Edges_obj, FreqMut_obj)

  }
  if('CNA' %in% input$NodeAttri_ProfDataID){

    CNA_obj <- Node_obj_CNA_ProfData(list=r_data$ListProfData$CNA)
    Edges_obj <- rbind(Edges_obj, CNA_obj)

  }

  if('Met_HM450' %in% input$NodeAttri_ProfDataID){

    Met_obj <- Node_obj_Met_ProfData(list= r_data$ListProfData$Met_HM450, type ='HM450',input$ThresholdMetHM450ID )
    Edges_obj <- rbind(Edges_obj, Met_obj)

  }

  if('Met_HM27' %in% input$NodeAttri_ProfDataID ){

    Met_obj <- Node_obj_Met_ProfData(list= r_data$ListProfData$Met_HM27, type='HM27', input$ThresholdMetHM27ID)
    Edges_obj <- rbind(Edges_obj, Met_obj)

  }


  ## convert Dataframe to graph object
  #cap <- capture.output(print(Edges_obj, row.names = FALSE)[-1])
  cap <- apply(Edges_obj, 1, function(x) paste(x, sep="\t", collapse=" "))
  ca <- paste(cap,"", collapse=";")
  obj <- paste0("\n","digraph{",
                "graph [layout=", input$ReacLayoutId,"]",
                "\n", ca, "\n","}", sep="")

  return(obj)

})

#' Plot network with nodes and edges attributes
#'
#' @usage diagrammeR(NodeAttri_Reactome,NodeAttri_Classifier,NodeAttri_ProfData)
#' @param NodeAttri_Reactome  Node attribute from Reactome database ('Freq. Interaction')
#' @param NodeAttri_Classifier Node attributes from geNetClassifier ('mRNA','Studies','mRNA/Studies')
#' @param NodeAttri_ProfData Node attributes from Profiles Data ('Mutation', 'CNA', 'Met_HM450', 'Met_HM27')
#'
#' @return plot
#'
#' @examples
#'example <- "runManually"
#' \dontrun{
#'  string1 <- "https://wiki.ubuntu.com/kmezhoud/bioCancer?"
#'  string2 <- "action=AttachFile&do=get&target=ListProfData.RData"
#'  link <- curl::curl(paste0(string1,string2, sep=""))
#'  load(link)
#' ##load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' ReactomeFI <- readRDS(paste0(system.file(package = "bioCancer"),"/extdata/DisGeNet.RDS"))
#' diagrammeR('Freq.Interaction', 'mRNA', 'Met_HM450')
#'}
output$diagrammeR <- DiagrammeR::renderGrViz({

  DiagrammeR::grViz(
    #     digraph{
    ## Edge Atrributes

    #         BRCA1  -> IFI16   [arrowhead = normal, color= LightGray, alpha=30, penwidth= 0.2] ;
    #         BRCA1  ->   NBN [arrowhead = normal] ;
    #         BRCA1  ->  TP53   [color= LightGray] ;
    #         IFI16  ->  TP53 [arrowtail = normal] ;
    #         PML  ->  TP53 [arrowtail = normal];


    ## Node Attributes

    #        BRCA1[node_id="BRCA1_id" ,shape = box, style= filled, fillcolor="#0007CD", color=red, penwidth=3, peripheries=2 ]

    #    },
    graph_obj(),
    ## Engine argument do not work in the future (update viz.js)
    ## https://github.com/rich-iannone/DiagrammeR/issues/150
    #engine =  input$ReacLayoutId,   #dot, neato|twopi|circo|
    width = 600
  )

})


# ld_diagrammeR_plot<- function(){
#   DiagrammeR::grViz(
#     graph_obj(),
#     #engine =  input$ReacLayoutId,   #dot, neato|twopi|circo|
#     width = 1200
#   )
# }

output$Save_diagrammeR_plot <- downloadHandler(
  filename = function() {
    paste0("Reactomeplot.html", sep="")
  },
  content = function(file) {
    htmlwidgets::saveWidget(
      DiagrammeR::grViz(
        graph_obj(),
        #engine =  input$ReacLayoutId,   #dot, neato|twopi|circo|
        width = 600
      ), file)

    #webshot::webshot("temp.html", file = "Rplot.png",
    #       cliprect = "viewport")

  }

  # browseURL(paste('file://',getwd(),"Reactomeplot.html", sep="/"))
  ## wait 1 sd before delete
  # p1 <- proc.time()
  # Sys.sleep(1)
  # proc.time() - p1
  #unlink(paste(getwd(),"Reactomeplot.html", sep="/"))

)

output$ReactomeLegend <- renderImage({
  # When input$n is 3, filename is ./images/image3.jpeg
  filename <- paste(getOption("radiant.path.bioCancer"),"/extdata/imgs/ReactomeLegend.png", sep="")

  # Return a list containing the filename and alt text
  list(src = filename,
       contentType = 'image/png',
       width = 500,
       height = 300,
       alt = paste("Image number"))

}, deleteFile = FALSE)


## REPORT
observeEvent(input$ReactomeHelp_report, {

DiagrammeR_net <- paste0("## Static network \n
```{r}\n",
                   paste0(" DiagrammeR::grViz(
        graph_obj(),
        width = 600
                   )
  ") ,
                   "\n",
                   "\n```\n"
  )


visNetwork <- paste0("## Dynamic network \n
```{r}\n",
                       paste0("visNetwork::visHierarchicalLayout(r_data$graphe,
                              enabled= input$enableHierarchiId,     # TRUE, FALSE
                              direction = input$Hierarchi_AttId,    # 'LR', 'RL', 'UD', 'DU'
                              sortMethod= input$MethodHierarchiId   # 'directed' or 'hubsize'
                       )"),
                       "\n",
                       "\n```\n"
                       )


  update_report_fun(DiagrammeR_net)
  update_report_fun(visNetwork)
})
