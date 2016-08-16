#' get Edges dataframe for visNetwork function
#' @usage Edges_df()
#'
#' @return A data frame with egdes attributes
#'
#' @examples
#' example <- "run manually"
#' \dontrun{
#'  string1 <- "https://wiki.ubuntu.com/kmezhoud/bioCancer?"
#'  string2 <- "action=AttachFile&do=get&target=ListProfData.RData"
#'  link <- curl::curl(paste0(string1,string2, sep=""))
#'  load(link)
#' ##load(paste(path.package("bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' Edges_df()
#' }
#'
#'
Edges_df <- function(){

  if(is.null(r_data$ReactomeFI)){
    shiny::withProgress(message = 'Loading ReactomeFI...', value = 0.1, {
      Sys.sleep(0.25)

      if("package:bioCancer" %in% search()) {
        r_data[['ReactomeFI']]  <- readRDS(paste0(system.file(package = "bioCancer"), "/extdata/ReactomeFI2015.RDS", sep=""))
      }else{
        r_data[['ReactomeFI']]  <- readRDS(file.path(paste(r_path,"/extdata/ReactomeFI2015.RDS", sep="")))
      }

    })
  }

  #GeneList <- c("DKK3", "NBN", "MYO6", "TP53","PML", "IFI16", "BRCA1")
  GeneList <- whichGeneList(input$GeneListID)

  ## Edges Attributes
  shiny::withProgress(message = 'load FI for GeneList...', value = 0.1, {
    Sys.sleep(0.25)
    fis <- getReactomeFI(2014,genes=GeneList, use.linkers = input$UseLinkerId) # input$UseLinkerNetId
  })
  shiny::withProgress(message = 'load gene relationships...', value = 0.1, {
    Sys.sleep(0.25)

    names(fis) <- c("Gene1", "Gene2")
    Edges_obj1 <- merge(r_data$ReactomeFI, fis, by=c("Gene1","Gene2"))
    names(fis) <- c("Gene2", "Gene1")
    Edges_obj2 <- merge(r_data$ReactomeFI, fis, by=c("Gene1","Gene2"))
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

    Edges_obj <- Edges_obj[grepl(paste0(input$FIs_AttId, collapse="|"),Edges_obj$Annotation),] #input$FIs_AttNetworkId

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
    FreqIn <- as.data.frame(FreqIn) %>% dplyr::add_rownames("Genes")
    r_data[['FreqIn']] <- plyr::ddply(FreqIn,~Genes, dplyr::summarise,FreqSum=sum(Freq))


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
  })
  ## add 3 column used with geneset enrichment
  Edges_obj <- cbind(Edges_obj,
                     dashes=FALSE,
                     smooth=FALSE,
                     shadow=FALSE
                     #length= NULL
                     )
  return(Edges_obj)
}

#' Data frame with Nodes id
#'
#' @return A data frame with nodes id
#'
#' @usage Node_df(genelist, freqIn)
#' @param genelist a vector of genes
#' @param freqIn dataframe with Node interaction frequencies
#'
#' @examples
#'
#' example <- "run Manually"
#' \dontrun{
#'  string1 <- "https://wiki.ubuntu.com/kmezhoud/bioCancer?"
#'  string2 <- "action=AttachFile&do=get&target=ListProfData.RData"
#'  link <- curl::curl(paste0(string1,string2, sep=""))
#'  load(link)
#' node_df <- Node_df(GeneList, FreqIn )
#'}
#'
# FreqIn
# Genes FreqSum
# 1   ATM       4
# 2   ATR       5
# 3 BRCA1       5
# 4 BRCA2       3

Node_df <- function(genelist, freqIn){
  FreqIn <- freqIn
  FreqIn[["shape"]]<- unname(sapply(FreqIn$Genes,  function(x) attriShape2Node(x, genelist)))
  FreqIn$FreqSum  <- FreqIn$FreqSum / 10
  names(FreqIn) <- c("id","FreqSum","shape")
  FreqIn[["label"]] <- FreqIn$id
  GeneFreq <- FreqIn[,c(1,3,4)]
  #GeneFreq <- data.frame(lapply(GeneFreq[,names(GeneFreq)] , as.factor))

  if(input$NodeAttri_ClassifierID == 'mRNA'|| input$NodeAttri_ClassifierID == 'mRNA/Studies'){
  colorsVector <- sapply(r_data$GenesClassDetails$exprsMeanDiff, function(x) as.character(attriColorVector(x,r_data$GenesClassDetails$exprsMeanDiff ,colors=c("blue","white","red"), feet=1)))
  colors_df <- data.frame(id = r_data$GenesClassDetails$Genes,color = colorsVector)
  colors_df <- data.frame(lapply(colors_df, as.character), stringsAsFactors=FALSE)
  merge1 <- dplyr::inner_join(GeneFreq, colors_df, by="id")
  merge1 <- data.frame(merge1, value= "1")
  diff1 <- dplyr::anti_join(GeneFreq, colors_df, by="id")
  diff1 <- data.frame (diff1, color= "lightgrey", value= "1")
  GeneFreq <- rbind(merge1, diff1)
  }else{
  GeneFreq <- cbind(GeneFreq,color="lightgrey", value="1")
  }
  return(GeneFreq)
}

#' Gene Set enrichment using Reactome FI methods
#'
#' @return A data frame with Gene Set enrichment: BP,MF, CC, Pathway, pval, FDR
#' @usage getAnnoGeneSet_df(genelist, type)
#' @param genelist vector of genes
#' @param type "BP", "pathway","CC", "MF"
#'
#' @examples
#' GeneList <- whichGeneList("DNA_damage_Response")
#' geneSet_df <- getAnnoGeneSet_df(GeneList, "BP")
#'
getAnnoGeneSet_df <- function(genelist,type){
  # type = c("Pathway", "BP", "CC", "MF")
  # type <- input$TypeGeneSetID
  #type <- match.arg(type)

  ## Query GeneSet Annotation
  AnnoGeneSet <- queryAnnotateGeneSet(2014, t(genelist) ,type)

  ## Filter significant annotation using FDR
  AnnoGeneSet <- AnnoGeneSet[AnnoGeneSet$fdr < input$GeneSetFDRID,]  #input$GeneSetFDRID

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
                             title= input$TypeGeneSetID,
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


output$network <- visNetwork::renderVisNetwork({
  GeneList <- whichGeneList(input$GeneListID)

  edges <- Edges_df()
  nodes <-  Node_df(genelist = GeneList, freqIn = r_data$FreqIn)

  if(input$NodeAttri_ReactomeID == 'Freq. Interaction'){ #input$NodeAttri_NetworkID
    ## Nodes Frequncy interaction Attributes
    value <- Node_df_FreqIn(genelist = GeneList, freqIn = r_data$FreqIn)
    nodes[['value']] <- value
  }
  if(input$NodeAttri_ReactomeID == 'FreqInt./GeneSet'||input$NodeAttri_ReactomeID == 'GeneSet'){ #input$NodeAttri_NetworkID
    ## Nodes Attributes
    value <- Node_df_FreqIn(GeneList, freqIn = r_data$FreqIn)
     nodes[['value']] <- value

     #if(input$NodeAttri_ReactomeID == 'GeneSet'){
       if(input$TypeGeneSetID =="None"){

       }else if(input$TypeGeneSetID =="Pathway" ||
       input$TypeGeneSetID =="BP" ||
       input$TypeGeneSetID =="CC" ||
       input$TypeGeneSetID =="MF"
    ){
      GeneSetAnno_df <- getAnnoGeneSet_df(GeneList,type = input$TypeGeneSetID)  #input$TypeGeneSetID== BP, Pathway, CC, FN
      GeneSetAnno_df[,1] <- as.factor(GeneSetAnno_df[,1])

      edges<- rbind(edges, GeneSetAnno_df)

      nodesForGeneSet <- data.frame(id=unique(GeneSetAnno_df[,1]),
                                    shape= "text",
                                    label= paste(input$TypeGeneSetID,unique(GeneSetAnno_df[,1]), sep=""),
                                    color= "lightgrey",
                                    value= "0.4"
                                    )
      nodes <- rbind(nodes, nodesForGeneSet)
     }
     #}
  }

  ###### Attributes from geNetClassifier

    if(input$NodeAttri_ClassifierID == 'Studies'|| input$NodeAttri_ClassifierID == 'mRNA/Studies'){
    nodes <- rbind(nodes, Node_Diseases_obj(genesclassdetails = r_data$GenesClassDetails)) #r_data$GenesClassDetails
    edges <- rbind(edges, Edges_Diseases_obj(r_data$GenesClassDetails))
  }

  #
  #   lnodes <- data.frame(label = c("Gr A", "Gr B"),
  #                        shape = c( "circle","square"), color = c("red", "blue"),
  #                        title = "mRNA", id = 1:2)
  #
  #   ledges <- data.frame(color = c("lightblue", "red"),
  #                        label = c("reverse", "depends"), arrows =c("to", "from"))



      graphe <- visNetwork::visNetwork(nodes, edges, height = "500%",width = "500%") #%>%


    ## resizing nodes is not possible without scaling option. this is useful for circle, box shape
    ## see https://github.com/DataKnowledge/visNetwork/issues/49
    visNetwork::visNodes(graph = graphe, scaling = list(label = list(enabled = TRUE)))#%>%
    visNetwork::visNodes(graph = graphe, color = list(label = list(enabled = TRUE)))#%>%
    #visEdges(scaling = list(label = list(enabled = TRUE)))%>%
    graphe <- visNetwork::visOptions(graph= graphe, manipulation = TRUE,
                           #selectedBy = "group",
                           highlightNearest = TRUE )#%>%
    # visNetwork::visHierarchicalLayout(graph = graphe,enabled =  as.logical(input$enableHierarchiId),
    #                      direction = input$Hierarchi_AttId,
    #                     sortMethod= input$MethodHierarchiId)#%>%
    #
    # visNetwork::visPhysics(graph = graphe, solver = input$PhysicLayoutId  ## ’barnesHut’, ’repulsion’, ’hierarchicalRepulsion’, ’forceAtlas2Based’
    #            #,forceAtlas2Based = "avoidOverlap"
    #            )

  #     visNodes(color = list(border =  '#2B7CE9',background = '#97C2FC',highlight = "red"),
  #              font = '10px arial red',
  #              labelHighlightBold = NULL,
  #              borderWidth=3.5 ,
  #              #scaling = c(10,15,13,17,2.0,2.3,6,3,2.3,11),
  #              borderWidthSelected = 7) %>%
  #
     # visNetwork::visSave(graph= graphe, file = "network.html") #%>%
      visNetwork::visExport(graph= graphe,type = "png", name = "network",
                 label = paste0("Export as png"), background = "#fff",
                 float = "left", style = NULL, loadDependencies = TRUE)

        #visNetwork::visLegend(graph = graphe,
         #                     addNodes= nodes,
          #                    addEdges= edges,
                              #useGroups= TRUE,
           #                   position="left")


})

observe({
   network <- visNetwork::visNetworkProxy("network")
 #  if(input$enableHierarchiId){
 conditionalPanel("input.enableHierarchiId==true",
    visNetwork::visHierarchicalLayout(network, enabled= input$enableHierarchiId,
                          direction = input$Hierarchi_AttId,
                          sortMethod= input$MethodHierarchiId)
  )
  # }else{
 conditionalPanel("input.enableHierarchiId==false",
     visNetwork::visPhysics(network, solver = input$PhysicLayoutId )

     )
  #}

})
