#### Load necessary packages and data ####














# forceNetwork(Links = Links_df, Nodes = Nodes_df, Source = "source",
#              Target = "target", Value = "value", NodeID = "name",
#              Group = "group", opacity = 0.8, zoom = TRUE)


#data(MisLinks)
#data(MisNodes)




#### Server ####

  output$simpleNetwork <- renderSimpleNetwork({

    if(input$GeneListID != "Genes"){
      GeneList <- unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep="")))
    }
    #GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")
    withProgress(message = paste('Loading Interactions from',input$ProviderID[1],'... Waiting...'), value = 0.2, {
      Sys.sleep(0.25)

    psicquic <- PSICQUIC()
    tbl <- interactions(psicquic, id=GeneList,species="9606", provider = input$ProviderID ,quiet=TRUE)
    if(nrow(tbl)==0){
        src <- c("N","O","_" ,"I", "n", "T", "E", "R", "A", "C", "t","i","o")
         target <- c("O","_","I" ,"n", "T", "E", "R", "A", "C", "t", "i","o","n!")
         networkData <- data.frame(src, target )
         simpleNetwork(networkData, opacity = input$opacity,linkDistance = 30,fontSize = 17,textColour = "#ff0000",nodeClickColour = "#E34A33",linkColour = "white",nodeColour = "white",charge = -150)

    } else{
    tbl <- addGeneInfo(IDMapper("9606"), tbl)
    tbl <- with(tbl, as.data.frame(table(detectionMethod, type, A.name, B.name, provider)))
    tbl <- subset(tbl, Freq > 0)
    tbl <- tbl[!duplicated(tbl[,c("A.name","B.name")]),]
    networkData <- data.frame(tbl$A.name, tbl$B.name)

    simpleNetwork(networkData, opacity = input$opacity)
    }
    })
  })

   output$forceNetwork <- renderForceNetwork({

  if(input$GeneListID != "Genes"){
    GeneList <- unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep="")))
  }
  #GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")
  #GeneList <- c("CHEK1","CHEK2","RAD51","BRCA1","BRCA2","MLH1","MSH2","ATM","ATR","MDC1","PARP1","FANCF")

  withProgress(message = 'Loading Interactions. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    psicquic <- PSICQUIC()
    tbl <- interactions(psicquic, id=GeneList,species="9606", provider = input$ProviderID,quiet=TRUE)

    if(nrow(tbl)==0){


    } else{
      tbl <- addGeneInfo(IDMapper("9606"), tbl)
      tbl <- with(tbl, as.data.frame(table(detectionMethod, type, A.name, B.name, provider)))
      tbl <- subset(tbl, Freq > 0)
      tbl <- tbl[!duplicated(tbl[,c("A.name","B.name")]),]

  Links_df <- as.data.frame(cbind(as.numeric(tbl$A.name),as.numeric(tbl$B.name),
                                  sample(1:20,length(tbl[,"A.name"]) , replace = TRUE)))
  names(Links_df) <- c("source", "target", "value")

  Nodes_df <- data.frame(tbl[,"A.name"], sample(1:4, length(tbl[,"A.name"]), replace = TRUE),
                                         sample(1:100,length(tbl[,"A.name"]) , replace = TRUE))
  names(Nodes_df) <- c("name", "group", "size")

    forceNetwork(Links = Links_df, Nodes = Nodes_df, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = input$opacity, zoom = TRUE, legend = TRUE)
   }

  })
  })

