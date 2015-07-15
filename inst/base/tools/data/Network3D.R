#### Server ####

  output$simpleNetwork <- networkD3::renderSimpleNetwork({

    if(input$GeneListID != "Genes"){
      GeneList <- unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep="")))
    }
    #GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")
    withProgress(message = paste('Loading Interactions from',input$ProviderID[1],'... Waiting...'), value = 0.2, {
      Sys.sleep(0.25)

    psicquic <- PSICQUIC::PSICQUIC()
    tbl <- PSICQUIC::interactions(psicquic, id=GeneList, species="9606", provider = input$ProviderID ,quiet=TRUE)
    if(nrow(tbl)==0){
        src <- c("N","O","_" ,"I", "n", "T", "E", "R", "A", "C", "t","i","o")
         target <- c("O","_","I" ,"n", "T", "E", "R", "A", "C", "t", "i","o","n!")
         networkData <- data.frame(src, target )
         networkD3::simpleNetwork(networkData, opacity = input$opacity,linkDistance = 30,fontSize = 17,textColour = "#ff0000",nodeClickColour = "#E34A33",linkColour = "white",nodeColour = "white",charge = -150)

    } else{
    tbl <- PSICQUIC::addGeneInfo(PSICQUIC::IDMapper("9606"), tbl)
    tbl <- with(tbl, as.data.frame(table(detectionMethod, type, A.name, B.name, provider)))
    tbl <- subset(tbl, Freq > 0)
    tbl <- tbl[!duplicated(tbl[,c("A.name","B.name")]),]
    tbL <<- tbl
    ## remove "-"
    tbl <- tbl[apply(tbl[c(3,4)], 1, function(row) all(row !="-" )),]
    networkData <- data.frame(tbl$A.name, tbl$B.name)

    networkD3::simpleNetwork(networkData, opacity = input$opacity)
    }
    })
  })

   output$forceNetwork <- networkD3::renderForceNetwork({

  if(input$GeneListID != "Genes"){
    GeneList <- unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep="")))
  }
  #GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")
  #GeneList <- c("CHEK1","CHEK2","RAD51","BRCA1","BRCA2","MLH1","MSH2","ATM","ATR","MDC1","PARP1","FANCF")

  withProgress(message = 'Loading Interactions. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    psicquic <- PSICQUIC::PSICQUIC()
    tbl <- PSICQUIC::interactions(psicquic, id=GeneList,species="9606", provider = input$ProviderID,quiet=TRUE)

    if(nrow(tbl)==0){


    } else{
      tbl <- PSICQUIC::addGeneInfo(PSICQUIC::IDMapper("9606"), tbl)
      tbl <- with(tbl, as.data.frame(table(detectionMethod, type, A.name, B.name, provider)))
      tbl <- subset(tbl, Freq > 0)
      tbl <- tbl[!duplicated(tbl[,c("A.name","B.name")]),]
      tBl <<- tbl
      tbl <- tbl[apply(tbl[c(3,4)], 1, function(row) all(row !="-" )),]


      ### The column 2 of Links_df "Sampling" is arbitrary information: Kind of interaction (Edge style)
  Links_df <- as.data.frame(cbind(as.numeric(tbl$A.name),as.numeric(tbl$B.name),
                                  sample(1:20,length(tbl[,"A.name"]) , replace = TRUE)))
  names(Links_df) <- c("source", "target", "value")

      ### the second and third columns of Modes_df are arbitrary: group: Processu biologic, size: Regulation

  Nodes_df <- data.frame(tbl[,"A.name"], sample(1:4, length(tbl[,"A.name"]), replace = TRUE),
                                         sample(1:100,length(tbl[,"A.name"]) , replace = TRUE))
  names(Nodes_df) <- c("name", "group", "size")
  #nodes_df <<- Nodes_df
  #links_df <<- Links_df

    networkD3::forceNetwork(Links = Links_df, Nodes = Nodes_df, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = input$opacity, zoom = TRUE, legend = TRUE)


  }

  })
  })

