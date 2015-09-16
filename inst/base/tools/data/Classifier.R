
output$list_Cases <- renderUI({
  withProgress(message = 'loading Cases...', value = 0.1, {
    Sys.sleep(0.25)
  checked_Studies <- input$StudiesIDClassifier
  listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,1])
  names(listCases) <- checked_Studies
  listCases <- lapply(listCases, function(x) x[grep("mrna", x)])
  listCases <- listCases[lapply(listCases,length)>0]
  selectizeInput('CasesIDClassifier','Select one Cases by Study', choices = listCases, multiple=TRUE, selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna"))
  #updateSelectizeInput(session, 'CasesIDClassifier', choices = listCases,selected = listCases[1])
})
})

output$PrintCases <- renderPrint({
input$CasesIDClassifier
})

output$list_GenProfs <- renderUI({
  withProgress(message = 'loading Genetic Profiles...', value = 0.1, {
    Sys.sleep(0.25)
  checked_Studies <- input$StudiesIDClassifier
  listGenProfs <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,1])
  names(listGenProfs) <- checked_Studies
  listGenProfs <- lapply(listGenProfs, function(x) x[grep("mrna", x)])
  listGenProfs <- listGenProfs[lapply(listGenProfs,length)>0]
  selectizeInput('GenProfsIDClassifier', 'Select one Genetic Profile (v2_mrna) by Study', listGenProfs, multiple = TRUE, selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna"))
  })
})

TableCases <- reactive({
  withProgress(message = 'loading Sample size...', value = 0.1, {
  Sys.sleep(0.25)
  dat <- data.frame(Studies=NA,Case=NA, GenProf=NA)
  checked_Studies <- input$StudiesIDClassifier
  listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,3])
  listGenProf <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,2])
  matchedCases <- lapply(listCases, function(x) x[grep("mRNA", x)])
  matchedGenProf <- lapply(listGenProf, function(x)x[grep("mRNA",x)])
  ## remove emply list
  matchedCases <- matchedCases[lapply(matchedCases,length)>0]
  matchedGenProf <- matchedGenProf[lapply(matchedGenProf,length)>0]

  for(s in 1:length(matchedCases)){

        dat[s,] <- c(checked_Studies[s], matchedCases[s], matchedGenProf[s])
  }
  return(dat)
  })
})

output$viewTableCases <- renderTable({
  if (is.null(input$StudiesIDClassifier))
    return()
    TableCases()
})

#Size <- input$SampleSizeClassifier

output$SampleSize <- renderPrint({
  if (is.null(input$SampleSizeClassifierID))
    return()
  str(input$SampleSizeClassifierID)
})

output$ClassifierThreshold <- renderPrint({
  if (is.null(input$ClassifierThresholdID))
    return()
  input$ClassifierThresholdID
})


output$Plot_enricher <- renderPlot({
  withProgress(message = 'Genes Diseases Association...', value = 0.1, {
    Sys.sleep(0.25)

  if(input$GeneListID == "Genes"){
    GeneList <- r_data$Genes

  }else if(input$GeneListID =="Reactome_GeneList"){
    GeneList <- r_data$Reactome_GeneList
  }else{
    GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  }

 ## clusterProfile package
  #GeneID = bitr(GeneList, fromType="SYMBOL", toType="ENTREZID", annoDb="org.Hs.eg.db")[,2]
  ## Symbol2GeneID
  GeneID<- unname(unlist(translate(GeneList, org.Hs.egSYMBOL2EG)))

  ## downloaded from http://www.disgenet.org/ds/DisGeNET/results/all_gene_disease_associations.tar.gz
  gda <- read.delim(paste0(getwd(),"/all_gene_disease_associations.txt", sep=""))
  disease2gene=gda[, c("diseaseId", "geneId")]
  disease2name=gda[, c("diseaseId", "diseaseName")]
  x <- enricher(GeneID, pvalueCutoff = 0.05,TERM2GENE=disease2gene, TERM2NAME=disease2name)
  options(scipen = 0, digits = 2)
  barplot(x,drop=TRUE, showCategory=10 ,digits=2)
})
})

## Disease - Genes - Studies Associations
output$compareClusterDO <- renderPlot({
  withProgress(message = 'Disease Onthology enrich...', value = 0.1, {
    Sys.sleep(0.25)

  genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
  GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))

  if (inherits(try(cdo <- compareCluster(GroupsID, fun="enrichDO"), silent=TRUE),"try-error"))
  {print("No enrichment found in any of gene cluster, please check your input...")
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("No Disease Onthology enrichment found\n",
                                 " in any of gene cluster, \n",
                                 "Please check your input..."),
                           cex = 1, col = "red")

    }else{
    cdo <- compareCluster(GroupsID, fun="enrichDO")
    plot(cdo)
    }
})
})

## Pathway Cluster Enrichment
output$compareClusterPathway <- renderPlot({
  withProgress(message = 'Cluster Pathway enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))

    if (inherits(try(cdo <- compareCluster(GroupsID, fun="enrichPathway"), silent=TRUE),"try-error"))
    { print("No enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No Pathway enrichment found\n",
                                    "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
      }else{
      cdo <- compareCluster(GroupsID, fun="enrichPathway")
      plot(cdo)
    }
  })
})


## Gene Onthology Studies Associations
output$compareClusterGO <- renderPlot({
  withProgress(message = 'Gene Onthology Enrich...', value = 0.1, {
    Sys.sleep(0.25)

  genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
  GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))
  if (inherits(try(cgo <- compareCluster(GroupsID, fun="enrichGO")),"try-error"))
  {
    stop("No GO enrichment found with actual Gene List...")
  }else{
    cgo <- compareCluster(GroupsID, fun="enrichGO")
    plot(cgo)
  }
})
})

## KEGG Pathway Enrichment
output$compareClusterKEGG <- renderPlot({
  withProgress(message = 'KEGG Pathway Enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))
    if (inherits(try(cgo <- compareCluster(GroupsID, fun="enrichKEGG")),"try-error"))
    {
      stop("No KEGG enrichment found with actual Gene List...")
    }else{
      cgo <- compareCluster(GroupsID, fun="enrichKEGG")
      plot(cgo)
    }
  })
})
