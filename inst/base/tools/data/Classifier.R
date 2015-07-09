
output$list_Cases <- renderUI({
  checked_Studies <- input$StudiesIDClassifier
  listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,1])
  names(listCases) <- checked_Studies
  listCases <- lapply(listCases, function(x) x[grep("mrna", x)])
  listCases <- listCases[lapply(listCases,length)>0]
  selectizeInput('CasesIDClassifier','Select one Cases by Study', choices = listCases, multiple=TRUE, selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna"))
  #updateSelectizeInput(session, 'CasesIDClassifier', choices = listCases,selected = listCases[1])

})
output$PrintCases <- renderPrint({
input$CasesIDClassifier
})

output$list_GenProfs <- renderUI({
  checked_Studies <- input$StudiesIDClassifier
  listGenProfs <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,1])
  names(listGenProfs) <- checked_Studies
  listGenProfs <- lapply(listGenProfs, function(x) x[grep("mrna", x)])
  listGenProfs <- listGenProfs[lapply(listGenProfs,length)>0]
  selectizeInput('GenProfsIDClassifier', 'Select one Genetic Profile by Study', listGenProfs, multiple = TRUE, selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna"))
})

TableCases <- reactive({
  dat <- data.frame(Studies=NA,Case=NA, GenProf=NA)
  checked_Studies <- input$StudiesIDClassifier
  listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,3])
  listGenProf <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,2])
  matchedCases <- lapply(listCases, function(x) x[grep("mRNA", x)])
  matchedGenProf <- lapply(listGenProf, function(x)x[grep("mRNA",x)])
  ## remove emply list
  matchedCases <- matchedCases[lapply(matchedCases,length)>0]
  matchedGenProf <- matchedGenProf[lapply(matchedGenProf,length)>0]

  for(s in 1:length(checked_Studies)){
        dat[s,] <- c(checked_Studies[s], matchedCases[s], matchedGenProf[s])
  }
  return(dat)
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
  }else{
    GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  }

 ## clusterProfile package
  ##GeneID = bitr(GeneList, fromType="SYMBOL", toType="ENTREZID", annoDb="org.Hs.eg.db")
  ## Symbol2GeneID
  GeneID<- unname(unlist(translate(GeneList, org.Hs.egSYMBOL2EG)))

  ## downloaded from http://www.disgenet.org/ds/DisGeNET/results/all_gene_disease_associations.tar.gz
  gda <- read.delim(paste0(getwd(),"/all_gene_disease_associations.txt", sep=""))
  disease2gene=gda[, c("diseaseId", "geneId")]
  disease2name=gda[, c("diseaseId", "diseaseName")]
  x <- enricher(GeneID, TERM2GENE=disease2gene, TERM2NAME=disease2name)
  barplot(x,drop=TRUE, showCategory=12)
})
})
output$compareClusterDO <- renderPlot({
  withProgress(message = 'Disease Onthology enrich...', value = 0.1, {
    Sys.sleep(0.25)

  genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
  GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))

  if (inherits(try(cdo <- compareCluster(GroupsID, fun="enrichDO"), silent=TRUE),"try-error"))
  { print("No enrichment found in any of gene cluster, please check your input...")}else{
    cdo <- compareCluster(GroupsID, fun="enrichDO")
    plot(cdo)
    }
})
})

output$compareClusterPathway <- renderPlot({
  withProgress(message = 'Cluster Pathway enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))

    if (inherits(try(cdo <- compareCluster(GroupsID, fun="enrichPathway"), silent=TRUE),"try-error"))
    { print("No enrichment found in any of gene cluster, please check your input...")}else{
      cdo <- compareCluster(GroupsID, fun="enrichPathway")
      plot(cdo)
    }
  })
})



output$compareClusterGO <- renderPlot({
  withProgress(message = 'Gene Onthology Enrich...', value = 0.1, {
    Sys.sleep(0.25)

  genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
  GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))
  if (inherits(try(cgo <- compareCluster(GroupsID, fun="enrichGO")),"try-error"))
  {}else{
    cgo <- compareCluster(GroupsID, fun="enrichGO")
    plot(cgo)
  }
})
})

output$compareClusterKEGG <- renderPlot({
  withProgress(message = 'KEGG Pathway Enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetails, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(translate(x, org.Hs.egSYMBOL2EG))))
    if (inherits(try(cgo <- compareCluster(GroupsID, fun="enrichKEGG")),"try-error"))
    {}else{
      cgo <- compareCluster(GroupsID, fun="enrichKEGG")
      plot(cgo)
    }
  })
})
