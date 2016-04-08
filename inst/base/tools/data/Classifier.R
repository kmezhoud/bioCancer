output$ClassifierHowto <- renderPrint({
  cat("
       1 - Select Studies
       2 - Get Samples size
       3 - Select Cases and Genetic Profiles
       4 - Run Classifier
      ")
})



output$list_Cases <- renderUI({
  shiny::withProgress(message = 'loading Cases...', value = 0.1, {
    Sys.sleep(0.25)
    checked_Studies <- input$StudiesIDClassifier
    listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,1])
    names(listCases) <- checked_Studies
    listCases <- lapply(listCases, function(x) x[grep("mrna", x)])
    listCases <- listCases[lapply(listCases,length)>0]
    r_data[['listCases']] <- listCases
    selectizeInput('CasesIDClassifier','Select one Cases by Study', choices = listCases, multiple=TRUE, selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna"))
    #updateSelectizeInput(session, 'CasesIDClassifier', choices = listCases,selected = listCases[1])
  })
})

output$list_GenProfs <- renderUI({
  shiny::withProgress(message = 'loading Genetic Profiles...', value = 0.1, {
    Sys.sleep(0.25)
    checked_Studies <- input$StudiesIDClassifier
    listGenProfs <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,1])
    names(listGenProfs) <- checked_Studies
    listGenProfs <- lapply(listGenProfs, function(x) x[grep("v2", x)])
    listGenProfs <- listGenProfs[lapply(listGenProfs,length)>0]
    r_data[['listGenProfs']] <- listGenProfs
    selectizeInput('GenProfsIDClassifier', 'Select one Genetic Profile by Study', listGenProfs, multiple = TRUE, selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna"))
  })
})


# output$selection <- renderPrint(
#   input$mychooser
# )

TableCases <- reactive({
  shiny::withProgress(message = 'loading Sample size...', value = 0.1, {
    Sys.sleep(0.25)

    checked_Studies <- input$StudiesIDClassifier
    listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,3])
    #listGenProf <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,2])
    matchedCases <- lapply(listCases, function(x) x[grep("mRNA expression", x)])
    #matchedGenProf <- lapply(listGenProf, function(x)x[grep("mRNA expression",x)])
    ## remove emply list
    matchedCases <- matchedCases[lapply(matchedCases,length)>0]
    #matchedGenProf <- matchedGenProf[lapply(matchedGenProf,length)>0]

    #  dat <- data.frame(Studies=NA,Case=NA, GenProf=NA)
    dat <- data.frame(Studies=NA,Cases=NA)
    for(s in 1:length(matchedCases)){
      dat <- rbind(dat, c(checked_Studies[s], matchedCases[[s]]))
      #dat[s,] <- c(checked_Studies[s], matchedCases[[s]], matchedGenProf[[s]])
    }
    return(dat)
  })
})

# output$viewTableCases <- renderTable({
#   if (is.null(input$StudiesIDClassifier))
#     return()
#     TableCases()
# })

output$viewTableCases <- DT::renderDataTable({
  dat <-   TableCases()
  action = DT::dataTableAjax(session, dat, rownames = FALSE)

  #DT::datatable(dat, filter = "top", rownames = FALSE, server = TRUE,
  DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                rownames = FALSE, style = "bootstrap", escape = FALSE,
                # class = "compact",
                options = list(
                  ajax = list(url = action),
                  search = list(regex = TRUE),
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  autoWidth = TRUE,
                  processing = FALSE,
                  pageLength = 10,
                  lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                )
  )
})


#Size <- input$SampleSizeClassifier

# output$SampleSize <- renderPrint({
#   if (is.null(input$SampleSizeClassifierID))
#     return()
#   str(input$SampleSizeClassifierID)
# })
#
# output$ClassifierThreshold <- renderPrint({
#   if (is.null(input$ClassifierThresholdID))
#     return()
#   input$ClassifierThresholdID
# })

# output$PrintCases <- renderPrint({
# input$CasesIDClassifier
# })

output$Plot_enricher <- renderPlot({
  shiny::withProgress(message = 'Genes Diseases Association...', value = 0.1, {
    Sys.sleep(0.25)

    GeneList <- whichGeneList()

    ## clusterProfile package
    #GeneID = bitr(GeneList, fromType="SYMBOL", toType="ENTREZID", annoDb="org.Hs.eg.db")[,2]
    ## Symbol2GeneID
    GeneID<- unname(unlist(AnnotationFuncs::translate(GeneList, org.Hs.eg.db::org.Hs.egSYMBOL2EG)))

    ## downloaded from http://www.disgenet.org/ds/DisGeNET/resultsDisGeNet.tar.gz
    if ("package:bioCancer" %in% search()) {
      gda <- readRDS(paste0(system.file(package = "bioCancer"),"/extdata/DisGeNet.RDS"))
      #gdafile <- read.delim(system.file("extdata", "all_gene_disease_associations.txt", package="bioCancer"))
      #gdafile <- system.file("extdata", "c5.cc.v5.0.entrez.gmt", package="clusterProfiler")

    }else{
      gda <- readRDS(file.path(paste(r_path,"/extdata/DisGeNet.RDS", sep="")))
    }
    disease2gene=gda[, c("diseaseId", "geneId")]
    disease2name=gda[, c("diseaseId", "diseaseName")]
    x <- clusterProfiler::enricher(GeneID, pvalueCutoff = 0.05,TERM2GENE=disease2gene, TERM2NAME=disease2name)
    r_data[['x']] <- x
    options(scipen = 0, digits = 2)
    barplot(x,drop=TRUE,showCategory=10 ,digits=2)
  })
})

Plot_enrich <- function(){
  options(scipen = 0, digits = 2)
  barplot(r_data$x,drop=TRUE, showCategory=10 ,digits=2)

}

## Disease - Genes - Studies Associations
output$compareClusterDO <- renderPlot({
  shiny::withProgress(message = 'Disease Onthology enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(AnnotationFuncs::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))

    if (inherits(try(cdo <- clusterProfiler::compareCluster(GroupsID, fun="enrichDO"), silent=TRUE),"try-error"))
    {print("No enrichment found in any of gene cluster, please check your input...")
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No Disease Onthology enrichment found\n",
                                   " in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")

    }else{
      cdo <- clusterProfiler::compareCluster(GroupsID, fun="enrichDO")
      r_data[['cdo']] <- cdo
      plot(cdo)
    }
  })
})

compareClusterDO <- function(){
  plot(r_data$cdo, type="dot", title="Disease Onthology Enrichment Comparison")

}


## Reactome Pathway Cluster Enrichment
output$compareClusterReactome <- renderPlot({
  shiny::withProgress(message = 'Reactome Pathway enrich...', value = 0.1, {
    Sys.sleep(0.25)
      require(reactome.db)
      require(ReactomePA)
    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(AnnotationFuncs::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))

    if (inherits(try(cdp <- clusterProfiler::compareCluster(GroupsID, fun="enrichPathway"), silent=TRUE),"try-error"))
    { print("No Reactome enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No Reactome Pathway enrichment found\n",
                                   "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
    }else{
      cdReactome <- clusterProfiler::compareCluster(GroupsID, fun="enrichPathway")
      r_data[['cdReactome']] <- cdReactome
      plot(cdReactome)
    }
  })
})

compareClusterReactome <- function(){
  plot(r_data$cdReactome, type="dot", title="Reactome Pathway Enrichment Comparison")
}
## Gene Onthology (GO) Studies Associations
output$compareClusterGO <- renderPlot({
  shiny::withProgress(message = 'Gene Onthology Enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(AnnotationFuncs::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
    if (inherits(try(cgo <- clusterProfiler::compareCluster(GroupsID, fun="enrichGO",OrgDb='org.Hs.eg.db')),"try-error"))
    {
      print("No Gene Onthology enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No Gene Onthology Pathway enrichment found\n",
                                   "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
    }else{
      cgo <- clusterProfiler::compareCluster(GroupsID, fun="enrichGO",OrgDb='org.Hs.eg.db')
      r_data[['cgo']] <- cgo
      plot(r_data$cgo)
    }
  })
})

compareClusterGO <- function(){

  plot(r_data$cgo, type="dot", title="GO Enrichment Comparison")
}
## KEGG Pathway Enrichment
output$compareClusterKEGG <- renderPlot({
  shiny::withProgress(message = 'KEGG Pathway Enrich...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(AnnotationFuncs::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
    if (inherits(try(cgo <- clusterProfiler::compareCluster(GroupsID, fun="enrichKEGG")),"try-error"))
    {
      #print("No KEGG enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No KEGG Pathway enrichment found\n",
                                   "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
    }else{
      ckegg <- clusterProfiler::compareCluster(GroupsID, fun="enrichKEGG")
      r_data[['ckegg']] <- ckegg
      plot(ckegg)
    }
  })
})
compareClusterKEGG <- function(){
  plot(r_data$ckegg, type="dot", title="KEGG Enrichment Comparison")
}

## Cellular Component  Enrichment
output$compareClusterCC<- renderPlot({
  shiny::withProgress(message = 'Cellular Component enrichment...', value = 0.1, {
    Sys.sleep(0.25)

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(AnnotationFuncs::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
    if (inherits(try(cgo <- clusterProfiler::compareCluster(GroupsID, fun="groupGO", OrgDb='org.Hs.eg.db')),"try-error"))
    {
      print("No Cellular Component enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No Cellular Component Pathway enrichment found\n",
                                   "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
    }else{
      cCC <- clusterProfiler::compareCluster(GroupsID, fun="groupGO", OrgDb='org.Hs.eg.db')
      r_data[['cCC']] <- cCC
      plot(cCC)
    }
  })
})
compareClusterCC <- function(){
  plot(r_data$cCC, type="dot", title="Cellular Component Enrichment Comparison")
}
