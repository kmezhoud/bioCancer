output$ClassifierHowto <- renderPrint({
  cat("
      1 - Select Studies
      2 - Set Samples size
      3 - Run Classification
      4 - Plot Cluster profiles
      ")
})



# output$list_Cases <- renderUI({
#   shiny::withProgress(message = 'loading Cases...', value = 0.1, {
#     Sys.sleep(0.25)
#     checked_Studies <- input$StudiesIDClassifier
#     listCases <- lapply(checked_Studies, function(x) getCaseLists(cgds,x)[,1])
#     names(listCases) <- checked_Studies
#     listCases <- lapply(listCases, function(x) x[grep("v2_mrna", x)])
#     listCases <- listCases[lapply(listCases,length)>0]
#     r_data[['listCases']] <- listCases
#     selectizeInput('CasesIDClassifier','Select one Cases by Study', choices = listCases, multiple=TRUE)
#     #selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna")
#     #updateSelectizeInput(session, 'CasesIDClassifier', choices = listCases,selected = listCases[1])
#   })
# })
#
# output$list_GenProfs <- renderUI({
#   shiny::withProgress(message = 'loading Genetic Profiles...', value = 0.1, {
#     Sys.sleep(0.25)
#     checked_Studies <- input$StudiesIDClassifier
#     listGenProfs <- lapply(checked_Studies, function(x) getGeneticProfiles(cgds,x)[,1])
#     names(listGenProfs) <- checked_Studies
#     listGenProfs <- lapply(listGenProfs, function(x) x[grep("v2_mrna$", x)])
#     listGenProfs <- listGenProfs[lapply(listGenProfs,length)>0]
#     r_data[['listGenProfs']] <- listGenProfs
#     selectizeInput('GenProfsIDClassifier', 'Select one Genetic Profile by Study', listGenProfs, multiple = TRUE)
#     #  selected=c("brca_tcga_rna_v2_mrna", "gbm_tcga_rna_v2_mrna","lihc_tcga_rna_v2_mrna","lusc_tcga_rna_v2_mrna")
#   })
# })


# output$selection <- renderPrint(
#   input$mychooser
# )

TableCases <- reactive({
  shiny::withProgress(message = 'loading Sample size...', value = 1, {

    checked_Studies <- input$StudiesIDClassifier
    listCases <- lapply(checked_Studies, function(x)  sampleLists(cgds, x) |>pull(sampleListId))
    #listGenProf <- lapply(checked_Studies, function(x)getGeneticProfiles(cgds,x)[,2])
    matchedCases <- lapply(listCases, function(x) x[grep("mRNA expression", x)])
    #matchedGenProf <- lapply(listGenProf, function(x)x[grep("mRNA expression",x)])
    ## remove emply list
    matchedCases <- matchedCases[lapply(matchedCases,length)>0]
    #matchedGenProf <- matchedGenProf[lapply(matchedGenProf,length)>0]
    if(length(matchedCases) < 2){
      dat <- as.data.frame('Check Cases for selected studies. Some ones do not have samples of mRNA expression.')
    }else if (length(matchedCases) < 3){
      dat <- as.data.frame('It is recommended to select at less 3 studies with mRNA data.')
    }else if (length(checked_Studies) > length(matchedCases)){
      dat <- as.data.frame('Some selected study does not have mRNA data. Select only study with mRNA data.')
    }else{
      #  dat <- data.frame(Studies=NA,Case=NA, GenProf=NA)
      dat <- data.frame(Studies=NA,Cases=NA)
      for(s in 1:length(matchedCases)){
        dat <- rbind(dat, c(checked_Studies[s], matchedCases[[s]]))
        #dat[s,] <- c(checked_Studies[s], matchedCases[[s]], matchedGenProf[[s]])
      }
    }
    return(dat)
  })
})


output$viewTableCases <- DT::renderDataTable({
  dat <-   TableCases()
  displayTable(dat) %>% DT::formatStyle(names(dat),
                                        color = DT::styleEqual('Check Cases for selected studies. Some ones do not have samples of mRNA expression.',
                                                               'red'))#, backgroundColor = 'white', fontWeight = 'bold'

})




output$getGenesClassifier <- DT::renderDataTable({

  shiny::withProgress(message = 'loading Genetic Profiles...', value = 1, {

    listGenProfs <-  getList_GenProfs(input$StudiesIDClassifier)
  })

  shiny::withProgress(message = 'loading Cases...', value = 1, {

    listCases <- getList_Cases(input$StudiesIDClassifier)
  })

  shiny::withProgress(message = 'geNetClassifier is running...', value = 1, {

    dat <-   getGenesClassification(checked_Studies = input$StudiesIDClassifier,
                                    GeneList = whichGeneList(input$GeneListID),
                                    samplesize = input$SampleSizeClassifierID,
                                    threshold = input$ClassifierThresholdID,
                                    listGenProfs= listGenProfs,
                                    listCases = listCases
    )
  })
  r_data[['GenesClassDetails']] <- dat
  displayTable(dat)%>% DT::formatStyle(names(dat),
                                       color = DT::styleEqual("Gene List is empty. copy and paste genes from text file (Gene/line) or use gene list from examples.",
                                                              'red'))#, backgroundColor = 'white', fontWeight = 'bold'
})


output$dl_GenesClassDetails_tab <- shiny::downloadHandler(
  filename = function() { paste0("Classification_tab.csv") },
  content = function(file) {
    get_data(r_data$GenesClassDetails, vars = NULL,
            rows = NULL, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)

## Render Gene-Disease Association using DisGeNet database
output$Plot_enricher <- renderPlot({
  shiny::withProgress(message = 'Genes Diseases Association...', value = 1, {

    GeneList <- whichGeneList(input$GeneListID)

    ## clusterProfile package
    #GeneID = bitr(GeneList, fromType="SYMBOL", toType="ENTREZID", annoDb="org.Hs.eg.db")[,2]
    ## Symbol2GeneID
    GeneID<- unname(unlist(bioCancer::translate(GeneList, org.Hs.eg.db::org.Hs.egSYMBOL2EG)))


    ## How to create DisGeNet.RDS file
    # download all_gene_disease_associations.tsv.gz from  http://www.disgenet.org/web/DisGeNET/menu/downloads
    # db <- read.delim("all_gene_disease_associations.tsv.gz")
    # saveRDS(db, "DisGeNet0918.RDS")
    # str(readRDS("DisGeNet0918.RDS))

    ## use disgenet2r package to get last version
    # install manually SPARKL from archive https://cran.r-project.org/src/contrib/Archive/SPARQL/
    # install disgenet2r from bitbuket
    # library(devtools)
    # install_bitbucket("ibi_group/disgenet2r")
    # get all gene list from Gene Association Tab: https://www.disgenet.org/downloads
    # Extract only Gene list
    # DisGeNet_2023 <- read.csv2("~/Downloads/gene_associations.tsv", header = TRUE, sep = "\t")
    # geneList <- DisGeNet_2023$geneSymbol
    # libsrary(disgenet2r)
    # disgenet_api_key <-get_disgenet_api_key(email = "kmezhoud@gmail.com", password = "$Chocolat01", verbose = FALSE, warnings = TRUE)

    # get_all_gene_disease_association <- function(genelist){
    #   gene2disease(
    #     genelist,
    #     vocabulary = "HGNC",
    #     database = "CURATED",
    #     score = c(0, 1),
    #     api_key = disgenet_api_key,
    #     verbose = FALSE,
    #     warnings = FALSE
    #   )
    # }
    #
    # map_output <- purrr::map(split(geneList,
    #                                ceiling(seq_along(geneList)/500)),
    #                          get_all_gene_disease_association)
    #
    # # remove umpty elemnt in list
    # map_output <- map_output[lapply(map_output,class)=='DataGeNET.DGN']
    #
    # # extract slot qresult from a list and  merge them in dataframe
    # DisGeNet2023  <- plyr::ldply(map_output, slot, "qresult") |>
    #   dplyr::select(geneid, gene_symbol, diseaseid, disease_name ) |>
    #   dplyr::rename(geneId = geneid, geneSymbol = gene_symbol, diseaseId= diseaseid, diseaseName = disease_name)


    ## download DisGeNet.RDS file from ubuntu/kmezhoud/bioCancer
    if(is.null(r_data$gda)){
      shiny::withProgress(message = 'Loading DisGeNet.RDS...', value = 1, {

        #download.file("https://wiki.ubuntu.com/kmezhoud/bioCancer?action=AttachFile&do=get&target=DisGeNet.RDS",tmp <- tempfile())
        download.file("https://github.com/kmezhoud/bioCancer/raw/master/inst/extdata/DisGeNet0223.RDS",tmp <- tempfile())
        gda <- readRDS(tmp)
        r_data[['gda']] <- gda

      })
    }

    ## from http://www.disgenet.org/ds/DisGeNET/resultsDisGeNet.tar.gz
    # ## downloaded local DisGeNet.RDS
    # if ("package:bioCancer" %in% search()) {
    #   gda <- readRDS(paste0(system.file(package = "bioCancer"),"/extdata/DisGeNet.RDS"))
    #
    # }else{
    #   gda <- readRDS(file.path(paste(r_path,"/extdata/DisGeNet.RDS", sep="")))
    # }
    # for Local RDC file
    disease2gene=r_data$gda[, c("diseaseId", "geneId")]
    disease2name=r_data$gda[, c("diseaseId", "diseaseName")]
    ###
    ## Use DisGeNet.R API
    # entity = "gene"
    # identifier = "entrez"
    # gda <- doQuery(entity, identifier)
    #
    # disease2gene = gda[,c("c1.cui","c2.geneId")]
    # disease2name = gda[, c("c1.cui", "c1.name")]
    ####
    GDA_enricher <- clusterProfiler::enricher(GeneID, pvalueCutoff = 0.05,TERM2GENE=disease2gene, TERM2NAME=disease2name)
    r_data[['GDA_enricher']] <- GDA_enricher
    options(scipen = 0, digits = 2)
    barplot(GDA_enricher,drop=TRUE,showCategory=10 ,digits=2)
  })
})

## function used to download the plot using plot_downloader
Plot_enrich <- function(){
  options(scipen = 0, digits = 2)
  barplot(r_data$GDA_enricher,drop=TRUE, showCategory=10 ,digits=2)

}

## Disease - Genes - Studies Associations
output$compareClusterDO <- renderPlot({
  shiny::withProgress(message = 'Disease Ontology enrich...', value = 1, {

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(bioCancer::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))

    if (inherits(try(cdo <- clusterProfiler::compareCluster(GroupsID, fun="enrichDO"), silent=TRUE),"try-error"))
    {print("No enrichment found in any of gene cluster, please check your input...")
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No Disease Ontology enrichment found\n",
                                   " in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")

    }else{
      cdo <- clusterProfiler::compareCluster(GroupsID, fun="enrichDO")
      r_data[['cdo']] <- cdo
      clusterProfiler::dotplot(cdo)
    }
  })
})

compareClusterDO <- function(){
  clusterProfiler::dotplot(r_data$cdo, title="Disease Ontology Enrichment Comparison")

}


## Reactome Pathway Cluster Enrichment
output$compareClusterReactome <- renderPlot({
  shiny::withProgress(message = 'Reactome Pathway enrich...', value = 1, {
    #   require(reactome.db)
    #  require(ReactomePA)
    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(bioCancer::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))

    if (inherits(try(cdp <- clusterProfiler::compareCluster(GroupsID, fun="enrichPathway"), silent=TRUE),"try-error"))
    { print("No Reactome enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No Reactome Pathway enrichment found\n",
                                   "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
    }else{
      cdReactome <- clusterProfiler::compareCluster(GroupsID, fun="enrichPathway")
      r_data[['cdReactome']] <- cdReactome
      clusterProfiler::dotplot(cdReactome)
    }
  })
})

compareClusterReactome <- function(){
  clusterProfiler::dotplot(r_data$cdReactome, title="Reactome Pathway Enrichment Comparison")
}
## Gene Ontology (GO) Studies Associations
output$compareClusterGO <- renderPlot({
  shiny::withProgress(message = 'Gene Ontology Enrich...', value = 1, {

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(bioCancer::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
    if (inherits(try(cgo <- clusterProfiler::compareCluster(GroupsID, fun="enrichGO",OrgDb='org.Hs.eg.db')),"try-error"))
    {
      print("No Gene Ontology enrichment found in any of gene cluster, please check your input...")
      text(x = 0.5, y = 0.5, paste("No Gene Ontology Pathway enrichment found\n",
                                   "in any of gene cluster, \n",
                                   "Please check your input..."),
           cex = 1, col = "red")
    }else{
      cgo <- clusterProfiler::compareCluster(GroupsID, fun="enrichGO",OrgDb='org.Hs.eg.db')
      r_data[['cgo']] <- cgo
      clusterProfiler::dotplot(r_data$cgo)
    }
  })
})

compareClusterGO <- function(){

  clusterProfiler::dotplot(r_data$cgo, title="GO Enrichment Comparison")
}
## KEGG Pathway Enrichment
output$compareClusterKEGG <- renderPlot({
  shiny::withProgress(message = 'KEGG Pathway Enrich...', value = 1, {

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(bioCancer::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
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
      clusterProfiler::dotplot(ckegg)
    }
  })
})

compareClusterKEGG <- function(){
  clusterProfiler::dotplot(r_data$ckegg, title="KEGG Enrichment Comparison")
}

## Cellular Component  Enrichment
output$compareClusterCC<- renderPlot({
  shiny::withProgress(message = 'Cellular Component enrichment...', value = 1, {

    genesGroups <- lapply(r_data$GenesClassDetailsForPlots, function(x)rownames(x))
    GroupsID <- lapply(genesGroups,function(x) unname(unlist(bioCancer::translate(x, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
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
      clusterProfiler::dotplot(cCC)
    }
  })
})
compareClusterCC <- function(){
  clusterProfiler::dotplot(r_data$cCC, title="Cellular Component Enrichment Comparison")
}


observeEvent(input$ClassifierHelp_report, {

  cmd1 <- paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                 paste0("get_data(r_data$GenesClassDetails, vars = NULL,
            rows = NULL, na.rm = FALSE)"),
                 "\n",
                 "\n```\n"
  )



  cmd2 <-paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0("options(scipen = 0, digits = 2)
                       if(!is.null(r_data$x))
                       barplot(r_data$x,drop=TRUE, title= 'Genes Diseases Association', showCategory=10 ,digits=2)"),
                "\n",
                "\n```\n"
  )

  cmd3 <-paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0("options(scipen = 0, digits = 2)
                       if(!is.null(r_data$cdo))
                       clusterProfiler::dotplot(r_data$cdo, title='Disease Ontology Enrichment Comparison')"),
                "\n",
                "\n```\n"
                )

  cmd4 <-paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0("options(scipen = 0, digits = 2)
                       if(!is.null(r_data$cdReactome))
                       clusterProfiler::dotplot(r_data$cdReactome, title= 'Reactome Pathway Enrichment Comparison')"),
                "\n",
                "\n```\n"
                )

  cmd5 <-paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0("options(scipen = 0, digits = 2)
                       if(!is.null(r_data$cgo))
                       clusterProfiler::dotplot(r_data$cgo, title= 'Gene Ontology Enrichment Comparison')"),
                "\n",
                "\n```\n"
                )


  cmd6 <-paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0("options(scipen = 0, digits = 2)
                       if(!is.null(r_data$ckegg))
                      clusterProfiler::dotplot(r_data$ckegg, title='KEGG Enrichment Comparison')"),
                "\n",
                "\n```\n"
                )


  cmd7 <-paste0("```{r fig.width=10.46, fig.height=5.54, dpi =72}\n",
                paste0("options(scipen = 0, digits = 2)
                       if(!is.null(r_data$cCC))
                       clusterProfiler::dotplot(r_data$cCC, title='Cellular Component Enrichment Comparison')"),
                "\n",
                "\n```\n"
                )

  update_report_fun(cmd1)
  update_report_fun(cmd2)
  update_report_fun(cmd3)
  update_report_fun(cmd4)
  update_report_fun(cmd5)
  update_report_fun(cmd6)
  update_report_fun(cmd7)
})
