#' Classify genes by Studies using gene expression
#'
#' @return A data frame with ranking genes.
#' @export
#'
#' @examples
#' example <- "runManually"
#' \dontrun{
#'  string1 <- "https://wiki.ubuntu.com/kmezhoud/bioCancer?"
#'  string2 <- "action=AttachFile&do=get&target=ListProfData.RData"
#'  link <- curl::curl(paste0(string1,string2, sep=""))
#'  load(link)
#' ##load(paste(system.file(package="bioCancer"),"/extdata/ListProfData.RData", sep=""))
#' getGenesClassifier()
#'}
#'
#'@importFrom geNetClassifier calculateGenesRanking
#'@importFrom geNetClassifier genesDetails
#'@importFrom Biobase ExpressionSet
#'@importFrom Biobase exprs
#'@importFrom AnnotationFuncs translate
#'@importFrom org.Hs.eg.db org.Hs.egSYMBOL2EG
#'@import DOSE
#'@importFrom clusterProfiler enricher
#'@importFrom clusterProfiler plot
#'@importFrom clusterProfiler compareCluster
#'@importFrom clusterProfiler enrichGO
#'@importFrom clusterProfiler enrichKEGG
#'@import reactome.db
#'@import ReactomePA
#'@importFrom magrittr %>%
#'
getGenesClassifier <- function(){
  #
  #   withProgress(message = 'geNetClassifier is running...', value = 0.1, {
  #     Sys.sleep(0.25)

  checked_Studies <- c("brca_tcga","gbm_tcga","lihc_tcga","lusc_tcga")

  GeneList <- whichGeneList()
  #GenProfs_list <<- lapply(checked_Studies, function(x) paste(x, "_rna_seq_v2_mrna", sep=""))
  #Cases_list <<-  lapply(checked_Studies, function(x) paste(x, "_rna_seq_v2_mrna", sep=""))

  SamplesSize <- 50
  Threshold <- 0.95

  SamplingProfsData <- 0
  DiseasesType <- 0
  for (s in 1:length(checked_Studies)){

    #progressBar_ProfilesData <- tkProgressBar(title = checked_Studies[s], min = 0,
    #                                         max = length(checked_Studies), width = 400)

    #Sys.sleep(0.1)
    #setTkProgressBar(progressBar_ProfilesData, s, label=paste( round(s/length(checked_Studies*100, 0),"% of Expression Set")))

    GenProf <- paste(checked_Studies[s],"_rna_seq_v2_mrna", sep = "")
    Case    <- paste(checked_Studies[s],"_rna_seq_v2_mrna", sep = "")

    #GenProf <- input$GenProfsIDClassifier[s]
    #Case <- input$CasesIDClassifier[s]


    if(length(GeneList)>500){
      ProfData <- getMegaProfData(GeneList,GenProf,Case, Class="ProfData" )
    } else{
      ProfData<- getProfileData(cgds,GeneList, GenProf,Case)
    }

    ProfData <- t(ProfData)
    #profdata <<- ProfData
    ##remove all NAs rows
    if (inherits(try(ProfData<- ProfData[which(apply(!( apply(ProfData,1,is.na) ),2,sum)!=0 ),] , silent=FALSE),"try-error"))
    {
      stop("Reselect Cases and Genetic Profiles from Samples. It is recommanded to use v2_mrna data. ")
    } else{
      ProfData<- ProfData[which( apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),]

    }
    if(ncol(ProfData) < SamplesSize){
      msgBigSampl <- paste(checked_Studies[s], "has only", ncol(ProfData),"samples.","\nSelect at Max: ",ncol(ProfData), "samples")
      withProgress(message= msgBigSampl, value = 0.1,
                   {p1 <- proc.time()
                   Sys.sleep(2) # wait 2 seconds
                   proc.time() - p1 })

      #tkmessageBox(message=msgBigSampl, icon="info")
      # close(progressBar_ProfilesData)
      stop(msgBigSampl)
    }
    set.seed(1234)
    SamplingProfData <- t(apply(ProfData, 1,function(x)sample(x[!is.na(x)],SamplesSize)))

    SamplingColnamesProfData <- sample(colnames(ProfData), SamplesSize)

    colnames(SamplingProfData) <- SamplingColnamesProfData
    SamplingProfsData <- cbind.na(SamplingProfsData,SamplingProfData)
    print(paste("Sampling from ",Case))
    ##Extracting Disease Type
    DiseaseType  <- as.matrix(rep(checked_Studies[s],times=SamplesSize))
    DiseasesType <- c(DiseasesType, DiseaseType)

  }
  #close(progressBar_ProfilesData)


  SamplingProfsData<- SamplingProfsData[,-1]
  DiseasesType <-DiseasesType[-1]
  DiseasesType <- as.data.frame(DiseasesType)
  print("converting DiseaseType as DataFrame...")

  if (inherits(try(rownames(DiseasesType) <- colnames(SamplingProfsData) , silent=FALSE),"try-error"))
  {
    msgDuplicateSamples <- paste("Duplicate sample names are not allowed. Do no select two studies from the same disease.")
    withProgress(message= msgDuplicateSamples, value = 0.1,
                 {p1 <- proc.time()
                 Sys.sleep(2) # wait 2 seconds
                 proc.time() - p1 })
    #tkmessageBox(message=msgDuplicateSamples , icon="warning")
    #diseasesType <<- DiseasesType
    stop(msgDuplicateSamples)
  } else{
    print(paste("SamplingProfsData:", dim(SamplingProfsData)))
    print(paste("DiseasesType:", dim(DiseasesType)))
    # diseasesType <<- DiseasesType

    #rname<- colnames(SamplingProfsData)
    #attr(DiseasesType, "row.names") <- rname
    rownames(DiseasesType) <- colnames(SamplingProfsData)
  }


  print("adding rownames to DiseasesType...")
  ## create labelDescription for columns of phenoData.
  ## labeldescription is used by Biobase packages
  ## In our case labelDescription is Equal to column names
  ##metaData <- data.frame(labelDescription= colnames(ClinicalData), row.names=colnames(ClinicalData))        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame
  metaData <- data.frame(labelDescription= "DiseasesType", row.names="DiseasesType")        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame

  print("getting metaData...")
  ##that conveniently stores and manipulates
  ##the phenotypic data and its metadata in a coordinated fashion.
  phenoData<-new("AnnotatedDataFrame", data=DiseasesType, varMetadata=metaData)
  print("getting phenoData...")
  ##Assembling an ExpressionSet


  eSetClassifier <- Biobase::ExpressionSet(assayData=SamplingProfsData, phenoData=phenoData, annotation="GO")
  print("getting eSetClassifier...")
  if(min(Biobase::exprs(eSetClassifier), na.rm=TRUE)<0){
    print("There are negative values. Translating values by adding the absolute of minimum value to all matrix")
    Biobase::exprs(eSetClassifier) <- Biobase::exprs(eSetClassifier)+
      (abs(min(exprs(eSetClassifier), na.rm=TRUE)))
  }

  #r_data[['eSetClassifier']] <- eSetClassifier
  # Biobase::exprs(eSetClassifier) <- Biobase::exprs(eSetClassifier)[,-1]
  #eSetClassifier <<- eSetClassifier

  if (inherits(try(signGenesRank_DiseaseType<- geNetClassifier::calculateGenesRanking(
    eSetClassifier[,1:(SamplesSize*length(checked_Studies))],
    sampleLabels="DiseasesType", lpThreshold= Threshold,
    returnRanking="significant", plotLp = FALSE),
    silent=TRUE),"try-error"))
  {
    msgNoSignificantDiff <- paste("The current genes don't differentiate the classes (Cancers)..")

    withProgress(message= msgNoSignificantDiff, value = 0.1,
                 {p1 <- proc.time()
                 Sys.sleep(2) # wait 2 seconds
                 proc.time() - p1 })
    #tkmessageBox(message=msgNoSignificantDiff , icon="warning")

    stop(msgNoSignificantDiff )
  } else{

    signGenesRank_DiseaseType <- geNetClassifier::calculateGenesRanking(
      eSetClassifier[,1:(SamplesSize*length(checked_Studies))],
      sampleLabels="DiseasesType", lpThreshold= Threshold,
      returnRanking="significant", plotLp = FALSE)
  }


  ## this line display the rank of postprob of all genes
  #apply(-signGenesRank_DiseaseType@postProb[,-1,drop=FALSE],2,rank, ties.method="random")


  GenesClassDetails <- geNetClassifier::genesDetails(signGenesRank_DiseaseType)
  #r_data[['GenesClassDetailsForPlots']] <- GenesClassDetails
  #GenesClassDetails_bkp1 <<- GenesClassDetails

  print("getting Genes Details...")
  GenesClassDetails_ls <- lapply(GenesClassDetails, function(x) x %>% add_rownames("Genes"))
  GenesClassDetails_df <- plyr::ldply(GenesClassDetails_ls)
  #r_data[['GenesClassDetails']] <- GenesClassDetails_df[,-1]
  #GenesClassDetails_bkp <<- GenesClassDetails_df[,-1]

  #GenesClassTab <- do.call(rbind.data.frame, GenesClassDetails)
  #GenesClassTab <- t(t(as.data.frame.matrix(GenesClassTab)))

  return(GenesClassDetails_df[,-1])
  #  })
}

# output$viewTablegetGenesClassifier  <- renderTable({
#   if (is.null(input$StudiesIDClassifier))
#     return()
#
#   getGenesClassifier()
# })


# output$getGenesClassifier <- DT::renderDataTable({
#   dat <-   getGenesClassifier()
# action = DT::dataTableAjax(session, dat, rownames = FALSE)
#
# #DT::datatable(dat, filter = "top", rownames = FALSE, server = TRUE,
# DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
#               rownames = FALSE, style = "bootstrap", escape = FALSE,
#               # class = "compact",
#               options = list(
#                 ajax = list(url = action),
#                 search = list(regex = TRUE),
#                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
#                 autoWidth = TRUE,
#                 processing = FALSE,
#                 pageLength = 10,
#                 lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
#               )
# )
# })

#
# output$dl_GenesClassDetails_tab <- shiny::downloadHandler(
#   filename = function() { paste0("MutData_tab.csv") },
#   content = function(file) {
#     data_filter <- if (input$show_filter) input$data_filter else ""
#     getdata(r_data$GenesClassDetails, vars = input$ui_Mut_vars, filt = data_filter,
#             rows = NULL, na.rm = FALSE) %>%
#       write.csv(file, row.names = FALSE)
#   }
# )
