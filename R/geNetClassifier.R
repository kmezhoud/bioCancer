#' get list of cases of each selected study in Classifier panel
#' @usage getList_Cases(checked_Studies)
#' @param checked_Studies checked studies
#'
#' @return listes of cases
#' @export
#'
#' @examples
#' cgds <- CGDS("http://www.cbioportal.org/")
#' listStudies <-  cgdsr::getCancerStudies(cgds)
#' \dontrun{
#' listCases <- getList_Cases(listStudies[1:3])
#'}
#'
getList_Cases <- function(checked_Studies){
  listCases <- lapply(checked_Studies, function(x) cgdsr::getCaseLists(cgds,x)[,1])
  names(listCases) <- checked_Studies
  listCases <- lapply(listCases, function(x) x[grep("v2_mrna", x)])
  listCases <- listCases[lapply(listCases,length)>0]

  return(listCases)
}


#' get list of genetic profiles of each selected study in Classifier panel
#' @usage getList_GenProfs(checked_Studies)
#' @param checked_Studies checked studies
#'
#' @return listes of genetics profiles
#' @export
#'
#' @examples
#' cgds <- CGDS("http://www.cbioportal.org/")
#' listStudies <-  cgdsr::getCancerStudies(cgds)
#' \dontrun{
#' listGenProfs <- getList_GenProfs(listStudies[1:3])
#'}
#'
getList_GenProfs <- function(checked_Studies){

  listGenProfs <- lapply(checked_Studies, function(x) cgdsr::getGeneticProfiles(cgds,x)[,1])
  names(listGenProfs) <- checked_Studies
  listGenProfs <- lapply(listGenProfs, function(x) x[grep("v2_mrna$", x)])
  listGenProfs <- listGenProfs[lapply(listGenProfs,length)>0]
  return(listGenProfs)
}


#' get genes classification
#' @usage getGenesClassification(checked_Studies, GeneList,
#'  samplesize, threshold, listGenProfs, listCases)
#' @param checked_Studies checked studies
#' @param GeneList gene list
#' @param samplesize sample size
#' @param threshold  p-value threshold
#' @param listGenProfs list of genetic profiles
#' @param listCases list of cases
#'
#' @return A table with genes classed by study
#' @export
#'
#' @examples
#' cgds <- CGDS("http://www.cbioportal.org/")
#' listStudies <-  cgdsr::getCancerStudies(cgds)
#' \dontrun{
#' checked_Stdudies <- listStudies[3:5]
#' listCases <- getList_Cases(listStudies[1:3])
#' listGenProfs <- getList_GenProfs(listStudies[1:3])
#' GeneList <- c('P53', 'IFI16', 'BRCA1')
#' samplesize <- 50
#' threshold <- 0.95
#' table <- getGenesClassification(checked_Studies, GeneList,
#' samplesize  ,threshold  ,listGenProfs, listCases)
#'}
#'
getGenesClassification <- function(checked_Studies,
                                   GeneList,
                                   samplesize,
                                   threshold,
                                   listGenProfs,
                                   listCases){
  if(length(listCases) < length(checked_Studies)){
    GenesClassDetails_df <-
      as.data.frame("Some selected study does not have mRNA data.
                    Select only study witn mRNA data.")
    return(GenesClassDetails_df)

  }else{
    SamplingProfsData <- 0
    DiseasesType <- 0
    for (s in 1:length(checked_Studies)){

      #GenProf <- input$GenProfsIDClassifier[s]
      #Case <- input$CasesIDClassifier[s]
      GenProf <- listGenProfs[s]
      Case <- listCases[s]

      if(length(GeneList)>500){
        shiny::withProgress(message = 'loading MegaProfData...', value = 0.1, {
          Sys.sleep(0.25)
          ProfData <- getMegaProfData(GeneList,GenProf,Case, Class="ProfData" )
        })
      } else{
        ProfData<- cgdsr::getProfileData(cgds,GeneList, GenProf,Case)
      }

      ProfData <- t(ProfData)
      ##remove all NAs rows
      if (inherits(try(ProfData<- ProfData[which(apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),] , silent=FALSE),"try-error"))
      {
        print("Reselect Cases and Genetic Profiles from Samples. Maybe some studies do not have mRNA data.")
      } else{
        ProfData<- ProfData[which( apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),]

      }
      if(ncol(ProfData) < input$SampleSizeClassifierID){
        msgBigSampl <- paste(checked_Studies[s], "has only", ncol(ProfData),"samples.","\nSelect at Max: ",ncol(ProfData), "samples")
        shiny::withProgress(message= msgBigSampl, value = 0.1,
                            {p1 <- proc.time()
                            Sys.sleep(2) # wait 2 seconds
                            proc.time() - p1 })

        print(msgBigSampl)
      }
      set.seed(1234)
      SamplingProfData <- t(apply(ProfData, 1,function(x)sample(x[!is.na(x)],input$SampleSizeClassifierID)))

      SamplingColnamesProfData <- sample(colnames(ProfData), input$SampleSizeClassifierID)

      colnames(SamplingProfData) <- SamplingColnamesProfData
      SamplingProfsData <- cbind.na(SamplingProfsData,SamplingProfData)
      print(paste("Sampling from ",Case))
      ##Extracting Disease Type
      DiseaseType  <- as.matrix(rep(checked_Studies[s],times=input$SampleSizeClassifierID))
      DiseasesType <- c(DiseasesType, DiseaseType)

    }
    SamplingProfsData<- SamplingProfsData[,-1]
    DiseasesType <-DiseasesType[-1]
    DiseasesType <- as.data.frame(DiseasesType)
    print("converting DiseaseType as DataFrame...")

    if (inherits(try(rownames(DiseasesType) <- colnames(SamplingProfsData) , silent=FALSE),"try-error"))
    {
      msgDuplicateSamples <- paste("Duplicate sample names are not allowed. Do no select two studies from the same disease.")
      shiny::withProgress(message= msgDuplicateSamples, value = 0.1,
                          {p1 <- proc.time()
                          Sys.sleep(2) # wait 2 seconds
                          proc.time() - p1 })
      print(msgDuplicateSamples)
    } else{
      print(paste("SamplingProfsData:", dim(SamplingProfsData)))
      print(paste("DiseasesType:", dim(DiseasesType)))
      rownames(DiseasesType) <- colnames(SamplingProfsData)
    }


    print("adding rownames to DiseasesType...")
    ## create labelDescription for columns of phenoData.
    ## labeldescription is used by Biobase packages
    ## In our case labelDescription is Equal to column names
    ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame
    metaData <- data.frame(labelDescription= "DiseasesType", row.names="DiseasesType")

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
      Biobase::exprs(eSetClassifier) <- Biobase::exprs(eSetClassifier)+(abs(min(Biobase::exprs(eSetClassifier), na.rm=TRUE)))
    }

    if (inherits(try(signGenesRank_DiseaseType<-
                     geNetClassifier::calculateGenesRanking(eSetClassifier[,1:(input$SampleSizeClassifierID*length(checked_Studies))],
                                                            sampleLabels="DiseasesType", lpThreshold= input$ClassifierThresholdID,
                                                            returnRanking="significant", plotLp = FALSE), silent=TRUE),"try-error"))
    {
      msgNoSignificantDiff <- paste("The current genes don't differentiate the classes (Cancers)..")

      shiny::withProgress(message= msgNoSignificantDiff, value = 0.1,
                          {p1 <- proc.time()
                          Sys.sleep(2) # wait 2 seconds
                          proc.time() - p1 })

      print(msgNoSignificantDiff)
    } else{

      signGenesRank_DiseaseType <-
        geNetClassifier::calculateGenesRanking(eSetClassifier[,1:(input$SampleSizeClassifierID*length(checked_Studies))],
                                               sampleLabels="DiseasesType", lpThreshold= input$ClassifierThresholdID,
                                               returnRanking="significant", plotLp = FALSE)
    }

    ## this line display the rank of postprob of all genes
    #apply(-signGenesRank_DiseaseType@postProb[,-1,drop=FALSE],2,rank, ties.method="random")

    GenesClassDetails <- geNetClassifier::genesDetails(signGenesRank_DiseaseType)
    r_data[['GenesClassDetailsForPlots']] <- GenesClassDetails
    #GenesClassDetails_bkp1 <<- GenesClassDetails

    print("getting Genes Details...")
    GenesClassDetails_ls <- lapply(GenesClassDetails, function(x) x %>% tibble::rownames_to_column("Genes"))
    GenesClassDetails_df <- plyr::ldply(GenesClassDetails_ls)
    #r_data[['GenesClassDetails']] <- GenesClassDetails_df[,-1]

    #GenesClassTab <- do.call(rbind.data.frame, GenesClassDetails)
    #GenesClassTab <- t(t(as.data.frame.matrix(GenesClassTab)))

    return(GenesClassDetails_df[,-1])
  }
}
