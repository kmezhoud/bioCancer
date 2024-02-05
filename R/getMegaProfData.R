#' Search and get genetic profiles (CNA,mRNA, Methylation, Mutation...) of gene list upper than 500
#'
#' @details See \url{https://github.com/kmezhoud/bioCancer/wiki}
#'
#' @return A data frame with Genetic profile
#'
#' @usage getMegaProfData(MegaGeneList, Study, GenProf, Case, Class)
#' @param MegaGeneList A list of genes upper than 500
#' @param Study Study ID
#' @param GenProf genetic profile reference
#' @param Case  Case reference
#' @param Class indicates the panel ProfData or Mutdata
#'
#' @examples
#' cgds <- cBioPortal(
#' hostname = "www.cbioportal.org",
#' protocol = "https",
#' api = "/api/v2/api-docs"
#' )
#' \dontrun{
#' getDataByGenes( api =  cgds,
#' studyId = "gbm_tcga_pub",
#' genes = c("NF1", "TP53", "ABL1"),
#' by = "hugoGeneSymbol",
#' molecularProfileIds = "gbm_tcga_pub_mrna"
#' )
#'}
#'@export
getMegaProfData <- function(MegaGeneList, Study, GenProf, Case, Class){


  if(is.integer(length(MegaGeneList)/500)){

    G <- length(MegaGeneList)/500
  }else{
    G <- as.integer(length(MegaGeneList)/500) + 1

  }
  # if(input=="Class"){
  #   GenProf <- input$GenProfsIDClassifier[s]
  #   Case <- input$CasesIDClassifier[s]
  # }else if(input=="ProfData"){
  #   GenProf <- input$GenProfID
  #   Case <- input$CasesID
  #
  # }

  MegaProfData <- 0
  SubMegaGeneList <- 0
  for(g in 1: G){

    if (length(MegaGeneList) - length(SubMegaGeneList) > 500){
      SubMegaGeneList <- MegaGeneList[(((g-1)*500)+1):((g)*500)]
    } else{
      SubMegaGeneList <- MegaGeneList[(((g-1)*500)+1):length(MegaGeneList) ]
    }


    print(paste("Getting Profile Data of Genes from: ", (((g-1)*500)+1), "to",((g)*500), sep= " "))

    if(Class=="ProfData"){
      #ProfData<-getProfileData.CGDS(cgds,SubMegaGeneList, GenProf,Case)

      ProfData <- cBioPortalData::getDataByGenes(api =  cgds, studyId = Study,
                                            genes = SubMegaGeneList, by = "hugoGeneSymbol",
                                            molecularProfileIds = GenProf)%>%
        .[[1]] |>
        select(-c("uniqueSampleKey", "uniquePatientKey", "molecularProfileId", "sampleId", "studyId"))

      MegaProfData <- cbind(MegaProfData, ProfData)

    }else if(Class=="MutData"){
      if (inherits(try(#ProfData <-  getMutationData.CGDS(cgds,Case, GenProf, SubMegaGeneList)
                       ProfData <- getDataByGenes(
                                   api = cgds,
                                   studyId = Study,
                                   genes = SubMegaGeneList,
                                   by = "hugoGeneSymbol",
                                   molecularProfileIds = GenProf
                       ) , silent=FALSE),"try-error")){
        msgbadGeneList <- "There are some Gene Symbols not supported by cbioportal server"
        #tkmessageBox(message=msgbadGeneList, icon="warning")

      }else{
        #ProfData <- getMutationData.CGDS(cgds,Case, GenProf, SubMegaGeneList)
        ProfData <- getDataByGenes(api = cgds,
                                    studyId = Study,
                                    genes = SubMegaGeneList,
                                    by = "hugoGeneSymbol",
                                    molecularProfileIds = GenProf) %>%
          .[[1]] |>
          select(-c("uniqueSampleKey", "uniquePatientKey", "molecularProfileId", "sampleId", "studyId"))
      }
      MegaProfData <- rbind(MegaProfData, ProfData)
    }

  }

  if(Class=="ProfData"){
    MegaProfData <- MegaProfData[,-1]
  }else if (Class=="MutData"){
    MegaProfData <- MegaProfData[-1,]

  }
  MegaProfData <- round(MegaProfData, digits = 3)

  return(MegaProfData)


}
