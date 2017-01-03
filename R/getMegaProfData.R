#' search and get genetic profiles (CNA,mRNA, Methylation, Mutation...) of gene list upper than 500
#'
#' @details See \url{https://github.com/kmezhoud/bioCancer/wiki}
#'
#' @return A data frame with Genetic profile
#'
#' @usage getMegaProfData(MegaGeneList, GenProf, Case, Class)
#' @param MegaGeneList A list of genes upper than 500
#' @param GenProf genetic profile reference
#' @param Case  Case reference
#' @param Class indicates the panel ProfData or Mutdata
#'
#' @examples
#' GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")
#' \dontrun{
#' cgds <- cgdsr::CGDS("http://www.cbioportal.org/public-portal/")
#' listCase_gbm_tcga_pub <- cgdsr::getCaseLists(cgds,"gbm_tcga_pub")[,1]
#' listGenProf_gbm_tcga_pub <- cgdsr::getGeneticProfiles(cgds,"gbm_tcga_pub")[,1]
#'
#' ProfData_Mut <- grepRef("gbm_tcga_pub_all", listCase_gbm_tcga_pub,
#'  "gbm_tcga_pub_mutations", listGenProf_gbm_tcga_pub, GeneList, Mut=1)
#'}
#'@export

getMegaProfData <- function(MegaGeneList,GenProf, Case, Class){


    if(is.integer(length(MegaGeneList)/500)){

      G <- lenght(MegaGeneList)/500
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
        ProfData<-getProfileData(cgds,SubMegaGeneList, GenProf,Case)
        MegaProfData <- cbind(MegaProfData, ProfData)

      }else if(Class=="MutData"){
        if (inherits(try(ProfData <- getMutationData(cgds,Case, GenProf, SubMegaGeneList), silent=FALSE),"try-error")){
          msgbadGeneList <- "There are some Gene Symbols not supported by cbioportal server"
          tkmessageBox(message=msgbadGeneList, icon="warning")

        }else{
          ProfData <- getMutationData(cgds,Case, GenProf, SubMegaGeneList)
        }
        MegaProfData <- rbind(MegaProfData, ProfData)
      }

    }

    if(Class=="ProfData"){
      MegaProfData <- MegaProfData[,-1]
    }else if (Class=="MutData"){
      MegaProfData <- MegaProfData[-1,]

    }

  return(MegaProfData)


}
