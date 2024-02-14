#' search and get genetic profiles (CNA,mRNA, Methylation, Mutation...)
#'
#' @details See \url{https://github.com/kmezhoud/bioCancer/wiki}
#'
#' @return A data frame with Genetic profile
#'
#' @usage getProfData(study,genProf, listGenProf, GeneList, Mut)
#' @param study  Study ID
#' @param genProf Genetic Profile id (cancer_study_id_[mutations, cna, methylation, mrna ]).
#' @param listGenProf A list of Genetic Profiles for one study.
#' @param GeneList A list of genes
#' @param Mut Condition to set if the genetic profile is mutation or not (0,1)
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
getProfData<-function(study, genProf, listGenProf, GeneList, Mut){


  if(length(grep(genProf, listGenProf))!= 0){

    ProfData_X <- cBioPortalData::getDataByGenes(api =  cgds,
                                                 studyId = study,
                                                 genes = GeneList,
                                                 by = "hugoGeneSymbol",
                                                 molecularProfileIds = genProf) |>
      unname() |>
      as.data.frame()

    # avoid error if no profile data exist for geneList
    if(nrow(ProfData_X)<1){
       #all(dim(ProfData_X)==c(0,1) ||
       #nrow(ProfData_X)==0)== TRUE){ #length(ProfData_X)== 0 ||

      #print(paste0("No Profile Data available for:", genProf))

      ## built empty data frame with gene Symbol in colnames
      if(Mut==0){

        ProfData_X <- as.data.frame(setNames(replicate(length(GeneList),numeric(1),
                                                       simplify = FALSE),
                                             GeneList[order(GeneList)]))
        return(ProfData_X)

      }else if (Mut==1){
        ## built emty data.frame as the same form of MutData
        hugoGeneSymbol <- as.vector(GeneList)
        proteinChange <- rep(character(1), length(GeneList))
        variantType <- rep(character(1), length(GeneList))
        MutData <- data.frame(hugoGeneSymbol, proteinChange, variantType)

        return(MutData)
      }

    }else if(Mut== 0){

      ProfData_X <- ProfData_X |>
        #.[[1]] |>
        select("sampleId", "hugoGeneSymbol", "value") |>
        spread("hugoGeneSymbol", "value") |>
        tibble::column_to_rownames("sampleId")
      ##  check the order of geneList and add gene with empty data NA.
      missing <- setdiff(GeneList[order(GeneList)], names(ProfData_X))
      ProfData_X[missing] <- NA
      ProfData_X |> select(GeneList[order(GeneList)])

      return(ProfData_X)

    } else if(Mut==1){

      #print(paste0("Getting Mutation Data of ", study ," ..."))

      MutData <- ProfData_X  |>
        #.[[1]] |>
        select("hugoGeneSymbol", "proteinChange", "variantType")

      return(MutData)

    }
  }else{
    shiny::withProgress(message=
                          paste("There is no genetic Profiles: ",
                                genProf ), value = 1,
                        {p1 <- proc.time()
                        Sys.sleep(2) # wait 2 seconds
                        proc.time() - p1 })
    print(paste("There is no genetic Profiles: ", genProf ))

    ProfData_X <- as.data.frame(setNames(replicate(
      length(GeneList),numeric(1), simplify = FALSE),
      GeneList[order(GeneList)]))

    return(ProfData_X)
  }
}



#' get list of data frame with profiles data (CNA,mRNA, Methylation, Mutation...)
#'
#' @usage getListProfData(checked_Studies, geneListLabel)
#'
#' @param checked_Studies checked studies in corresponding panel (input$StudiesIDCircos, input$StudiesIDReactome).
#' @param geneListLabel The label of GeneList. There are three cases:
#'        "Genes" user gene list,
#'        "Reactome_GeneList" GeneList plus genes from reactomeFI
#'       "file name" from Examples
#'
#' @return A LIST of profiles data (CNA, mRNA, Methylation, Mutation, miRNA, RPPA).
#'         Each dimension content a list of studies.
#'
#'@examples
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
#' @export
getListProfData <- function(checked_Studies, geneListLabel){

  GeneList <- whichGeneList(geneListLabel)


  ListProfData <- NULL
  r_info$ListProfData <- NULL #reinit tree for circomics
  ListMetData <- NULL
  ListMutData <- NULL
  i <- 0
  for(s in checked_Studies){
    i<-i+1
    #Si = myGlobalEnv$checked_StudyIndex[s]
     shiny::withProgress(message= paste(s,":"), value = 1, {

     incProgress(0.1, detail=paste(round((i/length(checked_Studies))*100, 0),"% of Profiles Data"))
     Sys.sleep(0.1)

    ### get Cases and Genetic Profiles  with cBioportal references
    GenProf_CNA <- paste(s,"_gistic", sep="")
    #Case_CNA   <- paste(s,"_cna", sep="")

    GenProf_Exp <- paste(s,"_rna_seq_v2_mrna", sep="")
    #Case_Exp   <- paste(s,"_rna_seq_v2_mrna", sep="")

    GenProf_Met_HM450 <- paste(s,"_methylation_hm450", sep="")
    #Case_Met_HM450   <- paste(s,"_methylation_hm450", sep="")

    GenProf_Met_HM27 <- paste(s,"_methylation_hm27", sep="")
    #Case_Met_HM27   <- paste(s,"_methylation_hm27", sep="")

    GenProf_RPPA <- paste(s,"_rppa", sep="")
    #Case_RPPA   <- paste(s,"_rppa", sep="")

    GenProf_miRNA <- paste(s,"_mirna", sep="")
    #Case_miRNA   <- paste(s,"_microrna", sep="")

    GenProf_Mut<- paste(s,"_mutations", sep="")
    #Case_Mut   <- paste(s,"_sequenced", sep="")

    ## get Genetics Profiles for selected each Study
    GenProfsRefStudies <- unname(unlist(apply(
      as.data.frame(s), 1,
      function(x) molecularProfiles(api = cgds, studyId = x)[,"molecularProfileId"])))


    ProfData_CNA <- getProfData(s, GenProf_CNA,
                                GenProfsRefStudies, GeneList, Mut=0)
    ProfData_Exp <- getProfData(s, GenProf_Exp,
                                GenProfsRefStudies, GeneList, Mut=0)
    ProfData_Met_HM450 <- getProfData(s, GenProf_Met_HM450,
                                      GenProfsRefStudies, GeneList, Mut=0)
    ProfData_Met_HM27 <- getProfData(s, GenProf_Met_HM27,
                                     GenProfsRefStudies, GeneList, Mut=0)
    ProfData_RPPA <- getProfData(s, GenProf_RPPA,
                                 GenProfsRefStudies, GeneList, Mut=0)
    ProfData_miRNA <- getProfData(s, GenProf_miRNA,
                                  GenProfsRefStudies, GeneList, Mut=0)
    MutData <- getProfData(s,GenProf_Mut,
                           GenProfsRefStudies,GeneList, Mut=1)


    ListProfData$CNA[[s]] <- ProfData_CNA
    ListProfData$Expression[[s]] <- ProfData_Exp
    ListProfData$Met_HM450[[s]] <- ProfData_Met_HM450
    ListProfData$Met_HM27[[s]] <- ProfData_Met_HM27
    ListMetData$HM450[[s]] <- ProfData_Met_HM450
    ListMetData$HM27[[s]] <- ProfData_Met_HM27
    ListProfData$RPPA[[s]] <- ProfData_RPPA
    ListProfData$miRNA[[s]] <- ProfData_miRNA
    ListMutData[[s]] <- MutData


    #print(" End Getting Profiles Data... ")
    })
  }
  r_info[['ListProfData']] <- ListProfData
  r_info[['ListMetData']] <- ListMetData
  r_info[['ListMutData']] <- ListMutData
  #}
}
