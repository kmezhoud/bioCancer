#' search and get genetic profiles (CNA,mRNA, Methylation, Mutation...)
#'
#' @details See \url{https://github.com/kmezhoud/bioCancer/wiki}
#'
#'
#'
#' @return A data frame with Genetic profile
#'
#' @usage grepRef(regex1, listRef1,regex2, listRef2, GeneList,Mut)
#' @param regex1 Case id (cancer_study_id_[mutations, cna, methylation, mrna ]).
#' @param listRef1 A list of cases for one study.
#' @param regex2 Genetic Profile id (cancer_study_id_[mutations, cna, methylation, mrna ]).
#' @param listRef2 A list of Genetic Profiles for one study.
#' @param GeneList A list of genes
#' @param Mut Condition to set if the genetic profile is mutation or not (0,1)
#'
#' @examples
#' \dontrun{
#' GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")
#' cgds <- cgdsr::CGDS("http://www.cbioportal.org/public-portal/")
#' listCase_gbm_tcga_pub <- cgdsr::getCaseLists(cgds,"gbm_tcga_pub")[,1]
#' listGenProf_gbm_tcga_pub <- cgdsr::getGeneticProfiles(cgds,"gbm_tcga_pub")[,1]
#' ProfData_Mut <- grepRef("gbm_tcga_pub_all", listCase_gbm_tcga_pub,
#'  "gbm_tcga_pub_mutations", listGenProf_gbm_tcga_pub, GeneList, Mut=1)
#'}
#'@export
#'
#'@importFrom cgdsr getCancerStudies
#'@importFrom cgdsr getProfileData
#'@importFrom cgdsr getCaseLists
#'@importFrom cgdsr getGeneticProfiles
#'@importFrom cgdsr getMutationData
#'
#'
grepRef<-function(regex1, listRef1,regex2, listRef2, GeneList,Mut){
  if(length(grep(regex1,listRef1)) != 0){
    if(length(grep(regex2,listRef2))!= 0){
      if(Mut== 0){
        #print(paste("Getting Profile Data of ",regex2,"...",sep=""))
        ProfData_X <- getProfileData(cgds,GeneList, regex2,regex1)
        ########################
        #ProfData_X <- ProfData_X[,as.factor(GeneList)]
        #ProfData_X <- ProfData_X[GeneList,,drop=FALSE]


        #                     > GeneList[c(179,306,338,400)]
        #                     [1] "HBXIP"     "C16orf5"   "SELS"      "MAGI2-IT1"
        #                     > colnames(ProfData)[c(82,198,205,404)]
        #                     [1] "CDIP1"     "LAMTOR5"   "MAGI2.IT1" "VIMP"
        #                     >
        ####################
        if(length(ProfData_X)== 0){
          if(length(GeneList) <= 500){
            ## built empty data frame with gene Symbol in colnames
            ProfData_X <- as.data.frame(setNames(replicate(length(GeneList),numeric(1), simplify = FALSE), GeneList[order(GeneList)]))
            return(ProfData_X)

          }else{
            ProfData_X <- as.data.frame(setNames(replicate(length(SubMegaGeneList),numeric(1), simplify = FALSE), SubMegaGeneList[order(SubMegaGeneList)]))
            return(ProfData_X)
          }
        }else{
          return(ProfData_X)
        }

      }else if(Mut==1){
        #print(paste("Getting Mutation Data of ",checked_Studies[s],"...",sep=""))
        MutData <- getMutationData(cgds,regex1, regex2, GeneList)
        #print(paste("MutData: ",dim(MutData)))
        if(length(MutData)==0){
          ## built emty data.frame as the same form of MutData
          MutData <- data.frame("gene_symbol"=character(1),"mutation_type"=character(1), "amino_acid_change"=character(1))
          return(MutData)
        }else{
          ## From Mut Data frame select only Gene_symbol, Mutation_Type, AA-Changes
          #myGlobalEnv$ListMutData[[checked_Studies[s]]] <- MutData[,c(2,6,8)]# Gene Symbol, Mut type, AA change
          return(MutData)
        }
        #return(MutData)

      }
    }else{
      #       withProgress(message= paste("There is no genetic Profiles: ", regex2 ), value = 0.1,
      #                    {p1 <- proc.time()
      #                    Sys.sleep(2) # wait 2 seconds
      #                    proc.time() - p1 })
      #tkmessageBox(message = paste("There is no genetic Profiles: ", regex2," for Study:",checked_Studies[s] ), icon="warning" )
      #print(paste("There is no genetic Profile: ", regex2," for Study:",checked_Studies[s],"..." ))
      ## built empty data frame with gene Symbol in colnames
      if(length(GeneList) <500){
        ProfData_X <- as.data.frame(setNames(replicate(length(GeneList),numeric(1), simplify = FALSE), GeneList[order(GeneList)]))
        return(ProfData_X)
      }else{
        ProfData_X <- as.data.frame(setNames(replicate(length(SubMegaGeneList),numeric(1), simplify = FALSE), SubMegaGeneList[order(SubMegaGeneList)]))
        return(ProfData_X)
      }
      return( ProfData_X)
    }
  }else{
    #     withProgress(message= paste("There is no Cases: ",regex1 ), value = 0.1,
    #                  {p1 <- proc.time()
    #                  Sys.sleep(2)
    #                  proc.time() - p1 })
    #tkmessageBox(message = paste("There is no Cases: ",regex1, " for Study:",checked_Studies[s] ), icon="warning" )
    #print(paste("There is no Cases: ", regex1," for Study:",checked_Studies[s],"..." ))
    ## built empty data frame with gene Symbol in colnames
    if(length(GeneList) <500){
      ProfData_X <-as.data.frame(setNames(replicate(length(GeneList),numeric(1), simplify = FALSE), GeneList[order(GeneList)]))
      return(ProfData_X)
    }else{
      ProfData_X <-as.data.frame(setNames(replicate(length(SubMegaGeneList),numeric(1), simplify = FALSE), SubMegaGeneList[order(SubMegaGeneList)]))
      return(ProfData_X)
    }
    return(ProfData_X)


  }
}




#' get list of data frame with profiles data (CNA,mRNA, Methylation, Mutation...)
#'
#' @details See \url{https://github.com/kmezhoud/bioCancer/wiki}
#'
#' @usage getListProfData(panel)
#' @param panel Panel name (string) in which Studies are selected. There are two panels ("Circomics" or "Reactome")
#'
#' @return A LIST of a list data frame. Each LIST is related to profiles data (CNA, mRNA, Methylation, Mutation, miRNA, RPPA).
#'         each list of data frame is related to studies.
#'
#'@export
#'
#'@examples
#'\dontrun{
#' load(paste(.libPaths(),"/bioCancer/extdata/ListProfData.RData", sep=""))
#' StudiesIDCircos <- c("luad_tcga_pub","blca_tcga_pub")
#' ListProfData <- getListProfData(panel= "Circomics")
#'}
#'
#'
getListProfData <- function(panel){

  #   if(input$GeneListID != "Genes"){
  #     GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  #   }

  GeneList <- c("ALK", "JAK3", "SHC3","TP53","MYC","PARP")

  ## Testing The existing  Cases and GenProf cgdsr references and getProfileData


  #   if(exists("ListProfData", envir = myGlobalEnv)){
  #     rm(ListProfData, envir=myGlobalEnv)
  #
  #   }
  #   if(exists("ListMetData", envir = myGlobalEnv)){
  #     rm(ListMetData, envir=myGlobalEnv)
  #
  #   }
  #   if(exists("ListMutData", envir = myGlobalEnv)){
  #     rm(ListMutData, envir=myGlobalEnv)
  #
  #   }

  #get Study references
  #StudiesRef <- getCancerStudies(cgds)[,1]

  #listStudies <- c("gbm_tcga_pub","brca_tcga_pub","prad_tcga_pub","ucec_tcga_pub")
  if (panel=="Circomics"){
    #checked_Studies <- input$StudiesIDCircos
    checked_Studies <- StudiesIDCircos
    Lchecked_Studies <- length(checked_Studies)
  }else if (panel=="Reactome"){
    #checked_Studies <- input$StudiesIDReactome
    checked_Studies <- StudiesIDReactome
    Lchecked_Studies <- length(checked_Studies)

  }
  ## get Cases for selected Studies
  CasesRefStudies <- unname(unlist(apply(as.data.frame(checked_Studies), 1,function(x) getCaseLists(cgds,x)[1])))
  ## ger Genetics Profiles for selected Studies
  GenProfsRefStudies <- unname(unlist(apply(as.data.frame(checked_Studies), 1,function(x) getGeneticProfiles(cgds,x)[1])))

  LengthGenProfs <- 0
  LengthCases <- 0
  ListProfData <- NULL
  ListMetData <- NULL
  ListMutData <- NULL
  for (s in 1: Lchecked_Studies){
    #Si = myGlobalEnv$checked_StudyIndex[s]
    #withProgress(message= paste(checked_Studies[s],":"), value = 0, {

    #incProgress(0.1, detail=paste(round((s/Lchecked_Studies)*100, 0),"% of Profiles Data"))
    #Sys.sleep(0.1)

    #     progressBar_ProfilesData <- tkProgressBar(title = checked_Studies[s], min = 0,
    #                                               max = Lchecked_Studies, width = 400)
    #
    #     Sys.sleep(0.1)
    #     setTkProgressBar(progressBar_ProfilesData, s, label=paste( round((s/Lchecked_Studies)*100, 0),"% of Profiles Data"))



    ### get Cases and Genetic Profiles  with cgdsr references
    GenProf_CNA<- paste(checked_Studies[s],"_gistic", sep="")
    Case_CNA   <- paste(checked_Studies[s],"_cna", sep="")

    GenProf_Exp<- paste(checked_Studies[s],"_rna_seq_v2_mrna", sep="")
    Case_Exp   <- paste(checked_Studies[s],"_rna_seq_v2_mrna", sep="")

    GenProf_Met_HM450<- paste(checked_Studies[s],"_methylation_hm450", sep="")
    Case_Met_HM450   <- paste(checked_Studies[s],"_methylation_hm450", sep="")

    GenProf_Met_HM27<- paste(checked_Studies[s],"_methylation_hm27", sep="")
    Case_Met_HM27   <- paste(checked_Studies[s],"_methylation_hm27", sep="")

    GenProf_RPPA<- paste(checked_Studies[s],"_RPPA_protein_level", sep="")
    Case_RPPA   <- paste(checked_Studies[s],"_rppa", sep="")

    GenProf_miRNA<- paste(checked_Studies[s],"_mirna", sep="")
    Case_miRNA   <- paste(checked_Studies[s],"_microrna", sep="")

    GenProf_Mut<- paste(checked_Studies[s],"_mutations", sep="")
    Case_Mut   <- paste(checked_Studies[s],"_sequenced", sep="")


    ## Subsettint of Gene List if bigger than 500
    if(length(GeneList)>500){
      MegaGeneList <- GeneList
      if(is.integer(length(MegaGeneList)/500)){
        G <- lenght(MegaGeneList)/500
      }else{
        G <- as.integer(length(MegaGeneList)/500) + 1
      }

      MegaProfData_CNA <- 0
      MegaProfData_Exp <- 0
      MegaProfData_Met_HM450 <- 0
      MegaProfData_Met_HM27 <- 0
      MegaProfData_RPPA <- 0
      MegaProfData_miRNA <- 0
      MegaMutData <- 0
      SubMegaGeneList <- 0
      LastSubMegaGeneList <- 0
      for(g in 1: G){

        if (length(MegaGeneList) - LastSubMegaGeneList > 500){
          SubMegaGeneList <- MegaGeneList[((g-1)*(500)+1):((g)*500)]
          LastSubMegaGeneList <- LastSubMegaGeneList + length(SubMegaGeneList)
          #print(paste("SubMega",g,length(SubMegaGeneList), sep=":"))
        } else{
          #print(paste("SubMegaGeneList <-MegaGeneList[",LastSubMegaGeneList, ":", length(MegaGeneList),"]"))
          SubMegaGeneList <- MegaGeneList[LastSubMegaGeneList:length(MegaGeneList)]
          #print(paste("SubMega else",g,length(SubMegaGeneList),sep=":"))
        }

        #print("*************************************************")
        #print(paste("Getting Profile Data of Genes from: ", (((g-1)*500)+1), "to",((g-1)*500)+length(SubMegaGeneList),"of", checked_Studies[s],sep= " "))
        #print("*************************************************")

        ProfData_CNA<-grepRef(Case_CNA, CasesRefStudies, GenProf_CNA, GenProfsRefStudies, SubMegaGeneList, Mut=0)
        ProfData_Exp<-grepRef(Case_Exp, CasesRefStudies, GenProf_Exp, GenProfsRefStudies, SubMegaGeneList, Mut=0)
        ProfData_Met_HM450<-grepRef(Case_Met_HM450, CasesRefStudies, GenProf_Met_HM450, GenProfsRefStudies, SubMegaGeneList, Mut=0)
        #print(paste("SubMega:",length(SubMegaGeneList)))
        ProfData_Met_HM27<-grepRef(Case_Met_HM27, CasesRefStudies, GenProf_Met_HM27, GenProfsRefStudies, SubMegaGeneList, Mut=0)
        ProfData_RPPA<-grepRef(Case_RPPA, CasesRefStudies, GenProf_RPPA, GenProfsRefStudies, SubMegaGeneList, Mut=0)
        ProfData_miRNA<-grepRef(Case_miRNA, CasesRefStudies, GenProf_miRNA, GenProfsRefStudies, SubMegaGeneList, Mut=0)
        MutData <- grepRef(Case_Mut,CasesRefStudies ,GenProf_Mut, GenProfsRefStudies,SubMegaGeneList, Mut=1)


        MegaProfData_CNA <- cbind(MegaProfData_CNA, ProfData_CNA)

        MegaProfData_Exp <- cbind(MegaProfData_Exp, ProfData_Exp)

        MegaProfData_Met_HM450 <- cbind(MegaProfData_Met_HM450, ProfData_Met_HM450)

        MegaProfData_Met_HM27 <- cbind(MegaProfData_Met_HM27, ProfData_Met_HM27)

        MegaProfData_RPPA <- cbind(MegaProfData_RPPA, ProfData_RPPA)

        MegaProfData_miRNA <- cbind(MegaProfData_miRNA, ProfData_miRNA)
        MegaMutData <- rbind(MegaMutData, MutData)

      }
      ProfData_CNA <- MegaProfData_CNA[,-1]
      ProfData_Exp <- MegaProfData_Exp[,-1]
      ProfData_Met_HM450 <- MegaProfData_Met_HM450[,-1]
      ProfData_Met_HM27 <- MegaProfData_Met_HM27[,-1]
      ProfData_RPPA <- MegaProfData_RPPA[,-1]
      ProfData_miRNA <- MegaProfData_miRNA[,-1]
      MutData <- MegaMutData[-1,]


    } else if (length(GeneList) > 0){


      ProfData_CNA<- grepRef(Case_CNA, CasesRefStudies, GenProf_CNA, GenProfsRefStudies, GeneList, Mut=0)
      ProfData_Exp<- grepRef(Case_Exp, CasesRefStudies, GenProf_Exp, GenProfsRefStudies, GeneList, Mut=0)
      ProfData_Met_HM450 <- grepRef(Case_Met_HM450, CasesRefStudies, GenProf_Met_HM450, GenProfsRefStudies, GeneList, Mut=0)
      ProfData_Met_HM27 <- grepRef(Case_Met_HM27, CasesRefStudies, GenProf_Met_HM27, GenProfsRefStudies, GeneList,Mut=0)
      ProfData_RPPA<- grepRef(Case_RPPA, CasesRefStudies, GenProf_RPPA, GenProfsRefStudies, GeneList,Mut=0)
      ProfData_miRNA<- grepRef(Case_miRNA, CasesRefStudies, GenProf_miRNA, GenProfsRefStudies, GeneList,Mut=0)
      MutData <- grepRef(Case_Mut,CasesRefStudies ,GenProf_Mut, GenProfsRefStudies,GeneList, Mut=1)

    } else {
      #tkmessageBox(message= "Load gene List", icon="warning")
      #close(progressBar_ProfilesData)
      stop("Load Gene List")
    }


    ListProfData$CNA[[checked_Studies[s]]] <- ProfData_CNA
    ListProfData$Expression[[checked_Studies[s]]] <- ProfData_Exp
    ListProfData$Met_HM450[[checked_Studies[s]]] <- ProfData_Met_HM450
    ListProfData$Met_HM27[[checked_Studies[s]]] <- ProfData_Met_HM27
    ListMetData$HM450[[checked_Studies[s]]] <- ProfData_Met_HM450
    ListMetData$HM27[[checked_Studies[s]]] <- ProfData_Met_HM27
    ListProfData$RPPA[[checked_Studies[s]]] <- ProfData_RPPA
    ListProfData$miRNA[[checked_Studies[s]]] <- ProfData_miRNA
    ListMutData[[checked_Studies[s]]] <- MutData

    #print(" End Getting Profiles Data... ")
    #close(progressBar_ProfilesData)
    #})
  }
  #   r_data[['ListProfData']] <- ListProfData
  #   r_data[['ListMetData']] <- ListMetData
  #   r_data[['ListMutData']] <- ListMutData

  ListProfData[['Mut']] <- ListMutData

  return(ListProfData)

}
