getMegaProfData <- function(MegaGeneList,GenProf, Case, Class){

  withProgress(message = 'loading MegaProfData...', value = 0.1, {
    Sys.sleep(0.25)

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

  })
  return(MegaProfData)


}
