output$CircomicsHowto <- renderPrint({
  cat("
      1 - Select Studies
      2 - Check availability
      3 - Load Genetic Profiles
      4 - Select Dimension
      5 - Compare colors with legend
      ")
})

####### Functions to get tree for coffeeWheel
attriColorValue <- function(Value, df, colors=c(a,b,c, d,e),feet){

  #df <- df *100
  df[is.na(df)] <- 0
  if(max(df,na.rm=TRUE)<1){
    ## for Methylation df
    Min <- 0
    Max <- 1
  }else{
    df <- round(df, digits = 0)
    Max <- max(df, na.rm=TRUE)
    Min <- min(df, na.rm=TRUE)
  }
  my.colors <- colorRampPalette(colors)
  color.df<-data.frame(COLOR_VALUE=seq(Min,Max,feet), color.name=my.colors(length(seq(Min,Max,feet)))) #generates Max-Min colors from the color ramp
  colorRef <- color.df[which(color.df[,1]==Value),2]
  return(colorRef)
}

attriColorGene <- function(df){
  ## check Mutation df Old
  #if(any(df[1,]%%1==0, na.rm = TRUE)&& any(df[1,]>0, na.rm = TRUE)){
  #   if(any(colnames(df) %in% input$StudieID) && any(df[1,]>0, na.rm = TRUE)){
  #     print(head(df))
  #     dfMeansOrCNA <- df
  #   }else
  ## check df is for CNA levels(-2,-1,0,1,2)
  if(all(apply(df,2, function(x)class(x)=='integer'))==TRUE
     #any(df[1,]=="-2", na.rm=TRUE)||
     #any(df[1,]=="-1", na.rm=TRUE)
     #any(df[1,]=="0", rn.rm=TRUE)||
     #any(df[1,]=="1", na.rm=TRUE)||
     #any(df[1,]=="2", na.rm=TRUE)
  ){

    ListFreqCNA <- apply(df, 2, function(x) as.data.frame(table(x[order(x)])))
    print("getting the most frequent CNA categorie...")
    dfMeansOrCNA <- as.data.frame(lapply(ListFreqCNA, function(x) x[,1][which(x[,2]== max(x[,2]))]))

    ## at this step the dfMeansOtCNA is not as numeric
    namedfMeansOrCNA <- names(dfMeansOrCNA)
    dfMeansOrCNA <- as.numeric(as.matrix(dfMeansOrCNA))
    names(dfMeansOrCNA) <- namedfMeansOrCNA

  }else if(all(apply(df,2, function(x)class(x)=='numeric'))==TRUE){
    ## Compute mean of FreqMutData and mRNA Expression
    dfMeansOrCNA<-apply(df,2,function(x) mean(x, na.rm=TRUE))
    dfMeansOrCNA <- round(dfMeansOrCNA, digits = 0)
  }

  ## Set colors if all value are 0 set only black
  if(all(dfMeansOrCNA=="0")||all(dfMeansOrCNA=="NaN")){
    colorls <- lapply(dfMeansOrCNA, function(x) attriColorValue(x, dfMeansOrCNA, colors=c("white"), feet=0.1))
    print("setting black color for empty data...")
  }else{
    colorls <- lapply(dfMeansOrCNA, function(x) attriColorValue(x, dfMeansOrCNA, colors=c("blue3","cyan3","white","yellow","red"), feet=0.1))
  }
  return(colorls)
}


reStrColorGene <- function(df){
  colorls <- attriColorGene(df)
  ## extract disease name
  disease_name <- unlist(lapply(strsplit(capture.output(substitute(df)), "\\$"),tail, 1))
  Children <- list(name=disease_name,unname(mapply(function(genes, color){list(list(name=genes, colour= color))},names(colorls), colorls)))
  names(Children)[2] <- "children"
  return(Children)

}
## this function restructure the Diseases
reStrDisease <- function(List){
  print("restructuring Selected Diseases...")
  child<-lapply(List, function(x)reStrColorGene(x))
  for(i in 1: length(names(child))){
    child[[i]]$name <- names(child)[i]
  }
  child <-unname(child)
  return(child)
}

## This function restructure the Dimensions
reStrDimension <- function(LIST){
  print("restructuring Dimensions...")
  Parent <- lapply(LIST, function(x)list(name="Dimension",children=reStrDisease(x)))
  for(i in 1: length(names(Parent))){
    Parent[[i]]$name <- names(Parent)[i]
  }
  Parent <- unname(Parent)
  return(Parent)
}


## get Wheel for Profiles Data
output$getCoffeeWheel_All <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    #getListProfData(panel="Circomics")
    #Shiny.unbindAll()
    CoffeewheelTreeProfData <- reStrDimension(r_data$ListProfData)
    title<- paste("Profiles Data: CNA, Met,Exp, RPPA, miRNA")
    coffeewheel(CoffeewheelTreeProfData, width=600, height=600, partitionAttribute="value", main=title)
  })

})

## get Wheel for Methylation
output$getCoffeeWheel_Met <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    #getListProfData()
    CoffeewheelTreeMetData <- reStrDimension(r_data$ListMetData)
    title<- paste("Methylations: HM450 and HM27")
    coffeewheel(CoffeewheelTreeMetData, width=600, height=600, main=title)
  })

})

## get Wheel for CNA
output$getCoffeeWheel_CNA <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    #getListProfData()
    CoffeewheelTreeMetData <- reStrDisease(r_data$ListProfData$CNA)
    title<- paste("Copy Number Alteration [-2, +2]")
    coffeewheel(CoffeewheelTreeMetData, width=600, height=600,main=title)
  })

})

## get Wheel for mRNA
output$getCoffeeWheel_mRNA <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    #getListProfData()
    CoffeewheelTreeMetData <- reStrDisease(r_data$ListProfData$Expression)
    title<- paste("mRNA expression")
    coffeewheel(CoffeewheelTreeMetData, width=600, height=600, main=title)
  })

})

## get Wheel for miRNA
output$getCoffeeWheel_miRNA <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    #getListProfData()
    CoffeewheelTreeMetData <- reStrDisease(r_data$ListProfData$miRNA)
    title<- paste("miRNA Expression")
    coffeewheel(CoffeewheelTreeMetData, width=600, height=600, main= title)
  })

})

## get Wheel for RPPA
output$getCoffeeWheel_RPPA <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    #getListProfData()
    CoffeewheelTreeMetData <- reStrDisease(r_data$ListProfData$RPPA)
    title<- paste("Reverse Phase Protein Arrays")
    coffeewheel(CoffeewheelTreeMetData, width=600, height=600,main=title)
  })

})


UnifyRowNames <- function(x, GeneList){
  df_MutData <-as.data.frame(table(x$gene_symbol)/sum(table(x$gene_symbol))*100)
  rownames(df_MutData) <- df_MutData$Var1
  ## ordering genes in MutData as in GeneList

  df_GeneList <- as.data.frame(t(GeneList))
  #df_GeneList <- as.data.frame(GeneList)
  rownames(df_GeneList) <- df_GeneList[,1]
  df_merge <- merge(df_GeneList, df_MutData, by="row.names",all.x=TRUE)
  Freq_Mut <- df_merge[,c(-2,-3)]
  return(Freq_Mut)
}


getFreqMutData <- function(list){
  #     if(input$GeneListID != "Genes"){
  #       GeneList <- t(unique(read.table(paste0(getwd(),"/data/GeneList/",input$GeneListID,".txt" ,sep=""))))
  #     }

    GeneList <- whichGeneList()

  if(is.null(list)){stop("Select a less one Study.")}

  Freq_ListMutData <- lapply(list,function(x) UnifyRowNames(x, GeneList))
  #output1 <- adply(Freq_ListMutData,1)



  ## convert the list of correlation matrices to Array
  Freq_ArrayMutData <- array(unlist( Freq_ListMutData), dim = c(nrow(Freq_ListMutData[[1]]), ncol( Freq_ListMutData[[1]]), length(Freq_ListMutData)))

  if (inherits(try(dimnames(Freq_ArrayMutData) <- list(Freq_ListMutData[[1]][,1], colnames(Freq_ListMutData[[1]]), names(Freq_ListMutData)), silent=TRUE),"try-error")){
    stop("There is a Study without Mutation Data. Use Mutation Panel to verify mutations data for selected studies.")
  }else{

  dimnames(Freq_ArrayMutData) <- list(Freq_ListMutData[[1]][,1], colnames(Freq_ListMutData[[1]]), names(Freq_ListMutData))
}
  # Freq_ArrayMutData_bkp <<- Freq_ArrayMutData

#if(length(input$StudiesIDReactome) < 2){
#  stop("Select more than one Study or use Mutation panel")
#}else{
  Freq_DfMutData <- apply(Freq_ArrayMutData[,2,],2,as.numeric)
  rownames(Freq_DfMutData) <- rownames(Freq_ArrayMutData[,2,])
  ## ordering gene list as in GeneList from MSigDB: grouping genes with the same biological process or gene Sets

  Freq_DfMutData <- Freq_DfMutData[GeneList,,drop=FALSE]
  Freq_DfMutData <- data.frame(round(Freq_DfMutData,digits=2))

  r_data[['Freq_DfMutData']] <- Freq_DfMutData
  # Freq_DfMutData_bkp <<-  Freq_DfMutData
  return(Freq_DfMutData)
#}
}


## get Wheel for Mutation
output$getCoffeeWheel_Mut <- renderCoffeewheel({
  withProgress(message = 'Creating Wheel. Waiting...', value = 0.1, {
    Sys.sleep(0.25)

    ## get Gene Mutation Frequency
    print("Start getting Frequency of Mutation ...")
    Freq_DfMutData <- getFreqMutData(list = r_data$ListMutData)
    print("End getting Mutation Frequency...")
    listMut_df <- apply(Freq_DfMutData,2,function(x)as.data.frame(t(x)))
    TreeMutData <- reStrDisease(listMut_df)
    coffeewheel(TreeMutData, width=600, height=600, main="Mutation Frequency: (Min,Max)")
  })

})


output$Save_Metabologram_All <- downloadHandler(
  filename = function() {
    paste(getwd(),"Metabologram_All.pdf", sep="/")
  },
  content = function(file) {
    #png(file)
    saveWidget(
    #file.copy(
   # pdf(file=file, width=12, height=8)
    #SaveMetabologram_All()
    #dev.off()
      #metabologramOutput('metabologram_All')
      CoffeewheelTreeProfData <- reStrDimension(r_data$ListProfData),
      title<- paste("Profiles Data: CNA, Met,Exp, RPPA, miRNA"),
      #coffeewheel(CoffeewheelTreeProfData, width=600, height=600, partitionAttribute="value", main=title)
      metabologram::metabologram(CoffeewheelTreeProfData, width=600, height=600, main=title,
                   showLegend = TRUE, fontSize = 10, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"),
                   legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")
      , file)
      #dev.off()
  }
)

SaveMetabologram_All <- reactive({
  CoffeewheelTreeProfData <- reStrDimension(r_data$ListProfData)
  title<- paste("Profiles Data: CNA, Met,Exp, RPPA, miRNA")
  metabologram(CoffeewheelTreeProfData, width=600, height=600, main=title, showLegend = TRUE, fontSize = 10, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"), legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")

})

# output$dl_metabologram <- downloadHandler(
#   filename='plot.png',
#   content= getmetabologram()
# )

# output$metabologram_All <- renderMetabologram({
#
#   CoffeewheelTreeData <- reStrDimension(r_data$ListProfData)
#
#   ### get Legend for static coffewheel
#   #devtools::install_github("armish/metabologram")
#   #library("metabologram")
#   title<- paste("Wheel with selected Studies")
#   metabologram(CoffeewheelTreeData, width=600, height=600, main=title,
#                showLegend = TRUE, fontSize = 10, legendBreaks=c("NA","Min","Negative", "0", "Positive", "Max"),
#                legendColors=c("black","blue","cyan","white","yellow","red") , legendText="Legend")
# })

## CNA, mRNA, methylation HM450/HM27, miRNA, RPPPA, Mutation
checkDimensions<- function(panel){

  if(panel == "Circomics"){
    checked_Studies <- input$StudiesIDCircos
    # get Cases for selected Studies
    CasesRefStudies <- unname(unlist(apply(as.data.frame(input$StudiesIDCircos), 1,function(x) getCaseLists(cgds,x)[1])))
    ## ger Genetics Profiles for selected Studies
    GenProfsRefStudies <- unname(unlist(apply(as.data.frame(input$StudiesIDCircos), 1,function(x) getGeneticProfiles(cgds,x)[1])))

  }else if (panel== "Reactome"){

    checked_Studies <- input$StudiesIDReactome
    # get Cases for selected Studies
    CasesRefStudies <- unname(unlist(apply(as.data.frame(input$StudiesIDReactome), 1,function(x) getCaseLists(cgds,x)[1])))
    ## ger Genetics Profiles for selected Studies
    GenProfsRefStudies <- unname(unlist(apply(as.data.frame(input$StudiesIDReactome), 1,function(x) getGeneticProfiles(cgds,x)[1])))

  }

  df <- data.frame(row.names = c("Case_CNA", "GenProf_GISTIC", "Case_mRNA", "GenProf_mRNA", "Case_Met_HM450", "GenProf_Met_HM450",
                                 "Case_Met_HM27", "GenProf_Met_HM27", "Case_RPPA", "GeneProf_RPPA", "Case_miRNA", "GenProf_miRNA",
                                 "Case_Mut","GeneProf_Mut"
  ) )

  for(i in 1: length(checked_Studies)){
    ### get Cases and Genetic Profiles  with cgdsr references
    GenProf_CNA<- paste(checked_Studies[i],"_gistic", sep="")
    Case_CNA   <- paste(checked_Studies[i],"_cna", sep="")

    GenProf_Exp<- paste(checked_Studies[i],"_rna_seq_v2_mrna", sep="")
    Case_Exp   <- paste(checked_Studies[i],"_rna_seq_v2_mrna", sep="")

    GenProf_Met_HM450<- paste(checked_Studies[i],"_methylation_hm450", sep="")
    Case_Met_HM450   <- paste(checked_Studies[i],"_methylation_hm450", sep="")

    GenProf_Met_HM27<- paste(checked_Studies[i],"_methylation_hm27", sep="")
    Case_Met_HM27   <- paste(checked_Studies[i],"_methylation_hm27", sep="")

    GenProf_RPPA<- paste(checked_Studies[i],"_RPPA_protein_level", sep="")
    Case_RPPA   <- paste(checked_Studies[i],"_rppa", sep="")

    GenProf_miRNA<- paste(checked_Studies[i],"_mirna", sep="")
    Case_miRNA   <- paste(checked_Studies[i],"_microrna", sep="")

    GenProf_Mut<- paste(checked_Studies[i],"_mutations", sep="")
    Case_Mut   <- paste(checked_Studies[i],"_sequenced", sep="")

    #c(df,checked_Studies[i]==0)


    if(length(grep(Case_CNA, CasesRefStudies)!=0)){
      df[1,i] <- "Yes"
    }else{
      df[1,i] <- "No"
    }

    if(length(grep(GenProf_CNA, GenProfsRefStudies)!=0)){
      df[2,i] <- "Yes"
    }else{
      df[2,i] <- "No"
    }


    if(length(grep(Case_Exp, CasesRefStudies)!=0)){
      df[3,i] <- "Yes"
    }else{
      df[3,i] <- "No"
    }

    if(length(grep(GenProf_Exp, GenProfsRefStudies)!=0)){
      df[4,i] <- "Yes"
    }else{
      df[4,i] <- "No"
    }


    if(length(grep(Case_Met_HM450, CasesRefStudies)!=0)){
      df[5,i] <- "Yes"
    }else{
      df[5,i] <- "No"
    }

    if(length(grep(GenProf_Met_HM450, GenProfsRefStudies)!=0)){
      df[6,i] <- "Yes"
    }else{
      df[6,i] <- "No"
    }


    if(length(grep(Case_Met_HM27, CasesRefStudies)!=0)){
      df[7,i] <- "Yes"
    }else{
      df[7,i] <- "No"
    }

    if(length(grep(GenProf_Met_HM27, GenProfsRefStudies)!=0)){
      df[8,i] <- "Yes"
    }else{
      df[8,i] <- "No"
    }


    if(length(grep(Case_RPPA, CasesRefStudies)!=0)){
      df[9,i] <- "Yes"
    }else{
      df[9,i] <- "No"
    }

    if(length(grep(GenProf_RPPA, GenProfsRefStudies)!=0)){
      df[10,i] <- "Yes"
    }else{
      df[10,i] <- "No"
    }


    if(length(grep(Case_miRNA, CasesRefStudies)!=0)){
      df[11,i] <- "Yes"
    }else{
      df[11,i] <- "No"
    }

    if(length(grep(GenProf_miRNA, GenProfsRefStudies)!=0)){
      df[12,i] <- "Yes"
    }else{
      df[12,i] <- "No"
    }

    if(length(grep(Case_Mut, CasesRefStudies)!=0)){
      df[13,i] <- "Yes"
    }else{
      df[13,i] <- "No"
    }

    if(length(grep(GenProf_Mut, GenProfsRefStudies)!=0)){
      df[14,i] <- "Yes"
    }else{
      df[14,i] <- "No"
    }


  }
  names(df)<- checked_Studies
  return(df)
}



output$CircosAvailability <- DT::renderDataTable({

  withProgress(message = 'Loading Data...', value = 0.1, {
    Sys.sleep(0.25)
    dat <- checkDimensions(panel="Circomics")
    ## remove rownames to column
    dat <- dat %>% add_rownames("Samples")

    # action = DT::dataTableAjax(session, dat, rownames = FALSE, toJSONfun = my_dataTablesJSON)
    action = DT::dataTableAjax(session, dat, rownames = FALSE)

    DT::datatable(dat, filter = list(position = "top", clear = FALSE, plain = TRUE),
                  rownames = FALSE, style = "bootstrap", escape = FALSE,
                  # class = "compact",
                  options = list(
                    ajax = list(url = action),
                    search = list(regex = TRUE),
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    autoWidth = TRUE,
                    processing = FALSE,
                    pageLength = 14,
                    lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
                  )
    )%>%  DT::formatStyle(names(dat),
                      color = DT::styleEqual("No", 'red'))#, backgroundColor = 'white', fontWeight = 'bold'




  })
})

