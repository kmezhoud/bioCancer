output$CircomicsHowto <- renderPrint({
  cat("
      1 - Select Studies
      2 - Check availability
      3 - Load Genetic Profiles
      4 - Select Dimension
      5 - Compare colors with legend
      ")
})


### It is not necessary. le ListProfData is loaded when the output$StrListProfDataCircos <- renderPrint({}) is printed
# observe({
#   if(not_pressed(input$loadListProfDataCircosId)) return()
#   isolate({
#     shiny::withProgress(message = 'loading Profiles Data... ', value = 0.1, {
#       Sys.sleep(0.25)
#       getListProfData(panel='Circomics',input$GeneListID)
#     })
#
#   })
# })


observe({
  if (not_pressed(input$pullUserDataButtonId)) return()
  isolate({
    if('CNA' %in% input$userDataID ){
      #if(ncol(r_data[[input$UserData_CNA_id]]) -1 == length(r_data[[input$GeneListID]])){
      if(all(apply(r_data[[input$UserData_CNA_id]][-1],2, function(x)class(x)=='integer'))){
        r_info$ListProfData$CNA[['UserData']] <- r_data[[input$UserData_CNA_id]][-1] # [-1] rm first column
        # }
      }
    }
    if('mRNA' %in% input$userDataID ){
      if(all(apply(r_data[[input$UserData_mRNA_id]][-1],2, function(x)class(x)=='numeric'))){
        r_info$ListProfData$Expression[['UserData']] <- r_data[[input$UserData_mRNA_id]][-1]
      }
    }
    if('MetHM27' %in% input$userDataID ){
      if(all(apply(r_data[[input$UserData_MetHM27_id]][-1],2, function(x)class(x)=='numeric'))){
        r_info$ListProfData$Met_HM27[['UserData']] <- r_data[[input$UserData_MetHM27_id]][-1]
        r_info$ListMetData$HM27[['UserData']] <- r_data[[input$UserData_MetHM27_id]][-1]
      }
    }
    if( 'MetHM450' %in% input$userDataID2){
      if(all(apply(r_data[[input$UserData_MetHM450_id]][-1],2, function(x)class(x)=='numeric'))){
        r_info$ListProfData$Met_HM450[['UserData']] <- r_data[[input$UserData_MetHM450_id]][-1]
        r_info$ListMetData$HM450[['UserData']] <- r_data[[input$UserData_MetHM450_id]][-1]
      }
    }
    if( 'miRNA' %in% input$userDataID2){
      if(all(apply(r_data[[input$UserData_miRNA_id]][-1],2, function(x)class(x)=='numeric'))){
        r_info$ListProfData$miRNA[['UserData']] <- r_data[[input$UserData_miRNA_id]][-1]
      }
    }
    if( 'RPPA' %in% input$userDataID2){
      if(all(apply(r_data[[input$UserData_RPPA_id]][-1],2, function(x)class(x)=='numeric'))){
        r_info$ListProfData$RPPA[['UserData']] <- r_data[[input$UserData_RPPA_id]][-1]
      }
    }
    if( 'Mutation' %in% input$userDataID){
      # gene_symbol mutation_type amino_acid_change
      ## works for a list of data frame
      #if(any(sapply(r_data[[input$UserData_FreqMut_id]], function(x) sapply(names(x), function(y) grepl(paste("gene_symbol","mutation_type", "amino_acid_change", sep = "|"), y))))){
      if(length(grep('TRUE',sapply(names(r_data[[input$UserData_FreqMut_id]]), function(y) grepl(paste("gene_symbol","mutation_type", "amino_acid_change", sep = "|"), y))))==3){
        r_info$ListMutData[['UserData']] <- r_data[[input$UserData_FreqMut_id]][-1]
      }
    }
  })
})

observe({
  if (not_pressed(input$pullUserDataButtonId)) #return()
  isolate({
    r_info$ListProfData$CNA[['UserData']] <- NULL
    r_info$ListProfData$Expression[['UserData']] <- NULL
    r_info$ListProfData$Met_HM27[['UserData']] <- NULL
    r_info$ListMetData$HM27[['UserData']] <- NULL
    r_info$ListProfData$Met_HM450[['UserData']] <- NULL
    r_info$ListMetData$HM450[['UserData']] <- NULL
    r_info$ListProfData$miRNA[['UserData']] <- NULL
    r_info$ListProfData$RPPA[['UserData']] <- NULL
    r_info$ListMutData[['UserData']] <- NULL
  })
})


# output$StrProfData <- renderPrint({
#
#                    r_data$ListProfData$CNA[['UserData']] <- r_data[[input$UserData_CNA_id]]
#                    #cat("PROFILES DATA:\n",str(r_data$ListProfData$CNA),sep = " " )
#
# })
#
# output$CNATable <- DT::renderDataTable({
#   #r_data$ListProfData$CNA[['UserData']] <- r_data[[input$UserData_CNA_id]]
#
#   dat <- r_data[[input$UserData_CNA_id]]
#
#   displayTable(dat)
#
# })


## get Wheel for Profiles Data
output$getCoffeeWheel_All <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    CoffeewheelTreeProfData <- reStrDimension(r_info$ListProfData)
    r_info[['TreeListProfData']] <- CoffeewheelTreeProfData
    title<- paste("Profiles Data: CNA, Met,Exp, RPPA, miRNA")
    coffeewheel(CoffeewheelTreeProfData, width=600, height=600, partitionAttribute="value", main=title)
  })

})

## get Wheel for Methylation
output$getCoffeeWheel_Met <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    CoffeewheelTreeMetData <- reStrDimension(r_info$ListMetData)
    r_info[['TreeMetData']] <- CoffeewheelTreeMetData
    title<- paste("Methylations: HM450 and HM27")
    coffeewheel(CoffeewheelTreeMetData, width=600, height=600, main=title)
    #coffeewheel(r_info$TreeMetData, width=600, height=600, main=title)
  })

})

## get Wheel for CNA
output$getCoffeeWheel_CNA <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    CoffeewheelTreeCNAData <- reStrDisease(r_info$ListProfData$CNA)
    r_info[['TreeCNAData']] <- CoffeewheelTreeCNAData
    title<- paste("Copy Number Alteration [-2, +2]")
    coffeewheel(CoffeewheelTreeCNAData, width=600, height=600,main=title)
  })

})

## get Wheel for mRNA
output$getCoffeeWheel_mRNA <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    CoffeewheelTreeExpData <- reStrDisease(r_info$ListProfData$Expression)
    r_info[['TreeMRNAData']] <- CoffeewheelTreeExpData
    title<- paste("mRNA expression")
    coffeewheel(CoffeewheelTreeExpData, width=600, height=600, main=title)
  })

})

## get Wheel for miRNA
output$getCoffeeWheel_miRNA <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    CoffeewheelTreeMiRNAData <- reStrDisease(r_info$ListProfData$miRNA)
    r_info[['TreeMiRNAData']] <- CoffeewheelTreeMiRNAData
    title<- paste("miRNA Expression")
    coffeewheel(CoffeewheelTreeMiRNAData, width=600, height=600, main= title)
  })

})

## get Wheel for RPPA
output$getCoffeeWheel_RPPA <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    CoffeewheelTreeRPPAData <- reStrDisease(r_info$ListProfData$RPPA)
    r_info[['TreeRPPAData']] <- CoffeewheelTreeRPPAData
    title<- paste("Reverse Phase Protein Arrays")
    coffeewheel(CoffeewheelTreeRPPAData, width=600, height=600,main=title)
  })

})

## get Wheel for Mutation
output$getCoffeeWheel_Mut <- renderCoffeewheel({
  shiny::withProgress(message = 'Creating Wheel. Waiting...', value = 1, {

    ## get Gene Mutation Frequency
    print("Start getting Frequency of Mutation ...")
    Freq_DfMutData <- getFreqMutData(list = r_info$ListMutData, geneListLabel = input$GeneListID)
    r_info[['Freq_DfMutData']] <- Freq_DfMutData
    print("End getting Mutation Frequency...")
    listMut_df <- apply(r_info$Freq_DfMutData,2,function(x)as.data.frame(t(x)))
    TreeMutData <- reStrDisease(listMut_df)
    r_info[['TreeMutData']] <- TreeMutData
    coffeewheel(TreeMutData, width=1024, height=600
                #,main= paste0("Mutation Frequency: (Min = ", min(r_data$Freq_DfMutData) ,", Max = ", max(r_data$Freq_DfMutData)  ,")", sep="")
    )
  })

})


### render static circos "metabologram"
output$metabologram_All <- renderMetabologram({

  CoffeewheelTreeData <- reStrDimension(r_info$ListProfData)

  title<- paste("All genomic profiles")

  metabologram(CoffeewheelTreeData, width=600, height=600, main=title,
               showLegend = FALSE, fontSize = 8, legendBreaks=c("Down", "0", "Up", "NA"),
               legendColors=c("blue","white","red", "black") , legendText="Legend")

})


output$CircosAvailability <- DT::renderDataTable({

  shiny::withProgress(message = 'Loading Data...', value = 1, {

    dat <- checkDimensions(StudyID= input$StudiesIDCircos )
    ## remove rownames to column
    dat <- dat %>% tibble::rownames_to_column("Samples")

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

output$Sequenced_SampleSize <- DT::renderDataTable({

  shiny::withProgress(message = 'Computing Sample sizes...', value = 1, {

    dat <- getSequensed_SampleSize(StudiesID = input$StudiesIDCircos)

    DT::datatable(dat,
                  caption= "Table 1: Sample Sizes by study",
                  autoHideNavigation = getOption("DT.autoHideNavigation")
    )
  })
})

output$FreqMutSummary <- DT::renderDataTable({
  # dat <- r_data$Freq_DfMutData %>% tibble::rownames_to_column("Genes")
  # rnames <- rownames(r_data$Freq_DfMutData)
  # rownames(r_data$Freq_DfMutData) <- NULL
  # FreqIn <- as.data.frame(r_data$Freq_DfMutData)
  # dat <- cbind("Genes"= rnames, r_data$Freq_DfMutData)
  DT::datatable(r_info$Freq_DfMutData,
                caption="Table 2: Percentage (%) of mutation by gene in each study",
                autoHideNavigation = getOption("DT.autoHideNavigation")
  )
})

output$mRNA_mean <- DT::renderDataTable({
  # if (inherits(try(lapply(r_info$ListProfData$Expression, function(x) lapply(x, function(y) colMeans(y))), silent=TRUE),"try-error")){
  #   r_data$ListProfData$ListMetData[['UserData']] <- NULL
  # }
  dat <- lapply(r_info$ListProfData$Expression, function(x) colMeans(x))
  dat <- as.data.frame(do.call(cbind,dat))
  dat <- round(dat, digits = 0)
  DT::datatable(dat,
                caption="Table 2: Means of mRNA expression",
                autoHideNavigation = getOption("DT.autoHideNavigation")
  )
})
output$CNA_Max <- DT::renderDataTable({
  # check if all dataframe have interger in each column
  #if(all(sapply((lapply(r_data$ListProfData$CNA,function(x) sapply(x, function(y) class(y)=='integer'))), function(f) all()))){
  #if (inherits(try(lapply(r_data$ListProfData$CNA,function(x) apply(x,2, function(y) as.data.frame(table(y[order(y)])))), silent=TRUE),"try-error")){
  # r_data$ListProfData$CNA[['UserData']] <- NULL
  #}
  ls <- lapply(r_info$ListProfData$CNA,function(x) apply(x,2, function(y) as.data.frame(table(y[order(y)]))))
  WhichMax <- lapply(ls, function(x) as.data.frame(do.call(cbind,lapply(x, function(y) y[,1][which(y[,2]== max(y[,2]))]))))
  genes_names <- lapply(WhichMax, function(x) names(x))[1]
  names(genes_names) <- 'Genes'
  WhichMax <- lapply(WhichMax, function(x) as.numeric(as.matrix(x)))
  WhichMax <-   as.data.frame(do.call(cbind, WhichMax))  # as.data.frame(WhichMax)
  dat <- cbind(Genes= genes_names , WhichMax )
  DT::datatable(dat, rownames = FALSE,
                caption="Table 2: The most frequent CNA prolife",
                autoHideNavigation = getOption("DT.autoHideNavigation")
  )
  # }
})

output$Methylation_mean <- DT::renderDataTable({
  # if (inherits(try(lapply(r_data$ListMetData, function(x) lapply(x, function(y) colMeans(y))), silent=TRUE),"try-error")){
  # r_data$ListProfData$ListMetData[['UserData']] <- NULL
  # }
  #dat <- list(r_data$ListMetData,r_data$ListProfData$Met_HM450)
  dat <- lapply(r_info$ListMetData, function(x) lapply(x, function(y) colMeans(y)))
  dat <- as.data.frame(dat)
  dat <- round(dat, digits = 3)
  DT::datatable(dat,
                caption="Table 2: Correlation of silencing gene by Methylation: (0:1)",
                autoHideNavigation = getOption("DT.autoHideNavigation")
  )
})


## REPORT
observeEvent(input$CircomicsHelp_report, {

  cmdAll <- paste0("```{r}\n",
                 paste0("if(is.null(r_info$TreeListProfData)){
                         r_info[['TreeListProfData']] <- reStrDimension(r_info$ListProfData)
                          }
                         title<- paste('All genomics data available')
                        wdgt <- metabologram(r_info$TreeListProfData, width=600, height=600, main=title,
                         showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
                         legendColors=c('blue','white','red', 'black') , legendText='Legend')
                         wdgt
                        ") ,
                 "\n",
                 "\n```\n"
  )

  cmdCNA <- paste0("```{r}\n",
                   paste0("if(is.null(r_info$TreeCNAData)){
                         r_info[['TreeCNAData']] <- reStrDisease(r_info$ListProfData$CNA)
                          }
                         title<- paste('Gene Copy Number Alteration')
                        wdgt <- metabologram(r_info$TreeCNAData, width=600, height=600, main=title,
                         showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
                         legendColors=c('blue','white','red', 'black') , legendText='Legend')
                         wdgt
                        ") ,
                   "\n",
                   "\n```\n"
  )

  cmdMet <- paste0("```{r}\n",
                   paste0("if(is.null(r_info$TreeMetData)){
                          r_info[['TreeMetData']] <- reStrDimension(r_info$ListMetData)
}
  title<- paste('Methylation HM450, HM27')
  wdgt <- metabologram(r_info$TreeMetData, width=600, height=600, main=title,
  showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
  legendColors=c('blue','white','red', 'black') , legendText='Legend')
  wdgt
  ") ,
                   "\n",
                   "\n```\n"
                   )

  cmdMut <- paste0("```{r}\n",
                   paste0("if(is.null(r_info$TreeMutData)){
    Freq_DfMutData <- getFreqMutData(list = r_info$ListMutData, geneListLabel = input$GeneListID)
    listMut_df <- apply(Freq_DfMutData,2,function(x)as.data.frame(t(x)))
    r_info[['TreeMutData']] <- reStrDisease(listMut_df)
}
title<- paste('Mutation')
wdgt <- metabologram(r_info$TreeMutData, width=600, height=600, main=title,
showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
legendColors=c('blue','white','red', 'black') , legendText='Legend')
wdgt
") ,
                   "\n",
                   "\n```\n"
                   )

  cmdMRNA <- paste0("```{r}\n",
                   paste0("if(is.null(r_info$TreeMRNAData)){
                          r_info[['TreeMRNAData']] <- reStrDisease(r_info$ListProfData$Expression)
}
title<- paste('mRNA expression')
wdgt <- metabologram(r_info$TreeMRNAData, width=600, height=600, main=title,
showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
legendColors=c('blue','white','red', 'black') , legendText='Legend')
wdgt
") ,
                   "\n",
                   "\n```\n"
                   )

  cmdRPPA <- paste0("```{r}\n",
                   paste0("if(is.null(r_info$TreeRPPAData)){
                          r_info[['TreeRPPAData']] <- reStrDisease(r_info$ListProfData$RPPA)
}
title<- paste('RPPA')
wdgt <- metabologram(r_info$TreeRPPAData, width=600, height=600, main=title,
showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
legendColors=c('blue','white','red', 'black') , legendText='Legend')
wdgt
") ,
                   "\n",
                   "\n```\n"
                   )

  cmdMiRNA <- paste0("```{r}\n",
                   paste0("if(is.null(r_info$TreeMiRNAData)){
                          r_info[['TreeMiRNAData']] <- reStrDisease(r_info$ListProfData$miRNA)
}
title<- paste('miRNA')
wdgt <- metabologram(r_info$TreeMiRNAData, width=600, height=600, main=title,
showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
legendColors=c('blue','white','red', 'black') , legendText='Legend')
wdgt
") ,
                   "\n",
                   "\n```\n"
                   )
  update_report_fun(cmdAll)
  update_report_fun(cmdCNA)
  update_report_fun(cmdMet)
  update_report_fun(cmdMut)
  update_report_fun(cmdMRNA)
  update_report_fun(cmdRPPA)
  update_report_fun(cmdMiRNA)

})

## Save circular layouts as png

# output$dl_metabologram_All <- shiny::downloadHandler(
#   filename = function() { },
#   content = function(file) {
#
#
#
#     wdgt <- metabologram(r_data$TreeListProfData, width=600, height=600, main='title',
#                  showLegend = FALSE, fontSize = 8, legendBreaks=c('Down', '0', 'Up', 'NA'),
#                  legendColors=c('blue','white','red', 'black') , legendText='Legend')
#     # temporarily switch to the temp dir, in case you do not have write
#     # permission to the current working directory
#     #owd <- setwd(tempdir())
#     #on.exit(setwd(owd))
#
#      #widgetThumbnail(wdgt, "temp")
#     saveWidget(wdgt, file="temp.html", selfcontained = F)
#
#
#     webshot::webshot("temp.html", file = "Rplot.png",
#                      cliprect = "viewport")
#   }
# )


observeEvent(input$saveCircosAll, {
  wdgt <- metabologram(r_info$TreeListProfData, width=1024, height=1024, main='All genomic profiles',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  #owd <- setwd(tempdir())
  #on.exit(setwd(owd))
    if(Sys.info()["sysname"] == "Windows"){
      setwd(Sys.getenv("R_USER"))
    }else{
        setwd('~/')
    }

  widgetThumbnail(wdgt, "CircosAll")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosAll.png file.", sep = "  ")
  ))
  #saveWidget(wdgt, file="temp.html", selfcontained = F)

  #webshot::webshot("temp.html", file = "Rplot.png", cliprect = "viewport")
  #file.copy("Rplot.png",file,overwrite = TRUE)
  #download.file(owd , 'temp.png')
})

observeEvent(input$saveCircosMet, {
  wdgt <- metabologram(r_info$TreeMetData, width=1024, height=1024, main='Methylation',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')

  if(Sys.info()["sysname"] == "Windows"){
    setwd(Sys.getenv("R_USER"))
  }else{
    setwd('~/')
  }

  widgetThumbnail(wdgt, "CircosMet")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosMet.png file.", sep = "  ")
  ))

})

observeEvent(input$saveCircosCNA, {
  wdgt <- metabologram(r_info$TreeCNAData, width=1024, height=1024, main='Copy Number Alteration',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')

  if(Sys.info()["sysname"] == "Windows"){
    setwd(Sys.getenv("R_USER"))
  }else{
    setwd('~/')
  }

  widgetThumbnail(wdgt, "CircosCNA")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosCNA.png file.", sep = " ")
  ))

})

observeEvent(input$saveCircosMRNA, {
  wdgt <- metabologram(r_info$TreeExpData, width=1024, height=1024, main='mRNA Expression',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')

  if(Sys.info()["sysname"] == "Windows"){
    setwd(Sys.getenv("R_USER"))
  }else{
    setwd('~/')
  }

  widgetThumbnail(wdgt, "CircosMRNA")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosMRNA.png file.", sep = " ")
  ))

})

observeEvent(input$saveCircosMiRNA, {
  wdgt <- metabologram(r_info$TreeMiRNAData, width=1024, height=1024, main='Micro RNA expression',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')

  if(Sys.info()["sysname"] == "Windows"){
    setwd(Sys.getenv("R_USER"))
  }else{
    setwd('~/')
  }

  widgetThumbnail(wdgt, "CircosMiRNA")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosMiRNA.png file.", sep = " ")
  ))

})

observeEvent(input$saveCircosRPPA, {
  wdgt <- metabologram(r_info$TreeRPPAData, width=1024, height=1024, main='Reverse Phase Protein Activity',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')

  if(Sys.info()["sysname"] == "Windows"){
    setwd(Sys.getenv("R_USER"))
  }else{
    setwd('~/')
  }

  widgetThumbnail(wdgt, "CircosRPPA")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosRPPA.png file.", sep = " ")
  ))

})

observeEvent(input$saveCircosMut, {
  wdgt <- metabologram(r_info$TreeMutData, width=1024, height=1024, main='Mutation',
                       showLegend = FALSE, fontSize = 12, legendBreaks=c('Down', '0', 'Up', 'NA'),
                       legendColors=c('blue','white','red', 'black') , legendText='Legend')

  if(Sys.info()["sysname"] == "Windows"){
    setwd(Sys.getenv("R_USER"))
  }else{
    setwd('~/')
  }

  widgetThumbnail(wdgt, "CircosMut")
  showModal(modalDialog(
    size = "s",
    title = "save success : html, png",
    paste0( " The file is saved successfully in ", getwd(), " folder as CircosMut.png file.", sep = " ")
  ))

})
