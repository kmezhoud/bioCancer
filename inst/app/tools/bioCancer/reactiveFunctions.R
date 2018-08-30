################# Load dataframe (Clinical data, Profile Data, ...) to Workspace
loadInDatasets <- function(fname, header= TRUE){

  objname <- fname

  if(fname=="ProfData"){
    GeneList <- whichGeneList(input$GeneListID)
    dat <- as.data.frame(cgdsr::getProfileData(cgds, GeneList, input$GenProfID,input$CasesID))
    r_data[[objname]] <- dat %>% tibble::rownames_to_column("Patients")
    #r_data[[objname]] <- data.frame(r_data[[objname]] , stringsAsFactors = TRUE)

  }else if (fname=="ClinicalData"){

    ## load only selected column
    # dat <- get_data(r_data$ClinicalData[input$ClinicalDataTable_rows_all,],
    #       vars = input$Clinical_varsID, na.rm = FALSE)
    # r_data[[objname]] <- dat

    ## load all clinical data
    dat <- as.data.frame(cgdsr::getClinicalData(cgds, input$CasesID))
    r_data[[objname]] <- dat %>% tibble::rownames_to_column("Patients")

  }else if (fname=="MutData"){
    GeneList <- whichGeneList(input$GeneListID)
    dat <- as.data.frame((cgdsr::getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)))
    r_data[[objname]] <- dat %>% tibble::rownames_to_column("Patients")
  } else if (fname=="xCNA"){
    dat <- plyr::ldply(r_info$ListProfData$CNA)
    r_data[[objname]] <- dat
  } else if(fname =="xmRNA"){
    dat <- plyr::ldply(r_info$ListProfData$Expression)
    r_data[[objname]] <- dat
  }else if(fname == "xMetHM450"){
    dat <- plyr::ldply(r_info$ListProfData$Met_HM450)
    r_data[[objname]] <- dat
  }else if(fname== "xMetHM27"){
    dat <- plyr::ldply(r_info$ListProfData$Met_HM27)
    r_data[[objname]] <- dat
  }else if (fname=="xMut"){
    dat <- plyr::ldply(r_info$ListMutData)
    r_data[[objname]] <- dat
  } else if(fname== "xFreqMut"){
    dat <- r_info$Freq_DfMutData
    r_data[[objname]] <- dat %>% tibble::rownames_to_column("Genes")
  }else if (fname== "xmiRNA"){
    dat <- plyr::ldply(r_info$ListProfData$miRNA)
    r_data[[objname]] <- dat
  }else if (fname== "xRPPA"){
    dat <- plyr::ldply(r_info$ListProfData$RPPA)
    r_data[[objname]] <- dat
  }
  r_info[[paste0(objname,"_descr")]] <- attr(r_info[[objname]], "description")
  #r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
  r_info[['datasetlist']] <- c(objname,r_info[['datasetlist']]) %>% unique
}






loadUserData <- function(fname, uFile, ext,
                         .csv = FALSE,
                         header = TRUE,
                         man_str_as_factor = TRUE,
                         sep = ",",
                         dec = ".") {

  filename <- basename(fname)

  fext <- tools::file_ext(filename) %>% tolower

  ## switch extension if needed
  if (fext == "rds" && ext == "rda") ext <- "rds"
  if (fext == "rda" && ext == "rds") ext <- "rda"

  ## objname is used as the name of the data.frame
  objname <- sub(paste0(".",ext,"$"),"", filename)

  ## if ext isn't in the filename nothing was replaced and so ...
  if (objname == filename) {
    if (fext %in% c("xls","xlsx")) {
      ret <- "### bioCancer does not load xls files directly. Please save the data as a csv file and try again."
    } else {
      ret <- paste0("### The filename extension (",fext,") does not match the specified file-type (",ext,").
                    Please specify the file type you are trying to upload (i.e., csv or rda).")
    }

    upload_error_handler(objname,ret)
    ext <- "---"
  }

  if (ext == 'rda') {
    ## objname will hold the name of the object(s) inside the R datafile
    robjname <- try(load(uFile), silent = TRUE)
    if (is(robjname, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in rda format.")
    } else if (length(robjname) > 1) {
      if (sum(robjname %in% c("r_state", "r_data")) == 2) {
        upload_error_handler(objname,"### To restore state from a state-file select 'state' from the 'Load data of type' drowdown before uploading the file")
        rm(r_state, r_data) ## need to remove the local copies of r_state and r_data
      } else {
        upload_error_handler(objname,"### More than one R object contained in the data.")
      }
    } else {
      r_data[[objname]] <- as.data.frame(get(robjname)) %>% {set_colnames(., gsub("^\\s+|\\s+$", "", names(.)))}
    }
    r_info[[paste0(objname,"_descr")]] <- attr(r_info[[objname]], "description")
    r_info[['datasetlist']] <<- c(objname, r_info[['datasetlist']]) %>% unique
  } else if (ext == 'rds') {
    ## objname will hold the name of the object(s) inside the R datafile
    robj <- try(readRDS(uFile), silent = TRUE)
    if (is(robj, 'try-error')) {
      upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in rds.")
    } else {
      r_info[[objname]] <- as.data.frame(robj) %>% {set_colnames(., gsub("^\\s+|\\s+$", "", names(.)))}
      r_info[[paste0(objname,"_descr")]] <- attr(r_info[[objname]], "description")
      r_info[['datasetlist']] <<- c(objname, r_info[['datasetlist']]) %>% unique
    }
  } else if (ext == 'csv') {
    r_info[[objname]] <- loadcsv(uFile, .csv = .csv, header = header, sep = sep, saf = man_str_as_factor) %>%
    {if (is.character(.)) upload_error_handler(objname, mess) else .} %>%
    {set_colnames(., gsub("^\\s+|\\s+$", "", names(.)))}

    r_info[[paste0(objname,"_descr")]] <- attr(r_info[[objname]], "description")
    r_info[['datasetlist']] <<- c(objname, r_info[['datasetlist']]) %>% unique

  } else if (ext != "---") {

    ret <- paste0("### The selected filetype is not currently supported (",fext,").")
    upload_error_handler(objname,ret)

  }
  if (ext == 'txt') {
    r_data[[objname]] <- try(read.table(uFile, header=header, sep=sep, dec=dec,
                                        stringsAsFactors=FALSE), silent = TRUE) %>%
                                        { if(is(., 'try-error')){ upload_error_handler(objname,
                                                  "### There was an error loading the data.
                                                  Please make sure the data are in either txt format,
                                                  one gene by row.")
                                          }else{.} } %>% {if(man_str_as_factor){
                                            lapply(., factor)
                                            } else{.} } # %>% tbl_df
    r_data[['genelist']] <- c(objname,r_data[['genelist']]) %>% unique

  }
}


loadClipboard_GeneList <- function(objname = "User_Genes", ret = "", header = FALSE, sep = "\t", tab) {

  dat <- sshhr(try(
    {if (Sys.info()["sysname"] == "Windows") {
      read.table("clipboard", header = header, sep = sep, as.is = TRUE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      read.table(pipe("pbpaste"), header = header, sep = sep, as.is = TRUE)
    } else if (!is_empty(tab)){
      read.table(text = tab, header = header, sep = sep, as.is = TRUE) #load_cdata
    }} %>% as.data.frame(check.names = TRUE), silent = TRUE))
  dat <- t(dat)
  if (is(dat, 'try-error') || nrow(dat) == 0) {
    if (ret == "") ret <- c("### Gene List in clipboard was not well formatted.")
    upload_error_handler(objname,ret)
    r_data[['genelist']] <- c("DNA_damage_Response",r_data[['genelist']]) %>% unique
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
    r_data[[objname]] <- dat
    r_data[[paste0(objname,"_descr")]] <- ret
    r_data[['genelist']] <- c(objname,r_data[['genelist']]) %>% unique
  }

}

