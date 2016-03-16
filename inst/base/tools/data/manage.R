descr_out <- function(descr, ret_type = 'html') {
  ## if there is no data description
  if (descr %>% is_empty) return("")

  ## if there is a data description and we want html output
  if (ret_type == 'html')
    descr <- markdown::markdownToHTML(text = descr, stylesheet = "")

  descr
}

upload_error_handler <- function(objname, ret) {
  ## create an empty data.frame and return error message as description
  r_data[[paste0(objname,"_descr")]] <- ret
  r_data[[objname]] <- data.frame(matrix(rep("",12), nrow = 2))
}

loadClipboardData <- function(objname = "copy_and_paste", ret = "", header = TRUE, sep = "\t") {

  dat <- sshhr(try(
    {if (Sys.info()["sysname"] == "Windows") {
      read.table("clipboard", header = header, sep = sep, comment.char = "", fill = TRUE,  as.is = TRUE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      read.table(pipe("pbpaste"), header = header, sep = sep, comment.char = "", fill = TRUE,  as.is = TRUE)
    } else {
      if (!is_empty(input$load_cdata))
        read.table(text = input$load_cdata, header = header, sep = sep, comment.char = "", fill = TRUE,  as.is = TRUE)
    }} %>% as.data.frame(check.names = FALSE), silent = TRUE))

  if (is(dat, 'try-error') || nrow(dat) == 0) {
    if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on ", lubridate::now())
    r_data[[objname]] <- dat %>% as.data.frame(check.names = FALSE)
    r_data[[paste0(objname,"_descr")]] <- ret
  }
  r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
}

saveClipboardData <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == 'Windows') {
    write.table(.getdata(), "clipboard-10000", sep="\t", row.names=FALSE)
  } else if (os_type == "Darwin") {
    write.table(.getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
  } else if (os_type == "Linux") {
    print("### Saving data through the clipboard is currently only supported on Windows and Mac. You can save your data to csv format to use it in a spreadsheet.")
  }
}


factorizer <- function(dat) {
  isChar <- sapply(dat,is.character)
  if (sum(isChar) == 0) return(dat)
  toFct <-
    dplyr::select(dat, which(isChar)) %>%
    summarise_each(funs(n_distinct(.) < 100 & (n_distinct(.)/length(.)) < .1)) %>%
    dplyr::select(which(. == TRUE)) %>% names
  # summarise_each(funs(n_distinct)) %>%
  # dplyr::select(which(. < 100 & ((. / nrow(dat)) < .1))) %>% names
  if (length(toFct) == 0) return(dat)
  mutate_each_(dat, funs(as.factor), vars = toFct)
}

################# Load dataframe (Clinical data, Profile Data, ...) in Datasets
loadInDatasets <- function(fname, header= TRUE){

  objname <- fname
  if(fname=="ProfData"){
    GeneList <- whichGeneList()
    dat <- as.data.frame(getProfileData(cgds, GeneList, input$GenProfID,input$CasesID))
    r_data[[objname]] <- dat %>% add_rownames("Patients")


  }else if (fname=="ClinicalData"){
    dat <- as.data.frame(getClinicalData(cgds, input$CasesID))
    r_data[[objname]] <- dat %>% add_rownames("Patients")

  }else if (fname=="MutData"){
    GeneList <- whichGeneList()
    dat <- as.data.frame((getMutationData(cgds,input$CasesID, input$GenProfID, GeneList)))
    r_data[[objname]] <- dat %>% add_rownames("Patients")
  } else if (fname=="xCNA"){
    dat <- plyr::ldply(r_data$ListProfData$CNA)
    r_data[[objname]] <- dat
  } else if(fname =="xmRNA"){
    dat <- plyr::ldply(r_data$ListProfData$Expression)
    r_data[[objname]] <- dat
  }else if(fname == "xMetHM450"){
    dat <- plyr::ldply(r_data$ListProfData$Met_HM450)
    r_data[[objname]] <- dat
  }else if(fname== "xMetHM27"){
    dat <- plyr::ldply(r_data$ListProfData$Met_HM27)
    r_data[[objname]] <- dat
  }else if (fname=="xMut"){
    dat <- plyr::ldply(r_data$ListMutData)
    r_data[[objname]] <- dat
  } else if(fname== "xFreqMut"){
    dat <- r_data$Freq_DfMutData
    r_data[[objname]] <- dat %>% add_rownames("Genes")
  }else if (fname== "xmiRNA"){
    dat <- plyr::ldply(r_data$ListProfData$miRNA)
    r_data[[objname]] <- dat
  }else if (fname== "xRPPA"){
    dat <- plyr::ldply(r_data$ListProfData$RPPA)
    r_data[[objname]] <- dat
  }
  r_data[[paste0(objname,"_descr")]] <- attr(r_data[[objname]], "description")
  r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
}
######
loadClipboard_GeneList <- function(objname = "Genes", ret = "", header = TRUE, sep = "\t") {

  dat <- sshhr(try(
    {if (Sys.info()["sysname"] == "Windows") {
      read.table("clipboard", header = header, sep = sep, as.is = TRUE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      read.table(pipe("pbpaste"), header = header, sep = sep, as.is = TRUE)
    } else {
      if (!is_empty(input$load_cdata))
        read.table(text = input$load_cdata, header = header, sep = sep, as.is = TRUE)
    }} %>% as.data.frame(check.names = FALSE), silent = TRUE))
  dat <- t(dat)
  if (is(dat, 'try-error') || nrow(dat) == 0) {
    if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
    upload_error_handler(objname,ret)
  } else {
    ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
    r_data[[objname]] <- dat
    r_data[[paste0(objname,"_descr")]] <- ret
  }
  r_data[['genelist']] <- c(objname,r_data[['genelist']]) %>% unique
}

## Load Gene Lists example from package to r_data$genelist
# loadClipboard_GeneList <- function(objname = "Genes", ret = "", header = FALSE, sep = "\n") {
#
#   if (Sys.info()["sysname"] == "Windows") {
#     GeneList <- try(read.table("clipboard", header = header, sep = sep), silent = TRUE)
#     GeneList <- t(GeneList)
#   } else if (Sys.info()["sysname"] == "Darwin") {
#     GeneList <- try(read.table(pipe("pbpaste"), header = header, sep = sep), silent = TRUE)
#    GeneList <- t(GeneList)
#   } else {
#     GeneList <- try(read.table(text = input$load_cdata, header = header, sep = sep), silent = TRUE)
#     GeneList <- t(GeneList)
#   }
#
#   if (is(GeneList, 'try-error')) {
#     if (ret == "") ret <- c("### Data in clipboard was not well formatted. Try exporting the data to csv format.")
#     upload_error_handler(objname,ret)
#   } else {
#     ret <- paste0("### Clipboard data\nData copied from clipboard on", lubridate::now())
#     r_data[[objname]] <- data.frame(GeneList, check.names = FALSE)
#     #r_data[[paste0(objname,"description")]] <- ret
#   }
#   r_data[['genelist']] <- c(objname,r_data[['genelist']]) %>% unique
#
#
# }



loadUserData <- function(fname, uFile, ext,
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
    # fext <- tools::file_ext(filename) %>% tolower

    if (fext %in% c("xls","xlsx")) {
      ret <- "### bioCancer does not load xls files directly. Please save the data as a csv file and try again."
    } else {
      ret <- paste0("### The filename extension (",fext,") does not match the specified file-type (",ext,"). Please specify the file type you are trying to upload (i.e., csv or rda).")
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
      # r_data[[objname]] <- as.data.frame(get(robjname)) %>% {gsub("^\\s+|\\s+$", "", names(.))}
      r_data[[objname]] <- as.data.frame(get(robjname)) %>% {set_colnames(., gsub("^\\s+|\\s+$", "", names(.)))}
      r_data[[paste0(objname,"_descr")]] <- attr(r_data[[objname]], "description")
    }

    #############
    r_data[['datasetlist']] <- c(objname,r_data[['datasetlist']]) %>% unique
    ############
  }

  if (ext == 'csv') {
    # r_data[[objname]] <- read.csv(uFile, header=header, sep=sep, dec=dec,
    # r_data[[objname]] <- readr::read_csv(uFile, col_names=header)
    # stringsAsFactors=man_str_as_factor), silent = TRUE) %>%
    r_data[[objname]] <- loadcsv(uFile, header = header, sep = sep, saf = man_str_as_factor) %>%
    {if (is.character(.)) upload_error_handler(objname, mess) else .}

  } else {
    ret <- paste0("### The selected filetype is not currently supported (",fext,").")
    upload_error_handler(objname,ret)
  }
  ############################
  if (ext == 'txt') {
    # r_data[[objname]] <- read.csv(uFile, header=header, sep=sep, dec=dec,
    # r_data[[objname]] <- readr::read_csv(uFile, col_names=header)
    # stringsAsFactors=man_str_as_factor), silent = TRUE) %>%
    r_data[[objname]] <- try(read.table(uFile, header=header, sep=sep, dec=dec,
                                        stringsAsFactors=FALSE), silent = TRUE) %>%
                                        { if (is(., 'try-error')) upload_error_handler(objname, "### There was an error loading the data. Please make sure the data are in either txt format, one gene by row.")
                                          else . } %>%
                                          { if (man_str_as_factor) factorizer(.) else . } # %>% tbl_df
    r_data[['genelist']] <- c(objname,r_data[['genelist']]) %>% unique


    #r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique
  }
}
