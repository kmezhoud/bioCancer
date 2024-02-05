library(R.oo)
# .onAttach <- function(libname, pkgname){
#     packageStartupMessage('Please send questions to cbioportal@googlegroups.com')
# }
#

#' CGDS  connect object to cBioPortal
#' @description Creates a CGDS connection object from a CGDS endpoint URL. This object must be passed on to the methods which query the server.
#' @param url  A CGDS URL (required).
#' @param token An optional 'Authorization: Bearer' token to connect to cBioPortal instances that require authentication (default NULL)
#' @param verbose A boolean variable specifying verbose output (default FALSE)
#' @param ploterrormsg An optional message to display in plots if an error occurs (default ”)
#'
#' @usage CGDS(url,verbose=FALSE,ploterrormsg='',token=NULL)
#'
#' @export
setConstructorS3("CGDS", function(url='',verbose=FALSE,ploterrormsg='',token=NULL) {
  R.oo::extend(R.oo::Object(), "CGDS",
         .url=url,
         .token=token,
         .verbose=verbose,
         .ploterrormsg='')
})


#' S3 method to test cBioPortal connection
#'
#'
#' @method test CGDS
#'
#' @param x connection object
#' @param ... not used
#'
#'
#' @export test.CGDS
setMethodS3("test","CGDS", function(x, ...) {
  checkEq = function(a,b) { if (identical(a,b)) "OK\n" else "FAILED!\n" }
  checkGrt = function(a,b) { if (a > b) "OK\n" else "FAILED!\n" }
  checkTrue = function(a) { if (a) "OK\n" else "FAILED!\n" }

  cancerstudies = cBioPortalData::getStudies(x) |> select("name", "description", "studyId")

  cat('getCancerStudies... ',
      checkEq(colnames(cancerstudies),c("name","description", "studyId")))

  ct = cancerstudies[2,3] # should be row 1 instead ...

  cat('getCaseLists (1/1) ... ',
      checkEq(#colnames(getCaseLists(x,ct)),
              cBioPortalData::sampleLists(x, ct) |> select("name", "description", "sampleListId") |> colnames(),
              c("name","description","sampleListId")))

  # clinical data
  # check colnames
  cat('getClinicalData (1/1) ... ',
      checkTrue("DFS_MONTHS" %in% colnames(cBioPortalData::clinicalData(x, "gbm_tcga_pub"))))

  # check one gene, one profile
  cat('getGeneticProfiles (1/1) ... ',
      checkEq(#colnames(getProfileData(x,'NF1','gbm_tcga_mrna','gbm_tcga_all')),
              molecularProfiles(api = x, studyId = "gbm_tcga_pub") |>
                select("name", "description", "molecularProfileId") |> colnames(),
              c("name", "description", "molecularProfileId")))
  # # check many genes, one profile
  # cat('getGeneticProfiles (2/6) ... ',
  #     checkEq(colnames(getProfileData(x,c('MDM2','MDM4'),'gbm_tcga_mrna','gbm_tcga_all')),
  #             c("MDM2","MDM4")))
  # # check one gene, many profile
  # cat('getGeneticProfiles (3/6) ... ',
  #     checkEq(colnames(getProfileData(x,'NF1',c('gbm_tcga_mrna','gbm_tcga_mutations'),'gbm_tcga_all')),
  #             c('gbm_tcga_mrna','gbm_tcga_mutations')))
  # # check 3 cases returns matrix with 3 columns
  # cat('getGeneticProfiles (4/6) ... ',
  #     checkEq(rownames(getProfileData(x,'BRCA1','gbm_tcga_mrna',cases=c('TCGA-02-0001-01','TCGA-02-0003-01'))),
  #             make.names(c('TCGA-02-0001-01','TCGA-02-0003-01'))))
  # # invalid gene names return empty data.frame
  # cat('getGeneticProfiles (5/6) ... ',
  #     checkEq(nrow(getProfileData(x,c('NF10','NF11'),'gbm_tcga_mrna','gbm_tcga_all')),as.integer(0)))
  # # invalid case_list_id returns error
  # cat('getGeneticProfiles (6/6) ... ',
  #     checkEq(colnames(getProfileData(x,'NF1','gbm_tcga_mrna','xxx')),
  #             'Error..Invalid.case_set_id...xxx.'))
  cat('getProfileData (1/1) ... ',
      checkEq(#colnames(getGeneticProfiles(x,ct)),
        cBioPortalData::getDataByGenes(
          api = x,
          studyId = "gbm_tcga_pub",
          genes = c("NF1", "TP53", "ABL1"),
          by = "hugoGeneSymbol",
          molecularProfileIds = "gbm_tcga_pub_mrna"
        )  %>% .[[1]]  |>
          select("hugoGeneSymbol","patientId", "value") |>
          colnames(),
        c("hugoGeneSymbol","patientId","value")))
})
