# Script to query disgenet using a list of genes or diseases
# requires as input the gene or disease list in a file
# the output file name
# the type of entity (gene or disease)
# the type of identifier
#
# Author: jpinero@imim.es
###############################################################################


# main
###############################################################################
# load packages
#require(RCurl)

###############################################################################
# subs
###############################################################################

doQuery = function(entity, identifier){
  #print(entity)
  #print(identifier)


  GeneSymbol <- whichGeneList()
  #inputFile = paste("inst/base/data/GeneList/102.txt", sep="")
  #GeneSymbol <- t(unique(read.table(inputFile, sep="")))
  inFile <- data.frame(V1=unname(unlist(AnnotationFuncs::translate(GeneSymbol, org.Hs.eg.db::org.Hs.egSYMBOL2EG))))
  #inFile = read.csv(file=paste(getwd(), inputFile, sep="/"), sep="\t", header=F)
  dataFin <- data.frame(matrix(nrow=0, ncol=14))

  STR = "";
  if (entity == "gene"){
    if (identifier == "entrez"){
      STR = "c2.geneId = '"
    }
    else  if (identifier == "hgnc"){
      STR = "c2.name = '"
    }
    else{
      stop ( "the type of identifier must be entrez gene identifiers or gene symbols \n")
    }
  }
  else if (entity == "disease"){
    if (identifier == "cui"){
      STR = "c1.cui = '"
    }
    else  if (identifier == "mesh"){
      STR = "c1.mesh = '"
    }
    else  if (identifier == "omim"){
      STR = "c1.omim = '"
    }

    else{
      stop  ("the type of identifier must be cui or mesh or omim identifiers\n")
    }
  }
  else{
    stop ("the type of entity must be disease or gene \n");
  }

  for (ent in inFile$V1 ){
    url <- "http://www.disgenet.org/oql"
    oql <- paste( "DEFINE
                  c0='/data/gene_disease_summary',
                  c1='/data/diseases',
                  c2='/data/genes',
                  c3='/data/gene_roles',
                  c4='/data/sources'
                  ON
                  'http://www.disgenet.org/web/DisGeNET'
                  SELECT
                  c2 (geneId, name, uniprotId, description, pathName, pantherName),
                  c1 (cui, name, diseaseClassName, STY, MESH, omimInt, type),
                  c3 (PI, PL),
                  c0 (score, pmids,  snps)

                  FROM
                  c0
                  WHERE
                  (
                  c4 = 'ALL'
                  AND ", STR, ent , "' )
                  ORDER BY
                  c0.score DESC" , sep = "")
    #print(oql)
    dataTsv <-  getURLContent(url, readfunction =charToRaw(oql), upload = TRUE, customrequest = "POST")
    #dataTsv <- rawToChar( getURLContent(url, readfunction =charToRaw(oql), upload = TRUE, customrequest = "POST"))
    data <- read.csv(textConnection(dataTsv), header = TRUE, sep="\t")
    if (dim(data)[1] == 0 ){
      print ( paste (entity , ent, " is not in DisGeNET ", sep = " "))
    }
    else  {
      dataFin <- rbind(dataFin, data)
    }

  }
  #address <-  paste(getwd(), outFile, sep="/")
  #print(address)

  #write.table(dataFin,  address, sep="\t", row.names = F,dec = ".", quote = FALSE)
  return(dataFin)
}
###############################################################################
# main
###############################################################################

#c("DKK3", "NBN", "MYO6", "TP53","PML", "IFI16", "BRCA1")
#myargs = commandArgs()

# inputFile = paste("inst/base/data/GeneList/102.txt", sep="")
#  ##outputFile = paste("GDA/output.txt", sep="")
 #entity = "gene"
 #identifier = "entrez"
#
#
# print("Querying the database ")
# DGA <- doQuery(inputFile, entity, identifier)
# print("Finished")

