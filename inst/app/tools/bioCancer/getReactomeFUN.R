

serviceURL <- function(version){

  #base.url = "http://reactomews.oicr.on.ca:8080/"
  base.url = "http://cpws.reactome.org/"

  if (version == "2005") {
    serv.url <- paste0(base.url, "caBigR3WebApp2015/FIService/network/")
  } else if (version == "2016") {
    serv.url <- paste0(base.url, "caBigR3WebApp2016/FIService/network/")
  } else  if (version == "2017"){
    serv.url <- paste0(base.url, "caBigR3WebApp2017/FIService/network/")
  } else if (version == "2018"){
    serv.url <- paste0(base.url, "caBigR3WebApp2018/FIService/network/")
  } else{
    serv.url <- paste0(base.url, "caBigR3WebApp2021/FIService/network/")
  }
  return(serv.url)
}

#' @export
queryBuildNetwork <- function(version, genes){
  service.url <- paste0(serviceURL(version), "buildNetwork")
  genes.str <- paste(genes, collapse = "\t")
  doc <- getPostXML(service.url, genes.str)
  return(extractFIs(doc))
}

extractFIs <- function(doc) {
  interactions <- XML::xpathApply(doc, "//interaction", function(x) {
    info <- XML::xmlChildren(x)
    first.protein <- XML::xmlValue(XML::xmlChildren(info$firstProtein)$name)
    second.protein <- XML::xmlValue(XML::xmlChildren(info$secondProtein)$name)
    data.frame(first.protein = first.protein,
               second.protein = second.protein, stringsAsFactors = FALSE)
  })
  fis <- do.call(rbind, interactions)
  if (is.null(fis)) return(data.frame())
  return(fis)
}

getPostXML <- function(url, body) {
  text.gatherer <- RCurl::basicTextGatherer()
  opts <- list(httpheader = c("Content-Type" = "text/plain;charset=UTF-8",
                              "Accept" = "application/xml"))
  RCurl::curlPerform(postfields = body, url = url, .opts = opts,
              .encoding = "UTF-8", writefunction = text.gatherer$update)
  xml <- XML::xmlInternalTreeParse(text.gatherer$value())
  return(xml)
}

queryFIs <- function(version, genes){
  service.url <- paste0(serviceURL(version), "queryFIs")
  genes.str <- paste(genes, collapse = "\t")
  doc <- getPostXML(service.url, genes.str)
  return(extractFIs(doc))
}

getReactomeFI <- function(version, genes, use.linkers = FALSE){
  if (length(genes) > 1) {

    if (use.linkers) {
      fis <- queryBuildNetwork(version, genes)
    } else {
      fis <- queryFIs(version, genes)
    }
  }
  version
  fis
}


#################### Get CLUSTER###########
fis2str <- function(fis) {
  fis[, "first.protein"] <- as.character(fis[, "first.protein"])
  fis[, "second.protein"] <- as.character(fis[, "second.protein"])
  fis <- cbind(data.frame(id = 1:nrow(fis)), fis)

  fis.list = c()
  for (i in 1:nrow(fis)) {
    first.protein <- fis[i, "first.protein"]
    second.protein <- fis[i, "second.protein"]
    if (first.protein < second.protein) {
      fi.str <- paste(first.protein, second.protein, sep = "\t")
    } else {
      fi.str <- paste(second.protein, first.protein, sep = "\t")
    }
    fi.str <- paste(fis[i, "id"], fi.str, sep = "\t")
    fis.list <- c(fis.list, fi.str)
  }
  fis.str <- paste(fis.list, collapse = "\n")
  return(fis.str)
}

queryCluster<-  function(version, fis) {
  service.url <- paste0(serviceURL(object), "cluster")
  fis.str <- fis2str(fis)
  doc <- getPostXML(service.url, fis.str)
  modules <- XML::xpathApply(doc, "//geneClusterPairs", function(x) {
    info <- XML::xmlChildren(x)
    module <- XML::xmlValue(info$cluster)
    gene <- XML::xmlValue(info$geneId)
    data.frame(gene = gene, module = module, stringsAsFactors = FALSE)
  })
  modules <- do.call(rbind, modules)
  modules$module <- as.numeric(modules$module)
  return(modules)
}


####################  Get ANNOTATION #############

extractAnnotations <- function(xml.node) {
  annotations <- XML::xpathApply(xml.node, "./annotations", function(x) {
    info <- XML::xmlChildren(x)
    data.frame(topic = XML::xmlValue(info$topic),
               hit.num = XML::xmlValue(info$hitNumber),
               number.in.topic = XML::xmlValue(info$numberInTopic),
               ratio.of.topic = XML::xmlValue(info$ratioOfTopic),
               p.value = XML::xmlValue(info$PValue),
               fdr = XML::xmlValue(info$fdr),
               hits = paste(XML::xpathSApply(x, "./hitIds", xmlValue),
                            collapse = ","),
               stringsAsFactors = FALSE)
  })
  annotations <- do.call(rbind, annotations)

  if (is.null(annotations)) return(data.frame())

  annotations$hit.num <- as.numeric(annotations$hit.num)
  annotations$number.in.topic <- as.numeric(annotations$number.in.topic)
  annotations$ratio.of.topic <- as.numeric(annotations$ratio.of.topic)
  annotations$p.value <- as.numeric(annotations$p.value)
  annotations$fdr <- gsub("<", "", annotations$fdr)
  annotations$fdr <- as.numeric(annotations$fdr)
  return(annotations[order(annotations$fdr), ])
}
queryAnnotateGeneSet <-  function(object, genes, type = c("Pathway", "BP", "CC", "MF")){
  type <- match.arg(type)
  service.url <- paste0(serviceURL(object), "annotateGeneSet/", type)
  genes.str <- paste(genes, collapse = "\n")
  doc <- getPostXML(service.url, genes.str)
  annot.node <- XML::xmlChildren(doc)$moduleGeneSetAnnotations
  annot.node <- XML::xmlChildren(annot.node)$moduleGeneSetAnnotation
  annotations <- extractAnnotations(annot.node)
  return(annotations)
}

annotate <- function(version, GeneList,fis,type = c("Pathway", "BP", "CC", "MF"),
                     include.linkers = FALSE) {
  if (nrow(fis) == 0) {
    warning("No FI network data found. Please build the network first.")
    return(object)
  }

  #fis <- fis(object)
  fi.genes <- union(fis$first.protein, fis$second.protein)

  if (!include.linkers) {
    fi.genes <- fi.genes[fi.genes %in% GeneList]
  }

  type <- match.arg(type)

  return(queryAnnotateGeneSet(object, fi.genes, type))
}


####  Get Annotation Module  #####

df2tsv <- function(dat) {
  for (i in 1:ncol(dat)) {
    if (class(dat[, i]) == "numeric") {
      dat[, i] <- format(dat[, i], trim = TRUE)
    }
  }
  tsv <- apply(dat, 1, function(x) paste(x, collapse = "\t"))
  tsv <- paste(tsv, collapse = "\n")
  return(tsv)
}



queryAnnotateModules <- function(object, module.nodes,type = c("Pathway", "BP", "CC", "MF")){
  type <- match.arg(type)
  service.url <- paste0(serviceURL(object), "annotateModules/", type)
  query <- df2tsv(module.nodes)
  doc <- getPostXML(service.url, query)
  module.annotations <- XML::xpathApply(doc, "//moduleGeneSetAnnotation",
                                   function(x) {
                                     module <- XML::xmlValue(XML::xmlChildren(x)$module)
                                     annotations <- extractAnnotations(x)
                                     if (all(is.na(annotations))) return(annotations)
                                     cbind(data.frame(module = module), annotations)
                                   })
  module.annotations <- module.annotations[!is.na(module.annotations)]
  module.annotations <- do.call(rbind, module.annotations)
  module.annotations$module <- as.numeric(module.annotations$module)
  module.annotations$module <- module.annotations$module - 1
  return(module.annotations)
}


##### FIs Between###

queryFIsBetween <- function(object, fis) {
  service.url <- paste0(serviceURL(object), "queryFIsBetween")
  gene.pairs <- as.matrix(fis)
  first.str <- paste(gene.pairs[, 1], collapse = ",")
  second.str <- paste(gene.pairs[, 2], collapse = ",")
  pairs.str <- paste(first.str, second.str, sep = "\n")
  doc <- getPostXML(service.url, pairs.str)
  return(extractFIs(doc))
}

#### EDGE #####

#' extract Protein Info
#
#' Extract protein information including accession ID and DB name, protein
#'  name, and sequence.
#
#' @param protein.node XML node containing protein information
#' @return data.frame Data frame where each row corresponds to a protein and
#'  the columns contain the information mentioned above.
extractProteinInfo <- function(protein.node) {
  accession <- XML::xmlValue(XML::xmlChildren(protein.node)$accession)
  db.name <- XML::xmlValue(XML::xmlChildren(protein.node)$dbName)
  name <- XML::xmlValue(XML::xmlChildren(protein.node)$name)
  short.name <- XML::xmlValue(XML::xmlChildren(protein.node)$shortName)
  prot.seq <- XML::xmlValue(XML::xmlChildren(protein.node)$sequence)
  info <- data.frame(accession = accession,
                     db.name = db.name,
                     short.name = short.name,
                     name = name,
                     sequence = prot.seq,
                     stringsAsFactors = FALSE)
  return(info)
}

#queryEdge(2012, "TP53", "BRCA1")
queryEdge <-  function(object, name1, name2) {
  service.url <- paste0(serviceURL(object), "queryEdge")
  edge.str <- paste(name1, name2, sep = "\t")
  doc <- getPostXML(service.url, edge.str)
  first.prot <- getNodeSet(doc, "//firstProtein", fun = extractProteinInfo)
  second.prot <- getNodeSet(doc, "//secondProtein", fun = extractProteinInfo)
  return(do.call(rbind, c(first.prot, second.prot)))
}

