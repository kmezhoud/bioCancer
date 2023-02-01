# Annotation translation functions
#'
#' \tabular{ll}{
#' Package:  \tab  AnnotationFuncs\cr
#' Type:  \tab  Package\cr
#' Version:  \tab  1.3.0\cr
#' Date:  \tab  2011-06-10\cr
#' License:  \tab  GPL-2\cr
#' LazyLoad:  \tab  yes\cr
#' }
#'
#' Functions for handling translations between different identifieres using
#' the Biocore Data Team data-packages (e.g. \code{org.Bt.eg.db}).
#' Primary functions are \code{\link{translate}} for translating
#' and \code{\link{getOrthologs}} for efficient lookup of homologes
#' using the Inparanoid databases.
#' Other functions include functions for selecting Refseqs or Gene Ontologies (GO).
#'
#' @name  AnnotationFuncs-package
#' @aliases  AnnotationFuncs
#' @docType  package
#' @title Annotation translation functions
#' @author  Stefan McKinnon Edwards  \email{stefan.hoj-edwards@@agrsci.dk}
#' @references
#' \url{http://www.iysik.com/index.php?page=annotation-functions}
#' @keywords  package
#' @seealso  \code{\link{translate}}, \code{\link{getOrthologs}}
#' @examples
#' \dontrun{
#' library(org.Bt.eg.db)
#' gene.symbols <- c('DRBP1','SERPINA1','FAKE','BLABLA')
#' # Find entrez identifiers of these genes.
#' eg <- translate(gene.symbols, org.Bt.egSYMBOL2EG)
#' # Note that not all symbols were translated.
#'
#' # Go directly to Refseq identifiers.
#' refseq <- translate(gene.symbols, from=org.Bt.egSYMBOL2EG, to=org.Bt.egREFSEQ)
#' # Pick the proteins:
#'  pickRefSeq(refseq, priorities=c('NP','XP'), reduce='all')
#'  }
NULL

#' Translate between different identifiers
#'
#' Function for translating from one annotation to another, eg. from RefSeq to
#' Ensemble.  This function takes a vector of annotation values and translates
#' first to the primary annotation in the Biocore Data Team package (ie. entrez gene identifier for org.Bt.eg.db)
#' and then to the desired product, while removing non-translated annotations
#' and optionally reducing the result so there is only a one-to-one relation.
#'
#' If you want to do some further mapping on the result, you will have to use
#' either \code{unlist} og \code{lapply}, where the first returns all the end-products
#' of the first mapping, returning a new list, and the latter produces a list-within-list.
#'
#' If \code{from} returns GO identifiers (e.g. \code{from = org.Bt.egGO}), then the
#' returned resultset is more complex and consists of several layers of lists instead of
#' the usual list of character vectors. If \code{to} has also been specified, the GO IDs
#' must be extracted (internally) and you have the option of filtering for evidence and category at this point.
#' See \code{\link{pickGO}}.
#'
#' @note Requires user to deliver the annotation packages such as org.Bt.egREFSEQ.
#' @param values Vector of annotations that needs translation. Coerced to character vector.
#' @param from Type of annotation \code{values} are given in. NB! take care in the
#'         orientation of the package, ie. if you have RefSeq annotations, use
#'         \code{org.Bt.egREFSEQ2EG} or (in some cases) \code{revmap(org.Bt.egREFSEQ)}.
#' @param to Desired goal, eg. \code{org.Bt.egENSEMBLPROT}. If \code{NULL} (default), goal
#'         if the packages primary annotation (eg. entrez gene for org.Bt.eg.db).
#'         Throws a warning if the organisms in \code{from} and \code{to} are not the same.
#' @param reduce Reducing method, either return all annotations (one-to-many relation)
#'         or the first or last found annotation. The reducing step is applied
#'         after translating to the goal:
#'         \code{all}: returns all annotations
#'         \code{first} or \code{last}: choose first or last of arbitrarily ordered list.
#' @param return.list Logical, when \code{TRUE}, returns the translation as a list where names
#         are the given values that could be translated. The function works
#         on lists, so this is just as easy to return.
#         When \code{FALSE}, a table.  Convenience for calling on the result.
#' @param remove.missing Logical, whether to remove non-translated values, defaults \code{TRUE}.
#' @param simplify Logical, unlists the result. Defaults to FALSE. Usefull when using \code{translate} in
#'                 a \code{lapply} or \code{sapply}.
#' @param ... Additional arguments sent to \code{\link{pickGO}} if \code{from} returns GO set.
#' @return List; names of elements are \code{values} and the elements are the translated elements,
#'        or \code{NULL} if not translatable with \code{remove.missing = TRUE}.
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @seealso \code{\link{pickRefSeq}}, \code{\link{pickGO}}
#' @export
#' @examples
#' \dontrun{
#' library(org.Bt.eg.db)
#' genes <- c(280705, 280706, 100327208)
#' translate(genes, org.Bt.egSYMBOL)
#'
#' symbols <- c("SERPINA1","KERA","CD5")
#' refseq <- translate(symbols, from=org.Bt.egSYMBOL2EG, to=org.Bt.egREFSEQ)
#' # Pick the proteins:
#' pickRefSeq(refseq, priorities=c('NP','XP'), reduce='all')
#'
#' # If you wanted do do some further mapping on the result from
#' # translate, simply use lapply.
#'
#' library(GO.db)
#' GO <- translate(genes, org.Bt.egGO)
#' # Get all biological processes:
#' pickGO(GO, category='BP')
#' # Get all ontologies with experimental evidence:
#' pickGO(GO, evidence=c('IMP','IGI','IPI','ISS','IDA','IEP','IEA'))
#' }
translate <- function(values, from, to=NULL,
                      reduce=c('all','first','last'),
                      return.list = TRUE,
                      remove.missing=TRUE,
                      simplify=FALSE,
                      ...) {
  # Roadmap:
  # Check validity of attributes.
  # Check that the organisms of from and to match.
  # Translate to primary annotation.
  # Optionally translate to end.
  # Optionally reduce result.
  # Optionally unstack.

  ###
  # Check validity of attributes
  ###
  values <- unique(as.vector(values, mode='character'))
  values <- values[!is.na(values)]
  values <- values[which(sapply(values, nchar)>0)]  # Removes zero-length entries.
  reduce <- match.arg(reduce)
  return.list <- as.logical(return.list)

  #l <- NULL # R compiler complains about l not being binded? Half a year later and I am still not sure what this does.

  ###
  # Check organisms - throw warning if not.
  ###
  if (!is.null(to)) {
    # Compare taxid:
    org.from <- NULL
    org.to <- NULL
    try( org.from <- dbmeta(dbconn(from), "ORGANISM"), silent=TRUE)
    try( org.to <- dbmeta(dbconn(to), "ORGANISM"), silent=TRUE)
    if (!is.null(org.from) & !is.null(org.to)) {
      if (org.from != org.to) {
        warning(sprintf("TAXID for 'to' and 'from' does not match! We found:
\t From:\t%s
\tTo:\t%s
\tFor cross species (ie. homologes) look at e.g. hom.Hs.inp.db.", org.from, org.to))
        # End of warning message.
      }
    }
  }

  ###
  # Translate to primary id:
  ###
  primary <- AnnotationDbi::mget(values, from, ifnotfound=NA)
  # primary is now a list.
  # Remove nulls and NAs
  primary <- primary[!is.na(primary)]
  # primary is now a list with non-empty, multiple length entries

  # Provide a method for reducing.
  pickOne <- switch(reduce,
                    all = function(x) x,
                    first = function(x) x[1],
                    last = function(x) x[length(x)])

  ###
  # Translate all primary ids to goal annotation
  ###
  if (is.null(to) == FALSE) {
    # Check for special case where the from-package is into GO.
    # If it is, then the primary must be reduced to only the GO identifiers and not the set of GO ID, evidence and category.
    isGO <- FALSE
    try(isGO <- from@objName == 'GO', silent=TRUE)
    if (isGO) {
      primary <- pickGO(primary, ...)
    }
    # Remember to unlist, so we get every entry of primary, which is every
    # possible translation of the starting values.
    goal <- AnnotationDbi::mget(unlist(primary, use.names=F), to, ifnotfound=NA)
    # Map all goal-annotations to the starting annotations:
    # lapply over primary, so we get primary's names.
    goal <- lapply(primary, function(x) {
      x <- unlist(goal[x], use.names=FALSE) # x is vector, so we get all entries in goal for all x.
      if (length(x) == 0 || is.na(x[1]))
        return(NA)
      x <- pickOne(x)
      return(x)
    })
  } else {
    # If goal is primary annotation, reduce accordingly.
    goal <- primary
    if (reduce != 'all')
      goal <- lapply(goal, pickOne)
  }

  ###
  # Remove nulls and NAs
  ###
  if (remove.missing) {
    goal <- goal[!is.na(goal)]
  } else {
    missing <- values[!(values %in% names(goal))]
    #missing.list <- as.list(rep(NA, length(missing)))
    #names(missing.list) <- missing
    #goal <- c(goal, missing.list)
    goal[missing] <- NA
  }

  ###
  # Return result if a list is desired,
  # else unstack to a table.
  ###
  if (simplify) return(unlist(goal, use.names=FALSE))
  if (return.list) {
    return(goal)
  } else {
    if (length(goal) == 0)
      return(data.frame(from=factor(),to=factor()))
    goal <- stack(goal)
    new.c <- c('from','to') # Matching 'ind' and 'values'.
    colnames(goal) <- new.c[match(c('ind','values'), colnames(goal))]
    return(goal)
  }
}

#' Picks a prioritised RefSeq identifier from a list of identifiers
#'
#' When translating to RefSeq, typically multiple identifiers are returned,
#' referring to different types of products, such as genomic molecule, mature
#' mRNA or the protein, and they can be predicted, properties that can be read
#' from the prefix (\url{http://www.ncbi.nlm.nih.gov/refseq/key.html}).  E.g. "XM_" is
#' predicted mRNA and "NP_" is a protein. Run \code{?org.Bt.egREFSEQ}.
#'
#' @param l  Vector or list of RefSeqs accessions to pick from.  If list given, applies the
#'            prioritation to each element in the list.
#' @param priorities Character vector of prioritised prefixes to pick by. Eg. \code{c("NP","NM")}
#'             returns RefSeqs starting 'NP', and if none found, those starting
#'             'NM'.  If no RefSeqs are found according to the priorities, Null
#'             is returned, unless the last element in priorities is '*'.
#'             Uses grepl, so see these for pattern matching.
#'             Default: c('NP','XP','NM','XM')
#' @param reduce Reducing method, either return all annotations (one-to-many relation)
#'         or the first or last found annotation.  The reducing step is applied
#'         after translating to the goal:
#'         \code{all}: returns all annotations
#'         \code{first} or \code{last}: choose first or last of arbitrarily ordered list.
#' @return If vector given, returns vector.  If list given, returns list without element where nothing could be picked.
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @export
#' @examples
#' \dontrun{
#' library(org.Bt.eg.db)
#' symbols <- c("SERPINA1","KERA","CD5")
#' refseq <- translate(symbols, from=org.Bt.egSYMBOL2EG, to=org.Bt.egREFSEQ)
#' mRNA <- pickRefSeq(refseq, priorities=c('NM','XM'))
#' proteins <- pickRefSeq(refseq, priorities=c('NP','XP'))
#' }
pickRefSeq <- function(l, priorities=c('NP','XP','NM','XM'),
                       reduce=c('all','first','last')) {
  if (is.list(l)) {
    res <- lapply(l, .pickRef, priorities=priorities, reduce=reduce)
    res <- res[!sapply(res, is.null)]
    return(res)
  } else {
    return(.pickRef(l, priorities = priorities, reduce=reduce))
  }
}

#' Secret function that does the magic for pickRefSeq.
#'
#' Do not use it, use \code{\link{pickRefSeq}}!
#'
#' @param l List.
#' @param priorities How to prioritize.
#' @param reduce How to reduce.
#' @return List.
#' @note Hey, you found a secret function! Keep it that way!
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @seealso \code{\link{pickRefSeq}}
.pickRef <- function(l, priorities, reduce=c('all','first','last')){
  # Roadmap:
  # Check attributes
  # Get prioritised element
  # Reduce

  ###
  # Check attributes
  ###
  l <- as.vector(l, mode="character")
  reduce <- match.arg(reduce)

  ###
  # Get prioritised element, by iterating over 'priorities' and grepping by it.
  ###
  for (p in priorities) {
    res <- grep(p, l, ignore.case=TRUE, value=TRUE)
    if (length(res) > 0) break
  }
  if (length(res) == 0) return(NULL)

  ###
  # Reduce
  ###
  # Provide a method for reducing.
  pickOne <- switch(reduce,
                    all = function(x) x,
                    first = function(x) x[1],
                    last = function(x) x[length(x)])
  return(pickOne(res))
}


#' Replaces contents of list A with elements of list B
#'
#' Combines two lists, \code{A} and \code{B}, such that \code{names(A)} are preserved, mapping to the
#' values of \code{B}, using \code{names(B)} as look up.  Ie. replaces values in \code{A} with values
#' in \code{B}, using \code{names(B)} as look up for values in \code{A}.
#' Once more?  See examples.
#' \emph{NB!} None-mapped entries are returned as NA, but can be removed using \code{\link{removeNAs}}.
#' @param A List, elements are coerced to character for mapping to B.
#' @param B List.
#' @param removeNAs Boolean, whether to remove the \code{NA}s that occur because an element was not found in \code{B}.
#' @return List.
#' @seealso \code{\link{removeNAs}}
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @export
#' @examples
#' A <- list('a1'='alpha','a2'='beta','a3'=c('gamma','delta'))
#' B <- list('alpha'='b1', 'gamma'=c('b2', 'b3'), 'delta'='b4')
#' mapLists(A, B)
#  # Returns: list('a1'='b1', 'a2'=NA, 'a3'=c('b2','b3','b4'))
mapLists <- function(A, B, removeNAs=TRUE) {
  res <- lapply(A, function(x) {
    x <- unlist(B[as.character(x)], use.names=FALSE)
    if (length(x) == 0 || is.na(x[1])) return(NA)
    return(x)
  })
  if (removeNAs) res <- removeNAs(res)
  return(res)
}

#' Removes entries equal \code{NA} from list or vector
#'
#' Removes entries equal \code{NA}, but not mixed entries containing, amongst others, \code{NA}.
#' Good for use after \code{\link{mapLists}} that might return entries equal \code{NA}.
#' @param l Vector or list.
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @export
#' @examples
#' removeNAs(list('a'=NA, 'b'=c(NA, 'B'), 'c'='C'))
removeNAs <- function(l) { return(l[!is.na(l)]) }

#' Cleans up result from org.Xx.egGO and returns specific GO identifiers
#'
#' Cleans up result from org.Xx.egGO and returns GO identifier for  either
#' biological process (BP), cellular component (CC), or molecular function (MF).
#' Can be used on list of GOs from \code{\link{translate}}, or a single list of GOs from an annotation package.
#' May reduce list, if the (sub)list does not contain the chosen class!
#' @param l Character vector, or list of, og GO identifiers.
#' @param evidence Character vector, filters on which kind of evidence to return; for a larger list see \code{\link{getEvidenceCodes}}. \\*
#'                 Evidence codes may be: \code{c('IMP','IGI','IPI','ISS','IDA','IEP','IEA','TAS','NAS','ND','IC')}. \\*
#'				   Leave as \code{NA} to ignore filtering on this part.
#' @param category Character vector, filters on which ontology to return: biological process (BP), cellular component (CC), or molecular function (MF). \\*
#'				   Leave as \code{NA} to ignore filtering on this part.
#' @return List with only the picked elements.
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @seealso \code{\link{pickRefSeq}}, \code{\link{getEvidenceCodes}}, \code{\link{translate}}
#' @export
#' @examples
#' \dontrun{
#' library(org.Bt.eg.db)
#' genes <- c(280705, 280706, 100327208)
#' GO <- translate(genes, org.Bt.egGO)
#' # Get all biological processes:
#' pickGO(GO, category='BP')
#' # Get all ontologies with experimental evidence:
#' pickGO(GO, evidence=c('IMP','IGI','IPI','ISS','IDA','IEP','IEA'))
#' pickGO <- function(l, evidence=c('IMP','IGI','IPI','ISS','IDA','IEP','IEA','TAS','NAS','ND','IC'), category=c('BP','CC','MF'))
#' }
pickGO <- function(l, evidence=NA, category=NA) {
  evidence <- toupper(evidence)
  category <- toupper(category)

  if (length(l) == 3) {
    if (names(l) == c('GOID','Evidence','Ontology')) {
      if (is.list(l)) l <- unlist(l, use.names=FALSE)
      if (is.na(evidence)) evidence = l[2]
      if (is.na(category)) category = l[3]
      if (l[2] %in% evidence & l[3] %in% category) return(l[1])
      return(NA)
    }
  }
  if (all(substr(names(l), 1, 3) == 'GO:')) {
    gos <- l[which(substr(names(l), 1, 3) == 'GO:')]
    gos <- matrix(unlist(gos, use.names=FALSE), ncol=3, byrow=TRUE)
    if (length(evidence) == 1 && is.na(evidence)) {
      from.evi <- rep(TRUE, nrow(gos))
    } else {
      from.evi <- gos[,2] %in% evidence
    }
    if (length(category) == 1 && is.na(category)) {
      from.cat <- rep(TRUE, nrow(gos))
    } else {
      from.cat <- gos[,3] %in% category
    }
    return(gos[from.evi & from.cat,1])
  }
  lapply(l, pickGO, evidence=evidence, category=category)
}

#' Returns GO evidence codes.
#'
#' @references \code{?org.Bt.egGO}
#' @return Matrix of two columns, first column with codes, second column with description of codes.
#' @seealso \code{\link{pickGO}}
#' @export
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @examples
#' getEvidenceCodes()
getEvidenceCodes <- function() {
  codes <- c('IMP','inferred from mutant phenotype',
             'IGI','inferred from genetic interaction',
             'IPI','inferred from physical interaction',
             'ISS','inferred from sequence similarity',
             'IDA','inferred from direct assay',
             'IEP','inferred from expression pattern',
             'IEA','inferred from electronic annotation',
             'TAS','traceable author statement',
             'NAS','non-traceable author statement',
             'ND','no biological data available',
             'IC','inferred by curator ')
  return(matrix(codes, ncol=2, byrow=TRUE, dimnames=list(c(), c('Code','Description'))))
}

#' Performs quicker lookup for orthologs in homologe data packages
#'
#' Using the INPARANOID data packages such as \code{hom.Hs.inp.db} is very, very slow and can take up to 11 min (on this particular developers workstation).
#' This function introduces a new method that can do it in just 20 seconds (on the developers workstation).
#' In addition, it includes options for translating between different identifers both before and after the mapping.
#'
#' @param values Vector, coerced to character vector, of values needed mapping by homology.
#' @param mapping Homology mapping object, such as \code{hom.Hs.inpBOSTA} or \code{revmap(hom.Hs.inpBOSTA)}.
#' @param genus Character vector. 5 character INPARANOID style genus name of the mapping object, e.g. 'BOSTA' for both \code{hom.Hs.inpBOSTA} and \code{revmap(hom.Hs.inpBOSTA)}.
#' @param threshold Numeric value between 0 and 1. Only clustered homologues with a parwise score above the threshold is included.
#'                  The native implementation has this set to 1.
#' @param pre.from Mapping object if \code{values} needs translation before mapping.
#'             E.g. \code{values} are entrez and \code{hom.Hs.inpBOSTA} requires ENSEMBLPROT, \code{hom.Hs.inpAPIME} requires Refseq (?).
#'             Arguments \code{from} and \code{to} are just like in \code{\link{translate}}.
#' @param pre.to Second part of translation before mapping.
#' @param post.from Translate the result from homology mapping to a desired id; just like in \code{\link{translate}}.
#' @param post.to Second part of translation after mapping.
#' @param ... Additional arguments sent to \code{\link{translate}}.
#' @return List. Names of list corresponds to \code{values}, except those that could not be mapped nor translated.
#'               Entries are character vectors.
#' @references \code{?hom.Hs.inp.db} - \url{http://inparanoid.sbc.su.se/}
#'
#'  Berglund, A.C., Sjolund, E., Ostlund, G., Sonnhammer, E.L.L. (2008)
#'  InParanoid 6: eukaryotic ortholog clusters with inparalogs
#'  \emph{Nucleic Acids Res.} \bold{36}:D263--266
#'
#'  O'Brien, K.P., Maido, R., Sonnhammer, E.L.L (2005)
#'  Inparanoid: A Comprehensive Database of Eukaryotic Orthologs
#'  \emph{NAR} \bold{33}:D476--D480
#'
#'  Remm, M., Storm, C.E.V, Sonnhammer, E.L.L (2001)
#'  Automatic clustering of orthologs and in-paralogs from pairwise species comparisons
#'  \emph{J. Mol. Biol.} \bold{314}:1041--1052
#'
#' @seealso \code{\link{translate}}, \code{\link{.getTableName}}, \code{\link{mapLists}}
#' @export
#' @author Stefan McKinnon Edwards \email{stefan.hoj-edwards@@agrsci.dk}
#' @examples
#' tmp <-1
#'

getOrthologs <- function(values, mapping, genus, threshold=1,
                         pre.from=NULL, pre.to=NULL,
                         post.from=NULL, post.to=NULL,
                         ...) {
  values <- as.character(values)
  values <- unique(values)

  threshold <- as.numeric(threshold)
  threshold <- max(0, min(threshold, 1))

  # Check if we do some translating first:
  if (!is.null(pre.from)) {
    trans1 <- translate(values, pre.from, pre.to, ...)
    values <- unlist(trans1, use.names=F)
  }

  # Check validity of `mapping` and genus.
  stopifnot(dbmeta(dbconn(mapping), "DBSCHEMA") == 'INPARANOID_DB')
  stopifnot(nchar(genus) == 5)
  .dbEscapeString(genus)
  genus <- toupper(genus)

  # Get the table name and check that it exists
  tbl <- .getTableName(genus)

  if ((length(tbl) == 0) | !(tbl %in% dbListTables(dbconn(mapping))))
    stop('Provided genus was not recognized for the mapping object.')

  # Do the DB magic
  conn <- dbConnect(dbDriver('SQLite'), '')   # Create a temporary table.
  dbSendQuery(conn, paste("ATTACH DATABASE '",dbfile(mapping),"' AS hom", sep=''))
  res <- dbSendQuery(conn, 'CREATE TABLE input (id1 TEXT);')
  dbClearResult(res)
  dbWriteTable(conn, 'input', data.frame(id1=values), row.names = FALSE, overwrite=FALSE, append=TRUE )
  #dbSendQuery(conn, 'BEGIN TRANSACTION')
  #bwahh <- sapply(values, function(s) dbSendQuery(conn, paste('INSERT INTO input (id1) VALUES ("',s,'")', sep='')))
  #dbSendQuery(conn, 'COMMIT TRANSACTION')

  # Proceed as normal, now that we have loaded our data. :)
  dbSendQuery(conn, sprintf('CREATE TABLE clusters AS SELECT id1, clust_id FROM input INNER JOIN hom.%s as inp ON inp.inp_id=id1 WHERE inp.score >= %i AND inp.seed_status = "100%%"', tbl, threshold))
  #res <- dbGetQuery(conn, sprintf('SELECT id1, clust_id FROM input INNER JOIN hom.%s as inp ON inp.inp_id=id1', tbl))
  isnot <- ifelse(direction(mapping) == 1, 'IS', 'IS NOT')
  res <- dbGetQuery(conn, sprintf('SELECT id1, inp_id FROM clusters INNER JOIN hom.%s as inp ON inp.clust_id=clusters.clust_id WHERE inp.score >= %i AND inp.species %s "%s" AND inp.seed_status = "100%%"', tbl, threshold, isnot, genus))

  dbDisconnect(conn)

  # A bit of cleaning up
  #res.list <- unstack(res, res$inp_id ~ res$id1)
  res.list <- split(res$inp_id, f=factor(res$id1, levels=unique(res$id1)))

  if (!is.null(pre.from)) {
    res.list <- mapLists(trans1, res.list)
  }
  if (!is.null(post.from)) {
    trans2 <- translate(res$inp_id, post.from, post.to, ...)
    res.list <- mapLists(res.list, trans2)
  }
  res.list <- lapply(res.list, unique)

  return(res.list)
}


#'  Gets the table name from the INPARANOID style genus names.
#' @param genus 5 character INPARANOID genus name, such as "BOSTA", "HOMSA" or "MUSMU".
#' @return Table name for genus.
#' @references \url{http://www.bioconductor.org/packages/release/bioc/html/AnnotationDbi.html}
#' @author Stefan McKinnon Edwards \email{stefanm.edwards@@agrsci.dk}
.getTableName <- function(genus) {
  # Find the AnnotationDbi source
  # Open createAnnObjs.INPARANOID_DB.R
  # Find the long bit of code that create variable 'fields' with all the species and the 5-character genus names.
  # Create variable fields from it and use the next three lines to swap names and values.
  # tmp <- fields
  # fields <- names(fields)
  # names(fields) <- tmp
  # Execute 'fix(fields)' and paste the result down here:
  fields <- structure(c("Acyrthosiphon_pisum", "Aedes_aegypti", "Anopheles_gambiae",
                        "Apis_mellifera", "Arabidopsis_thaliana", "Aspergillus_fumigatus",
                        "Batrachochytrium_dendrobatidis", "Bombyx_mori", "Bos_taurus",
                        "Branchiostoma_floridae", "Brugia_malayi", "Caenorhabditis_brenneri",
                        "Caenorhabditis_briggsae", "Caenorhabditis_elegans", "Caenorhabditis_japonica",
                        "Caenorhabditis_remanei", "Candida_albicans", "Candida_glabrata",
                        "Canis_familiaris", "Capitella_spI", "Cavia_porcellus", "Chlamydomonas_reinhardtii",
                        "Ciona_intestinalis", "Ciona_savignyi", "Coccidioides_immitis",
                        "Coprinopsis_cinereus", "Cryptococcus_neoformans", "Cryptosporidium_hominis",
                        "Cryptosporidium_parvum", "Culex_pipiens", "Cyanidioschyzon_merolae",
                        "Danio_rerio", "Daphnia_pulex", "Debaryomyces_hansenii", "Dictyostelium_discoideum",
                        "Drosophila_ananassae", "Drosophila_grimshawi", "Drosophila_melanogaster",
                        "Drosophila_mojavensis", "Drosophila_pseudoobscura", "Drosophila_virilis",
                        "Drosophila_willistoni", "Entamoeba_histolytica", "Equus_caballus",
                        "Escherichia_coliK12", "Fusarium_graminearum", "Gallus_gallus",
                        "Gasterosteus_aculeatus", "Giardia_lamblia", "Helobdella_robusta",
                        "Homo_sapiens", "Ixodes_scapularis", "Kluyveromyces_lactis",
                        "Leishmania_major", "Lottia_gigantea", "Macaca_mulatta", "Magnaporthe_grisea",
                        "Monodelphis_domestica", "Monosiga_brevicollis", "Mus_musculus",
                        "Nasonia_vitripennis", "Nematostella_vectensis", "Neurospora_crassa",
                        "Ornithorhynchus_anatinus", "Oryza_sativa", "Oryzias_latipes",
                        "Ostreococcus_tauri", "Pan_troglodytes", "Pediculus_humanus",
                        "Physcomitrella_patens", "Phytophthora_ramorum", "Phytophthora_sojae",
                        "Plasmodium_falciparum", "Plasmodium_vivax", "Pongo_pygmaeus",
                        "Populus_trichocarpa", "Pristionchus_pacificus", "Puccinia_graminis",
                        "Rattus_norvegicus", "Rhizopus_oryzae", "Saccharomyces_cerevisiae",
                        "Schistosoma_mansoni", "Schizosaccharomyces_pombe", "Sclerotinia_sclerotiorum",
                        "Sorghum_bicolor", "Stagonospora_nodorum", "Strongylocentrotus_purpuratus",
                        "Takifugu_rubripes", "Tetrahymena_thermophila", "Tetraodon_nigroviridis",
                        "Thalassiosira_pseudonana", "Theileria_annulata", "Theileria_parva",
                        "Tribolium_castaneum", "Trichomonas_vaginalis", "Trichoplax_adhaerens",
                        "Trypanosoma_cruzi", "Ustilago_maydis", "Xenopus_tropicalis",
                        "Yarrowia_lipolytica"), .Names = c("ACYPI", "AEDAE", "ANOGA",
                                                           "APIME", "ARATH", "ASPFU", "BATDE", "BOMMO", "BOSTA", "BRAFL",
                                                           "BRUMA", "CAEBRE", "CAEBR", "CAEEL", "CAEJA", "CAERE", "CANAL",
                                                           "CANGL", "CANFA", "CAPSP", "CAVPO", "CHLRE", "CIOIN", "CIOSA",
                                                           "COCIM", "COPCI", "CRYNE", "CRYHO", "CRYPA", "CULPI", "CYAME",
                                                           "DANRE", "DAPPU", "DEBHA", "DICDI", "DROAN", "DROGR", "DROME",
                                                           "DROMO", "DROPS", "DROVI", "DROWI", "ENTHI", "EQUCA", "ESCCO",
                                                           "FUSGR", "GALGA", "GASAC", "GIALA", "HELRO", "HOMSA", "IXOSC",
                                                           "KLULA", "LEIMA", "LOTGI", "MACMU", "MAGGR", "MONDO", "MONBR",
                                                           "MUSMU", "NASVI", "NEMVE", "NEUCR", "ORNAN", "ORYSA", "ORYLA",
                                                           "OSTTA", "PANTR", "PEDPA", "PHYPA", "PHYRA", "PHYSO", "PLAFA",
                                                           "PLAVI", "PONPY", "POPTR", "PRIPA", "PUCGR", "RATNO", "RHIOR",
                                                           "SACCE", "SCHMA", "SCHPO", "SCLSC", "SORBI", "STANO", "STRPU",
                                                           "TAKRU", "TETTH", "TETNI", "THAPS", "THEAN", "THEPA", "TRICA",
                                                           "TRIVA", "TRIAD", "TRYCR", "USTMA", "XENTR", "YARLI"))

  genus <- toupper(genus)
  return(removeNAs(fields[genus]))
}

#'  Private Escape string
#'
#'  Does not escape strings, but raises an error if any character expect normal letters and underscores are found in the string.
#' @param str String to test
#' @param raise.error Logical, whether to raise an error or not.
#' @return Invisible logical
#'
.dbEscapeString <- function(str, raise.error=TRUE) {
  matches <- grepl('[^A-Za-z0-9_]+', str)
  res <- any(matches)
  if (raise.error & res) stop('Supplied SQL string contains illegal characters!')
  invisible(res)
}
