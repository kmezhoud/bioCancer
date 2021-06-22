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
#' library(org.Bt.eg.db)
#' genes <- c(280705, 280706, 100327208)
#' GO <- translate(genes, org.Bt.egGO)
#' # Get all biological processes:
#' pickGO(GO, category='BP')
#' # Get all ontologies with experimental evidence:
#' pickGO(GO, evidence=c('IMP','IGI','IPI','ISS','IDA','IEP','IEA'))
#pickGO <- function(l, evidence=c('IMP','IGI','IPI','ISS','IDA','IEP','IEA','TAS','NAS','ND','IC'), category=c('BP','CC','MF')) {
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
