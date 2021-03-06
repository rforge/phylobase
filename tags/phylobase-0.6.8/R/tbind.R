## appropriate behavior ???

## IF all missing data -- create multiPhylo4
## IF some have data -- create multiPhylo4d (user can coerce to multiPhylo4)
## IF (checkData) then stop if all data not identical to first data
##
## need constructors for multiPhylo4, multiPhylo4d!!
## FIXME: need code to construct tree.names ...

## function to bind trees together into a multi-tree object
tbind <- function(...,checkData=TRUE) {
    L <- list(...)
    namevec <- names(L)
    treeclasses <- c("multiPhylo4d","multiPhylo4","phylo4","phylo4d")
    tdataclasses <- c("multiPhylo4d","phylo4d")
    classes <- sapply(L,class)
    if (!all(classes %in% treeclasses)) {
        stop("all elements must be trees or multitrees")
    }
    hasData <- any(classes %in% tdataclasses)
    allData <- all(classes %in% tdataclasses)
    xfun <- function(x) {
        switch(class(x),
               phylo4=x,


#' Combine a phylogenetic tree with data
#' 
#' \code{phylo4d} is a generic constructor which merges a phylogenetic tree
#' with data frames to create a combined object of class \code{phylo4d}
#' 
#' 
#' You can provide several data frames to define traits associated with tip
#' and/or internal nodes. By default, data row names are used to link data to
#' nodes in the tree, with any number-like names (e.g., \dQuote{10}) matched
#' against node ID numbers, and any non-number-like names (e.g., \dQuote{n10})
#' matched against node labels. Alternative matching rules can be specified by
#' passing additional arguments to \code{formatData}; these include positional
#' matching, matching exclusively on node labels, and matching based on a
#' column of data rather than on row names. See \code{\link{formatData}} for
#' more information.
#' 
#' Matching rules will apply the same way to all supplied data frames.  This
#' means that you need to be consistent with the row names of your data frames.
#' It is good practice to use tip and node labels (or node numbers) when you
#' combine data with a tree.
#' 
#' If you provide both \code{tip.data} and \code{node.data}, the treatment of
#' columns with common names will depend on the \code{merge.data} argument. If
#' TRUE, columns with the same name in both data frames will be merged; when
#' merging columns of different data types, coercion to a common type will
#' follow standard R rules. If \code{merge.data} is FALSE, columns with common
#' names will be preserved independently, with \dQuote{.tip} and \dQuote{.node}
#' appended to the names. This argument has no effect if \code{tip.data} and
#' \code{node.data} have no column names in common.
#' 
#' If you provide \code{all.data} along with either of \code{tip.data} and
#' \code{node.data}, it must have distinct column names, otherwise an error
#' will result. Additionally, although supplying columns with the same names
#' \emph{within} data frames is not illegal, automatic renaming for uniqeness
#' may lead to surprising results, so this practice should be avoided.
#' 
#' @name phylo4d
#' @aliases phylo4d phylo4d-methods phylo4d,phylo4-method
#' phylo4d,phylo4d-method phylo4d,matrix-method phylo4d,phylo-method
#' @docType methods
#' @param x an object of class \code{phylo4}, \code{phylo} or a matrix of edges
#' (see above)
#' @param tip.data a data frame (or object to be coerced to one) containing
#' only tip data (Optional)
#' @param node.data a data frame (or object to be coerced to one) containing
#' only node data (Optional)
#' @param all.data a data frame (or object to be coerced to one) containing
#' both tip and node data (Optional)
#' @param merge.data if both \code{tip.data} and \code{node.data} are provided,
#' should columns with common names will be merged together (default TRUE) or
#' not (FALSE)? See details.
#' @param metadata any additional metadata to be passed to the new object
#' @param edge.length Edge (branch) length. (Optional)
#' @param tip.label A character vector of species names (names of "tip" nodes).
#' (Optional)
#' @param node.label A character vector of internal node names. (Optional)
#' @param edge.label A character vector of edge (branch) names. (Optional)
#' @param order character: tree ordering (allowable values are listed in
#' \code{phylo4_orderings}, currently "unknown", "preorder" (="cladewise" in
#' \code{ape}), and "postorder", with "cladewise" and "pruningwise" also
#' allowed for compatibility with \code{ape})
#' @param annote any additional annotation data to be passed to the new object
#' @param check.node.labels if \code{x} is of class \code{phylo}, use either
#' \dQuote{keep} (the default) to retain internal node labels, \dQuote{drop} to
#' drop them, or \dQuote{asdata} to convert them to numeric tree data. This
#' argument is useful if the \code{phylo} object has non-unique node labels or
#' node labels with informative data (e.g., posterior probabilities).
#' @param \dots further arguments to be passed to \code{\link{formatData}}.
#' Notably, these additional arguments control the behavior of the constructor
#' in the case of missing/extra data and where to look for labels in the case
#' of non-unique labels that cannot be stored as row names in a data frame.
#' @return An object of class \linkS4class{phylo4d}.
#' @note Checking on matches between the tree and the data will be done by the
#' validity checker (label matches between data and tree tips, number of rows
#' of data vs. number of nodes/tips/etc.)
#' @section Methods: \describe{ \item{x = "phylo4"}{merges a tree of class
#' \code{phylo4} with a data.frame into a \code{phylo4d} object} \item{x =
#' "matrix"}{merges a matrix of tree edges similar to the edge slot of a
#' \code{phylo4} object (or to \$edge of a \code{phylo} object) with a
#' data.frame into a \code{phylo4d} object} \item{x = "phylo"}{merges a tree of
#' class \code{phylo} with a data.frame into a \code{phylo4d} object } }
#' @author Ben Bolker, Thibaut Jombart, Steve Kembel, Francois Michonneau, Jim
#' Regetz
#' @seealso \code{\link{coerce-methods}} for translation functions. The
#' \linkS4class{phylo4d} class, the \code{\link{formatData}} function to check
#' the validity of \code{phylo4d} objects; \linkS4class{phylo4} class and
#' \link{phylo4} constructor.
#' @keywords misc
#' @examples
#' 
#' treeOwls <- "((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3);"
#' tree.owls.bis <- ape::read.tree(text=treeOwls)
#' try(phylo4d(as(tree.owls.bis,"phylo4"),data.frame(wing=1:3)), silent=TRUE)
#' obj <- phylo4d(as(tree.owls.bis,"phylo4"),data.frame(wing=1:3), match.data=FALSE)
#' obj
#' print(obj)
#' 
#' ####
#' 
#' data(geospiza_raw)
#' geoTree <- geospiza_raw$tree
#' geoData <- geospiza_raw$data
#' 
#' ## fix differences in tip names between the tree and the data
#' geoData <- rbind(geoData, array(, dim = c(1,ncol(geoData)),
#'                   dimnames = list("olivacea", colnames(geoData))))
#' 
#' ### Example using a tree of class 'phylo'
#' exGeo1 <- phylo4d(geoTree, tip.data = geoData)
#' 
#' ### Example using a tree of class 'phylo4'
#' geoTree <- as(geoTree, "phylo4")
#' 
#' ## some random node data
#' rNodeData <- data.frame(randomTrait = rnorm(nNodes(geoTree)),
#'                         row.names = nodeId(geoTree, "internal"))
#' 
#' exGeo2 <- phylo4d(geoTree, tip.data = geoData, node.data = rNodeData)
#' 
#' ### Example using 'merge.data'
#' data(geospiza)
#' trGeo <- extractTree(geospiza)
#' tDt <- data.frame(a=rnorm(nTips(trGeo)), row.names=nodeId(trGeo, "tip"))
#' nDt <- data.frame(a=rnorm(nNodes(trGeo)), row.names=nodeId(trGeo, "internal"))
#' 
#' (matchData1 <- phylo4d(trGeo, tip.data=tDt, node.data=nDt, merge.data=FALSE))
#' (matchData2 <- phylo4d(trGeo, tip.data=tDt, node.data=nDt, merge.data=TRUE))
#' 
#' ## Example with 'all.data'
#' nodeLabels(geoTree) <- as.character(nodeId(geoTree, "internal"))
#' rAllData <- data.frame(randomTrait = rnorm(nTips(geoTree) + nNodes(geoTree)),
#' row.names = labels(geoTree, 'all'))
#' 
#' exGeo5 <- phylo4d(geoTree, all.data = rAllData)
#' 
#' ## Examples using 'rownamesAsLabels' and comparing with match.data=FALSE
#' tDt <- data.frame(x=letters[1:nTips(trGeo)],
#'                   row.names=sample(nodeId(trGeo, "tip")))
#' tipLabels(trGeo) <- as.character(sample(1:nTips(trGeo)))
#' (exGeo6 <- phylo4d(trGeo, tip.data=tDt, rownamesAsLabels=TRUE))
#' (exGeo7 <- phylo4d(trGeo, tip.data=tDt, rownamesAsLabels=FALSE))
#' (exGeo8 <- phylo4d(trGeo, tip.data=tDt, match.data=FALSE))
#' 
#' ## generate a tree and some data
#' set.seed(1)
#' p3 <- ape::rcoal(5)
#' dat <- data.frame(a = rnorm(5), b = rnorm(5), row.names = p3$tip.label)
#' dat.defaultnames <- dat
#' row.names(dat.defaultnames) <- NULL
#' dat.superset <- rbind(dat, rnorm(2))
#' dat.subset <- dat[-1, ]
#' 
#' ## create a phylo4 object from a phylo object
#' p4 <- as(p3, "phylo4")
#' 
#' ## create phylo4d objects with tip data
#' p4d <- phylo4d(p4, dat)
#' ###checkData(p4d)
#' p4d.sorted <- phylo4d(p4, dat[5:1, ])
#' try(p4d.nonames <- phylo4d(p4, dat.defaultnames))
#' p4d.nonames <- phylo4d(p4, dat.defaultnames, match.data=FALSE)
#' 
#' \dontrun{
#' p4d.subset <- phylo4d(p4, dat.subset)
#' p4d.subset <- phylo4d(p4, dat.subset)
#' try(p4d.superset <- phylo4d(p4, dat.superset))
#' p4d.superset <- phylo4d(p4, dat.superset)
#' }
#' 
#' ## create phylo4d objects with node data
#' nod.dat <- data.frame(a = rnorm(4), b = rnorm(4))
#' p4d.nod <- phylo4d(p4, node.data = nod.dat, match.data=FALSE)
#' 
#' 
#' ## create phylo4 objects with node and tip data
#' p4d.all1 <- phylo4d(p4, node.data = nod.dat, tip.data = dat, match.data=FALSE)
#' nodeLabels(p4) <- as.character(nodeId(p4, "internal"))
#' p4d.all2 <- phylo4d(p4, all.data = rbind(dat, nod.dat, match.data=FALSE))
#' 
#' 
#' 
               phylo4d=extractTree(x),
               multiPhylo4=x@phylolist,
               multiPhylo4d=suppressWarnings(as("multiPhylo4",x)@phylolist))}
    ## decompose multi-trees into lists
    treelist <- unlist(lapply(L,xfun))
    if (hasData) alldat <- lapply(L[classes %in% tdataclasses], tdata,
        type="tip")
    hasNodeData <- sapply(L[classes %in% tdataclasses], hasNodeData)
    if (any(hasNodeData)) warning("internal node data discarded")
    if (checkData) {
        ident <- sapply(alldat,identical,y=alldat[[1]])
        if (!all(ident)) stop(paste("tip data sets differ"))
    } ## ?? implement code to check which ones differ (taking
    ## null/multiple values in original set into account)
    if (hasData) return(new("multiPhylo4d",phylolist=treelist,
                            tip.data=alldat[[1]]))
    return(new("multiPhylo4",phylolist=treelist))
}
            

