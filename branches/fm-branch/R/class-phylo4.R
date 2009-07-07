setClass("phylo4",
         representation(edge = "matrix",
                        edge.length = "numeric",
                        Nnode = "integer",
                        node.label = "character",
                        tip.label = "character",
                        edge.label = "character",
                        order = "character"),
         prototype = list(
                        edge = matrix(nrow = 0, ncol = 2,
                            dimname = list(NULL, c("ancestor", "descendant"))),
                        edge.length = numeric(0),
                        Nnode = as.integer(0),
                        tip.label = character(0),
                        node.label = character(0),
                        edge.label = character(0),
                        order = "unknown"
                       ),
         validity = checkPhylo4)

#####################
## Labels constructor
#####################

.createLabels <- function(value, ntips, nnodes, use.names = TRUE,
                         which = c("tip", "internal")) {

    which <- match.arg(which)

    ## set up final length of object to return
    lgthRes <- switch(which, tip=ntips, internal=nnodes, allnode=ntips+nnodes)

    ## create NA character vector of node labels
    res <- character(lgthRes)
    is.na(res) <- TRUE
    names(res) <- switch(which,
                         tip = 1:ntips,
                         internal = seq(from=ntips+1, length=lgthRes),
                         allnode = 1:(ntips+nnodes))


    ## if value is NULL
    if(is.null(value) || all(is.na(value))) {
        ## tip labels can't be NULL
        if(!identical(which, "internal")) {
            tipLbl <- .genlab("T", ntips)
            res[1:ntips] <- tipLbl
        }
    }
    ## if labels are provided
    else {
        ## check that not only numbers
        ##if(length(grep("[a-zA-Z]", value)) == 0)
        ##    stop("Labels need to contain characters. ",
        ##         "They can't just be numerical values")

        ## check that lengths match
        if(length(value) != lgthRes)
            stop("Number of labels does not match number of nodes.")

        ## check if vector 'value' has name, and if so match with node.label names
        if(use.names && !is.null(names(value))) {
            if(!all(names(value) %in% names(res)))
                stop("Names provided don't match internal labels names.")
            res[match(names(value), names(res))] <- value
        }
        else
            res[1:lgthRes] <- value
    }

    res
}

#####################
## phylo4 constructor
#####################

## generic
setGeneric("phylo4", function(x, ...) { standardGeneric("phylo4")} )

# ape orderings should be allowed for so we can import trees from ape e.g. during subsetting
phylo4_orderings <- c("unknown", "preorder", "postorder", "pruningwise", "cladewise")

## first arg is a matrix
setMethod("phylo4", "matrix",
    function(x, edge.length = NULL, tip.label = NULL, node.label = NULL,
             edge.label = NULL, order="unknown", ...) {

    ## edge
    edge <- x
    mode(edge) <- "integer"
    #if(any(is.na(edge))) stop("NA are not allowed in edge matrix")
    if(ncol(edge) > 2) warning("the edge matrix has more than two columns")
    edge <- as.matrix(edge[, 1:2])
    colnames(edge) <- c("ancestor", "descendant")

    ## edge.length
    if(!is.null(edge.length)) {
        if(!is.numeric(edge.length)) stop("edge.length is not numeric")
        edge.length <- edge.length
    } else {
        edge.length <- numeric(0)
    }

    if(length(edge.length) > 0) {
        if(length(edge.length) != nrow(edge))
            stop("The number of edge lengths is different from the number of edges.")
        ## FM - 2009-04-19
        ## edge.length is named according to the nodes the edge links together
        ## (ancestor-descendant). This should allow more robust edge/edge.length
        ## association and limit the problems associated with reordering trees.
        names(edge.length) <- paste(edge[,1], edge[,2], sep="-")
    }

    ## number of tips and number of nodes
    ntips <- sum(tabulate(na.omit(edge[, 1])) == 0)
    nnodes <- length(unique(na.omit(c(edge)))) - ntips

    ## tip.label
    tip.label <- .createLabels(value=tip.label, ntips=ntips, nnodes=nnodes,
                               which="tip")

    ## edge.label
    if(is.null(edge.label)) {
      edge.label <- character(0)
    } else if (length(edge.label)>0 && length(edge.label) != nrow(edge))
      stop("number of edge labels is not consistent with the number of edges")

    ## fill in the result
    res <- new("phylo4")
    res@edge <- edge
    res@edge.length <- edge.length
    res@Nnode <- nnodes
    res@tip.label <- tip.label
    res@node.label <- .createLabels(node.label, ntips=ntips, nnodes=nnodes,
                                    which="internal")
    res@edge.label <- edge.label
    res@order <- order

    ## checkPhylo4 will return a character string if object is
    ##  bad, otherwise TRUE
    if (is.character(checkval <- checkPhylo4(res))) stop(checkval)
    return(res)
})

## first arg is a phylo
setMethod("phylo4", c("phylo"), function(x, check.node.labels=c("keep",
  "drop")){

  check.node.labels <- match.arg(check.node.labels)
  if (check.node.labels == "drop") x$node.label <- NULL
  res <- as(x, "phylo4")

  return(res)
})
