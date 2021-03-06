###################################
## phylo4d class
## extend: phylo with data
setClass("phylo4d",
         representation(tip.data="data.frame",
                        node.data="data.frame"),
         ##                        edgedata="data.frame"),
         prototype = list( tip.data = data.frame(NULL),
           node.data = data.frame(NULL) ),
         ##all.data = data.frame(NULL) ),
         validity = function(object) {
             ## FIXME: finish this by intercepting FALSE, char string, etc.
             check1 <- check_data(object)
             check2 <- check_phylo4(object)
         },
         contains="phylo4")

######################
## phylo4d constructor
######################
## TEST ME
## '...' recognized args for data are tipdata and nodedata.
## other recognized options are those known by the phylo4 constructor
##

## generic
setGeneric("phylo4d", function(x, ...) { standardGeneric("phylo4d")} )

## first arg is a phylo4
setMethod("phylo4d", c("phylo4"),
   function(x, tip.data = NULL, node.data = NULL, all.data = NULL,
            merge.tip.node = TRUE, ...) {

       classData <- function(someData) {
           if(!is.null(someData)) {
               if(is.vector(someData)) someData <- as.data.frame(someData)
               if(!is.data.frame(someData)) {
                   nmSomedata <- deparseSubstitute(someData)
                   return(paste(nmSomeData, "must be a vector or a data frame"))
               }
               return(TRUE)
           }
           else return(TRUE)
       }

       if(is.character(checkval <- check_phylo4(x))) stop(checkval)

       if(is.character(checkClass <- classData(all.data))) stop(checkClass)
       if(is.character(checkClass <- classData(tip.data))) stop(checkClass)
       if(is.character(checkClass <- classData(node.data))) stop(checkClass)

       res <- new("phylo4d")
       res@edge <- x@edge
       res@edge.length <- x@edge.length
       res@Nnode <- x@Nnode
       res@tip.label <- x@tip.label
       res@node.label <- x@node.label
       res@edge.label <- x@edge.label
       res@root.edge <- x@root.edge

       if(!is.null(all.data)) {
           tmpData <- all.data
           if(!is.null(tip.data)) {
               emptyNodeData <- array(, dim = c(nNodes(x), ncol(tip.data)),
                                      dimnames = list(nodeLabels(x), colnames(tip.data)))
               tmpTipData <- rbind(tip.data, emptyNodeData)
               ## TODO? - have a test on names between
               tmpTipData <- tmpTipData[match(rownames(all.data), rownames(tmpTipData)) ,, drop = FALSE]
               tmpData <- cbind(all.data, tmpTipData)
           }
           if(!is.null(node.data)) {
               emptyTipData <- array(, dim = c(nTips(x), ncol(node.data)),
                                     dimnames = list(labels(x), colnames(node.data)))
               tmpNodeData <- rbind(emptyTipData, node.data)
               ## TODO? - add test
               tmpNodeData <- tmpNodeData[match(rownames(all.data), rownames(tmpNodeData)) ,, drop = FALSE]
               tmpData <- cbind(tmpData, tmpNodeData)

           }
           res@tip.data <- tmpData[1:nTips(x) ,, drop = FALSE]
           res@node.data <- tmpData[-(1:nTips(x)) ,, drop = FALSE]
       }

       else {
           if((!is.null(tip.data) && (!is.null(node.data)))) {
               if(identical(colnames(tip.data), colnames(node.data)) &&  merge.tip.node) {
                   tmpAllData <- rbind(tip.data, node.data)
                   res@tip.data <- tmpAllData[1:nTips(x) ,, drop = FALSE]
                   res@node.data <- tmpAllData[-(1:nTips(x)) ,, drop = FALSE]
               }
               else {
                   emptyTipData <- array(, dim = c(nTips(x), ncol(node.data)),
                                           dimnames = list(labels(x), colnames(node.data)))
                   emptyNodeData <- array(, dim = c(nNodes(x), ncol(tip.data)),
                                            dimnames = list(nodeLabels(x), colnames(tip.data)))
                   tmpTipData <- rbind(tip.data, emptyNodeData)
                   tmpNodeData <- rbind(emptyTipData, node.data)
                   tmpData <- cbind(tmpTipData, tmpNodeData)
                   res@tip.data <- tmpData[1:nTips(x) ,, drop = FALSE]
                   res@node.data <- tmpData[-(1:nTips(x)) ,, drop = FALSE]
               }
           }
           else {
               ## at this point provide NULL data frame for empty arguments
               if(is.null(tip.data)) tip.data <- data.frame(NULL)
               if(is.null(node.data)) node.data <- data.frame(NULL)

               res@tip.data <- tip.data
               res@node.data <- node.data
           }
       }

       check_data(res, ...)
       res <- attach_data(res,...)
       return(res)

})

## first arg is a matrix of edges
setMethod("phylo4d", c("matrix"), function(x, tip.data=NULL, node.data=NULL, all.data=NULL, ...){
    tree <- phylo4(edge=x,...)
    res <- phylo4d(tree, tip.data, node.data, all.data, ...)
    return(res)
})

## first arg is a phylo
setMethod("phylo4d", c("phylo"), function(x, tip.data=NULL, node.data=NULL, all.data=NULL, ...){
    tree <- as(x, "phylo4")
    res <- phylo4d(tree, tip.data, node.data, all.data, ...)
    return(res)
})

### first arg is a phylo4d
setMethod("phylo4d", c("phylo4d"), function(x, ...) {
          stop("Your object is already a phylo4d object. If you want to modify the data attached to it look help for tdata()<-")
      })

