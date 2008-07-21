setGeneric("nNodes", function(x) {
    standardGeneric("nNodes")
})

setGeneric("nEdges", function(x) {
    standardGeneric("nEdges")
})

setGeneric("edges", function(x,order,...) {
    standardGeneric("edges")
})

setGeneric("rootEdge", function(x,order,...) {
    standardGeneric("rootEdge")
})

setGeneric("isRooted", function(x) {
    standardGeneric("isRooted")
})

setGeneric("rootNode", function(x) {
    standardGeneric("rootNode")
})

setGeneric("rootNode<-", function(x,value) {
    standardGeneric("rootNode<-")
})

setGeneric("hasEdgeLength", function(x) {
    standardGeneric("hasEdgeLength")
})

setGeneric("edgeLength", function(x) {
    standardGeneric("edgeLength")
})

setGeneric("hasNodeLabels", function(x) {
    standardGeneric("hasNodeLabels")
})

setGeneric("hasEdgeLabels", function(x) {
    standardGeneric("hasEdgeLabels")
})

setGeneric("labels")

setGeneric("labels<-",
           function(object,...,value) {
               standardGeneric("labels<-")
           })

setGeneric("nodeLabels", function(x) {
    standardGeneric("nodeLabels")
})
setGeneric("nodeLabels<-",
           function(object,...,value) {
               standardGeneric("nodeLabels<-")
           })

setGeneric("edgeLabels", function(x) {
    standardGeneric("edgeLabels")
})

setGeneric("edgeLabels<-",
           function(object,...,value) {
               standardGeneric("edgeLabels<-")
           })

setGeneric("print")

setGeneric("tdata", function(x,...) {
    standardGeneric("tdata")
})

setGeneric("hasNodeData", function(x) {
    standardGeneric("hasNodeData")
})

setGeneric("na.omit")

setGeneric("reorder", def = function(object, type = 'pruningwise') {
    reorder.prune <- function(edge, tips, root = tips + 1) {
        ## if(is.null(root)) {
        ##     root <- tips + 1
        ## }
        ## if(root <= tips) {return()}
        index <- edge[, 1] == root
        nextr <- edge[index, 2]
        ## paths <- apply(as.matrix(nextr), 1, reorder, edge = edge, tips = tips)
        nord <- NULL
        for(i in nextr) {
            if(i <= tips) {next()}
            nord <- c(nord, myorder(edge, tips, root = i))
        }
        c(nord, which(index))
    }
    if(type == 'pruningwise') {
        index <- reorder.prune(phy@edge, length(phy@tip.label))
    }
    phy@edge        <- phy@edge[index, ]
    phy@edge.label  <- phy@edge.label[index]
    phy@edge.length <- phy@edge.length[index]
    phy
},
    useAsDefault = function(object, type = 'pruningwise') {
        reorder.prune <- function(edge, tips, root = tips + 1) {
            ## if(is.null(root)) {
            ##     root <- tips + 1
            ## }
            ## if(root <= tips) {return()}
            index <- edge[, 1] == root
            nextr <- edge[index, 2]
            ## paths <- apply(as.matrix(nextr), 1, reorder, edge = edge, tips = tips)
            nord <- NULL
            for(i in nextr) {
                if(i <= tips) {next()}
                nord <- c(nord, myorder(edge, tips, root = i))
            }
            c(nord, which(index))
        }
        if(type == 'pruningwise') {
            index <- reorder.prune(phy@edge, length(phy@tip.label))
        }
        phy@edge        <- phy@edge[index, ]
        phy@edge.label  <- phy@edge.label[index]
        phy@edge.length <- phy@edge.length[index]
        phy
})



###################
## Function .genlab
###################
## recursive function to have labels of constant length
## base = a character string
## n = number of labels
.genlab <- function(base,n) {
    if (n<=0) return("")
    s <- seq(length.out=n)
    fw <- max(nchar(as.character(s)))
    numstr <- formatC(s,flag="0",width=fw)
    paste(base,numstr,sep="")
}


