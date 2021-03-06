\name{phylo4-accessors}
\docType{methods}
\alias{nNodes}
\alias{nNodes-methods}
\alias{nNodes,phylo4-method}
\alias{nTips}
\alias{nTips-methods}
\alias{nTips,phylo4-method}
\alias{nTips,ANY-method}
\alias{labels<-}
\alias{labels,phylo4-method}
\alias{labels<-,phylo4-method}
\alias{labels<-,phylo4d-method}
\alias{edges}
\alias{edges-methods}
\alias{edges,phylo4-method}
\alias{nEdges}
\alias{nEdges-methods}
\alias{nEdges,phylo4-method}
\alias{hasEdgeLength}
\alias{hasEdgeLength-methods}
\alias{hasEdgeLength,phylo4-method}
\alias{edgeLength}
\alias{edgeLength-methods}
\alias{edgeLength,phylo4-method}
\alias{isRooted}
\alias{isRooted-methods}
\alias{isRooted,phylo4-method}
\alias{rootEdge}
\alias{rootEdge-methods}
\alias{rootEdge,phylo4-method}
\alias{rootNode}
\alias{rootNode-methods}
\alias{rootNode,phylo4-method}
\alias{rootNode<-}
\alias{rootNode<-,phylo4-method}
\alias{hasNodeLabels}
\alias{hasNodeLabels-methods}
\alias{hasNodeLabels,phylo4-method}
\alias{hasNodeData}
\alias{hasNodeData-methods}
\alias{hasNodeData,phylo4d-method}
\alias{nodeLabels}
\alias{nodeLabels-methods}
\alias{nodeLabels,phylo4-method}
\alias{nodeLabels<-}
\alias{nodeLabels<-,phylo4-method}
\alias{nodeLabels<-,phylo4d-method}
\alias{hasEdgeLabels}
\alias{hasEdgeLabels-methods}
\alias{hasEdgeLabels,phylo4-method}
\alias{edgeLabels}
\alias{edgeLabels<-}
\alias{edgeLabels-methods}
\alias{edgeLabels,phylo4-method}
\alias{edgeLabels<-,phylo4-method}
\alias{tdata}
\alias{tdata<-}
\alias{tdata-methods}
\alias{tdata,phylo4d-method}
\alias{tdata<-,phylo4d-method}
\title{Methods for S4 phylogeny classes}
\description{
   Generic methods for phylogenetic trees represented as S4 classes
}
\section{Methods}{
\describe{
  \item{nTips}{\code{signature(object="phylo4")}: number of tips}
  \item{labels}{\code{signature(object = "phylo4")}: tip labels}
  \item{nNodes}{\code{signature(object="phylo4")}: number of internal nodes}
  \item{hasNodeLabels}{\code{signature(object = "phylo4")}: whether
    tree has (internal) node labels}
  \item{hasNodeData}{\code{signature(object = "phylo4d")}: whether
    tree has (internal) node data}
  \item{nodeLabels}{\code{signature(object = "phylo4")}: internal
    node labels}
  \item{nEdges}{\code{signature(object = "phylo4")}: number of edges}
  \item{edges}{\code{signature(object = "phylo4")}: edge matrix}
  \item{hasEdgeLength}{\code{signature(object = "phylo4")}: whether
    tree has edge (branch) lengths}
  \item{edgeLength}{\code{signature(object = "phylo4")}: edge (branch)
    lengths (or NULL if none)}
  \item{hasEdgeLabels}{\code{signature(object = "phylo4")}: whether
    tree has (internal) edge labels}
  \item{edgeLabels}{\code{signature(object = "phylo4")}: internal
    edge labels}

  \item{isRooted}{\code{signature(object = "phylo4")}: whether tree is
    rooted (i.e. has explicit root edge defined \emph{or} root node has
    <= 2 descendants)}
  \item{rootEdge}{\code{signature(object = "phylo4")}: root edge}
  \item{tdata}{\code{signature(object = "phylo4d")}: trait data}
}
}
\section{usage}{
  \S4method{tdata}{phylo4d}(x, which = "tip", \dots)
  \S4method{tdata<-}{phylo4d}(object, which = "tip", \dots, value)
  \S4method{edgeLength}{phylo4}(x, which)
}
\arguments{
  \item{which}{(tdata) which data to extract: "tip" (tips only), "node"
    (internal nodes only), "allnode" (tips and internal nodes), "edge"
    (edges) (edgeLength) which edge lengths to extract (indexed by
    descendant node)}
  \item{x}{a phylo4d object}
  \item{object}{a phylo4d object}
  \item{value}{a data frame}
}
\examples{
data(geospiza)
edgeLength(geospiza,5)
edgeLength(geospiza,"olivacea")
edgeLength(geospiza,5:7)
}
\keyword{methods}
