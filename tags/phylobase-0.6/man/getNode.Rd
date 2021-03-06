\name{getNode}
\alias{getNode}
\alias{getEdge}
\alias{nodeId}
\alias{nodeId,phylo4-method}
\alias{edgeId}
\alias{edgeId,phylo4-method}
\title{node and edge look-up functions}
\description{
  Functions for retrieving node and edge IDs (possibly with
  corresponding labels) from a phylogenetic tree.
}
\usage{
getNode(x, node, type=c("all", "tip", "internal"),
    missing=c("warn","OK","fail"))
nodeId(x, type=c("all", "tip", "internal", "root"))
edgeId(x, type=c("all", "tip", "internal", "root"))
getEdge(x, node, type=c("descendant", "ancestor"),
    missing = c("warn", "OK", "fail")) 
}
\arguments{
  \item{x}{a \linkS4class{phylo4} object (or one inheriting from
    \linkS4class{phylo4}, e.g. a \linkS4class{phylo4d} object)
  }
  \item{node}{either an integer vector corresponding to node ID numbers,
    or a character vector corresponding to node labels; if missing, all
    nodes appropriate to the specified type will be returned by
    \code{getNode}, and all edges appropriate to the specified type will
    be returned by \code{getEdge}.}
  \item{type}{
    (\code{getNode}) specify whether to return nodes matching "all" tree
      nodes (default), only "tip" nodes, or only "internal" nodes;
    (\code{nodeId, edgeId}) specify whether to return "all" tree
      nodes, or only those corresponding to "tip", "internal", or "root"
      nodes;
    (\code{getEdge}) specify whether to look up edges based on their
      descendant node ("descendant") or ancestral node ("ancestor")}
  \item{missing}{what to do if some requested node IDs or names are not
    in the tree: warn, do nothing, or stop with an error}
}
\details{
  \code{getNode} and \code{getEdge} are primarily intended for looking
  up the IDs either of nodes themselves or of edges associated
  with those nodes. Note that they behave quite differently. With
  \code{getNode}, any input nodes are looked up against tree nodes of
  the specified type, and those that match are returned as numeric node
  IDs with node labels (if they exist) as element names. With
  \code{getEdge}, any input nodes are looked up against edge ends of the
  specified type, and those that match are returned as character edge
  IDs with the corresponding node ID as element names.
 
  If \code{missing} is \dQuote{warn} or \dQuote{OK}, \code{NA} is
  returned for any nodes that are unmatched for the specified type. This
  can provide a mechanism for filtering a set of nodes or edges.

  \code{nodeId} provides similar output to \code{getNode} in the case
  when no node is supplied, but it is faster and returns an unnamed
  vector of the numeric IDs of all nodes of the specified node type.
  Similarly, \code{edgeId} simply returns an unnamed vector of the
  character IDs of all edges for which the descendant node is of the
  specified node type.
}
\value{
  \item{\code{getNode}}{returns a named integer vector of node IDs,
    in the order of input nodes if provided, otherwise in nodeId order}
  \item{\code{getEdge}}{returns a named character vector of edge IDs,
    in the order of input nodes if provide, otherwise in nodeId order}
  \item{\code{nodeId}}{returns an unnamed integer vector of node IDs,
    in ascending order}
  \item{\code{getEdge}}{returns an unnamed character vector of edge IDs,
    in edge matrix order}
}
\examples{
  data(geospiza)
  nodeLabels(geospiza) <- LETTERS[1:nNodes(geospiza)]
  plot(as(geospiza, "phylo4"), show.node.label=TRUE)
  getNode(geospiza, 18)
  getNode(geospiza, "D")
  getEdge(geospiza, "D")
  getEdge(geospiza, "D", type="ancestor")

  ## match nodes only to tip nodes, flagging invalid cases as NA
  getNode(geospiza, c(1, 18, 999), type="tip", missing="OK")

  ## get all edges that descend from internal nodes
  getEdge(geospiza, type="ancestor")

  ## identify an edge from its terminal node
  getEdge(geospiza, c("olivacea", "B", "fortis"))
  getNode(geospiza, c("olivacea", "B", "fortis"))
  geospiza@edge[c(26, 1, 11),]

  ## quickly get all tip node IDs and tip edge IDs
  nodeId(geospiza, "tip")
  edgeId(geospiza, "tip")
}
\keyword{misc}
