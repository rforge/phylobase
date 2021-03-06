\name{treewalk}
\alias{children}
\alias{descendants}
\alias{ancestor}
\alias{ancestors}
\alias{siblings}
\alias{MRCA}
\alias{getnodes}
\alias{getedges}
\alias{shortestPath}
\alias{sumEdgeLength}
\alias{sumEdgeLength,phylo4-method}
\title{tree traversal and utility functions}
\description{
  Functions for describing relationships among
  phylogenetic nodes (i.e. internal nodes or tips).
}
\usage{
getnodes(phy,node)
ancestors(phy, node, which=c("all","parent","ALL"))
ancestor(phy, node)
siblings(phy,node,include.self=FALSE)
children(phy, node)
descendants(phy, node, which=c("tips","children","all"))
MRCA(phy,\dots)
shortestPath(phy, node1, node2)
\S4method{sumEdgeLength}{phylo4}(phy, node)
}
\arguments{
  \item{phy}{a \linkS4class{phylo4} object (or one inheriting from
    \linkS4class{phylo4}, e.g. a \linkS4class{phylo4d} object)
  }
  \item{node}{a node number (or name)}
  \item{which}{(\code{ancestors}) specify whether to return just direct
    ancestor ("parent"), all ancestor nodes ("all"), or all ancestor
    nodes including self ("ALL"); (\code{descendants})
    specify whether to return
    just direct descendants ("children"), all extant descendants
    ("tips"), or all descendant nodes ("all")}
  \item{include.self}{whether to include self in list of siblings}
  \item{\dots}{a list of node numbers or names,
    or a vector of node numbers or names}
  \item{node1}{a node number (or name)}
  \item{node2}{a node number (or name)}
}
\value{
  \item{\code{getnodes}}{takes character strings corresponding to tip
    or node labels, or node numbers; it returns a named
    vector of node numbers}
  \item{\code{getedges}}{takes character strings naming terminal
    node of edges, or the corresponding node numbers; it returns a named
    vector of edge numbers.}
  \item{\code{ancestors} and \code{descendants}}{
  return named vectors of the ancestors and descendants of
  a node; \code{ancestor} is a synonym for
  \item \code{ancestors(\dots,which="parent")} (i.e. direct
  ancestor only), while
  \code{children} is a synonym for
  \code{descendants(\dots,which="children")} (i.e. direct
  descendants only)}
  \item{\code{siblings}}{returns sibling nodes (children
  of the same parent)}
  \item{\code{mrca}}{returns the most recent common ancestor of two or
  more nodes}
  \item{\code{shortestPath}}{returns the nodes of the shortest path from one
  node to another (excluding \code{node1} and \code{node2})}
  \item{\code{sumEdgeLength}}{returns the sum of branch length for branches
    starting at nodes provided.}
}
\note{\code{MRCA} is uppercase to avoid conflict with \code{mrca}
  in ape}
\seealso{\code{\link[pkg:ape]{mrca}}, in the ape package, gives
  a list of all subtrees}
\examples{
  data(geospiza)
  getnodes(geospiza,18)
  getnodes(geospiza,"N04")
  plot(as(geospiza,"phylo4"),
    use.edge.length=FALSE,show.node.label=TRUE)
  ancestor(geospiza,"N11")
  children(geospiza,"N05")
  descendants(geospiza,"N11",which="tips")
  descendants(geospiza,"N11",which="all")
  ancestors(geospiza,"N11")
  MRCA(geospiza,"conirostris","difficilis","fuliginosa")
  MRCA(geospiza,"olivacea","conirostris")

  ## shortest path between 2 nodes
  shortestPath(geospiza, "fortis","fuliginosa")
  shortestPath(geospiza, "N02","N05")

  ## identifying an edge fron its terminal node
  getedges(geospiza,c("olivacea","N02","fortis"))
  getnodes(geospiza, c("olivacea","N02","fortis"))
  geospiza$edge[c(26,1,11),]
  if(require(ape)){ edgelabels() }

  ## branch length from a tip to the root
  sumEdgeLength(geospiza, ancestors(geospiza, "fortis", which="ALL"))

}
\keyword{misc}
