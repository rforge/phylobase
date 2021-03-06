% Generated by roxygen2 (4.0.0.99): do not edit by hand
\docType{methods}
\name{print}
\alias{head}
\alias{head,phylo4-method}
\alias{names}
\alias{names,phylo4-method}
\alias{print}
\alias{print,phylo4-method}
\alias{show}
\alias{show,phylo4-method}
\alias{tail}
\alias{tail,phylo4-method}
\title{print a phylogeny}
\usage{
print(x, ...)

\S4method{print}{phylo4}(x, edgeOrder = c("pretty", "real"),
  printall = TRUE)

show(object)

\S4method{show}{phylo4}(object)

names(x)

\S4method{names}{phylo4}(x)

head(x, ...)

\S4method{head}{phylo4}(x, n = 20)

tail(x, ...)

\S4method{tail}{phylo4}(x, n = 20)
}
\arguments{
\item{x}{a \code{phylo4} tree or \code{phylo4d} tree+data object}

\item{edgeOrder}{in the data frame returned, the option 'pretty' returns the
internal nodes followed by the tips, the option 'real' returns the nodes in
the order they are stored in the edge matrix.}

\item{printall}{default prints entire tree. printall=FALSE returns the first
6 rows}

\item{...}{further arguments passed to or from other methods.}

\item{object}{Any R object}

\item{n}{a single integer. If positive, size for the resulting
    object: number of elements for a vector (including lists), rows for
    a matrix or data frame or lines for a function. If negative, all but
    the \code{n} last/first number of elements of \code{x}.}
}
\value{
A data.frame with a row for each node (descendant), sorted as
follows: root first, then other internal nodes, and finally tips.\cr The
returned data.frame has the following columns:\cr \item{label}{Label for the
taxon at the node (usually species name).} \item{node}{Node number, i.e. the
number identifying the node in edge matrix.} \item{ancestor}{Node number
of the node's ancestor.} \item{branch.length}{The branch length connecting
the node to its ancestor (NAs if missing).} \item{node.type}{"root",
"internal", or "tip". (internally generated)} \item{data}{phenotypic data
associated with the nodes, with separate columns for each variable.}
}
\description{
Prints a phylo4 or phylo4d object in data.frame format with user-friendly
column names
}
\details{
This is a user-friendly version of the tree representation, useful for
checking that objects were read in completely and translated correctly. The
phylogenetic tree is represented as a list of numbered nodes, linked in a
particular way through time (or rates of evolutionary change).  The topology
is given by the pattern of links from each node to its ancestor. Also given
are the taxon names, node type (root/internal/tip) and phenotypic data (if
any) associated with the node, and the branch length from the node to its
ancestor. A list of nodes (descendants) and ancestors is minimally required
for a phylo4 object.
}
\note{
This is the default show() method for phylo4, phylo4d. It prints the
user-supplied information for building a phylo4 object. For a full
description of the phylo4 S4 object and slots, see \code{\link{phylo4}}.
}
\examples{
tree.phylo <- ape::read.tree(text="((a,b),c);")
tree <- as(tree.phylo, "phylo4")
##plot(tree,show.node=TRUE) ## plotting broken with empty node labels: FIXME
tip.data <- data.frame(size=c(1,2,3), row.names=c("a", "b", "c"))
treedata <- phylo4d(tree, tip.data)
plot(treedata)
print(treedata)
}
\author{
Marguerite Butler, Thibaut Jombart \email{jombart@biomserv.univ-lyon1.fr}, Steve Kembel
}
\keyword{methods}

