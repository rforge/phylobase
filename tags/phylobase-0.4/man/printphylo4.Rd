\name{printphylo4}
\alias{printphylo4}
\title{print a phylogeny}
\description{
Prints a phylo4 or phylo4d object in data.frame format with user-friendly column names
}
\usage{
printphylo4(x, printall = TRUE)
}
\arguments{
  \item{x}{a phylo4 tree or phylo4d tree+data object}
  \item{printall}{default prints entire tree. printall=FALSE returns the first 6 rows}
}
\details{
This is a user-friendly version of the tree representation, useful for checking that objects were read in completely and translated correctly. The phylogenetic tree is represented as a list of numbered nodes, linked in a particular way through time (or rates of evolutionary change). 
The topology is given by the pattern of links from each node to its ancestor. Also given are the taxon names, node type (root/internal/tip) and phenotypic data (if any) associated with the node, and the branch length from the node to its ancestor. A list of nodes (descendants) and ancestors is minimally required for a phylo4 object.
}
\value{
  A data.frame with a row for each node (descendant), sorted as
  follows: root first, then other internal nodes, and finally tips.\cr
  The returned data.frame has the following columns:\cr
  \item{label}{Label for the taxon at the node (usually species name).}
  \item{node}{Node number, i.e. the number identifying the node in \code{x@edge}.}
  \item{ancestor}{Node number of the node's ancestor.}
  \item{branch.length}{The branch length connecting the node to its
    ancestor (NAs if missing).}
  \item{node.type}{"root", "internal", or "tip". (internally generated)}
  \item{data}{phenotypic data associated with the nodes, with separate columns for each variable.}
}
\author{
  Marguerite Butler
  Thibaut Jombart \email{jombart@biomserv.univ-lyon1.fr}
  Steve Kembel
}
\note{This is the default show() method for phylo4, phylo4d. It prints the user-supplied information for building a phylo4 object. For a full description of the phylo4 S4 object and slots, see \code{\link{phylo4}}. }
\seealso{See Also as \code{\link{printphylo}} for an \code{ape}-like version of \code{print()}. }
\examples{

tree.phylo <- read.tree(text="((a,b),c);")
tree <- as(tree.phylo, "phylo4")
plot(tree,show.node=TRUE)
tip.data <- data.frame(size=c(1,2,3), row.names=c("a", "b", "c"))
treedata <- phylo4d(tree, tip.data)
plot(treedata,ratio=.7,cent=FALSE,scale=FALSE)
print(treedata)

}
\keyword{methods}
