\name{phyloXXYY}
\alias{phyloXXYY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate node x and y coordinates }
\description{
Calculates the node x and y locations for plotting a phylogenetic tree.}
\usage{
phyloXXYY(phy, tip.order = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{ A \code{phylo4} or \code{phylo4d} object. }
  \item{tip.order}{ Unused. A character vector indicating the ordering of tip nodes}
}
\details{
The y coordinates of the tips are evenly spaced from 0 to 1 in pruningwise order.  Ancestor y nodes are given the mean value of immediate descendants.  The root is given the x coordinate 0 and descendant nodes are placed according to the cumulative branch length from the root, with a maximum x value of 1.
}
\value{
  \item{xxyy}{A list with three items: \code{yy}, the y locations, \code{xx}, the x locations, \code{traverse}, the tree traversal order}
  \item{phy}{A \code{phylo4} or \code{phylo4d} object}
  \item{phy.orig}{The original \code{phylo4} or \code{phylo4d} object}
}

% \references{ ~put references to the literature/web site here ~ }
\author{Peter Cowan \email{pdc@berkeley.edu}}

\seealso{\code{\link{segs}}, \code{\link{treePlot}}, \code{\link{tree.plot}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\keyword{methods}
