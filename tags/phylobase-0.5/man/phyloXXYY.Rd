\name{phyloXXYY}
\alias{phyloXXYY}
\title{ Calculate node x and y coordinates }
\description{
Calculates the node x and y locations for plotting a phylogenetic tree.}
\usage{
phyloXXYY(phy, tip.order = NULL)
}
\arguments{
  \item{phy}{ A \code{phylo4} or \code{phylo4d} object. }
  \item{tip.order}{A character vector of tip labels, indicating their order along the y axis (from top to bottom). Or, a numeric vector of tip node IDs indicating the order.}
}
\details{
The y coordinates of the tips are evenly spaced from 0 to 1 in pruningwise order.  Ancestor y nodes are given the mean value of immediate descendants.  The root is given the x coordinate 0 and descendant nodes are placed according to the cumulative branch length from the root, with a maximum x value of 1.
}
\value{
  \item{yy}{Internal node and tip y coordinates}
  \item{xx}{Internal node and tip x coordinates}
  \item{phy}{A \code{phylo4} or \code{phylo4d} object}
  \item{segs}{A list of \code{h0x, h1x, v0x, v1x} and \code{h0y, h1y, v0y, v1y} describing the start and end points for the plot line segments}
  \item{torder}{The tip order provided as \code{tip.order} or if NULL the preoder tip order}
  \item{eorder}{The an index of the reordered edges compared to the result of \code{edges(phy)}}

}

\author{Peter Cowan \email{pdc@berkeley.edu}}

\seealso{\code{treePlot}, \code{\link{plotOneTree}}}
\examples{

data(geospiza)
coor <- phyloXXYY(geospiza)
plot(coor$xx, coor$yy, pch = 20)

}
\keyword{methods}
