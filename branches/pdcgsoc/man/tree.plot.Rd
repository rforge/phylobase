\name{tree.plot}
\alias{tree.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot a phylo4 object }
\description{
 Plots the phylogenetic tree contained in a \code{phylo4} or \code{phylo4d} object.
}
\usage{
tree.plot(xxyy, type, show.tip.label, show.node.label, 
  edge.color, node.color, tip.color, edge.width, rot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xxyy}{ A list created by the  \code{\link{phyloXXYY}} function }
  \item{type}{ A character string indicating the shape of plotted tree }
  \item{show.tip.label}{ Logical, indicating whether tip labels should be shown }
  \item{show.node.label}{ Logical, indicating whether node labels should be shown }
  \item{edge.color}{ A vector of colors in the order of \code{edges(phy)} }
  \item{node.color}{ A vector of colors indicating the colors of the node labels }
  \item{tip.color}{ A vector of colors indicating the colors of the tip labels }
  \item{edge.width}{ A vector in the order of \code{edges(phy)} indicating the widths of edge lines}
  \item{rot}{ Numeric indicating the rotation of the plot in degrees }
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
Returns no values, function invoked for the plotting side effect.}

\author{Peter Cowan \email{pdc@berkeley.edu}}

\seealso{ \code{\link{treePlot}}, \code{\link{phyloXXYY}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

\keyword{methods}
