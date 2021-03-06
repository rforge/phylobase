\name{tip.data.plot}
\Rdversion{1.1}
\alias{tip.data.plot}
\title{Plotting trees and associated data}
\description{Plotting phylogenetic trees and associated data}
\usage{
tip.data.plot(xxyy, type = c("phylogram", "cladogram", "fan"), show.tip.label = TRUE, show.node.label = FALSE, rot = 0, tip.plot.fun = grid.points, edge.color = "black", node.color = "black", tip.color = "black", edge.width = 1, ...)
}
\arguments{
  \item{xxyy}{A list created by the  \code{\link{phyloXXYY}} function}
  \item{type}{ A character string indicating the shape of plotted tree }
  \item{show.tip.label}{ Logical, indicating whether tip labels should be shown }
  \item{show.node.label}{ Logical, indicating whether node labels should be shown }
  \item{rot}{ Numeric indicating the rotation of the plot in degrees }
  \item{tip.plot.fun}{A function used to plot the data elements of a \code{phylo4d} object}
  \item{edge.color}{ A vector of colors in the order of \code{edges(phy)} }
  \item{node.color}{ A vector of colors indicating the colors of the node labels }
  \item{tip.color}{ A vector of colors indicating the colors of the tip labels }
  \item{edge.width}{ A vector in the order of \code{edges(phy)} indicating the widths of edge lines}
  \item{\dots}{Additional parameters passed to \code{tip.plot.fun}}

}
\value{
  creates a plot on the current graphics device.
}
\author{
  Peter Cowan
}
\keyword{methods}
