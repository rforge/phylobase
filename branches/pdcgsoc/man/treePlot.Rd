\name{treePlot}
\alias{treePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
treePlot(phy, type = c("phylogram", "cladogram", "fan"), show.tip.label = TRUE, show.node.label = FALSE, tip.order = NULL, plot.data = is(phy, "phylo4d"), rot = 0, tip.plot.fun = "bubbles", edge.color = "black", node.color = "black", tip.color = "black", edge.width = 1, newpage = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{ A \code{phylo4} or \code{phylo4d} object }
  \item{type}{ A character string indicating the shape of plotted tree }
  \item{show.tip.label}{ Logical, indicating whether tip labels should be shown }
  \item{show.node.label}{ Logical, indicating whether node labels should be shown }
  \item{tip.order}{ Unused, a character vector of tip labels, indicating they're order along the y axis }
  \item{plot.data}{ Logical indicating whether \code{phylo4d} data should be plotted }
  \item{rot}{ Numeric indicating the rotation of the plot in degrees }
  \item{tip.plot.fun}{ A function used to generate plot at the each tip of the phylogenetic trees }
  \item{edge.color}{ A vector of colors in the order of \code{edges(phy)} }
  \item{node.color}{ A vector of colors indicating the colors of the node labels }
  \item{tip.color}{ A vector of colors indicating the colors of the tip labels }
  \item{edge.width}{ A vector in the order of \code{edges(phy)} indicating the widths of edge lines}
  \item{newpage}{ Logical indicating whether the page should be cleared before plotting }
  \item{\dots}{ Currently unused, parameters to be passed on to \code{gpar} }  
}

\value{ No return value, function invoked for plotting side effect
}
\author{Peter Cowan \email{pdc@berkeley.edu}}

\seealso{ \code{\link{phylobubbles}}  }
\keyword{methods}
