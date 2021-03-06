\name{formatData}
\alias{formatData}

\title{Format data for use in phylo4d objects}

\description{
  Associates data with tree nodes and applies consistent formatting
  rules.
}

\usage{
  formatData(phy, dt, type=c("tip", "internal", "all"),
              match.data=TRUE, rownamesAsLabels=FALSE,
              label.type=c("rownames", "column"),
              label.column=1, missing.data=c("fail", "warn", "OK"),
              extra.data=c("warn", "OK", "fail"), keep.all=TRUE)
}

\arguments{
  \item{phy}{a valid \code{phylo4} object}
  \item{dt}{a data frame, matrix, vector, or factor}
  \item{type}{type of data to attach}
  \item{match.data}{(logical) should the rownames of the data frame
    be used to be matched against tip and internal node identifiers? See
    details.}
  \item{rownamesAsLabels}{(logical), should the row names of the data
    provided be matched only to labels (TRUE), or should any number-like
    row names be matched to node numbers (FALSE and default)}
  \item{label.type}{character, \code{rownames} or \code{column}: should
    the labels be taken from the row names of \code{dt} or from
    the \code{label.column} column of \code{dt}?}
  \item{label.column}{if \code{label.type=="column"}, column specifier
    (number or name) of the column containing tip labels}
  \item{missing.data}{action to take if there are missing data or if
    there are data labels that don't match}
  \item{extra.data}{action to take if there are extra data or if there
    are labels that don't match} 
  \item{keep.all}{(logical), should the returned data have rows for all
    nodes (with NA values for internal rows when type='tip', and vice
    versa) (TRUE and default) or only rows corresponding to the type
    argument}
}

\value{
  \code{formatData} returns a data frame having node numbers as row
  names. The data frame is also formatted to have the correct dimension
  given the \code{phylo4} object provided.
}

\details{

  \code{formatData} is an internal function that should not be called
  directly by the user. It is used to format data provided by the user
  before associating it with a tree, and is called internally by the
  \code{phylo4d}, \code{tdata}, and \code{addData} methods. However,
  users may pass additional arguments to these methods in order to
  control how the data are matched to nodes.
  
  Rules for matching rows of data to tree nodes are determined jointly
  by the \code{match.data} and \code{rownamesAsLabels} arguments. If
  \code{match.data} is TRUE, data frame rows will be matched exclusively
  against tip and node labels if \code{rownamesAsLabels} is also TRUE,
  whereas any all-digit row names will be matched against tip and node
  numbers if \code{rownamesAsLabels} is FALSE (the default). If
  \code{match.data} is FALSE, \code{rownamesAsLabels} has no effect, and
  row matching is purely positional with respect to the order returned
  by \code{nodeId(phy, type)}.
  
  \code{formatData} (1) converts labels provided in the data into node
  numbers, (2) makes sure that the data are appropriately matched
  against tip and/or internal nodes, (3) checks for differences between
  data and tree, (4) creates a data frame with the correct dimensions
  given a tree.
  
}

\seealso{the \code{\link{phylo4d}} constructor, the
  \linkS4class{phylo4d} class. See also the \code{\link{checkPhylo4}},
  the \code{\link{phylo4}} constructor and the \linkS4class{phylo4}
  class. See \code{\link{coerce-methods}} for translation functions.}

\author{Francois Michonneau}

\keyword{misc}
