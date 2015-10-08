\name{getFirstNeighbors}
\alias{getFirstNeighbors}
\alias{getFirstNeighbors,CytoscapeWindowClass-method}
\title{getFirstNeighbors}
\description{
Returns a non-redundant ('uniquified') list of all of the first
neighbors of the supplied list of nodes.
}
\usage{
getFirstNeighbors(obj, node.names, as.nested.list = FALSE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{as.nested.list}{ a \code{Boolean} object of if a nested list or a concatenated list should be returned}
  \item{node.names}{a \code{String} list object.}
}

\value{
A list of node names.
}
\author{Paul Shannon}

\seealso{
  selectNodes  
  selectFirstNeighborsOfSelectedNodes
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getFirstNeighbors.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'grid')
  print (getFirstNeighbors (cw, 'A'))
  selectNodes (cw, getFirstNeighbors (cw, 'A'))  # note that A is not selected
}


\keyword{graph}
