\name{getFirstNeighbors}
\alias{getFirstNeighbors}
\alias{getFirstNeighbors,CytoscapeWindowClass-method}
\title{getFirstNeighbors}
\description{
Returns a non-redundant ('uniquified') list of all of the first
neighbors of the supplied list of nodes.
}
\usage{
getFirstNeighbors(obj, node.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
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
  cw <- new.CytoscapeWindow ('getFirstNeighbors.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  print (getFirstNeighbors (cw, 'A'))
  selectNodes (cw, getFirstNeighbors (cw, 'A'))  # note that A is not selected
}


\keyword{graph}
