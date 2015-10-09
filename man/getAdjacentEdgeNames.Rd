\name{getAdjacentEdgeNames}
\alias{getAdjacentEdgeNames}
\title{
getAdjacentEdgeNames
}
\description{
Given one or more node names, this method returns the 'cy2-style' names
of the immediately adjacent edges -- suitable for being passed, for
instance, to selectEdges, and thereby extending the selection.
}
\usage{
getAdjacentEdgeNames(graph, node.names)
}
\arguments{
  \item{graph}{An R graph}
  \item{node.names}{character strings}
}

\value{
Zero or more cy2-style edge names.
}
\author{
Paul Shannon
}

\seealso{
  cy2.edge.names, selectEdges, getSelectedNodes, selectFirstNeighborsOfSelectedNodes
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  g <- makeSimpleGraph ()
  print (nodes (g))
  print (getAdjacentEdgeNames (g, 'A'))
}

\keyword{graph}
