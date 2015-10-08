\name{getEdgeCount}
\alias{getEdgeCount}
\alias{getEdgeCount,CytoscapeWindowClass-method}
\title{getEdgeCount}
\description{
Reports the number of the edges in the current graph.
}
\usage{
getEdgeCount(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getEdgeCount.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
    # in Cytoscape, interactively select two nodes, or
  getEdgeCount (cw)
  # [1] 3
}

\keyword{graph}
