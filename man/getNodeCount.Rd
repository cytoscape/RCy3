\name{getNodeCount}
\alias{getNodeCount}
\alias{getNodeCount,CytoscapeWindowClass-method}
\title{getNodeCount}
\description{
Reports the number of nodes in the current graph.
}
\usage{
getNodeCount(obj)
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

  cw <- CytoscapeWindow ('getNodeCount.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
    # in Cytoscape, interactively select two nodes, or
  getNodeCount (cw)
  # [1] 3
}

\keyword{graph}
