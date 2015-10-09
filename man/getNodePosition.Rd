\name{getNodePosition}
\alias{getNodePosition}
\alias{getNodePosition,CytoscapeWindowClass-method}
\title{getNodePosition}
\description{
  Get the position of the specified nodes on the CytoscapeWindow
  canvas.  Useful in retrieving the current position of nodes in the window.
}
\usage{
getNodePosition(obj, node.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{list} of strings, the names of nodes to select. }
}

\value{
A names list of x,y pairs; names are the identifiers of nodes supplied
when the graph was created.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getNodePosition.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  getNodePosition (cw, c ('A', 'B', 'C'))
}

\keyword{graph}
