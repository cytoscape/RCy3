\name{setNodePosition}
\alias{setNodePosition}
\alias{setNodePosition,CytoscapeWindowClass-method}
\title{setNodePosition}
\description{
  Set the position of the specified nodes on the CytoscapeWindow
  canvas.  Use this for any hand-crafted layouts, or novel layout
  algorithms, you wish to use.
}
\usage{
setNodePosition(obj, node.names, x.coords, y.coords)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{list} of strings, the names of nodes to select. }
  \item{x.coords}{a \code{list} of floating point numbers, one for each
    node in the node.names list. }
  \item{y.coords}{a \code{list} of floating point numbers, one for each
    node in the node.names list. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getNodePosition
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodePosition.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  setNodePosition (cw, c ('A', 'B', 'C'), c (10.0, 20.0, 500), c (0.0,
               100.0, 3))  
}

\keyword{graph}
