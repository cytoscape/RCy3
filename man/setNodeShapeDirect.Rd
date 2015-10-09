\name{setNodeShapeDirect}
\alias{setNodeShapeDirect}
\alias{setNodeShapeDirect,CytoscapeWindowClass-method}
\title{setNodeShapeDirect}
\description{
In the specified CytoscapeWindow, set the shape of the specified node(s).
}
\usage{
setNodeShapeDirect(obj, node.names, new.shapes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.shapes}{one or more \code{String} objects, one of the allowed values
    returned by getNodeShape.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getNodeShapes
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'grid')
  getNodeShapes(cw)
  setNodeShapeDirect (cw, 'A', 'TRIANGLE')
  redraw (cw)
}


\keyword{graph}
