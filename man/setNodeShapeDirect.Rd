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
  \item{new.shapes}{one or more \code{String} objects, each of the
        allowed values returned by getNodeShape.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getNodeShapes
 }

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  getNodeShapes (cw)
  setNodeShapeDirect (cw, 'A', 'TRIANGLE')
  setNodeShapeDirect (cw, c('B', 'C'), getNodeShapes(cw)[2:3])
  setNodeShapeDirect (cw, c('A', 'B', 'C'), 'PARALLELOGRAM')
}

}
\keyword{graph}
