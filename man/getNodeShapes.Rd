\name{getNodeShapes}
\alias{getNodeShapes}
\alias{getNodeShapes,CytoscapeConnectionClass-method}
\title{getNodeShapes}
\description{
Retrieve the names of the currently supported node shapes, which can then be used in calls to setNodeShapeRule and setDefaultVizMapValue 
}
\usage{
getNodeShapes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings, e.g., 'trapezoid', 'ellipse', 'rect'
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getNodeShapes(cy)
   # "trapezoid" "round_rect" "ellipse" "triangle"   "rect_3d" "diamond"
   # "parallelogram" "octagon"   "trapezoid_2" "rect" "hexagon"

}

\keyword{graph}
