\name{getNodeShapes}
\alias{getNodeShapes}
\alias{getNodeShapes,CytoscapeConnectionClass-method}
\title{getNodeShapes}
\description{
Retrieve the names of the currently supported node shapes,
which can then be used in calls to setNodeShapeRule and
setDefaultVizMapValue.
}
\usage{
getNodeShapes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings, e.g. 'ELLIPSE', 'RECTANGLE'.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getNodeShapes(cy)
   # [1] "VEE"             "RECTANGLE"       "PARALLELOGRAM"   "DIAMOND"        
   # [5] "ROUND_RECTANGLE" "ELLIPSE"         "TRIANGLE"        "HEXAGON"        
   # [9] "OCTAGON" 
}

}
\keyword{graph}
