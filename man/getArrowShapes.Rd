\name{getArrowShapes}
\alias{getArrowShapes}
\alias{getArrowShapes,CytoscapeConnectionClass-method}
\title{getArrowShapes}
\description{
Retrieve the names of the currently supported 'arrows' -- the
decorations can (optionally) appear at the ends of edges, adjacent to
the nodes they connect, and conveying information about the nature of
the nodes' relationship.
}
\usage{
getArrowShapes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings, e.g., 'DIAMOND', 'T', 'ARROW'
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getArrowShapes (cy)
    # [1] "DIAMOND_SHORT_1" "HALF_TOP"        "DELTA_SHORT_2"   "DELTA"          
    # [5] "HALF_BOTTOM"     "CIRCLE"          "DIAMOND"         "DIAMOND_SHORT_2"
    # [9] "NONE"            "T"               "ARROW"           "ARROW_SHORT"    
    # [13] "DELTA_SHORT_1"  
}

}
\keyword{graph}
