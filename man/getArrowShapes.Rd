\name{getArrowShapes}
\alias{getArrowShapes}
\alias{getArrowShapes,CytoscapeConnectionClass-method}
\title{getArrowShapes}
\description{
Retrieve the names of the currently supported 'arrows' -- the
decorations can (optionally) appear at the ends of edges, adjacent to
the nodes they connect, and conveying information about the nature of
the nodes' relationship.
of strings.
}
\usage{
getArrowShapes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of character strings, e.g., 'WHITE_DIAMOND', 'BLACK_T'
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getArrowShapes (cy)
    # [1] "No Arrow" "Diamond" "Delta" "Arrow" "T" "Circle" "Half Arrow Top"  "Half Arrow Bottom"
}

\keyword{graph}
