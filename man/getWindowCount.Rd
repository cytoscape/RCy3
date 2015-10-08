\name{getWindowCount}
\alias{getWindowCount}
\alias{getWindowCount,CytoscapeConnectionClass-method}
\title{getWindowCount}
\description{
Returns the number of windows which currently exist in the Cytoscape Desktop.
}
\usage{
getWindowCount(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
An integer.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  count.at.start = getWindowCount (cy)
  cw2 <- CytoscapeWindow ('getWindowCount.test1', graph=makeSimpleGraph())
  cw3 <- CytoscapeWindow ('getWindowCount.test2', graph=makeSimpleGraph())
  getWindowCount (cy)
    # should be two greater than 'count.at.start'
}


\keyword{graph}
