\name{getWindowList}
\alias{getWindowList}
\alias{getWindowList,CytoscapeConnectionClass-method}
\title{getWindowList}
\description{
Returns a named list of windows in the current Cytoscape Desktop. 
}
\usage{
getWindowList(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A named list, in which the values are the titles of
the windows; the names of the list are integers.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  getWindowList (cy)
    # NULL
  cw2 <- CytoscapeWindow ('getWindowList.test1', graph=makeSimpleGraph())
  cw3 <- CytoscapeWindow ('getWindowList.test2', graph=makeSimpleGraph())
  getWindowList (cy)
    # [1] "getWindowList.test2" "getWindowList.test1"
}

}
\keyword{graph}
