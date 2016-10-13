\name{getGraphFromCyWindow}
\alias{getGraphFromCyWindow}
\alias{getGraphFromCyWindow,CytoscapeConnectionClass-method}
\title{getGraphFromCyWindow}
\description{
Returns the Cytoscape network as a Bioconductor graph
}
\usage{
getGraphFromCyWindow(obj, window.title)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{window.title}{a \code{string} object. }
}

\value{
A Bioconductor graph object.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getGraphFromCyWindow.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw)
  g.cy <- getGraphFromCyWindow (cw, 'getGraphFromCyWindow.test')
  print (g.cy)  # 3 nodes, 3 edges
}

}
\keyword{graph}
