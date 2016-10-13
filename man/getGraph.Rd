\name{getGraph}
\alias{getGraph}
\alias{getGraph,CytoscapeWindowClass-method}
\title{getGraph}
\description{
Returns the Bioconductor graph object which belongs to the specified
CytoscapeWindow object
}
\usage{
getGraph(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A graph object.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getGraph.test', graph=makeSimpleGraph())
  displayGraph (cw)
  print (getGraph (cw))
}

}
\keyword{graph}
