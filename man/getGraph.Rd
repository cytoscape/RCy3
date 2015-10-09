\name{getGraph}
\alias{getGraph}
\alias{getGraph,CytoscapeWindowClass-method}
\title{getGraph}
\description{
Returns the bioconductor graph object which belongs to the specified
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
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getGraph.test', graph=makeSimpleGraph())
  displayGraph (cw)
  print (getGraph (cw))
}

\keyword{graph}
