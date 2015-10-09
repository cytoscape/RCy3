\name{setGraph}
\alias{setGraph}
\alias{setGraph,CytoscapeWindowClass-method}
\title{setGraph}
\description{
Assigns the supplied graph object to the appropriate slot in the specified
CytoscapeWindow object.
}
\usage{
setGraph(obj, graph)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{graph}{a \code{graph} object. }
}

\value{
The modified CytoscapeWindow object.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setGraph.test')  # an empty graph is created by default
  graph <- makeSimpleGraph ()
  setGraph (cw, graph)
  print (length (nodes (getGraph (cw))))

}

\keyword{graph}
