\name{sendEdges}
\alias{sendEdges}
\alias{sendEdges,CytoscapeWindowClass-method}
\title{sendEdges}
\description{
Transfer the edges of the R graph (found in obj@graph) to Cytoscape.
This method is not recommended for the average user.  It is called
behind the scenes by displayGraph.
}
\usage{
sendEdges(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  displayGraph
  sendNodes
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('sendEdges.test', graph=makeSimpleGraph())
  sendNodes (cw)
  sendEdges (cw)
}


\keyword{graph}
