\name{sendNodes}
\alias{sendNodes}
\alias{sendNodes,CytoscapeWindowClass-method}
\title{sendNodes}
\description{
Transfer the nodes of the R graph (found in obj@graph) to Cytoscape.  This method is not recommended for the average user.  It is called
behind the scenes by displayGraph.

}
\usage{
sendNodes(obj)
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
  sendEdges
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('sendNodes.test', graph=makeSimpleGraph())
  sendNodes (cw)
}

\keyword{graph}
