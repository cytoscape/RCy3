\name{addCyEdge}
\alias{addCyEdge}
\alias{addCyEdge,CytoscapeWindowClass-method}
\title{addCyEdge}
\description{
Given a CytoscapeWindow containing a (possibly empty) graph, this method
adds an edge.  Edge attributes are added separately, via successive calls
to sendEdgeAttributesDirect. The two nodes must already exist in the
Cytoscape network.

}
\usage{
addCyEdge(obj, sourceNode, targetNode, edgeType, directed)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{sourceNode}{a \code{character string} object. }
  \item{targetNode}{a \code{character string} object. }
  \item{edgeType}{a \code{character string} object. }
  \item{directed}{a \code{boolean} object. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  sendEdgeAttributesDirect
  addCyNode
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())
  window.name <- 'demo addCyEdge'
  cw <- CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork(cw)
  directed = TRUE
  addCyEdge (cw, 'A', 'B', 'synthetic rescue', directed)


}

\keyword{graph}
