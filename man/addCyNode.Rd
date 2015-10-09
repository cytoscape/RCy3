\name{addCyNode}
\alias{addCyNode}
\alias{addCyNode,CytoscapeWindowClass-method}
\title{addCyNode}
\description{
Given a CytoscapeWindow containing a (possibly empty) graph, this method
adds a node.  Node attributes are added separately, via successive calls
to sendNodeAttributesDirect.  The new node must be unique -- not already
a member of the graph as known to Cytoscape.

}
\usage{
addCyNode(obj, nodeName)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{nodeName}{a \code{character string} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  sendNodeAttributesDirect
  addCyEdge
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())
  window.name <- 'demo addCyNode'
  cw <- CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw)
  addCyNode (cw, 'A NEW NODE')
  redraw (cw)
  layoutNetwork(cw)


}

\keyword{graph}
