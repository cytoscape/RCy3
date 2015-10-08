\name{setNodeAttributes}
\alias{setNodeAttributes}
\alias{setNodeAttributes,CytoscapeWindowClass-method}
\title{setNodeAttributes}
\description{
Transfer the named node attribute from the the R graph (found in
obj@graph) to Cytoscape. This method is typically called by
displayGraph, which will suffice for most users' needs.  It transfers
the specified node attributes, for all nodes, from the cw@graph slot to Cytoscape.
}
\usage{
setNodeAttributes(obj, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{a \code{string} one of the attributes defined on
    the nodes. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeAttributesDirect
  setEdgeAttributes
  setEdgeAttributesDirect
  sendEdges
  sendNodes
  displayGraph
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeAttributes.test', graph=makeSimpleGraph())
  attribute.names = noa.names (cw@graph)

  for (attribute.name in attribute.names)
    result = setNodeAttributes (cw, attribute.name)

}


\keyword{graph}
