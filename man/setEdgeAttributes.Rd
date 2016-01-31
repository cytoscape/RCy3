\name{setEdgeAttributes}
\alias{setEdgeAttributes}
\alias{setEdgeAttributes,CytoscapeWindowClass-method}
\title{setEdgeAttributes}
\description{
Transfer the named edge attribute from the R graph (found in
obj@graph) to Cytoscape.This method is typically called by
displayGraph, which will suffice for most users' needs.
It transfers the specified edge attributes, for all edges,
from the cw@graph slot to Cytoscape.
}
\usage{
setEdgeAttributes(obj, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{a \code{string}, one of the
    attributes defined on the edges. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setEdgeAttributesDirect
  setNodeAttributes
  setNodeAttributesDirect
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeAttributes.test', graph=makeSimpleGraph())
  attribute.names <- eda.names (cw@graph)
  sendNodes (cw)
  sendEdges (cw)
  
  for (attribute.name in attribute.names){
    setEdgeAttributes (cw, attribute.name)
  }
}

\keyword{graph}
