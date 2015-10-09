\name{getEdgeAttribute}

\alias{getEdgeAttribute}
\alias{getEdgeAttribute,CytoscapeConnectionClass-method}

\title{getEdgeAttribute}
\description{
Node and edge attributes are usually added to a Cytoscape network by
defined them on the graph used to construct a CytoscapeWindow.  The
small family of methods described here, however, provide another avenue for adding an
edge attribute, for learning which are currently defined, and for
deleting and edge attribute.

Note that edge (and node) attibutes are defined, not just for a specific, single
CytoscapeWindow, but for an entire Cytoscape application session.  Thus if you
have two nodes (or edges) with the same ID in two
different windows, adding a node attribute results in both nodes having
that attribute.

}
\usage{
getEdgeAttribute(obj, edge.name, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
  \item{edge.name}{a \code{character string} specifying the
    Cytoscape-style name of an edge.}
  \item{attribute.name}{a \code{character string}, the name of the
    attribute you wish to retrieve.}
}

\value{
The attribute in question, which may be of any scalar type.
}
\author{Paul Shannon}

\seealso{
  getEdgeAttributeNames
  deleteEdgeAttribute
}



\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.name = 'demo.getEdgeAttribute'
  cw = CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  setDefaultEdgeLineWidth (cw, 5);
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)

  score.bc = getEdgeAttribute (cw, "B (synthetic lethal) C", 'score')
  print (paste ("should be -12: ", score.bc))

}

\keyword{graph}
