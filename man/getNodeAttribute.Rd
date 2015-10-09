\name{getNodeAttribute}

\alias{getNodeAttribute}
\alias{getNodeAttribute,CytoscapeConnectionClass-method}

\title{getNodeAttribute}
\description{
Node and node attributes are usually added to a Cytoscape network by
defined them on the graph used to construct a CytoscapeWindow.  The
small family of methods described here, however, provide another avenue for adding an
node attribute, for learning which are currently defined, and for
deleting and node attribute.

Note that node (and node) attibutes are defined, not just for a specific, single
CytoscapeWindow, but for an entire Cytoscape application session.  Thus if you
have two nodes (or nodes) with the same ID (the same name) in two
different windows, adding a node attribute results in both nodes having
that attribute.

}
\usage{
getNodeAttribute(obj, node.name, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
  \item{node.name}{a \code{character string} specifying the
    Cytoscape-style name of an node.}
  \item{attribute.name}{a \code{character string}, the name of the
    attribute you wish to retrieve.}
}

\value{
The attribute in question, which may be of any scalar type.
}
\author{Paul Shannon}

\seealso{
  getNodeAttributeNames
  deleteNodeAttribute
}



\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.name = 'demo.getNodeAttribute'
  cw = CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)

  count.B = getNodeAttribute (cw, "B", 'count')

}

\keyword{graph}
