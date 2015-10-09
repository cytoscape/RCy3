\name{deleteSelectedEdges}
\alias{deleteSelectedEdges}
\alias{deleteSelectedEdges,CytoscapeWindowClass-method}
\title{deleteSelectedEdges}
\description{In Cytoscape, remove all selected edges.  These edges will
  still exist in the corresponding R graph until you delete them there
  as well.
}
\usage{
deleteSelectedEdges(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  selectEdges
  cy2.edge.names
  deleteSelectedNodes
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('deleteSelectedEdges.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed') 
  print (cy2.edge.names (cw@graph))   # find out Cytoscape's names for  these edges
  selectEdges (cw, "B (synthetic lethal) C")
  deleteSelectedEdges (cw)
  redraw (cw)
}


\keyword{graph}
