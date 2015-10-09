\name{hideSelectedEdges}
\alias{hideSelectedEdges}
\alias{hideSelectedEdges,CytoscapeWindowClass-method}
\title{hideSelectedEdges}
\description{
Hide (but do not delete) the currently selected edges.   'Unhide' is
supposed to return them to view, but this is broken in Cytoscape 2.7.
}
\usage{
hideSelectedEdges(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  unhideAll
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('hideSelectedEdges.test', graph=makeSimpleGraph())
  # selectEdges (cw, 'B (synthetic lethal) C')
  hideSelectedEdges (cw)
  unhideAll (cw)
   # alas, Cytoscape requires that you render these edges, and redo the
   # layout, so that they are visible again
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
}


\keyword{graph}
