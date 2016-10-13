\name{hideSelectedEdges}
\alias{hideSelectedEdges}
\alias{hideSelectedEdges,CytoscapeWindowClass-method}
\title{hideSelectedEdges}
\description{
Hide (but do not delete) the currently selected edges.
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  unhideAll
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('hideSelectedEdges.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'grid')
  
  selectEdges (cw, 'B (synthetic lethal) C')
  hideSelectedEdges (cw)
  # unhideAll (cw) # makes the edges appear again
   # previously, Cytoscape required that you render these edges,
   # and redo the layout, so that they are visible again.
   # This is no longer required.
}

}
\keyword{graph}
