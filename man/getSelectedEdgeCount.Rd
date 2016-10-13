\name{getSelectedEdgeCount}
\alias{getSelectedEdgeCount}
\alias{getSelectedEdgeCount,CytoscapeWindowClass-method}
\title{getSelectedEdgeCount}
\description{
Returns the number of edges currently selected.
}
\usage{
getSelectedEdgeCount(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
An integer.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getSelectedEdgeCount.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
  clearSelection (cw)
  getSelectedEdgeCount (cw)
    # [1] 0
    # in Cytoscape, interactively select an edge, or programmatically
  selectEdges (cw, "A (phosphorylates) B")
  getSelectedEdgeCount (cw)
    # [1] 1
}

}
\keyword{graph}
