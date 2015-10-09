\name{selectEdges}
\alias{selectEdges}
\alias{selectEdges,CytoscapeWindowClass-method}
\title{selectEdges}
\description{
Select the specified edges.
}
\usage{
selectEdges(obj, edge.names, preserve.current.selection=TRUE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{a \code{list} of strings, the names of edges to select. }
  \item{preserve.current.selection}{a \code{logical} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  selectEdge
  getSelectedEdgeCount
  getSelectedEdges
  hideSelectedEdges
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('selectEdges.test', graph=makeSimpleGraph())
  displayGraph (cw); layoutNetwork(cw); redraw (cw)
  clearSelection (cw)
  selectEdges (cw, c ("A (phosphorylates) B", "B (synthetic lethal) C"))
  getSelectedEdges (cw)
    # more complicated, but more realistic:
  #selectEdges (cw, as.character ( cy2.en (g, names (which (eda (g, 'edgeType') == 'phosphorylates')))))
}

\keyword{graph}
