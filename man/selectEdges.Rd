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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  clearSelection
  getSelectedEdgeCount
  getSelectedEdges
  hideSelectedEdges
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('selectEdges.test', graph=makeSimpleGraph())
  displayGraph (cw); layoutNetwork(cw); redraw (cw)
  clearSelection (cw)
  selectEdges (cw, c ("A (phosphorylates) B", "B (synthetic lethal) C"))
  getSelectedEdges (cw)
}

}
\keyword{graph}
