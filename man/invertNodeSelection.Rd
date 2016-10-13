\name{invertNodeSelection}
\alias{invertNodeSelection}
\alias{invertNodeSelection,CytoscapeWindowClass-method}
\title{invertNodeSelection}
\description{
Invert the node selection, i.e. select all nodes that were
not selected and deselect all nodes that were selected.
}
\usage{
invertNodeSelection(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  clearSelection
  invertEdgeSelection
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('invertNodeSelection demo', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  invertNodeSelection (cw)
     # all nodes should be selected, since none were before
  invertNodeSelection (cw)
     # no node should be selected, since all were before
  selectNodes (cw, 'A')
  invertNodeSelection (cw)
    # only B and C should be selected
}

}
\keyword{graph}
