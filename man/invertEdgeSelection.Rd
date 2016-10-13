\name{invertEdgeSelection}
\alias{invertEdgeSelection}
\alias{invertEdgeSelection,CytoscapeWindowClass-method}
\title{invertEdgeSelection}
\description{
Invert edge selection, i.e. select all edges that were
not selected and deselect all edges that were selected.
}
\usage{
invertEdgeSelection(obj)
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
  invertNodeSelection
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('invertEdgeSelection demo', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  invertEdgeSelection (cw)
    # all edges should be selected, since none were before
}

}
\keyword{graph}
