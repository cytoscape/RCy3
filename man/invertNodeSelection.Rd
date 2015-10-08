\name{invertNodeSelection}
\alias{invertNodeSelection}
\alias{invertNodeSelection,CytoscapeWindowClass-method}
\title{invertNodeSelection}
\description{
Invert the node selection, i.e. select all nodes that were not selected and deselect all nodes that were selected.

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
\author{Paul Shannon}

\seealso{
  clearSelection
  invertEdgeSelection
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('invertNodeSelection demo', graph=makeSimpleGraph())
     # all nodes should be selected, since none were before
  selectNodes (cw, 'A')
  invertNodeSelection (cw)
}

\keyword{graph}
