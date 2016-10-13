\name{hideSelectedNodes}
\alias{hideSelectedNodes}
\alias{hideSelectedNodes,CytoscapeWindowClass-method}
\title{hideSelectedNodes}
\description{
Hide (but do not delete) the currently selected nodes. We strongly
recommened that you save the current layout before hiding any nodes:
'unhideAll' previously often placed restored nodes in unpredictable
positions.
}
\usage{
hideSelectedNodes(obj)
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
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('hideSelectedNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  saveLayout (cw, 'layout.tmp.RData')
  selectNodes (cw, c ('A', 'B'))
  hideSelectedNodes (cw)
  unhideAll (cw)
  restoreLayout (cw, 'layout.tmp.RData')
}

}
\keyword{graph}
