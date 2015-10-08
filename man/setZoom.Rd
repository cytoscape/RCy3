\name{setZoom}
\alias{setZoom}
\alias{setZoom,CytoscapeWindowClass-method}
\title{setZoom}
\description{
  This method expands or contracts the relative size of the objects (the graph)
  displayed in the CytoscapeWindow.
  A value of 1.0 typically renders the graph with an ample margin.  A
  call to fitContent produces a zoom level of about 1.5.
}
\usage{
setZoom(obj, new.level)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.level}{a \code{numeric} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getZoom
  getCenter
  setCenter
  getViewCoordinates
  fitContent
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title = 'setZoom demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setZoom (cw, 0.3)
  system ('sleep 1')
  setZoom (cw, 3.0)
  system ('sleep 1')
  setZoom (cw, 1.0)
}

\keyword{graph}
