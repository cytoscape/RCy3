\name{setWindowSize}
\alias{setWindowSize}
\alias{setWindowSize,CytoscapeWindowClass-method}
\title{setWindowSize}
\description{
Control the size of the CytoscapeWindow by specifying a width and height,  On a typical screen, there may
be 1200 pixels in the width of a full-size window, and 800 pixels in height.
}
\usage{
setWindowSize(obj, width, height)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{width}{a \code{numeric} object. }
  \item{height}{a \code{numeric} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getZoom
  setZoom
  getCenter
  setCenter
  getViewCoordinates
  fitContent
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title = 'setWindowSize demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setWindowSize (cw, 1200, 800)
  fitContent (cw)
  system ('sleep 1')
  setWindowSize (cw, 120, 80)
  fitContent (cw)
  system ('sleep 1')
  setWindowSize (cw, 600, 400)
  fitContent (cw)
}

\keyword{graph}
