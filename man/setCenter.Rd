\name{setCenter}
\alias{setCenter}
\alias{setCenter,CytoscapeWindowClass-method}
\title{setCenter}
\description{
  This method can be used to pan and scroll the Cytoscape canvas, which
  is adjusted (moved) so that the  specified x and y coordinates are at
  the center of the visible window.
}
\usage{
setCenter(obj, x, y)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{x}{a \code{numeric} object.}
  \item{y}{a \code{numeric} object.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getCenter
  getZoom
  setZoom
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title = 'setCenter demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  original.center <- getCenter (cw)   # named list, "x" and "y".  initial values might be 140 and 90
    # now pan the display to the left, by setting the the visual center
    # to increasing values of x, without changing the location of the
    #  simple graph
  setCenter (cw, 200, 90)
  system ('sleep 1')
  setCenter (cw, 300, 90)
  system ('sleep 1')
  setCenter (cw, 400, 90)
  system ('sleep 1')
    # and now pan back to the original position
  setCenter (cw, 300, 90)
  system ('sleep 1')
  setCenter (cw, 200, 90)
  system ('sleep 1')
  setCenter (cw, original.center$x, original.center$y)
}


\keyword{graph}
