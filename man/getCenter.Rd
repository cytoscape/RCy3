\name{getCenter}
\alias{getCenter}
\alias{getCenter,CytoscapeWindowClass-method}
\title{getCenter}
\description{
  This method returns the coordinates of the current center of the
  visible Cytoscape canvas, or drawing surface.  The intial values are a
  little unpredictable, but seem to be on the order of 100 for both x
  and y.
}
\usage{
getCenter(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A names list, x and y.
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

  window.title = 'getCenter demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  print (getCenter (cw))
}

\keyword{graph}
