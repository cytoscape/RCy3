\name{getViewCoordinates}
\alias{getViewCoordinates}
\alias{getViewCoordinates,CytoscapeWindowClass-method}
\title{getViewCoordinates}
\description{
  This method returns the four numbers (top.x, top.y, bottom.x,
  bottom.y) which implicitly specify the bounds of the current window.
}
\usage{
getViewCoordinates(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A named list of four numbers, with these names:  top.x, top.y, bottom.x, bottom.y
}
\author{Paul Shannon}

\seealso{
  getViewCoordinates
  getZoom
  setZoom
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title = 'getViewCoordinates demo'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  print (getViewCoordinates (cw))
}

\keyword{graph}
