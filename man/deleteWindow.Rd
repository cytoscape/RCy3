\name{deleteWindow}
\alias{deleteWindow}
\alias{deleteWindow,CytoscapeConnectionClass-method}
\title{deleteWindow}
\description{
Delete the window associated with the supplied CytoscapeConnection object.  In addition,
Cytoscape's copy of the network is deleted from Cytoscape's memory
store, but the R graph object is unaffected.

There are two different ways to use this method.  First, if you call it on a
CytoscapeWindow object, using the default window.title value of NA, the
Cytoscape window itself will be deleted.  Alternatively, if you supply a
window.title as the second argument -- independent of whether or not the
first argument is a CytoscapeConnection object, or its subclass, a
CytoscapeWindow object, the named window is deleted.
}
\usage{
deleteWindow(obj, window.title=NA)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object, or subclass }
  \item{window.title}{a \code{string} object, optional title}
}

\value{
Nothing.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  window.title <- 'demo deleteWindow'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw)
  deleteWindow (cw)
  cw2 <- CytoscapeWindow ('demo 2')
  cy = CytoscapeConnection ()
  deleteWindow (cy, 'demo 2')
}

\keyword{graph}
