\name{raiseWindow}
\alias{raiseWindow}
\alias{raiseWindow,CytoscapeConnectionClass-method}
\title{raiseWindow}
\description{
Raise this window to the top on the Cytoscape desktop, so that it can be seen.
}
\usage{
raiseWindow(obj, window.title=NA)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object, or its subclass, \code{CytoscapeWindowClass}. }
  \item{window.title}{a \code{string}. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  resizeWindow
}

\examples{
  # WARNING: Method RCy3::raiseWindow() is not implemented in RCy3!
  
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('raiseWindow.test', graph=makeSimpleGraph())
  cw2 <- CytoscapeWindow ('raiseWindow.test2', graph=makeSimpleGraph())
  
  raiseWindow (cw)
}

\keyword{graph}
