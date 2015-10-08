\name{CytoscapeConnection}
\alias{CytoscapeConnection}
\title{
CytoscapeConnection
}
\description{
The constructor for the CytoscapeConnectionClass.  This class is both
the base class for CytoscapeWindow objects, and quite usefully, and
instantiable object in its own right.   It is very useful for calling
the many RCytoscape methods which do not address a single window in
particular:  getWindowList, getWindowCount, deleteWindow,
getNodeShapes, etc.
}
\usage{
CytoscapeConnection (host = "localhost", port = 1234)
}

\arguments{
  \item{host}{
    Defaults to 'localhost', this is the domain name of a machine which
    is running Cytoscape with the appropriate CyREST plugin.
}
  \item{port}{
    Defaults to 1234, this may be any port to which CyREST
    is listening.
}
}
\value{
  An object of the CytoscapeConnection Class.
}
\author{
  Paul Shannon
}

\seealso{
  ping
  version
  getWindowCount
  getWindowID
  getWindowCount
  getWindowList
  deleteWindow
  deleteAllWindows
  getNodeShapes
  getAttributeClassNames
  getLineStyles
  getArrowShapes
  getLayoutNames
  haveNodeAttribute
  haveEdgeAttribute
  getGraphFromCyWindow
  hidePanel
  dockPanel
  floatPanel
  }


\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  deleteAllWindows (cy)
  getNodeShapes (cy)
  hidePanel (cy, 'Control')
}

\keyword{ graph }
