\name{setNodeHeightDirect}
\alias{setNodeHeightDirect}
\alias{setNodeHeightDirect,CytoscapeWindowClass-method}
\title{setNodeHeightDirect}
\description{
In the specified CytoscapeWindow, set the height of the specified node(s).
Not that the node dimensions (height and width) must be unlocked for
this to work.  If they ARE locked, then node and height change together,
as specified by a node size rule, or the setNodeSizeDirect method
}
\usage{
setNodeHeightDirect(obj, node.names, new.heights)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one ore more \code{String} objects.}
  \item{new.heights}{one or more \code{integers}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeWidthDirect
  lockNodeDimensions
  setNodeSizeDirect
  setNodeHeightDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeHeightDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  lockNodeDimensions (cw, 'default', FALSE)
  setNodeHeightDirect (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
