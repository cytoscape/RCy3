\name{setNodeSizeDirect}
\alias{setNodeSizeDirect}
\alias{setNodeSizeDirect,CytoscapeWindowClass-method}
\title{setNodeSizeDirect}
\description{
In the specified CytoscapeWindow, set the size of the specified node(s).
Note that the node dimensions (height and width) must be locked (the default
state) for this to work.  Node height and width change together.
}
\usage{
setNodeSizeDirect(obj, node.names, new.sizes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.sizes}{one or more \code{integers}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  lockNodeDimensions
  setNodeWidthDirect
  setNodeHeightDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeSizeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'grid')
  setNodeSizeDirect (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
