\name{getNodeSize}
\alias{getNodeSize}
\alias{getNodeSize,CytoscapeWindowClass-method}
\title{getNodeSize}
\description{
  Get the size of the specified nodes on the CytoscapeWindow
  canvas.  
}
\usage{
getNodeSize(obj, node.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{list} of strings, the names of nodes to select. }
}

\value{
A named list containing two equal-lengthed vectors, width and height.
Unless node dimensions are 'unlocked' these two vectors will be identical.
}
\author{Paul Shannon}

\seealso{
  setNodeSizeRule, setNodeSizeDirect, lockNodeDimensions
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getNodeSize.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  sizes = getNodeSize (cw, c ('A', 'B', 'C'))
  print (sizes$width)
  print (sizes$height)
  setNodeSizeDirect (cw, 'A',  180)
  redraw (cw)
  print (getNodeSize (cw, 'A'))
  lockNodeDimensions (cw, FALSE)
  setNodeWidthDirect (cw, 'A', 300)
  setNodeHeightDirect (cw, 'A', 100)
  redraw (cw)
  sizes = getNodeSize (cw, 'A')
  print (sizes$width)
  print (sizes$height)
  lockNodeDimensions (cw, TRUE)
  setNodeSizeDirect (cw, 'A', 80)
  redraw (cw)
  print (getNodeSize (cw, 'A'))
}

\keyword{graph}
