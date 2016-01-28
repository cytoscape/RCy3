\name{getNodeSize}
\alias{getNodeSize}
\alias{getNodeSize,CytoscapeWindowClass-method}
\title{getNodeSize}
\description{
  Get the size of the specified nodes on the CytoscapeWindow canvas.  
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeSizeRule, setNodeSizeDirect, lockNodeDimensions
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getNodeSize.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  sizes <- getNodeSize (cw, c ('A', 'B', 'C'))
  print (sizes$width)
    # [1] 75 75 75
  print (sizes$height)
    # [1] 35 35 35
  setNodeSizeDirect (cw, 'A',  18)
  redraw (cw)
  print (getNodeSize (cw, 'A'))
  # lockNodeDimensions (cw, FALSE) # not required anymore
  setNodeWidthDirect (cw, 'A', 30)
  setNodeHeightDirect (cw, 'A', 10)
  redraw (cw) # not required anymore
  sizes <- getNodeSize (cw, 'A')
  print (sizes$width)
  print (sizes$height)
  # lockNodeDimensions (cw, TRUE) # not required anymore
  setNodeSizeDirect (cw, 'A', 80)
  redraw (cw) # not required anymore
  print (getNodeSize (cw, 'A'))
}

\keyword{graph}
