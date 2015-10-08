\name{setNodeFillOpacityDirect}
\alias{setNodeFillOpacityDirect}
\alias{setNodeFillOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeFillOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the fill color of
the specified node.
}
\usage{
setNodeFillOpacityDirect(obj, node.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.values}{a \code{numeric} object, ranging from 0 to 255.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeLabelOpacityDirect
  setNodeOpacityDirect
  setNodeBorderOpacityDirect
 }


\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeFillOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeFillOpacityDirect (cw, 'A', 220)
  setNodeFillOpacityDirect (cw, c('B', 'C'), c(222, 233))
  redraw (cw)
}


\keyword{graph}
