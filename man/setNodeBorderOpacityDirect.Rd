\name{setNodeBorderOpacityDirect}
\alias{setNodeBorderOpacityDirect}
\alias{setNodeBorderOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeBorderOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the border of the specified node(s).
}
\usage{
setNodeBorderOpacityDirect(obj, node.names, new.values)
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
  setNodeFillOpacityDirect
  setNodeLabelOpacityDirect
  setNodeOpacityDirect
 }



\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeBorderOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeBorderOpacityDirect (cw, 'A', 220)
  setNodeBorderOpacityDirect (cw, c('B', 'C'), c(22, 55))
  redraw (cw)
}


\keyword{graph}
