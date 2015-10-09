\name{setNodeLabelOpacityDirect}
\alias{setNodeLabelOpacityDirect}
\alias{setNodeLabelOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeLabelOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the label of the specified node(s).
}
\usage{
setNodeLabelOpacityDirect(obj, node.names, new.values)
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
  setNodeOpacityDirect
  setNodeBorderOpacityDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeLabelOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeLabelOpacityDirect (cw, 'A', 220)
  setNodeLabelOpacityDirect (cw, c('B', 'C'), c(211, 233))
  redraw (cw)
}


\keyword{graph}
