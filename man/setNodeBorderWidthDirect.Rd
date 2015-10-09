\name{setNodeBorderWidthDirect}
\alias{setNodeBorderWidthDirect}
\alias{setNodeBorderWidthDirect,CytoscapeWindowClass-method}
\title{setNodeBorderWidthDirect}
\description{
In the specified CytoscapeWindow, set the width of the border of the specified node(s).
}
\usage{
setNodeBorderWidthDirect(obj, node.names, new.sizes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.sizes}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeSizeDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeBorderWidthDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeBorderWidthDirect (cw, 'A', 10)
  redraw (cw)
}


\keyword{graph}
