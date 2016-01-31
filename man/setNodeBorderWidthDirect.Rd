\name{setNodeBorderWidthDirect}
\alias{setNodeBorderWidthDirect}
\alias{setNodeBorderWidthDirect,CytoscapeWindowClass-method}
\title{setNodeBorderWidthDirect}
\description{
In the specified CytoscapeWindow, set the width of the border of
the specified node(s).
}
\usage{
setNodeBorderWidthDirect(obj, node.names, new.sizes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects, the node names.}
  \item{new.sizes}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeBorderColorDirect
  setNodeBorderOpacityDirect
  setNodeColorDirect
  setNodeFillOpacityDirect
  setNodeFontSizeDirect
  setNodeHeightDirect
  setNodeImageDirect
  setNodeLabelColorDirect
  setNodeLabelDirect
  setNodeLabelOpacityDirect
  setNodeOpacityDirect
  setNodeShapeDirect
  setNodeSizeDirect
  setNodeWidthDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeBorderWidthDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setNodeBorderWidthDirect (cw, 'A', 10)
}

\keyword{graph}
