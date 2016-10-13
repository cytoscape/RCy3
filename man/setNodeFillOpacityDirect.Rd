\name{setNodeFillOpacityDirect}
\alias{setNodeFillOpacityDirect}
\alias{setNodeFillOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeFillOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the fill
color of the specified node(s).
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeBorderColorDirect
  setNodeBorderOpacityDirect
  setNodeBorderWidthDirect
  setNodeColorDirect
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
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeFillOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setNodeFillOpacityDirect (cw, 'A', 20)
  setNodeFillOpacityDirect (cw, c('B', 'C'), c(122, 133))
  setNodeFillOpacityDirect (cw, c('B', 'C'), 1)
}

}
\keyword{graph}
