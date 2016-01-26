\name{setNodeFontSizeDirect}
\alias{setNodeFontSizeDirect}
\alias{setNodeFontSizeDirect,CytoscapeWindowClass-method}
\title{setNodeFontSizeDirect}
\description{
In the specified CytoscapeWindow, set the size of the font used in
rendering the label of the specified node(s).
}
\usage{
setNodeFontSizeDirect(obj, node.names, new.sizes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.sizes}{one or more \code{integer}, in pixel units.}
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

  cw <- CytoscapeWindow ('setNodeFontSizeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeFontSizeDirect (cw, 'A', 32)
  setNodeFontSizeDirect (cw, c('B', 'C'), 32)
  setNodeFontSizeDirect (cw, c('A', 'B'), c(10, 15))
}

\keyword{graph}
