\name{setNodeLabelColorDirect}
\alias{setNodeLabelColorDirect}
\alias{setNodeLabelColorDirect,CytoscapeWindowClass-method}
\title{setNodeLabelColorDirect}
\description{
In the specified CytoscapeWindow, set the color of the font used in
rendering the label of the specified node(s).
}
\usage{
setNodeLabelColorDirect(obj, node.names, new.colors)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.colors}{one or more \code{String}, specifying the color
        using standard hex notation.}
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
  setNodeLabelDirect
  setNodeLabelOpacityDirect
  setNodeOpacityDirect
  setNodeShapeDirect
  setNodeSizeDirect
  setNodeWidthDirect
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeLabelColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setNodeFontSizeDirect (cw, getAllNodes(cw), 50)
  setNodeLabelColorDirect (cw, 'A', '#FFFF00')
  setNodeLabelColorDirect (cw, c('B', 'C'), '#AA00AA')
  setNodeLabelColorDirect (cw, c('B', 'C'), c('#FF0AAA', '#AAFFAA'))
}

}
\keyword{graph}
