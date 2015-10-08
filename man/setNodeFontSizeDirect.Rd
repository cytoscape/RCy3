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
  \item{new.sizes}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeWidthDirect
  setNodeHeightDirect
  setNodeSizeDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeFontSizeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setNodeFontSizeDirect (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
