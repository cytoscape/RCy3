\name{setNodeLabelOpacityDirect}
\alias{setNodeLabelOpacityDirect}
\alias{setNodeLabelOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeLabelOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the
label of the specified node(s).
}
\usage{
setNodeLabelOpacityDirect(obj, node.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.values}{one or more \code{numeric} objects, ranging from 0 to 255.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeFillOpacityDirect
  setNodeOpacityDirect
  setNodeBorderOpacityDirect
 }

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeLabelOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setNodeLabelOpacityDirect (cw, 'A', 210)
  setNodeLabelOpacityDirect (cw, c('B', 'C'), c(111, 133))
  setNodeLabelOpacityDirect (cw, c('B', 'C'), 11)
}

}
\keyword{graph}
