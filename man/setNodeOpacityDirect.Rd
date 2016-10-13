\name{setNodeOpacityDirect}
\alias{setNodeOpacityDirect}
\alias{setNodeOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of all aspectes of the
specified node(s): fill color, border, label.
}
\usage{
setNodeOpacityDirect(obj, node.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.values}{one or more \code{numeric} objects,
        ranging from 0 to 255.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeFillOpacityDirect
  setNodeLabelOpacityDirect
  setNodeBorderOpacityDirect
 }

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  setDefaultNodeBorderWidth (cw, 4)
  setDefaultNodeBorderColor(cw, '#0000FF')
  
  setNodeOpacityDirect (cw, 'A', 20)
  setNodeOpacityDirect (cw, c('B', 'C'), c(111, 133))
  setNodeOpacityDirect (cw, c('B', 'C'), 10)
}

}
\keyword{graph}
