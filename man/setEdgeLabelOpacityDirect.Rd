\name{setEdgeLabelOpacityDirect}
\alias{setEdgeLabelOpacityDirect}
\alias{setEdgeLabelOpacityDirect,CytoscapeWindowClass-method}
\title{setEdgeLabelOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the specified edge
or edges.  Low numbers, near zero, are transparent.
High numbers, near 255, are maximally opaque:  they are fully visible.
}
\usage{
setEdgeLabelOpacityDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.value}{a \code{numeric} object, ranging from 0 to 255.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeLabelOpacityDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLabelOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 1:10) {
    setEdgeLabelOpacityDirect (cw, edge.names, 255 - (i * 25))
    redraw (cw)
    }
  for (i in 1:10) {
    setEdgeLabelOpacityDirect (cw, edge.names, i * 25)
    redraw (cw)
    }

}

\keyword{graph}
