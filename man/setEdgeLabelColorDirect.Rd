\name{setEdgeLabelColorDirect}
\alias{setEdgeLabelColorDirect}
\alias{setEdgeLabelColorDirect,CytoscapeWindowClass-method}
\title{setEdgeLabelColorDirect}
\description{
In the specified CytoscapeWindow, set the labelColor of the specified edge
or edges.  Low numbers, near zero, are transparent.
High numbers, near 255, are maximally opaque:  they are fully visible.
}
\usage{
setEdgeLabelColorDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.value}{a \code{String} object, an RGB color in '#RRGGBB' form.}
}

\value{
None.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLabelColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  setEdgeLabelColorDirect (cw, edge.names, '#FF0000')
  redraw (cw)
  setEdgeLabelColorDirect (cw, edge.names, '#00FF00')
  redraw (cw)
  setEdgeLabelColorDirect (cw, edge.names, '#000000')
  redraw (cw)

}

\keyword{graph}
