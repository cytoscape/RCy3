\name{setEdgeLineWidthDirect}
\alias{setEdgeLineWidthDirect}
\alias{setEdgeLineWidthDirect,CytoscapeWindowClass-method}
\title{setEdgeLineWidthDirect}
\description{
In the specified CytoscapeWindow, set the lineWidth of the specified edge
or edges.  Width is measured in pixels.
}
\usage{
setEdgeLineWidthDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.value}{an \code{integer} object, typically in the range of 0 to 10.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeLineWidthDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLineWidthDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 1:10) {
    setEdgeLineWidthDirect (cw, edge.names, i)
    redraw (cw)
    }

 setEdgeLineWidthDirect (cw, edge.names, 1)

}

\keyword{graph}
