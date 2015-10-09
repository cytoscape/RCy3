\name{setEdgeLabelDirect}
\alias{setEdgeLabelDirect}
\alias{setEdgeLabelDirect,CytoscapeWindowClass-method}
\title{setEdgeLabelDirect}
\description{
In the specified CytoscapeWindow, set the edgeLabel of the specified edge
or edges.  
}
\usage{
setEdgeLabelDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.value}{a \code{string} object, the new label.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeEdgeLabelDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLabelDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 1:10) {
    setEdgeLabelDirect (cw, edge.names, 255 - (i * 25))
    redraw (cw)
    }
  for (i in 1:10) {
    setEdgeLabelDirect (cw, edge.names, i * 25)
    redraw (cw)
    }

}

\keyword{graph}
