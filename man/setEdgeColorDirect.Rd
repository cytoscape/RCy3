\name{setEdgeColorDirect}
\alias{setEdgeColorDirect}
\alias{setEdgeColorDirect,CytoscapeWindowClass-method}
\title{setEdgeColorDirect}
\description{
In the specified CytoscapeWindow, set the color of the specified edge
or edges.  
}
\usage{
setEdgeColorDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one ore more \code{String} objects, cy2-style edge names.}
  \item{new.value}{a \code{numeric} object, a color in hex notation.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeColorDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph))[1:2]
  setEdgeColorDirect (cw, edge.names, '#F833AA')
  redraw (cw)

}

\keyword{graph}
