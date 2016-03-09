\name{setEdgeOpacityDirect}
\alias{setEdgeOpacityDirect}
\alias{setEdgeOpacityDirect,CytoscapeWindowClass-method}
\title{setEdgeOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the specified edge
or edges at the same time. Low numbers, near zero, are transparent.
High numbers, near 255, are maximally opaque: they are fully visible.
}
\usage{
setEdgeOpacityDirect(obj, edge.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names. }
  \item{new.values}{a \code{numeric} object, ranging from 0 to 255. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeOpacityDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 1:10) {
    setEdgeOpacityDirect (cw, edge.names, 255 - (i * 25))
    }
}

\keyword{graph}
