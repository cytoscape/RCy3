\name{setEdgeFontSizeDirect}
\alias{setEdgeFontSizeDirect}
\alias{setEdgeFontSizeDirect,CytoscapeWindowClass-method}
\title{setEdgeFontSizeDirect}
\description{
In the specified CytoscapeWindow, set the font size of the specified edge
or edges.
}
\usage{
setEdgeFontSizeDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more  \code{String} objects, cy2-style edge names.}
  \item{new.value}{an \code{integer} objects, specifying font size in pixels.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeFontSizeDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeFontSizeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 8:30) {
    setEdgeFontSizeDirect (cw, edge.names, i)
    redraw (cw)
    }
  setEdgeFontSizeDirect (cw, edge.names, 12)

}

\keyword{graph}
