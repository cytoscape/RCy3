\name{setEdgeLabelColorDirect}
\alias{setEdgeLabelColorDirect}
\alias{setEdgeLabelColorDirect,CytoscapeWindowClass-method}
\title{setEdgeLabelColorDirect}
\description{
In the specified CytoscapeWindow, set the color of the label of the
specified edge or edges.
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setEdgeLabelColorDirect
  setEdgeLabelDirect
  setEdgeFontSizeDirect
 }
 
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeLabelColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  edge.names <- as.character (cy2.edge.names (cw@graph))
  setEdgeLabelDirect (cw, edge.names, 'some label')
  setEdgeLabelColorDirect (cw, edge.names [1:2], '#FF0000')
  setEdgeLabelColorDirect (cw, edge.names, '#00FF00')
  setEdgeLabelColorDirect (cw, edge.names [3], '#000000')
}

\keyword{graph}
