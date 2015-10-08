\name{setEdgeTargetArrowColorDirect}
\alias{setEdgeTargetArrowColorDirect}
\alias{setEdgeTargetArrowColorDirect,CytoscapeWindowClass-method}
\title{setEdgeTargetArrowColorDirect}
\description{
In the specified CytoscapeWindow, set the edgeTargetArrowColor of the specified edge
or edges.  Low numbers, near zero, are transparent.
High numbers, near 255, are maximally opaque:  they are fully visible.
}
\usage{
setEdgeTargetArrowColorDirect(obj, edge.names, new.colors)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, edges in standard
    Cytoscape form.}
  \item{new.colors}{one or more \code{String} object, representing a color in a
    '#RRGGBB' hex format.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeEdgeTargetArrowColorDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTargetArrowColorDirect.test', graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')

  arrows = c ('Arrow', 'Diamond', 'Circle')
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)

  colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
  colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")

  edge.names = as.character (cy2.edge.names (cw@graph)) [1:3]

  for (i in 1:2) {
    setEdgeTargetArrowColorDirect (cw, edge.names, colors.1)
    redraw (cw)
    Sys.sleep (1)
    setEdgeTargetArrowColorDirect (cw, edge.names, colors.2)
    redraw (cw)
    Sys.sleep (1)
    } # for i
}

\keyword{graph}
