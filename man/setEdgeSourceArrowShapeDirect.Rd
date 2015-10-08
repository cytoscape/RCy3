\name{setEdgeSourceArrowShapeDirect}
\alias{setEdgeSourceArrowShapeDirect}
\alias{setEdgeSourceArrowShapeDirect,CytoscapeWindowClass-method}
\title{setEdgeSourceArrowShapeDirect}
\description{
In the specified CytoscapeWindow, set the source arrow shape of the specified edge
or edges, using one of the supported shapes.
}
\usage{
setEdgeSourceArrowShapeDirect(obj, edge.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more cy2-style edge names, \code{String} objects.}
  \item{new.values}{one or more \code{String} objects, from the supported
    set.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  cy2.edge.names
  getArrowShapes
  setEdgeTargetArrowRule
  setEdgeSourceArrowShapeDirect
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeSourceArrowShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'grid')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (cw@graph))
  supported.arrow.shapes = getArrowShapes (cw)

    # first try passing three edges and three arrow shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [2:5])
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one arrow.shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [6])
  redraw (cw)

    # now loop through all of the arrow.shapes

  for (shape in supported.arrow.shapes) {
    setEdgeSourceArrowShapeDirect (cw, edges.of.interest, shape)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, 'NONE')
  redraw (cw)


}

\keyword{graph}
