\name{setEdgeTargetArrowShapeDirect}
\alias{setEdgeTargetArrowShapeDirect}
\alias{setEdgeTargetArrowShapeDirect,CytoscapeWindowClass-method}
\title{setEdgeTargetArrowShapeDirect}
\description{
In the specified CytoscapeWindow, set the target arrow shape of
the specified edge or edges, using one of the supported shapes.
}
\usage{
setEdgeTargetArrowShapeDirect(obj, edge.names, new.values)
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getArrowShapes
  setDefaultEdgeSourceArrowColor
  setEdgeSourceArrowColorDirect
  setEdgeSourceArrowColorRule
  setEdgeSourceArrowRule
  setEdgeSourceArrowShapeDirect
  setEdgeTargetArrowColorDirect
  setEdgeTargetArrowColorRule
  setEdgeTargetArrowRule
  setEdgeTargetArrowShapeDirect
  setDefaultEdgeTargetArrowColor
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeTargetArrowShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')

  edges.of.interest <- as.character (cy2.edge.names (cw@graph))
  supported.arrow.shapes <- getArrowShapes (cw)

    # first try passing three edges and three arrow shapes
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [2:4])

    # now try passing three edges and one arrow.shapes
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [6])

    # restore the default
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, 'NONE')
}

\keyword{graph}
