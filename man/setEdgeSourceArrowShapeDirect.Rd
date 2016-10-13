\name{setEdgeSourceArrowShapeDirect}
\alias{setEdgeSourceArrowShapeDirect}
\alias{setEdgeSourceArrowShapeDirect,CytoscapeWindowClass-method}
\title{setEdgeSourceArrowShapeDirect}
\description{
In the specified CytoscapeWindow, set the source arrow shape of the
specified edge or edges, using one of the supported shapes.
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getArrowShapes
  setEdgeTargetArrowRule
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
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setEdgeSourceArrowShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')

  edges.of.interest <- as.character (cy2.edge.names (cw@graph))
  supported.arrow.shapes <- getArrowShapes (cw)

    # first try passing three edges and three arrow shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [2:4])
  
    # now try passing three edges and one arrow.shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [6])

    # restore the default
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, 'NONE')
}

}
\keyword{graph}
