\name{setDefaultNodeShape}
\alias{setDefaultNodeShape}
\alias{setDefaultNodeShape,CytoscapeConnectionClass-method}
\title{setDefaultNodeShape}
\description{
For all CytoscapeWindow's, specify the default node shape.
}
\usage{
setDefaultNodeShape(obj, new.shape, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.shape}{a \code{String} object, one of the permissible values
    (see getNodeShapes).}
  \item{vizmap.style.name}{a \code{String} object.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
   getNodeShapes
   setDefaultNodeShape
   setDefaultNodeColor
   setDefaultNodeSize
   setDefaultNodeColor
   setDefaultNodeBorderColor
   setDefaultNodeBorderWidth
   setDefaultNodeFontSize
   setDefaultNodeLabelColor
   setDefaultEdgeLineWidth
   setEdgeColorRule
   setNodeBorderColorRule
 }

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultNodeShape.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'grid')
  legal.shapes <- getNodeShapes (cw)
  # stopifnot ('DIAMOND' %in% legal.shapes)
  setDefaultNodeShape (cw, 'DIAMOND')
  redraw (cw)
}


\keyword{graph}
