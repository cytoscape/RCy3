\name{setEdgeLabelWidthDirect}
\alias{setEdgeLabelWidthDirect}
\alias{setEdgeLabelWidthDirect,CytoscapeWindowClass-method}
\title{setEdgeLabelWidthDirect}
\description{
In the specified CytoscapeWindow, set the labelWidth of the specified edge
or edges.  Low numbers, near zero, are transparent.
High numbers, near 255, are maximally opaque:  they are fully visible.
}
\usage{
setEdgeLabelWidthDirect(obj, edge.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.value}{a \code{integer} object, pixel units.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
   setEdgeColorDirect
   setEdgeLineTypeDirect
   setEdgeSourceArrowDirect
   setEdgeTargetArrowDirect
   setEdgeLabelDirect
   setEdgeFontSizeDirect
   setEdgeLabelColorDirect
   setEdgeTooltipDirect
   setEdgeLineWidthDirect
   setEdgeLineStyleDirect
   setEdgeSourceArrowShapeDirect
   setEdgeTargetArrowShapeDirect
   setEdgeSourceArrowColorDirect
   setEdgeTargetArrowColorDirect
   setEdgeLabelOpacityDirect
   setEdgeSourceArrowOpacityDirect
   setEdgeTargetArrowOpacityDirect
   setEdgeLabelWidthDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setEdgeLabelWidthDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 1:10) {
    setEdgeLabelWidthDirect (cw, edge.names, i)
    redraw (cw)
    }
  for (i in 10:1) {
    setEdgeLabelWidthDirect (cw, edge.names, i)
    redraw (cw)
    }

}

\keyword{graph}
