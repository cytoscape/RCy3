\name{setDefaultEdgeFontSize}
\alias{setDefaultEdgeFontSize}
\alias{setDefaultEdgeFontSize,CytoscapeConnectionClass-method}
\title{setDefaultEdgeFontSize}
\description{
In the specified CytoscapeConnection, stipulate the color for all edges
other than those mentioned in a edge color rule.
}
\usage{
setDefaultEdgeFontSize(obj, new.size, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.size}{an \code{integer}.}
  \item{vizmap.style.name}{a visual style.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
   setDefaultNodeShape
   setDefaultNodeFontSize
   setDefaultNodeSize
   setDefaultNodeColor
   setDefaultNodeBorderColor
   setDefaultNodeBorderWidth
   setDefaultNodeFontSize
   setDefaultNodeLabelColor
   setDefaultEdgeLineWidth
   setEdgeColorRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('test setDefaultEdgeFontSize', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setEdgeLabelRule (cw, 'edgeType')
  setDefaultEdgeFontSize (cw, 66)
  redraw (cw)
}


\keyword{graph}
