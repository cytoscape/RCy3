\name{setDefaultNodeBorderWidth}
\alias{setDefaultNodeBorderWidth}
\alias{setDefaultNodeBorderWidth,CytoscapeConnectionClass-method}
\title{setDefaultNodeBorderWidth}
\description{
In the specified CytoscapeConnection, stipulate the color for all nodeBorders
other than those mentioned in a node border color rule.
}
\usage{
setDefaultNodeBorderWidth(obj, new.width, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.width}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
  \item{vizmap.style.name}{a \code{String} object.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
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

  cw <- CytoscapeWindow ('setDefaultNodeBorderWidth.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultNodeBorderWidth (cw, 5)
  redraw (cw)
}


\keyword{graph}
