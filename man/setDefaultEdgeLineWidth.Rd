\name{setDefaultEdgeLineWidth}
\alias{setDefaultEdgeLineWidth}
\alias{setDefaultEdgeLineWidth,CytoscapeConnectionClass-method}
\title{setDefaultEdgeLineWidth}
\description{
In the specified CytoscapeConnection, stipulate the line width, in pixels for all edges.
}
\usage{
setDefaultEdgeLineWidth(obj, new.width, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.width}{an \code{integer} object, typically from 0 to 5.}
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
   setEdgeColorRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultEdgeLineWidth.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultEdgeLineWidth (cw, 5) 
  redraw (cw)
}


\keyword{graph}
