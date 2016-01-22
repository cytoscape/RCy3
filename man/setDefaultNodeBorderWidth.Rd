\name{setDefaultNodeBorderWidth}
\alias{setDefaultNodeBorderWidth}
\alias{setDefaultNodeBorderWidth,CytoscapeConnectionClass-method}
\title{setDefaultNodeBorderWidth}
\description{
In the specified CytoscapeConnection, stipulate the default
border width for all node borders other than those mentioned
in a node border width rule.
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

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
  layoutNetwork(cw, 'force-directed')
  setDefaultNodeBorderWidth (cw, 5)
  # redraw (cw) --> not required anymore
}

\keyword{graph}
