\name{setDefaultEdgeSourceArrowColor}
\alias{setDefaultEdgeSourceArrowColor}
\alias{setDefaultEdgeSourceArrowColor,CytoscapeConnectionClass-method}
\title{setDefaultEdgeSourceArrowColor}
\description{
In the specified CytoscapeConnection, stipulate the default
color for all edge source arrows other than those mentioned
in an edge source arrow color rule.
}
\usage{
setDefaultEdgeSourceArrowColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
  \item{vizmap.style.name}{a \code{String} object, if this vizmap style
    needs to be distinguished from the default type.}
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
   setDefaultEdgeFontSize
   setEdgeColorRule
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultEdgeSourceArrowColor test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultEdgeSourceArrowColor (cw, '#F0F0F0')
  setEdgeSourceArrowShapeDirect (cw, c('A (phosphorylates) B', 'B (synthetic lethal) C'), 'DIAMOND')
  # redraw (cw) --> not required anymore
}

}
\keyword{graph}
