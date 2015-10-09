\name{setDefaultEdgeTargetArrowColor}
\alias{setDefaultEdgeTargetArrowColor}
\alias{setDefaultEdgeTargetArrowColor,CytoscapeConnectionClass-method}
\title{setDefaultEdgeTargetArrowColor}
\description{
In the specified CytoscapeConnection, stipulate the color for all edge target arrows
other than those mentioned in an edge target arrow color rule.
}
\usage{
setDefaultEdgeTargetArrowColor(obj, new.color, vizmap.style.name = "default")
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
\author{Tanja Muetze}

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
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultEdgeTargetArrowColor test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'grid')
  setDefaultEdgeTargetArrowColor (cw, '#FFFFFF') # white edges
  redraw (cw)
}


\keyword{graph}
