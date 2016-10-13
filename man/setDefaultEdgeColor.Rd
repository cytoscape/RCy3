\name{setDefaultEdgeColor}
\alias{setDefaultEdgeColor}
\alias{setDefaultEdgeColor,CytoscapeConnectionClass-method}
\title{setDefaultEdgeColor}
\description{
Set the default edge color.
}
\usage{
setDefaultEdgeColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
  \item{vizmap.style.name}{a \code{String} object if this vizmap style
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

  cw <- CytoscapeWindow ('setDefaultEdgeColor test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'grid')
  setDefaultEdgeColor (cw, '#888888') # grey edges
  redraw (cw) # redraw is not required anymore
}

}
\keyword{graph}
