\name{setDefaultNodeBorderColor}
\alias{setDefaultNodeBorderColor}
\alias{setDefaultNodeBorderColor,CytoscapeConnectionClass-method}
\title{setDefaultNodeBorderColor}
\description{
In the specified CytoscapeConnection, stipulate the default
color for all node borders other than those mentioned
in a node border color rule.
}
\usage{
setDefaultNodeBorderColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
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
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultNodeBorderColor.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultNodeBorderWidth (cw, 4)
  setDefaultNodeBorderColor (cw, '#000000') # white borders
  # redraw (cw) --> not required anymore
}

}
\keyword{graph}
