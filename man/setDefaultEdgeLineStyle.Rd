\name{setDefaultEdgeLineStyle}
\alias{setDefaultEdgeLineStyle}
\alias{setDefaultEdgeLineStyle,CytoscapeConnectionClass-method}
\title{setDefaultEdgeLineStyle}
\description{
In the specified CytoscapeConnection, stipulate the default
line style for all edges.
}
\usage{
setDefaultEdgeLineStyle(obj, new.line.style, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.line.style}{an \code{String} object, an avaiable line style,
  check getLineStyles for possible options.}
  \item{vizmap.style.name}{a \code{String} object, the visual style,
  check getVisualStyleNames for possible options.}
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
   setEdgeColorRule
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultEdgeLineStyle.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultEdgeLineStyle (cw, 'DASH_DOT') 
  #redraw (cw)
}

}
\keyword{graph}
