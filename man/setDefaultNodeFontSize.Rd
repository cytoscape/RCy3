\name{setDefaultNodeFontSize}
\alias{setDefaultNodeFontSize}
\alias{setDefaultNodeFontSize,CytoscapeConnectionClass-method}
\title{setDefaultNodeFontSize}
\description{
In the specified CytoscapeWindow, stipulate the default font
size for all nodes other than those mentioned in a
node font size rule.
}
\usage{
setDefaultNodeFontSize(obj, new.size, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.size}{a \code{String} object, a positive integer.}
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

  cw <- CytoscapeWindow ('setDefaultNodeFontSize.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultNodeFontSize (cw, 32)
  # redraw (cw) --> not required anymore
}

\keyword{graph}
