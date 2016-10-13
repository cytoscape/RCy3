\name{setDefaultNodeSize}
\alias{setDefaultNodeSize}
\alias{setDefaultNodeSize,CytoscapeConnectionClass-method}
\title{setDefaultNodeSize}
\description{
In the specified CytoscapeConnection, stipulate the default size
for all nodes other than those mentioned in a node size rule.
}
\usage{
setDefaultNodeSize(obj, new.size, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.size}{a \code{integer} object, typically 20 to 100.}
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
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultNodeSize.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'grid')
  setDefaultNodeSize (cw, 60) # an intermediate value
  # redraw (cw) --> not required anymore
}

}
\keyword{graph}
