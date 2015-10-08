\name{setDefaultNodeLabelColor}
\alias{setDefaultNodeLabelColor}
\alias{setDefaultNodeLabelColor,CytoscapeConnectionClass-method}
\title{setDefaultNodeLabelColor}
\description{
In the specified CytoscapeWindow, stipulate the color for all node
labels.  There is, at present, no mapping rule for this trait.

}
\usage{
setDefaultNodeLabelColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
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
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setDefaultNodeLabelColor.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed')
  setDefaultNodeLabelColor (cw, '#FFFFFF') # white node labels
  redraw (cw)
}


\keyword{graph}
