\name{setNodeHeightDirect}
\alias{setNodeHeightDirect}
\alias{setNodeHeightDirect,CytoscapeWindowClass-method}
\title{setNodeHeightDirect}
\description{
In the specified CytoscapeWindow, set the height of the specified node(s).
Note that the node dimensions (height and width) must be unlocked for
this to work. If they ARE locked, then node and height change together,
as specified by a node size rule, or the setNodeSizeDirect method.
}
\usage{
setNodeHeightDirect(obj, node.names, new.heights)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one ore more \code{String} objects.}
  \item{new.heights}{one or more \code{integers}, in pixel units.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeWidthDirect
  lockNodeDimensions
  setNodeSizeDirect
  setNodeHeightDirect
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeHeightDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'force-directed')
  lockNodeDimensions (cw, FALSE)
  setNodeHeightDirect (cw, 'A', 32)
  setNodeHeightDirect (cw, c('A', 'B'), 15)
  setNodeHeightDirect (cw, c('C', 'B'), c(15, 30))
}

}
\keyword{graph}
