\name{setNodeSizeDirect}
\alias{setNodeSizeDirect}
\alias{setNodeSizeDirect,CytoscapeWindowClass-method}
\title{setNodeSizeDirect}
\description{
In the specified CytoscapeWindow, set the size of the specified node(s).
Note that the node dimensions (height and width) must be unlocked for
this to work. Node height and width change together.
}
\usage{
setNodeSizeDirect(obj, node.names, new.sizes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.sizes}{one or more \code{integers}, in pixel units.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  lockNodeDimensions
  setNodeWidthDirect
  setNodeHeightDirect
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('setNodeSizeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  setNodeSizeDirect (cw, 'A', 32)
  setNodeSizeDirect (cw, getAllNodes(cw), 15)
  setNodeSizeDirect (cw, c('A', 'B'), c(30, 40))
}

}
\keyword{graph}
