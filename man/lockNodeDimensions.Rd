\name{lockNodeDimensions}
\alias{lockNodeDimensions}
\alias{lockNodeDimensions,CytoscapeConnectionClass-method}
\title{lockNodeDimensions}
\description{
Select the specified nodes.
}
\usage{
lockNodeDimensions(obj, new.state, visual.style.name='default')
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.state}{a \code{boolean} object, TRUE or FALSE}
  \item{visual.style.name}{a \code{string} object, naming the visual
    style whose 'locked' you wish to change. Defaults to 'default'}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeSizeDirect
  setNodeWidthDirect
  setNodeHeightDirect
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('lockNodeDimensions demo', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  redraw (cw)
  lockNodeDimensions (cw, FALSE)
  setNodeWidthDirect (cw, 'A', 100)
  setNodeHeightDirect (cw, 'A', 50)

}

\keyword{graph}
