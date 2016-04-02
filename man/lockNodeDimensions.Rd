\name{lockNodeDimensions}
\alias{lockNodeDimensions}
\alias{lockNodeDimensions,CytoscapeConnectionClass-method}
\title{lockNodeDimensions}
\description{
Lock the node dimensions. Required for setting the same height and
width if the node size is set.
}
\usage{
lockNodeDimensions(obj, new.state, visual.style.name='default')
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.state}{a \code{boolean} object, TRUE or FALSE}
  \item{visual.style.name}{a \code{string} object, naming the visual
    style whose 'locked' you wish to change. Defaults to 'default'.}
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  setNodeSizeDirect
  setNodeWidthDirect
  setNodeHeightDirect
  setNodeSizeRule
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('lockNodeDimensions demo', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  lockNodeDimensions (cw, FALSE) # not required anymore as this function
   # is called by the other functions.
  setNodeWidthDirect (cw, 'A', 100)
  setNodeHeightDirect (cw, 'A', 50)
}

\keyword{graph}
