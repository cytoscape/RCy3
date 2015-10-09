\name{unhideAll}
\alias{unhideAll}
\alias{unhideAll,CytoscapeWindowClass-method}
\title{unhideAll}
\description{
Currently not implemented yet
}
\usage{
unhideAll(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  selectNodes
  clearSelection
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('unhideAll.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
  clearSelection (cw)
  selectNodes (cw, 'A')
  hideSelectedNodes (cw)
  system ('sleep 2')
  unhideAll (cw)
}


\keyword{graph}
