\name{layoutNetwork}
\alias{layoutNetwork}
\alias{layoutNetwork,CytoscapeWindowClass-method}
\title{layoutNetwork}
\description{
Layout the current graph according to the specified algorithm.
}
\usage{
layoutNetwork(obj, layout.name='grid')
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{layout.name}{a \code{string}, one of the values returned by
    \code{getLayoutNames}, 'grid' by default. }
}

\value{
Nothing.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  getLayoutNameMapping
  getLayoutNames
  restoreLayout
  saveLayout
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('layout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  possible.layout.names <- getLayoutNames(cw)
  # choose one of the layouts e.g.:
  layoutNetwork (cw, possible.layout.names[1])
  # or:
  layoutNetwork (cw, 'force-directed')
  
  redraw (cw)  # applies default vizmap (rendering) rules, plus any you
               # have specified
}

\keyword{graph}
