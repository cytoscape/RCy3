\name{layoutNetwork}
\alias{layoutNetwork}
\alias{layoutNetwork,CytoscapeWindowClass-method}
\title{layoutNetwork}
\description{
Layout the current graph according to the specified algorithm.
}
\usage{
layoutNetwork(obj, layout.name='jgraph-spring')
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{layout.name}{a \code{string}, one of the values returned by
    \code{getLayoutNames}, 'jgraph-spring' by default. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  getNodeShapes
}

\examples{
  cw <- new.CytoscapeWindow ('layout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'jgraph-spring')
  redraw (cw)  # applies default vizmap (rendering) rules, plus any you
               # have specified
}


\keyword{graph}
