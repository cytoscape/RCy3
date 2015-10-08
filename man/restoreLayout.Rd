\name{restoreLayout}
\alias{restoreLayout}
\alias{restoreLayout,CytoscapeWindowClass-method}
\title{restoreLayout}
\description{
restore the current layout (that is, node positions) from the
information saved in the supplied filename.
}
\usage{
restoreLayout(obj, filename)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{filename}{a \code{string}}
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  saveLayout
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('restoreLayout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  saveLayout (cw, 'layout.RData')
  layoutNetwork(cw, 'jgraph-circle')
  restoreLayout (cw, 'layout.RData')
}


\keyword{graph}
