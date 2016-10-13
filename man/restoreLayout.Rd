\name{restoreLayout}
\alias{restoreLayout}
\alias{restoreLayout,CytoscapeWindowClass-method}
\title{restoreLayout}
\description{
Restore the current layout (that is, node positions) from the
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  saveLayout
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('restoreLayout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  saveLayout (cw, 'layout.RData')
  layoutNetwork(cw, 'isom')
  restoreLayout (cw, 'layout.RData')
  # you might need to adjust the zoom
}

}
\keyword{graph}
