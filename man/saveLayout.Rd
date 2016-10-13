\name{saveLayout}
\alias{saveLayout}
\alias{saveLayout,CytoscapeWindowClass-method}
\title{saveLayout}
\description{
Save the current layout (that is, node positions) to the specified file.
}
\usage{
saveLayout(obj, filename, timestamp.in.filename=FALSE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{filename}{a \code{string}.}
  \item{timestamp.in.filename}{\code{logical}.}
}

\value{
Nothing.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  restoreLayout
}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('saveLayout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  saveLayout (cw, 'layout.RData')
  layoutNetwork(cw, 'fruchterman-rheingold')
  restoreLayout (cw, 'layout.RData')
  # you might need to adjust the zoom
  saveLayout (cw, 'layout2', timestamp.in.filename=TRUE)
}

}
\keyword{graph}
