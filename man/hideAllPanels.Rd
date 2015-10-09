\name{hideAllPanels}
\alias{hideAllPanels}
\alias{hideAllPanels,CytoscapeConnectionClass-method}
\title{hideAllPanels}

\description{
All panels will be hidden, and no longer visible in the Cytoscape
Desktop of, if floating, elsewhere on the computer screen.  }
\usage{
hideAllPanels(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  floatPanel
  dockPanel
  hidePanel
}


\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  hideAllPanels (cy)
}


\keyword{graph}
