\name{redraw}
\alias{redraw}
\alias{redraw,CytoscapeWindowClass-method}
\title{redraw}
\description{
Asks Cytoscape to redraw all nodes and edges, applying the vizmap rules.
}
\usage{
redraw(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  displayGraph
  layout
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('redraw.test', graph=makeSimpleGraph())
  redraw (cw)
}

\keyword{graph}
