\name{redraw}
\alias{redraw}
\alias{redraw,CytoscapeWindowClass-method}
\title{redraw}
\description{
Ask Cytoscape to redraw all nodes and edges,
applying the vizmap rules.
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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  displayGraph
  layoutNetwork
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('redraw.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)  # applies default vizmap (rendering) rules, plus any you
               # have specified
}

}
\keyword{graph}
