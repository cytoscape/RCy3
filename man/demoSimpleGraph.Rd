\name{demoSimpleGraph}
\alias{demoSimpleGraph}
\title{
demoSimpleGraph
}
\description{
Create, display and render the 3-node, 3-edge graph, with some biological trappings.
}
\usage{
demoSimpleGraph()
}
\value{
Returns a CytoscapeWindow object, for subsequent manipulation
}
\author{
Paul Shannon
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

cwd <- demoSimpleGraph ()
}

\keyword{graph}
