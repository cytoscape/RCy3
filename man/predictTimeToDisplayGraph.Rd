\name{predictTimeToDisplayGraph}
\alias{predictTimeToDisplayGraph}
\alias{predictTimeToDisplayGraph,CytoscapeWindowClass-method}
\title{predictTimeToDisplayGraph}
\description{
Use simple heuristics and previously collected timing to predict the
length of time that will be required to send the R graph across the
CyREST wire to Cytoscape.
}
\usage{
predictTimeToDisplayGraph(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
Time in seconds.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  CytoscapeWindow
}

\examples{
  dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('predictTimeToDisplayGraph.test', graph=makeSimpleGraph(), 
                             collectTimings=TRUE)
  message (paste ('estimated time: ', predictTimeToDisplayGraph (cw)))
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
}

}
\keyword{graph}
