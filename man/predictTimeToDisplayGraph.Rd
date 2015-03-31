\name{predictTimeToDisplayGraph}
\alias{predictTimeToDisplayGraph}
\alias{predictTimeToDisplayGraph,CytoscapeWindowClass-method}
\title{predictTimeToDisplayGraph}
\description{
Use simple heuristics and previously collected timimg to predict the
length of time that will be required to send the R graph across the
XMLRPC wire to Cytoscape.
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
\author{Paul Shannon}

\seealso{
  new.CytoscapeWindow
}

\examples{
  cw <- CytoscapeWindow ('predictTimeToDisplayGraph.test', graph=makeSimpleGraph(), 
                             collectTimings=TRUE)
  message (paste ('estimated time: ', predictTimeToDisplayGraph (cw)))
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
}


\keyword{graph}
