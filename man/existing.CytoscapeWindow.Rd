\name{existing.CytoscapeWindow}
\alias{existing.CytoscapeWindow}
\title{
existing.CytoscapeWindow
}
\description{
The constructor for the CytoscapeWindowClass, used when Cytoscape
already contains and displays a network.
}
\usage{
existing.CytoscapeWindow (title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE)
}

\arguments{
  \item{title}{
    A character string, this is the name of an existing Cytoscape
    network window.   This name enables RCytoscape to identify and
  connect to the proper Cytoscape window and network that it contains.
}
  \item{host}{
    Defaults to 'localhost', this is the domain name of a machine which
    is running Cytoscape with the appropriate CyREST plugin.
}
  \item{port}{
    Defaults to 1234, this may be any port to which CyREST
    is listening.
}
  \item{copy.graph.from.cytoscape.to.R}{
     Defaults to FALSE, but you may want a copy in R, for further exploration.
}
}
\value{
  An object of the existing.CytoscapeWindow Class.
}
\author{
  Paul Shannon
}


\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cy <- CytoscapeConnection ()
  cw <- CytoscapeWindow ('demo.existing', graph=makeSimpleGraph ())
  displayGraph (cw)
  #cw2 <- existing.CytoscapeWindow ('demo.existing',  copy.graph.from.cytoscape.to.R=TRUE)

}

\keyword{ graph }
