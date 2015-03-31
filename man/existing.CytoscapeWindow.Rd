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
existing.CytoscapeWindow (title, host='localhost', rpcPort=9000, copy.graph.from.cytoscape.to.R=FALSE)
}

\arguments{
  \item{title}{
    A character string, this is the name of an existing Cytoscape
    network window.   This name enables RCytoscape to identify and
  connect to the proper Cytoscape window and network that it contains.
}
  \item{host}{
    Defaults to 'localhost', this is the domain name of a machine which
    is running Cytoscape with the appropriate XMLRPC server plugin.
}
  \item{rpcPort}{
    Defaults to 9000, this may be any port to which the CytoscapeRPC
    server is listening.
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
  cy <- CytoscapeConnection ()
  cw <- new.CytoscapeWindow ('demo.existing', graph=makeSimpleGraph ())
  displayGraph (cw)
  cw2 <- existing.CytoscapeWindow ('demo.existing',  copy.graph.from.cytoscape.to.R=TRUE)

}

\keyword{ graph }
