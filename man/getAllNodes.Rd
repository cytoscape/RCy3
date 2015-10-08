\name{getAllNodes}
\alias{getAllNodes}
\alias{getAllNodes,CytoscapeWindowClass-method}
\title{getAllNodes}
\description{
Retrieve the identifiers of all the nodes in the current graph - a list
of strings.
}
\usage{
getAllNodes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings.  Note that node names are returned -- their original and
primary identifiers -- and that these may be different from the node labels that you see when you
look at the graph in Cytoscape.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getAllNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  print (getAllNodes (cw))
    #  [1] "C" "B" "A"
}

\keyword{graph}
