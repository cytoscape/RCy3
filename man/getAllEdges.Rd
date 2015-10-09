\name{getAllEdges}
\alias{getAllEdges}
\alias{getAllEdges,CytoscapeWindowClass-method}
\title{getAllEdges}
\description{
Retrieve all edges in the current graph, expressed in the standard Cytoscape notation.
}
\usage{
getAllEdges(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getAllEdges.test', graph=makeSimpleGraph())
  displayGraph (cw)
  print (getAllEdges (cw))
    # [1] "C (undefined) A"  "B (synthetic lethal) C" "A (phosphorylates) B"  
}

\keyword{graph}
