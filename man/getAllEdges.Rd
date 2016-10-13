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
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('getAllEdges.test', graph=makeSimpleGraph())
  displayGraph (cw)
  print (getAllEdges (cw))
    # [1] "A (phosphorylates) B"   "B (synthetic lethal) C" "C (undefined) A"    
}

}
\keyword{graph}
