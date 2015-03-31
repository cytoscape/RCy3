\name{getSelectedEdges}
\alias{getSelectedEdges}
\alias{getSelectedEdges,CytoscapeWindowClass-method}
\title{getSelectedEdges}
\description{
Retrieve the identifiers of all the edges selected in the current graph.
}
\usage{
getSelectedEdges(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings.
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('getSelectedEdges.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
    # in Cytoscape, interactively select two edges
    # doesn't work yet:  selectEdges (cwe, "A (phosphorylates) B")
  getSelectedEdges (cw)
}

\keyword{graph}
