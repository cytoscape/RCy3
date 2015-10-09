\name{getSelectedNodes}
\alias{getSelectedNodes}
\alias{getSelectedNodes,CytoscapeWindowClass-method}
\title{getSelectedNodes}
\description{
Retrieve the names of all the nodes selected in the current graph.
}
\usage{
getSelectedNodes(obj)
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

  cw <- CytoscapeWindow ('getSelectedNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'force-directed')
  redraw (cw)
    # in Cytoscape, interactively select two nodes, or
  selectNodes (cw, c ('A','B'))
  getSelectedNodes (cw)
  # [1] "A" "B"
}

\keyword{graph}
