\name{selectFirstNeighborsOfSelectedNodes}
\alias{selectFirstNeighborsOfSelectedNodes}
\alias{sfn,CytoscapeWindowClass-method}
\alias{sfn}
\alias{selectFirstNeighborsOfSelectedNodes,CytoscapeWindowClass-method}
\title{selectFirstNeighborsOfSelectedNodes}
\description{
Expand the selection by adding the first neighbors, in the Cytosape network, of
the nodes currently selected (again, in the Cytoscape network).  The R
graph is unchanged.
}
\usage{
selectFirstNeighborsOfSelectedNodes (obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  getSelectedNodeCount
  getSelectedNodes
  hideSelectedNodes
  getFirstNeighbors
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('selectFirstNeighborsOfSelecteNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw)
  clearSelection (cw)
  selectNodes (cw, 'A')
  selectFirstNeighborsOfSelectedNodes (cw)
  print (sort (getSelectedNodes (cw)))
    # [1] "A" "B" "C"
}

\keyword{graph}
