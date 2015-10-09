\name{selectNodes}
\alias{selectNodes}
\alias{selectNodes,CytoscapeWindowClass-method}
\title{selectNodes}
\description{
Select the specified nodes.
}
\usage{
selectNodes(obj, node.names, preserve.current.selection=TRUE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{list} of strings, the names of nodes to select. }
  \item{preserve.current.selection}{a \code{logical} object. }
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
}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('selectNodes.test', graph=makeSimpleGraph())
  clearSelection (cw)
  selectNodes (cw, c ('A', 'B'))
  getSelectedNodes (cw)
    # [1] "A" "B"
}

\keyword{graph}
