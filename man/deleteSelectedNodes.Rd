\name{deleteSelectedNodes}
\alias{deleteSelectedNodes}
\alias{deleteSelectedNodes,CytoscapeWindowClass-method}
\title{deleteSelectedNodes}
\description{This function deletes all the selected nodes in Cytoscape.  Edges originating
  or terminating in these nodes will also be deleted. The nodes and edges will
  still exist in the corresponding R graph until you explicitly delete them there
  as well.
}
\usage{
deleteSelectedNodes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Tanja Muetze, Georgi Kolishovski, Paul Shannon}

\seealso{
  selectNodes
  deleteSelectedEdges
 }

\examples{
  \dontrun{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

  cw <- CytoscapeWindow ('deleteSelectedNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'force-directed') 
  print (nodes (cw@graph))
    # [1] "A" "B" "C"
  selectNodes (cw, "B")
  deleteSelectedNodes (cw)
}
}
\keyword{graph}
