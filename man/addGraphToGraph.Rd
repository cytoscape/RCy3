\name{addGraphToGraph}
\alias{addGraphToGraph}
\alias{addGraphToGraph,CytoscapeWindowClass-method}
\title{addGraphToGraph}
\description{
Given a CytoscapeWindow containing a graph, this method adds new nodes,
edges, and their attributes.  Thus, it is the way to extend a graph --
to merge a new graph with an existing one.  A typical use would be to
add a second KEGG pathway to a CytoscapeWindow upon discovering that
two KEGG pathways overlap, sharing some enzymes and some reactions.  No
existing attributes are written over.
}
\usage{
addGraphToGraph(obj, other.graph)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{other.graph}{a \code{graph} object. }
}

\value{
None.
}
\author{Paul Shannon}

\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())
  window.name <- 'demo addGraphToGraph'
  cw3 <- CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork(cw3)

    # create a new graph, which adds two nodes, and edges between them
    # and an existing node, A

  g2 <- new("graphNEL", edgemode = "directed")
  g2 <- graph::addNode ('A', g2)
  g2 <- graph::addNode ('D', g2)
  g2 <- graph::addNode ('E', g2)

  g2 <- initNodeAttribute (g2, "label", "char", "default node label")
  g2 <- initEdgeAttribute (g2, "edgeType", "char", "unspecified")
  g2 <- initEdgeAttribute (g2, "probability", "numeric", 0.0)

  nodeData (g2, 'D', 'label') <- 'Gene D'
  nodeData (g2, 'E', 'label') <- 'Gene E'

  g2 <- graph::addEdge ('D', 'E', g2)
  g2 <- graph::addEdge ('A', 'E', g2)

  edgeData (g2, 'D', 'E', 'probability') <- 0.95
  edgeData (g2, 'D', 'E', 'edgeType') <- 'literature'
  edgeData (g2, 'A', 'E', 'edgeType') <- 'inferred'

  addGraphToGraph (cw3, g2)
  redraw (cw3)
  layoutNetwork(cw3)

}

\keyword{graph}
