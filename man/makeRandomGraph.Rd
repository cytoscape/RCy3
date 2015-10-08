\name{makeRandomGraph}
\alias{makeRandomGraph}
\title{
makeRandomGraph
}
\description{
Create a random undirected graphNEL, useful for testing.  Two default edge
attributes are added, for demonstration purposes.
}
\usage{
makeRandomGraph(node.count=12, seed=123)
}
\arguments{
  \item{node.count}{the number of nodes you wish to see in the graph}
  \item{seed}{an integer which, when supplied, allows reproducibility}
}

\value{
Returns (by default) a 12-node, rather dense undirected  graph, with some attributes on the
nodes and edges.
}
\author{
Paul Shannon
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

g = makeRandomGraph (node.count=12, seed=123)

## The function is currently defined as
function (node.count = 12, seed = 123) 
{
    set.seed(seed)
    node.names = as.character(1:node.count)
    g = randomGraph(node.names, M <- 1:2, p = 0.6)
    attr(edgeDataDefaults(g, attr = "weight"), "class") = "DOUBLE"
    edgeDataDefaults(g, "pmid") = "9988778899"
    attr(edgeDataDefaults(g, attr = "pmid"), "class") = "STRING"
    return(g)
}

}

\keyword{graph}
