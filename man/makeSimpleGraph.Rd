\name{makeSimpleGraph}
\alias{makeSimpleGraph}
\title{
makeSimpleGraph
}
\description{
A 3-node, 3-edge graph, with some biological trappings, useful for demonstrations.
}
\usage{
makeSimpleGraph()
}
\value{
Returns a 3-node, 3-edge graph, with some attributes on the nodes
and edges.
}
\author{
Paul Shannon
}
\examples{
  # first, delete existing windows to save memory:
  deleteAllWindows(CytoscapeConnection())

g = makeSimpleGraph ()

## The function is currently defined as
function () 
{
    g = new("graphNEL", edgemode = "directed")
    nodeDataDefaults(g, attr = "type") = "undefined"
    attr(nodeDataDefaults(g, attr = "type"), "class") = "STRING"
    nodeDataDefaults(g, attr = "lfc") = 1
    attr(nodeDataDefaults(g, attr = "lfc"), "class") = "DOUBLE"
    nodeDataDefaults(g, attr = "label") = "default node label"
    attr(nodeDataDefaults(g, attr = "label"), "class") = "STRING"
    nodeDataDefaults(g, attr = "count") = "0"
    attr(nodeDataDefaults(g, attr = "count"), "class") = "INTEGER"
    edgeDataDefaults(g, attr = "edgeType") = "undefined"
    attr(edgeDataDefaults(g, attr = "edgeType"), "class") = "STRING"
    edgeDataDefaults(g, attr = "score") = 0
    attr(edgeDataDefaults(g, attr = "score"), "class") = "DOUBLE"
    edgeDataDefaults(g, attr = "misc") = ""
    attr(edgeDataDefaults(g, attr = "misc"), "class") = "STRING"
    g = graph::addNode("A", g)
    g = graph::addNode("B", g)
    g = graph::addNode("C", g)
    nodeData(g, "A", "type") = "kinase"
    nodeData(g, "B", "type") = "transcription factor"
    nodeData(g, "C", "type") = "glycoprotein"
    nodeData(g, "A", "lfc") = "-3.0"
    nodeData(g, "B", "lfc") = "0.0"
    nodeData(g, "C", "lfc") = "3.0"
    nodeData(g, "A", "count") = "2"
    nodeData(g, "B", "count") = "30"
    nodeData(g, "C", "count") = "100"
    nodeData(g, "A", "label") = "Gene A"
    nodeData(g, "B", "label") = "Gene B"
    nodeData(g, "C", "label") = "Gene C"
    g = graph::addEdge("A", "B", g)
    g = graph::addEdge("B", "C", g)
    g = graph::addEdge("C", "A", g)
    edgeData(g, "A", "B", "edgeType") = "phosphorylates"
    edgeData(g, "B", "C", "edgeType") = "synthetic lethal"
    edgeData(g, "A", "B", "score") = 35
    edgeData(g, "B", "C", "score") = -12
    return(g)
  }
}

\keyword{graph}
