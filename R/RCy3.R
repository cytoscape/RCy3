#' RCy3: Functions to Access and Control Cytoscape
#'
#' @description Vizualize, analyze and explore networks using Cytoscape via R.
#' 
#' @details To learn more about RCy3, start with the vignettes:
#' \code{browseVignettes(package = "RCy3")}
#' 
#' @name RCy3
#' @docType package
NULL

# ==============================================================================
# Miscellaneous functions for working with RCy3
#-------------------------------------------------------------------------------
#' @title Make Simple Graph
#'
#' @description This function creates a simple graphNEL object with various
#' node and edge attribute types to help demonstrate round trip conversion with
#' Cytoscape networks via RCy3.
#' @return A \code{graphNEL} object with a few nodes, edges and attributes
#' @seealso createNetworkFromGraph, createGraphFromNetwork, makeSimpleIgraph, 
#' makeSimpleDataFrame
#' @examples {
#' makeSimpleGraph()
#' }
#' @importFrom methods new
#' @importFrom graph addNode
#' @importFrom graph addEdge
#' @importFrom graph nodeData
#' @importFrom graph edgeData
#' @importFrom graph nodeDataDefaults
#' @importFrom graph edgeDataDefaults
#' @export
makeSimpleGraph = function() {
    g = new ('graphNEL', edgemode='directed')
    
    graph::nodeDataDefaults (g, attr='type') = 'undefined'
    graph::nodeDataDefaults (g, attr='lfc') = 1.0
    graph::nodeDataDefaults (g, attr='label') = 'default node label'
    graph::nodeDataDefaults (g, attr='count') = 0
    graph::edgeDataDefaults (g, attr='edgeType') = 'undefined'
    graph::edgeDataDefaults (g, attr='score') = 0.0
    
    g = graph::addNode ('A', g)
    g = graph::addNode ('B', g)
    g = graph::addNode ('C', g)
    graph::nodeData (g, 'A', 'type') = 'kinase'
    graph::nodeData (g, 'B', 'type') = 'transcription factor'
    graph::nodeData (g, 'C', 'type') = 'glycoprotein'
    graph::nodeData (g, 'A', 'lfc') = -3.0
    graph::nodeData (g, 'B', 'lfc') = 0.0
    graph::nodeData (g, 'C', 'lfc') = 3.0
    graph::nodeData (g, 'A', 'count') = as.integer(2)
    graph::nodeData (g, 'B', 'count') = as.integer(30)
    graph::nodeData (g, 'C', 'count') = as.integer(100)
    graph:: nodeData (g, 'A', 'label') = 'Gene A'
    graph::nodeData (g, 'B', 'label') = 'Gene B'
    graph::nodeData (g, 'C', 'label') = 'Gene C'
    
    g = graph::addEdge ('A', 'B', g)
    g = graph::addEdge ('B', 'C', g)
    g = graph::addEdge ('C', 'A', g)
    graph::edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
    graph::edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'
    graph::edgeData (g, 'A', 'B', 'score') =  35.0
    graph::edgeData (g, 'B', 'C', 'score') =  -12
    
    return (g)
} # makeSimpleGraph

#-------------------------------------------------------------------------------
#' @title Make Simple Igraph
#'
#' @description This function creates a simple iGraph object with various
#' node and edge attribute types to help demonstrate round trip conversion with
#' Cytoscape networks via RCy3.
#' @return A \code{igraph} object with a few nodes, edges and attributes
#' @seealso createNetworkFromGraph, createGraphFromNetwork, makeSimpleGraph, 
#' makeSimpleDataFrame
#' @examples {
#' makeSimpleIgraph()
#' }
#' @importFrom igraph make_graph
#' @importFrom igraph set.vertex.attribute
#' @importFrom igraph set.edge.attribute
#' @export
makeSimpleIgraph = function() {
    ig <- igraph::make_graph(c("A","B","B","C","C","A"), directed = TRUE)
    ig <- igraph::set.vertex.attribute(ig, 'type', value=c('kinase','transcription factor','glycoprotein'))
    ig <- igraph::set.vertex.attribute(ig, 'lfc', value=c(-3.0,0.0,3.0))
    ig <- igraph::set.vertex.attribute(ig, 'count', value=as.integer(c(2,30,100)))
    ig <- igraph::set.vertex.attribute(ig, 'label', value=c('Gene A','Gene B','Gene C'))
    ig <- igraph::set.edge.attribute(ig, 'edgeType', value=c('phosphorylates','synthetic lethal','undefined'))
    ig <- igraph::set.edge.attribute(ig, 'score', value=c(35.0,-12,0.0))
    return (ig)
} 


#' nodes <- data.frame(id=c("node 0","node 1","node 2","node 3"),
#'            group=c("A","A","B","B"), # categorical strings
#'            score=as.integer(c(20,10,15,5)), # integers
#'            stringsAsFactors=FALSE)
#' edges <- data.frame(source=c("node 0","node 0","node 0","node 2"),
#'            target=c("node 1","node 2","node 3","node 3"),
#'            interaction=c("inhibits","interacts","activates","interacts"),  # optional
#'            weight=c(5.1,3.0,5.2,9.9), # numeric
#'            stringsAsFactors=FALSE)
#'
#' createNetworkFromDataFrames(nodes,edges)
