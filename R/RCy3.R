#' RCy3: Functions to Access and Control Cytoscape
#'
#' @description Vizualize, analyze and explore networks using Cytoscape via R.
#' 
#' @details To learn more about RCy3, start with the vignettes:
#' \code{browseVignettes("RCy3")}
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
#' @seealso createNetworkFromGraph, createGraphFromNetwork, makeSimpleIgraph
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
    
    g = graph::addNode ('node 1', g)
    g = graph::addNode ('node 3', g)
    g = graph::addNode ('node 2', g, edges = list('node 3'))
    g = graph::addNode ('node 0', g, edges = list(c('node 1','node 2','node 3')))
    
    graph::nodeDataDefaults (g, attr='group') = 'none'
    graph::nodeDataDefaults (g, attr='score') = as.integer(0)
    graph::edgeDataDefaults (g, attr='interaction') = 'undefined'
    graph::edgeDataDefaults (g, attr='weight') = 0.0
    
    graph::nodeData (g, 'node 0', 'group') = 'A'
    graph::nodeData (g, 'node 1', 'group') = 'A'
    graph::nodeData (g, 'node 2', 'group') = 'B'
    graph::nodeData (g, 'node 3', 'group') = 'B'
    graph::nodeData (g, 'node 0', 'score') = as.integer(20)
    graph::nodeData (g, 'node 1', 'score') = as.integer(10)
    graph::nodeData (g, 'node 2', 'score') = as.integer(15)
    graph::nodeData (g, 'node 3', 'score') = as.integer(5)
    
    graph::edgeData (g, 'node 0', 'node 1', 'interaction') = 'inhibits'
    graph::edgeData (g, 'node 0', 'node 2', 'interaction') = 'interacts'
    graph::edgeData (g, 'node 0', 'node 3', 'interaction') = 'activates'
    graph::edgeData (g, 'node 2', 'node 3', 'interaction') = 'interacts'
    graph::edgeData (g, 'node 0', 'node 1', 'weight') = 5.1
    graph::edgeData (g, 'node 0', 'node 2', 'weight') = 3.0
    graph::edgeData (g, 'node 0', 'node 3', 'weight') = 5.2
    graph::edgeData (g, 'node 2', 'node 3', 'weight') = 9.9
    
    return (g)
} # makeSimpleGraph

#-------------------------------------------------------------------------------
#' @title Make Simple Igraph
#'
#' @description This function creates a simple iGraph object with various
#' node and edge attribute types to help demonstrate round trip conversion with
#' Cytoscape networks via RCy3.
#' @return A \code{igraph} object with a few nodes, edges and attributes
#' @seealso createNetworkFromIgraph, createIgraphFromNetwork, makeSimpleGraph
#' @examples {
#' makeSimpleIgraph()
#' }
#' @importFrom igraph make_graph
#' @importFrom igraph set.vertex.attribute
#' @importFrom igraph set.edge.attribute
#' @export
makeSimpleIgraph = function() {
    ig <- igraph::make_graph(c("node 0","node 1",
                               "node 0","node 2",
                               "node 0","node 3",
                               "node 2","node 3"), 
                             directed = TRUE)
    ig <- igraph::set.vertex.attribute(ig, 'group', value=c('A','A','B','B'))
    ig <- igraph::set.vertex.attribute(ig, 'score', value=as.integer(c(20,10,15,5)))
    ig <- igraph::set.edge.attribute(ig, 'interaction', value=c("inhibits",
                                                                "interacts",
                                                                "activates",
                                                                "interacts"))
    ig <- igraph::set.edge.attribute(ig, 'weight', value=c(5.1,3.0,5.2,9.9))
    return (ig)
} 

#-------------------------------------------------------------------------------
#' @title Set Catchup Filter Delay
#'
#' @description This function sets an internal delay variable that allows
#' Cytoscape to "catchup" prior to subsequent functions. Call without specifying
#' \code{secs} to restore default value. 
#' @param secs Number of seconds to delay.
#' @return None
#' @details This delay is only necessary while concurrency bugs exist in the
#' Cytoscape application. This delay may need to be increased from the default
#' value in certain use cases, e.g., larger networks.
#' @seealso setModelPropagationSecs, setCatchupNetworkSecs
#' @examples {
#' setCatchupFilterSecs(2)
#' setCatchupFilterSecs() #restores default delay
#' }
#' @export
setCatchupFilterSecs <- function(secs=1){
    assign(".CATCHUP_FILTER_SECS", secs, envir = RCy3env)
}

#-------------------------------------------------------------------------------
#' @title Set Model Propagation Delay
#'
#' @description This function sets an internal delay variable that allows
#' Cytoscape to "catchup" prior to subsequent functions. Call without specifying
#' \code{secs} to restore default value.
#' @param secs Number of seconds to delay.
#' @return None
#' @details This delay is only necessary while concurrency bugs exist in the
#' Cytoscape application. This delay may need to be increased from the default
#' value in certain use cases, e.g., larger networks.
#' @seealso setCatchupFilterSecs, setCatchupNetworkSecs
#' @examples {
#' setModelPropagationSecs(2)
#' setModelPropagationSecs() #restores default delay
#' }
#' @export
setModelPropagationSecs <- function(secs=5){
    assign(".MODEL_PROPAGATION_SECS", secs, envir = RCy3env)
}

#-------------------------------------------------------------------------------
#' @title Set Catchup Network Delay
#'
#' @description This function sets an internal delay variable that allows
#' Cytoscape to "catchup" prior to subsequent functions. Call without specifying
#' \code{secs} to restore default value.
#' @param secs Number of seconds to delay.
#' @return None
#' @details This delay is only necessary while concurrency bugs exist in the
#' Cytoscape application. This delay may need to be increased from the default
#' value in certain use cases, e.g., larger networks.
#' @seealso setModelPropagationSecs, setCatchupFilterSecs
#' @examples {
#' setCatchupNetworkSecs(2)
#' setCatchupNetworkSecs() #restores default delay
#' }
#' @export
setCatchupNetworkSecs <- function(secs=2){
    assign(".CATCHUP_NETWORK_SECS", secs, envir = RCy3env)
}