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


#------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods new
#' @importFrom graph addNode
#' @importFrom graph addEdge
#' @importFrom graph nodeData
#' @importFrom graph edgeData
#' @importFrom graph nodeDataDefaults
#' @importFrom graph edgeDataDefaults
make_graphnel = function() {
    g = new ('graphNEL', edgemode='directed')
    
    nodeDataDefaults (g, attr='type') = 'undefined'
    nodeDataDefaults (g, attr='lfc') = 1.0
    nodeDataDefaults (g, attr='label') = 'default node label'
    nodeDataDefaults (g, attr='count') = 0
    
    edgeDataDefaults(g, attr='edgeType') = 'undefined'
    edgeDataDefaults(g, attr='score') = 0.0
    edgeDataDefaults(g, attr='misc') = 'default misc'

    g = graph::addNode ('A', g)
    g = graph::addNode ('B', g)
    g = graph::addNode ('C', g)
    graph::nodeData (g, 'A', 'type') = 'kinase'
    nodeData (g, 'B', 'type') = 'transcription factor'
    nodeData (g, 'C', 'type') = 'glycoprotein'
    
    nodeData (g, 'A', 'lfc') = -3.0
    nodeData (g, 'B', 'lfc') = 0.0
    nodeData (g, 'C', 'lfc') = 3.0
    
    nodeData (g, 'A', 'count') = as.integer(2)
    nodeData (g, 'B', 'count') = as.integer(30)
    nodeData (g, 'C', 'count') = as.integer(100)
    
    nodeData (g, 'A', 'label') = 'Gene A'
    nodeData (g, 'B', 'label') = 'Gene B'
    nodeData (g, 'C', 'label') = 'Gene C'
    
    g = graph::addEdge ('A', 'B', g)
    g = graph::addEdge ('B', 'C', g)
    g = graph::addEdge ('C', 'A', g)
    
    edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
    edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'
    
    edgeData (g, 'A', 'B', 'score') =  35.0
    edgeData (g, 'B', 'C', 'score') =  -12
    
    return (g)
    
} # makeSimpleGraph

