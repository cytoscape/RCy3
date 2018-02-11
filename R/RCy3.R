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


# ------------------------------------------------------------------------------
makeRandomGraph = function(node.count=12, seed=123) {
    set.seed(seed); 
    #if(node.count > 26) node.count = 26
    node.names = as.character(1:node.count)
    g = randomGraph(node.names, M <- 1:2, p = 0.6)
    attr(edgeDataDefaults(g, attr="weight"), "class") = "DOUBLE"
    edgeDataDefaults(g, 'pmid') = '9988778899'
    attr(edgeDataDefaults(g, attr="pmid"), "class") = "STRING"
    return(g)
} 

#------------------------------------------------------------------------------------------------------------------------
#' @importFrom methods new
makeSimpleGraph = function() {
    g = new ('graphNEL', edgemode='directed')
    
    g = .initNodeAttribute (g, 'type', 'char', 'undefined')
    g = .initNodeAttribute (g, 'lfc', 'numeric', 1.0)
    g = .initNodeAttribute (g, 'label', 'char', 'default node label')
    g = .initNodeAttribute (g, 'count', 'integer', 0)
    
    g = .initEdgeAttribute (g, 'edgeType', 'char', 'undefined')
    g = .initEdgeAttribute (g, 'score', 'numeric', 0.0)
    g = .initEdgeAttribute (g, 'misc',   'char', 'default misc')
    
    g = graph::addNode ('A', g)
    g = graph::addNode ('B', g)
    g = graph::addNode ('C', g)
    nodeData (g, 'A', 'type') = 'kinase'
    nodeData (g, 'B', 'type') = 'transcription factor'
    nodeData (g, 'C', 'type') = 'glycoprotein'
    
    nodeData (g, 'A', 'lfc') = -3.0
    nodeData (g, 'B', 'lfc') = 0.0
    nodeData (g, 'C', 'lfc') = 3.0
    
    nodeData (g, 'A', 'count') = 2
    nodeData (g, 'B', 'count') = 30
    nodeData (g, 'C', 'count') = 100
    
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
#------------------------------------------------------------------------------------------------------------------------
# create, display and render the 3-node, 3-edge simple graph
demoNetworkFromGraph = function (base.url=.defaultBaseUrl) {
    window.title = 'demo.simpleGraph'
    if (window.title %in% as.character(getNetworkList (base.url)))
        deleteNetwork(window.title, base.url)
    
    g.simple = makeSimpleGraph()
    net.suid = createNetworkFromGraph(g.simple, window.title)
    layoutNetwork('force-directed', network=net.suid, base.url = base.url)
    setNodeLabelMapping ('label', network=net.suid, base.url = base.url)
    
    node.attribute.values = c("kinase",  "transcription factor")
    colors =                c('#A0AA00', '#FF0000')
    setNodeBorderWidthDefault(5, network=net.suid, base.url = base.url)
    setNodeBorderColorMapping(cws, 'type', node.attribute.values, colors, 
                              mode='lookup', default.color='#88FF22', 
                              network=net.suid, base.url = base.url)
    count.control.points = c(2, 30, 100)
    sizes                = c(20, 50, 100)
    setNodeSizeMapping('count', count.control.points, sizes, mode='interpolate',
                       network=net.suid, base.url = base.url)
    setNodeColorMapping('lfc', c(-3.0, 0.0, 3.0), 
                        c('#00FF00', '#FFFFFF', '#FF0000'), 
                        mode='interpolate',
                        network=net.suid, base.url = base.url)
    return(net.suid)
} # demoNetworkFromGraph
