#' @include CytoscapeConnectionClass.R Internal.R 

# ------------------------------------------------------------------------------
setClass("CytoscapeWindowClass", 
         slots = c(
             title="character", 
             suid='character', 
             graph="graphBase", 
             collectTimings="logical",
             node.suid.name.dict="list",
             edge.node.suid.name.dict="list",
             view.id='numeric'), 
         contains = 'CytoscapeConnectionClass', 
         prototype = prototype(
             title="R graph", 
             uri="http://localhost:1234",
             api="v1",
             graph=new("graphNEL", edgemode='directed'), 
             collectTimings=FALSE, 
             node.suid.name.dict=list(),
             edge.node.suid.name.dict=list())
)
setValidity("CytoscapeWindowClass", function(object) {
    if (length(object@title) != 1){
        "'title' is not a single string" 
    }
    else if (!nzchar(object@title)){
        "'title' is an empty string"
    }
    validObject(object@graph)
}) 
# Constructor
CytoscapeWindow = function(title, graph=new('graphNEL', edgemode='directed'), uri="http://localhost:1234", api="v1",
                           create.window=TRUE, overwriteWindow=FALSE, collectTimings=FALSE){
    
    # new 'CytoscapeConnectionClass' object
    cy.conn = new('CytoscapeConnectionClass', uri=uri, api=api)
    
    # if the user has specified, delete already existing window(s) with the same title
    if (overwriteWindow) {
        if (title %in% as.character(getNetworkList(cy.conn))) {
            deleteNetwork(cy.conn, title)
        }
    }
    
    if (!is.na(getNetworkSuid(cy.conn, title))) {
        write(sprintf('There is already a window in Cytoscape named "%s".', title), stderr())
        write(sprintf('Please use a unique name, or set "overwriteWindow=TRUE".'), stderr())
        stop()
    }
    
    # add a label to each node if not already present. default label is the node name, the node ID    	
    if (is.classic.graph(graph)){
        if (edgemode(graph) == 'undirected') {
            graph = remove.redundancies.in.undirected.graph(graph) #AP: not sure this is needed anymore...
        }
    }
    # are all node attributes properly initialized?
    node.attributes = noa.names(graph)
    if (length(node.attributes) > 0) {
        check.list = list()
        for (node.attribute in node.attributes) {
            check.list[[node.attribute]] = properlyInitializedNodeAttribute(graph, node.attribute)
        }
        uninitialized.attributes = which(check.list == FALSE)
        if (length(uninitialized.attributes) > 0) {
            write(sprintf("%d uninitialized node attribute/s", length(uninitialized.attributes)), stderr())
            return()
        }
    } # if node.attributes
    
    # are all edge attributes properly initialized?
    edge.attributes = eda.names(graph)
    if (length(edge.attributes) > 0) {
        check.list = list()
        for (edge.attribute in edge.attributes) {
            check.list[[edge.attribute]] = properlyInitializedEdgeAttribute(graph, edge.attribute)
        }
        uninitialized.attributes = which(check.list == FALSE)
        if (length(uninitialized.attributes) > 0) {
            write(sprintf("%d uninitialized edge attribute/s", length(uninitialized.attributes)), stderr())
            return()
        }
    } # if edge.attributes
    
    if (!'label' %in% noa.names(graph)) {
        write('nodes have no label attribute -- adding default labels', stderr())
        graph = initNodeAttribute(graph, 'label', 'char', 'noLabel')
        if (length(nodes(graph) > 0)) {
            nodeData(graph, nodes(graph), 'label') = nodes(graph) # nodes(graph) returns strings
        }
    }
    
    # create new 'CytoscapeWindow' object
    cw = new('CytoscapeWindowClass', title=title, graph=graph, uri=uri, api=api,
             collectTimings=collectTimings, node.suid.name.dict = list(), edge.node.suid.name.dict=list())
    
    if (create.window) {
        cw@suid = sendNetworkFromGraph(cw)
    }
    # let user know that a new window was created
    write(sprintf('New window named "%s" was created in Cytoscape.', title), stderr())
    
    return (cw)
    
} # END 'CytoscapeWindow' constructor

#------------------------------------------------------------------------------------------------------------------------
properlyInitializedNodeAttribute = function (graph, attribute.name) {
    
    if (length (nodes (graph)) == 0)
        return (TRUE)
    
    caller.specified.attribute.class = attr (nodeDataDefaults (graph, attribute.name), 'class')
    
    if (is.null (caller.specified.attribute.class)) {
        msg1 = sprintf ('Error!  Node attribute not initialized "%s"', attribute.name)
        msg2 = sprintf ('        You should call:')
        msg3 = sprintf ('        initNodeAttribute (graph, attribute.name, attribute.type, default.value)')
        msg4 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
        msg5 = sprintf ('        example:  g <- initNodeAttribute (g, "nodeType", "char", "molecule")')
        msg6 = sprintf ('             or:  g <- initNodeAttribute (g, "pValue", "numeric", 1.0)')
        write (msg1, stderr ())
        write (msg2, stderr ())
        write (msg3, stderr ())
        write (msg4, stderr ())
        write (msg5, stderr ())
        write (msg6, stderr ())
        return (FALSE)
    }
    return (TRUE)
    
} # properlyInitializedNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
properlyInitializedEdgeAttribute = function (graph, attribute.name) {
    
    if (length (edgeNames (graph)) == 0)
        return (TRUE)
    
    caller.specified.attribute.class = attr (edgeDataDefaults (graph, attribute.name), 'class')
    
    if (is.null (caller.specified.attribute.class)) {
        msg1 = sprintf ('Error!  "%s" edge attribute not initialized.', attribute.name)
        msg2 = sprintf ('        You should call:')
        msg3 = sprintf ('        initEdgeAttribute (graph, attribute.name, attribute.type, default.value)')
        msg4 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
        msg5 = sprintf ('        example:  g <- initEdgeAttribute (g, "edgeType", "char", "molecule")')
        msg6 = sprintf ('             or:  g <- initEdgeAttribute (g, "pValue", "numeric", 1.0)')
        write (msg1, stderr ())
        write (msg2, stderr ())
        write (msg3, stderr ())
        write (msg4, stderr ())
        write (msg5, stderr ())
        write (msg6, stderr ())
        return (FALSE)
    }
    return (TRUE)
    
} # properlyInitializedEdgeAttribute

# ------------------------------------------------------------------------------
setClassUnion('OptionalCyWinClass', c('missing', 'CytoscapeWindowClass'))
setClassUnion('OptionalCyObjClass', c('missing', 'CytoscapeWindowClass','CytoscapeConnectionClass'))
