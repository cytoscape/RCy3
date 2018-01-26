# RCy3.R
#--------------------------------------------------------------------------------
# this code is for the Bioconductor build system. You should never need to set or
# read these environment variables in ordinary use.
.BBSOverride <- function(host, port) {
    ret <- list()
    if ((Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE") != "")) {
        host = Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE")
        port = as(Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE"),"integer")
    }
    if (.Platform$r_arch == "x64") {
        if ((Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE_64") != "") &&  (Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE_64") != "")) {
            host = Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE_64")
            port = as(Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE_64"),"integer")
        }
    }
    #cat(paste("Using host", host, "and port", port, "."))
    
    ret["host"] <- host
    ret["port"] <- port
    ret
}


# ------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))

# ------------------------------------------------------------------------------
setClass("CytoscapeConnectionClass", 
         slots = c(uri="character",api="character"), 
         prototype = prototype(uri="http://localhost:1234", api="v1")
)
# Constructor
CytoscapeConnection = function(host='localhost', port=1234) {
    res <- .BBSOverride(host, port)
    host = res$host
    port = res$port
    uri = sprintf('http://%s:%s', host, port)
    cc = new('CytoscapeConnectionClass', uri = uri)
    if (!url.exists(uri)){
        write(sprintf('Connection failed.'), stderr())
        write(sprintf('To troubleshoot: 1) Please ensure that you have Cytoscape open'), stderr())
        write(sprintf('2) that the latest version of CyREST is installed.'), stderr())
        write(sprintf('3) that Cytoscape uses Java 8 (not 7 or below).'), stderr())
        write(sprintf('To help troubleshooting, please check:'), stderr())
        write(sprintf('http://www.cytoscape.org/troubleshooting.html'), stderr())
        return()
    }
    cc@api = apiVersion(cc)
    return(cc)
} # END CytoscapeConnection

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
