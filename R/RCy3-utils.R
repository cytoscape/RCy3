#------ Package Variables ----------------------------------------------------------------------------------------------
.defaultBaseUrl <- 'http://localhost:1234/v1'

#------ Package Functions ----------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# the bioc graph 'edgeNames' function does not detect, distinguish or report 
# reciprocal edges 
# this is fixed here
.rcyEdgeNames = function(g) 
{
    nodes.list = edges(g)
    result = c()
    for(source.node in names(nodes.list)) {
        target.nodes = nodes.list[[source.node]]
        
        if(length(target.nodes) == 0) {
            next;
        }
        for(target.node in target.nodes) {
            tilde.edge.name = sprintf('%s~%s', source.node, target.node) 
            result = c(result, tilde.edge.name)
        } # for target.node
    } # for source.node
    
    return(result)
}


# ------------------------------------------------------------------------------
.isNotHexColor <- function(color){
    if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
        write (sprintf ('Error. %s is not a valid hexadecimal color (has to begin with # and be 7 characters long).', color), stderr ())
        return(TRUE)
    }else{
        return(FALSE)
    }
}
# ------------------------------------------------------------------------------
.findColumnType <- function(columnType){
    if (columnType=="double"){
        return("Double")
    } else if (columnType == "integer"){
        return("Integer")
    } else if (columnType == "logical"){
        return("Boolean")
    } else{
        return("String")
    }
} # findColumnType
# ------------------------------------------------------------------------------
.nodeNameToNodeSUID<-function(node.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        node.SUIDs <- dict[which(dict$name  %in% node.names),'SUID']
        return(node.SUIDs)
}
# ------------------------------------------------------------------------------
.nodeSUIDToNodeName<-function(node.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('node',c('SUID','name'),'default',network, base.url)
        node.names <- dict[which(dict$SUID  %in% node.suids),'name']
        return(node.names)
}

# ------------------------------------------------------------------------------
.edgeNameToEdgeSUID<-function(edge.names, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        edge.SUIDs <- dict[which(dict$name  %in% edge.names),'SUID']
        return(edge.SUIDs)
}

# ------------------------------------------------------------------------------
.edgeSUIDToEdgeName<-function(edge.suids, network=NULL, base.url=.defaultBaseUrl) {
        dict <- getTableColumns('edge',c('SUID','name'),'default',network, base.url)
        edge.names <- dict[which(dict$SUID  %in% edge.suids),'name']
        return(edge.names)
}

#------------------------------------------------------------------------------------------------------------------------
.initNodeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
    stopifnot (attribute.type %in% c ('char', 'integer', 'numeric', 'boolean'))
    if (attribute.type == 'char')
        attribute.type = 'STRING'
    else if (attribute.type == 'integer')
        attribute.type = 'INTEGER'
    else if (attribute.type == 'numeric')
        attribute.type = 'FLOATING'
    else if (attribute.type == 'boolean')
        attribute.type = 'BOOLEAN'
    
    nodeDataDefaults (graph, attr=attribute.name) = default.value
    attr (nodeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type
    
    return (graph)
    
} 

#------------------------------------------------------------------------------------------------------------------------
.initEdgeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
    stopifnot (attribute.type %in% c ('char', 'integer', 'numeric', 'boolean'))
    if (attribute.type == 'char')
        attribute.type = 'STRING'
    else if (attribute.type == 'integer')
        attribute.type = 'INTEGER'
    else if (attribute.type == 'numeric')
        attribute.type = 'FLOATING'
    else if (attribute.type == 'boolean')
        attribute.type = 'BOOLEAN'
    
    edgeDataDefaults (graph, attr=attribute.name) = default.value
    attr (edgeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type
    
    return (graph)
} 

#------------------------------------------------------------------------------------------------------------------------
.copyNodeAttributesToGraph <- function (node.suid.name.dict, suid, existing.graph, base.url) {
    
    node.attribute.names = getNodeAttributeNames(suid, base.url)
    
    for(attribute.name in node.attribute.names) {
        known.node.names = sapply(node.suid.name.dict, function(n) { n$name })
        # nodes that store values for this attribute (meaning the value is not empty)
        nodes.with.attribute = haveNodeAttribute(suid, known.node.names, attribute.name, base.url)
        if(length(nodes.with.attribute) > 0) {
            attribute.type = getNodeAttributeType(attribute.name, suid, base.url)
            write(sprintf("\t retrieving attribute '%s' values for %d nodes", attribute.name, length(nodes.with.attribute)), stderr())
            # write(sprintf("\t retrieving %s '%s' attribute for %d nodes", attribute.type, attribute.name, length(nodes.with.attribute)), stderr())
            if(attribute.type == 'Integer' || attribute.type == 'Long') {
                attribute.type = 'integer'
                default.value = as.integer(0)
            } else if(attribute.type == 'String') {
                attribute.type = 'char'
                default.value = as.character('unassigned')
            } else if(attribute.type == 'Double' || attribute.type == 'Float') {
                attribute.type = 'numeric'
                default.value = as.numeric(0.0)
            } else if(attribute.type == 'Boolean') {
                attribute.type = 'boolean'
                default.value = as.logical(FALSE)
            } else {
                write(sprintf('RCy3::.copyNodeAttributesToGraph, no support yet for attributes of type %s', attribute.type), stderr())
                next()
            }
            existing.graph = 
                .initNodeAttribute(existing.graph, attribute.name, attribute.type, default.value)
            
            attribute.values = c()
            
            for(i in 1:length(nodes.with.attribute)) {
                attribute.values = c(attribute.values, getNodeAttribute(nodes.with.attribute[i], attribute.name, suid, base.url))
            }
            graph::nodeData(existing.graph, nodes.with.attribute, attribute.name) = attribute.values
        } ## END if there are nodes that have values for the attribute
    } ## END for loop : looping through each node attribute
    return(existing.graph)
}

#------------------------------------------------------------------------------------------------------------------------
.copyEdgeAttributesToGraph <- function (suid, existing.graph, base.url) {
    edge.attribute.names = getEdgeAttributeNames(suid, base.url)
    
    cy2.edgenames = as.character(cy2.edge.names(existing.graph)) # < 2 seconds for > 9000 edges
    
    for(attribute.name in edge.attribute.names) {
        edges.with.attribute = haveEdgeAttribute(cy2.edgenames, attribute.name, suid, base.url)
        
        if(length(edges.with.attribute) > 0) {
            attribute.type = getEdgeAttributeType(attribute.name, suid, base.url) 
            
            write(sprintf("\t retrieving attribute '%s' values for %d edges", attribute.name, length(edges.with.attribute)), stderr())
            if(attribute.type == 'Integer' || attribute.type == 'Long') {
                attribute.type = 'integer'
                default.value = as.integer(0)
            } else if(attribute.type == 'String') {
                attribute.type = 'char'
                default.value = as.character('unassigned')
            } else if(attribute.type == 'Double' || attribute.type == 'Float') {
                attribute.type = 'numeric'
                default.value = as.numeric(0.0)
            } else if(attribute.type == 'Boolean') {
                attribute.type = 'boolean'
                default.value = as.logical(FALSE)
            } else {
                write(sprintf('RCy3::.copyEdgeAttributesToGraph, no support yet for attributes of type %s', attribute.type), stderr())
                next()
            }
            existing.graph = 
                .initEdgeAttribute(existing.graph, attribute.name, attribute.type, default.value)
            eda.values = c()
            
            for(i in 1:length(edges.with.attribute)) {
                eda.values = c(eda.values, getEdgeAttribute(edges.with.attribute[i], attribute.name, suid, base.url))
            }
            
            regex = ' *[\\(|\\)] *'
            edges.tokens = strsplit(edges.with.attribute, regex)
            source.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[1]))
            target.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[3]))
            edge.types = unlist(lapply(edges.tokens, function(tokens) tokens[2])) 
            
            edgeData(existing.graph, source.nodes, target.nodes, attribute.name) = eda.values
            
        } ## END if
    } ## END for
    
    return(existing.graph)
} 

# #------------------------------------------------------------------------------------------------------------------------
# .getNovelEdges = function (g.old, g.new)
# {
#     if (length (edges (g.old)) == 0){
#         gOld.edgeCount = 0
#     } else {
#         gOld.edgeCount = length (edgeNames (g.old))
#     }
#     
#     if (length (edges (g.new)) == 0){
#         gNew.edgeCount = 0
#     } else{
#         gNew.edgeCount = length (edgeNames (g.new))
#     }
#     
#     if (gNew.edgeCount == 0){
#         return (NA)
#     }
#     if (gOld.edgeCount == 0){
#         return (cy2.edge.names (g.new))
#     }
#     
#     old.edges = cy2.edge.names (g.old)
#     new.edges = cy2.edge.names (g.new)
#     novel.edges = setdiff (new.edges, old.edges)
#     novel.edges.indices = match (novel.edges, as.character (new.edges))
#     return (new.edges [novel.edges.indices])
#     
#     
# } # .getNovelEdges
# 
# # ------------------------------------------------------------------------------
# is.classic.graph = function(obj)
# {
#     obj.classes = is(obj)
#     
#     return ('graph' %in% obj.classes)
#     
# } # is.classic.graph
# 
# # ------------------------------------------------------------------------------
# is.multiGraph = function(obj)
# {
#     obj.classes = is(obj)
#     
#     return('MultiGraph' %in% obj.classes)
#     
# } # is.multiGraph

