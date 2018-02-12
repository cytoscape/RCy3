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
        nodes.with.attribute = .haveNodeAttribute(suid, known.node.names, attribute.name, base.url)
        if(length(nodes.with.attribute) > 0) {
            attribute.type = .getNodeAttributeType(attribute.name, suid, base.url)
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
        edges.with.attribute = .haveEdgeAttribute(cy2.edgenames, attribute.name, suid, base.url)
        
        if(length(edges.with.attribute) > 0) {
            attribute.type = .getEdgeAttributeType(attribute.name, suid, base.url) 
            
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

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

.haveNodeAttribute <- function(node.names,
                               attribute.name,
                               network = NULL,
                               base.url = .defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    # check the attribute exists
    if (attribute.name %in% getNodeAttributeNames(net.SUID, base.url)) {
        # get the node SUIDs
        node.SUIDs = .nodeNameToNodeSUID(node.names, net.SUID, base.url)
        nodes.that.have.attribute = c()
        
        for (i in 1:length(node.SUIDs)) {
            resource.uri = 
                node.attribute.value = cyrestGET(paste("networks",
                                                       net.SUID,
                                                       "tables/defaultnode/rows",
                                                       as.character(node.SUIDs[i]),
                                                       attribute.name,
                                                       sep = "/"), 
                                                 base.url=base.url)
            if (nchar(node.attribute.value) > 0) {
                nodes.that.have.attribute = c(nodes.that.have.attribute, node.SUIDs[i])
            }
        }
        
        return (as.character(
            .nodeSUIDToNodeName(nodes.that.have.attribute, net.SUID, base.url)
        ))
    } else {
        write(
            sprintf(
                "Error: '%s' is not an existing node attribute name",
                attribute.name
            ),
            stderr()
        )
    }
}

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

.haveEdgeAttribute <- function (edge.names,
                                attribute.name,
                                network = NULL,
                                base.url = .defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    
    if (attribute.name %in% getEdgeAttributeNames(net.SUID, base.url)) {
        edge.SUIDs = .edgeNameToEdgeSUID(edge.names, net.SUID, base.url)
        edges.that.have.attribute = c()
        
        for (i in 1:length(edge.SUIDs)) {
            edge.attribute.value = cyrestGET(paste("networks",
                                                   net.SUID,
                                                   "tables/defaultedge/rows",
                                                   as.character(edge.SUIDs[i]),
                                                   attribute.name,
                                                   sep = "/"), 
                                             base.url = base.url)
            
            if (nchar(edge.attribute.value) > 0) {
                edges.that.have.attribute = c(edges.that.have.attribute, edge.SUIDs[i])
            }
        }
        
        return(as.character(
            .edgeSUIDToEdgeName(edges.that.have.attribute, net.SUID, base.url)
        ))
    } else {
        write(
            sprintf(
                "Error: '%s' is no an existing edge attribute name",
                attribute.name
            ),
            stderr()
        )
    }
}

# ------------------------------------------------------------------------------
.getNodeAttributeType <- function(attribute.name,
                                 network = NULL,
                                 base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    
    if (attribute.name %in% getNodeAttributeNames(net.SUID, base.url)) {
        node.attributes.info <- cyrestGET(paste("networks",
                                                net.SUID,
                                                "tables/defaultnode/columns",
                                                sep = "/"),
                                          base.url = base.url)
        return(node.attributes.info[[which(lapply(node.attributes.info, function(a) {
            a$name
        }) %in% attribute.name)]]$type)
    } else {
        write(
            sprintf(
                "WARNING in RCy3::getNodeAttributeType():\n\t '%s' could not be recognized as a valid node attribute >> function returns empty value",
                attribute.name
            ),
            stderr()
        )
        
        return("")
    }
}

# ------------------------------------------------------------------------------
.getEdgeAttributeType <- function(attribute.name, network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    if (attribute.name %in% getEdgeAttributeNames(net.SUID, base.url)) {
        edge.attributes.info <- cyrestGET(paste("networks",
                                                net.SUID,
                                                "tables/defaultedge/columns",
                                                sep = "/"),
                                          base.url=base.url)
        return(edge.attributes.info[[which(lapply(edge.attributes.info, function(a) {
            a$name
        }) %in% attribute.name)]]$type)
    } else {
        write(
            sprintf(
                "WARNING in RCy3::getEdgeAttributeType():\n\t '%s' could not be recognized as a valid edge attribute >> function returns empty value",
                attribute.name
            ),
            stderr()
        )
        
        return("")
    }
}
## END getEdgeAttributeType

# ------------------------------------------------------------------------------
# TODO
.setNodeAttributesDirect <- function(
    attribute.name,
    attribute.type,
    node.names,
    values, network = NULL, base.url = .defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    
    caller.specified.attribute.class = tolower(attribute.type)
    # the switch-block ensures the attribute values have the correct data type
    switch(
        caller.specified.attribute.class,
        "floating" = ,
        "numeric" = ,
        "double" = {
            caller.specified.attribute.class = 'Double'
            values = as.numeric(values)
        },
        "integer" = ,
        "int" = {
            caller.specified.attribute.class = "Integer"
            values = as.integer(values)
        },
        "boolean" = {
            caller.specified.attribute.class = "Boolean"
            values = as.logical(values)
        },
        {
            caller.specified.attribute.class = "String"
            values = as.character(values)
        }
    )
    
    # CREATES NEW COLUMN (IF NEEDED)
    if (!attribute.name %in% getNodeAttributeNames(net.SUID,base.url)) {
        # create new table column in Cytoscape 'Node Table' to store the attribute values
        tbl.col = list(name = attribute.name, type = caller.specified.attribute.class)
        cyrestPOST(paste("networks",
                         net.SUID,
                         "tables/defaultnode/columns",
                         sep = "/"),
                   body = tbl.col,
                   base.url = base.url)
    }
    
    if (length(node.names) > 0) {
        if (length(node.names) != length(values)) {
            write(
                sprintf(
                    "ERROR in RCy3::setNodeAttributesDirect():\n\t the number of values(%d) for attribute '%s' must equal the number of nodes(%d) >> function aborted",
                    length(values),
                    attribute.name,
                    length(node.names)
                ),
                stderr()
            )
        } else {
            node.SUIDs = .nodeNameToNodeSUID(node.names, net.SUID, base.url)
            node.name.suid.value.df = data.frame(node.names, node.SUIDs, values)
            
            # converts the above data frame data in the cyREST [SUID:value]-pairs format
            node.SUID.value.pairs =
                apply(node.name.suid.value.df[, c('node.SUIDs', 'values')], 1, function(x) {
                    list(SUID = unname(x[1]), value = unname(x[2]))
                })
            res = cyrestPUT(paste("networks",
                                  net.SUID,
                                  "tables/defaultnode/columns",
                                  attribute.name,
                                  sep = "/"),
                            body = node.SUID.value.pairs,
                            base.url = base.url)
            invisible(res)
        }
    }
}
## END setNodeAttributesDirect

# ------------------------------------------------------------------------------
.setEdgeAttributesDirect <-  function(
    attribute.name,
    attribute.type,
    edge.names,
    values, network = NULL, base.url = .defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    
    if (length(edge.names) > 0) {
        if (length(edge.names) != length(values)) {
            write(
                sprintf(
                    "ERROR in RCy3::setEdgeAttributesDirect():\n\t the number of values(%d) for attribute '%s' must equal the number of edges(%d) >> function aborted",
                    length(values),
                    attribute.name,
                    length(edge.names)
                ),
                stderr()
            )
            
        } else {
            caller.specified.attribute.class = tolower(attribute.type)
            # the switch-block ensures the attribute values have the correct data type
            switch(
                caller.specified.attribute.class,
                "floating" = ,
                "numeric" = ,
                "double" = {
                    caller.specified.attribute.class = 'Double'
                    values = as.numeric(values)
                },
                "integer" = ,
                "int" = {
                    caller.specified.attribute.class = "Integer"
                    values = as.integer(values)
                },
                "boolean" = {
                    caller.specified.attribute.class = "Boolean"
                    values = as.logical(values)
                },
                {
                    caller.specified.attribute.class = "String"
                    values = as.character(values)
                }
            )
            
            if (!attribute.name %in% getEdgeAttributeNames(net.SUID, base.url)) {
                tbl.col = list(name = attribute.name, type = caller.specified.attribute.class)
                cyrestPOST( paste("networks",
                                  net.SUID,
                                  "tables/defaultedge/columns",
                                  sep = "/"),
                            body = tbl.col,
                            base.url = base.url)
            }
            
            edge.SUIDs = .edgeNameToEdgeSUID(edge.names, net.SUID ,base.url)
            edge.name.suid.value.df = data.frame(edge.names, edge.SUIDs, values)
            
            edge.SUID.value.pairs =
                apply(edge.name.suid.value.df[, c('edge.SUIDs', 'values')], 1, function(x) {
                    list(SUID = unname(x[1]), value = unname(x[2]))
                })
            res = cyrestPUT( paste( "networks",
                                    net.SUID,
                                    "tables/defaultedge/columns",
                                    attribute.name,
                                    sep = "/"),
                             body = edge.SUID.value.pairs,
                             base.url = base.url)
            invisible(res)
        }
    }
}
## END setEdgeAttributesDirect

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

