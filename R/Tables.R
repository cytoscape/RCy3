# ------ TODO -----------------------------------------------------------------
# mapIdentifiers(title = NA,table = NA,column.name,species,mapFrom,mapTo,force.single = TRUE)
# http://localhost:1234/v1/commands/idmapper/map%20column
#
# ------------------------------------------------------------------------------
#' Get table column values
#'
#' @details The 'name' column is always retrieved along with specified columns. The 'name' values are used
#' as row names in the returned data frame. Note that this function fails on columns with missing values.
#' @param table name of table, e.g., node, edge, network
#' @param columns names of columns to retrieve values from as list object or comma-separated list; default is all columns
#' @param namespace namespace of table; default is "default"
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return data frame of column values
#' @examples
#' \donttest{
#' example(createNetworkFromDataFrames)
#'
#' getTableColumns('node','group')
#' }
#' @export

getTableColumns <- function(table,
                             columns = NULL,
                             namespace = 'default',
                             network = NULL,
                             base.url = .defaultBaseUrl) {
    #TODO handle column types like getNodeAttributes?
    suid = getNetworkSuid(network)
    
    #all columns
    if (is.null(columns))
        columns = listTableColumns(table, namespace, network, base.url)
    
    #handle comma separated lists and list objects
    col.list = columns
    if (length(col.list) == 1)
        col.list = unlist(strsplit(columns, ","))
    
    #get name column first
    tbl = paste0(namespace, table)
    res.names <- cyrestGET(paste('networks',suid,'tables',tbl,'columns','name',sep="/"),
                           base.url = base.url)
    df = data.frame(row.names = res.names$values)
    
    #retrieve column names
    table.col.list = listTableColumns(table, namespace, network, base.url)
    
    # then append other requested columns
    for (col in col.list) {
        #check for column names
        if (!col %in% table.col.list) {
            cat(sprintf("Error: Column %s not found in %s table \n", col, table))
            next()
        }
        res.col <- cyrestGET(paste('networks',suid,'tables',tbl,'columns',col, sep="/"),
                             base.url = base.url)
        #convert NULL to NA, then unlist
        cvv <- unlist(lapply(res.col$values, function(x)
            ifelse(is.null(x), NA, x)))
        if (length(res.names$values) == length(cvv)) {
            for (i in 1:length(res.names$values)) {
                df[i, col] <- cvv[i]
            }
        } else {
            print(
                "Warning: Requested column has missing values. Returning single column without row.names..."
            )
            df2 = data.frame(col = unlist(res.col$values))
            return(df2)
        }
    }
    return(df)
}

# ------------------------------------------------------------------------------
#' List names of all columns in a table
#'
#' @param table name of table, e.g., node, edge, network; default is "node"
#' @param namespace namespace of table, e.g., default
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return list of column names
#' @examples
#' \donttest{
#' listTableColumns()
#' listTableColumns('edge')
#' listTableColumns('network')
#' }
#' @export

listTableColumns <-  function(table = 'node',
                              namespace = 'default',
                              network = NULL,
                              base.url = .defaultBaseUrl) {
    title = getNetworkName(network)
    cmd = paste(table,
                ' list attributes network="',
                title,
                '" namespace="',
                namespace,
                sep = '')
    return(commandsPOST(cmd, base.url = base.url))
}

# ------------------------------------------------------------------------------
#' Loads data into Cytoscape tables keyed by row
#'
#' @description This function loads data into Cytoscape node/edge/network tables provided a
#' common key, e.g., name. Data.frame column names will be used to set Cytoscape table column
#' names.
#' @details Numeric (or integer) values will be stored as Doubles in Cytoscape tables.
#' Character or mixed values will be stored as Strings. Logical values will be stored as Boolean.
#' Existing columns with the same names will be overwritten.
#' @param data (data.frame) each row is a node and columns contain node attributes
#' @param data.key.column (char) name of data.frame column to use as key; default is "row.names"
#' @param table (char) name of Cytoscape table to load data into, e.g., node, edge or network; default is "node"
#' @param table.key.column (char) name of Cytoscape table column to use as key; default is "name"
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @importFrom BiocGenerics colnames
#' @export

loadTableData <- function(data,
                          data.key.column = 'row.names',
                          table = 'node',
                          table.key.column = 'name',
                          network = NULL,
                          base.url = .defaultBaseUrl) {
    
    net.suid <- getNetworkSuid(network)
    table.key.column.values =  getTableColumns(table = table,
                                               columns = table.key.column,
                                               network = net.suid,
                                               base.url = base.url)
    
    ##ERROR if table.key.column.values is empty
    if (ncol(table.key.column.values) == 0)
        return("Failed to load data: Please check table.key.column")
    
    if (data.key.column == 'row.names')
        # if key in row.names...
        data$row.names <-
        row.names(data)  # then copy to new "row.names" column :)
    
    ##ERROR if data.key.column not in data
    if (!data.key.column %in% colnames(data))
        return("Failed to load data: Please check data.key.column")
    
    filter = data[, data.key.column] %in% table.key.column.values[, 1]
    
    ##ERROR if filter is empty
    if (!TRUE %in% filter)
        return("Failed to load data: Provided key columns do not contain any matches")
    
    data.subset = data[filter, ]
    
    #remove troublesome factors
    if (class(data.subset[, data.key.column]) == "factor")
        data.subset[, data.key.column] = levels(droplevels(data.subset[, data.key.column]))
    
    data.list <- c()
    for (i in 1:dim(data.subset)[1]) {
        #each rows
        rest <- list()
        for (j in 1:dim(data.subset)[2]) {
            #each column
            rest[[colnames(data.subset)[j]]] = data.subset[i, j]
        }
        data.list[[i]] <- rest
    }
    
    table = paste0("default", table) #add prefix
    
    tojson.list <-
        list(key = table.key.column,
             dataKey = data.key.column,
             data = data.list)
    
    cyrestPUT(paste('networks', net.suid, "tables", table, sep = '/'),
              body = tojson.list,
              base.url = base.url)
    return(sprintf("Success: Data loaded in %s table", table))
}

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

haveNodeAttribute <- function(node.names,
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

haveEdgeAttribute <- function (edge.names,
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
setNodeAttributesDirect <- function(
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
setEdgeAttributesDirect <-  function(
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

#------------------------------------------------------------------------------------------------------------------------
# retrieve the names of the recognized and supported names for the class of any node or edge attribute.
getAttributeClassNames <- function () {
    return (c (
        'floating|numeric|double',
        'integer|int',
        'string|char|character'
    ))
}

# ------------------------------------------------------------------------------
getNodeAttribute <- function(node.name, attribute.name, network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    node.SUID <-
        as.character(.nodeNameToNodeSUID(node.name, net.SUID, base.url))
    
    if (length(node.SUID) < 1) {
        write(
            sprintf(
                "WARNING in RCy3::getNodeAttribute():\n\t no node with name '%s' could be found >> function returns empty value",
                node.name
            ),
            stderr()
        )
        
        return("")
    } else {
        node.attribute.type <- getNodeAttributeType(attribute.name,net.SUID,base.url)
        
        if (length(node.attribute.type) > 0) {
            res = commandsPOST(
                paste0(
                    'node get attribute nodeList=',
                    node.name,
                    ' columnList=',
                    attribute.name
                )
            )
            res <- gsub("}", "", res)
            node.attribute.value <-
                unlist(strsplit(res, ":"))[4]

            switch(
                node.attribute.type,
                "Double" = {
                    if (is.na(node.attribute.value))
                        node.attribute.value = 0.0
                    return(as.numeric(node.attribute.value))
                },
                "Long" = ,
                "Integer" = {
                    if (is.na(node.attribute.value))
                        node.attribute.value = 0
                    return(as.integer(node.attribute.value))
                },
                "Boolean" = {
                    if (is.na(node.attribute.value))
                        node.attribute.value = FALSE
                    return(as.logical(node.attribute.value))
                },
                {
                    if (is.na(node.attribute.value))
                        node.attribute.value = ''
                    return(as.character(node.attribute.value))
                }
            )
        }
        return("")
    }
}

# ------------------------------------------------------------------------------
getNodeAttributeType <- function(attribute.name,
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
getAllNodeAttributes <- function(onlySelectedNodes = FALSE, 
                                 network=NULL, 
                                 base.url=.defaultBaseUrl) {
    #TODO implement onlySelectedNodes
    getTableColumns('node', network = network, base.url = base.url)
}

# ------------------------------------------------------------------------------
getEdgeAttribute <- function(edge.name, 
                             attribute.name, 
                             network=NULL, 
                             base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    edge.SUID <-
        as.character(.edgeNameToEdgeSUID(edge.name, net.SUID, base.url))
    
    if (length(edge.SUID) < 1) {
        write(
            sprintf(
                "WARNING in RCy3::getEdgeAttribute():\n\t no edge with name '%s' could be found >> function returns empty value",
                edge.name
            ),
            stderr()
        )
        
        return("")
    } else {
        edge.attribute.type <- getEdgeAttributeType(attribute.name, net.SUID, base.url)
        
        if (length(edge.attribute.type) > 0) {
            edge.attribute.value <- cyrestGET(paste("networks",
                                                    net.SUID,
                                                    "tables/defaultedge/rows",
                                                    edge.SUID,
                                                    attribute.name,
                                                    sep = "/"),
                                              base.url=base.url)
            switch(
                edge.attribute.type,
                "Double" = {
                    return(as.numeric(edge.attribute.value))
                },
                "Long" = ,
                "Integer" = {
                    return(as.integer(edge.attribute.value))
                },
                "Boolean" = {
                    return(as.logical(edge.attribute.value))
                },
                {
                    return(as.character(edge.attribute.value))
                }
            )
        }
        return("")
    }
}
## END getEdgeAttribute

# ------------------------------------------------------------------------------
getEdgeAttributeType <- function(attribute.name, network=NULL, base.url=.defaultBaseUrl) {
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

#------------------------------------------------------------------------------------------------------------------------
getAllEdgeAttributes <- function (onlySelectedEdges = FALSE,
                                  network = NULL, 
                                  base.url = .defaultBaseUrl) {
    getTableColumns('edge', network=network, base.url = base.url)
}
# ------------------------------------------------------------------------------
getNodeAttributeNames <-  function(network = NULL, 
                                   base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    request.res <- cyrestGET(paste("networks",
                                   net.SUID,
                                   "tables/defaultnode/columns",
                                   sep = "/"),
                             base.url = base.url)
    request.res <-
        data.frame(t(sapply(request.res, base::c)))
    request.res <- unlist(request.res$name)
    # exclude some node attributes
    node.attribute.names <-
        request.res[!request.res %in% c("SUID", "shared name", "selected")]
    if (length(node.attribute.names) <= 2) {
        write(
            sprintf(
                'Please ensure that you sent the R graph to Cytoscape before calling this function, e.g. using displayGraph. Otherwise names might not be displayed (correctly).'
            ),
            stderr()
        )
    }
    return (node.attribute.names)
}

# ------------------------------------------------------------------------------
getEdgeAttributeNames <- function(network = NULL, 
                                  base.url = .defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    request.res <- cyrestGET(paste("networks",
                                   net.SUID,
                                   "tables/defaultedge/columns",
                                   sep = "/"),
                             base.url = base.url)
    request.res <-
        data.frame(t(sapply(request.res, base::c)))
    request.res <- unlist(request.res$name)
    # exclude some edge attributes
    edge.attribute.names <-
        request.res[!request.res %in% c("SUID", "shared name", "shared interaction", "selected")]
    return(edge.attribute.names)
}

# ------------------------------------------------------------------------------
# delete node attribute by deleting its column in the node table
deleteNodeAttribute <- function(attribute.name,
                                network = NULL, 
                                base.url = .defaultBaseUrl) {
    net.suid <- getNetworkSuid(network)
    if (attribute.name %in% getNodeAttributeNames(net.suid, base.url)) {
        cyrestDELETE(paste("networks",
                           net.suid,
                           "tables/defaultnode/columns",
                           as.character(attribute.name),
                           sep = "/"),
                     base.url = base.url)
        write(sprintf('Attribute "%s" has been deleted...', attribute.name),
              stderr())
    } else{
        msg = paste (attribute.name,
                     'does not exist and thus could not be deleted.')
        write (msg, stderr ())
    }
}

# ------------------------------------------------------------------------------
# delete edge attribute by deleting its column in the edge table
deleteEdgeAttribute <- function(attribute.name,network = NULL, base.url = .defaultBaseUrl) {
    net.suid <- getNetworkSuid(network)
    if (attribute.name %in% getEdgeAttributeNames(net.suid, base.url)) {
        cyrestDELETE(paste("networks",
                           net.suid,
                           "tables/defaultedge/columns",
                           as.character(attribute.name),
                           sep = "/"),
                     base.url = base.url)
        write(sprintf('Attribute "%s" has been deleted...', attribute.name),
              stderr())
    } else{
        msg = paste (attribute.name,
                     'does not exist and thus could not be deleted.')
        write (msg, stderr ())
    }
}


