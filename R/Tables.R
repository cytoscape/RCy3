#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R 
NULL

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/v1/commands/idmapper/map%20column 
setGeneric ('mapIdentifiers', function (obj, title=NA, table=NA, column.name, species, mapFrom, mapTo, force.single=TRUE) standardGeneric('mapIdentifiers'))

# ------------------------------------------------------------------------------
#setGeneric ('haveNodeAttribute',             signature='obj', function (obj=CytoscapeConnection(), node.names, attribute.name) standardGeneric ('haveNodeAttribute'))
#setGeneric ('haveEdgeAttribute',             signature='obj', function (obj=CytoscapeConnection(), edge.names, attribute.name) standardGeneric ('haveEdgeAttribute'))
setGeneric ('setNodeAttributesDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name, attribute.type, node.names, values) standardGeneric ('setNodeAttributesDirect'))
setGeneric ('setEdgeAttributesDirect', 	 signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name, attribute.type, edge.names, values) standardGeneric ('setEdgeAttributesDirect'))
setGeneric ('getAttributeClassNames', 	 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getAttributeClassNames'))
#setGeneric ('getNodeAttributeType',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('getNodeAttributeType'))
setGeneric ('getEdgeAttributeType',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('getEdgeAttributeType'))
setGeneric ('getNodeAttributeNames',     signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getNodeAttributeNames'))
#setGeneric ('getEdgeAttributeNames',     signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getEdgeAttributeNames'))
setGeneric ('deleteNodeAttribute',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('deleteNodeAttribute'))
setGeneric ('deleteEdgeAttribute',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('deleteEdgeAttribute'))
setGeneric ('getAllNodeAttributes',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), onlySelectedNodes=FALSE) standardGeneric ('getAllNodeAttributes'))
setGeneric ('getAllEdgeAttributes',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), onlySelectedEdges=FALSE) standardGeneric ('getAllEdgeAttributes'))
setGeneric ('getNodeAttribute',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.name, attribute.name) standardGeneric ('getNodeAttribute'))
setGeneric ('getEdgeAttribute',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.name, attribute.name) standardGeneric ('getEdgeAttribute'))


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
#' @export
#' @import httr
#' @import RJSONIO
#' @importFrom utils URLencode
#' @examples
#' \donttest{
#' example(createNetworkFromDataFrames)
#'
#' getTableColumns('node','group')
#' }

getTableColumns<-function(table,columns=NULL,namespace='default',network=NULL,base.url=.defaultBaseUrl){
    
    suid=getNetworkSuid(network)
    
    #all columns
    if(is.null(columns))
        columns = listTableColumns(table, namespace, network, base.url)
    
    #handle comma separated lists and list objects
    col.list = columns
    if(length(col.list)==1)
        col.list = unlist(strsplit(columns, ","))
    
    #get name column first
    tbl = paste0(namespace,table)
    cmd = paste(base.url,'networks',suid,'tables',tbl,'columns','name',sep = '/')
    res = GET(URLencode(cmd))
    res.html = htmlParse(rawToChar(res$content), asText=TRUE)
    res.elem = xpathSApply(res.html, "//p", xmlValue)
    names <- fromJSON(res.elem[1])
    df = data.frame(row.names=names$values)
    
    #retrieve column names
    table.col.list = listTableColumns(table,namespace,network,base.url)
    
    # then append other requested columns
    for (col in col.list){
        
        #check for column names
        if(!col %in% table.col.list){
            cat(sprintf("Error: Column %s not found in %s table \n",col,table))
            next()
        }
        
        cmd = paste(base.url,'networks',suid,'tables',tbl,'columns',col,sep = '/')
        res = GET(URLencode(cmd))
        res.html = htmlParse(rawToChar(res$content), asText=TRUE)
        res.elem = xpathSApply(res.html, "//p", xmlValue)
        col.vals <- fromJSON(res.elem[1])
        #convert NULL to NA, then unlist
        cvv <- unlist(lapply(col.vals$values, function(x) ifelse(is.null(x),NA,x)))
        if(length(names$values)==length(cvv)){
            for(i in 1:length(names$values)){
                df[i,col] <- cvv[i]
            }
        } else {
            print("Warning: Requested column has missing values. Returning single column without row.names...")
            df2 = data.frame(col=unlist(col.vals$values))
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
#' @export
#' @import httr
#' @import RJSONIO
#' @examples
#' \donttest{
#' listTableColumns()
#' listTableColumns('edge')
#' listTableColumns('network')
#' }

listTableColumns<-function(table='node',namespace='default',network=NULL,base.url=.defaultBaseUrl){
    title=getNetworkName(network)
    cmd = paste(table,' list attributes network="',title,'" namespace="',namespace,sep='')
    return(commandsPOST(cmd,base.url=base.url))
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
#' @export

loadTableData<-function(data, data.key.column='row.names', table='node', table.key.column='name',
                        obj=CytoscapeWindowFromNetwork()){
    
    base.url = paste(obj@uri,obj@api,sep="/")
    
    table.key.column.values =  getTableColumns(table=table,columns = table.key.column, obj=obj)
    
    ##ERROR if table.key.column.values is empty
    if(ncol(table.key.column.values)==0)
        return("Failed to load data: Please check table.key.column")
    
    if(data.key.column=='row.names')  # if key in row.names...
        data$row.names<-row.names(data)  # then copy to new "row.names" column :)
    
    ##ERROR if data.key.column not in data
    if (!data.key.column %in% colnames(data))
        return("Failed to load data: Please check data.key.column")
    
    filter = data[,data.key.column] %in% table.key.column.values[,1]
    
    ##ERROR if filter is empty
    if(!TRUE %in% filter)
        return("Failed to load data: Provided key columns do not contain any matches")
    
    data.subset = data[filter,]
    
    #remove troublesome factors
    if(class(data.subset[,data.key.column])=="factor")
        data.subset[,data.key.column] = levels(droplevels(data.subset[,data.key.column]))
    
    data.list <- c()
    for(i in 1:dim(data.subset)[1]){  #each rows
        rest <- list()
        for(j in 1:dim(data.subset)[2]){  #each column
            rest[[colnames(data.subset)[j]]] = data.subset[i,j]
        }
        data.list[[i]] <- rest
    }
    
    table = paste0("default",table) #add prefix
    
    tojson.list <- list(key=table.key.column,dataKey=data.key.column,data=data.list)
    jsonData <- toJSON(tojson.list)
    data.url <- paste(base.url,'networks',obj@suid,"tables",table, sep = '/')
    
    PUT(url=data.url,body=jsonData, encode="json")
    return(sprintf("Success: Data loaded in %s table", table))
}

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

haveNodeAttribute <- function(node.names, attribute.name, network=NULL, base.url=.defaultBaseUrl) {
    
    net.SUID = getNetworkSuid(network)
    # check the attribute exists
    if (attribute.name %in% getNodeAttributeNames(net.SUID,base.url)) {
        # get the node SUIDs
        node.SUIDs = .nodeNameToNodeSUID(node.names, net.SUID, base.url)
        nodes.that.have.attribute = c()
        
        for (i in 1:length(node.SUIDs)) {
            resource.uri = paste(base.url, "networks", net.SUID, "tables/defaultnode/rows", as.character(node.SUIDs[i]), attribute.name, sep="/")
            request.res = GET(url=resource.uri)
            node.attribute.value = rawToChar(request.res$content)
            
            if(nchar(node.attribute.value) > 0) {
                nodes.that.have.attribute = c(nodes.that.have.attribute, node.SUIDs[i])
            }
        }
        
        return (as.character(.nodeSUIDToNodeName(nodes.that.have.attribute,net.SUID,base.url)))
    } else {
        write(sprintf("Error: '%s' is not an existing node attribute name", attribute.name), stderr())
    }
}

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

haveEdgeAttribute <- function (edge.names, attribute.name, network=NULL, base.url=.defaultBaseUrl) {
               net.SUID = getNetworkSuid(network)
               
               if(attribute.name %in% getEdgeAttributeNames(net.SUID, base.url)) {
                   edge.SUIDs = .edgeNameToEdgeSUID(edge.names,net.SUID,base.url)
                   edges.that.have.attribute = c()
                   
                   for(i in 1:length(edge.SUIDs)) {
                       resource.uri = paste(base.url, "networks", net.SUID, "tables/defaultedge/rows", as.character(edge.SUIDs[i]), attribute.name, sep="/")
                       request.res = GET(url=resource.uri)
                       edge.attribute.value = rawToChar(request.res$content)
                       
                       if(nchar(edge.attribute.value) > 0) {
                           edges.that.have.attribute = c(edges.that.have.attribute, edge.SUIDs[i])
                       }
                   }
                   
                   return(as.character(.edgeSUIDToEdgeName(edges.that.have.attribute, net.SUID, base.url)))
               } else {
                   write(sprintf("Error: '%s' is no an existing edge attribute name", attribute.name), stderr())
               }
           }

# ------------------------------------------------------------------------------
setMethod('setNodeAttributesDirect', 'OptionalCyWinClass', 
          function(obj, attribute.name, attribute.type, node.names, values) {
              net.SUID = as.character(obj@suid)
              
              caller.specified.attribute.class = tolower(attribute.type)
              # the switch-block ensures the attribute values have the correct data type
              switch(caller.specified.attribute.class,
                     "floating"=,
                     "numeric"=,
                     "double"={
                         caller.specified.attribute.class = 'Double'
                         values = as.numeric(values)
                     },
                     "integer"=,
                     "int"={
                         caller.specified.attribute.class = "Integer"
                         values = as.integer(values)
                     },
                     "boolean"={
                         caller.specified.attribute.class = "Boolean"
                         values = as.logical(values)
                     },{
                         caller.specified.attribute.class = "String"
                         values = as.character(values)
                     }
              )
              
              # CREATES NEW COLUMN (IF NEEDED)
              if(!attribute.name %in% getNodeAttributeNames(obj)) {
                  # create new table column in Cytoscape 'Node Table' to store the attribute values
                  tbl.col = list(name=attribute.name, type=caller.specified.attribute.class)
                  tbl.col.JSON = toJSON(tbl.col)
                  resource.uri = 
                      paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
                  request.res = POST(url=resource.uri, body=tbl.col.JSON, encode="json")
              }
              
              if(length(node.names) > 0) {
                  if(length(node.names) != length(values)) {
                      write(sprintf("ERROR in RCy3::setNodeAttributesDirect():\n\t the number of values(%d) for attribute '%s' must equal the number of nodes(%d) >> function aborted", 
                                    length(values), attribute.name, length(node.names)), stderr())
                  } else {
                      node.SUIDs = .nodeNameToNodeSUID(obj, node.names)
                      node.name.suid.value.df = data.frame(node.names, node.SUIDs, values)
                      
                      # converts the above data frame data in the cyREST [SUID:value]-pairs format
                      node.SUID.value.pairs = 
                          apply(node.name.suid.value.df[,c('node.SUIDs','values')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
                      node.SUID.value.pairs.JSON = toJSON(node.SUID.value.pairs)
                      
                      resource.uri = 
                          paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns", attribute.name, sep="/")
                      request.res = PUT(url=resource.uri, body=node.SUID.value.pairs.JSON, encode="json")
                      invisible(request.res)
                  }
              }
          })
## END setNodeAttributesDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeAttributesDirect', 'OptionalCyWinClass', 
          function(obj, attribute.name, attribute.type, edge.names, values) {
              net.SUID = as.character(obj@suid)
              
              if(length(edge.names) > 0) {
                  if(length(edge.names) != length(values)) {
                      write(sprintf("ERROR in RCy3::setEdgeAttributesDirect():\n\t the number of values(%d) for attribute '%s' must equal the number of edges(%d) >> function aborted", length(values), attribute.name, length(edge.names)), stderr())
                      
                  } else {
                      caller.specified.attribute.class = tolower(attribute.type)
                      # the switch-block ensures the attribute values have the correct data type
                      switch(caller.specified.attribute.class,
                             "floating"=,
                             "numeric"=,
                             "double"={
                                 caller.specified.attribute.class = 'Double'
                                 values = as.numeric(values)
                             },
                             "integer"=,
                             "int"={
                                 caller.specified.attribute.class = "Integer"
                                 values = as.integer(values)
                             },
                             "boolean"={
                                 caller.specified.attribute.class = "Boolean"
                                 values = as.logical(values)
                             },{
                                 caller.specified.attribute.class = "String"
                                 values = as.character(values)
                             }
                      )
                      
                      if(!attribute.name %in% getEdgeAttributeNames(obj)) {
                          tbl.col = list(name=attribute.name, type=caller.specified.attribute.class)
                          tbl.col.JSON = toJSON(tbl.col)
                          resource.uri = 
                              paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
                          request.res = POST(url=resource.uri, body=tbl.col.JSON, encode="json")
                      }
                      
                      edge.SUIDs = .edgeNameToEdgeSUID(obj, edge.names)
                      edge.name.suid.value.df = data.frame(edge.names, edge.SUIDs, values)
                      
                      edge.SUID.value.pairs = 
                          apply(edge.name.suid.value.df[,c('edge.SUIDs','values')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
                      edge.SUID.value.pairs.JSON = toJSON(edge.SUID.value.pairs)
                      resource.uri = 
                          paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", attribute.name, sep="/")
                      request.res = PUT(url=resource.uri, body=edge.SUID.value.pairs.JSON, encode="json")
                      invisible(request.res)        
                  }
              }
          }) 
## END setEdgeAttributesDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAttributeClassNames', 'OptionalCyObjClass',
           # retrieve the names of the recognized and supported names for the class of any node or edge attribute.
           function (obj) {
               return (c ('floating|numeric|double', 'integer|int', 'string|char|character'))
           })

# ------------------------------------------------------------------------------
setMethod('getNodeAttribute', 'OptionalCyWinClass', 
          function(obj, node.name, attribute.name) {
              net.SUID <- as.character(obj@suid)
              
              
              node.SUID <- as.character(.nodeNameToNodeSUID(obj, node.name))
              
              if(length(node.SUID) < 1) {
                  write(sprintf("WARNING in RCy3::getNodeAttribute():\n\t no node with name '%s' could be found >> function returns empty value", node.name), stderr())
                  
                  return("")
              } else {
                  node.attribute.type <- getNodeAttributeType(obj, attribute.name)
                  
                  if(length(node.attribute.type) > 0) {
                      res=commandsPOST(paste0('node get attribute nodeList=',node.name,' columnList=',attribute.name))
                      res <- gsub("}","",res)
                      node.attribute.value <- unlist(strsplit(res,":"))[4]
                      # resource.uri <- 
                      #     paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/rows", node.SUID, attribute.name, sep="/")
                      # request.res <- GET(url=resource.uri)
                      # node.attribute.value <- unname(rawToChar(request.res$content))
                      
                      switch(node.attribute.type, 
                             "Double"={
                                 if(is.na(node.attribute.value))
                                     node.attribute.value = 0.0
                                 return(as.numeric(node.attribute.value))
                             },
                             "Long"=,
                             "Integer"={
                                 if(is.na(node.attribute.value))
                                     node.attribute.value = 0
                                 return(as.integer(node.attribute.value))
                             },
                             "Boolean"={
                                 if(is.na(node.attribute.value))
                                     node.attribute.value = FALSE
                                 return(as.logical(node.attribute.value))
                             },{
                                 if(is.na(node.attribute.value))
                                     node.attribute.value = ''
                                 return(as.character(node.attribute.value))
                             }
                      )
                  }
                  return("")
              }
          })

# ------------------------------------------------------------------------------
getNodeAttributeType <- function(attribute.name, network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    
    if(attribute.name %in% getNodeAttributeNames(net.SUID, base.url)) {
        resource.uri <- 
            paste(base.url, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
        request.res <- GET(url=resource.uri)
        
        node.attributes.info <- fromJSON(rawToChar(request.res$content))
        return(node.attributes.info[[which(lapply(node.attributes.info, function(a) {a$name}) %in% attribute.name)]]$type)
    } else {
        write(sprintf("WARNING in RCy3::getNodeAttributeType():\n\t '%s' could not be recognized as a valid node attribute >> function returns empty value", attribute.name), stderr())
        
        return("")
    }
}

# ------------------------------------------------------------------------------
setMethod('getAllNodeAttributes', 'OptionalCyWinClass', 
          function(obj, onlySelectedNodes = FALSE) {
              
              if(length(nodes(obj@graph))==0){ #i.e., empty obj@graph 
                  getTableColumns('node',obj=obj)
              }
              
              g = obj@graph
              attribute.names = names(nodeDataDefaults(g))
              nodes.of.interest = nodes(g)
              if(onlySelectedNodes) {
                  if(getSelectedNodeCount(obj) == 0) {
                      return(NA)
                  }
                  nodes.of.interest = getSelectedNodes(obj)
              }
              result = cbind (unlist (nodeData (g, nodes.of.interest, attr=attribute.names [1])))
              if (length (attribute.names) > 1) {
                  for (name in attribute.names [2:length (attribute.names)]) {
                      new.column = unlist (nodeData (g, nodes.of.interest, attr=name))
                      if (is.null (new.column)){
                          new.column = rep ('NULL', nrow (result))
                      }
                      result = cbind (result, new.column)
                  } # for name
              } # if length > 1
              
              colnames (result) = attribute.names
              result = as.data.frame (result, stringsAsFactors=FALSE)
              
              for (name in attribute.names) {
                  attribute.class = attr (nodeDataDefaults (obj@graph, name), 'class')
                  if (attribute.class == 'FLOATING'){
                      result [, name] = as.numeric (result [, name])
                  }else if (attribute.class == 'STRING'){
                      result [, name] = as.character (result [, name])
                  }else if (attribute.class == 'INTEGER'){
                      result [, name] = as.integer (result [, name])
                  }
              } # for name
              
              return (result)
          })

# ------------------------------------------------------------------------------
setMethod('getEdgeAttribute', 'OptionalCyWinClass', 
          function(obj, edge.name, attribute.name) {
              net.SUID <- as.character(obj@suid)
              
              
              edge.SUID <- as.character(.edgeNameToEdgeSUID(obj, edge.name))
              
              if(length(edge.SUID) < 1) {
                  write(sprintf("WARNING in RCy3::getEdgeAttribute():\n\t no edge with name '%s' could be found >> function returns empty value", edge.name), stderr())
                  
                  return("")
              } else {
                  edge.attribute.type <- getEdgeAttributeType(obj, attribute.name)
                  
                  if(length(edge.attribute.type) > 0) {
                      resource.uri <- 
                          paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/rows", edge.SUID, attribute.name, sep="/")
                      request.res <- GET(url=resource.uri)
                      
                      edge.attribute.value <- unname(rawToChar(request.res$content))
                      
                      switch(edge.attribute.type, 
                             "Double"={
                                 return(as.numeric(edge.attribute.value))
                             },
                             "Long"=,
                             "Integer"={
                                 return(as.integer(edge.attribute.value))
                             },
                             "Boolean"={
                                 return(as.logical(edge.attribute.value))
                             },{
                                 return(as.character(edge.attribute.value))
                             }
                      )
                  }
                  return("")
              }
          })
## END getEdgeAttribute

# ------------------------------------------------------------------------------
setMethod('getEdgeAttributeType', 'OptionalCyWinClass', 
          function(obj, attribute.name) {
              net.SUID <- as.character(obj@suid)
              
              
              if(attribute.name %in% getEdgeAttributeNames(obj)) {
                  resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
                  request.res <- GET(url=resource.uri)
                  
                  edge.attributes.info <- fromJSON(rawToChar(request.res$content))
                  return(edge.attributes.info[[which(lapply(edge.attributes.info, function(a) {a$name}) %in% attribute.name)]]$type)
              } else {
                  write(sprintf("WARNING in RCy3::getEdgeAttributeType():\n\t '%s' could not be recognized as a valid edge attribute >> function returns empty value", attribute.name), stderr())
                  
                  return("")
              }
          })
## END getEdgeAttributeType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdgeAttributes', 'OptionalCyWinClass',
           function (obj, onlySelectedEdges=FALSE) {
               
               if(length(edgeL(obj@graph))==0){ #i.e., empty obj@graph 
                   getTableColumns('edge',obj=obj)
               }
               
               g = obj@graph
               attribute.names = names (edgeDataDefaults (g))
               edges.of.interest = edgeNames (g)
               if (onlySelectedEdges) {
                   if (getSelectedEdgeCount (obj) == 0){
                       return (NA)
                   }
                   edges.of.interest = getSelectedEdges (obj)
               } # if onlySelectedEdges
               
               source.and.target.nodes = unlist (strsplit (edges.of.interest, '~'))
               node.count = length (source.and.target.nodes)
               source = source.and.target.nodes [seq (1, node.count, 2)]
               target = source.and.target.nodes [seq (2, node.count, 2)]
               
               #printf ('source nodes: %s', list.to.string (source))
               #printf ('target nodes: %s', list.to.string (target))
               #printf ('attribute names: %s', list.to.string (attribute.names))
               
               result = cbind (unlist (edgeData (g, source, target, attr=attribute.names [1])))
               result = cbind (result, source)
               result = cbind (result, target)
               
               if (length (attribute.names) > 1) {
                   for (name in attribute.names [2:length (attribute.names)]) {
                       new.column = unlist (edgeData (g, source, target, attr=name))
                       result = cbind (result, new.column)
                   } # for name
               } # if > 1
               
               column.names = c (attribute.names [1], 'source', 'target')
               if (length (attribute.names) > 1){
                   column.names = c (column.names, attribute.names [2:length(attribute.names)])
               }
               
               colnames (result) = column.names
               result = as.data.frame (result, stringsAsFactors=FALSE)
               
               # we had a matrix of character strings, now a data.frame of character strings
               # use the embedded type information (created by initEdgeAttribute) to correct to the proper types
               # must be a more direct way to do this in the calls to cbind on a data.frame.
               
               for (name in attribute.names) {
                   attribute.class = attr (edgeDataDefaults (obj@graph, name), 'class')
                   if (attribute.class == 'FLOATING'){
                       result [, name] = as.numeric (result [, name])
                   } else if (attribute.class == 'STRING'){
                       result [, name] = as.character (result [, name])
                   } else if (attribute.class == 'INTEGER'){
                       result [, name] = as.integer (result [, name])
                   }
               } # for name
               
               return (result)
           })

# ------------------------------------------------------------------------------
getNodeAttributeNames <- function(network=NULL, base.url=.defaultBaseUrl) {
              
              net.SUID <- getNetworkSuid(network)
              
              resource.uri <- 
                  paste(base.url, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
              # request result
              request.res <- GET(url=resource.uri)
              request.res <- fromJSON(rawToChar(request.res$content))
              request.res <- data.frame(t(sapply(request.res, base::c)))
              request.res <- unlist(request.res$name)
              # exclude some node attributes
              node.attribute.names <- request.res[! request.res %in% c("SUID", "shared name", "selected")]
              if (length(node.attribute.names) <=2 ){
                  write(sprintf('Please ensure that you sent the R graph to Cytoscape before calling this function, e.g. using displayGraph. Otherwise names might not be displayed (correctly).'), stderr())
              }
              return (node.attribute.names)
          }

# ------------------------------------------------------------------------------
getEdgeAttributeNames <- function(network=NULL, base.url=.defaultBaseUrl) {
              net.SUID <- getNetworkSuid(network)
              resource.uri <- 
                  paste(base.url, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
              # request result
              request.res <- GET(url=resource.uri)
              request.res <- fromJSON(rawToChar(request.res$content))
              request.res <- data.frame(t(sapply(request.res, base::c)))
              request.res <- unlist(request.res$name)
              # exclude some edge attributes
              edge.attribute.names <- request.res[! request.res %in% c("SUID", "shared name", "shared interaction", "selected")]
              return(edge.attribute.names)
          }

# ------------------------------------------------------------------------------
# delete node attribute by deleting its column in the node table
setMethod('deleteNodeAttribute', 'OptionalCyObjClass', 
          function(obj, attribute.name) {
              if (attribute.name %in% getNodeAttributeNames(obj)){
                  resource.uri <- paste(obj@uri, obj@api, "networks", as.character(obj@suid), "tables/defaultnode/columns", as.character(attribute.name), sep="/")
                  result <- DELETE(url= resource.uri)
                  write(sprintf('Attribute "%s" has been deleted...', attribute.name), stderr())
                  invisible(result)
              } else{
                  msg = paste (attribute.name, 'does not exist and thus could not be deleted.')
                  write (msg, stderr ())
              }
          })

# ------------------------------------------------------------------------------
# delete edge attribute by deleting its column in the edge table
setMethod('deleteEdgeAttribute', 'OptionalCyObjClass', 
          function(obj, attribute.name) {
              if (attribute.name %in% getEdgeAttributeNames(obj)){
                  resource.uri <- paste(obj@uri, obj@api, "networks", as.character(obj@suid), "tables/defaultedge/columns", as.character(attribute.name), sep="/")
                  request.res <- DELETE(url= resource.uri)
                  write(sprintf('Attribute "%s" has been deleted...', attribute.name), stderr())
                  invisible(request.res)
              } else{
                  msg = paste (attribute.name, 'does not exist and thus could not be deleted.')
                  write (msg, stderr ())
              }
          })

