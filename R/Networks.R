#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R Internal.R

# ------ TODO -----------------------------------------------------------------
#http://localhost:1234/v1/collections/1547.cx
setGeneric ('getNetworkAsCx', function (obj) standardGeneric('getNetworkAsCx'))

# ------------------------------------------------------------------------------
setGeneric ('createNetworkFromSelection',signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.title, return.graph=FALSE, exclude.edges=FALSE) standardGeneric ('createNetworkFromSelection'))
setGeneric ('saveNetwork',               signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename, type='cys') standardGeneric ('saveNetwork'))
setGeneric ('getNetworkName',            signature='obj', function (network.suid=NA,obj=CytoscapeConnection()) standardGeneric ('getNetworkName'))
setGeneric ('getNetworkSuid',            signature='obj', function (obj=CytoscapeConnection(), title=NA) standardGeneric ('getNetworkSuid'))
setGeneric ('getNetworkCount',	         signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getNetworkCount'))
setGeneric ('getNetworkList',            signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getNetworkList'))
setGeneric ('deleteAllNetworks',	     signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('deleteAllNetworks'))
setGeneric ('cloneNetwork',              signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.title, return.graph = FALSE) standardGeneric('cloneNetwork'))
setGeneric ('deleteNetwork',  	         signature='obj', function (obj=CytoscapeWindowFromNetwork(), title=NA) standardGeneric ('deleteNetwork'))
setGeneric ('renameNetwork',             signature='obj', function (obj=CytoscapeWindowFromNetwork(), old.title=NA, new.title, return.graph = FALSE) standardGeneric('renameNetwork'))
setGeneric ('clearSelection',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('clearSelection'))
setGeneric ('addCyNode',                 signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.name) standardGeneric ('addCyNode'))
setGeneric ('addCyEdge',	             signature='obj', function (obj=CytoscapeWindowFromNetwork(), source.node.name, target.node.name, edgeType, directed) standardGeneric ('addCyEdge'))
setGeneric ('getNodeCount',              signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getNodeCount'))
setGeneric ('getEdgeCount',              signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getEdgeCount'))
setGeneric ('getAllNodes',               signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getAllNodes'))
setGeneric ('getAllEdges',               signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getAllEdges'))
setGeneric ('selectNodes',               signature='obj', function (obj, node.names, by.col='name', preserve.current.selection=TRUE) standardGeneric ('selectNodes'))
setGeneric ('selectEdges',               signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, by.col='name', preserve.current.selection=TRUE) standardGeneric ('selectEdges'))
setGeneric ('selectAllNodes',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric('selectAllNodes'))
setGeneric ('selectAllEdges',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric('selectAllEdges'))
setGeneric ('getSelectedNodes',          signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedNodes'))
setGeneric ('getSelectedEdges',          signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedEdges'))
setGeneric ('getSelectedNodeCount',      signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedNodeCount'))
setGeneric ('getSelectedEdgeCount',      signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedEdgeCount'))
setGeneric ('invertNodeSelection',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('invertNodeSelection'))
setGeneric ('invertEdgeSelection',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('invertEdgeSelection'))
setGeneric ('deleteSelectedNodes',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('deleteSelectedNodes'))
setGeneric ('deleteSelectedEdges',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('deleteSelectedEdges'))
setGeneric ('getFirstNeighbors',         signature='obj', function (obj, node.names, as.nested.list=FALSE) standardGeneric ('getFirstNeighbors'))
setGeneric ('selectFirstNeighbors',      signature='obj', function (obj, direction='any') standardGeneric ('selectFirstNeighbors'))
setGeneric ('selectEdgesConnectedBySelectedNodes', 
            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('selectEdgesConnectedBySelectedNodes'))

#------------------------------------------------------------------------------------------------------------------------
setMethod ('createNetworkFromSelection', 'OptionalCyWinClass',
           function (obj, new.title, return.graph=FALSE, exclude.edges=FALSE) {
               if (getSelectedNodeCount (obj) == 0) {
                   write (noquote ('RCy3::createNetworkFromSelection error:  no nodes are selected'), stderr ())
                   return (NA)
               }
               
               if (new.title %in% as.character (getNetworkList (obj))) {
                   msg <- sprintf ('RCy3::createNetworkFromSelection error:  window "%s" already exists', new.title)
                   write (noquote (msg), stderr ())
                   return (NA)
               }
               
               if(exclude.edges){
                   exclude.edges = "true"
               } else {
                   exclude.edges = "false"
               }
               
               json_sub=NULL
               json_sub$source=obj@title
               json_sub$nodeList="selected"
               json_sub$excludeEdges=exclude.edges
               
               subnetwork.arg = NULL
               if(!missing(new.title)){
                   json_sub$networkName=new.title
               }
               
               sub <- toJSON(as.list(json_sub))
               url<- sprintf("%s/%s/commands/network/create", obj@uri,obj@api,sep="")
               response <- POST(url=url,body=sub, encode="json",content_type_json())
               subnetwork.suid=unname(fromJSON(rawToChar(response$content)))[[1]][[1]]
               cat(sprintf("Subnetwork SUID is : %i \n", subnetwork.suid))
               sub.cw<-CytoscapeWindowFromNetwork(obj,new.title,return.graph=return.graph)
               return(sub.cw)
           }) # createNetworkFromSelection


# ------------------------------------------------------------------------------
#' @title Select first neighbor nodes
#' @description Select nodes directly connected to currently selected nodes. Can
#' specify connection directionality using the direction param.
#' @param direction direction of connections to neighbors to follow, e.g., incoming, outgoing, undirected, or any (default)
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return list of names of selected nodes, including original selection
#' @export
#' @import RJSONIO
#' @import httr
#' @examples
#' \donttest{
#' selectFirstNeighbors()
#' selectFirstNeighors('outgoing')
#' selectFirstNeighors('incoming')
#' }

#' @rdname  selectFirstNeighbors
setMethod( 'selectFirstNeighbors', 'missing',
           function(direction='any'){
               cw<-CytoscapeWindowFromNetwork()
               selectFirstNeighbors(cw,direction)               
           });
#' @rdname  selectFirstNeighbors
setMethod( 'selectFirstNeighbors', 'CytoscapeConnectionClass',
           function(obj, direction='any'){
               cw<-CytoscapeWindowFromNetwork(obj)
               selectFirstNeighbors(cw,direction)
           });
#' @rdname  selectFirstNeighbors
setMethod( 'selectFirstNeighbors', 'CytoscapeWindowClass',
           function(obj, direction='any'){
               network = obj@title
               cmd<-paste0('network select firstNeighbors="',direction,'" network="',network,'"')
               res <- commandRun(cmd,obj)
               return(res[-1])
           });

# ------------------------------------------------------------------------------
#' @title Set current network
#'
#' @description Selects the given network as "current"
#' @param network name or suid of the network that you want set as current
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @examples
#' \donttest{
#' setCurrentNetwork('MyNetwork')
#' }

setCurrentNetwork <- function(obj=CytoscapeConnection(),title=NA){
    
    if(!is.na(title))
        network = title
    else if(class(obj) == 'CytoscapeWindowClass')
        network = obj@title
    else { # a CyConn was provided, but no title, so there is nothing to do 
        write("Neither a network title nor a Cytoscape Window was provided. No action performed.")
        stop()
    }
    
    cmd<-paste0('network set current network="',network,'"')
    res <- commandRun(cmd,obj)
    return(res)
}

#' Clone a Cytoscape Network 
#'
#' @description Makes a copy of a Cytoscape Network with all of its edges and nodes. 
#' @param obj (optional) \code{CytoscapeConnection} or \code{CytoscapeWindow} 
#' @param new.title Name for the new network
#' @param return.graph \code{logical} Whether to include the \code{graphNEL} representation 
#' of the network as part of the returned \code{CytoscapeWindow}. Warning: this is slow.
#' @return A \code{CytoscapeWindow} object associated with the new network 
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' copy_of_your_net <- cloneNetwork(cw, "new_copy")
#' }
#' @author Alexander Pico, Julia Gustavsen
#' @seealso 
#' \code{\link{createNetworkFromSelection}}, 
#' \code{\link{CytoscapeWindowFromNetwork}}, 
#' \code{\link{renameNetwork}}
#' @export
#' 
#' @rdname cloneNetwork
setMethod('cloneNetwork',
          'OptionalCyWinClass', 
          function(obj,
                   new.title,
                   return.graph = FALSE) {
              if (obj@title == new.title){
                  print("Copy not made. The titles of the original window and its copy are the same. Please pick a new name for the copy.")
                  stderr()
              }
              else{
                  selectAllNodes(obj)
                  selectAllEdges(obj)
                  request.uri <- paste(obj@uri,
                                       obj@api,
                                       "networks",
                                       obj@suid,
                                       sep = "/")
                  
                  request.res <- POST(url = request.uri,
                                      query = list(title = new.title))
                  
                  invisible(request.res)
                  
                  if (return.graph){
                      connect_window <- CytoscapeWindowFromNetwork(obj,new.title,
                                                                   return.graph = TRUE)
                      print(paste("Cytoscape window",
                                  obj@title,
                                  "successfully copied to",
                                  connect_window@title,
                                  "and the graph was copied to R."))
                  } 
                  else {
                      connect_window <- CytoscapeWindowFromNetwork(obj,new.title,
                                                                   return.graph = FALSE) 
                      print(paste("Cytoscape window",
                                  obj@title,
                                  "successfully copied to",
                                  connect_window@title,
                                  "and the graph was not copied to the R session."))
                  }
                  
                  return(connect_window)
              }
              
          })

#' Rename a network 
#'
#' Renames a Cytoscape Network. 
#'
#' @param obj (optional) \code{CytoscapeConnection} or \code{CytoscapeWindow} 
#' @param new.title New name for the copy
#' @param return.graph Logical whether to copy the graph to a new object in R 
#' 
#' @return Connection to the renamed network. 
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{createNetworkFromSelection}}, \code{\link{CytoscapeWindowFromNetwork}}, \code{\link{cloneNetwork}}
#'
#' @examples \dontrun{
#' cw <- CytoscapeWindow('network', makeSimpleGraph())
#' renameNetwork(cw, "renamed network")
#' }
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('renameNetwork',
          'OptionalCyWinClass', 
          function(obj, old.title=NA, new.title,
                   return.graph = FALSE) {
              key_value_pairs <- c()
              current <- list( id = paste(obj@suid),name = new.title)
              key_value_pairs <- c(key_value_pairs,list(current))
              selection <- list( key = "SUID", dataKey="id", data = key_value_pairs)
              selection <- toJSON(selection)
              update.name.url <- paste(obj@uri,obj@api,"networks",obj@suid,"tables/defaultnetwork",sep="/")
              
              invisible(PUT(url=update.name.url,
                            body=selection, encode="json"))    
              
              # if object was provided, then update its title as well
              if(!missing(obj)){
                  loc.obj <- obj
                  loc.obj@title<-new.title
                  eval.parent(substitute(obj <- loc.obj))
              }
          })

# ------------------------------------------------------------------------------
setMethod('getNetworkCount', 'OptionalCyObjClass',
          function(obj) {
              resource.uri <- paste(obj@uri, obj@api, "networks/count", sep="/")
              res <- GET(url=resource.uri)
              num.cytoscape.windows <- unname(fromJSON(rawToChar(res$content)))
              return(as.integer(num.cytoscape.windows))
          }) # END getNetworkCount

# ------------------------------------------------------------------------------
#' Get the name of a network
#'
#' @param network.suid SUID of the network; default is current network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return network name
#' @export
#' @examples
#' \donttest{
#' getNetworkName()
#' getNetworkName(1111)
#' }

setMethod('getNetworkName', 'OptionalCyObjClass', 
          function(network.suid=NA, obj=CytoscapeConnection()) {
    base.url = paste(obj@uri,obj@api,sep="/")
    if(is.na(network.suid))
        network.suid = getNetworkSuid(obj)
    
    url <- paste0(base.url,"/networks.names?column=suid&query=",network.suid)
    response <- GET(url=url)
    network.name <- unname(fromJSON(rawToChar(response$content)))
    network.name <- network.name[[1]]$name
    return(network.name)
});

# ------------------------------------------------------------------------------
setMethod('getNetworkSuid', 'OptionalCyObjClass', 
          function(obj, title=NA) {
              
              if(is.na(title)){
                  if(class(obj) == 'CytoscapeWindowClass'){
                      title = obj@title
                  } else { # a CyConn was provided, but no title, so just get current network 
                      cmd<-paste0('network get attribute network=current namespace="default" columnList="SUID"')
                      res <- commandRun(cmd,obj)
                      network.suid <- gsub("\\{SUID:|\\}","",res)
                      return(network.suid)
                  }
              }
              
              # get all window suids and associates names
              resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
              request.res <- GET(resource.uri)
              # SUIDs list of the existing Cytoscape networks	
              cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
              # names list of the existing Cytoscape networks
              cy.networks.names = c()
              
              for(net.SUID in cy.networks.SUIDs)	{
                  res.uri <- paste(obj@uri, obj@api, "networks", as.character(net.SUID), sep="/")
                  result <- GET(res.uri)
                  net.name <- fromJSON(rawToChar(result$content))$data$name
                  cy.networks.names <- c(cy.networks.names, net.name)
              }
              
              if(!title %in% as.character(cy.networks.names)) {
                  write(sprintf("Cytoscape window named '%s' does not exist yet", title), stderr())
                  return (NA)
              } # if unrecognized title
              
              window.entry = which(as.character(cy.networks.names) == title)
              suid = as.character(cy.networks.SUIDs[window.entry])
              
              return(suid)
          })

# ------------------------------------------------------------------------------
setMethod('getNetworkList', 'OptionalCyObjClass', 
          function(obj) {
              if(getNetworkCount(obj) == 0) {
                  return(c())
              }
              resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
              request.res <- GET(resource.uri)
              # SUIDs list of the existing Cytoscape networks	
              cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
              # names list of the existing Cytoscape networks
              cy.networks.names = c()
              
              for(net.SUID in cy.networks.SUIDs)	{
                  res.uri <- paste(obj@uri, obj@api, "networks", as.character(net.SUID), sep="/")
                  result <- GET(res.uri)
                  net.name <- fromJSON(rawToChar(result$content))$data$name
                  cy.networks.names <- c(cy.networks.names, net.name)
              }
              
              return(cy.networks.names)
          })

# ------------------------------------------------------------------------------
setMethod('deleteNetwork', 'OptionalCyObjClass',
          function (obj, title=NA) {
              if(!is.na(title))
                  suid = getNetworkSuid(obj, title)
              else if(class(obj) == 'CytoscapeWindowClass')
                  suid = as.character(obj@suid)
              else { # a CyConn was provided, but no title, so just get current network 
                  suid = getNetworkSuid(obj)
              }
              resource.uri = paste(obj@uri, obj@api, "networks", suid, sep="/")
              request.res = DELETE(url=resource.uri)
              invisible(request.res)
          })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteAllNetworks',	'OptionalCyObjClass', function (obj) {
    # deletes all networks and associated windows in Cytoscape
    resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
    request.res <- DELETE(resource.uri)
    invisible(request.res)
})

# ------------------------------------------------------------------------------
setMethod('addCyNode', 'OptionalCyWinClass', function(obj,node.name) {
    
    if(node.name %in% getAllNodes(obj)) {
        write(sprintf('RCy3::addCyNode, node "%s" already present in Cytoscape graph', node.name), stderr())
        return()
    }
    
    # get the network suid
    net.suid <- as.character(obj@suid)
    resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "nodes", sep="/")
    nodename.json = toJSON(c(node.name))
    
    # add the node to the Cytoscape graph
    new.cynode.res <- POST(url=resource.uri, body=nodename.json, encode="json")
    new.cynode.suid.name <- unname(fromJSON(rawToChar(new.cynode.res$content)))
    
    
    # if obj was provided, then modify
    if(!missing(obj)){
        loc.obj <- obj
        # add the node to the R graph object
        loc.obj@graph <- addNode(node.name, loc.obj@graph)
        # add the new node to the cw@node.suid.name.dict
        loc.obj@node.suid.name.dict[[length(loc.obj@node.suid.name.dict)+1]] <- 
            list(name=new.cynode.suid.name[[1]]$name, SUID=new.cynode.suid.name[[1]]$SUID)
        eval.parent(substitute(obj <- loc.obj))
    }
}) # addCyNode

# ------------------------------------------------------------------------------
setMethod('addCyEdge', 'OptionalCyWinClass', 
          function (obj, source.node.name, target.node.name, edgeType, directed) {
              
              good.args = TRUE
              # confirm that the user has provided exactly one source and one target nodes
              if(length(source.node.name) > 1 || length(target.node.name) > 1) {
                  good.args = FALSE
                  write(sprintf('RCy3::addEdge can have only one source and one target nodes'), stderr())
              }
              
              if(!source.node.name %in% getAllNodes(obj)) {
                  good.args = FALSE
                  write(sprintf('RCy3::addEdge. Error: source node %s does not exist in the Cytoscape graph. Edge cannot be created.', source.node.name), stderr())
              }
              if(!target.node.name %in% getAllNodes(obj)) {
                  good.args = FALSE
                  write(sprintf('RCy3::addEdge. Error: source node %s does not exist in the Cytoscape graph. Edge cannot be created.', target.node.name), stderr())
              }
              if(!good.args) {
                  return()
              }
              
              net.suid <- as.character(obj@suid)
              resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "edges", sep="/")
              
              node.names.vec <- sapply(obj@node.suid.name.dict, "[[", 1)
              edge.data <- list(source = obj@node.suid.name.dict[[which(node.names.vec %in% source.node.name)]]$SUID, 
                                target = obj@node.suid.name.dict[[which(node.names.vec %in% target.node.name)]]$SUID, 
                                directed = directed, interaction = edgeType)
              
              edge.data.JSON <- toJSON(list(edge.data))
              
              new.cyedge.res <- POST(url=resource.uri, body=edge.data.JSON, encode='json')
              invisible(new.cyedge.res)
              
              # if obj was provided, then modify
              if(!missing(obj)){
                  loc.obj <- obj
                  # add the edge to the R graph object
                  loc.obj@graph <- addEdge(source.node.name, target.node.name, loc.obj@graph)
                  eval.parent(substitute(obj <- loc.obj))
              }
          }) # addCyEdge

# ------------------------------------------------------------------------------
setMethod('getNodeCount', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes/count", sep="/")
              request.res <- GET(resource.uri)
              node.count <- unname(fromJSON(rawToChar(request.res$content)))
              
              return(as.integer(node.count))
          })
## END getNodeCount

# ------------------------------------------------------------------------------
setMethod('getEdgeCount', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- as.character(obj@suid)
              
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "edges/count", sep="/")
              request.res <- GET(resource.uri)
              edge.count <- unname(fromJSON(rawToChar(request.res$content)))
              
              return(as.integer(edge.count))
          })
## END getEdgeCount

# ------------------------------------------------------------------------------
setMethod('getAllNodes', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- getNetworkSuid(obj)
              
              n.count <- getNodeCount(obj)
              
              if(n.count == 0) {
                  return()
              }
              
              # get SUIDs of existing (in Cytoscape) nodes
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes", sep="/")
              # get the SUIDs of the nodes in the Cytoscape graph
              cy.nodes.SUIDs <- fromJSON(rawToChar(GET(resource.uri)$content))
              
              dict.nodes.SUIDs <- sapply(obj@node.suid.name.dict, "[[", 2)
              
              # check that the nodes presented in Cytoscape & RCy3's session dictionary do match
              diff.nodes <- setdiff(cy.nodes.SUIDs, dict.nodes.SUIDs)
              
              # in case that differences exist, run synchronization b/n RCy3 and RCytoscape
              if(length(diff.nodes) > 0) {
                  write(sprintf("WARNING in RCy3::getAllNodes():\n\t the following node(s) exist in Cytoscape, but don't exist in RCy3's session"), stderr())
                  
                  nodes.only.in.cytoscape <- c()
                  for(i in 1:length(diff.nodes)) {
                      resource.uri <- 
                          paste(obj@uri, obj@api, "networks", net.SUID, "nodes", as.character(diff.nodes[i]), sep="/")
                      node.name <- fromJSON(rawToChar(GET(resource.uri)$content))$data$name 
                      nodes.only.in.cytoscape <- c(nodes.only.in.cytoscape, node.name)
                      #    [GIK, Jul 2015] synch to be implemented
                      #    obj@node.suid.name.dict[[length(obj@node.suid.name.dict) + 1]] <- 
                      #        list(name=node.name, SUID=diff.nodes[i])
                  }
                  print(nodes.only.in.cytoscape)
              }
              node.names <- .nodeSUIDToNodeName(obj, cy.nodes.SUIDs[order(cy.nodes.SUIDs)])
              
              return(node.names)
          })
## END getAllNodes

# ------------------------------------------------------------------------------
setMethod('getAllEdges', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- getNetworkSuid(obj)
              
              
              count <- getEdgeCount(obj)
              if(count == 0) {
                  return()
              }
              
              # get edge name column and return its values
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns/name", sep="/")
              request.res <- GET(url=resource.uri)
              request.res <- fromJSON(rawToChar(request.res$content))
              names <- request.res$values
              return(names)
          })
## END getAllEdges

# ------------------------------------------------------------------------------
setMethod('clearSelection', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- getNetworkSuid(obj)
              
              
              # if any nodes are selected, unselect them
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns/selected?default=false", sep="/")
              request.res <- PUT(url=resource.uri, body=FALSE)
              
              # if any edges are selected, unselect them
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns/selected?default=false", sep="/")
              request.res <- PUT(url=resource.uri, body=FALSE)
              
              invisible(request.res)
          }) 
## END clearSelection

# ------------------------------------------------------------------------------
setMethod('selectNodes', 'missing', 
          function(node.names, by.col='name', preserve.current.selection = TRUE) {
              cw<-CytoscapeWindowFromNetwork()
              selectNodes(obj=cw,node.names,by.col,preserve.current.selection)
          });
setMethod('selectNodes', 'CytoscapeConnectionClass', 
          function(obj, node.names, by.col='name', preserve.current.selection = TRUE) {
              cw<-CytoscapeWindowFromNetwork(obj)
              selectNodes(obj=cw,node.names,by.col,preserve.current.selection)
          });
setMethod('selectNodes', 'CytoscapeWindowClass', 
          function(obj, node.names, by.col='name', preserve.current.selection = TRUE) {
              base.url=paste(obj@uri,obj@api,sep = "/")
              network=obj@title
              
              if (!preserve.current.selection )
                  clearSelection(obj)
              
              node.list.str = NULL
              for (n in node.names){
                  if(is.null(node.list.str))
                      node.list.str = paste(by.col,n,sep=":")
                  else
                      node.list.str = paste(node.list.str,paste(by.col,n,sep=":"),sep=",")
              }
              
              json_sel<-list(
                  network=network,
                  nodeList=node.list.str
              )
              sel <- toJSON(json_sel)
              url<- sprintf("%s/commands/network/select", base.url,sep="")
              response <- POST(url=url,body=sel, encode="json",content_type_json())
              selectedNodes=unname(fromJSON(rawToChar(response$content)))[[1]]
              if(length(selectedNodes)==0)
                  selectedNodes = c()
              return(selectedNodes)
          }) 
## END selectNodes

#' Select all nodes
#'
#' Selects all nodes in a Cytoscape Network 
#'
#' @param obj (optional) \code{CytoscapeConnection} or \code{CytoscapeWindow} 
#' 
#' @return Selects all nodes in a specified network. 
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{selectNodes}}
#'
#' @concept RCy3
#' @export
#' 
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' selectAllNodes(cw)
#' }
#' 
#' @importFrom methods setGeneric
setMethod('selectAllNodes',
          'OptionalCyWinClass', 
          function(obj) {
              
              resource.uri <- paste(obj@uri,
                                    obj@api,
                                    "networks",
                                    obj@suid,
                                    "nodes",
                                    sep = "/")
              
              request.res <- GET(resource.uri) # returns all of the node SUIDs
              all_node_SUIDs <- fromJSON(rawToChar(request.res$content))
              SUID.value.pairs <- lapply(all_node_SUIDs,
                                         function(s) {list('SUID' = s, 'value' = TRUE)})
              SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
              
              resource.uri <- paste(obj@uri,
                                    obj@api,
                                    "networks",
                                    obj@suid,
                                    "tables/defaultnode/columns/selected",
                                    sep = "/")
              request.res <- PUT(url = resource.uri,
                                 body = SUID.value.pairs.JSON,
                                 encode = "json")
              invisible(request.res)
          })



# ------------------------------------------------------------------------------
setMethod('getSelectedNodeCount', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- getNetworkSuid(obj)
              
              
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
              request.res <- GET(url=resource.uri)
              
              num.selected.nodes <- length(fromJSON(rawToChar(request.res$content)))
              
              return(num.selected.nodes)
          }) 
## END getSelectedNodeCount

# ------------------------------------------------------------------------------
setMethod('getSelectedNodes', 'missing', 
          function() {
              cw<-CytoscapeWindowFromNetwork()
              getSelectedNodes(cw)
          });
setMethod('getSelectedNodes', 'CytoscapeConnectionClass', 
          function(obj) {
              cw<-CytoscapeWindowFromNetwork(obj)
              getSelectedNodes(cw)
          });
setMethod('getSelectedNodes', 'CytoscapeWindowClass', 
          function(obj) {
              net.SUID <- getNetworkSuid(obj)
              
              
              if(getSelectedNodeCount(obj) == 0) {
                  write (sprintf ('warning!  No nodes selected.'), stdout ())
                  return(NA)
              } else {
                  resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
                  request.res <- GET(url=resource.uri)
                  
                  selected.node.SUIDs <- fromJSON(rawToChar(request.res$content))
                  selected.node.names <- .nodeSUIDToNodeName(obj, selected.node.SUIDs)
                  return(selected.node.names)
              }
          }) 
## END getSelectedNodes

# ------------------------------------------------------------------------------
# select all nodes that were not selected and deselect all nodes that were selected
setMethod('invertNodeSelection', 'OptionalCyWinClass', 
          function(obj) {
              commandRun(paste0('network select invert=nodes network=',obj@title), obj)
          }) 
## END invertNodeSelection

# ------------------------------------------------------------------------------
# [GIK - Jul, 2015] function might break if self-loops exist in the graph
setMethod('deleteSelectedNodes', 'OptionalCyWinClass', 
          function(obj) {
              
              if(missing(obj))
                  commandRun(paste0('network delete nodeList=selected network=',obj@title), obj)
              
              loc.obj <- obj
              
              net.SUID <- as.character(loc.obj@suid)
              
              selected.node.names <- getSelectedNodes(loc.obj)
              selected.node.SUIDs <- .nodeNameToNodeSUID(loc.obj, selected.node.names)
              
              for(i in 1:length(selected.node.SUIDs)) {
                  node.SUID <- selected.node.SUIDs[i]
                  # (list of) edges that have this particular node as their source node
                  source.bound.edges <- list()
                  # (list of) edges that have this particular node as their target node
                  target.bound.edges <- list()
                  
                  source.bound.edge.indices <- 
                      which(sapply(loc.obj@edge.node.suid.name.dict, function(n) {n$source.node}) %in% node.SUID)
                  
                  if(length(source.bound.edge.indices) > 0) {
                      # get edge SUIDs
                      source.bound.edges <- 
                          sapply(loc.obj@edge.node.suid.name.dict[source.bound.edge.indices], function(e) { e$SUID })
                      # delete all edges, whose source node is to-be deleted
                      for(k in 1:length(source.bound.edges)) {
                          resource.uri <- paste(loc.obj@uri, obj@api, "networks", net.SUID, "edges", as.character(source.bound.edges[k]), sep="/")
                          
                          request.res <- DELETE(url=resource.uri)
                          # [GIK] TO-DO: delete the edge row/entry from Cytoscape's Edge table
                      }
                      # also, delete those edges from the session dictionary
                      loc.obj@edge.node.suid.name.dict[source.bound.edge.indices] <- NULL
                  }
                  
                  target.bound.edge.indices <- 
                      which(sapply(loc.obj@edge.node.suid.name.dict, function(n) {n$target.node}) %in% node.SUID)
                  
                  if(length(target.bound.edge.indices) > 0) {
                      # get edge SUIDs
                      target.bound.edges <- 
                          sapply(loc.obj@edge.node.suid.name.dict[target.bound.edge.indices], function(e) { e$SUID })
                      # delete all edges, whose target node is to-be deleted
                      for(k in 1:length(target.bound.edges)) {
                          resource.uri <- paste(loc.obj@uri, obj@api, "networks", net.SUID, "edges", as.character(target.bound.edges[k]), sep="/")
                          
                          request.res <- DELETE(url=resource.uri)
                          # [GIK] TO-DO: delete the edge row/entry from Cytoscape's Edge table
                      }
                      # also, delete those edges from the session dictionary
                      loc.obj@edge.node.suid.name.dict[target.bound.edge.indices] <- NULL
                  }
                  
                  # delete the node from the Cytoscape network
                  resource.uri <- 
                      paste(loc.obj@uri, obj@api, "networks", net.SUID, "nodes", as.character(node.SUID), sep="/")
                  request.res <- DELETE(url=resource.uri)
                  
                  # [GIK] TO-DO: delete the node row/entry in the Cytoscape node table
                  
                  # delete the node from the session disctionary
                  node.index <- 
                      which(sapply(loc.obj@node.suid.name.dict, function(n) { n$SUID }) %in% node.SUID)
                  loc.obj@node.suid.name.dict[node.index] <- NULL
              }
              eval.parent(substitute(obj <- loc.obj))
          })
## END deleteSelectedNodes

# ------------------------------------------------------------------------------
setMethod('selectEdges', 'OptionalCyWinClass', 
          function(obj, edge.names, by.col, preserve.current.selection=TRUE) {
              base.url=paste(obj@uri,obj@api,sep = "/")
              network=obj@title
              
              if (!preserve.current.selection )
                  clearSelection(network=network,base.url=base.url)
              
              edge.list.str = NULL
              for (n in edge.names){
                  if(is.null(edge.list.str))
                      edge.list.str = paste(by.col,n,sep=":")
                  else
                      edge.list.str = paste(edge.list.str,paste(by.col,n,sep=":"),sep=",")
              }
              
              json_sel<-list(
                  network=network,
                  edgeList=edge.list.str
              )
              sel <- toJSON(json_sel)
              url<- sprintf("%s/commands/network/select", base.url,sep="")
              response <- POST(url=url,body=sel, encode="json",content_type_json())
              selectedEdges=unname(fromJSON(rawToChar(response$content)))[[1]]
              if(length(selectedEdges)==0)
                  selectedEdges = c()
              return(selectedEdges)
          }) 
## END selectEdges

#' Select all edges 
#'
#' Selects all edges in a Cytoscape Network 
#'
#' @param obj (optional) \code{CytoscapeConnection} or \code{CytoscapeWindow} 
#' 
#' @return Selects all edges in a specified network. 
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{selectEdges}}
#'
#' @concept RCy3
#' @export
#' 
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' selectAllEdges(cw)
#' }
#' 
#' @importFrom methods setGeneric
setMethod('selectAllEdges',
          'OptionalCyWinClass', 
          function(obj) {
              
              resource.uri <- paste(obj@uri,
                                    obj@api,
                                    "networks",
                                    obj@suid,
                                    "edges",
                                    sep = "/")
              
              request.res_edges <- GET(resource.uri) ## returns all of the edge suids
              all_edge_SUIDs <- fromJSON(rawToChar(request.res_edges$content))
              SUID.value.pairs <- lapply(all_edge_SUIDs,
                                         function(s) {list('SUID' = s, 'value' = TRUE)})
              SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
              
              resource.uri <- paste(obj@uri,
                                    obj@api,
                                    "networks",
                                    obj@suid,
                                    "tables/defaultedge/columns/selected",
                                    sep = "/")
              request.res <- PUT(url = resource.uri,
                                 body = SUID.value.pairs.JSON,
                                 encode = "json")
              invisible(request.res)
          })

# ------------------------------------------------------------------------------
setMethod('invertEdgeSelection', 'OptionalCyWinClass', 
          function(obj) {
              commandRun(paste0('network select invert=edges network=',obj@title), obj)
          }) 
## END invertEdgeSelection

# ------------------------------------------------------------------------------
setMethod('deleteSelectedEdges', 'OptionalCyWinClass', 
          function(obj) {
              
              if(missing(obj))
                  commandRun(paste0('network delete edgeList=selected network=',obj@title), obj)
              
              loc.obj <- obj
              
              net.SUID = as.character(loc.obj@suid)
              
              selected.edge.names = getSelectedEdges(loc.obj)
              selected.edge.SUIDs = .edgeNameToEdgeSUID(loc.obj, selected.edge.names)
              
              for(i in 1:length(selected.edge.SUIDs)) {
                  edge.SUID = selected.edge.SUIDs[i]
                  resource.uri = paste(loc.obj@uri, obj@api, "networks", net.SUID, "edges", edge.SUID, sep="/")
                  # delete edge from canvas / view
                  request.res = DELETE(url=resource.uri)
                  
                  # delete edge from edge table : NOT possible in the API
                  
                  # delete edge record from the session dictionary
                  loc.obj@edge.node.suid.name.dict[which(sapply(loc.obj@edge.node.suid.name.dict, function(e) { e$SUID }) %in% edge.SUID)] <- NULL
              }
              
              eval.parent(substitute(obj <- loc.obj))
          }) 
## END deleteSelectedEdges

# ------------------------------------------------------------------------------
setMethod('getSelectedEdgeCount', 'OptionalCyWinClass', 
          function(obj) {
              net.SUID <- getNetworkSuid(obj)
              
              
              resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
              request.res <- GET(url=resource.uri)
              
              num.selected.edges <- length(fromJSON(rawToChar(request.res$content)))
              return(num.selected.edges)
          })
## END getSelectedEdgeCount

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdges', 'OptionalCyWinClass',
           function (obj) {
               net.SUID = getNetworkSuid(obj)
               if(getSelectedEdgeCount(obj) == 0) {
                   return (NA)
               } else {
                   resource.uri = paste(obj@uri, obj@api, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
                   request.res = GET(url=resource.uri)
                   selected.edges.SUIDs = fromJSON(rawToChar(request.res$content))
                   selected.edges = .edgeSUIDToEdgeName(obj, selected.edges.SUIDs)
                   
                   return(selected.edges)
               }
           }) # getSelectedEdges

#------------------------------------------------------------------------------------------------------------------------

#' Get list of nodes neighboring provided list
#' 
#' @description Returns a non-redundan list of first
#' neighbors of the supplied list of nodes.
#' @param obj (optional) \code{CytoscapeConnection} or \code{CytoscapeWindow} 
#' @param node.names A \code{list} of node names from the \code{name} column of the \code{node table}
#' @param as.nested.list \code{logical} Whether to return lists of neighbors per query node
#' @return A list of unique node names, optionally nested per query node name.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon 
#' @seealso 
#' selectNodes  
#' selectFirstNeighbors
#' @examples /dontrun {
#' # first, delete existing windows to save memory:
#' deleteAllWindows(CytoscapeConnection())
#' 
#' cw <- CytoscapeWindow ('getFirstNeighbors.test', graph=makeSimpleGraph())
#' displayGraph (cw)
#' redraw (cw)
#' layoutNetwork(cw, 'grid')
#' print (getFirstNeighbors (cw, 'A'))
#' selectNodes (cw, getFirstNeighbors (cw, 'A'))  # note that A is not selected
#' }

#' @rdname getFirstNeighbors
setMethod ('getFirstNeighbors', 'missing',
           function (node.names, as.nested.list=FALSE) {
               if (length (node.names) == 0)
                   return()
               
               cw<-CytoscapeWindowFromNetwork()
               getFirstNeighbors(cw,node.names = node.names,as.nested.list = as.nested.list)
           });
#' @rdname getFirstNeighbors
setMethod ('getFirstNeighbors', 'CytoscapeConnectionClass',
           function (obj, node.names, as.nested.list=FALSE) {
               if (length (node.names) == 0)
                   return()
               
               cw<-CytoscapeWindowFromNetwork(obj)
               getFirstNeighbors(cw,node.names = node.names,as.nested.list = as.nested.list)
           });

#' @rdname getFirstNeighbors
setMethod ('getFirstNeighbors', 'CytoscapeWindowClass',
           function (obj, node.names, as.nested.list=FALSE) {
               if (length (node.names) == 0)
                   return()
               
               net.suid = as.character(obj@suid)
               neighbor.names <- c()
               
               for (node.name in node.names){
                   # get first neighbors for each node
                   node.SUID = .nodeNameToNodeSUID(obj,node.name)
                   resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "nodes", as.character(node.SUID), "neighbors", sep="/")
                   request.res <- GET(resource.uri)
                   first.neighbors.SUIDs <- fromJSON(rawToChar(request.res$content))
                   
                   if (as.nested.list){
                       neighbor.names <- append(neighbor.names, list(c(node.name, list(.nodeSUIDToNodeName(obj,first.neighbors.SUIDs)))))
                   }else{
                       neighbor.names <- c(neighbor.names, .nodeSUIDToNodeName(obj,first.neighbors.SUIDs))
                       neighbor.names <- unique(unlist(neighbor.names, use.names = FALSE))
                   }
                   
               }
               return (neighbor.names)
               
           })  # getFirstNeighbors

#------------------------------------------------------------------------------------------------------------------------
#' Select the edges connecting selected nodes in Cytoscape Network 
#'
#' Selects edges in a Cytoscape Network connecting the selected nodes 
#'
#' @param obj (optional) \code{CytoscapeConnection} or \code{CytoscapeWindow} 
#' 
#' @return network with edges selected 
#'
#' @examples \dontrun{
#' cw <- CytoscapeWindow('vignette select edges', graph = RCy3::makeSimpleGraph(), overwrite = TRUE)
#' displayGraph(cw)
#' selectNodes(cw,"A") # selects specific nodes
#' getSelectedNodes(cw)
#' getSelectedEdges(cw)
#' selectFirstNeighborsOfSelectedNodes(cw)
#' ## This has only selected the nodes, but not the edges in Cytoscape, so we will need to select all of the edges before we make the new subnetwork.
#' selectEdgesConnectedBySelectedNodes(cw)
#' getSelectedNodes(cw)
#' getSelectedEdges(cw)
#' }
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{createNetworkFromSelection}}, \code{\link{selectEdgesConnectedBySelectedNodes}}, \code{\link{renameNetwork}}
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
selectEdgesConnectedBySelectedNodes <- function(obj) {
    selectedNodes = getSelectedNodes(obj)
    if (length (selectedNodes) == 1 && is.na (selectedNodes))
        return ()
    graphEdges <- getAllEdges(obj)  
    selectedEdges <- unlist(mapply(function(x) return(graphEdges [grep(x, graphEdges)]), selectedNodes)) 
    if (length (selectedEdges) > 0)
        selectEdges(obj, selectedEdges)
}
# END selectEdgesConnectedBySelectedNodes	

#------------------------------------------------------------------------------------------------------------------------
#' Save a network in one of mulitple file formats 
setMethod ('saveNetwork', 'OptionalCyWinClass',
           
           function (obj, filename, type='cys') {
               if (!file.exists(filename)){
                   type=toupper(type)
                   if(type=='CYS'){ # save entire session
                       saveSession(filename = filename, obj = obj)
                   }
                   else { #e.g., CX, CYJS, GraphML, NNF, SIF, XGMML (case sensitive)
                       if(type=="GRAPHML") #fix case for exceptions
                           type = 'GraphML'
                       commandRun(paste0('network export options=',type,' OutputFile="',filename,'"'),obj)
                   }
               }else{
                   write (sprintf ('choose another filename. File exists: %s', filename), stderr ())
               }
           })



# ------------------------------------------------------------------------------
#' Create an igraph network from a Cytoscape network
#'
#' @description Takes a Cytoscape network and generates data frames for vertices and edges to
#' send to the graph_from_data_frame function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Nodes and edges from the Cytoscape network will be translated into vertices and edges
#' in igraph. Associated table columns will also be passed to igraph as vertiex and edge attributes.
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return (igraph) an igraph network
#' @export
#' @import igraph
#' @examples
#' \donttest{
#' createNetworkFromIgraph(g)
#' }
#' @seealso createNetworkFromDataFrames, createIgraphFromNetwork

createIgraphFromNetwork <- function(title=NA, obj=CytoscapeWindowFromNetwork(), ...){
    
    if(!is.na(title))
        obj<-CytoscapeWindowFromNetwork(title)
    else if(class(obj) == 'CytoscapeConnectionClass'){
        obj<-CytoscapeWindowFromNetwork()
    }
    network = obj@suid
    
    #get dataframes
    cyedges <- getTableColumns('edge',obj=obj)
    cynodes <- getTableColumns('node',obj=obj)
    
    #check for source and target columns
    if(!"source" %in% colnames(cyedges)||(!"target" %in% colnames(cyedges))){
        st=data.frame(do.call('rbind',strsplit(cyedges$name,"\ \\(.*\\)\ ")))
        colnames(st) <- c("source","target")
        cyedges <- cbind(st,cyedges)
    }
    
    #setup columns for igraph construction
    colnames(cyedges)[colnames(cyedges)=="source"]<-"from"
    colnames(cyedges)[colnames(cyedges)=="target"]<-"to"
    cyedges2=cbind(cyedges[c("from","to")], cyedges[ ,!(names(cyedges) %in% c("from","to"))])
    cynodes2=cbind(cynodes["name"], cynodes[ ,!(names(cynodes)=="name")])
    
    #ship
    graph_from_data_frame(cyedges2, directed=TRUE, vertices=cynodes2)
}

# ------------------------------------------------------------------------------
#' Create a Cytoscape network from an igraph network
#'
#' @description Takes an igraph network and generates data frames for nodes and edges to
#' send to the createNetwork function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Vertices and edges from the igraph network will be translated into nodes and edges
#' in Cytoscape. Associated attributes will also be passed to Cytoscape as node and edge table columns.
#' @param igraph (igraph) igraph network object
#' @param new.title (char) network name
#' @param collection.title (char) network collection name
#' @param base.url cyrest base url for communicating with cytoscape
#' @param ... params for nodeSet2JSON() and edgeSet2JSON(); see createNetwork
#' @return (int) network SUID
#' @export
#' @import igraph
#' @examples
#' \donttest{
#' createNetworkFromIgraph(g)
#' }
#' @seealso createNetworkFromDataFrames, createIgraphFromNetwork

createNetworkFromIgraph <- function(igraph, new.title="MyNetwork",
                                    collection.title="myNetworkCollection",return.graph=FALSE, obj=CytoscapeConnection(),...) {
    
    #extract dataframes
    igedges = as_data_frame(igraph, what="edges")
    ignodes = as_data_frame(igraph, what="vertices")
    
    #setup columns for Cytoscape import
    ignodes$id <- row.names(ignodes)
    colnames(igedges)[colnames(igedges)=="from"]<-"source"
    colnames(igedges)[colnames(igedges)=="to"]<-"target"
    
    #ship
    createNetworkFromDataFrames(ignodes,igedges,new.title,collection.title,return.graph,obj)
}

# ------------------------------------------------------------------------------
#' Create a network from data frames
#'
#' @description Takes data frames for nodes and edges, as well as naming parameters to
#' generate the JSON data format required by the "networks" POST operation via CyREST.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details NODES should contain a column named: id. This name can be overridden by
#' the arg: node.id.list. Additional columns are loaded as node attributes.
#' EDGES should contain columns named: source, target and interaction. These names can be overridden by
#' args: source.id.list, target.id.list, interaction.type.list. Additional columns
#' are loaded as edge attributes. The 'interaction' list can contain a single
#' value to apply to all rows; and if excluded altogether, the interaction type
#' wiil be set to "interacts with". NOTE: attribute values of types (num) and (int) will be imported
#' as (Double); (chr) as (String); and (logical) as (Boolean).
#' @param nodes (data.frame) see details and examples below; default NULL to derive nodes from edge sources and targets
#' @param edges (data.frame) see details and examples below; default NULL for disconnected set of nodes
#' @param new.title (char) network name
#' @param collection.title (char) network collection name
#' @param base.url cyrest base url for communicating with cytoscape
#' @param ... params for nodeSet2JSON() and edgeSet2JSON()
#' @return (int) network SUID
#' @export
#' @import RJSONIO
#' @seealso createSubnetwork
#' @examples
#' \donttest{
#' nodes <- data.frame(id=c("node 0","node 1","node 2","node 3"),
#'            group=c("A","A","B","B"), # optional
#'            stringsAsFactors=FALSE)
#' edges <- data.frame(source=c("node 0","node 0","node 0","node 2"),
#'            target=c("node 1","node 2","node 3","node 3"),
#'            interaction=c("inhibits","interacts","activates","interacts"),  # optional
#'            weight=c(5,3,5,9), # optional
#'            stringsAsFactors=FALSE)
#'
#' createNetworkFromDataFrames(nodes,edges)
#' }

createNetworkFromDataFrames <- function(nodes=NULL,edges=NULL,new.title="MyNetwork",
                                        collection.title="MyNetworkCollection",return.graph=FALSE, obj=CytoscapeConnection(),...) {
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    #defining variable names to be used globally later on (to avoid devtools::check() NOTES)
    RCy3.CreateNetworkFromDataFrames.temp.global.counter <- NULL
    RCy3.CreateNetworkFromDataFrames.temp.global.size <- NULL
    RCy3.CreateNetworkFromDataFrames.temp.global.json_set <- NULL
    
    if (is.null(nodes)) {
        if (!is.null(edges)) {
            nodes = data.frame(id=c(edges$source,edges$target),stringsAsFactors = FALSE)
        }else
            return("Create Network Failed: Must provide either nodes or edges")
    }
    
    json_nodes <- nodeSet2JSON(nodes,...)
    # cleanup global environment variables (which can be quite large)
    remove(RCy3.CreateNetworkFromDataFrames.temp.global.counter, envir = globalenv())
    remove(RCy3.CreateNetworkFromDataFrames.temp.global.size, envir = globalenv())
    remove(RCy3.CreateNetworkFromDataFrames.temp.global.json_set, envir = globalenv())
    
    json_edges<-c()
    
    if(!is.null(edges)){
        json_edges <- edgeSet2JSON(edges,...)
        # cleanup global environment variables (which can be quite large)
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.counter, envir = globalenv())
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.size, envir = globalenv())
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.json_set, envir = globalenv())
    } else {
        json_edges <- "[]" #fake empty array
    }
    
    json_network <- list(
        data<-list(name=new.title),
        elements<-c(nodes=list(json_nodes),edges=list(json_edges))
    )
    
    network <- toJSON(json_network)
    
    #swap any spaces in names
    new.title <- gsub(" ","%20",new.title)
    collection.title <- gsub(" ","%20",collection.title)
    
    url<- sprintf("%s/networks?title=%s&collection=%s",
                  base.url,new.title,collection.title,sep="")
    
    response <- POST(url=url,body=network, encode="json",content_type_json())
    
    network.suid <- unname(fromJSON(rawToChar(response$content)))
    if(is.numeric(network.suid))
        cat(sprintf("Network SUID is : %i \n", network.suid))
    else
        return(response)
    
    cat("Applying default style\n")
    commandRun('vizmap apply styles="default"',obj)
    
    cat(sprintf("Applying %s layout\n", invisible(commandRun('layout get preferred network="current"'))))
    commandRun('layout apply preferred networkSelected="current',obj)
    
    net.title = getNetworkName(network.suid)
    net.cw<-CytoscapeWindowFromNetwork(net.title,return.graph=return.graph)
    return(net.cw)
}

# Convert edges to JSON format needed for CyRest network creation
#
# @param edge_set (data.frame) Rows contain pairwise interactions.
# @param source.id.list (char) override default list name for source node ids
# @param target.id.list (char) override default list name for target node ids
# @param interaction.type.list (char) override default list name for interaction types
#
edgeSet2JSON <- function(edge_set, source.id.list = 'source',
                         target.id.list = 'target', interaction.type.list='interaction',...){
    
    #using global environment variables for performance
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter<-0
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size<-1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set<-c()
    #json_edges <- c()
    
    if(!(interaction.type.list %in% names(edge_set)))
        edge_set[,interaction.type.list] = rep('interacts with')
    
    computed_name <- paste(edge_set[,source.id.list], paste('(',edge_set[,interaction.type.list],')',sep=''),
                           edge_set[,target.id.list],sep=" ")
    
    for(i in 1:dim(edge_set)[1]){
        rest <- list()
        rest[["name"]] = computed_name[i]
        for(j in 1:dim(edge_set)[2]){
            rest[[colnames(edge_set)[j]]] = edge_set[i,j]
        }
        current_edge = list("data"=rest)
        #json_edges[[i]] <- current_edge
        FastAppendListGlobal(current_edge)
    }
    return(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[1:.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter])
}

# Creates a table of nodes to CyREST JSON
#
# @param node.set (data.frame) each row is a node and columns contain node attributes
# @param node.id.list (char) override default list name for node ids
# Adapted from Ruth Isserlin's CellCellINteractions_utility_functions.R
nodeSet2JSON <- function(node.set, node.id.list='id',...){
    
    #using global environment variables for performance
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter<-0
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size<-1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set<-c()
    #json_nodes <- c()
    
    for(i in 1:dim(node.set)[1]){
        rest <- list()
        for(j in 1:dim(node.set)[2]){
            rest[[colnames(node.set)[j]]] = node.set[i,j]
        }
        current_node = list("data"=rest)
        #json_nodes[[i]] <- current_node
        FastAppendListGlobal(current_node)
    }
    return(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[1:.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter])
}

# FastAppendListGlobal
# Appends lists at high performance using global variables explictly
#  Note: relies on managing gloval environment variables: initializing and removing
#  https://stackoverflow.com/questions/17046336/here-we-go-again-append-an-element-to-a-list-in-r
#
FastAppendListGlobal <- function(item)
{
    if( .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter == .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size )
        length(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set) <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size * 2
    
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter + 1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[[.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter]] <- item
}

# ------------------------------------------------------------------------------
#' @title Create subnetwork from existing network
#'
#' @description Copies a subset of nodes and edges into a newly created subnetwork.
#' @details If you specify both nodes and edges, the resulting subset will be the union of those sets.
#' Typical usage only requires specifying either nodes or edges. Note that selected nodes will bring
#' along their connecting edges by default (see exclude.edges arg) and selected edges will always
#' bring along their source and target nodes.
#' @param nodes list of node names or keyword: selected, unselected or all
#' @param nodes.by.col name of node table column corresponding to provided nodes list; default is 'name'
#' @param edges list of edge names or keyword: selected, unselected or all
#' @param edges.by.col name of edge table column corresponding to provided edges list; default is 'name'
#' @param exclude.edges (boolean) whether to exclude connecting edges; default is FALSE
#' @param new.title name of new subnetwork to be created;
#' default is to add a numbered suffix to source network name
#' @param network name or suid of the source network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return SUID of new subnetwork
#' @export
#' @examples
#' \donttest{
#' createSubnetwork("selected")
#' createSubnetwork("selected",new.title="mySubnetwork")
#' createSubnetwork(c("node 1","node 2","node 3"))
#' createSubnetwork(c("AKT1","TP53","PIK3CA"),"display name")
#' createSubnetwork(edges="all") #subnetwork of all connected nodes
#' }
#' @seealso createNetworkFromDataFrames

createSubnetwork <- function(nodes,nodes.by.col='name',edges,edges.by.col='name',
                             exclude.edges='F',new.title, return.graph=FALSE, obj=CytoscapeWindowFromNetwork()){
    base.url=paste(obj@uri,obj@api,sep = "/")
    network=obj@title
    
    if(exclude.edges){
        exclude.edges = "true"
    } else {
        exclude.edges = "false"
    }
    
    json_sub=NULL
    json_sub$source=network
    json_sub$excludeEdges=exclude.edges
    
    node.str = NULL
    if(missing(nodes)){
        json_sub$nodeList="selected" #need something here for edge selections to work
    } else {
        if(!nodes[1] %in% c('all','selected','unselected')){
            for (n in nodes){
                if(is.null(node.str))
                    node.str = paste(nodes.by.col,n,sep=":")
                else
                    node.str = paste(node.str,paste(nodes.by.col,n,sep=":"),sep=",")
            }
        } else {
            node.str = nodes
        }
        json_sub$nodeList=node.str
    }
    
    edge.str = NULL
    if(!missing(edges)){
        if(!edges[1] %in% c('all','selected','unselected')){
            for (e in edges){
                if(is.null(edge.str))
                    edge.str = paste(edges.by.col,e,sep=":")
                else
                    edge.str = paste(edge.str,paste(edges.by.col,e,sep=":"),sep=",")
            }
        } else {
            edge.str = edges
        }
        json_sub$edgeList=edge.str
    }
    
    subnetwork.arg = NULL
    if(!missing(new.title)){
        json_sub$networkName=new.title
    }
    
    sub <- toJSON(as.list(json_sub))
    url<- sprintf("%s/commands/network/create", base.url,sep="")
    response <- POST(url=url,body=sub, encode="json",content_type_json())
    subnetwork.suid=unname(fromJSON(rawToChar(response$content)))[[1]][[1]]
    
    sub.title = getNetworkName(subnetwork.suid)
    sub.cw<-CytoscapeWindowFromNetwork(sub.title,return.graph=return.graph)
    return(sub.cw)
}
