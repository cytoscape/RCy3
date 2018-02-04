# ------ TODO -----------------------------------------------------------------
#getNetworkAsCx http://localhost:1234/v1/collections/1547.cx

#------------------------------------------------------------------------------------------------------------------------
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
#' @param subnetwork.name name of new subnetwork to be created;
#' default is to add a numbered suffix to source network name
#' @param network name or suid of the source network; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return SUID of new subnetwork
#' @export
#' @examples
#' \donttest{
#' createSubnetwork("selected")
#' createSubnetwork("selected",subnetwork.name="mySubnetwork")
#' createSubnetwork(c("node 1","node 2","node 3"))
#' createSubnetwork(c("AKT1","TP53","PIK3CA"),"display name")
#' createSubnetwork(edges="all") #subnetwork of all connected nodes
#' }
createSubnetwork <- function(nodes,nodes.by.col='name',edges,edges.by.col='name',
                             exclude.edges='F',subnetwork.name,network=NULL, base.url='http://localhost:1234/v1'){
    
    title = getNetworkName(network, base.url)
    
    if(exclude.edges){
        exclude.edges = "true"
    } else {
        exclude.edges = "false"
    }
    
    json_sub=NULL
    json_sub$source=title
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
    if(!missing(subnetwork.name)){
        json_sub$networkName=subnetwork.name
    }
    
    sub <- toJSON(as.list(json_sub))
    url<- sprintf("%s/commands/network/create", base.url,sep="") ##TODO swap with commandsPOST (POST)?
    response <- POST(url=url,body=sub, encode="json",content_type_json())
    subnetwork.suid=unname(fromJSON(rawToChar(response$content)))[[1]][[1]]
    #cat(sprintf("Subnetwork SUID is : %i \n", subnetwork.suid))
    return(subnetwork.suid)
}

# ------------------------------------------------------------------------------
#' @title Select first neighbor nodes
#' @description Select nodes directly connected to currently selected nodes. Can
#' specify connection directionality using the direction param.
#' @param direction direction of connections to neighbors to follow, e.g., incoming, outgoing, undirected, or any (default)
#' @param network name or suid of the network; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return list of suids of selected nodes, including original selection
#' @examples
#' \donttest{
#' selectFirstNeighbors()
#' selectFirstNeighors('outgoing')
#' selectFirstNeighors('incoming')
#' }
#' @export
selectFirstNeighbors <- function(direction='any', network=NULL, base.url=.defaultBaseUrl){
    suid=getNetworkSuid(network)
    cmd<-paste0('network select firstNeighbors="',direction,'" network=SUID:"',suid,'"')
    res <- commandsPOST(cmd,base.url=base.url)
    return(res['nodes'])
}

# ------------------------------------------------------------------------------
#' @title Set current network
#'
#' @description Selects the given network as "current"
#' @param network name or suid of the network that you want set as current
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return server response
#' @export
#' @examples
#' \donttest{
#' setCurrentNetwork('MyNetwork')
#' }

setCurrentNetwork <- function(network=NULL, base.url=.defaultBaseUrl){
    suid = getNetworkSuid(network)
    cmd<-paste0('network set current network=SUID:"',suid,'"')
    commandsPOST(cmd,base.url=base.url)
}

#' Clone a Cytoscape Network 
#'
#' @description Makes a copy of a Cytoscape Network with all of its edges and nodes. 
#' @param network name or suid of the network you want to clone; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return The \code{suid} of the new network 
#' @examples \dontrun{
#' cloneNetwork("cloned network")
#' }
#' @author Alexander Pico, Julia Gustavsen
#' @export

cloneNetwork <- function(network=NULL, base.url =.defaultBaseUrl) {
    suid = getNetworkSuid(network)
    cmd<-paste0('network clone network=SUID:"',suid,'"')
    res <- commandsPOST(cmd,base.url=base.url)
    return(res['network'])
}

#' Rename a network 
#'
#' @description Sets a new name for this network 
#' @details Duplicate network names are not allowed
#' @param new.title New name for the network
#' @param network name or suid of the network that you want to rename; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return None
#' @author Alexander Pico, Julia Gustavsen
#' @examples \dontrun{
#' renameNetwork("renamed network")
#' }
#' @export
renameNetwork <- function(new.title, network=NULL, base.url =.defaultBaseUrl) {
    old.suid = getNetworkSuid(network)
    cmd<-paste0('network rename name="',new.title,'" sourceNetwork=SUID:"',old.suid,'"')
    res <- commandsPOST(cmd,base.url=base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' Get the number of Cytoscape networks 
#'
#' @description Returns the number of Cytoscape networks in the current Cytoscape session  
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return \code{numeric}
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \dontrun{
#' getNetworkCount()
#' # 3
#' }
#' @export
getNetworkCount <- function(base.url =.defaultBaseUrl) {
    res <- cyrestGET('networks/count', base.url = base.url)
    return(as.integer(unname(res)))
}

# ------------------------------------------------------------------------------
#' Get the name of a network
#'
#' @param suid SUID of the network; default is current network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return network name
#' @export
#' @examples
#' \donttest{
#' getNetworkName()
#' getNetworkName(1111)
#' }

getNetworkName <- function(suid=NULL, base.url=.defaultBaseUrl) {
    
    if (is.character(suid)){ #title provided
        if (suid=='current'){
            network.suid = getNetworkSuid() 
        } else {
            net.names <- getNetworkList(base.url = base.url)
            if(suid %in% net.names){
                return(suid)
            }else {
                stop(paste0("Network does not exist: ",suid))
            }
        }
    }else if (is.numeric(suid)){ #suid provided
        network.suid = suid
    } else {  #use current network
        network.suid = getNetworkSuid() 
    }
    
    res <- cyrestGET('networks.names',list(column="suid",query=network.suid,base.url=base.url))
    network.name <- unname(res)[[1]]$name
    return(network.name)
}

# ------------------------------------------------------------------------------
#' Get the SUID of a network
#'
#' @param title Name of the network; default is "current" network
#' @param base.url (optional)  URL prefix for CyREST calls
#' @return (\code{numeric}) Network suid
#' @author Alexander Pico
#' @examples
#' \donttest{
#' getNetworkSuid()
#' getNetworkSuid("myNetwork")
#' # 80
#' }
#' @export
getNetworkSuid <- function(title=NULL, base.url = .defaultBaseUrl) {
    
    if (is.character(title)){ #title provided
        if (title=='current'){
            network.title = title 
        } else {
            net.names <- getNetworkList(base.url = base.url)
            if(title %in% net.names){
                network.title = title
            }else {
                stop(paste0("Network does not exist: ",title))
            }
        }
    }else if (is.numeric(title)){ #suid provided
        net.suids <- cyrestGET('networks',base.url=base.url)
        if(title %in% net.suids){
            return(title)
        }else {
            stop(paste0("Network does not exist: ",title))
        }
    } else {  #use current network
        network.title= 'current' 
    }

    cmd<-paste0('network get attribute network="',network.title,'" namespace="default" columnList="SUID"')
    res <- commandsPOST(cmd,base.url=base.url)
    suid <- gsub("\\{SUID:|\\}","",res)
    return(as.numeric(suid))
}

# ------------------------------------------------------------------------------
#' Get the list of Cytoscape networks 
#'
#' @description Returns the list of Cytoscape network names in the current Cytoscape session  
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return \code{list}
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \dontrun{
#' getNetworkList()
#' # 3
#' }
#' @export
getNetworkList <- function(base.url = .defaultBaseUrl) {
    if(getNetworkCount(base.url) == 0) {
        return(c())
    }
    cy.networks.SUIDs <- cyrestGET('networks',base.url=base.url)
    cy.networks.names = c()
    for(suid in cy.networks.SUIDs)	{
        res <- cyrestGET(paste("networks", as.character(suid), sep="/"), base.url = base.url)
        net.name <- res$data$name
        cy.networks.names <- c(cy.networks.names, net.name)
    }
    return(cy.networks.names)
}

# ------------------------------------------------------------------------------
#' @export
deleteNetwork <- function (network=NULL, base.url=.defaultBaseUrl) {
    suid = getNetworkSuid(network)
    resource.uri = paste(base.url, "networks", suid, sep="/")
    request.res = DELETE(url=resource.uri)
    invisible(request.res)
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
deleteAllNetworks <- function (base.url=.defaultBaseUrl) {
    resource.uri <- paste(base.url, "networks", sep="/")
    request.res <- DELETE(resource.uri)
    invisible(request.res)
}

# ------------------------------------------------------------------------------
#' @export
addCyNode <- function(node.name, network=NULL, base.url = .defaultBaseUrl) {
    net.suid <- getNetworkSuid(network)
    
    if(node.name %in% getAllNodes(net.suid, base.url)) {
        write(sprintf('RCy3::addCyNode, node "%s" already present in Cytoscape graph', node.name), stderr())
        return()
    }
    
    resource.uri <- paste(base.url, "networks", net.suid, "nodes", sep="/")
    nodename.json = toJSON(c(node.name))
    invisible(POST(url=resource.uri, body=nodename.json, encode="json"))
}

# ------------------------------------------------------------------------------
#' @export
addCyEdge <- function (source.node.name, target.node.name, edgeType='pp', directed=FALSE, 
                       network=NULL, base.url=.defaultBaseUrl) {
    
    good.args = TRUE
    # confirm that the user has provided exactly one source and one target nodes
    if(length(source.node.name) > 1 || length(target.node.name) > 1) {
        good.args = FALSE
        write(sprintf('RCy3::addEdge can have only one source and one target nodes'), stderr())
    }
    
    net.suid <- getNetworkSuid(network)
    source.node.suid <- .nodeNameToNodeSUID(source.node.name, net.suid, base.url)
    target.node.suid <- .nodeNameToNodeSUID(target.node.name, net.suid, base.url)
    
    if(!source.node.name %in% getAllNodes(net.suid, base.url)) {
        good.args = FALSE
        write(sprintf('RCy3::addEdge. Error: source node %s does not exist. Edge cannot be created.', source.node.name), stderr())
    }
    if(!target.node.name %in% getAllNodes(net.suid, base.url)) {
        good.args = FALSE
        write(sprintf('RCy3::addEdge. Error: source node %s does not exist. Edge cannot be created.', target.node.name), stderr())
    }
    if(!good.args) {
        return()
    }

    edge.data <- list(list(source = source.node.suid, 
                      target = target.node.suid, 
                      directed = directed, interaction = edgeType))
    res <- cyrestPOST(paste("networks", net.suid, "edges", sep="/"),
                      body=edge.data,
                      base.url=base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @export
getNodeCount <- function(network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <- cyrestGET(paste("networks", net.SUID, "nodes/count", sep="/"),base.url=base.url)
    return(as.integer(unname(res)))
}

# ------------------------------------------------------------------------------
#' @export
getEdgeCount <- function(network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <- cyrestGET(paste("networks", net.SUID, "edges/count", sep="/"),base.url=base.url)
    return(as.integer(unname(res)))
}

# ------------------------------------------------------------------------------
#' @export
getAllNodes <- function(network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    n.count <- getNodeCount(net.SUID, base.url)
    if(n.count == 0) {
        return()
    }
    res <- cyrestGET(
        paste("networks", net.SUID, "tables/defaultnode/columns/name", sep="/"), base.url=base.url)
    return(res$values)
}
# ------------------------------------------------------------------------------
#' @export
getAllEdges <- function(network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    e.count <- getEdgeCount(net.SUID,base.url)
    if(e.count == 0) {
        return()
    }
    res <- cyrestGET(
        paste("networks", net.SUID, "tables/defaultedge/columns/name", sep="/"),base.url=base.url)
    return(res$values)
}

# ------------------------------------------------------------------------------
#' @export
clearSelection <- function(type='both',network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    
    if(type %in% c('nodes','both')){
        resource.uri <- 
            res <- cyrestPUT(
                paste("networks", net.SUID, "tables/defaultnode/columns/selected", sep="/"), 
                parameters=list(default="false"),
                base.url=base.url)
    }
    if(type %in% c('edges','both')){
        res <- cyrestPUT(
            paste("networks", net.SUID, "tables/defaultedge/columns/selected", sep="/"), 
            parameters=list(default="false"),
            base.url=base.url)
    }
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @export
selectNodes <- function(node.names, by.col='name', preserve.current.selection = TRUE, 
                        network=NULL, base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    
    if (!preserve.current.selection )
        clearSelection(type='nodes',suid,base.url)
    
    node.list.str = NULL
    for (n in node.names){
        if(is.null(node.list.str))
            node.list.str = paste(by.col,n,sep=":")
        else
            node.list.str = paste(node.list.str,paste(by.col,n,sep=":"),sep=",")
    }
    res <- commandsPOST(paste0('network select network=SUID:"',suid,'" nodeList="',node.list.str),
                        base.url = base.url)
    return(res$nodes)
}

#' Select all nodes
#'
#' @description Selects all nodes in a Cytoscape Network 
#' @param network name or suid of the network into which you want to select; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return Selects all nodes in a specified network. 
#' @author Alexander Pico, Julia Gustavsen
#' @seealso \code{\link{selectNodes}}
#' @examples \donttest{
#' selectAllNodes()
#' }
#' @export

selectAllNodes <- function(network=NULL, base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    all_node_SUIDs <- cyrestGET(paste("networks", suid, "nodes", sep = "/"),base.url=base.url)
    
    # prepare selection values
    SUID.value.pairs <- lapply(all_node_SUIDs,
                               function(s) {list('SUID' = s, 'value' = TRUE)})
    res <- cyrestPUT(paste("networks",suid, "tables/defaultnode/columns/selected", sep = "/"),
                     body = SUID.value.pairs, 
                     base.url=base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @export
getSelectedNodeCount <- function(network=NULL, base.url=.defaultBaseUrl) {
              net.SUID <- getNetworkSuid(network)
              res <- cyrestGET(paste('networks',net.SUID,'nodes', sep="/"),
                               list(column="selected", query="true"),
                               base.url=base.url)
              return(length(res))
          }

# ------------------------------------------------------------------------------
#' @export
getSelectedNodes <- function(network=NULL, base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    
    if(getSelectedNodeCount(net.SUID, base.url) == 0) {
        write (sprintf ('No nodes selected.'), stdout ())
        return(NA)
    } else {
        selected.node.SUIDs <- cyrestGET(paste("networks", net.SUID, "nodes", sep="/"),
                                         list(column="selected",query="true"),
                                         base.url=base.url)
        selected.node.names <- .nodeSUIDToNodeName(selected.node.SUIDs,net.SUID,base.url)
        return(selected.node.names)
    }
}


# ------------------------------------------------------------------------------
# select all nodes that were not selected and deselect all nodes that were selected
#' @export
invertNodeSelection <- function(network=NULL,base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    res <- commandsPOST(paste0('network select invert=nodes network=SUID:',suid), base.url=base.url)
    return(res$nodes)
}

# ------------------------------------------------------------------------------
#' Delete Selected Nodes
#' 
#' @return A \code{named list} of deleted node suids ($nodes) as well as edge suids 
#' ($edges) deleted as a result of the node deletion
#' @export
deleteSelectedNodes <- function(network=NULL,base.url=.defaultBaseUrl) {
    title=getNetworkName(network)
    commandsPOST(paste0('network delete nodeList=selected network=',title), base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @export
selectEdges <- function(edge.names, by.col='name', preserve.current.selection=TRUE,
                        network=NULL,base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    
    if (!preserve.current.selection )
        clearSelection(type='edges',network=suid,base.url=base.url)
    
    edge.list.str = NULL
    for (n in edge.names){
        if(is.null(edge.list.str))
            edge.list.str = paste(by.col,n,sep=":")
        else
            edge.list.str = paste(edge.list.str,paste(by.col,n,sep=":"),sep=",")
    }
    res <- commandsPOST(paste0('network select network=SUID:"',suid,'" edgeList="',edge.list.str), 
                             base.url=base.url)
    return(res$edges)
}

#' Select all edges 
#'
#' @description Selects all edges in a Cytoscape Network 
#' @param network name or suid of the network; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return Selects all edges in a specified network. 
#' @author Alexander Pico, Julia Gustavsen
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' selectAllEdges(cw)
#' }
#' @export
selectAllEdges <- function(network=NULL,base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    all_edge_SUIDs <- cyrestGET(paste('networks',suid,'edges',sep="/"), base.url = base.url)
    SUID.value.pairs <- lapply(all_edge_SUIDs,
                               function(s) {list('SUID' = s, 'value' = TRUE)})
    res <- cyrestPUT(paste('networks',suid,'tables/defaultedge/columns/selected',sep="/"),
                     body = SUID.value.pairs,
                     base.url=base.url)
    invisible(res)
}

# ------------------------------------------------------------------------------
#' @export
invertEdgeSelection <- function(network=NULL,base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    res <- commandsPOST(paste0('network select invert=edges network=SUID:',suid), base.url = base.url)
    return(res$edges)
}

# ------------------------------------------------------------------------------
#' @export
deleteSelectedEdges <- function(network=NULL,base.url=.defaultBaseUrl) {
    suid <- getNetworkSuid(network)
    res <- commandsPOST(paste0('network delete edgeList=selected network=SUID:', suid), base.url = base.url)
    return(res$edges)
}

# ------------------------------------------------------------------------------
#' @export
getSelectedEdgeCount <- function(network=NULL,base.url=.defaultBaseUrl) {
    net.SUID <- getNetworkSuid(network)
    res <- cyrestGET(paste("networks", net.SUID, "edges", sep="/"),
                     list(column="selected", query="true"),
                     base.url=base.url)
    return(length(res))
}

#------------------------------------------------------------------------------------------------------------------------
#' @export
getSelectedEdges <- function (network=NULL,base.url=.defaultBaseUrl) {
    net.SUID = getNetworkSuid(network)
    if(getSelectedEdgeCount(net.SUID,base.url) == 0) {
        return (NA)
    } else {
        selected.edges.SUIDs = cyrestGET(paste("networks", net.SUID, "edges", sep="/"),
                                         list(column="selected", query="true"),
                                         base.url=base.url)
        selected.edges = .edgeSUIDToEdgeName(selected.edges.SUIDs,net.SUID,base.url)
        return(selected.edges)
    }
}

#------------------------------------------------------------------------------------------------------------------------

#' Get list of nodes neighboring provided list
#' 
#' @description Returns a non-redundan list of first
#' neighbors of the supplied list of nodes.
#' @param node.names A \code{list} of node names from the \code{name} column of the \code{node table}
#' @param as.nested.list \code{logical} Whether to return lists of neighbors per query node
#' @param network name or suid of the network; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return A list of unique node names, optionally nested per query node name.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon 
#' @seealso 
#' selectNodes  
#' selectFirstNeighbors
#' @examples /dontrun {
#' }
#' @export

getFirstNeighbors <- function (node.names, as.nested.list=FALSE, network=NULL, base.url=.defaultBaseUrl) {
    if (length (node.names) == 0)
        return()
    
    net.SUID = getNetworkSuid(network)
    neighbor.names <- c()
    
    for (node.name in node.names){
        # get first neighbors for each node
        node.SUID = .nodeNameToNodeSUID(node.name,net.SUID,base.url)
        first.neighbors.SUIDs <- cyrestGET(
            paste("networks", net.SUID, "nodes", as.character(node.SUID), "neighbors", sep="/"),
            base.url = base.url)
        first.neighbors.names <- .nodeSUIDToNodeName(first.neighbors.SUIDs,net.SUID,base.url)
        
        if (as.nested.list){
            neighbor.names <- append(neighbor.names, list(c(node.name, list(first.neighbors.names))))
        } else {
            neighbor.names <- c(neighbor.names, first.neighbors.names)
            neighbor.names <- unique(unlist(neighbor.names, use.names = FALSE))
        }
    }
    return (neighbor.names)
}

#------------------------------------------------------------------------------------------------------------------------
#' Select the edges connecting selected nodes in Cytoscape Network 
#'
#' @description Selects edges in a Cytoscape Network connecting the selected nodes 
#' @param network name or suid of the network; default is "current" network
#' @param base.url  (optional)  URL prefix for CyREST calls
#' @return network with edges selected 
#' @examples \dontrun{
#' }
#'
#' @author Alexander Pico, Julia Gustavsen
#' @export

selectEdgesConnectingSelectedNodes <- function(network=NULL, base.url=.defaultBaseUrl) {
    net.suid <- getNetworkSuid(network)
    selectedNodes = getSelectedNodes(net.suid,base.url)
    if (length (selectedNodes) == 1 && is.na (selectedNodes))
        return ()
    allEdges <- getAllEdges(net.suid,base.url)  
    selectedSources <- unlist(mapply(function(x) return(allEdges [startsWith(allEdges, x)]), selectedNodes)) 
    selectedTargets <- unlist(mapply(function(x) return(allEdges [endsWith(allEdges, x)]), selectedNodes)) 
    selectedEdges <- intersect(selectedSources,selectedTargets)
    selectEdges(selectedEdges, preserve.current.selection = FALSE, network=net.suid, base.url=base.url)
}

#' @export
selectEdgesAdjacentToSelectedNodes <- function(network=NULL, base.url=.defaultBaseUrl){
    suid <- getNetworkSuid(network)
    clearSelection(type='edges',suid,base.url)
    res <- commandsPOST(paste0('network select adjacentEdges="true" nodeList="selected network="',suid,'"'))
    return(res$edges)
}
 
#------------------------------------------------------------------------------------------------------------------------
#' Save a network in one of mulitple file formats 
#' @export
saveNetwork <- function (filename, type='cys',base.url=.defaultBaseUrl) {
    if (!file.exists(filename)){
        type=toupper(type)
        if(type=='CYS'){ # save entire session
            saveSession(filename = filename, base.url=base.url)
        }
        else { #e.g., CX, CYJS, GraphML, NNF, SIF, XGMML (case sensitive)
            if(type=="GRAPHML") #fix case for exceptions
                type = 'GraphML'
            commandsPOST(paste0('network export options=',type,' OutputFile="',filename,'"'),base.url = base.url)
        }
    }else{
        write (sprintf ('choose another filename. File exists: %s', filename), stderr ())
    }
}

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
#' createIgraphFromNetwork('myNetwork')
#' }
#' @seealso createNetworkFromDataFrames, createNetworkFromIgraph

createIgraphFromNetwork <- function(network=NULL, base.url=.defaultBaseUrl, ...){
    suid = getNetworkSuid(network) 
    #get dataframes
    cyedges <- getTableColumns('edge',network=suid,base.url = base.url) 
    cynodes <- getTableColumns('node',network=suid,base.url = base.url) 
    
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
                                    collection.title="myNetworkCollection",
                                    return.graph=FALSE, base.url=.defaultBaseUrl,...) {
    
    #extract dataframes
    igedges = as_data_frame(igraph, what="edges")
    ignodes = as_data_frame(igraph, what="vertices")
    
    #setup columns for Cytoscape import
    ignodes$id <- row.names(ignodes)
    colnames(igedges)[colnames(igedges)=="from"]<-"source"
    colnames(igedges)[colnames(igedges)=="to"]<-"target"
    
    #ship
    createNetworkFromDataFrames(ignodes,igedges,new.title,collection.title,base.url)
}

#------------------------------------------------------------------------------------------------------------------------
#' createGraphFromNetwork
#' 
#' @description Returns the Cytoscape network as a Bioconductor graph.
#' @return A Bioconductor graph object.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{cw <- CytoscapeWindow('network', graph=makeSimpleGraph())
#' displayGraph(cw)
#' layoutNetwork(cw)
#' g.net1 <- createGraphFromNetwork(cw)
#' 
#' cc <- CytoscapeConnection()
#' g.net2 <- createGraphFromNetwork(cc, 'network')
#' 
#' g.net3 <- createGraphFromNetwork('network') #default connection
#' 
#' g.net4 <- createGraphFromNetwork() #current network
#' }
#' @export

createGraphFromNetwork <- function (network= NULL, base.url=.defaultBaseUrl) {

    suid = getNetworkSuid(network)
    title = getNetworkName(network)
    
    if (!is.na(suid)) {
        res = cyrestGET(paste("networks", suid, sep="/"), base.url)
        g = new("graphNEL", edgemode='directed') # create graph object
        g.nodes = res$elements$nodes
        # if there are no nodes in the graph received from Cytoscape, return an empty 'graphNEL' object
        if(length(g.nodes) == 0) {
            write(sprintf("NOTICE in RCy3::createGraphFromNetwork():\n\t returning an empty 'graphNEL'"), stderr())
            return(g)
        }
        
        # else get the node names and add them to the R graph
        node.suid.name.dict = lapply(g.nodes, function(n) { 
            list(name=n$data$name, SUID=n$data$SUID) })
        g.node.names = sapply(node.suid.name.dict, function(n) { n$name })
        write(sprintf("\t received %d NODES from '%s'", length(g.nodes), title), stderr())
        g = graph::addNode(g.node.names, g)
        write(sprintf("\t - added %d nodes to the returned graph\n", length(g.node.names)), stderr())
        
        # GET NODE ATTRIBUTES (if any)
        g = .copyNodeAttributesToGraph(node.suid.name.dict, suid, g)
        
        # Bioconductor's 'graph' edges require the 'edgeType' attribute, so its default value is assigned
        g = .initEdgeAttribute (g, 'edgeType', 'char', 'assoc')
        
        # GET GRAPH EDGES
        g.edges = request.res$elements$edges
        
        if (length(g.edges) > 0) {
            regex = ' *[\\(|\\)] *'
            write(sprintf("\n\t received %d EDGES from '%s'", length(g.edges), title), stderr())
            
            edge.node.suid.name.dict = lapply(g.edges, function(e) {
                list(name=e$data$name, SUID=e$data$SUID) })
            g.edge.names = sapply(edge.node.suid.name.dict, function(e) { e$name })
            edges.tokens = strsplit(g.edge.names, regex)
            source.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[1]))
            target.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[3]))
            edge.types = unlist(lapply(edges.tokens, function(tokens) tokens[2]))
            write(sprintf('\t - adding %d edges to the returned graph\n', length(edges.tokens)), stderr())
            
            tryCatch({
                g = addEdge(source.nodes, target.nodes, g)
                edgeData(g, source.nodes, target.nodes, 'edgeType') = edge.types
                
                # GET EDGE ATTRIBUTES (if any)
                g = .copyEdgeAttributesToGraph(edge.node.suid.name.dict, suid, g)
            },
            error = function(cond){
                write(sprintf("ERROR in RCy3::createGraphFromNetwork(): '%s'", cond), stderr())
                return(NA)
            })
            
            
        }
        
    } else {
        write(sprintf("ERROR in RCy3::createGraphFromNetwork():\n\t there is no graph with name '%s' in Cytoscape", title), stderr())
        return(NA)
    }
    
    return(g)
}
## END createGraphFromNetwork

#------------------------------------------------------------------------------------------------------------------------
#' createNetworkFromGraph
#' 
#' @description Creates a Cytoscape network from a Bioconductor graph.
#' @return A Bioconductor graph object.
#' @author Alexander Pico, Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' }
#' @export
createNetworkFromGraph <- function (graph, title=NULL) {
    #TODO
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
                                        collection.title="MyNetworkCollection",base.url=.defaultBaseUrl,...) {
    
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
    commandsPOST('vizmap apply styles="default"',base.url = base.url)
    
    cat(sprintf("Applying %s layout\n", invisible(commandsPOST('layout get preferred network="current"',base.url = base.url))))
    commandsPOST('layout apply preferred networkSelected="current',base.url = base.url)
    
    return(network.suid)
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
#  https://stackoverflow.com/questions/17046336/here-we-go-again-append-an-element-to-a-list-in-r
#
FastAppendListGlobal <- function(item)
{
    if( .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter == .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size )
        length(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set) <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size * 2
    
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter + 1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[[.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter]] <- item
}

