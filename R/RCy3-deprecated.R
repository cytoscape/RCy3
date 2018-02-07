#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R
NULL

# ======== Deprecation Process =======================================================
# 1. Move deprecated functions here and update docs
# 2. Change .Deprecated functions to .Defunct on next release
# 3. Remove .Defunct functions on next release
# ===================================================================================

# ======== Deprecation Notes  =======================================================
# # Extract base.url string from CytoscapeConnection and CytoscapeWindow objects 
# base.url <- paste(obj@uri, obj@api, sep="/")
#
# # Extract network title or suid from CytoscapeWindow objects
# title = obj@title
# suid = obj@suid
# ===================================================================================

#' DEFUNCT: getNodePosition
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \code{layoutCopycat}
#' @export
#' @rdname getNodePosition-defunct
getNodePosition <- function (obj, node.names) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: setNodePosition
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \code{layoutCopycat}
#' @export
#' @rdname setNodePosition-defunct
setNodePosition <- function (obj, node.names) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: saveLayout
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \code{layoutCopycat}
#' @export
#' @rdname saveLayout-defunct
saveLayout<- function (obj, filename, timestamp.in.filename=FALSE) {
}

#' DEFUNCT: restoreLayout
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \code{layoutCopycat}
#' @export
#' @rdname restoreLayout-defunct
restoreLayout<-function (obj, filename) {
}

#' DEFUNCT: getGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \code{createGraphFromNetwork}
#' @export
#' @rdname restoreLayout-defunct
getGraph <- function(obj) {
    .Defunct("createGraphFromNetwork")
}

# --------------------------------------------------------------------------
#' DEPRECATED: CytoscapeConnection
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement variable instead:
#' \code{base.url}
#' Rely on the default value or overwrite with custom host and port details.
#' @export
#' @rdname CytoscapeConnection-deprecated
CytoscapeConnection<-function(host, port){
    .Deprecated("default value of base.url or overwrite with custom host and port details")
    return(.defaultBaseUrl)
}

# --------------------------------------------------------------------------
#' DEPRECATED: CytoscapeWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{createNetworkFromGraph}
#' Rely on the default value or overwrite with custom host and port details.
#' @export
#' @rdname CytoscapeWindow-deprecated
CytoscapeWindow<-function(title, graph=new('graphNEL', edgemode='directed'), uri="http://localhost:1234", api="v1",
                          create.window=TRUE, overwriteWindow=FALSE, collectTimings=FALSE){
    .Deprecated("createNetworkFromGraph")
    createNetworkFromGraph()
}

#' DEPRECATED: copyCytoscapeNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{cloneNetwork}}
#' @export
#' @rdname copyCytoscapeNetwork-deprecated
copyCytoscapeNetwork<-function(obj,new.title,return.graph = FALSE) {
    .Deprecated("cloneNetwork")
    cloneNetwork(obj=obj,new.title=new.title,return.graph=return.graph)
}
#' DEPRECATED: createWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{sendNetworkFromGraph}}
#' @export
#' @rdname createWindow-deprecated
createWindow<-function(obj) {
    .Deprecated("sendNetworkFromGraph")
    sendNetworkFromGraph(obj=obj)
}
#' DEPRECATED: createWindowFromSelection
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{createNetworkFromSelection}}
#' @export
#' @rdname createWindowFromSelection-deprecated
createWindowFromSelection<-function(obj,new.windowTitle,return.graph){
    .Deprecated("createSubnetwork(obj, new.title, return.graph, exclude.edges=FALSE)")
    createSubnetwork(nodes="selected",subnetwork.title=new.windowTitle)
}
#' DEPRECATED: deleteAllWindows
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{deleteAllNetworks}}
#' @export
#' @rdname deleteAllWindows-deprecated
deleteAllWindows<-function(obj){
    .Deprecated("deleteAllNetworks(obj)")
    deleteAllNetworks(obj=obj)
}
#' DEPRECATED: deleteWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{deleteNetwork}}
#' @export
#' @rdname deleteWindow-deprecated
deleteWindow<-function(obj,window.title){
    .Deprecated("deleteNetwork(obj,title)")
    deleteNetwork(obj=obj,title=window.title)
}
#' DEPRECATED: existing.CytoscapeWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getNetworkSuid}}
#' @export
#' @rdname existing.CytoscapeWindow-deprecated
existing.CytoscapeWindow<-function(title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE){
    .Deprecated("getNetworkSuid(title,host,port,return.graph")
    getNetworkSuid(title = title)
}
#' DEPRECATED: getGraphFromCyWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{createGraphFromNetwork}}
#' @export
#' @rdname getGraphFromCyWindow-deprecated
getGraphFromCyWindow<-function(obj,window.title){
    .Deprecated("createGraphFromNetwork")
    createGraphFromNetwork(title=window.title)
}
#' DEPRECATED: getWindowCount
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getNetworkCount}}
#' @export
#' @rdname getWindowCount-deprecated
getWindowCount<-function(obj){
    .Deprecated("getNetworkCount")
    getNetworkCount(obj=obj)
}
#' DEPRECATED: getWindowID
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getNetworkSuid}}
#' @export
#' @rdname getWindowID-deprecated
getWindowID<-function(obj,window.title){
    .Deprecated("getNetworkSuid")
    getNetworkSuid(title=window.title)
}
#' DEPRECATED: getWindowList
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getNetworkList}}
#' @export
#' @rdname getWindowList-deprecated
getWindowList<-function(obj){
    .Deprecated("getNetworkList")
    getNetworkList()
}
#' DEPRECATED: pluginVersion
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{cytoscapeVersionInfo}}
#' @export
#' @rdname pluginVersion-deprecated
pluginVersion<-function(obj){
    .Deprecated("getVersionInfo")
    cytoscapeVersionInfo()[['apiVersion']]
}
#' DEPRECATED: renameCytoscapeNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{renameNetwork}}
#' @export
#' @rdname renameCytoscapeNetwork-deprecated
renameCytoscapeNetwork<-function(obj, new.title, return.graph = FALSE) {
    .Deprecated("renameNetwork")
    renameNetwork(obj=obj,old.title=NA,new.title=new.title,return.graph=return.graph)
}
#' DEPRECATED: selectFirstNeighborsOfSelectedNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{selectFirstNeighbors}}
#' @export
#' @rdname selectFirstNeighborsOfSelectedNodes-deprecated
selectFirstNeighborsOfSelectedNodes<-function (obj) {
    selectFirstNeighbors(obj=obj)
}
#' DEPRECATED: sfn
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{selectFirstNeighbors}}
#' @export
#' @rdname sfn-deprecated
sfn<-function (obj) {
    selectFirstNeighbors(obj=obj)
}
#' DEPRECATED: sendNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{sendNodesFromGraph}}
#' @export
#' @rdname sendNodes-deprecated
sendNodes<-function(obj){
    .Deprecated("sendNodesFromGraph(obj)")
    sendNodesFromGraph(obj=obj)
}
#' DEPRECATED: sendEdges
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{sendEdgesFromGraph}}
#' @export
#' @rdname sendEdges-deprecated
sendEdges<-function(obj){
    .Deprecated("sendEdgesFromGraph(obj)")
    sendEdgesFromGraph(obj=obj)
}
#' DEPRECATED: setNodeAttributes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{sendNodeAttributesFromGraph}}
#' @export
#' @rdname setNodeAttributes-deprecated
setNodeAttributes<-function(obj, attribute.name){
    .Deprecated("sendNodeAttributesFromGraph(obj)")
    sendNodeAttributesFromGraph(obj=obj, attribute.name=attribute.name)
}
#' DEPRECATED: setEdgeAttributes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{sendEdgeAttributesFromGraph}}
#' @export
#' @rdname setEdgeAttributes-deprecated
setEdgeAttributes<-function(obj, attribute.name){
    .Deprecated("sendEdgeAttributesFromGraph(obj)")
    sendEdgeAttributesFromGraph(obj=obj, attribute.name=attribute.name)
}

#' DEPRECATED: saveImage
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{exportImage}}
#' @export
#' @rdname saveImage-deprecated
saveImage<-function(obj, file.name, image.type, h = 600){
    .Deprecated("exportImage")
    exportImage(filename, type=image.type ,height=h)
}


#' DEPRECATED: redraw
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{setVisualStyle}}
#' @export
#' @rdname redraw-deprecated
redraw<-function(obj){
    .Deprecated("setVisualStyle")
    setVisualStyle(style.name='default')
}

#' DEPRECATED: getDirectlyModifiableVisualProperties
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getVisualPropertyNames}}
#' @export
#' @rdname getDirectlyModifiableVisualProperties-deprecated
getDirectlyModifiableVisualProperties<-function(obj, style.name){
    .Deprecated("getVisualPropertyNames")
    getVisualPropertyNames()
}


#------------------------------------------------------------------------------------------------------------------------
#' DEPRECATED: addGraphToGraph
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage No replacement
#' @export
#' @rdname addGraphToGraph-deprecated
# This method adds a new graph to an existing graph.
# First the new nodes, then the new edges, then node attributes, then edge attributes
addGraphToGraph <- function (obj, other.graph) {
    .Deprecated("addGraphToNetwork")
    addGraphToNetwork(graph=other.graph, network=obj.suid, base.url=.defaultBaseUrl)
}
addGraphToNetwork <- function(graph, network=NULL, base.url=.defaultBaseUrl){
    net.suid <- getNetworkSuid(network)
    new.node.indices = .addNodes(graph, net.suid, base.url)
    new.edge.indices = .addEdges(graph, net.suid, base.url)
    
    node.attribute.names = noa.names(graph)
    for (attribute.name in node.attribute.names) {
        sprintf('sending noa %s', attribute.name)
        .sendNodeAttributesForGraph(loc.obj, other.graph, attribute.name, new.node.indices)
    }
    
    edge.attribute.names = eda.names(other.graph)
    for (attribute.name in edge.attribute.names) {
        sprintf('sending eda %s', attribute.name)
        .sendEdgeAttributesForGraph(loc.obj, other.graph, attribute.name, new.edge.indices)
    }
    
    # needed for 'pass-by-reference' R functionality
    eval.parent(substitute(obj <- loc.obj))
} 
.addNodes<-function (obj, other.graph) {
    loc.obj <- obj
    if(length(nodes(other.graph)) == 0) {
        write("NOTICE in RCy3::.addNodes():\n\t no nodes in other.graph >> function returns", stderr())
        return()
    }
    # new.nodes = setdiff(nodes(other.graph), getAllNodes(loc.obj))
    new.node.indices = which(!nodes(other.graph) %in% getAllNodes(loc.obj))
    
    new.nodes = nodes(other.graph)[new.node.indices]
    
    if(length(new.node.indices) > 0) {
        net.SUID = as.character(loc.obj@suid)

        res = cyrestPOST(paste("networks", net.SUID, "nodes", sep="/"), 
                                 body=new.nodes.JSON, 
                                 base.url = base.url)
        new.node.SUIDs = unname(res)
        
        for(i in 1:length(new.node.SUIDs)) {
            loc.obj@node.suid.name.dict[[length(loc.obj@node.suid.name.dict)+1]] = new.node.SUIDs[[i]]
        }
    } else {
        write(sprintf("NOTICE in RCy3::.addNodes():\n\t all %d nodes already exist in Cytoscape - nothing new to add >> function returns", length(nodes(other.graph))), stderr())
        return()
    }
    # needed for 'pass-by-reference' R functionality
    eval.parent(substitute(obj <- loc.obj))
    
    return(new.node.indices)
}
.addEdges<-function (obj, other.graph) {
    loc.obj <- obj
    net.SUID = as.character(loc.obj@suid)
    
    if(length(edgeNames(other.graph)) == 0) {
        write("NOTICE in RCy3::.addEdges():\n\t no edges in graph >> function returns", stderr())
        return()
    }
    
    if(is.classic.graph(other.graph)) {
        tbl.edges = .classicGraphToNodePairTable(other.graph)
    } else if(is.multiGraph(other.graph)) {
        tbl.edges = .multiGraphToNodePairTable(other.graph)
    }
    # get the 'other.graph' edge names
    other.graph.edge.names = unname(cy2.edge.names(other.graph))
    
    cytoscape.existing.edge.names = 
        sapply(loc.obj@edge.node.suid.name.dict, function(e) {return(e$name)})
    new.edge.indices = which(!other.graph.edge.names %in% cytoscape.existing.edge.names)
    
    if(length(new.edge.indices) > 0) {
        # source nodes vector
        source.nodes = tbl.edges$source[new.edge.indices]
        # target nodes vector
        target.nodes = tbl.edges$target[new.edge.indices]
        # edge types vector
        edge.type = tbl.edges$edgeType[new.edge.indices]
        directed = rep(TRUE, length(source.nodes))
        
        # get the SUIDs of the source nodes for the new edges
        source.node.SUIDs = .nodeNameToNodeSUID(loc.obj, source.nodes)
        # get the SUIDs of the target nodes for the new edges
        target.node.SUIDs = .nodeNameToNodeSUID(loc.obj, target.nodes)
        
        # format the new edges data for sending to Cytoscape
        edge.tbl.records = 
            apply(cbind(source.node.SUIDs, target.node.SUIDs, directed, edge.type), MARGIN=1,
                  FUN=function(r) {list(source=unname(r[[1]]), target=unname(r[[2]]), directed=unname(r[[3]]), interaction=unname(r[[4]]))})
        edge.tbl.records.JSON = toJSON(edge.tbl.records)
        resource.uri = paste(loc.obj@uri, loc.obj@api, "networks", net.SUID, "edges", sep="/")
        request.res = POST(url=resource.uri, body=edge.tbl.records.JSON, encode="json")
        
        # request.res.edge.SUIDs contains 
        # [edge.SUID, source.node.SUID, targetn.node.SUID] for each edge
        request.res.edge.data = fromJSON(rawToChar(request.res$content))
        
        new.edge.names = cy2.edge.names(other.graph)[new.edge.indices]
        # ctreates matrix of the format : 
        # note: column 1 contains edge.SUIDs, and columns 3 & 4 contain node.SUIDs
        #      [,1]   [,2]                     [,3]   [,4]
        # [1,] "412"  "A (phosphorylates) B"   "413"  "404"
        # [2,] "406"  "B (synthetic lethal C"  "407"  "408"
        # [3,] "407"  "C (undefined) A"        "408"  "406"
        edge.names.tbl.records = 
            apply(unname(cbind(unname(t(sapply(request.res.edge.data, unlist))), new.edge.names)), 
                  MARGIN=1, 
                  FUN=function(r) {list(SUID=as.numeric(unname(r[[1]])), value=unname(r[[4]]), 
                                        source.node=as.numeric(unname(r[[2]])), 
                                        target.node=as.numeric(unname(r[[3]])))})
        # CREATES DICT ENTRIES for the new edges in the following format :
        # [edge.SUID, edge.name, source.node.SUID, target.node.SUID]
        for(i in 1:length(edge.names.tbl.records)) {
            loc.obj@edge.node.suid.name.dict[[length(loc.obj@edge.node.suid.name.dict)+1]] = 
                list(SUID=edge.names.tbl.records[[i]]$SUID, name=edge.names.tbl.records[[i]]$value, 
                     source.node=edge.names.tbl.records[[i]]$source.node, 
                     target.node=edge.names.tbl.records[[i]]$target.node)
        }
        
        # invisible(request.res)
    } else {
        write(sprintf("NOTICE in RCy3::.addEdges():\n\t all %d edges already exists in Cytoscape - nothing new to add >> function returns", length(other.graph.edge.names)), stderr())
        return()
    }
    
    eval.parent(substitute(obj <- loc.obj))
    
    return(new.edge.indices)
}# END addGraphToGraph


