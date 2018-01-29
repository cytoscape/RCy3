#' @include CytoscapeWindowClass.R CytoscapeConnectionClass.R Internal.R
NULL

# ======== Deprecation Process =======================================================
# 1. Move deprecated functions here and update docs
# 2. Change .Deprecated functions to .Defunct on next release
# 3. Remove .Defunct functions on next release
# ===================================================================================

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
    .Deprecated("createNetworkFromSelection(obj, new.title, return.graph, exclude.edges=FALSE)")
    createNetworkFromSelection(obj=obj, new.title=new.windowTitle, return.graph=return.graph, exclude.edges=FALSE)
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
#' \code{\link[RCy3]{CytoscapeWindowFromNetwork}}
#' @export
#' @rdname existing.CytoscapeWindow-deprecated
existing.CytoscapeWindow<-function(title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE){
    .Deprecated("CytoscapeWindowFromNetwork(title,host,port,return.graph")
    cc<-CytoscapeConnection(host=host,port=port)
    CytoscapeWindowFromNetwork(cc,title = title,return.graph = copy.graph.from.cytoscape.to.R)
}
#' DEPRECATED: getGraphFromCyWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getGraphFromNetwork}}
#' @export
#' @rdname getGraphFromCyWindow-deprecated
getGraphFromCyWindow<-function(obj,window.title){
    .Deprecated("getGraphFromNetwork(obj,title)")
    getGraphFromNetwork(obj=obj,title=window.title)
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
    .Deprecated("getNetworkSuid(obj,title")
    getNetworkSuid(obj=obj,title=window.title)
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
    getNetworkList(obj=obj)
}
#' DEPRECATED: pluginVersion
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{apiVersion}}
#' @export
#' @rdname pluginVersion-deprecated
pluginVersion<-function(obj){
    .Deprecated("apiVersion(obj)")
    apiVersion(obj=obj)
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
    .Deprecated("No replacement")
    loc.obj <- obj
    # RCy3 keeps a dictionary of the network nodes 
    # the below vector stores the indices of the newly added nodes in this dictionary
    new.node.indices = .addNodes(loc.obj, other.graph)
    
    # RCy3 keeps a dictionary of the network edges
    # the below vector stores the indices of the newly added edges to this dictionary
    new.edge.indices = .addEdges(loc.obj, other.graph)
    
    node.attribute.names = noa.names(other.graph)
    
    for (attribute.name in node.attribute.names) {
        printf('sending noa %s', attribute.name)
        .sendNodeAttributesForGraph(loc.obj, other.graph, attribute.name, new.node.indices)
    }
    
    edge.attribute.names = eda.names(other.graph)
    for (attribute.name in edge.attribute.names) {
        printf('sending eda %s', attribute.name)
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
        
        resource.uri = paste(loc.obj@uri, obj@api, "networks", net.SUID, "nodes", sep="/")
        new.nodes.JSON = toJSON(new.nodes)
        
        request.res = POST(url=resource.uri, body=new.nodes.JSON, encode="json")
        new.node.SUIDs = unname(fromJSON(rawToChar(request.res$content)))
        
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


