#### Deprecation Process ####
# 1. Move deprecated functions here and update docs
# 2. Change .Deprecated functions to .Defunct on next release
# 3. Remove .Defunct functions on next release
#############################

#' Deprecated: copyCytoscapeNetwork
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
#' Deprecated: createWindow
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
#' Deprecated: createWindowFromSelection
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
#' Deprecated: deleteAllWindows
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
#' Deprecated: deleteWindow
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
#' Deprecated: existing.CytoscapeWindow
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
#' Deprecated: getGraphFromCyWindow
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
#' Deprecated: getWindowCount
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
#' Deprecated: getWindowID
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
#' Deprecated: getWindowList
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
#' Deprecated: pluginVersion
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
#' Deprecated: renameCytoscapeNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{renameNetwork}}
#' @export
#' @rdname renameCytoscapeNetwork-deprecated
renameCytoscapeNetwork<-function(obj, new.title, return.graph = FALSE) {
    .Deprecated("renameNetwork")
    renameNetwork(obj=obj,new.title=new.title,return.graph=return.graph)
}
#' Deprecated: selectFirstNeighborsOfSelectedNodes
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
#' Deprecated: sfn
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
#' Deprecated: sendNodes
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
#' Deprecated: sendEdges
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
#' Deprecated: setNodeAttributes
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
#' Deprecated: setEdgeAttributes
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

