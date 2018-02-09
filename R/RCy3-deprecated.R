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

#' DEFUNCT: addGraphToGraph
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage No replacement
#' @export
#' @rdname addGraphToGraph-defunct
# This method adds a new graph to an existing graph.
# First the new nodes, then the new edges, then node attributes, then edge attributes
addGraphToGraph <- function (obj, other.graph) {
    .Defunct("No replacement")
}


# ===================================================================================
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

#' DEPRECATED: setCenter
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getVisualPropertyNames}}
#' @export
#' @rdname setCenter-deprecated
setCenter<-function(obj, x, y){
    .Deprecated("setNetworkCenterBypass")
    setNetworkCenterBypass(x, y)
}

#' DEPRECATED: getZoom
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getVisualPropertyNames}}
#' @export
#' @rdname getZoom-deprecated
getZoom<-function(obj){
    .Deprecated("getNetworkZoomBypass")
    getNetworkZoomBypass()
}
#' DEPRECATED: getCenter
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getVisualPropertyNames}}
#' @export
#' @rdname getCenter-deprecated
getCenter<-function(obj){
    .Deprecated("getNetworkCenterBypass")
    getNetworkCenterBypass()
}

#' DEPRECATED: setZoom
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \code{\link[RCy3]{getVisualPropertyNames}}
#' @export
#' @rdname setZoom-deprecated
setZoom<-function(obj, new.level){
    .Deprecated("setNetworkZoomBypass")
    setNetworkZoomBypass(new.level)
}




