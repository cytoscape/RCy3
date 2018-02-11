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
#' \link{layoutCopycat}
#' @export
#' @rdname getNodePosition-defunct
getNodePosition <- function (obj, node.names) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: setNodePosition
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \link{layoutCopycat}
#' @export
#' @rdname setNodePosition-defunct
setNodePosition <- function (obj, node.names) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: saveLayout
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \link{layoutCopycat}
#' @export
#' @rdname saveLayout-defunct
saveLayout<- function (obj, filename, timestamp.in.filename=FALSE) {
}

#' DEFUNCT: restoreLayout
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \link{layoutCopycat}
#' @export
#' @rdname restoreLayout-defunct
restoreLayout<-function (obj, filename) {
}

#' DEFUNCT: getGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @usage Use the replacement function instead:
#' \link{createGraphFromNetwork}
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

#' DEFUNCT: setEdgeAttributes
#' 
#' @description This function is defunct and removed in the next releases.
#' @usage No replacement
#' @export
#' @rdname setEdgeAttributes-defunct
setEdgeAttributes<-function(obj, attribute.name){
    .Deprecated("No replacement")
}

#' DEFUNCT: setNodeAttributes
#' 
#' @description This function is defunct and removed in the next releases.
#' @usage No replacement
#' @export
#' @rdname setNodeAttributes-defunct
setNodeAttributes<-function(obj, attribute.name){
    .Deprecated("No replacement")
}

#' DEFUNCT: sendNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage No replacement
#' @export
#' @rdname sendNodes-defunct
sendNodes<-function(obj){
    .Deprecated("No replacement")
}
#' DEFUNCT: sendEdges
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage No replacement
#' @export
#' @rdname sendEdges-defunct
sendEdges<-function(obj){
    .Deprecated("No replacement")
}

# ===================================================================================
#' DEPRECATED: CytoscapeConnection
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement variable instead: base.url
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
#' \link{createNetworkFromGraph}
#' Rely on the default value or overwrite with custom host and port details.
#' @export
#' @importFrom methods new
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
#' \link{cloneNetwork}
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
#' \link{createNetworkFromGraph}
#' @export
#' @rdname createWindow-deprecated
createWindow<-function(obj) {
    .Deprecated("createNetworkFromGraph")
    createNetworkFromGraph(obj@graph)
}
#' DEPRECATED: createWindowFromSelection
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{createSubnetwork}
#' @export
#' @rdname createWindowFromSelection-deprecated
createWindowFromSelection<-function(obj,new.windowTitle,return.graph){
    .Deprecated("createSubnetwork")
    createSubnetwork("selected",subnetwork.name=new.windowTitle)
}
#' DEPRECATED: deleteAllWindows
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{deleteAllNetworks}
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
#' \link{deleteNetwork}
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
#' \link{getNetworkSuid}
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
#' \link{createGraphFromNetwork}
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
#' \link{getNetworkCount}
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
#' \link{getNetworkSuid}
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
#' \link{getNetworkList}
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
#' \link{cytoscapeVersionInfo}
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
#' \link{renameNetwork}
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
#' \link{selectFirstNeighbors}
#' @export
#' @rdname selectFirstNeighborsOfSelectedNodes-deprecated
selectFirstNeighborsOfSelectedNodes<-function (obj) {
    .Deprecated("selectFirstNeighbors")
    selectFirstNeighbors()
}
#' DEPRECATED: sfn
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{selectFirstNeighbors}
#' @export
#' @rdname sfn-deprecated
sfn<-function (obj) {
    selectFirstNeighbors(obj=obj)
}

#' DEPRECATED: saveImage
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{exportImage}
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
#' \link{setVisualStyle}
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
#' \link{getVisualPropertyNames}
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
#' \link{setNetworkCenterBypass}
#' @export
#' @rdname setCenter-deprecated
setCenter<-function(obj, x, y){
    .Deprecated("setNetworkCenterBypass")
    setNetworkCenterBypass(x, y, bypass=FALSE)
}

#' DEPRECATED: getZoom
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{getNetworkZoom}
#' @export
#' @rdname getZoom-deprecated
getZoom<-function(obj){
    .Deprecated("getNetworkZoom")
    getNetworkZoom()
}
#' DEPRECATED: getCenter
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{getNetworkCenter}
#' @export
#' @rdname getCenter-deprecated
getCenter<-function(obj){
    .Deprecated("getNetworkCenter")
    getNetworkCenter()
}

#' DEPRECATED: setZoom
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @usage Use the replacement function instead:
#' \link{setNetworkZoomBypass}
#' @export
#' @rdname setZoom-deprecated
setZoom<-function(obj, new.level){
    .Deprecated("setNetworkZoomBypass")
    setNetworkZoomBypass(new.level, bypass=FALSE)
}




