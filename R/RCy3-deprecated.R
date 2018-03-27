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
#' Use the replacement function instead: 
#' \link{layoutCopycat}
#' @return None
#' @aliases getNodePositiondefunct
#' @usage getNodePositiondefunct
#' @export
#' @rdname getNodePosition-defunct
getNodePosition <- function (obj, node.names) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: setNodePosition
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{layoutCopycat}
#' @return None
#' @aliases setNodePositiondefunct
#' @usage setNodePositiondefunct
#' @export
#' @rdname setNodePosition-defunct
setNodePosition <- function (obj, node.names) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: saveLayout
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{layoutCopycat}
#' @return None
#' @aliases saveLayoutdefunct
#' @usage saveLayoutdefunct
#' @export
#' @rdname saveLayout-defunct
saveLayout<- function (obj, filename, timestamp.in.filename=FALSE) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: restoreLayout
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{layoutCopycat}
#' @return None
#' @aliases restoreLayoutdefunct
#' @usage restoreLayoutdefunct
#' @export
#' @rdname restoreLayout-defunct
restoreLayout<-function (obj, filename) {
    .Defunct("layoutCopycat")
}

#' DEFUNCT: getGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{createGraphFromNetwork}
#' @return None
#' @aliases restoreLayoutdefunct
#' @usage restoreLayoutdefunct
#' @export
#' @rdname restoreLayout-defunct
getGraph <- function(obj) {
    .Defunct("createGraphFromNetwork")
}

#' DEFUNCT: addGraphToGraph
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @return None
#' @aliases addGraphToGraphdefunct
#' @usage addGraphToGraphdefunct
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
#' @return None
#' @aliases setEdgeAttributesdefunct
#' @usage setEdgeAttributesdefunct
#' @export
#' @rdname setEdgeAttributes-defunct
setEdgeAttributes<-function(obj, attribute.name){
    .Deprecated("No replacement")
}

#' DEFUNCT: setNodeAttributes
#' 
#' @description This function is defunct and removed in the next releases.
#' @return None
#' @aliases setNodeAttributesdefunct
#' @usage setNodeAttributesdefunct
#' @export
#' @rdname setNodeAttributes-defunct
setNodeAttributes<-function(obj, attribute.name){
    .Deprecated("No replacement")
}

#' DEFUNCT: sendNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @return None
#' @aliases sendNodesdefunct
#' @usage sendNodesdefunct
#' @export
#' @rdname sendNodes-defunct
sendNodes<-function(obj){
    .Deprecated("No replacement")
}
#' DEFUNCT: sendEdges
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' @return None
#' @aliases sendEdgesdefunct
#' @usage sendEdgesdefunct
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
#' Use the replacement variable instead: base.url
#' Rely on the default value or overwrite with custom host and port details.
#' @return None
#' @aliases CytoscapeConnectiondeprecated
#' @usage CytoscapeConnectiondeprecated
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
#' Use the replacement function instead: 
#' \link{createNetworkFromGraph}
#' Rely on the default value or overwrite with custom host and port details.
#' @return Network SUID
#' @aliases CytoscapeWindowdeprecated
#' @usage CytoscapeWindowdeprecated
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
#' Use the replacement function instead: 
#' \link{cloneNetwork}
#' @return Network SUID
#' @aliases copyCytoscapeNetworkdeprecated
#' @usage copyCytoscapeNetworkdeprecated
#' @export
#' @rdname copyCytoscapeNetwork-deprecated
copyCytoscapeNetwork<-function(obj,new.title,return.graph = FALSE) {
    .Deprecated("cloneNetwork")
    cloneNetwork()
}
#' DEPRECATED: createWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{createNetworkFromGraph}
#' @return Network SUID
#' @aliases createWindowdeprecated
#' @usage createWindowdeprecated
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
#' Use the replacement function instead: 
#' \link{createSubnetwork}
#' @return Network SUID
#' @aliases createWindowFromSelectiondeprecated
#' @usage createWindowFromSelectiondeprecated
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
#' Use the replacement function instead: 
#' \link{deleteAllNetworks}
#' @return None
#' @aliases deleteAllWindowsdeprecated
#' @usage deleteAllWindowsdeprecated
#' @export
#' @rdname deleteAllWindows-deprecated
deleteAllWindows<-function(obj){
    .Deprecated("deleteAllNetworks(obj)")
    deleteAllNetworks()
}
#' DEPRECATED: deleteWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteNetwork}
#' @return None
#' @aliases deleteWindowdeprecated
#' @usage deleteWindowdeprecated
#' @export
#' @rdname deleteWindow-deprecated
deleteWindow<-function(obj,window.title){
    .Deprecated("deleteNetwork(obj,title)")
    deleteNetwork(window.title)
}
#' DEPRECATED: existing.CytoscapeWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkSuid}
#' @return Network SUID
#' @aliases existing.CytoscapeWindowdeprecated
#' @usage existing.CytoscapeWindowdeprecated
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
#' Use the replacement function instead: 
#' \link{createGraphFromNetwork}
#' @return Network SUID
#' @aliases getGraphFromCyWindowdeprecated
#' @usage getGraphFromCyWindowdeprecated
#' @export
#' @rdname getGraphFromCyWindow-deprecated
getGraphFromCyWindow<-function(obj,window.title){
    .Deprecated("createGraphFromNetwork")
    createGraphFromNetwork(network=window.title)
}
#' DEPRECATED: getWindowCount
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkCount}
#' @return Integer
#' @aliases getWindowCountdeprecated
#' @usage getWindowCountdeprecated
#' @export
#' @rdname getWindowCount-deprecated
getWindowCount<-function(obj){
    .Deprecated("getNetworkCount")
    getNetworkCount()
}
#' DEPRECATED: getWindowID
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkSuid}
#' @return Network SUID
#' @aliases getWindowIDdeprecated
#' @usage getWindowIDdeprecated
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
#' Use the replacement function instead: 
#' \link{getNetworkList}
#' @return List of networks
#' @aliases getWindowListdeprecated
#' @usage getWindowListdeprecated
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
#' Use the replacement function instead: 
#' \link{cytoscapeVersionInfo}
#' @return Version information
#' @aliases pluginVersiondeprecated
#' @usage pluginVersiondeprecated
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
#' Use the replacement function instead: 
#' \link{renameNetwork}
#' @return None
#' @aliases renameCytoscapeNetworkdeprecated
#' @usage renameCytoscapeNetworkdeprecated
#' @export
#' @rdname renameCytoscapeNetwork-deprecated
renameCytoscapeNetwork<-function(obj, new.title, return.graph = FALSE) {
    .Deprecated("renameNetwork")
    renameNetwork(title=new.title)
}
#' DEPRECATED: selectFirstNeighborsOfSelectedNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{selectFirstNeighbors}
#' @return List of node SUIDs
#' @aliases selectFirstNeighborsOfSelectedNodesdeprecated
#' @usage selectFirstNeighborsOfSelectedNodesdeprecated
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
#' Use the replacement function instead: 
#' \link{selectFirstNeighbors}
#' @return List of node SUIDs
#' @aliases sfndeprecated
#' @usage sfndeprecated
#' @export
#' @rdname sfn-deprecated
sfn<-function (obj) {
    .Deprecated("selectFirstNeighbors")
    selectFirstNeighbors()
}

#' DEPRECATED: saveImage
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{exportImage}
#' @return None
#' @aliases saveImagedeprecated
#' @usage saveImagedeprecated
#' @export
#' @rdname saveImage-deprecated
saveImage<-function(obj, file.name, image.type, h = 600){
    .Deprecated("exportImage")
    exportImage(filename=file.name, type=image.type ,height=h)
}


#' DEPRECATED: redraw
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{setVisualStyle}
#' @return None
#' @aliases redrawdeprecated
#' @usage redrawdeprecated
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
#' Use the replacement function instead: 
#' \link{getVisualPropertyNames}
#' @return List of property names
#' @aliases getDirectlyModifiableVisualPropertiesdeprecated
#' @usage getDirectlyModifiableVisualPropertiesdeprecated
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
#' Use the replacement function instead: 
#' \link{setNetworkCenterBypass}
#' @return None
#' @aliases setCenterdeprecated
#' @usage setCenterdeprecated
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
#' Use the replacement function instead: 
#' \link{getNetworkZoom}
#' @return Network zoom factor
#' @aliases getZoomdeprecated
#' @usage getZoomdeprecated
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
#' Use the replacement function instead: 
#' \link{getNetworkCenter}
#' @return Network center
#' @aliases getCenterdeprecated
#' @usage getCenterdeprecated
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
#' Use the replacement function instead: 
#' \link{setNetworkZoomBypass}
#' @return None
#' @aliases setZoom_deprecated
#' @usage setZoom_deprecated
#' @export
#' @rdname setZoom-deprecated
setZoom<-function(obj, new.level){
    .Deprecated("setNetworkZoomBypass")
    setNetworkZoomBypass(new.level, bypass=FALSE)
}
