# ======== Deprecation Process =======================================================
# 1. Move deprecated functions here and update docs
# 2. Change .Deprecated functions to .Defunct on next release
# 3. Remove .Defunct functions on next release
# ===================================================================================

# ======== DEFUNCT ==================================================================
#' DEFUNCT: CytoscapeConnection
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement variable instead: base.url
#' Rely on the default value or overwrite with custom host and port details.
#' @return None
#' @aliases CytoscapeConnectiondeprecated
#' @usage CytoscapeConnectiondeprecated
#' @export
#' @rdname CytoscapeConnection-defunct
CytoscapeConnection<-function(host, port){
    .Defunct("default value of base.url or overwrite with custom host and port details")
}

# --------------------------------------------------------------------------
#' DEFUNCT: CytoscapeWindow
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
#' @rdname CytoscapeWindow-defunct
CytoscapeWindow<-function(title, graph=new('graphNEL', edgemode='directed'), uri="http://localhost:1234", api="v1",
                          create.window=TRUE, overwriteWindow=FALSE, collectTimings=FALSE){
    .Defunct("createNetworkFromGraph")
}

#' DEFUNCT: copyCytoscapeNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{cloneNetwork}
#' @return Network SUID
#' @aliases copyCytoscapeNetworkdeprecated
#' @usage copyCytoscapeNetworkdeprecated
#' @export
#' @rdname copyCytoscapeNetwork-defunct
copyCytoscapeNetwork<-function(obj,new.title,return.graph = FALSE) {
    .Defunct("cloneNetwork")
}
#' DEFUNCT: createWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{createNetworkFromGraph}
#' @return Network SUID
#' @aliases createWindowdeprecated
#' @usage createWindowdeprecated
#' @export
#' @rdname createWindow-defunct
createWindow<-function(obj) {
    .Defunct("createNetworkFromGraph")
}
#' DEFUNCT: createWindowFromSelection
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{createSubnetwork}
#' @return Network SUID
#' @aliases createWindowFromSelectiondeprecated
#' @usage createWindowFromSelectiondeprecated
#' @export
#' @rdname createWindowFromSelection-defunct
createWindowFromSelection<-function(obj,new.windowTitle,return.graph){
    .Defunct("createSubnetwork")
}
#' DEFUNCT: deleteAllWindows
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteAllNetworks}
#' @return None
#' @aliases deleteAllWindowsdeprecated
#' @usage deleteAllWindowsdeprecated
#' @export
#' @rdname deleteAllWindows-defunct
deleteAllWindows<-function(obj){
    .Defunct("deleteAllNetworks(obj)")
}
#' DEFUNCT: deleteWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteNetwork}
#' @return None
#' @aliases deleteWindowdeprecated
#' @usage deleteWindowdeprecated
#' @export
#' @rdname deleteWindow-defunct
deleteWindow<-function(obj,window.title){
    .Defunct("deleteNetwork(obj,title)")
}
#' DEFUNCT: existing.CytoscapeWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkSuid}
#' @return Network SUID
#' @aliases existing.CytoscapeWindowdeprecated
#' @usage existing.CytoscapeWindowdeprecated
#' @export
#' @rdname existing.CytoscapeWindow-defunct
existing.CytoscapeWindow<-function(title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE){
    .Defunct("getNetworkSuid(title,host,port,return.graph")
}
#' DEFUNCT: getGraphFromCyWindow
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{createGraphFromNetwork}
#' @return Network SUID
#' @aliases getGraphFromCyWindowdeprecated
#' @usage getGraphFromCyWindowdeprecated
#' @export
#' @rdname getGraphFromCyWindow-defunct
getGraphFromCyWindow<-function(obj,window.title){
    .Defunct("createGraphFromNetwork")
}
#' DEFUNCT: getWindowCount
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkCount}
#' @return Integer
#' @aliases getWindowCountdeprecated
#' @usage getWindowCountdeprecated
#' @export
#' @rdname getWindowCount-defunct
getWindowCount<-function(obj){
    .Defunct("getNetworkCount")
}
#' DEFUNCT: getWindowID
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkSuid}
#' @return Network SUID
#' @aliases getWindowIDdeprecated
#' @usage getWindowIDdeprecated
#' @export
#' @rdname getWindowID-defunct
getWindowID<-function(obj,window.title){
    .Defunct("getNetworkSuid")
}
#' DEFUNCT: getWindowList
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkList}
#' @return List of networks
#' @aliases getWindowListdeprecated
#' @usage getWindowListdeprecated
#' @export
#' @rdname getWindowList-defunct
getWindowList<-function(obj){
    .Defunct("getNetworkList")
}
#' DEFUNCT: pluginVersion
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{cytoscapeVersionInfo}
#' @return Version information
#' @aliases pluginVersiondeprecated
#' @usage pluginVersiondeprecated
#' @export
#' @rdname pluginVersion-defunct
pluginVersion<-function(obj){
    .Defunct("getVersionInfo")
}
#' DEFUNCT: renameCytoscapeNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{renameNetwork}
#' @return None
#' @aliases renameCytoscapeNetworkdeprecated
#' @usage renameCytoscapeNetworkdeprecated
#' @export
#' @rdname renameCytoscapeNetwork-defunct
renameCytoscapeNetwork<-function(obj, new.title, return.graph = FALSE) {
    .Defunct("renameNetwork")
}
#' DEFUNCT: selectFirstNeighborsOfSelectedNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{selectFirstNeighbors}
#' @return List of node SUIDs
#' @aliases selectFirstNeighborsOfSelectedNodesdeprecated
#' @usage selectFirstNeighborsOfSelectedNodesdeprecated
#' @export
#' @rdname selectFirstNeighborsOfSelectedNodes-defunct
selectFirstNeighborsOfSelectedNodes<-function (obj) {
    .Defunct("selectFirstNeighbors")
}
#' DEFUNCT: sfn
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{selectFirstNeighbors}
#' @return List of node SUIDs
#' @aliases sfndeprecated
#' @usage sfndeprecated
#' @export
#' @rdname sfn-defunct
sfn<-function (obj) {
    .Defunct("selectFirstNeighbors")
}

#' DEFUNCT: saveImage
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{exportImage}
#' @return None
#' @aliases saveImagedeprecated
#' @usage saveImagedeprecated
#' @export
#' @rdname saveImage-defunct
saveImage<-function(obj, file.name, image.type, h = 600){
    .Defunct("exportImage")
}


#' DEFUNCT: redraw
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{setVisualStyle}
#' @return None
#' @aliases redrawdeprecated
#' @usage redrawdeprecated
#' @export
#' @rdname redraw-defunct
redraw<-function(obj){
    .Defunct("setVisualStyle")
}

#' DEFUNCT: getDirectlyModifiableVisualProperties
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getVisualPropertyNames}
#' @return List of property names
#' @aliases getDirectlyModifiableVisualPropertiesdeprecated
#' @usage getDirectlyModifiableVisualPropertiesdeprecated
#' @export
#' @rdname getDirectlyModifiableVisualProperties-defunct
getDirectlyModifiableVisualProperties<-function(obj, style.name){
    .Defunct("getVisualPropertyNames")
}

#' DEFUNCT: setCenter
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{setNetworkCenterBypass}
#' @return None
#' @aliases setCenterdeprecated
#' @usage setCenterdeprecated
#' @export
#' @rdname setCenter-defunct
setCenter<-function(obj, x, y){
    .Defunct("setNetworkCenterBypass")
}

#' DEFUNCT: getZoom
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkZoom}
#' @return Network zoom factor
#' @aliases getZoomdeprecated
#' @usage getZoomdeprecated
#' @export
#' @rdname getZoom-defunct
getZoom<-function(obj){
    .Defunct("getNetworkZoom")
}
#' DEFUNCT: getCenter
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getNetworkCenter}
#' @return Network center
#' @aliases getCenterdeprecated
#' @usage getCenterdeprecated
#' @export
#' @rdname getCenter-defunct
getCenter<-function(obj){
    .Defunct("getNetworkCenter")
}

#' DEFUNCT: setZoom
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{setNetworkZoomBypass}
#' @return None
#' @aliases setZoom_deprecated
#' @usage setZoom_deprecated
#' @export
#' @rdname setZoom-defunct
setZoom<-function(obj, new.level){
    .Defunct("setNetworkZoomBypass")
}

#' DEFUNCT: fitSelectedContent
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{fitContent}
#' @return None
#' @aliases fitSelectedContent_deprecated
#' @usage fitSelectedContent_deprecated
#' @export
#' @rdname fitSelectedContent-defunct
fitSelectedContent<-function(obj){
    .Defunct("fitContent")
}

#' DEFUNCT: getAllEdgeAttributes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumns}
#' @return None
#' @aliases getAllEdgeAttributes_deprecated
#' @usage getAllEdgeAttributes_deprecated
#' @export
#' @rdname getAllEdgeAttributes-defunct
getAllEdgeAttributes<-function(obj, onlySelectedEdges){
    .Defunct("getTableColumns")
}

#' DEFUNCT: getAllNodeAttributes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumns}
#' @return None
#' @aliases getAllNodeAttributes_deprecated
#' @usage getAllNodeAttributes_deprecated
#' @export
#' @rdname getAllNodeAttributes-defunct
getAllNodeAttributes<-function(obj, onlySelectedNodes){
    .Defunct("getTableColumns")
}

#' DEFUNCT: getCommandNames
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{commandsHelp}
#' @return None
#' @aliases getCommandNames_deprecated
#' @usage getCommandNames_deprecated
#' @export
#' @rdname getCommandNames-defunct
getCommandNames<-function(obj){
    .Defunct("commandsHelp")
}

#' DEFUNCT: getCommandNamesWithinNamespace
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{commandsHelp}
#' @return None
#' @aliases getCommandNamesWithinNamespace_deprecated
#' @usage getCommandNamesWithinNamespace_deprecated
#' @export
#' @rdname getCommandNamesWithinNamespace-defunct
getCommandNamesWithinNamespace<-function(obj, namespace){
    .Defunct("commandsHelp")
}

#' DEFUNCT: getEdgeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableValue}
#' @return None
#' @aliases getEdgeAttribute_deprecated
#' @usage getEdgeAttribute_deprecated
#' @export
#' @rdname getEdgeAttribute-defunct
getEdgeAttribute<-function(obj, edge.name, attribute.name){
    .Defunct("getTableValue")
}


#' DEFUNCT: getNodeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableValue}
#' @return None
#' @aliases getNodeAttribute_deprecated
#' @usage getNodeAttribute_deprecated
#' @export
#' @rdname getNodeAttribute-defunct
getNodeAttribute<-function(obj, node.name, attribute.name){
    .Defunct("getTableValue")
}

#' DEFUNCT: getEdgeAttributeNames
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumnNames}
#' @return None
#' @aliases getEdgeAttributeNames_deprecated
#' @usage getEdgeAttributeNames_deprecated
#' @export
#' @rdname getEdgeAttributeNames-defunct
getEdgeAttributeNames<-function(obj){
    .Defunct("getTableColumnNames")
}


#' DEFUNCT: getNodeAttributeNames
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumnNames}
#' @return None
#' @aliases getNodeAttributeNames_deprecated
#' @usage getNodeAttributeNames_deprecated
#' @export
#' @rdname getNodeAttributeNames-defunct
getNodeAttributeNames<-function(obj){
    .Defunct("getTableColumnNames")
}

#' DEFUNCT: ping
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{cytoscapePing}
#' @return None
#' @aliases ping_deprecated
#' @usage ping_deprecated
#' @export
#' @rdname ping-defunct
ping<-function(obj){
    .Defunct("cytoscapePing")
}

#' DEFUNCT: saveNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{saveSession}
#' @return None
#' @aliases saveNetwork_deprecated
#' @usage saveNetwork_deprecated
#' @export
#' @rdname saveNetwork-defunct
saveNetwork<-function(obj, file.name, format){
    .Defunct("saveSession")
}

#' DEFUNCT: selectEdgesConnectedBySelectedNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{selectEdgesConnectingSelectedNodes}
#' @return None
#' @aliases selectEdgesConnectedBySelectedNodes_deprecated
#' @usage selectEdgesConnectedBySelectedNodes_deprecated
#' @export
#' @rdname selectEdgesConnectedBySelectedNodes-defunct
selectEdgesConnectedBySelectedNodes<-function(obj){
    .Defunct("selectEdgesConnectingSelectedNodes")
}

#' DEFUNCT: showGraphicsDetails
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{toggleGraphicsDetails}
#' @return None
#' @aliases showGraphicsDetails_deprecated
#' @usage showGraphicsDetails_deprecated
#' @export
#' @rdname showGraphicsDetails-defunct
showGraphicsDetails<-function(obj, new.value){
    .Defunct("toggleGraphicsDetails")
}

#' DEFUNCT: deleteEdgeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteTableColumn}
#' @return None
#' @aliases deleteEdgeAttribute_deprecated
#' @usage deleteEdgeAttribute_deprecated
#' @export
#' @rdname deleteEdgeAttribute-defunct
deleteEdgeAttribute<-function(obj, attribute.name){
    .Defunct("deleteTableColumn")
}

#' DEFUNCT: deleteNodeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteTableColumn}
#' @return None
#' @aliases deleteNodeAttribute_deprecated
#' @usage deleteNodeAttribute_deprecated
#' @export
#' @rdname deleteNodeAttribute-defunct
deleteNodeAttribute<-function(obj, attribute.name){
    .Defunct("deleteTableColumn")
}

# ======== DEPRECATED ===============================================================


