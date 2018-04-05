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

#' DEFUNCT: setTooltipDismissDelay
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases setTooltipDismissDelaydefunct
#' @usage setTooltipDismissDelaydefunct
#' @export
#' @rdname setTooltipDismissDelay-defunct
setTooltipDismissDelay <- function (obj, msecs) {
    .Defunct("No replacement")
}

#' DEFUNCT: setTooltipInitialDelay
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases setTooltipInitialDelaydefunct
#' @usage setTooltipInitialDelaydefunct
#' @export
#' @rdname setTooltipInitialDelay-defunct
setTooltipInitialDelay <- function (obj, msecs) {
    .Defunct("No replacement")
}

#' DEFUNCT: setWindowSize
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases setWindowSizedefunct
#' @usage setWindowSizedefunct
#' @export
#' @rdname setWindowSize-defunct
setWindowSize <- function (obj, width, height) {
    .Defunct("No replacement")
}

#' DEFUNCT: setGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases setGraphdefunct
#' @usage setGraphdefunct
#' @export
#' @rdname setGraph-defunct
setGraph <- function (obj, graph) {
    .Defunct("No replacement")
}

#' DEFUNCT: setCommandProperties
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{commandsRun}
#' @return None
#' @aliases setCommandPropertiesdefunct
#' @usage setCommandPropertiesdefunct
#' @export
#' @rdname setCommandProperties-defunct
setCommandProperties<- function (obj, command.name, properties.list, copy.graph.to.R = FALSE) {
    .Defunct("commandsRun")
}

#' DEFUNCT: predictTimeToDisplayGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases predictTimeToDisplayGraphdefunct
#' @usage predictTimeToDisplayGraphdefunct
#' @export
#' @rdname predictTimeToDisplayGraph-defunct
predictTimeToDisplayGraph <- function (obj) {
    .Defunct("No replacement")
}

#' DEFUNCT: raiseWindow
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases raiseWindowdefunct
#' @usage raiseWindowdefunct
#' @export
#' @rdname raiseWindow-defunct
raiseWindow <- function (obj, window.title) {
    .Defunct("No replacement")
}

#' DEFUNCT: noa
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases noadefunct
#' @usage noadefunct
#' @export
#' @rdname noa-defunct
noa <- function (graph, node.attribute.name) {
    .Defunct("No replacement")
}

#' DEFUNCT: noa.names
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases noa.namesdefunct
#' @usage noa.namesdefunct
#' @export
#' @rdname noa.names-defunct
noa.names <- function (graph) {
    .Defunct("No replacement")
}

#' DEFUNCT: makeRandomGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases makeRandomGraphdefunct
#' @usage makeRandomGraphdefunct
#' @export
#' @rdname makeRandomGraph-defunct
makeRandomGraph <- function (node.count, seed) {
    .Defunct("No replacement")
}

#' DEFUNCT: initEdgeAttribute
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases initEdgeAttributedefunct
#' @usage initEdgeAttributedefunct
#' @export
#' @rdname initEdgeAttribute-defunct
initEdgeAttribute <- function (graph, attribute.name, attribute.type, default.value) {
    .Defunct("No replacement")
}

#' DEFUNCT: initNodeAttribute
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases initNodeAttributedefunct
#' @usage initNodeAttributedefunct
#' @export
#' @rdname initNodeAttribute-defunct
initNodeAttribute <- function (graph, attribute.name, attribute.type, default.value) {
    .Defunct("No replacement")
}

#' DEFUNCT: getViewCoordinates
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases getViewCoordinatesdefunct
#' @usage getViewCoordinatesdefunct
#' @export
#' @rdname getViewCoordinates-defunct
getViewCoordinates <- function (obj) {
    .Defunct("No replacement")
}

#' DEFUNCT: getDefaultEdgeReverseSelectionColor
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases getDefaultEdgeReverseSelectionColordefunct
#' @usage getDefaultEdgeReverseSelectionColordefunct
#' @export
#' @rdname getDefaultEdgeReverseSelectionColor-defunct
getDefaultEdgeReverseSelectionColor <- function (obj, vizmap.style.name) {
    .Defunct("No replacement")
}

#' DEFUNCT: getDefaultNodeReverseSelectionColor
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases getDefaultNodeReverseSelectionColordefunct
#' @usage getDefaultNodeReverseSelectionColordefunct
#' @export
#' @rdname getDefaultNodeReverseSelectionColor-defunct
getDefaultNodeReverseSelectionColor <- function (obj, vizmap.style.name) {
    .Defunct("No replacement")
}

#' DEFUNCT: setDefaultEdgeReverseSelectionColor
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases setDefaultEdgeReverseSelectionColordefunct
#' @usage setDefaultEdgeReverseSelectionColordefunct
#' @export
#' @rdname setDefaultEdgeReverseSelectionColor-defunct
setDefaultEdgeReverseSelectionColor <- function (obj, vizmap.style.name) {
    .Defunct("No replacement")
}

#' DEFUNCT: setDefaultNodeReverseSelectionColor
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases setDefaultNodeReverseSelectionColordefunct
#' @usage setDefaultNodeReverseSelectionColordefunct
#' @export
#' @rdname setDefaultNodeReverseSelectionColor-defunct
setDefaultNodeReverseSelectionColor <- function (obj, vizmap.style.name) {
    .Defunct("No replacement")
}

#' DEFUNCT: getAttributeClassNames
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases getAttributeClassNamesdefunct
#' @usage getAttributeClassNamesdefunct
#' @export
#' @rdname getAttributeClassNames-defunct
getAttributeClassNames <- function (obj) {
    .Defunct("No replacement")
}

#' DEFUNCT: getAdjacentEdgeNames
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use these replacement functions instead: 
#' \link{selectEdgesAdjacentToSelectedNodes}, \link{getSelectedEdges}
#' @return None
#' @aliases getAdjacentEdgeNamesdefunct
#' @usage getAdjacentEdgeNamesdefunct
#' @export
#' @rdname getAdjacentEdgeNames-defunct
getAdjacentEdgeNames <- function (graph, node.names) {
    .Defunct("selectEdgesAdjacentToSelectedNodes")
}

#' DEFUNCT: eda
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases edadefunct
#' @usage edadefunct
#' @export
#' @rdname eda-defunct
eda <- function (graph, edge.attribute.name) {
    .Defunct("No replacement")
}

#' DEFUNCT: eda.names
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases eda.namesdefunct
#' @usage eda.namesdefunct
#' @export
#' @rdname eda.names-defunct
eda.names <- function (graph) {
    .Defunct("No replacement")
}

#' DEFUNCT: displayGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases displayGraphdefunct
#' @usage displayGraphdefunct
#' @export
#' @rdname displayGraph-defunct
displayGraph <- function (obj) {
    .Defunct("No replacement")
}

#' DEFUNCT: demoSimpleGraph
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases demoSimpleGraphdefunct
#' @usage demoSimpleGraphdefunct
#' @export
#' @rdname demoSimpleGraph-defunct
demoSimpleGraph <- function () {
    .Defunct("No replacement")
}

#' DEFUNCT: cyPlot
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases cyPlotdefunct
#' @usage cyPlotdefunct
#' @export
#' @rdname cyPlot-defunct
cyPlot <- function (node.df, edge.df) {
    .Defunct("No replacement")
}

#' DEFUNCT: cy2.edge.names
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases cy2.edge.namesdefunct
#' @usage cy2.edge.namesdefunct
#' @export
#' @rdname cy2.edge.names-defunct
cy2.edge.names <- function (graph, R.edge.names) {
    .Defunct("No replacement")
}

#' DEFUNCT: connectToNewestCyWindow
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases connectToNewestCyWindowdefunct
#' @usage connectToNewestCyWindowdefunct
#' @export
#' @rdname connectToNewestCyWindow-defunct
connectToNewestCyWindow <- function (obj, copyToR) {
    .Defunct("No replacement")
}

#' DEFUNCT: addCyEdge
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases addCyEdgedefunct
#' @usage addCyEdgedefunct
#' @export
#' @rdname addCyEdge-defunct
addCyEdge <- function (obj,sourceNode, targetNode, edgeType, directed) {
    .Defunct("No replacement")
}

#' DEFUNCT: addCyNode
#' 
#' @description This function is defunct and will be removed in the next release.
#' @return None
#' @aliases addCyNodedefunct
#' @usage addCyNodedefunct
#' @export
#' @rdname addCyNode-defunct
addCyNode <- function (obj, node.names) {
    .Defunct("No replacement")
}

#' DEFUNCT: getNodePosition
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{getNodeProperty}
#' @return None
#' @aliases getNodePositiondefunct
#' @usage getNodePositiondefunct
#' @export
#' @rdname getNodePosition-defunct
getNodePosition <- function (obj, node.names) {
    .Defunct("getNodeProperty")
}

#' DEFUNCT: setNodePosition
#' 
#' @description This function is defunct and will be removed in the next release.
#' Use the replacement function instead: 
#' \link{setNodePropertyBypass}
#' @return None
#' @aliases setNodePositiondefunct
#' @usage setNodePositiondefunct
#' @export
#' @rdname setNodePosition-defunct
setNodePosition <- function (obj, node.names, x.coords, y.coords) {
    .Defunct("setNodePropertyBypass")
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
#' @aliases getGraphdefunct
#' @usage getGraphdefunct
#' @export
#' @rdname getGraph-defunct
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

#' DEPRECATED: fitSelectedContent
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{fitContent}
#' @return None
#' @aliases fitSelectedContent_deprecated
#' @usage fitSelectedContent_deprecated
#' @export
#' @rdname fitSelectedContent-deprecated
fitSelectedContent<-function(obj){
    .Deprecated("fitContent")
    fitContent(selected.only = TRUE)
}

#' DEPRECATED: getAllEdgeAttributes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumns}
#' @return None
#' @aliases getAllEdgeAttributes_deprecated
#' @usage getAllEdgeAttributes_deprecated
#' @export
#' @rdname getAllEdgeAttributes-deprecated
getAllEdgeAttributes<-function(obj, onlySelectedEdges){
    .Deprecated("getTableColumns")
    getTableColumns('edge')
}

#' DEPRECATED: getAllNodeAttributes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumns}
#' @return None
#' @aliases getAllNodeAttributes_deprecated
#' @usage getAllNodeAttributes_deprecated
#' @export
#' @rdname getAllNodeAttributes-deprecated
getAllNodeAttributes<-function(obj, onlySelectedNodes){
    .Deprecated("getTableColumns")
    getTableColumns()
}

#' DEPRECATED: getCommandNames
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{commandsHelp}
#' @return None
#' @aliases getCommandNames_deprecated
#' @usage getCommandNames_deprecated
#' @export
#' @rdname getCommandNames-deprecated
getCommandNames<-function(obj){
    .Deprecated("commandsHelp")
    commandsHelp()
}

#' DEPRECATED: getCommandNamesWithinNamespace
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{commandsHelp}
#' @return None
#' @aliases getCommandNamesWithinNamespace_deprecated
#' @usage getCommandNamesWithinNamespace_deprecated
#' @export
#' @rdname getCommandNamesWithinNamespace-deprecated
getCommandNamesWithinNamespace<-function(obj, namespace){
    .Deprecated("commandsHelp")
    commandsHelp(namespace)
}

#' DEPRECATED: getEdgeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableValue}
#' @return None
#' @aliases getEdgeAttribute_deprecated
#' @usage getEdgeAttribute_deprecated
#' @export
#' @rdname getEdgeAttribute-deprecated
getEdgeAttribute<-function(obj, edge.name, attribute.name){
    .Deprecated("getTableValue")
    getTableValue('edge',edge.name, attribute.name)
}


#' DEPRECATED: getNodeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableValue}
#' @return None
#' @aliases getNodeAttribute_deprecated
#' @usage getNodeAttribute_deprecated
#' @export
#' @rdname getNodeAttribute-deprecated
getNodeAttribute<-function(obj, node.name, attribute.name){
    .Deprecated("getTableValue")
    getTableValue('node',node.name, attribute.name)
}

#' DEPRECATED: getEdgeAttributeNames
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumnNames}
#' @return None
#' @aliases getEdgeAttributeNames_deprecated
#' @usage getEdgeAttributeNames_deprecated
#' @export
#' @rdname getEdgeAttributeNames-deprecated
getEdgeAttributeNames<-function(obj){
    .Deprecated("getTableColumnNames")
    getTableColumnNames('edge')
}


#' DEPRECATED: getNodeAttributeNames
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{getTableColumnNames}
#' @return None
#' @aliases getNodeAttributeNames_deprecated
#' @usage getNodeAttributeNames_deprecated
#' @export
#' @rdname getNodeAttributeNames-deprecated
getNodeAttributeNames<-function(obj){
    .Deprecated("getTableColumnNames")
    getTableColumnNames('node')
}

#' DEPRECATED: ping
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{cytoscapePing}
#' @return None
#' @aliases ping_deprecated
#' @usage ping_deprecated
#' @export
#' @rdname ping-deprecated
ping<-function(obj){
    .Deprecated("cytoscapePing")
    cytoscapePing()
}

#' DEPRECATED: saveNetwork
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{saveSession}
#' @return None
#' @aliases saveNetwork_deprecated
#' @usage saveNetwork_deprecated
#' @export
#' @rdname saveNetwork-deprecated
saveNetwork<-function(obj, file.name, format){
    .Deprecated("saveSession")
    saveSession(filename = file.name)
}

#' DEPRECATED: selectEdgesConnectedBySelectedNodes
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{selectEdgesConnectingSelectedNodes}
#' @return None
#' @aliases selectEdgesConnectedBySelectedNodes_deprecated
#' @usage selectEdgesConnectedBySelectedNodes_deprecated
#' @export
#' @rdname selectEdgesConnectedBySelectedNodes-deprecated
selectEdgesConnectedBySelectedNodes<-function(obj){
    .Deprecated("selectEdgesConnectingSelectedNodes")
    selectEdgesConnectingSelectedNodes()
}

#' DEPRECATED: showGraphicsDetails
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{toggleGraphicsDetails}
#' @return None
#' @aliases showGraphicsDetails_deprecated
#' @usage showGraphicsDetails_deprecated
#' @export
#' @rdname showGraphicsDetails-deprecated
showGraphicsDetails<-function(obj, new.value){
    .Deprecated("toggleGraphicsDetails")
    toggleGraphicsDetails()
}

#' DEPRECATED: deleteEdgeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteTableColumn}
#' @return None
#' @aliases deleteEdgeAttribute_deprecated
#' @usage deleteEdgeAttribute_deprecated
#' @export
#' @rdname deleteEdgeAttribute-deprecated
deleteEdgeAttribute<-function(obj, attribute.name){
    .Deprecated("deleteTableColumn")
    deleteTableColumn(attribute.name,'edge')
}

#' DEPRECATED: deleteNodeAttribute
#' 
#' @description This function is only provided for compatibility with older
#' versions of RCy3 and will be defunct and removed in the next releases.
#' Use the replacement function instead: 
#' \link{deleteTableColumn}
#' @return None
#' @aliases deleteNodeAttribute_deprecated
#' @usage deleteNodeAttribute_deprecated
#' @export
#' @rdname deleteNodeAttribute-deprecated
deleteNodeAttribute<-function(obj, attribute.name){
    .Deprecated("deleteTableColumn")
    deleteTableColumn(attribute.name, 'node')
}




