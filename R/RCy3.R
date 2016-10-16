# RCy3.R
#--------------------------------------------------------------------------------
# this code is for the Bioconductor build system. You should never need to set or
# read these environment variables in ordinary use.
.BBSOverride <- function(host, port) {
    ret <- list()
    if ((Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE") != "")) {
      host = Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE")
      port = as(Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE"),"integer")
      }
    if (.Platform$r_arch == "x64") {
        if ((Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE_64") != "") &&  (Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE_64") != "")) {
          host = Sys.getenv("RCYTOSCAPE3_HOST_OVERRIDE_64")
          port = as(Sys.getenv("RCYTOSCAPE3_PORT_OVERRIDE_64"),"integer")
          }
    }
    #cat(paste("Using host", host, "and port", port, "."))

    ret["host"] <- host
    ret["port"] <- port
    ret
}


# ------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
# ------------------------------------------------------------------------------
setClass("CytoscapeConnectionClass", 
	representation = representation (uri="character"), 
	prototype = prototype(uri="http://localhost:1234/v1")
)

# ------------------------------------------------------------------------------
setClass("CytoscapeWindowClass", 
	representation = representation(
		title="character", 
		window.id='character', 
		graph="graphBase", 
		collectTimings="logical",
		suid.name.dict="list",
		edge.suid.name.dict="list",
                view.id='numeric'), 
	contains = 'CytoscapeConnectionClass', 
	prototype = prototype(title="R graph", 
		graph=new("graphNEL", edgemode='directed'), 
		uri="http://localhost:1234/v1", 
		collectTimings=FALSE, 
		suid.name.dict=list(),
		edge.suid.name.dict=list())
)

# ------------------------------------------------------------------------------
setGeneric ('ping', 
	signature='obj', function(obj) standardGeneric('ping'))
setGeneric ('pluginVersion', 
	signature='obj', function(obj) standardGeneric('pluginVersion'))
setGeneric ('getServerStatus',
    signature='obj', function(obj, api.version) standardGeneric('getServerStatus'))
setGeneric ('createWindow', 
	signature='obj', function(obj) standardGeneric('createWindow'))
setGeneric ('createWindowFromSelection', 
	signature='obj', function(obj, new.windowTitle, return.graph) standardGeneric ('createWindowFromSelection'))
setGeneric('copyCytoscapeNetwork',
  signature = 'obj', function(obj, new_title, copy.graph.to.R = FALSE) standardGeneric('copyCytoscapeNetwork'))
setGeneric('renameCytoscapeNetwork',	
  signature = 'obj',function(obj, new_title, copy.graph.to.R = FALSE) standardGeneric('renameCytoscapeNetwork'))
setGeneric ('getWindowCount', 
	signature='obj', function(obj) standardGeneric ('getWindowCount'))
setGeneric ('getWindowList',
	signature='obj', function(obj) standardGeneric ('getWindowList'))
setGeneric ('deleteWindow', 
	signature='obj', function(obj, window.title=NA) standardGeneric ('deleteWindow'))
setGeneric ('deleteAllWindows', 
	signature='obj', function(obj) standardGeneric ('deleteAllWindows'))
setGeneric ('getArrowShapes', 
	signature='obj', function(obj) standardGeneric ('getArrowShapes'))
setGeneric ('getLayoutNames', 
	signature='obj', function(obj) standardGeneric ('getLayoutNames'))
setGeneric ('getLayoutNameMapping',	
	signature='obj', function(obj) standardGeneric ('getLayoutNameMapping'))
setGeneric ('getLayoutPropertyNames',	
	signature='obj', function(obj, layout.name) standardGeneric ('getLayoutPropertyNames'))
setGeneric ('getLayoutPropertyType',	
	signature='obj', function(obj, layout.name, property.name) standardGeneric ('getLayoutPropertyType'))
setGeneric ('getLayoutPropertyValue', 
	signature='obj', function(obj, layout.name, property.name) standardGeneric ('getLayoutPropertyValue'))
setGeneric('getCommandNames', 
           signature='obj',
           function(obj) standardGeneric ('getCommandNames'))
setGeneric('getCommandsWithinNamespace', 
           signature = 'obj',
           function(obj,
                    namespace) standardGeneric('getCommandsWithinNamespace'))
setGeneric('setCommandProperties', 
           signature = 'obj',
           function(obj,
                    command.name,
                    properties.list, 
                    copy.graph.to.R = FALSE) standardGeneric('setCommandProperties')
)
setGeneric ('setLayoutProperties', 
	signature='obj', function(obj, layout.name, properties.list) standardGeneric ('setLayoutProperties'))
setGeneric ('getLineStyles', 
	signature='obj', function(obj) standardGeneric ('getLineStyles'))
setGeneric ('getNodeShapes', 
	signature='obj', function(obj) standardGeneric ('getNodeShapes'))
setGeneric ('getDirectlyModifiableVisualProperties', 
	signature='obj', function(obj, vizmap.style.name="default") standardGeneric ('getDirectlyModifiableVisualProperties'))
setGeneric ('getAttributeClassNames',	
	signature='obj', function(obj) standardGeneric ('getAttributeClassNames'))
setGeneric ('setGraph', 
	signature='obj', function(obj, graph) standardGeneric ('setGraph'))
setGeneric ('getGraph', 
	signature='obj', function(obj) standardGeneric ('getGraph'))
setGeneric ('sendNodes', 
	signature='obj', function(obj) standardGeneric ('sendNodes'))
setGeneric ('sendEdges', 
	signature='obj', function(obj) standardGeneric ('sendEdges'))

setGeneric ('addCyNode', 
	signature='obj', function(obj, nodeName) standardGeneric ('addCyNode'))
setGeneric ('addCyEdge', 
	signature='obj', function(obj, sourceNode, targetNode, edgeType, directed) standardGeneric ('addCyEdge'))
setGeneric ('addGraphToGraph', 
	signature='obj', function(obj, other.graph) standardGeneric ('addGraphToGraph'))

setGeneric ('setNodeAttributes', 
	signature='obj', function(obj, attribute.name) standardGeneric ('setNodeAttributes'))
setGeneric ('setNodeAttributesDirect', 
	signature='obj', function(obj, attribute.name, attribute.type, node.names, values) standardGeneric ('setNodeAttributesDirect'))

setGeneric ('setEdgeAttributes', 
	signature='obj', function(obj, attribute.name) standardGeneric ('setEdgeAttributes'))
setGeneric ('setEdgeAttributesDirect', 
	signature='obj', function(obj, attribute.name, attribute.type, edge.names, values) standardGeneric ('setEdgeAttributesDirect'))

setGeneric ('displayGraph', 
	signature='obj', function(obj) standardGeneric ('displayGraph'))
setGeneric ('predictTimeToDisplayGraph', 
	signature='obj', function(obj) standardGeneric ('predictTimeToDisplayGraph'))
setGeneric ('layoutNetwork', 
	signature='obj', function(obj, layout.name='grid') standardGeneric ('layoutNetwork'))
setGeneric ('saveLayout', 
	signature='obj', function(obj, filename, timestamp.in.filename=FALSE) standardGeneric ('saveLayout'))
setGeneric ('restoreLayout', 
	signature='obj', function(obj, filename) standardGeneric ('restoreLayout'))
setGeneric ('setNodePosition', 
	signature='obj', function (obj, node.names, x.coords, y.coords) standardGeneric ('setNodePosition'))
setGeneric ('getNodePosition',				signature='obj', function (obj, node.names) standardGeneric ('getNodePosition'))
setGeneric ('getNodeSize',					signature='obj', function (obj, node.names) standardGeneric ('getNodeSize'))
setGeneric ('redraw',							signature='obj', function (obj) standardGeneric ('redraw'))
setGeneric ('hidePanel',						signature='obj', function (obj, panelName) standardGeneric ('hidePanel'))
setGeneric ('hideAllPanels',					signature='obj', function (obj) standardGeneric ('hideAllPanels'))
setGeneric ('dockPanel',						signature='obj', function (obj, panelName) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',						signature='obj', function (obj, panelName) standardGeneric ('floatPanel'))

setGeneric ('setTooltipInitialDelay',		signature='obj', function (obj, msecs) standardGeneric ('setTooltipInitialDelay'))
setGeneric ('setTooltipDismissDelay',	signature='obj', function (obj, msecs) standardGeneric ('setTooltipDismissDelay'))

setGeneric ('raiseWindow',					signature='obj', function (obj, window.title=NA) standardGeneric ('raiseWindow'))
setGeneric ('setWindowSize',				signature='obj', function (obj, width, height) standardGeneric ('setWindowSize'))
setGeneric ('showGraphicsDetails',		signature='obj', function (obj, new.value) standardGeneric ('showGraphicsDetails'))
setGeneric ('fitContent',						signature='obj', function (obj) standardGeneric ('fitContent'))
setGeneric ('fitSelectedContent',			signature='obj', function (obj) standardGeneric ('fitSelectedContent'))
setGeneric ('getCenter',						signature='obj', function (obj) standardGeneric ('getCenter'))
setGeneric ('setCenter',						signature='obj', function (obj, x, y) standardGeneric ('setCenter'))
setGeneric ('getZoom',						signature='obj', function (obj) standardGeneric ('getZoom'))
setGeneric ('setZoom',						signature='obj', function (obj, new.level) standardGeneric ('setZoom'))
setGeneric ('getViewCoordinates',		signature='obj', function (obj) standardGeneric ('getViewCoordinates'))

setGeneric ('getDefaultBackgroundColor',	signature='obj',
            function (obj, vizmap.style.name='default') standardGeneric ('getDefaultBackgroundColor'))
setGeneric ('setDefaultBackgroundColor',  signature='obj', 
             function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultBackgroundColor'))

setGeneric ('getDefaultNodeSelectionColor',  signature='obj', 
             function (obj, vizmap.style.name='default') standardGeneric ('getDefaultNodeSelectionColor'))
setGeneric ('setDefaultNodeSelectionColor',  signature='obj', 
             function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeSelectionColor'))

setGeneric ('getDefaultNodeReverseSelectionColor',  signature='obj',
                function (obj, vizmap.style.name='default') standardGeneric ('getDefaultNodeReverseSelectionColor'))
setGeneric ('setDefaultNodeReverseSelectionColor',  signature='obj',
                function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeReverseSelectionColor'))

setGeneric ('getDefaultEdgeSelectionColor',  signature='obj', 
                function (obj, vizmap.style.name='default') standardGeneric ('getDefaultEdgeSelectionColor'))
setGeneric ('setDefaultEdgeSelectionColor',  signature='obj', 
             function (obj, new.color,  vizmap.style.name='default') standardGeneric ('setDefaultEdgeSelectionColor'))

setGeneric ('getDefaultEdgeReverseSelectionColor',  signature='obj',
                function (obj, vizmap.style.name='default') standardGeneric ('getDefaultEdgeReverseSelectionColor'))
setGeneric ('setDefaultEdgeReverseSelectionColor',  signature='obj',
                function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeReverseSelectionColor'))

setGeneric ('saveImage',                  signature='obj', function (obj, file.name, image.type, h=600) standardGeneric ('saveImage'))
setGeneric ('saveNetwork',                signature='obj', function (obj, file.name, format='cys') standardGeneric ('saveNetwork'))

setGeneric ('setDefaultNodeShape',        signature='obj', function (obj, new.shape, vizmap.style.name='default') standardGeneric ('setDefaultNodeShape'))
setGeneric ('setDefaultNodeSize',         signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultNodeSize'))
setGeneric ('setDefaultNodeColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeColor'))
setGeneric ('setDefaultNodeBorderColor',  signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeBorderColor'))
setGeneric ('setDefaultNodeBorderWidth',  signature='obj', function (obj, new.width, vizmap.style.name='default') standardGeneric ('setDefaultNodeBorderWidth'))
setGeneric ('setDefaultNodeFontSize',     signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultNodeFontSize'))
setGeneric ('setDefaultNodeLabelColor',   signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeLabelColor'))

setGeneric ('setDefaultEdgeLineWidth',    signature='obj', function (obj, new.width, vizmap.style.name='default') standardGeneric ('setDefaultEdgeLineWidth'))
setGeneric ('setDefaultEdgeColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeColor'))
setGeneric ('setDefaultEdgeSourceArrowColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeSourceArrowColor'))
setGeneric ('setDefaultEdgeTargetArrowColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeTargetArrowColor'))
setGeneric ('setDefaultEdgeFontSize',     signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultEdgeFontSize'))

setGeneric ('setNodeTooltipRule',       signature='obj', function (obj, node.attribute.name) standardGeneric ('setNodeTooltipRule'))
setGeneric ('setEdgeTooltipRule',       signature='obj', function (obj, edge.attribute.name) standardGeneric ('setEdgeTooltipRule'))
setGeneric ('setNodeLabelRule',         signature='obj', function (obj, node.attribute.name) standardGeneric ('setNodeLabelRule'))
setGeneric ('setEdgeLabelRule',         signature='obj', function (obj, edge.attribute.name) standardGeneric ('setEdgeLabelRule'))

setGeneric ('setNodeColorRule',         signature='obj', 
    function (obj, node.attribute.name, control.points, colors, mode, default.color='#FFFFFF') standardGeneric ('setNodeColorRule'))

setGeneric ('setNodeBorderColorRule',   signature='obj', 
    function (obj, node.attribute.name, control.points, colors, mode, default.color='#000000') standardGeneric ('setNodeBorderColorRule'))

setGeneric ('setNodeBorderWidthRule',   signature='obj', 
    function (obj, node.attribute.name, attribute.values, line.widths, default.width=1) standardGeneric ('setNodeBorderWidthRule'))

setGeneric ('setNodeShapeRule',         signature='obj', 
    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ELLIPSE') standardGeneric ('setNodeShapeRule'))
setGeneric ('setNodeSizeRule',          signature='obj', 
    function (obj, node.attribute.name, control.points, node.sizes, mode, default.size=40) standardGeneric ('setNodeSizeRule'))

setGeneric ('setNodeOpacityRule',          signature='obj', 
    function (obj, node.attribute.name, control.points, opacities, mode, aspect='all') standardGeneric ('setNodeOpacityRule'))


setGeneric ('setNodeSizeDirect',          signature='obj', function (obj, node.names, new.sizes) standardGeneric ('setNodeSizeDirect'))
setGeneric ('setNodeLabelDirect',         signature='obj', function (obj, node.names, new.labels) standardGeneric ('setNodeLabelDirect'))
setGeneric ('setNodeFontSizeDirect',      signature='obj', function (obj, node.names, new.sizes) standardGeneric ('setNodeFontSizeDirect'))
setGeneric ('setNodeLabelColorDirect',    signature='obj', function (obj, node.names, new.colors) standardGeneric ('setNodeLabelColorDirect'))
setGeneric ('setNodeWidthDirect',         signature='obj', function (obj, node.names, new.widths) standardGeneric ('setNodeWidthDirect'))
setGeneric ('setNodeHeightDirect',        signature='obj', function (obj, node.names, new.heights) standardGeneric ('setNodeHeightDirect'))
setGeneric ('setNodeShapeDirect',         signature='obj', function (obj, node.names, new.shapes) standardGeneric ('setNodeShapeDirect'))
setGeneric ('setNodeImageDirect',         signature='obj', function (obj, node.names, image.positions) standardGeneric ('setNodeImageDirect'))
setGeneric ('setNodeColorDirect',         signature='obj', function (obj, node.names, new.colors) standardGeneric ('setNodeColorDirect'))
setGeneric ('setNodeBorderWidthDirect',   signature='obj', function (obj, node.names, new.sizes) standardGeneric ('setNodeBorderWidthDirect'))
setGeneric ('setNodeBorderColorDirect',   signature='obj', function (obj, node.names, new.colors) standardGeneric ('setNodeBorderColorDirect'))

setGeneric ('setNodeOpacityDirect',       signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeOpacityDirect'))
setGeneric ('setNodeFillOpacityDirect',   signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeFillOpacityDirect'))
setGeneric ('setNodeLabelOpacityDirect',  signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeLabelOpacityDirect'))
setGeneric ('setNodeBorderOpacityDirect', signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeBorderOpacityDirect'))
setGeneric ('setEdgeOpacityDirect',         signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeOpacityDirect'))

setGeneric ('setEdgeColorDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeColorDirect'))
setGeneric ('setEdgeLabelDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLabelDirect'))
setGeneric ('setEdgeFontFaceDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeFontFaceDirect'))
setGeneric ('setEdgeFontSizeDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeFontSizeDirect'))
setGeneric ('setEdgeLabelColorDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLabelColorDirect'))
setGeneric ('setEdgeTooltipDirect', signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeTooltipDirect'))
setGeneric ('setEdgeLineWidthDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLineWidthDirect'))
setGeneric ('setEdgeLineStyleDirect', signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeLineStyleDirect'))
setGeneric ('setEdgeSourceArrowShapeDirect', signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeSourceArrowShapeDirect'))
setGeneric ('setEdgeTargetArrowShapeDirect', signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeTargetArrowShapeDirect'))
setGeneric ('setEdgeSourceArrowColorDirect', signature='obj', function (obj, edge.names, new.colors) standardGeneric ('setEdgeSourceArrowColorDirect'))
setGeneric ('setEdgeTargetArrowColorDirect', signature='obj', function (obj, edge.names, new.colors) standardGeneric ('setEdgeTargetArrowColorDirect'))
setGeneric ('setEdgeLabelOpacityDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLabelOpacityDirect'))
setGeneric ('setEdgeSourceArrowOpacityDirect', signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeSourceArrowOpacityDirect'))
setGeneric ('setEdgeTargetArrowOpacityDirect', signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeTargetArrowOpacityDirect'))
#setGeneric ('setEdgeLabelPositionDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLabelPositionDirect'))
#setGeneric ('setEdgeLabelWidthDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLabelWidthDirect'))


setGeneric ('setEdgeLineStyleRule',     signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') standardGeneric ('setEdgeLineStyleRule'))

setGeneric ('setEdgeLineWidthRule', signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.widths, default.width='1') standardGeneric ('setEdgeLineWidthRule'))

setGeneric ('setEdgeTargetArrowRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, arrows, default='ARROW') standardGeneric ('setEdgeTargetArrowRule'))
setGeneric ('setEdgeSourceArrowRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, arrows, default='ARROW') standardGeneric ('setEdgeSourceArrowRule'))

setGeneric ('setEdgeTargetArrowColorRule',   signature='obj', 
    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000') standardGeneric ('setEdgeTargetArrowColorRule'))
setGeneric ('setEdgeSourceArrowColorRule',   signature='obj', 
    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000') standardGeneric ('setEdgeSourceArrowColorRule'))

setGeneric ('setEdgeColorRule',         signature='obj',
    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#FFFFFF') standardGeneric ('setEdgeColorRule'))

setGeneric ('setEdgeOpacityRule',          signature='obj', 
    function (obj, edge.attribute.name, control.points, opacities, mode) standardGeneric ('setEdgeOpacityRule'))


setGeneric ('getNodeCount',             signature='obj', function (obj) standardGeneric ('getNodeCount'))
setGeneric ('getEdgeCount',             signature='obj', function (obj) standardGeneric ('getEdgeCount'))
setGeneric ('getNodeAttribute',         signature='obj', function (obj, node.name, attribute.name) standardGeneric ('getNodeAttribute'))
setGeneric ('getNodeAttributeType',     signature='obj', function (obj, attribute.name) standardGeneric ('getNodeAttributeType'))
setGeneric ('getAllNodeAttributes',     signature='obj', function (obj, onlySelectedNodes=FALSE) standardGeneric ('getAllNodeAttributes'))
setGeneric ('getEdgeAttribute',         signature='obj', function (obj, edge.name, attribute.name) standardGeneric ('getEdgeAttribute'))
setGeneric ('getEdgeAttributeType',     signature='obj', function (obj, attribute.name) standardGeneric ('getEdgeAttributeType'))
setGeneric ('getAllEdgeAttributes',     signature='obj', function (obj, onlySelectedEdges=FALSE) standardGeneric ('getAllEdgeAttributes'))
setGeneric ('getNodeAttributeNames',    signature='obj', function (obj) standardGeneric ('getNodeAttributeNames'))
setGeneric ('getEdgeAttributeNames',    signature='obj', function (obj) standardGeneric ('getEdgeAttributeNames'))
setGeneric ('deleteNodeAttribute',      signature='obj', function (obj, attribute.name) standardGeneric ('deleteNodeAttribute'))
setGeneric ('deleteEdgeAttribute',      signature='obj', function (obj, attribute.name) standardGeneric ('deleteEdgeAttribute'))
setGeneric ('getAllNodes',              signature='obj', function (obj) standardGeneric ('getAllNodes'))
setGeneric ('getAllEdges',              signature='obj', function (obj) standardGeneric ('getAllEdges'))
setGeneric ('selectNodes',              signature='obj', function (obj, node.names, preserve.current.selection=TRUE) standardGeneric ('selectNodes'))
setGeneric('selectAllNodes',            signature = 'obj',function(obj) standardGeneric('selectAllNodes'))
setGeneric ('getSelectedNodes',         signature='obj', function (obj) standardGeneric ('getSelectedNodes'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedNodeCount',     signature='obj', function (obj) standardGeneric ('getSelectedNodeCount'))
setGeneric ('hideNodes',                signature='obj', function (obj, node.names) standardGeneric ('hideNodes'))
setGeneric ('unhideNodes',              signature='obj', function (obj, node.names) standardGeneric ('unhideNodes'))
setGeneric ('hideSelectedNodes',        signature='obj', function (obj) standardGeneric ('hideSelectedNodes'))
setGeneric ('invertNodeSelection',      signature='obj', function (obj) standardGeneric ('invertNodeSelection'))
setGeneric ('deleteSelectedNodes',      signature='obj', function (obj) standardGeneric ('deleteSelectedNodes'))
setGeneric ('selectEdges',              signature='obj', function (obj, edge.names, preserve.current.selection=TRUE) standardGeneric ('selectEdges'))
setGeneric('selectAllEdges',            signature = 'obj', function(obj) standardGeneric('selectAllEdges'))
setGeneric ('invertEdgeSelection',      signature='obj', function (obj) standardGeneric ('invertEdgeSelection'))
setGeneric ('deleteSelectedEdges',      signature='obj', function (obj) standardGeneric ('deleteSelectedEdges'))
setGeneric ('getSelectedEdges',         signature='obj', function (obj) standardGeneric ('getSelectedEdges'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedEdgeCount',     signature='obj', function (obj) standardGeneric ('getSelectedEdgeCount'))
setGeneric ('hideSelectedEdges',        signature='obj', function (obj) standardGeneric ('hideSelectedEdges'))

setGeneric ('unhideAll',                signature='obj', function (obj) standardGeneric ('unhideAll'))

setGeneric ('getFirstNeighbors',        signature='obj', function (obj, node.names, as.nested.list=FALSE) standardGeneric ('getFirstNeighbors'))
setGeneric ('selectFirstNeighborsOfSelectedNodes',
                                        signature='obj', function (obj) standardGeneric ('selectFirstNeighborsOfSelectedNodes'))
setGeneric ('sfn',                      signature='obj', function (obj) standardGeneric ('sfn'))
setGeneric ('selectEdgesConnectedBySelectedNodes', 
            signature='obj', function(obj) standardGeneric ('selectEdgesConnectedBySelectedNodes'))

#-----------------------------------------------------------
# methods related to transmitting data from Cytoscape to R
#-----------------------------------------------------------
setGeneric ('getWindowID',                   signature='obj', function (obj, window.title) standardGeneric ('getWindowID'))
setGeneric ('haveNodeAttribute',             signature='obj', function (obj, node.names, attribute.name) standardGeneric ('haveNodeAttribute'))
setGeneric ('haveEdgeAttribute',             signature='obj', function (obj, edge.names, attribute.name) standardGeneric ('haveEdgeAttribute'))
setGeneric ('copyNodeAttributesFromCyGraph', signature='obj', function (obj, window.id, existing.graph) standardGeneric ('copyNodeAttributesFromCyGraph'))
setGeneric ('copyEdgeAttributesFromCyGraph', signature='obj', function (obj, window.id, existing.graph) standardGeneric ('copyEdgeAttributesFromCyGraph'))
setGeneric ('getGraphFromCyWindow',          signature='obj', function (obj, window.title) standardGeneric ('getGraphFromCyWindow'))
setGeneric('connectToNewestCyWindow', 
           signature = 'obj',
           function(obj,
                    copyToR = FALSE) standardGeneric('connectToNewestCyWindow')
)

#-----------------------------------------------------------
# methods related to visual styles
#-----------------------------------------------------------
setGeneric ('getVisualStyleNames',    signature='obj', function (obj) standardGeneric ('getVisualStyleNames'))
setGeneric ('copyVisualStyle',        signature='obj', function (obj, from.style, to.style) standardGeneric ('copyVisualStyle'))
setGeneric ('setVisualStyle',         signature='obj', function (obj, new.style.name) standardGeneric ('setVisualStyle'))
setGeneric ('lockNodeDimensions',     signature='obj', function (obj, new.state, visual.style.name='default') standardGeneric ('lockNodeDimensions'))

#-----------------------------------------------------------
# private methods, for internal use only
#-----------------------------------------------------------
setGeneric ('.addNodes',
            signature='obj', function (obj, other.graph) standardGeneric ('.addNodes'))
setGeneric ('.addEdges', 
            signature='obj', function (obj, other.graph) standardGeneric ('.addEdges'))
setGeneric ('.getWindowNameFromSUID', 
            signature='obj', function (obj, win.suid) standardGeneric ('.getWindowNameFromSUID'))
setGeneric ('.getNetworkViews', 
            signature='obj', function (obj) standardGeneric ('.getNetworkViews'))
setGeneric ('.nodeNameToNodeSUID',
            signature='obj', function (obj, node.names) standardGeneric ('.nodeNameToNodeSUID'))
setGeneric ('.nodeSUIDToNodeName', 
            signature='obj', function (obj, node.suids) standardGeneric ('.nodeSUIDToNodeName'))
setGeneric ('.edgeNameToEdgeSUID',
            signature='obj', function (obj, edge.names) standardGeneric ('.edgeNameToEdgeSUID'))
setGeneric ('.edgeSUIDToEdgeName', 
            signature='obj', function (obj, edge.suids) standardGeneric ('.edgeSUIDToEdgeName'))
setGeneric ('cyPlot', function (node.df, edge.df) standardGeneric('cyPlot'))

# ------------------------------------------------------------------------------
setValidity("CytoscapeWindowClass", function(object) {
    if (length(object@title) != 1){
        "'title' is not a single string" 
    }
    else if (!nzchar(object@title)){
        "'title' is an empty string"
    }
    validObject(object@graph)
}) # END setValidity

# ------------------------------------------------------------------------------
CytoscapeConnection = function(host='localhost', port=1234) {

    res <- .BBSOverride(host, port)
    host = res$host
    port = res$port
    uri = sprintf('http://%s:%s', host, port)
    cc = new('CytoscapeConnectionClass', uri = uri)
    if (!url.exists(uri)){
        write(sprintf('Connection failed.'), stderr())
        write(sprintf('To troubleshoot: 1) Please ensure that you have Cytoscape open'), stderr())
        write(sprintf('2) that the latest version of CyREST is installed.'), stderr())
        write(sprintf('3) that Cytoscape uses Java 8 (not 7 or below).'), stderr())
        write(sprintf('To help troubleshooting, please check:'), stderr())
        write(sprintf('http://www.cytoscape.org/troubleshooting.html'), stderr())
        return()
    }
    return(cc)
} # END CytoscapeConnection

# ------------------------------------------------------------------------------
# the 'CytoscapeWindow' class constructor, defined as a simple function
CytoscapeWindow = function(title, graph=new('graphNEL', edgemode='directed'), host='localhost', 
                           port=1234, create.window=TRUE, overwriteWindow=FALSE, collectTimings=FALSE){
    res <- .BBSOverride(host, port)
    host = res$host
    port = res$port
	uri = sprintf('http://%s:%s', host, port)
	
    # new 'CytoscapeConnectionClass' object
	cy.conn = CytoscapeConnection(host, port)
	if (is.null(cy.conn)){
	    return()
	}
	check.cytoscape.plugin.version(cy.conn)
	# if the user has specified, delete already existing window(s) with the same title
	if (overwriteWindow) {
        if (title %in% as.character(getWindowList(cy.conn))) {
		    deleteWindow(cy.conn, title)
		}
	}
	
	if (!is.na(getWindowID(cy.conn, title))) {
		write(sprintf('There is already a window in Cytoscape named "%s".', title), stderr())
		write(sprintf('Please use a unique name, or set "overwriteWindow=TRUE".'), stderr())
		stop()
	}
    
    # add a label to each node if not already present. default label is the node name, the node ID    	
    if (is.classic.graph(graph)){
        if (edgemode(graph) == 'undirected') {
            graph = remove.redundancies.in.undirected.graph(graph)
        }
    }
	# are all node attributes properly initialized?
	node.attributes = noa.names(graph)
	if (length(node.attributes) > 0) {
		check.list = list()
		for (node.attribute in node.attributes) {
			check.list[[node.attribute]] = properlyInitializedNodeAttribute(graph, node.attribute)
		}
		uninitialized.attributes = which(check.list == FALSE)
		if (length(uninitialized.attributes) > 0) {
			write(sprintf("%d uninitialized node attribute/s", length(uninitialized.attributes)), stderr())
			return()
		}
	} # if node.attributes

	# are all edge attributes properly initialized?
	edge.attributes = eda.names(graph)
	if (length(edge.attributes) > 0) {
		check.list = list()
		for (edge.attribute in edge.attributes) {
			check.list[[edge.attribute]] = properlyInitializedEdgeAttribute(graph, edge.attribute)
		}
		uninitialized.attributes = which(check.list == FALSE)
		if (length(uninitialized.attributes) > 0) {
			write(sprintf("%d uninitialized edge attribute/s", length(uninitialized.attributes)), stderr())
			return()
		}
	} # if edge.attributes
	
	if (!'label' %in% noa.names(graph)) {
		write('nodes have no label attribute -- adding default labels', stderr())
		graph = initNodeAttribute(graph, 'label', 'char', 'noLabel')
		if (length(nodes(graph) > 0)) {
			nodeData(graph, nodes(graph), 'label') = nodes(graph) # nodes(graph) returns strings
		}
	}
    
	# create new 'CytoscapeWindow' object
	cw = new('CytoscapeWindowClass', title=title, graph=graph, uri=uri, 
					 collectTimings=collectTimings, suid.name.dict = list(), edge.suid.name.dict=list())
	
	if (create.window) {
		cw@window.id = createWindow(cw)
	}
	cw@collectTimings = collectTimings
	
	# let user know that a new window was created
	write(sprintf('New window named "%s" was created in Cytoscape.', title), stderr())
	
	return (cw)

} # END 'CytsoscapeWindow' constructor

# ------------------------------------------------------------------------------
existing.CytoscapeWindow = 
    function(title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE) 
        {
        res <- .BBSOverride(host, port)
        host <- res$host
        port <- res$port
        
		uri <- sprintf('http://%s:%s', host, port)
		
        # establish a connection to Cytoscape
        cy.conn <- CytoscapeConnection(host, port)
		
        if (is.null(cy.conn)) {
            write(sprintf("ERROR in existing.CytoscapeWindow():\n\t Cytoscape connection could not be established >> NULL returned"), stderr())
            
            return()
        }
        # ensure the script is using the latest cyREST plugin version 
        check.cytoscape.plugin.version(cy.conn)
        
		existing.window.id = as.character(getWindowID(cy.conn, title))
		
		# inform user if the window does not exist
        if (is.na(existing.window.id)) {
            write(sprintf("ERROR in RCy3::existing.CytoscapeWindow():\n\t no window named '%s' exists in Cytoscape >> choose from the following titles: ", title), stderr())
			write(as.character(getWindowList(cy.conn)), stderr())
            return(NA)
		}
		
		# get graph from Cytoscape
        cy.window <- new('CytoscapeWindowClass', title=title, window.id=existing.window.id, uri=uri)

        if (copy.graph.from.cytoscape.to.R) {
            # copy over graph
            g.cy <- getGraphFromCyWindow(cy.window, title)
            cy.window <- setGraph(cy.window, g.cy)

            # copy over obj@suid.name.dict
            resource.uri <- paste(cy.window@uri, pluginVersion(cy.window), "networks", as.character(cy.window@window.id), sep="/")
            request.res <- GET(url=resource.uri)
            request.res <- fromJSON(rawToChar(request.res$content))

            if (length(request.res$elements$nodes) != 0){
                cy.window@suid.name.dict = lapply(request.res$elements$nodes, function(n) { 
                    list(name=n$data$name, SUID=n$data$SUID) })
            }
            if (length(request.res$elements$edges) != 0){
                cy.window@edge.suid.name.dict = lapply(request.res$elements$edges, function(e) {
                    list(name=e$data$name, SUID=e$data$SUID) })
            }
        }
        return (cy.window)
}
## END existing.CytsoscapeWindow


# ------------------------------------------------------------------------------
check.cytoscape.plugin.version = function(cyCon) 
{
	plugin.version.string = pluginVersion(cyCon)
	string.tmp1 = strsplit(plugin.version.string, ' ')[[1]][1]
	string.tmp2 = gsub('[a-z]', '', string.tmp1)
	string.tmp3 = gsub('[A-Z]', '', string.tmp2)
	plugin.version = as.numeric(string.tmp3)
	
	expected.version = 1
	
	if(plugin.version < expected.version) { 
		write(' ', stderr())
		write(sprintf('This version of the RCy3 package requires CyREST plugin version %s or greater.', expected.version), 
					stderr ())
		write(sprintf('However, you are using version %s. You must upgrade.', plugin.version), stderr ())
		write('Please visit the plugins page at http://www.cytoscape.org.', stderr ())
		write(' ', stderr())
		stop('Wrong CyREST version.')
	}
} # END check.cytoscape.plugin.version

#------------------------------------------------------------------------------------------------------------------------
setMethod('ping', signature = 'CytoscapeConnectionClass',
	function(obj) {
		conn.str <- paste(obj@uri, pluginVersion(obj), sep="/")
		res <- GET(conn.str)
		apiVersion <- fromJSON(rawToChar(res$content))$apiVersion
		
		if(length(apiVersion) > 0) {
			return("It works!")
		} else {
			write(sprintf('CyREST connection problem. RCy3 exits!'), stderr())
			stop()
		}
	}) # END ping
#------------------------------------------------------------------------------------------------------------------------
setMethod('pluginVersion', 'CytoscapeConnectionClass', 
	function(obj) {
		res <- GET(obj@uri)
		# get vector with available plugin versions
		available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
		
		api.version <- character(0)
		
		# loop through the vector and check which is the correct plugin version
		for(i in 1:length(available.api.versions)) {
		    server.status = getServerStatus(obj, available.api.versions[i])
		    
		    if(server.status$status_code == 200) {
		        api.version = fromJSON(rawToChar(server.status$content))$apiVersion
		    }
		}
		# current api.version will be the highest/latest version
		return(api.version)
	}) # END pluginVersion

# ------------------------------------------------------------------------------
setMethod('getServerStatus', 'CytoscapeConnectionClass',
          function(obj, api.version) {
              request.uri = paste(obj@uri, api.version, sep="/")
              request.res = GET(url=request.uri)
              return(request.res)
          }) # END getServerStatus

# ------------------------------------------------------------------------------
setMethod('createWindow', 'CytoscapeWindowClass', 
	function(obj) {
		obj@graph@graphData$name <- obj@title
		graph.attributes <- obj@graph@graphData
		graph.elements = list(nodes = list(), edges = list())
		
		cygraph <- toJSON(list(data = graph.attributes, elements = graph.elements))
		resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", sep="/")
		request.res <- POST(url = resource.uri, body = cygraph, encode = "json")
		window.id <- unname(fromJSON(rawToChar(request.res$content)))
		
		return(as.character(window.id))
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('createWindowFromSelection', 'CytoscapeWindowClass',

    function (obj, new.windowTitle, return.graph=FALSE) {
        if (getSelectedNodeCount (obj) == 0) {
            write (noquote ('RCy3::createWindowFromSelection error:  no nodes are selected'), stderr ())
            return (NA)
        }

        if (new.windowTitle %in% as.character (getWindowList (obj))) {
            msg <- sprintf ('RCy3::createWindowFromSelection error:  window "%s" already exists', new.windowTitle)
            write (noquote (msg), stderr ())
            return (NA)
        }
        
        # create new window
        cy.window <- CytoscapeWindow (new.windowTitle)
        
        net.SUID <- as.character(cy.window@window.id)
        version <- pluginVersion(obj)
        
        
        # copy nodes over
        selected.nodes <- getSelectedNodes(obj)
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "nodes", sep="/")
        new.nodes.JSON <- toJSON(selected.nodes)
        request.res <- POST(url=resource.uri, body=new.nodes.JSON, encode="json")
        new.node.SUIDs <- unname(fromJSON(rawToChar(request.res$content)))
        new.node.names <- do.call(rbind, lapply(new.node.SUIDs, data.frame, stringsAsFactors=FALSE))
        invisible(request.res)
        
        
        # copy node attributes over
        node.attribute.names <- noa.names(obj@graph)
        for (attribute.name in node.attribute.names) {
            printf('sending node attribute "%s"', attribute.name)
            caller.specified.attribute.class <- attr(nodeDataDefaults(obj@graph, attribute.name), 'class')
            values <- noa(obj@graph, attribute.name)[selected.nodes]
            values <- data.frame(values)
            values["name"] <- rownames(values)
            node.name.suid.value.df <- merge(new.node.names, values, by='name')
            
            # converts the above data frame data in the cyREST [SUID:value]-pairs format
            node.SUID.value.pairs <- 
                apply(node.name.suid.value.df[,c('SUID','values')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
            apply(data.frame(new.node.SUIDs, values), 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
            node.SUID.value.pairs.JSON = toJSON(node.SUID.value.pairs)
            resource.uri <- 
                paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns", attribute.name, sep="/")
            request.res <- PUT(url=resource.uri, body=node.SUID.value.pairs.JSON, encode="json")
            invisible(request.res)
        }
        
        
        # copy edges over
        if (is.classic.graph(obj@graph)) {
            tbl.edges <- .classicGraphToNodePairTable(obj@graph)
        } else if (is.multiGraph(obj@graph)) {
            tbl.edges <- .multiGraphToNodePairTable(obj@graph)
        }
        new.tbl.edges <- tbl.edges[tbl.edges$source %in% selected.nodes & tbl.edges$target %in% selected.nodes,]
        num.edges.to.copy <- nrow(new.tbl.edges)
        if (num.edges.to.copy > 0) {
            directed <- rep(TRUE, nrow(new.tbl.edges)) #TODO enable undirected?
            
            if (num.edges.to.copy == 1) {
                # get the SUIDs of the source nodes for the new edges
                source.node.SUIDs <- new.node.names$SUID[new.node.names$name==new.tbl.edges$source]
                # get the SUIDs of the target nodes for the new edges
                target.node.SUIDs <- new.node.names$SUID[new.node.names$name==new.tbl.edges$target]
            }else{
                source.node.SUIDs <- new.node.names$SUID[match(new.tbl.edges$source, new.node.names$name)]
                target.node.SUIDs <- new.node.names$SUID[match(new.tbl.edges$target, new.node.names$name)]
            }
            # format the new edges data for sending to Cytoscape
            edge.tbl.records = 
                apply(cbind(source.node.SUIDs, target.node.SUIDs, directed, new.tbl.edges$edgeType), MARGIN=1,
                      FUN=function(r) {list(source=unname(r[[1]]), target=unname(r[[2]]), directed=unname(r[[3]]), interaction=unname(r[[4]]))})
            edge.tbl.records.JSON = toJSON(edge.tbl.records)
            resource.uri = paste(cy.window@uri, version, "networks", net.SUID, "edges", sep="/")
            request.res = POST(url=resource.uri, body=edge.tbl.records.JSON, encode="json")
            new.edge.SUIDs <- unname(fromJSON(rawToChar(request.res$content)))
            invisible(request.res)
        }
        
        
        # copy edge attributes over
        edge.attribute.names = eda.names(obj@graph)
        for (attribute.name in edge.attribute.names) {
            printf('sending edge attribute "%s"', attribute.name)
            values <- eda(obj@graph, attribute.name)

            if (num.edges.to.copy == 1) {
                # add edge name to the dataframe
                edge.name.suid.value.df <- data.frame(rbind(unlist(new.edge.SUIDs)))
                edge.name.suid.value.df$edgeName <- paste0(new.node.names$name[new.node.names$SUID==edge.name.suid.value.df$source], "|",
                                                           new.node.names$name[new.node.names$SUID==edge.name.suid.value.df$target])
                edge.name.suid.value.df$edgeValue <- unname(values)[(names(values)==edge.name.suid.value.df$edgeName)]
            }else{
                edge.name.suid.value.df <- data.frame(matrix(unlist(new.edge.SUIDs), nrow=3, byrow = TRUE))
                names(edge.name.suid.value.df) <-  names(data.frame(rbind(unlist(new.edge.SUIDs[[1]]))))
                edge.name.suid.value.df$edgeName <- paste0(new.node.names$name[match(edge.name.suid.value.df$source, new.node.names$SUID)], "|",
                                                           new.node.names$name[match(edge.name.suid.value.df$target, new.node.names$SUID)])
                edge.name.suid.value.df$edgeValue <- unname(values)[match(edge.name.suid.value.df$edgeName, names(values))]
            }
            edge.SUID.value.pairs <- 
                apply(edge.name.suid.value.df[,c('SUID','edgeValue')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
            edge.SUID.value.pairs.JSON = toJSON(edge.SUID.value.pairs)
            resource.uri <- 
                paste(cy.window@uri, version, "networks", net.SUID, "tables/defaultedge/columns", attribute.name, sep="/")
            request.res <- PUT(url=resource.uri, body=edge.SUID.value.pairs.JSON, encode="json")
            invisible(request.res)
        }
        
        return (existing.CytoscapeWindow (new.windowTitle, copy.graph.from.cytoscape.to.R = return.graph))
    }) # createWindowFromSelection

#' Copy a Cytoscape Network 
#'
#' Makes a copy of a Cytoscape Network with all of its edges and nodes 
#'
#' @param obj Cytoscape network 
#' @param new_title New name for the copy
#' @param copy.graph.to.R Logical whether to copy the graph to a new object in R 
#' 
#' @return Connection to new copy of network. 
#'
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' copy_of_your_net <- copyCytoscapeNetwork(cw, "new_copy")
#' }
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{createWindowFromSelection}}, \code{\link{existing.CytoscapeWindow}}, \code{\link{renameCytoscapeNetwork}}
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('copyCytoscapeNetwork',
          'CytoscapeWindowClass', 
          function(obj,
                   new_title,
                   copy.graph.to.R = FALSE) {
            if (obj@title == new_title){
              print("Copy not made. The titles of the original window and its copy are the same. Please pick a new name for the copy.")
              stderr()
            }
            else{
              selectAllNodes(obj)
              selectAllEdges(obj)
              request.uri <- paste(obj@uri,
                                   pluginVersion(obj),
                                   "networks",
                                   obj@window.id,
                                   sep = "/")
              
              request.res <- POST(url = request.uri,
                                  query = list(title = new_title))
              
              invisible(request.res)
              
              if (copy.graph.to.R){
                connect_window <- existing.CytoscapeWindow(new_title,
                                                           copy.graph.from.cytoscape.to.R = TRUE)
                print(paste("Cytoscape window",
                            obj@title,
                            "successfully copied to",
                            connect_window@title,
                            "and the graph was copied to R."))
              } 
              else {
                connect_window <- existing.CytoscapeWindow(new_title,
                                                           copy.graph.from.cytoscape.to.R = FALSE) 
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
#' @param object Cytoscape network 
#' @param new_title New name for the copy
#' @param copy.graph.to.R Logical whether to copy the graph to a new object in R 
#' 
#' @return Connection to the renamed network. 
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{createWindowFromSelection}}, \code{\link{existing.CytoscapeWindow}}, \code{\link{copyCytoscapeNetwork}}
#'
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' renamed_net <- renameCytoscapeNetwork(cw, "renamed_network")
#' }
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('renameCytoscapeNetwork',
          'CytoscapeWindowClass', 
          function(obj,
                   new_title,
                   copy.graph.to.R = FALSE) {
            new_net <- copyCytoscapeNetwork(obj,
                                            new_title)  
            deleteWindow(obj,
                         obj@title)
            return(new_net)
          })

# ------------------------------------------------------------------------------
setMethod('getWindowCount', 'CytoscapeConnectionClass',
	function(obj) {
		resource.uri <- paste(obj@uri, pluginVersion(obj), "networks/count", sep="/")
		res <- GET(url=resource.uri)
		num.cytoscape.windows <- unname(fromJSON(rawToChar(res$content)))
		return(as.integer(num.cytoscape.windows))
}) # END getWindowCount

# ------------------------------------------------------------------------------
setMethod('getWindowID', 'CytoscapeConnectionClass', 
	function(obj, window.title) {
		# get all window suids and associates names
		resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", sep="/")
		request.res <- GET(resource.uri)
		# SUIDs list of the existing Cytoscape networks	
		cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
		# names list of the existing Cytoscape networks
		cy.networks.names = c()
		
		for(net.SUID in cy.networks.SUIDs)	{
			 res.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(net.SUID), sep="/")
			 result <- GET(res.uri)
			 net.name <- fromJSON(rawToChar(result$content))$data$name
			 cy.networks.names <- c(cy.networks.names, net.name)
		}

		if(!window.title %in% as.character(cy.networks.names)) {
			write(sprintf("Cytoscape window named '%s' does not exist yet", window.title), stderr())
			return (NA)
		} # if unrecognized window.title
		
		window.entry = which(as.character(cy.networks.names) == window.title)
		window.id = as.character(cy.networks.SUIDs[window.entry])
		
		return(window.id)
})

# ------------------------------------------------------------------------------
setMethod('getWindowList', 'CytoscapeConnectionClass', 
	function(obj) {
		if(getWindowCount(obj) == 0) {
			return(c())
		}
		resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", sep="/")
		request.res <- GET(resource.uri)
		# SUIDs list of the existing Cytoscape networks	
		cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
		# names list of the existing Cytoscape networks
		cy.networks.names = c()
		
		for(net.SUID in cy.networks.SUIDs)	{
			res.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(net.SUID), sep="/")
			result <- GET(res.uri)
			net.name <- fromJSON(rawToChar(result$content))$data$name
			cy.networks.names <- c(cy.networks.names, net.name)
		}
		
		return(cy.networks.names)
})

# ------------------------------------------------------------------------------
setMethod('deleteWindow', 'CytoscapeConnectionClass',
	function (obj, window.title=NA) {
		if(!is.na(window.title))
			window.id = getWindowID(obj, window.title)
		else if(class(obj) == 'CytoscapeWindowClass')
			window.id = as.character(obj@window.id)
		else {
			write(sprintf('RCy::deleteWindow error. You must provide a valid 
										CytoscapeWindow object, or a CytoscapeConnection object and 
										a window title'), stderr())
			return()
		}
		resource.uri = paste(obj@uri, pluginVersion(obj), "networks", window.id, sep="/")
		request.res = DELETE(url=resource.uri)
		invisible(request.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteAllWindows',	'CytoscapeConnectionClass', function (obj) {
    # deletes all networks and associated windows in Cytoscape
    resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", sep="/")
    request.res <- DELETE(resource.uri)
    invisible(request.res)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'CytoscapeConnectionClass', function (obj) {
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles/visualproperties/NODE_SHAPE/values", sep="/")
    request.res <- GET(resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    return(request.res$values)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDirectlyModifiableVisualProperties', 'CytoscapeConnectionClass',
    function (obj, vizmap.style.name="default") {
        resource.uri = paste(obj@uri, pluginVersion(obj), "styles", as.character(vizmap.style.name), "defaults", sep="/")
        request.res = GET(url=resource.uri)
        visual.properties <- unname(fromJSON(rawToChar(request.res$content))[[1]])
        visual.properties <- sapply(visual.properties, '[[', 1)
        return(visual.properties)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAttributeClassNames', 'CytoscapeConnectionClass',
# retrieve the names of the recognized and supported names for the class of any node or edge attribute.
	function (obj) {
		 return (c ('floating|numeric|double', 'integer|int', 'string|char|character'))
		 })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLineStyles', 'CytoscapeConnectionClass', function (obj) {
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles/visualproperties/EDGE_LINE_TYPE/values", sep="/")
	request.res <- GET(resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    return(request.res$values)
    })

# ------------------------------------------------------------------------------
setMethod('getArrowShapes', 'CytoscapeConnectionClass', 
    function(obj) {
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "styles/visualproperties/EDGE_TARGET_ARROW_SHAPE/values", sep="/")
        # TanjaM: EDGE_SOURCE_ARROW_SHAPE rather than TARGET returns the same results as of April 2015
        request.res <- GET(resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        return(request.res$values)
})
## END getArrowShapes

# ------------------------------------------------------------------------------
setMethod('getLayoutNames', 'CytoscapeConnectionClass', 
	function(obj) {
        request.uri <- paste(obj@uri, pluginVersion(obj), "apply/layouts", sep="/")
        request.res <- GET(url=request.uri)
        
        available.layouts <- unname(fromJSON(rawToChar(request.res$content)))
        return(available.layouts)
}) 
## END getLayoutNames

# ------------------------------------------------------------------------------
setMethod('getLayoutNameMapping', 'CytoscapeConnectionClass', 
    function(obj) {
        layout.names <- getLayoutNames(obj)
        layout.full.names <- c()
        
        # get the English/full name of a layout
        for (layout.name in layout.names){
            request.uri <- paste(obj@uri, pluginVersion(obj), "apply/layouts", as.character(layout.name), sep="/")
            request.res <- GET(url=request.uri)
            
            layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
            layout.full.names <- c(layout.full.names, layout.property.names[[4]])
        }
        names(layout.names) <- layout.full.names
        
        return(layout.names)
})
## END getLayoutNameMapping

# ------------------------------------------------------------------------------
setMethod('getLayoutPropertyNames', 'CytoscapeConnectionClass', 
    function(obj, layout.name) {
        request.uri <- paste(obj@uri, pluginVersion(obj), "apply/layouts", as.character(layout.name), "parameters/", sep="/")
        request.res <- GET(url=request.uri)
        
        layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
        return(sapply(layout.property.names, '[[', 1))
})
## END getLayoutPropertyNames

# ------------------------------------------------------------------------------
setMethod('getLayoutPropertyType', 'CytoscapeConnectionClass', 
    function(obj, layout.name, property.name) {
        request.uri <- paste(obj@uri, pluginVersion(obj), "apply/layouts", as.character(layout.name), "parameters/", sep="/")
        request.res <- GET(url=request.uri)
        
        layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
        layout.property.names <- sapply(layout.property.list, '[[', 1)
        position <- layout.property.names == property.name
        return(sapply(layout.property.list, '[[', 3)[position])
}) 
## END getLayoutPropertyType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyValue', 'CytoscapeConnectionClass', 

   function (obj, layout.name, property.name) {
       request.uri <- paste(obj@uri, pluginVersion(obj), "apply/layouts", as.character(layout.name), "parameters/", sep="/")
       request.res <- GET(url=request.uri)
       
       layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
       layout.property.names <- sapply(layout.property.list, '[[', 1)
       position <- layout.property.names == property.name
       return(sapply(layout.property.list, '[[', 4)[position])
     }) # getLayoutPropertyValue

#------------------------------------------------------------------------------------------------------------------------
#' Gets commands available from within cytoscape from 
#' functions within cytoscape and from installed plugins.
#'
#' @param obj Cytoscape network where commands are fetched via RCy3 
#' @return Vector of available commands from all namespaces (e.g. functions and plugins) 
#'
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('getCommandNames','CytoscapeConnectionClass',
          function(obj) { 
            request.uri <- paste(obj@uri,
                                 pluginVersion(obj),
                                 "commands",
                                 sep="/")
            request.res <- GET(url=request.uri)
            
            available.commands <- unlist(strsplit(rawToChar(request.res$content),
                                                  split="\n\\s*"))
            ## to remove "Available namespaces" title
            ## remove the first value
            available.commands <- available.commands[-1]
            return(available.commands) 
          })
# END getCommandNames

#' Gets commands available from within a namespace in Cytoscape from 
#' functions within cytoscape and from installed plugins.
#'
#' @param obj Cytoscape network where commands are fetched via RCy3 
#' @param namespace Cytoscape function (e.g. layout or network settings) or Cytoscape plugin function
#' 
#' @return Vector of available commands from a specific plugin or Cytoscape function (e.g. namespace)
#'
#' @concept RCy3
#' @export
#' 
setMethod('getCommandsWithinNamespace','CytoscapeConnectionClass',
          function(obj,
                   namespace) { 
            request.uri <- paste(obj@uri,
                                 pluginVersion(obj),
                                 "commands",
                                 namespace,
                                 sep = "/")
            request.res <- GET(url = request.uri)
            
            available.commands <- unlist(strsplit(rawToChar(request.res$content),
                                                  split = "\n\\s*"))
            ## remove "Available commands" title
            available.commands <- available.commands[-1]
            return(available.commands) })

# END getCommandsWithinNamespace

#' Runs a Cytoscape command (for example from a plugin) with a list of parameters and creates a connection to the network (if a new one is created) so that it can be further manipulated from R. 
#'
#' @param obj Cytoscape network where command is run via RCy3 
#' @param command.name Need more info here - how to specify..
#' @param properties.list Parameters (e.g. files, p-values, etc) to be used to set to run the command
#' @param copy.graph.to.R If true this copies the graph information to R. This step can be quite slow. Default is false. 
#' 
#' @return Runs in Cytoscape and creates a connection to the Cytoscape window so that it can be further manipulated from R 
#' 
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' selectAllNodes(cw)
#' }
#'
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('setCommandProperties','CytoscapeConnectionClass', 
          function(obj,
                   command.name,
                   properties.list, 
                   copy.graph.to.R = FALSE) {
            all.possible.props <- getCommandsWithinNamespace(obj,
                                                             command.name)
            if (all(names(properties.list) %in% all.possible.props) == FALSE) {
              print('You have included a name which is not in the commands')
              stderr ()
            } else {
              request.uri <- paste(obj@uri,
                                   pluginVersion(obj),
                                   "commands",
                                   as.character(command.name),
                                   sep = "/")
              
              request.res <- GET(url = request.uri,
                                 query = properties.list)
              if (request.res$status == 200){
                print("Successfully built the EnrichmentMap.")
                stdout ()
                resource.uri <- paste(obj@uri,
                                      pluginVersion(obj),
                                      "networks",
                                      sep = "/")
                request.res <- GET(resource.uri)
                # SUIDs list of the existing Cytoscape networks	
                cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
                # most recently made enrichment map will have the highest SUID
                cy.networks.SUIDs.last <- max(cy.networks.SUIDs)
                
                res.uri.last <- paste(obj@uri,
                                      pluginVersion(obj),
                                      "networks",
                                      as.character(cy.networks.SUIDs.last),
                                      sep = "/")
                result <- GET(res.uri.last)
                net.name <- fromJSON(rawToChar(result$content))$data$name
                
                if (copy.graph.to.R){
                  connect_window_to_R_session <- existing.CytoscapeWindow(net.name,
                                                                          copy.graph.from.cytoscape.to.R = TRUE)
                  print(paste0("Cytoscape window",
                               net.name,
                               " successfully connected to R session and graph copied to R."))
                } 
                else {
                  connect_window_to_R_session <- existing.CytoscapeWindow(net.name,
                                                                          copy.graph.from.cytoscape.to.R = FALSE) 
                  print(paste0("Cytoscape window ",
                               net.name,
                               " successfully connected to R session."))
                }
                
                
              } else {
                print("Something went wrong. Unable to run command.")
                stderr ()
              }
              invisible(request.res)
            }
            return(connect_window_to_R_session)
          }) 

# END setCommandProperties

setMethod ('setLayoutProperties', 'CytoscapeConnectionClass', 

    function (obj, layout.name, properties.list) {
        all.possible.props <- getLayoutPropertyNames (obj, layout.name)
        
        # set properties iteratively, this could have been done with a single API call
        for (prop in names (properties.list)) {
            if (!prop %in% all.possible.props) {
                write (sprintf ('%s is not a property in layout %s', prop, layout.name), stderr ())
            } else {
                new.value <- properties.list [[prop]]
                new.property.value.list <- list("name"=prop, "value"=new.value)
                new.property.value.list.JSON <- toJSON(list(new.property.value.list))
                
                request.uri <- paste(obj@uri, pluginVersion(obj), "apply/layouts", as.character(layout.name), "parameters/", sep="/")
                request.res <- PUT(url=request.uri, body= new.property.value.list.JSON, encode="json")
                if (request.res$status == 200){
                    write (sprintf ("Successfully updated the property '%s'.", prop), stdout ())
                } else {
                    write (sprintf ("Something went wrong. Unable to update property '%s'.", prop), stderr ())
                }
                invisible(request.res)
            }
        } # for prop
     }) # setLayoutProperties

# ------------------------------------------------------------------------------
setMethod ('setGraph', 'CytoscapeWindowClass', function(obj, graph) {
    # copy the graph over
    loc.obj <- obj
    if (edgemode(graph) == 'undirected'){
        graph = remove.redundancies.in.undirected.graph (graph)
    }
    
    loc.obj@graph = graph
    
    eval.parent(substitute(obj <- loc.obj))
})

# ------------------------------------------------------------------------------
setMethod('getGraph', 'CytoscapeWindowClass', 
  function(obj) {
    return(obj@graph)
})

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveNodeAttribute', 'CytoscapeConnectionClass',
    function(obj, node.names, attribute.name) {
    
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        # check the attribute exists
        if (attribute.name %in% getNodeAttributeNames(obj)) {
        # get the node SUIDs
            node.SUIDs = .nodeNameToNodeSUID(obj, node.names)
            nodes.that.have.attribute = c()
            
            for (i in 1:length(node.SUIDs)) {
                resource.uri = paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/rows", as.character(node.SUIDs[i]), attribute.name, sep="/")
                request.res = GET(url=resource.uri)
                node.attribute.value = rawToChar(request.res$content)
                
                if(nchar(node.attribute.value) > 0) {
                    nodes.that.have.attribute = c(nodes.that.have.attribute, node.SUIDs[i])
                }
            }
            
            return (as.character(.nodeSUIDToNodeName(obj, nodes.that.have.attribute)))
            } else {
                write(sprintf("Error: '%s' is not an existing node attribute name", attribute.name), stderr())
        }
    })
## END haveNodeAttribute

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveEdgeAttribute', 'CytoscapeConnectionClass',

    function (obj, edge.names, attribute.name) {
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        
        if(attribute.name %in% getEdgeAttributeNames(obj)) {
            edge.SUIDs = .edgeNameToEdgeSUID(obj, edge.names)
            edges.that.have.attribute = c()
            
            for(i in 1:length(edge.SUIDs)) {
                resource.uri = paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/rows", as.character(edge.SUIDs[i]), attribute.name, sep="/")
                request.res = GET(url=resource.uri)
                edge.attribute.value = rawToChar(request.res$content)
                
                if(nchar(edge.attribute.value) > 0) {
                    edges.that.have.attribute = c(edges.that.have.attribute, edge.SUIDs[i])
                }
            }
            
            return(as.character(.edgeSUIDToEdgeName(obj, edges.that.have.attribute)))
        } else {
            write(sprintf("Error: '%s' is no an existing edge attribute name", attribute.name), stderr())
        }
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyNodeAttributesFromCyGraph', 'CytoscapeConnectionClass',

    function (obj, window.id, existing.graph) {
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        
        node.attribute.names = getNodeAttributeNames(obj)
        
        for(attribute.name in node.attribute.names) {
            known.node.names = sapply(obj@suid.name.dict, function(n) { n$name })
            # nodes that store values for this attribute (meaning the value is not empty)
            nodes.with.attribute = haveNodeAttribute(obj, known.node.names, attribute.name)
            if(length(nodes.with.attribute) > 0) {
                attribute.type = getNodeAttributeType(obj, attribute.name)
                
                write(sprintf("\t retrieving attribute '%s' values for %d nodes", attribute.name, length(nodes.with.attribute)), stderr())
                # write(sprintf("\t retrieving %s '%s' attribute for %d nodes", attribute.type, attribute.name, length(nodes.with.attribute)), stderr())
                if(attribute.type == 'Integer') {
                    attribute.type = 'integer'
                    default.value = as.integer(0)
                } else if(attribute.type == 'String') {
                    attribute.type = 'char'
                    default.value = as.character('unassigned')
                } else if(attribute.type == 'Double') {
                    attribute.type = 'numeric'
                    default.value = as.numeric(0.0)
                } else if(attribute.type == 'Boolean') {
                    attribute.value = 'boolean'
                    default.value = as.logical(FALSE)
                } else {
                    write(sprintf('RCy3::copyNodeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr())
                    next()
                }
                existing.graph = 
                    initNodeAttribute(existing.graph, attribute.name, attribute.type, default.value)
                
                attribute.values = c()
                
                for(i in 1:length(nodes.with.attribute)) {
                    attribute.values = c(attribute.values, getNodeAttribute(obj, nodes.with.attribute[i], attribute.name))
                }
                nodeData(existing.graph, nodes.with.attribute, attribute.name) = attribute.values
            } ## END if there are nodes that have values for the attribute
        } ## END for loop : looping through each node attribute
        return(existing.graph)
    })
## END copyNodeAttributesFromCyGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyEdgeAttributesFromCyGraph', 'CytoscapeConnectionClass',

    function (obj, window.id, existing.graph) {
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        
        edge.attribute.names = getEdgeAttributeNames(obj)
        
        cy2.edgenames = as.character(cy2.edge.names(existing.graph)) # < 2 seconds for > 9000 edges
        
        for(attribute.name in edge.attribute.names) {
            edges.with.attribute = haveEdgeAttribute(obj, cy2.edgenames, attribute.name)
            
            if(length(edges.with.attribute) > 0) {
                attribute.type = getEdgeAttributeType(obj, attribute.name) 
                
                write(sprintf("\t retrieving attribute '%s' values for %d edges", attribute.name, length(edges.with.attribute)), stderr())
                if(attribute.type == 'Integer') {
                    attribute.type = 'integer' 
                    default.value = 0
                } else if(attribute.type == 'String') {
                    attribute.type = 'char'
                    default.value = 'unassigned'
                } else if(attribute.type == 'Double') {
                    attribute.type = 'numeric' 
                    default.value = as.numeric(0.0)
                } else {
                    write(sprintf('RCy3::copyEdgeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr())
                    next()
                }
                existing.graph = 
                    initEdgeAttribute(existing.graph, attribute.name, attribute.type, default.value)
                eda.values = c()
                
                for(i in 1:length(edges.with.attribute)) {
                    eda.values = c(eda.values, getEdgeAttribute(obj, edges.with.attribute[i], attribute.name))
                }
                
                regex = ' *[\\(|\\)] *'
                edges.tokens = strsplit(edges.with.attribute, regex)
                source.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[1]))
                target.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[3]))
                edge.types = unlist(lapply(edges.tokens, function(tokens) tokens[2])) 
                
                edgeData(existing.graph, source.nodes, target.nodes, attribute.name) = eda.values
                
                # for(i in 1:length(edgeData(existing.graph, from=source.nodes, to=target.nodes, attr=attribute.name))) {
                #     attr(edgeData(existing.graph, from=source.nodes, to=target.nodes, attr=attribute.name)[[i]], 'class') = 
                #         getEdgeAttributeType(obj, attribute.name)
                # }
            } ## END if
        } ## END for
        
        return(existing.graph)
    }) # END copyEdgeAttributesFromCyGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getGraphFromCyWindow', 'CytoscapeConnectionClass',

    function (obj, window.title) {
        window.id = NULL
        # handles the case when 'obj' is 'CytoscapeConnectionClass', instead of 'CytoscapeWindowClass' 
        if (class(obj) == "CytoscapeConnectionClass") {
            window.id = as.character(getWindowID(obj, window.title))
            loc.obj = 
                new('CytoscapeWindowClass', title=window.title, window.id=window.id, uri=obj@uri)
        } else {
            loc.obj = obj
        }
        # network id and cyREST plugin version
        net.SUID = as.character(loc.obj@window.id)
        version = pluginVersion(loc.obj)
        
        if (!is.na(net.SUID)) {
            # get the graph from Cytoscape
            resource.uri = paste(loc.obj@uri, version, "networks", net.SUID, sep="/")
            request.res = GET(url=resource.uri)
            request.res = fromJSON(rawToChar(request.res$content))
            
            g = new("graphNEL", edgemode='directed') # create graph object
            
            # GET GRAPH NODES
            g.nodes = request.res$elements$nodes
            # if there are no nodes in the graph received from Cytoscape, return an empty 'graphNEL' object
            if(length(g.nodes) == 0) {
                write(sprintf("NOTICE in RCy3::getGraphFromCyWindow():\n\t returning an empty 'graphNEL'"), stderr())
                return(g)
            }
            
            # else get the node names and add them to the R graph
            loc.obj@suid.name.dict = lapply(g.nodes, function(n) { 
            list(name=n$data$name, SUID=n$data$SUID) })
            g.node.names = sapply(loc.obj@suid.name.dict, function(n) { n$name })
            write(sprintf("\t received %d NODES from '%s'", length(g.nodes), window.title), stderr())
            g = graph::addNode(g.node.names, g)
            write(sprintf("\t - added %d nodes to the returned graph\n", length(g.node.names)), stderr())
            
            # GET NODE ATTRIBUTES (if any)
            g = copyNodeAttributesFromCyGraph(loc.obj, net.SUID, g)
            
            # Bioconductor's 'graph' edges require the 'edgeType' attribute, so its default value is assigned
            g = initEdgeAttribute (g, 'edgeType', 'char', 'assoc')
            
            # GET GRAPH EDGES
            g.edges = request.res$elements$edges
            
            if (length(g.edges) > 0) {
                regex = ' *[\\(|\\)] *'
                write(sprintf("\n\t received %d EDGES from '%s'", length(g.edges), window.title), stderr())
                
                loc.obj@edge.suid.name.dict = lapply(g.edges, function(e) {
                    list(name=e$data$name, SUID=e$data$SUID) })
                g.edge.names = sapply(loc.obj@edge.suid.name.dict, function(e) { e$name })
                edges.tokens = strsplit(g.edge.names, regex)
                source.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[1]))
                target.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[3]))
                edge.types = unlist(lapply(edges.tokens, function(tokens) tokens[2]))
                write(sprintf('\t - adding %d edges to the returned graph\n', length(edges.tokens)), stderr())
               
                tryCatch({
                    g = addEdge(source.nodes, target.nodes, g)
                    edgeData(g, source.nodes, target.nodes, 'edgeType') = edge.types
                    
                    # GET EDGE ATTRIBUTES (if any)
                    g = copyEdgeAttributesFromCyGraph(loc.obj, window.id, g)
                },
                error = function(cond){
                    write(sprintf("ERROR in RCy3::getGraphFromCyWindow(): Node names cannot contain parentheses.", window.title), stderr())
                    return(NA)
                })
                

            }
          
        } else {
            write(sprintf("ERROR in RCy3::getGraphFromCyWindow():\n\t there is no graph with name '%s' in Cytoscape", window.title), stderr())
            return(NA)
        }
        
        return(g)
  })
## END getGraphFromCyWindow

#' Creates a connection to the newest Cytoscape window so that it can be further manipulated from R.
#'
#' @param obj Cytoscape network where command is run via RCy3 
#' @param copyToR If true this copies the graph information to R. This step can be quite slow. Default is false. 
#' 
#' @return Creates a connection to the newest Cytoscape window so that it can be further manipulated from R 
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('connectToNewestCyWindow',
          'CytoscapeConnectionClass',
          function(obj,
                                    copyToR = FALSE) {
  resource.uri <- paste(obj@uri,
                        pluginVersion(obj),
                        "networks",
                        sep = "/")
  request.res <- GET(resource.uri)
  # SUIDs list of the existing Cytoscape networks
  cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
  # most recently made enrichment map will have the highest SUID
  cy.networks.SUIDs.last <- max(cy.networks.SUIDs)
  
  res.uri.last <- paste(obj@uri,
                        pluginVersion(obj),
                        "networks",
                        as.character(cy.networks.SUIDs.last),
                        sep = "/")
  result <- GET(res.uri.last)
  net.name <- fromJSON(rawToChar(result$content))$data$name
  
  ## to get edges request.res$elements$edges
  newest_CyWindow <- existing.CytoscapeWindow(net.name,
                                              copy.graph.from.cytoscape.to.R = copyToR) 
  return(newest_CyWindow)
})


# ------------------------------------------------------------------------------
setMethod('sendNodes', 'CytoscapeWindowClass', function(obj) {
    loc.obj <- obj
    # returns the nodes currently stored in the graph object
    graph.network.nodes = nodes(loc.obj@graph)
    # returns the nodes currently displayed in Cytoscape
    current.cytoscape.nodes = sapply(loc.obj@suid.name.dict, function(n) n$name)
    
    node.suid.name.dict <- (0)
    
    diff.nodes = setdiff(graph.network.nodes, current.cytoscape.nodes)
    # if new nodes need to be added
    if(length(diff.nodes) > 0) {
        net.SUID = as.character(loc.obj@window.id)
        version = pluginVersion(loc.obj)
        
        resource.uri = paste(loc.obj@uri, version, "networks", net.SUID, "nodes", sep="/")
        diff.nodes.JSON = toJSON(diff.nodes)
        
        write(sprintf('sending %d node(s)', length(diff.nodes)), stderr())
        
        request.res = POST(url=resource.uri, body=diff.nodes.JSON, encode="json")
        new.node.SUIDs = unname(fromJSON(rawToChar(request.res$content)))
        
        for(i in 1:length(new.node.SUIDs)) {
            loc.obj@suid.name.dict[[length(loc.obj@suid.name.dict)+1]] = new.node.SUIDs[[i]]
        }
    } else {
        write('CytoscapeWindow.sendNodes(), no new nodes to send, returning', stderr())
        return()
    }
    
    write('sendNodes completed', stderr())
    # needed for 'pass-by-reference' R functionality 
    eval.parent(substitute(obj <- loc.obj))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addNodes', signature (obj='CytoscapeWindowClass'),

    function (obj, other.graph) {
        loc.obj <- obj
        if(length(nodes(other.graph)) == 0) {
            write("NOTICE in RCy3::.addNodes():\n\t no nodes in other.graph >> function returns", stderr())
            return()
        }
        # new.nodes = setdiff(nodes(other.graph), getAllNodes(loc.obj))
        new.node.indices = which(!nodes(other.graph) %in% getAllNodes(loc.obj))
        
        new.nodes = nodes(other.graph)[new.node.indices]
        
        if(length(new.node.indices) > 0) {
            net.SUID = as.character(loc.obj@window.id)
            version = pluginVersion(loc.obj)
            
            resource.uri = paste(loc.obj@uri, version, "networks", net.SUID, "nodes", sep="/")
            new.nodes.JSON = toJSON(new.nodes)
            
            request.res = POST(url=resource.uri, body=new.nodes.JSON, encode="json")
            new.node.SUIDs = unname(fromJSON(rawToChar(request.res$content)))
            
            for(i in 1:length(new.node.SUIDs)) {
                loc.obj@suid.name.dict[[length(loc.obj@suid.name.dict)+1]] = new.node.SUIDs[[i]]
            }
        } else {
            write(sprintf("NOTICE in RCy3::.addNodes():\n\t all %d nodes already exist in Cytoscape - nothing new to add >> function returns", length(nodes(other.graph))), stderr())
            return()
        }
        # needed for 'pass-by-reference' R functionality
        eval.parent(substitute(obj <- loc.obj))
        
        return(new.node.indices)
    }) # END .addNodes
#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addEdges', signature (obj='CytoscapeWindowClass'),

    function (obj, other.graph) {
        loc.obj <- obj
        net.SUID = as.character(loc.obj@window.id)
        version = pluginVersion(loc.obj)
        
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
            sapply(loc.obj@edge.suid.name.dict, function(e) {return(e$name)})
        new.edge.indices = which(!other.graph.edge.names %in% cytoscape.existing.edge.names)
        
        if(length(new.edge.indices) > 0) {
            # source nodes vector
            source.nodes = tbl.edges$source[new.edge.indices]
            # target nodes vector
            target.nodes = tbl.edges$target[new.edge.indices]
            # edge types vector
            edge.type = tbl.edges$edgeType[new.edge.indices]
            directed = rep(TRUE, length(source.nodes))
            
            # convert the [node.SUID, node.name] dict(list) to data frame object
            suid.name.dict.df = 
                data.frame(matrix(unlist(loc.obj@suid.name.dict), nrow=length(loc.obj@suid.name.dict), byrow=TRUE), stringsAsFactors=FALSE)
            colnames(suid.name.dict.df) <- c("name", "SUID")
            
            # get the SUIDs of the source nodes for the new edges
            source.node.SUIDs = .nodeNameToNodeSUID(loc.obj, source.nodes)
            # get the SUIDs of the target nodes for the new edges
            target.node.SUIDs = .nodeNameToNodeSUID(loc.obj, target.nodes)
            
            # format the new edges data for sending to Cytoscape
            edge.tbl.records = 
                apply(cbind(source.node.SUIDs, target.node.SUIDs, directed, edge.type), MARGIN=1,
                      FUN=function(r) {list(source=unname(r[[1]]), target=unname(r[[2]]), directed=unname(r[[3]]), interaction=unname(r[[4]]))})
            edge.tbl.records.JSON = toJSON(edge.tbl.records)
            resource.uri = paste(loc.obj@uri, pluginVersion(loc.obj), "networks", net.SUID, "edges", sep="/")
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
                loc.obj@edge.suid.name.dict[[length(loc.obj@edge.suid.name.dict)+1]] = 
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
    }) # END .addEdges

# ------------------------------------------------------------------------------
# helper function: returns the name of the Cytoscape window based on its SUID
setMethod('.getWindowNameFromSUID', 'CytoscapeConnectionClass', 
    function(obj, win.suid) {
        suid <- as.character(win.suid)
        resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", suid, sep="/")
        request.res <- GET(url=resource.uri)
        win.name <- fromJSON(rawToChar(request.res$content))$data$name
        return(win.name)
}) 
## END .getWindowNameFromSUID

# ------------------------------------------------------------------------------
# helper function: returns the SUIDs of all views belonging to specific network
setMethod('.getNetworkViews', 'CytoscapeConnectionClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        
        resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", net.SUID, "views", sep="/")
        request.res <- GET(url=resource.uri)
        network.view.SUIDs <- unname(fromJSON(rawToChar(request.res$content)))
        return(network.view.SUIDs)
}) 
## END .getNetworkViews

# ------------------------------------------------------------------------------
setMethod('.nodeNameToNodeSUID', 'CytoscapeConnectionClass', 
    function(obj, node.names) {
        # initial source used 'which', but it did not return SUIDs in the input names order  
        # dict.indices = which(node.names %in% sapply(obj@suid.name.dict, function(n) { n$name}))
        # 'match' achieves this desired behavior
        dict.node.names <- sapply(obj@suid.name.dict, function(n) {n$name})
        dict.indices <- match(node.names, dict.node.names)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector 
        node.SUIDs <- sapply(obj@suid.name.dict[dict.indices[!is.na(dict.indices)]], function(i) {i$SUID})
        return(node.SUIDs)
}) 
## END .nodeNamesToNodeSUID

# ------------------------------------------------------------------------------
setMethod('.nodeSUIDToNodeName', 'CytoscapeConnectionClass', 
    function(obj, node.suids) {
        dict.node.SUIDs <- sapply(obj@suid.name.dict, function(s) {s$SUID})
        dict.indices <- match(node.suids, dict.node.SUIDs)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
        node.names <- sapply(obj@suid.name.dict[dict.indices[!is.na(dict.indices)]], function(n) {n$name})
        return(node.names)
}) 
## END .nodeSUIDToNodeName

# ------------------------------------------------------------------------------
setMethod('.edgeNameToEdgeSUID', 'CytoscapeConnectionClass', 
    function(obj, edge.names) {
        dict.edge.names <- sapply(obj@edge.suid.name.dict, function(e) {e$name})
        dict.indices <- match(edge.names, dict.edge.names)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
        edge.SUIDs <- sapply(obj@edge.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(i){ i$SUID })
        return(edge.SUIDs)
}) 
## END .edgeNamesToEdgeSUID

# ------------------------------------------------------------------------------
setMethod('.edgeSUIDToEdgeName', 'CytoscapeConnectionClass', 
    function(obj, edge.suids) {
        dict.edge.SUIDs = sapply(obj@edge.suid.name.dict, function(s) {s$SUID})
        dict.indices = match(edge.suids, dict.edge.SUIDs)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
        edge.names = sapply(obj@edge.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(e) {e$name})
        return(edge.names)
}) 
## END .edgeSUIDToEdgeName

# ------------------------------------------------------------------------------
setMethod('addCyNode', 'CytoscapeWindowClass', function(obj, nodeName) {
    loc.obj <- obj

    if(nodeName %in% getAllNodes(loc.obj)) {
        write(sprintf('RCy3::addCyNode, node "%s" already present in Cytoscape graph', nodeName), stderr())
        return()
    }
    
    # get the network suid
    net.suid <- as.character(loc.obj@window.id)
    resource.uri <- paste(loc.obj@uri, pluginVersion(loc.obj), "networks", net.suid, "nodes", sep="/")
    nodename.json = toJSON(c(nodeName))
    
    # add the node to the Cytoscape graph
    new.cynode.res <- POST(url=resource.uri, body=nodename.json, encode="json")
    new.cynode.suid.name <- unname(fromJSON(rawToChar(new.cynode.res$content)))
    
    # add the new node to the cw@suid.name.dict
    loc.obj@suid.name.dict[[length(loc.obj@suid.name.dict)+1]] <- 
        list(name=new.cynode.suid.name[[1]]$name, SUID=new.cynode.suid.name[[1]]$SUID)
    
    # add the node to the R graph object
    loc.obj@graph <- addNode(nodeName, loc.obj@graph)

    eval.parent(substitute(obj <- loc.obj))
}) # addCyNode

# ------------------------------------------------------------------------------
setMethod('addCyEdge', 'CytoscapeWindowClass', 
  function (obj, sourceNode, targetNode, edgeType, directed) {
    loc.obj <- obj
    
    good.args = TRUE
    # confirm that the user has provided exactly one source and one target nodes
    if(length(sourceNode) > 1 || length(targetNode) > 1) {
      good.args = FALSE
      write(sprintf('RCy3::addEdge can have only one source and one target nodes'), stderr())
    }
    
    if(!sourceNode %in% getAllNodes(loc.obj)) {
      good.args = FALSE
      write(sprintf('RCy3::addEdge. Error: source node %s does not exist in the Cytoscape graph. Edge cannot be created.', sourceNode), stderr())
    }
    if(!targetNode %in% getAllNodes(loc.obj)) {
      good.args = FALSE
      write(sprintf('RCy3::addEdge. Error: source node %s does not exist in the Cytoscape graph. Edge cannot be created.', targetNode), stderr())
    }
    if(!good.args) {
      return()
    }
    
    net.suid <- as.character(loc.obj@window.id)
    resource.uri <- paste(loc.obj@uri, pluginVersion(loc.obj), "networks", net.suid, "edges", sep="/")
    
    node.names.vec <- sapply(loc.obj@suid.name.dict, "[[", 1)
    edge.data <- list(source = loc.obj@suid.name.dict[[which(node.names.vec %in% sourceNode)]]$SUID, 
                      target = loc.obj@suid.name.dict[[which(node.names.vec %in% targetNode)]]$SUID, 
                      directed = directed, interaction = edgeType)
    
    edge.data.JSON <- toJSON(list(edge.data))
    
    new.cyedge.res <- POST(url=resource.uri, body=edge.data.JSON, encode='json')
    invisible(new.cyedge.res)
    
    # add the edge to the R graph object
    loc.obj@graph <- addEdge(sourceNode, targetNode, loc.obj@graph)
    
    eval.parent(substitute(obj <- loc.obj))
}) # addCyEdge

#------------------------------------------------------------------------------------------------------------------------
# This method adds a new graph to an existing graph.
# First the new nodes, then the new edges, then node attributes, then edge attributes
setMethod ('addGraphToGraph', 'CytoscapeWindowClass',

    function (obj, other.graph) {
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
    }) # END addGraphToGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod('sendEdges', 'CytoscapeWindowClass',
  function(obj) {
      loc.obj <- obj
      net.SUID = as.character(loc.obj@window.id)
      version = pluginVersion(loc.obj)
      # check that there are edges in the graph
      if(length(edgeNames(loc.obj@graph)) == 0) {
          write('NOTICE in RCy3::sendEdges():\n\t no edges in graph >> function returns', stderr())
          return()
      }
      
      write(sprintf('transforming (%d) graph edges to nodePairTable', length(edgeNames(loc.obj@graph))), stderr())
      if(loc.obj@collectTimings) {
          start.time = Sys.time()
      }
      
      if(is.classic.graph(loc.obj@graph)) {
          tbl.edges = .classicGraphToNodePairTable(loc.obj@graph)
      }
      else if(is.multiGraph(loc.obj@graph)) {
          tbl.edges = .multiGraphToNodePairTable(loc.obj@graph)
      }
      
      if (loc.obj@collectTimings){
          write (sprintf(' *** create node pair table: %f secs', difftime (Sys.time(), start.time, units='secs')), stderr ())
      }
      
      # get the list of edges to be send to Cytoscape
      in.graph.edge.names = unname(cy2.edge.names(loc.obj@graph))
      # get the list of currently existing esges (from dict)
      existing.edge.names = 
          sapply(loc.obj@edge.suid.name.dict, function(n) {return(n$name)})
      
      diff.edges = setdiff(in.graph.edge.names, existing.edge.names)
      # in new edges need to be send to the network
      if(length(diff.edges) > 0) {
          write (sprintf('sending %d edges', nrow(tbl.edges)), stderr())
          # source nodes vector
          source.nodes = tbl.edges$source
          # target nodes vector
          target.nodes = tbl.edges$target
          # edge types vector
          edge.type = tbl.edges$edgeType
          directed = rep(TRUE, length(source.nodes))
          
          # convert the [node.SUID, node.name] dict(list) to data frame object 
          suid.name.dict.df = 
              data.frame(matrix(unlist(loc.obj@suid.name.dict), nrow=length(loc.obj@suid.name.dict), byrow=TRUE), stringsAsFactors=FALSE)
          colnames(suid.name.dict.df) <- c("name", "SUID")
          # get the SUIDs of the source nodes for the new edges
          source.node.SUIDs = .nodeNameToNodeSUID(loc.obj, source.nodes)
          # get the SUIDs of the target nodes for the new edges
          target.node.SUIDs = .nodeNameToNodeSUID(loc.obj, target.nodes)
          
          # format the new edges data for sending to Cytoscape
          edge.tbl.records = 
              apply(cbind(source.node.SUIDs, target.node.SUIDs, directed, edge.type), MARGIN=1,
                    FUN=function(r) {list(source=unname(r[[1]]), target=unname(r[[2]]), directed=unname(r[[3]]), interaction=unname(r[[4]]))})
          edge.tbl.records.JSON = toJSON(edge.tbl.records)
          resource.uri = paste(loc.obj@uri, pluginVersion(loc.obj), "networks", net.SUID, "edges", sep="/")
          request.res = POST(url=resource.uri, body=edge.tbl.records.JSON, encode="json")
          
          # request.res.edge.SUIDs contains 
          # [edge.SUID, source.node.SUID, targetn.node.SUID] for each edge
          request.res.edge.data = fromJSON(rawToChar(request.res$content))
          
          edge.names = cy2.edge.names(obj@graph)
          # ctreates matrix of the format : 
          # note: column 1 contains edge.SUIDs, and columns 3 & 4 contain node.SUIDs
          #      [,1]   [,2]                     [,3]   [,4]
          # [1,] "412"  "A (phosphorylates) B"   "413"  "404"
          # [2,] "406"  "B (synthetic lethal C"  "407"  "408"
          # [3,] "407"  "C (undefined) A"        "408"  "406"
          edge.names.tbl.records = 
              apply(unname(cbind(unname(t(sapply(request.res.edge.data, unlist))), edge.names)), 
                    MARGIN=1, 
                    FUN=function(r) {list(SUID=as.numeric(unname(r[[1]])), value=unname(r[[4]]), 
                                          source.node=as.numeric(unname(r[[2]])), 
                                          target.node=as.numeric(unname(r[[3]])))})
          # CREATES DICT ENTRIES for the new edges in the following format :
          # [edge.SUID, edge.name, source.node.SUID, target.node.SUID]
          for(i in 1:length(edge.names.tbl.records)) {
              loc.obj@edge.suid.name.dict[[length(loc.obj@edge.suid.name.dict)+1]] = 
                  list(SUID=edge.names.tbl.records[[i]]$SUID, name=edge.names.tbl.records[[i]]$value, 
                       source.node=edge.names.tbl.records[[i]]$source.node, 
                       target.node=edge.names.tbl.records[[i]]$target.node)
          }
          invisible(request.res)
      } else {
          write(sprintf("NOTICE in RCy3::sendEdges():\n\t all %d edges already exist in Cytoscape - nothing new to add >> function returns", length(in.graph.edge.names)), stderr())
          return()
      }
      # simulate 'pass-by-reference' in R
      eval.parent(substitute(obj <- loc.obj))
}) # sendEdges

# ------------------------------------------------------------------------------
setMethod('layoutNetwork', 'CytoscapeWindowClass', 
    function(obj, layout.name = 'grid') {
        if(!layout.name %in% getLayoutNames(obj)) {
            write(sprintf("layout.name '%s' is not recognized; call getLayoutNames(<CytoscapeWindow>) to see those which are supported", layout.name), stderr())
    }
    id = as.character(obj@window.id)
    
    api.str <- paste(obj@uri, pluginVersion(obj), "apply/layouts", layout.name, id, sep = "/")
    
    res <- GET(api.str)
    invisible(res)
  }) # layoutNetwork

#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveLayout', 'CytoscapeWindowClass',

  function (obj, filename, timestamp.in.filename=FALSE) {

     custom.layout <- getNodePosition (obj,  getAllNodes (obj))
     if (timestamp.in.filename) {
        dateString <- format (Sys.time (), "%a.%b.%d.%Y-%H.%M.%S")
        stem <- strsplit (filename, '\\.RData')[[1]]
        filename <- sprintf ('%s.%s.RData', stem, dateString)
        write (sprintf ('saving layout to %s\n', filename), stderr ())
     }
     save (custom.layout, file=filename)
    }) # save.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('restoreLayout', 'CytoscapeWindowClass',

  function (obj, filename) {
     custom.layout <- local({x=load(filename); get(x)})
     node.names <- names (custom.layout)
     node.names.filtered <- intersect (node.names, getAllNodes (obj))
     x <- as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$x)))
     y <- as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$y)))
     setNodePosition (obj, node.names.filtered, x, y)
    }) # restoreLayout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodePosition', 'CytoscapeWindowClass',

    function (obj, node.names, x.coords, y.coords) {
        unknown.nodes <- setdiff (node.names, getAllNodes (obj))
        recognized.nodes <- intersect(node.names, getAllNodes(obj))
        
        # ensure that nodes were provided
        if (length (node.names) == 0){
            return ()
        }

        # throw error if nodes in node.names don't exist in the network
        if (length (unknown.nodes) > 0) {
            node.names = intersect (node.names, nodes (obj@graph))
            write (sprintf ("Error! Unknown nodes in RCy3::setNodePosition"), stderr ())
            for (i in 1:length (unknown.nodes)){
                write (sprintf ("     %s", unknown.nodes [i]), stderr ())
            } # end for
            return ()
        } # end if 

        # ensure that node.names, x.coords, y.coords are of the same length
        if (length(unique(c(length(node.names), length(x.coords), length(y.coords))))>1){
            write (sprintf ("Error! RCy3::setNodePosition: The node names vector has to be the same length as each of the x and y coordiante vectors."), stderr ())
            return()
        }
        
        # check if the coordinates are valid numbers
        if (!any(is.numeric(c(x.coords, y.coords)))){
            write (sprintf ("Error! RCy3::setNodePosition: x and y coordiante vectors must be numeric."), stderr ())
            return()
        }
            
        # set x position
        setNodePropertyDirect(obj, node.names, x.coords, "NODE_X_LOCATION")
        
        # set y position
        setNodePropertyDirect(obj, node.names, y.coords, "NODE_Y_LOCATION")
        
    }) # setNodePosition

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodePosition', 'CytoscapeWindowClass',
  function (obj, node.names) {
      net.suid = as.character(obj@window.id)
      # cyREST API version
      version = pluginVersion(obj)
      # get the views for the given network model
      resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", sep="/")
      request.res <- GET(resource.uri)
      net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
      
      view.SUID <- as.character(net.views.SUIDs[[1]])
      
      # if multiple views are found, inform the user about it
      if(length(net.views.SUIDs) > 1) {
          write(sprintf("RCy3::getNodePosition() - %d views found... getting node coordinates of the first one", length(net.views.SUIDs)), stderr())
      }
      
      coordinates.list <- list()
      # get node position for each node
      for (node.name in node.names){
          # convert node name into node SUID
          dict.indices = which(sapply(obj@suid.name.dict, function(s) { s$name }) %in% node.name)
          query.node = sapply(obj@suid.name.dict[dict.indices], function(i) {i$SUID})
          
          # get node x coordinate
          resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "nodes", as.character(query.node) ,"NODE_X_LOCATION", sep="/")
          request.res <- GET(resource.uri)
          node.x.position <- fromJSON(rawToChar(request.res$content))
          node.x.position <- node.x.position[[2]]
          
          # get node y coordinate
          resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "nodes", as.character(query.node) ,"NODE_Y_LOCATION", sep="/")
          request.res <- GET(resource.uri)
          node.y.position <- fromJSON(rawToChar(request.res$content))
          node.y.position <- node.y.position[[2]]
          
          # add x and y coordinates to the coordinates list
          coordinates.list[[node.name]] <- list(x= node.x.position, y=node.y.position)
      }
      return(coordinates.list)
    }) # getNodePosition

# ------------------------------------------------------------------------------
setMethod ('getNodeSize', 'CytoscapeWindowClass',

  function (obj, node.names) {
     # get network ID and version
     net.SUID = as.character(obj@window.id)
     version = pluginVersion(obj)
     
     # get the views for the given network model
     resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", sep="/")
     request.res <- GET(resource.uri)
     net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
     view.SUID <- as.character(net.views.SUIDs[[1]])
     
     widths <- c()
     heights <- c()
     
     for (pos in seq(node.names)){
        node.name <- node.names[pos]
        
        # map node name to node SUID
        dict.indices <- which(sapply(obj@suid.name.dict, function(s) { s$name }) %in% node.name)
        node.SUID <- sapply(obj@suid.name.dict[dict.indices], function(i) {i$SUID})
        
        # request 
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "nodes", as.character(node.SUID), sep="/")
    
        # request result
        request.res <- GET(resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        visual.properties <- sapply(request.res, '[[', "visualProperty")
        visual.values <- sapply(request.res, '[[', "value")
        widths <- c(widths, as.integer(visual.values[which(visual.properties =="NODE_WIDTH")]))
        heights <- c(heights, as.integer(visual.values[which(visual.properties =="NODE_HEIGHT")]))         
     } # end for (node.name in node.names)
     
     invisible(request.res)
     return (list (width=widths, height=heights))
    }) # getNodeSize

#------------------------------------------------------------------------------------------------------------------------
properlyInitializedNodeAttribute = function (graph, attribute.name) {

 if (length (nodes (graph)) == 0)
   return (TRUE)

 caller.specified.attribute.class = attr (nodeDataDefaults (graph, attribute.name), 'class')

 if (is.null (caller.specified.attribute.class)) {
    msg1 = sprintf ('Error!  Node attribute not initialized "%s"', attribute.name)
    msg2 = sprintf ('        You should call:')
    msg3 = sprintf ('        initNodeAttribute (graph, attribute.name, attribute.type, default.value)')
    msg4 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
    msg5 = sprintf ('        example:  g <- initNodeAttribute (g, "nodeType", "char", "molecule")')
    msg6 = sprintf ('             or:  g <- initNodeAttribute (g, "pValue", "numeric", 1.0)')
    write (msg1, stderr ())
    write (msg2, stderr ())
    write (msg3, stderr ())
    write (msg4, stderr ())
    write (msg5, stderr ())
    write (msg6, stderr ())
    return (FALSE)
    }
  return (TRUE)

} # properlyInitializedNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
properlyInitializedEdgeAttribute = function (graph, attribute.name) {

 if (length (edgeNames (graph)) == 0)
   return (TRUE)

 caller.specified.attribute.class = attr (edgeDataDefaults (graph, attribute.name), 'class')

 if (is.null (caller.specified.attribute.class)) {
    msg1 = sprintf ('Error!  "%s" edge attribute not initialized.', attribute.name)
    msg2 = sprintf ('        You should call:')
    msg3 = sprintf ('        initEdgeAttribute (graph, attribute.name, attribute.type, default.value)')
    msg4 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
    msg5 = sprintf ('        example:  g <- initEdgeAttribute (g, "edgeType", "char", "molecule")')
    msg6 = sprintf ('             or:  g <- initEdgeAttribute (g, "pValue", "numeric", 1.0)')
    write (msg1, stderr ())
    write (msg2, stderr ())
    write (msg3, stderr ())
    write (msg4, stderr ())
    write (msg5, stderr ())
    write (msg6, stderr ())
    return (FALSE)
    }
  return (TRUE)

} # properlyInitializedEdgeAttribute

# ------------------------------------------------------------------------------
setMethod('setNodeAttributes', 'CytoscapeWindowClass', 
    function(obj, attribute.name) { 
        # it might be the case that 'obj@graph' contains nodes that do NOT exist in Cytoscape
        # the below line identifies the indices of those graph nodes, which DO exist in Cytoscape
        node.indices = which(nodes(obj@graph) %in% getAllNodes(obj))
        
        if(length(node.indices) > 0) {
            node.names = nodes(obj@graph)[node.indices]
            
            values = noa(obj@graph, attribute.name)[node.indices]
            
            caller.specified.attribute.class = 
                attr(nodeDataDefaults(obj@graph, attribute.name), 'class')
            invisible(setNodeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, node.names, values))
        } else {
            write(sprintf("WARNING in RCy3::setNodeAttributes():\n\t before setting node attributes, please first send the graph nodes to Cytoscape >> function aborted"), stderr())
        }
})
## END setNodeAttributes

# ------------------------------------------------------------------------------
setMethod('setNodeAttributesDirect', 'CytoscapeWindowClass', 
    function(obj, attribute.name, attribute.type, node.names, values) {
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        
        caller.specified.attribute.class = tolower(attribute.type)
        # the switch-block ensures the attribute values have the correct data type
        switch(caller.specified.attribute.class,
               "floating"=,
               "numeric"=,
               "double"={
                   caller.specified.attribute.class = 'Double'
                   values = as.numeric(values)
               },
               "integer"=,
               "int"={
                   caller.specified.attribute.class = "Integer"
                   values = as.integer(values)
               },
               "boolean"={
                   caller.specified.attribute.class = "Boolean"
                   values = as.logical(values)
               },{
                   caller.specified.attribute.class = "String"
                   values = as.character(values)
               }
        )
        
        # CREATES NEW COLUMN (IF NEEDED)
        if(!attribute.name %in% getNodeAttributeNames(obj)) {
            # create new table column in Cytoscape 'Node Table' to store the attribute values
            tbl.col = list(name=attribute.name, type=caller.specified.attribute.class)
            tbl.col.JSON = toJSON(tbl.col)
            resource.uri = 
                paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
            request.res = POST(url=resource.uri, body=tbl.col.JSON, encode="json")
        }
        
        if(length(node.names) > 0) {
            if(length(node.names) != length(values)) {
                write(sprintf("ERROR in RCy3::setNodeAttributesDirect():\n\t the number of values(%d) for attribute '%s' must equal the number of nodes(%d) >> function aborted", 
                              length(values), attribute.name, length(node.names)), stderr())
            } else {
                node.SUIDs = .nodeNameToNodeSUID(obj, node.names)
                node.name.suid.value.df = data.frame(node.names, node.SUIDs, values)
                
                # converts the above data frame data in the cyREST [SUID:value]-pairs format
                node.SUID.value.pairs = 
                    apply(node.name.suid.value.df[,c('node.SUIDs','values')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
                node.SUID.value.pairs.JSON = toJSON(node.SUID.value.pairs)
                
                resource.uri = 
                    paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns", attribute.name, sep="/")
                request.res = PUT(url=resource.uri, body=node.SUID.value.pairs.JSON, encode="json")
                invisible(request.res)
            }
        }
})
## END setNodeAttributesDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeAttributes', 'CytoscapeWindowClass', 
    function(obj, attribute.name) {
        cyrest.edge.names = as.character(cy2.edge.names(obj@graph))
        # user might have entered the names of edges that do NOT exist
        # the below line will return the indices of the nodes that DO exist
        edge.indices = which(cyrest.edge.names %in% getAllEdges(obj))
        
        if(length(edge.indices) > 0) {
            edge.names = cyrest.edge.names[edge.indices]
            edge.names.tilde = names(cy2.edge.names(obj@graph)[edge.indices])
            edge.names.with.bars = gsub('~', '|', edge.names.tilde)
            
            values = eda(obj@graph, attribute.name)[edge.names.with.bars]
            
            caller.specified.attribute.class = attr(edgeDataDefaults(obj@graph, attribute.name), 'class')
            
            invisible(setEdgeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, edge.names, values))
        } else {
            write(sprintf("WARNING in RCy3::setEdgeAttributes():\n\t before setting edge attributes, please first send the graph edges to Cytoscape >> function aborted"), stderr())
        }        
}) 
## END setEdgeAttributes

# ------------------------------------------------------------------------------
setMethod('setEdgeAttributesDirect', 'CytoscapeWindowClass', 
    function(obj, attribute.name, attribute.type, edge.names, values) {
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        
        if(length(edge.names) > 0) {
            if(length(edge.names) != length(values)) {
                write(sprintf("ERROR in RCy3::setEdgeAttributesDirect():\n\t the number of values(%d) for attribute '%s' must equal the number of edges(%d) >> function aborted", length(values), attribute.name, length(edge.names)), stderr())
                
            } else {
                caller.specified.attribute.class = tolower(attribute.type)
                # the switch-block ensures the attribute values have the correct data type
                switch(caller.specified.attribute.class,
                       "floating"=,
                       "numeric"=,
                       "double"={
                           caller.specified.attribute.class = 'Double'
                           values = as.numeric(values)
                       },
                       "integer"=,
                       "int"={
                           caller.specified.attribute.class = "Integer"
                           values = as.integer(values)
                       },
                       "boolean"={
                           caller.specified.attribute.class = "Boolean"
                           values = as.logical(values)
                       },{
                           caller.specified.attribute.class = "String"
                           values = as.character(values)
                       }
                )
                
                if(!attribute.name %in% getEdgeAttributeNames(obj)) {
                    tbl.col = list(name=attribute.name, type=caller.specified.attribute.class)
                    tbl.col.JSON = toJSON(tbl.col)
                    resource.uri = 
                        paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
                    request.res = POST(url=resource.uri, body=tbl.col.JSON, encode="json")
                }
                
                edge.SUIDs = .edgeNameToEdgeSUID(obj, edge.names)
                edge.name.suid.value.df = data.frame(edge.names, edge.SUIDs, values)
                
                edge.SUID.value.pairs = 
                    apply(edge.name.suid.value.df[,c('edge.SUIDs','values')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
                edge.SUID.value.pairs.JSON = toJSON(edge.SUID.value.pairs)
                resource.uri = 
                    paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns", attribute.name, sep="/")
                request.res = PUT(url=resource.uri, body=edge.SUID.value.pairs.JSON, encode="json")
                invisible(request.res)        
            }
        }
}) 
## END setEdgeAttributesDirect

# ------------------------------------------------------------------------------
setMethod('displayGraph', 'CytoscapeWindowClass', function(obj) {
    # needed to simulate 'pass-by-reference' behavior in R
    loc.obj <- obj
    
    if(length(nodes(loc.obj@graph)) == 0) {
        write('RCy3::displayGraph, cannot display empty(0 nodes) graph, returning...', stderr())
        return()
    }
    
    node.count = length(nodes(loc.obj@graph))
    edge.count = length(edgeNames(loc.obj@graph))
    node.attribute.count = length(noa.names(loc.obj@graph)) * node.count
    edge.attribute.count = length(eda.names(loc.obj@graph)) * edge.count
    
    estimated.time = predictTimeToDisplayGraph(loc.obj)
    # if (execution)time measurement option is turned on, save the current time
    if (loc.obj@collectTimings) {
        method.start.time = Sys.time()
        # start time (for sending nodes to Cytoscape) 
        stepwise.start.time = Sys.time()
    }
    
    write(sprintf('estimated displayGraph time: %8.1f seconds', estimated.time), stderr())
    write(sprintf('adding %d nodes...', length(nodes(obj@graph))), stderr())
    
    sendNodes(loc.obj)
    
    if(loc.obj@collectTimings) {
        current.step.exec.time = difftime(Sys.time(), stepwise.start.time, units='secs')
        write(sprintf(' *** sendNodes: %f secs', current.step.exec.time, stderr()))
        # start time (for sending node attributes to Cytoscape)
        stepwise.start.time = Sys.time()
    }
    
    # sends edges to Cytoscape
    write (sprintf ('adding %d edges...', length (edgeNames (loc.obj@graph))), stderr ())
    sendEdges (loc.obj)
    
    if (obj@collectTimings) {
        write (sprintf (' *** sendEdges: %f secs', difftime (Sys.time (), stepwise.start.time, units='secs')), stderr ())
        stepwise.start.time = Sys.time ()
    }
    
    # sending node attributes
    write ('adding node attributes...', stderr ())
    
    # send node attributes from R to Cytoscape
    sapply (noa.names (loc.obj@graph), function (name) {print (name); setNodeAttributes (loc.obj, name)})
    
    if (obj@collectTimings) {
        write (sprintf (' *** send node attributes: %f secs', difftime (Sys.time (), stepwise.start.time, units='secs')), stderr ())
        stepwise.start.time = Sys.time ()
    }
    
    # send edge attributes
    write ('adding edge attributes...', stderr ())
    edgeAttributeNames = eda.names (loc.obj@graph)
    sapply (eda.names (loc.obj@graph), function (name) {print (name); setEdgeAttributes (loc.obj, name)})
    
    if (obj@collectTimings) {
        write (sprintf (' *** send edge attributes: %f secs', difftime (Sys.time (), stepwise.start.time, units='secs')), stderr ())
        stepwise.start.time = Sys.time ()
        actual.time = difftime (Sys.time (), method.start.time, units='secs')
        write (sprintf (' *** leaving displayGraph, predicted duration %f secs,  actual %f secs', as.integer (round (estimated.time)),
                        as.integer (round (actual.time))), stderr ())
    } # if collectTimings
    
    # pseudo R 'pass-by-reference': cw now contains the [node suid,node name] pairs
    eval.parent(substitute(obj <- loc.obj))
}) 
## END displayGraph

# ------------------------------------------------------------------------------
setMethod('predictTimeToDisplayGraph', 'CytoscapeWindowClass', 
    function(obj) {
        g = obj@graph
        node.count = length(nodes(g))
        edge.count = length(edgeNames(g))
        noa.count = length(noa.names(g)) * node.count
        eda.count = length(eda.names(g)) * edge.count
        prediction = (0.002 * node.count) + (0.010 * edge.count) + (0.001 * noa.count) + (0.001 * eda.count)
        return (prediction)
})
## END predictTimeToDisplayGraph

# ------------------------------------------------------------------------------
setMethod('redraw', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "apply/styles", "default", net.SUID, sep = "/")
        request.res <- GET(url=resource.uri)
        invisible(request.res)
}) 
## END redraw

# ------------------------------------------------------------------------------
setMethod('setWindowSize', 'CytoscapeWindowClass', 
    function(obj, width, height) {
        write(sprintf("WARNING: Method RCy3::setWindowSize() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END setWindowSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipInitialDelay', 'CytoscapeConnectionClass',

   function (obj, msecs) {
       write(sprintf("WARNING: Method RCy3::setTooltipInitialDelay() is not implemented in RCy3!"), stderr())
       return(FALSE)
       
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipInitialDelay', as.integer (msecs)))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipDismissDelay', 'CytoscapeConnectionClass',

   function (obj, msecs) {
       write(sprintf("WARNING: Method RCy3::setTooltipDismissDelay() is not implemented in RCy3!"), stderr())
       return(FALSE)
       
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipDismissDelay', as.integer (msecs)))
     })

# ------------------------------------------------------------------------------
setMethod('raiseWindow', 'CytoscapeConnectionClass', 
    function(obj, window.title = NA) {
        write(sprintf("WARNING: Method RCy3::raiseWindow() is not implemented in RCy3!"), stderr())
        return(FALSE)
        
#         if (is.na(window.title)) {
#             if(class(obj) == 'CytoscapeWindowClass') {
#                 window.id = obj@window.id
#             } else {
#                 write(sprintf('error in RCy3::raiseWindow(), no window title provided'), stderr())
#                 return()
#             }
#         } # no window title
#         
#         # if window title was provided
#         if(!is.na(window.title)) {
#             window.id = getWindowID(obj, window.title)
#             
#             if(is.na(window.id)) {
#                 write(sprintf('error in RCy3::raiseWindow(), unrecognized window title: %s', window.title), stderr ())
#                 return()
#             }
#             # TO DO: call to raise the view
#         } # window title was provided
    }) # raiseWindow

#------------------------------------------------------------------------------------------------------------------------
setMethod ('showGraphicsDetails', 'CytoscapeConnectionClass',

    function (obj, new.value) {
        resource.uri <- paste(obj@uri, pluginVersion(obj), "ui/lod/", sep="/")
        request.res <- PUT(resource.uri)
        invisible (request.res)
        if (class (obj) == 'CytoscapeWindowClass'){
            redraw (obj)
        }
        write(sprintf('RCy3::showGraphicsDetails(), Switching between show and hide full graphics details.'), stdout())
        
    })

# ------------------------------------------------------------------------------
# display the graph using all of the available window space (the Cytoscape drawing canvas)
setMethod('fitContent', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        resource.uri <- paste(obj@uri, pluginVersion(obj), "apply/fit", net.SUID, sep="/")
        request.res <- GET(url=resource.uri)
        invisible(request.res)
})
## END fitContent

# ------------------------------------------------------------------------------
setMethod('fitSelectedContent', 'CytoscapeWindowClass', 
    function(obj) {
        write(sprintf("WARNING: Method RCy3::fitSelectedContent() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END fitSelectedContent

# ------------------------------------------------------------------------------
setMethod('getCenter', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        # get all Cytoscape views belonging to that network
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::getCenter() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
        }
        # get the X-coordinate
        resource.uri <- 
            paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_X_LOCATION", sep="/")
        request.res <- GET(resource.uri)
        x.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
        # get the Y-coordinate
        resource.uri <- 
            paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_Y_LOCATION", sep="/")
        request.res <- GET(resource.uri)
        y.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
        
        return(list(x = x.coordinate, y = y.coordinate))
})
## END getCenter

# ------------------------------------------------------------------------------
# this method could be used to pan and scroll the Cytoscape canvas, which is adjusted(moved) 
# so that the specified x and y coordinates are at the center of the visible window.
setMethod('setCenter', 'CytoscapeWindowClass', 
    function(obj, x, y) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::setCenter() - %d views found... setting coordinates of the first one", length(net.views.SUIDs)), stderr())
        }
        
        # set the X-coordinate
        resource.uri <- 
            paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "network", sep="/")
        new.x.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_X_LOCATION", value=x)))
        request.res <- PUT(resource.uri, body=new.x.coordinate.JSON, encode="json")
        # set the Y-coordinate
        resource.uri <- 
            paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "network", sep="/")
        new.y.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_Y_LOCATION", value=y)))
        request.res <- PUT(resource.uri, body=new.y.coordinate.JSON, encode="json")
        invisible(request.res)
})
## END setCenter

# ------------------------------------------------------------------------------
setMethod('getZoom', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        # get the existing views for the given network model
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
        }
        
        resource.uri <- 
            paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "network/NETWORK_SCALE_FACTOR", sep="/")
        request.res <- GET(resource.uri)
        zoom.level <- fromJSON(rawToChar(request.res$content))$value[[1]]
        
        return(zoom.level)
})
## END getZoom

# ------------------------------------------------------------------------------
setMethod('setZoom', 'CytoscapeWindowClass', 
    function(obj, new.level) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first view", length(net.views.SUIDs)), stderr())
        }
        
        view.zoom.value <- list(visualProperty='NETWORK_SCALE_FACTOR', value=new.level)
        view.zoom.value.JSON <- toJSON(list(view.zoom.value))
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "network", sep="/")
        request.res <- PUT(url=resource.uri, body=view.zoom.value.JSON, encode="json")
        
        invisible(request.res)
})
## END setZoom

# ------------------------------------------------------------------------------
setMethod('getViewCoordinates', 'CytoscapeWindowClass', 
    function(obj) {
        write(sprintf("WARNING: Method RCy3::getViewCoordinates() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END getViewCoordinates

# ------------------------------------------------------------------------------
setMethod('hidePanel', 'CytoscapeConnectionClass', 
    function(obj, panelName) {
        version <- pluginVersion(obj)
        
        if (tolower(panelName) %in% c('data panel', 'd', 'data', 'da')){
            panelName <- 'SOUTH'
        }else if (tolower(panelName) %in% c('control panel', 'control', 'c', 'co')){
            panelName <- 'WEST'
        }else if (!(panelName %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
            write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
            return(NA)
        }
        
        panel.name.state = list(name=panelName, state='HIDE')
        
        resource.uri <- paste(obj@uri, version, "ui/panels", sep="/")
        request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
        
        invisible(request.res)
})
## END hidePanel

# ------------------------------------------------------------------------------
setMethod('hideAllPanels', 'CytoscapeConnectionClass', 
    function(obj) {
        hidePanel(obj, "SOUTH")
        hidePanel(obj, "EAST")
        hidePanel(obj, "WEST")
        hidePanel(obj, "SOUTH_WEST")
})
## END hideAllPanels

# ------------------------------------------------------------------------------
setMethod('dockPanel', 'CytoscapeConnectionClass', 
    function(obj, panelName) {
        version <- pluginVersion(obj)

        if (tolower(panelName) %in% c('data panel', 'd', 'data', 'da')){
            panelName <- 'SOUTH'
        }else if (tolower(panelName) %in% c('control panel', 'control', 'c', 'co')){
            panelName <- 'WEST'
        }else if (!(panelName %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
            write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
            return(NA)
        }
        
        panel.name.state = list(name=panelName, state='DOCK')
        
        resource.uri <- paste(obj@uri, version, "ui/panels", sep="/")
        request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
        
        invisible(request.res)
})
## END dockPanel

# ------------------------------------------------------------------------------
setMethod('floatPanel', 'CytoscapeConnectionClass', 
    function(obj, panelName) {
        version <- pluginVersion(obj)
        
        if (tolower(panelName) %in% c('data panel', 'd', 'data', 'da')){
            panelName <- 'SOUTH'
        }else if (tolower(panelName) %in% c('control panel', 'control', 'c', 'co')){
            panelName <- 'WEST'
        }else if (!(panelName %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
            write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
            return(NA)
        }
        
        panel.name.state = list(name=panelName, state='FLOAT')
        
        resource.uri <- paste(obj@uri, version, "ui/panels", sep="/")
        request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
        
        invisible(request.res)
})
## END floatPanel

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeTooltipRule', 'CytoscapeWindowClass',

      function (obj, node.attribute.name) {
          id <- as.character (obj@window.id)
          viz.style.name = 'default'
          if (!node.attribute.name %in% noa.names (obj@graph)) {
              write (sprintf ('Warning! RCy3::setNodeTooltipRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
              return ()
          }
          attribute.values = noa (obj@graph, node.attribute.name)
          
          # set default tooltip
          default.tooltip <- list(visualProperty = "NODE_TOOLTIP", value = "")
          setVisualProperty(obj, default.tooltip, viz.style.name)
          
          # define the column type
          sample.node.attribute <- getNodeAttribute (obj, getAllNodes(obj)[1], node.attribute.name)
          columnType <- findColumnType(typeof(sample.node.attribute))

          # discrete mapping
          discreteMapping(obj, node.attribute.name, attribute.values, attribute.values,
                          visual.property="NODE_TOOLTIP", columnType=columnType, style=viz.style.name)
          
    })  # END setNodeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipRule', 'CytoscapeWindowClass',

    function (obj, edge.attribute.name) {
        id = as.character (obj@window.id)
        viz.style.name = 'default'
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('warning!  setEdgeTooltipRule passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        attribute.values = as.character (eda (obj@graph, edge.attribute.name))
        
        # set default tooltip
        default.tooltip <- list(visualProperty = "EDGE_TOOLTIP", value = "")
        setVisualProperty(obj, default.tooltip, viz.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(attribute.values[1]))
        
        # discrete mapping
        discreteMapping(obj, edge.attribute.name, attribute.values, attribute.values,
                        visual.property="EDGE_TOOLTIP", columnType=columnType, style=viz.style.name)

    })  # setEdgeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelRule', 'CytoscapeWindowClass',
    function (obj, node.attribute.name) {
        id = as.character (obj@window.id)
        vizmap.style.name = 'default'
        if (!node.attribute.name %in% noa.names (obj@graph)) {
            write (sprintf ('warning!  setNodeLabelRule passed non-existent node attribute: %s', node.attribute.name), stderr ())
            return ()
        }
        attribute.values = as.character (noa (obj@graph, node.attribute.name))
        
        # set default label
        default.label <- list(visualProperty = "NODE_LABEL", value = "")
        setVisualProperty(obj, default.label, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(attribute.values[1]))
        
        # discrete mapping
        discreteMapping(obj, node.attribute.name, attribute.values, attribute.values,
                        visual.property="NODE_LABEL", columnType=columnType, style=vizmap.style.name)
    })  # setNodeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelRule', 'CytoscapeWindowClass',

    function (obj, edge.attribute.name) {
        id = as.character (obj@window.id)
        vizmap.style.name = 'default'
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('warning!  setEdgeLabelRule passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        attribute.values = as.character (eda (obj@graph, edge.attribute.name))
        
        # set default label
        default.label <- list(visualProperty = "EDGE_LABEL", value = "")
        setVisualProperty(obj, default.label, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(attribute.values[1]))
        
        # discrete mapping
        discreteMapping(obj, edge.attribute.name, attribute.values, attribute.values,
                        visual.property="EDGE_LABEL", columnType=columnType, style=vizmap.style.name)
    })  # setEdgeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorRule', 'CytoscapeWindowClass',
           
           function (obj, node.attribute.name, control.points, colors, mode, default.color='#FFFFFF') {
               if (!mode %in% c ('interpolate', 'lookup')) {
                   write ("Error! RCy3:setNodeColorRule. Mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
                   return ()
               }
               
               # check if colors are formatted correctly
               for (color in colors){
                   if (.isNotHexColor(color)){
                       return()
                   } 
               }
               
               #TODO Comment TanjaM we should give the user the option to choose the style as an input parameter which defaults to default.
               vizmap.style.name = 'default'
               
               #set default
               setDefaultNodeColor (obj, default.color, vizmap.style.name)
               
               # define the column type
               columnType <- findColumnType(typeof(control.points[1]))

               # interpolate
               if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
                   if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
                       colors = c (colors [1], colors, colors [length (colors)])
                       write ("RCy3::setNodeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
                } #
                good.args = length (control.points) == (length (colors) - 2)
                if (!good.args) {
                    write (sprintf ('cp: %d', length (control.points)), stderr ())
                    write (sprintf ('co: %d', length (colors)), stderr ())
                    write ("Error! RCy3:setNodeColorRule, interpolate mode.", stderr ())
                    write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
                    return ()
                }
                
                continuousMapping (obj, node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=vizmap.style.name)
                 
                } # if mode==interpolate
                else { # use a discrete rule, with no interpolation, mode==lookup
                   good.args = length (control.points) == length (colors)
                   if (!good.args) {
                       write (sprintf ('control points: %d', length (control.points)), stderr ())
                       write (sprintf ('colors: %d', length (colors)), stderr ())
                       write ("Error! RCy3:setNodeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                       return ()
                }
                   
                discreteMapping(obj, node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=vizmap.style.name)    
              
                } # else: !interpolate, aka lookup
     }) # setNodeColorRule

#------------------------------------------------------------------------------------------------------------------------
# Cytoscape distinguishes between Node Opacity, Node Border Opacity, and Node Label Opacity.  we call this 'aspect' here.

setMethod ('setNodeOpacityRule', 'CytoscapeWindowClass',

    function (obj, node.attribute.name, control.points, opacities, mode, aspect='all') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setNodeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        #TODO Comment TanjaM we should give the user the option to choose the style 
        # as an input parameter which defaults to default.
        vizmap.style.name = 'default'

        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        # set default # Comment TanjaM: Current version does not set default
        #setDefaultNodeOpacity (obj, default.opacity, vizmap.style.name)
        
        aspect.all = length (grep ('all', aspect))  > 0
        aspect.fill = length (grep ('fill', aspect)) > 0
        aspect.border = length (grep ('border', aspect)) > 0
        aspect.label = length (grep ('label', aspect)) > 0
        
        if (aspect.all) {
            aspect.fill = TRUE
            aspect.border = TRUE
            aspect.label = TRUE
        }
        
        if (aspect.fill == FALSE && aspect.border == FALSE && aspect.label == FALSE) {
            specific.options = 'fill, border, label'
            msg.1 = "Error! RCy3:setNodeOpacityRule. Aspect must be 'all' (the default) "
            msg.2 = sprintf ("or some combination, in any order, of %s", specific.options)
            write (msg.1, stderr ())
            write (msg.2, stderr ())
            return ()
        }

        if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
            if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
                opacities = c (opacities [1], opacities, opacities [length (opacities)])
                write ("RCy3::setNodeOpacityRule, no 'below' or 'above' opacities specified.  Inferred from supplied opacities.", stderr ());
            }
            
            good.args = length (control.points) == (length (opacities) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (opacities)), stderr ())
                write ("Error! RCy3:setNodeOpacityRule, interpolate mode.", stderr ())
                write ("Expecting 1 opacity for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
                return ()
            }
            
            if (aspect.fill){
                continuousMapping (obj, node.attribute.name, control.points, opacities,
                                   visual.property="NODE_TRANSPARENCY",
                                   columnType=columnType, style=vizmap.style.name)
            }
            if (aspect.border){
                continuousMapping (obj, node.attribute.name, control.points, opacities,
                                   visual.property="NODE_BORDER_TRANSPARENCY",
                                   columnType=columnType, style=vizmap.style.name)
            }
            if (aspect.label){
                continuousMapping (obj, node.attribute.name, control.points, opacities,
                                   visual.property="NODE_LABEL_TRANSPARENCY",
                                   columnType=columnType, style=vizmap.style.name)
            }
        } # if mode==interpolate
        
        else { # mode==lookup, use a discrete rule, with no interpolation
            good.args = length (control.points) == length (opacities)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (opacities)), stderr ())
                write ("Error! RCy3:setNodeOpacityRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
                return ()
            }
            
            default.opacity = 255;
            if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
                control.points = rep (control.points, 2)
                opacities = rep (opacities, 2)
            }
            
            if (aspect.fill){
                discreteMapping(obj, node.attribute.name, control.points, opacities,
                                visual.property="NODE_TRANSPARENCY",
                                columnType=columnType, style=vizmap.style.name)
            }
            
            if (aspect.border){
                discreteMapping(obj, node.attribute.name, control.points, opacities,
                                visual.property="NODE_BORDER_TRANSPARENCY",
                                columnType=columnType, style=vizmap.style.name)
            }
            
            if (aspect.label){
                discreteMapping(obj, node.attribute.name, control.points, opacities,
                                visual.property="NODE_LABEL_TRANSPARENCY",
                                columnType=columnType, style=vizmap.style.name)
            }
        } # else: !interpolate
     }) # setNodeOpacityRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorRule', 'CytoscapeWindowClass',

    function (obj, node.attribute.name, control.points, colors, mode, default.color='#000000') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setNodeBorderColorRule. Mode must be 'interpolate' or 'lookup'.", stderr ())
            return ()
        }
        
        # check if colors are formatted correctly
        for (color in colors){
            if (.isNotHexColor(color)){
                return()
            } 
        }
        
        #TODO Comment TanjaM we should give the user the option to choose the style as an input parameter which defaults to default.
        vizmap.style.name = 'default'
        
        # set default
        setDefaultNodeBorderColor (obj, default.color, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        # mode==interpolate
        if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
            if (length (control.points) == length (colors)){  # caller did not supply 'below' and 'above' values; manufacture them
                colors = c (default.color, colors, default.color)
            }
            good.args = length (control.points) == (length (colors) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setNodeBorderColorRule, interpolate mode.", stderr ())
                write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
                return ()
            }
            # continous mapping
            continuousMapping (obj, node.attribute.name, control.points, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=vizmap.style.name)

        } # if mode==interpolate
        else { # use a discrete rule, with no interpolation
            good.args = length (control.points) == length (colors)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setNodeBorderColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                return ()
            }
            discreteMapping(obj, node.attribute.name, control.points, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=vizmap.style.name)
        } # else: !interpolate
     }) # setNodeBorderColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, line.widths, default.width=1) {
       id = as.character (obj@window.id)
       #TODO the style should be passed as a parameter
       vizmap.style.name = 'default'
       #TODO we should add interpolate as mode in the function
       mode = "lookup"
       if (!node.attribute.name %in% noa.names (obj@graph)) {
           write (sprintf ('warning!  setNodeBorderWidthRule passed non-existent node attribute: %s', node.attribute.name), stderr ())
           return ()
       }
       
       # set default
       setDefaultNodeBorderWidth(obj, default.width, vizmap.style.name)
       
       # define the column type
       columnType <- "String" #findColumnType(typeof(line.widths[1]))
       # discrete mapping
       if (mode=="lookup"){
           discreteMapping (obj, node.attribute.name, attribute.values, line.widths,
                       visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=vizmap.style.name)
       } else{
           # continuous mapping
           # TODO need to check here if 2 more values were passed in for width
           continuousMapping (obj, node.attribute.name, attribute.values, line.widths, visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=vizmap.style.name)
       }
     })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeShape', 'CytoscapeConnectionClass', 
  function(obj, new.shape, vizmap.style.name='default') {
      new.shape <- toupper(new.shape)
      if (new.shape %in% getNodeShapes(obj)){
          style = list(visualProperty = "NODE_SHAPE", value = new.shape)
          setVisualProperty(obj, style, vizmap.style.name)
      }else{
          write (sprintf ('%s is not a valid shape. Use getNodeShapes() to find valid values.', new.shape), stderr ())
      }
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSize', 'CytoscapeConnectionClass', 
    function(obj, new.size, vizmap.style.name='default') {
        # lock node dimensions
        lockNodeDimensions (obj, TRUE)
        
        style <- list(visualProperty = "NODE_SIZE", value = new.size)
        setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeColor', 'CytoscapeConnectionClass', 
  function(obj, new.color, vizmap.style.name='default') {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeBorderColor', 'CytoscapeConnectionClass', 
  function(obj, new.color, vizmap.style.name='default') {
    if (.isNotHexColor(new.color)){
      return()
    }
    style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderWidth', 'CytoscapeConnectionClass', 
  function(obj, new.width, vizmap.style.name='default') {
    style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeFontSize', 'CytoscapeConnectionClass', 
  function(obj, new.size, vizmap.style.name='default') {
    style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeLabelColor', 'CytoscapeConnectionClass', 
  function(obj, new.color, vizmap.style.name='default') {
    if (.isNotHexColor(new.color)){
      return()
    }      
    style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeLineWidth', 'CytoscapeConnectionClass', 
  function(obj, new.width, vizmap.style.name='default') {
    style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeColor', 'CytoscapeConnectionClass', 
  function(obj, new.color, vizmap.style.name='default') {
    if (.isNotHexColor(new.color)){
      return()
    }
     # TODO Comment Tanja: maybe change to EDGE_UNSELECTED_PAINT
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setVisualProperty(obj, style, vizmap.style.name)
})

setMethod('setDefaultEdgeSourceArrowColor', 'CytoscapeConnectionClass', 
          function(obj, new.color, vizmap.style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, vizmap.style.name)
          })

setMethod('setDefaultEdgeTargetArrowColor', 'CytoscapeConnectionClass', 
          function(obj, new.color, vizmap.style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, vizmap.style.name)
          })
# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeFontSize', 'CytoscapeConnectionClass', 
  function(obj, new.size, vizmap.style.name='default') {
    style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
    setVisualProperty(obj, style, vizmap.style.name)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeRule', 'CytoscapeWindowClass',

    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ELLIPSE') {
        id = as.character (obj@window.id)
        
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'

        if (!node.attribute.name %in% noa.names (obj@graph)) {
            write (sprintf ('Error in RCy3::setNodeShapeRule. Passed non-existent node attribute: %s', node.attribute.name), stderr ())
            return ()
        }
        
        # ensure correct node shapes
        node.shapes <- toupper(node.shapes)
        unique.node.shapes <- unique(node.shapes)
        wrong.node.shape <- sapply(unique.node.shapes, function(x) !(x %in% getNodeShapes(obj)))
        if (any(wrong.node.shape)){
            write (sprintf('ERROR in RCy3::setNodeShapeRule. You tried to use invalid node shapes. For valid ones use getNodeShapes'), stderr())
            return(NA)
        }

        # set default
        setDefaultNodeShape (obj, default.shape, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(node.shapes[1]))
        
        # discrete mapping
        discreteMapping (obj, node.attribute.name, attribute.values, node.shapes,
                             visual.property="NODE_SHAPE", columnType=columnType, style=vizmap.style.name)
     }) # setNodeShapeRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeSizeRule', 'CytoscapeWindowClass',

    function (obj, node.attribute.name, control.points, node.sizes, mode, default.size=40) {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setNodeSizeRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }

        #TODO Comment TanjaM we should give the user the option to choose the style 
        # as an input parameter which defaults to default.
        vizmap.style.name = 'default'
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))

        # lock node dimensions
        lockNodeDimensions (obj, TRUE)
        
        # set default
        setDefaultNodeSize (obj, default.size, vizmap.style.name)
        
        if (mode=='interpolate') {  # need a 'below' size and an 'above' size.  so there should be two more colors than control.points
            if (length (control.points) == length (node.sizes)) { # caller did not supply 'below' and 'above' values; manufacture them
                node.sizes = c (node.sizes [1], node.sizes, node.sizes [length (node.sizes)])
                write ("RCy3::setNodeSizeRule, no 'below' or 'above' sizes specified.  Inferred from node.sizes.", stderr ())
            }

            good.args = length (control.points) == (length (node.sizes) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (node.sizes)), stderr ())
                write ("Error! RCy3:setNodeSizeRule, interpolate mode.", stderr ())
                write ("Expecting 1 node.size for each control.point, one for 'above' size, one for 'below' size.", stderr ())
                return ()
            }
            continuousMapping (obj, node.attribute.name, control.points, node.sizes,
                               visual.property="NODE_SIZE",
                               columnType=columnType, style=vizmap.style.name)
            
        } # if mode==interpolate

        else { # use a discrete rule, with no interpolation
            good.args = length (control.points) == length (node.sizes)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (node.sizes)), stderr ())
                write ("Error! RCy3:setNodeSizeRule. Expecting exactly as many node.sizes as control.points in lookup mode.", stderr ())
                return ()
            }
            discreteMapping(obj, node.attribute.name, control.points, node.sizes,
                            visual.property="NODE_SIZE",
                            columnType=columnType, style=vizmap.style.name)
        } # else: !interpolate, aka lookup
        
    }) # setNodeSizeRule
#
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorRule', 'CytoscapeWindowClass',

    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#FFFFFF') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setEdgeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        # check if colors are formatted correctly
        for (color in colors){
            if (.isNotHexColor(color)){
                return()
            } 
        }
        
        #TODO Comment TanjaM we should give the user the option to choose the style 
        # as an input parameter which defaults to default.
        vizmap.style.name = 'default'
        
        #set default
        setDefaultEdgeColor (obj, default.color, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points
            if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
                colors = c (colors [1], colors, colors [length (colors)])
                write ("RCy3::setEdgeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
            } 
        good.args = length (control.points) == (length (colors) - 2)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeColorRule, interpolate mode.", stderr ())
            write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
            return ()
            }
        
        continuousMapping (obj, edge.attribute.name, control.points, colors,
                           visual.property="EDGE_STROKE_UNSELECTED_PAINT",
                           columnType=columnType, style=vizmap.style.name)
        } # if mode==interpolate
        else { # use a discrete rule, with no interpolation, mode==lookup
        good.args = length (control.points) == length (colors)
        if (!good.args) {
            write (sprintf ('cp: %d', length (control.points)), stderr ())
            write (sprintf ('co: %d', length (colors)), stderr ())
            write ("Error! RCy3:setEdgeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
            return ()
        }
        
        discreteMapping(obj, edge.attribute.name, control.points, colors,
                        visual.property="EDGE_STROKE_UNSELECTED_PAINT",
                        columnType=columnType, style=vizmap.style.name)
        } # else: !interpolate, aka lookup
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeOpacityRule', 'CytoscapeWindowClass',

    function (obj, edge.attribute.name, control.points, opacities, mode) {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setEdgeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        #TODO Comment TanjaM we should give the user the option to choose the style 
        # as an input parameter which defaults to default.
        vizmap.style.name = 'default'
        
        # set default # Comment TanjaM: Current version does not set default
        #setDefaultEdgeOpacity (obj, default.opacity, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        # in a previous Cytoscape version the three elements were set seperately
        #aspects = c ('Edge Opacity', 'Edge Target Arrow Opacity', 'Edge Source Arrow Opacity')

        if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
            if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
                opacities = c (opacities [1], opacities, opacities [length (opacities)])
                write ("RCy3::setEdgeOpacityRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
            } 
            good.args = length (control.points) == (length (opacities) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (opacities)), stderr ())
                write ("Error! RCy3:setEdgeOpacityRule, interpolate mode.", stderr ())
                write ("Expecting 1 opacity value for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
                return ()
            }
            continuousMapping (obj, edge.attribute.name, control.points, opacities,
                                   visual.property="EDGE_TRANSPARENCY",
                                   columnType=columnType, style=vizmap.style.name)
        } # if mode==interpolate
        else { # use a discrete rule, with no interpolation
            good.args = length (control.points) == length (opacities)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (opacities)), stderr ())
                write ("Error! RCy3:setEdgeColorRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
                return ()
            }
            discreteMapping(obj, edge.attribute.name, control.points, opacities,
                            visual.property="EDGE_TRANSPARENCY",
                            columnType=columnType, style=vizmap.style.name)
        } # else: !interpolate
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleRule', 'CytoscapeWindowClass',

    function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') {
        id = as.character (obj@window.id)
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'
        
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('warning!  setEdgeLineStyleRule passed non-existent node attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # ensure correct values
        line.styles <- toupper(line.styles)
        unique.values <- unique(line.styles)
        wrong.values <- sapply(unique.values, function(x) !(x %in% getLineStyles(obj)))
        if (any(wrong.values)){
            write (sprintf ('ERROR in RCy3::setEdgeLineStyleRule. Invalid value. For valid values use getLineStyles'), stderr ())
            return(NA)
        }
        
        # set default
        default.style.list <- list(visualProperty = "EDGE_LINE_TYPE", value = default.style)
        setVisualProperty(obj, default.style.list, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(line.styles[1]))
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, line.styles,
                         visual.property="EDGE_LINE_TYPE", columnType=columnType, style=vizmap.style.name)
     }) # setEdgeLineStyleRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineWidthRule', 'CytoscapeWindowClass',

    function (obj, edge.attribute.name, attribute.values, line.widths, default.width=1) {
        id = as.character (obj@window.id)
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'
        
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('Warning! setEdgeLineWidthRule passed non-existent node attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # set default
        default.width.list <- list(visualProperty = "EDGE_WIDTH", value = default.width)
        setVisualProperty(obj, default.width.list, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(line.widths[1]))
        #columnType <- 'String'
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, line.widths,
                         visual.property="EDGE_WIDTH", columnType=columnType, style=vizmap.style.name)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'CytoscapeWindowClass', 

    function (obj, edge.attribute.name, attribute.values, arrows, default='ARROW') {
        id = as.character (obj@window.id)
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'
        
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('Warning! setEdgeTargetArrowRule passed non-existent node attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # set default
        default.style.list <- list(visualProperty = "EDGE_TARGET_ARROW_SHAPE", value = default)
        setVisualProperty(obj, default.style.list, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(arrows[1]))
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, arrows,
                         visual.property="EDGE_TARGET_ARROW_SHAPE", columnType=columnType, style=vizmap.style.name)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowRule', 'CytoscapeWindowClass', 

    function (obj, edge.attribute.name, attribute.values, arrows, default='ARROW') {
        id = as.character (obj@window.id)
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'
        
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('warning!  setEdgeSourceArrowRule passed non-existent node attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # set default
        default.style.list <- list(visualProperty = "EDGE_SOURCE_ARROW_SHAPE", value = default)
        setVisualProperty(obj, default.style.list, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(arrows[1]))
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, arrows,
                         visual.property="EDGE_SOURCE_ARROW_SHAPE", columnType=columnType, style=vizmap.style.name)
    }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorRule', 'CytoscapeWindowClass', 

    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setEdgeTargetArrowColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        # check if colors are formatted correctly
        for (color in colors){
            if (.isNotHexColor(color)){
                return()
            } 
        }
        
        #TODO Comment TanjaM we should give the user the option to choose the style 
        # as an input parameter which defaults to default.
        vizmap.style.name = 'default'
        
        #set default
        setDefaultEdgeTargetArrowColor (obj, default.color, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points
            if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
                colors = c (colors [1], colors, colors [length (colors)])
                write ("RCy3::setEdgeTargetArrowColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
            } 
            good.args = length (control.points) == (length (colors) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setEdgeTargetArrowColorRule, interpolate mode.", stderr ())
                write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
                return ()
           }
           
           continuousMapping (obj, edge.attribute.name, control.points, colors,
                              visual.property="EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                              columnType=columnType, style=vizmap.style.name)
        } # if mode==interpolate
        else { # use a discrete rule, with no interpolation, mode==lookup
            good.args = length (control.points) == length (colors)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setEdgeTargetArrowColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                return ()
            }
           
            discreteMapping(obj, edge.attribute.name, control.points, colors,
                            visual.property="EDGE_TARGET_ARROW_UNSELECTED_PAINT",
                            columnType=columnType, style=vizmap.style.name)
        } # else: !interpolate, aka lookup
    }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorRule', 'CytoscapeWindowClass', 

    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000') {

        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setEdgeSourceArrowColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        # check if colors are formatted correctly
        for (color in colors){
            if (.isNotHexColor(color)){
                return()
            } 
        }
        
        #TODO Comment TanjaM we should give the user the option to choose the style 
        # as an input parameter which defaults to default.
        vizmap.style.name = 'default'
        
        #set default
        setDefaultEdgeSourceArrowColor (obj, default.color, vizmap.style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        
        if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points
            if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
                colors = c (colors [1], colors, colors [length (colors)])
                write ("RCy3::setEdgeSourceArrowColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
            } 
            good.args = length (control.points) == (length (colors) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setEdgeSourceArrowColorRule, interpolate mode.", stderr ())
                write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
                return ()
            }
            
            continuousMapping (obj, edge.attribute.name, control.points, colors,
                               visual.property="EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                               columnType=columnType, style=vizmap.style.name)
        } # if mode==interpolate
        else { # use a discrete rule, with no interpolation, mode==lookup
            good.args = length (control.points) == length (colors)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setEdgeSourceArrowColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                return ()
            }
            
            discreteMapping(obj, edge.attribute.name, control.points, colors,
                            visual.property="EDGE_SOURCE_ARROW_UNSELECTED_PAINT",
                            columnType=columnType, style=vizmap.style.name)
        } # else: !interpolate, aka lookup
        
     }) # setEdgeSourceArrowColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.colors) {
      for (current.color in new.colors){
         # ensure the new color string is in correct hexadecimal format
         if (.isNotHexColor(current.color)){
             return()
         } 
      }
      # set the node color direct
      return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_FILL_COLOR"))
     })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are unlocked (that is not tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeSizeDirect', 'CytoscapeWindowClass',
    function (obj, node.names, new.sizes) {
        # unlock node dimensions
        lockNodeDimensions (obj, FALSE)
        
        for (current.size in new.sizes){
            # ensure the sizes are numbers
            if (!is.double(current.size)) {
                write (sprintf ('illegal size string "%s" in RCy3::setNodeSizeDirect. It needs to be a number.', current.size), stderr ())
                return ()
            }
        }
        # set the node properties direct
        setNodePropertyDirect(obj, node.names, new.sizes, "NODE_WIDTH")
        setNodePropertyDirect(obj, node.names, new.sizes, "NODE_HEIGHT")
    })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is not tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeWidthDirect', 'CytoscapeWindowClass',
    function (obj, node.names, new.widths) {
        # unlock node dimensions
        lockNodeDimensions (obj, FALSE)
        
        for (current.width in new.widths){
            # ensure the width(s) are numbers
            if (!is.double(current.width)) {
                write (sprintf ('illegal node width "%s" in RCy3::setNodeWidthDirect. Width needs to be a number.', current.width), stderr ())
                return ()
            }
        }
        # set the node property direct
        return(setNodePropertyDirect(obj, node.names, new.widths, "NODE_WIDTH"))
    })

#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeHeightDirect', 'CytoscapeWindowClass',
    function (obj, node.names, new.heights) { # Comment Tanja: Could pass in visual style here
        # unlock node dimensions
        lockNodeDimensions (obj, FALSE)
        
        for (current.height in new.heights){
            # ensure the height(s) are numbers
            if (!is.double(current.height)) {
                write (sprintf ('illegal height string "%s" in RCy3::setNodeHeightDirect. It needs to be a number.', current.height), stderr ())
                return ()
            }
        }
        # set the node property direct
        return(setNodePropertyDirect(obj, node.names, new.heights, "NODE_HEIGHT"))
    })

# ------------------------------------------------------------------------------
setMethod('setNodeLabelDirect', 'CytoscapeWindowClass', 
    function(obj, node.names, new.labels) {
        setNodePropertyDirect(obj, node.names, new.labels, "NODE_LABEL")
})
## END setNodeLabelDirect

# ------------------------------------------------------------------------------
setMethod('setNodeFontSizeDirect', 'CytoscapeWindowClass', 
    function(obj, node.names, new.sizes) {
        size.type.errors = 0
        
        for(current.size in new.sizes) {
            if(!is.double(current.size)) {
                write(sprintf("ERROR in RCy3::setNodeFontSizeDirect():\n\t font size '%s' has to be numerical value", current.size), stderr())
                
                size.type.errors = size.type.errors + 1
            }
        }
        
        if(size.type.errors < 1) {
            setNodePropertyDirect(obj, node.names, new.sizes, "NODE_LABEL_FONT_SIZE")
        }
})
## END setNodeFontSizeDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.colors) {
      for (current.color in new.colors){
        # ensure the color is formated in the correct hexadecimal style
        if (.isNotHexColor(current.color)){
          return()
        } 
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_LABEL_COLOR"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeDirect', 'CytoscapeWindowClass',
    function (obj, node.names, new.shapes) {
        if (length (node.names) != length (new.shapes)) {
            if (length(new.shapes) != 1){
                msg = sprintf ('error in RCy3::setNodeShapeDirect.  new.shapes count (%d) is neither 1 nor same as node.names count (%d)',
                               length (new.shapes), length (node.names))
                write (msg, stderr ())
                return ()
            }
        }
        
        # convert old to new node shapes
        new.shapes[new.shapes=='round_rect'] <- 'ROUND_RECTANGLE'
        new.shapes[new.shapes=='rect'] <- 'RECTANGLE'
        
        # ensure correct node shapes
        new.shapes <- toupper(new.shapes)
        unique.node.shapes <- unique(new.shapes)
        wrong.node.shape <- sapply(unique.node.shapes, function(x) !(x %in% getNodeShapes(obj)))
        if (any(wrong.node.shape)){
            write (sprintf ('ERROR in RCy3::setNodeShapeDirect. %s is not a valid shape. Please note that some older shapes are no longer available. For valid ones check getNodeShapes.', new.shapes), stderr ())
            return(NA)
        }
        # set the node property direct
        return(setNodePropertyDirect(obj, node.names, new.shapes, "NODE_SHAPE"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeImageDirect', 'CytoscapeWindowClass',

    function (obj, node.names, image.positions) {
        
        write(sprintf("WARNING: Method RCy3::setNodeImageDirect() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
        
        #THIS WILL NOT BE EXECUTED
        # insert a warning 
        if (!is.numeric(image.positions)){
            msg = sprintf ('Error in RCy3::setNodeImageDirect. Note that image urls are no longer supported. Upload your image into the Image Manager in the style tab in the control panel and report its position in the Image Manager as number.')
            write (msg, stderr ())
            return()
        }
        
        if (length (node.names) != length (image.positions)) {
            if (length (image.positions) == 1){
                msg = sprintf ('Error in RCy3::setNodeImageDirect. image.positions count (%d) is neither 1 nor same as node.names count (%d)',
                               length (image.positions), length (node.names))
                write (msg, stderr ())
                return ()
            }
        }
        # only allow for upto 9 custom graphics
        if ((length(unique(image.positions)))>9){
            msg = sprintf ('Error in RCy3::setNodeImageDirect. Cytoscape only supports upto 9 custom graphics.')
            write (msg, stderr ())
            return()
        }
        
        # TODO check if enough open spaces
        # get node images from properties
        
        # pseudo code:
        # for loop
        # if is == "org.cytoscape.ding.customgraphics.NullCustomGraphics,0,[ Remove Graphics ],"
        # if "NODE_CUSTOMGRAPHICS_9" passed and still not: error message
        # not working: return(setNodePropertyDirect(obj, node.names, image.urls, "NODE_CUSTOMGRAPHICS_1"))
        return(setNodePropertyDirect(obj, node.names, paste0("org.cytoscape.ding.customgraphics.bitmap.URLImageCustomGraphics,", image.positions, ",bundle"), "NODE_CUSTOMGRAPHICS_1"))
        
        # test code to remove an image
        # setNodePropertyDirect(obj, node.names, "org.cytoscape.ding.customgraphics.NullCustomGraphics,0,[ Remove Graphics ],", "NODE_CUSTOMGRAPHICS_1")
        
#         
#         # the below code is from a previous RCytoscape version
#         for (i in 1:length (node.names)) {
#             setNodeShapeDirect (obj, node.names [i], 'rect')
#             setNodeLabelDirect (obj, node.names [i], '')
#             result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names [i], 'Node Custom Graphics 1', image.urls [i])
#         }
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.sizes) {
      for (current.size in new.sizes){
         # ensure the widths are numbers
         if (!is.double(current.size)) {
            write (sprintf ('illegal width string "%s" in RCy3::setNodeBorderWidthDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.sizes, "NODE_BORDER_WIDTH"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.colors) {
       # ensure the color is formated in correct hexadecimal style
       for (color in new.colors){
           if (.isNotHexColor(color)){
               return()
           } 
       }
      # set the node border color direct
      return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_BORDER_PAINT"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeOpacityDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.values) {
      for (current.value in new.values){
         # ensure the opacity value is a double and between 0 and 255
         if (! is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('RCy3::setNodeOpacityDirect: illegal opacity string "%s". It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      setNodePropertyDirect(obj, node.names, new.values, "NODE_TRANSPARENCY")
      setNodePropertyDirect(obj, node.names, new.values, "NODE_BORDER_TRANSPARENCY")
      setNodePropertyDirect(obj, node.names, new.values, "NODE_LABEL_TRANSPARENCY")
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeFillOpacityDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.values) {
      for (current.value in new.values){
         # ensure the opacity value is between 0 and 255
         if (!is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('illegal opacity string "%s" in RCy3::setNodeFillOpacityDirect. It needs to be a double and between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      # set the node border color direct
      return(setNodePropertyDirect(obj, node.names, new.values, "NODE_TRANSPARENCY"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderOpacityDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.values) {
      for (current.value in new.values){
         # ensure the opacity value is a double and between 0 and 255
         if (! is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('illegal opacity string "%s" in RCy3::setNodeBorderOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.values, "NODE_BORDER_TRANSPARENCY"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelOpacityDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.values) {
      for (current.value in new.values){
         # ensure the opacity value is a double and between 0 and 255
         if (! is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('illegal opacity string "%s" in RCy3::setNodeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.values, "NODE_LABEL_TRANSPARENCY"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeOpacityDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
      for (current.value in new.values){
         # ensure the opacity value is a double and between 0 and 255
         if (! is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('illegal opacity string "%s" in RCy3::setEdgeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      # set the edge property direct
      #     property.names = c ('Edge Opacity',  'Edge Source Arrow Opacity', 'Edge Target Arrow Opacity')
      setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_LABEL_TRANSPARENCY")
      setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TRANSPARENCY")
     })

# ------------------------------------------------------------------------------
setMethod('setEdgeColorDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
       # ensure the color is formated in correct hexadecimal style
       for (color in new.value){
           if (.isNotHexColor(color)){
               return()
           } 
       }
      # set the edge color direct
      # TODO maybe this should be EDGE_PAINT
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_STROKE_UNSELECTED_PAINT"))
})

# ------------------------------------------------------------------------------
setMethod('setEdgeLabelDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.value) {
        setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL")
})
## END setEdgeLabelDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeFontFaceDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.value) {
        setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_FACE")
})
## END setEdgeFontFaceDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeFontSizeDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.value) {
        size.type.errors = 0
        
        for(current.size in new.value) {
            # ensure the sizes are valid numbers
            if(!is.numeric(current.size)) {
                write(sprintf ('illegal font string "%s" in RCy3::setEdgeFontSizeDirect():\t\n it needs to be a valid number.', current.size), stderr ())
                
                size.type.errors = size.type.errors + 1
            }
        }
        
        if(size.type.errors < 1) {
            setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_SIZE")
        }
})
## END setEdgeFontSizeDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelColorDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      for (current.color in new.value){
         # ensure the color is formated in correct hexadecimal style
          if (.isNotHexColor(current.color)){
              return()
          }
      }
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_COLOR"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipDirect', 'CytoscapeWindowClass',
    function (obj, edge.names, new.values) {
        if (length (edge.names) != length (new.values)) {
            if (length(new.values) != 1){
                msg = sprintf ('error in RCy3::setEdgeTooltipDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                               length (new.values), length (edge.names))
                write (msg, stderr ())
                return ()
            }
        }
        # set the edge property direct
        return(setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TOOLTIP"))
     })

# ------------------------------------------------------------------------------
setMethod('setEdgeLineWidthDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.value) {
      for (current.size in new.value){
         # ensure the sizes are numbers
         if (!is.numeric(current.size)) {
            write (sprintf ('illegal size string "%s" in RCy3::setEdgeLineWidthDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_WIDTH"))
})

# ------------------------------------------------------------------------------
setMethod('setEdgeLineStyleDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.values) {
        unique.new.values <- unique(new.values)
        
        wrong.values <- 
            sapply(unique.new.values, function(v) { !(toupper(v) %in% getLineStyles(obj)) })
        
        if(any(wrong.values)) {
            error.msg <- paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])), sep="", collapse=", ")
            
            error.msg <- paste("\n\t\tERROR in setEdgeLineStyleDirect() >> INVALID line style value(s): ", error.msg, "\n", sep="")
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(setEdgePropertyDirect(obj, edge.names, toupper(new.values), "EDGE_LINE_TYPE"))
})
## END setEdgeLineStyleDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeSourceArrowShapeDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.values) {
        unique.new.values <- unique(new.values)
        
        wrong.values <- sapply(unique.new.values, function(v) { !(toupper(v) %in% getArrowShapes(obj)) })
        
        if(any(wrong.values)) {
            error.msg <- paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])), sep="", collapse=", ")
            
            error.msg <- paste("\n\t\tERROR in setEdgeSourceArrowShapeDirect() >> INVALID arrow shape value(s): ", error.msg, "\n", sep="")
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(setEdgePropertyDirect(obj, edge.names, toupper(new.values), "EDGE_SOURCE_ARROW_SHAPE"))
})
## END setEdgeSourceArrowShapeDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowShapeDirect', 'CytoscapeWindowClass', 
    function (obj, edge.names, new.values) {
        unique.new.values <- unique(new.values)
        
        wrong.values <- sapply(unique.new.values, function(v) { !(toupper(v) %in% getArrowShapes(obj)) })
        
        if(any(wrong.values)) {
            error.msg <- paste(sprintf("'%s'", names(wrong.values[which(wrong.values)])), sep="", collapse=", ")
            
            error.msg <- paste("\n\t\tERROR in setEdgeTargetArrowShapeDirect() >> INVALID arrow shape value(s): ", error.msg, "\n", sep="")
            
            write(error.msg, stderr())
            return(FALSE)
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(setEdgePropertyDirect(obj, edge.names, toupper(new.values), "EDGE_TARGET_ARROW_SHAPE"))
})
## END setEdgeTargetArrowShapeDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeSourceArrowColorDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.colors) {
        for(current.color in new.colors) {
            # check the color is represented in hexadecimal format
            if (.isNotHexColor(current.color)){
                return()
            }
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(setEdgePropertyDirect(obj, edge.names, new.colors, "EDGE_SOURCE_ARROW_UNSELECTED_PAINT"))
})
## END setEdgeSourceArrowColorDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowColorDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.colors) {
        for(current.color in new.colors) {
            if (.isNotHexColor(current.color)){
                return()
            }
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(setEdgePropertyDirect(obj, edge.names, new.colors, "EDGE_TARGET_ARROW_UNSELECTED_PAINT"))
})
## END setEdgeTargetArrowColorDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeLabelOpacityDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.value) {
        for(current.value in new.value) {
            # check that the opacity value is DOUBLE number between 0 and 255
            if(!is.double(current.value) || current.value < 0  || current.value > 255) {
                write(sprintf("\n\t\tERROR in setEdgeLabelOpacityDirect(): illegal opacity value '%s'. Opacity needs to be number between 0 and 255", current.value), stderr())
                
                return(FALSE)
            }
        }
        # returns TRUE or FALSE if issues have been found (like invalid edges, ...)
        return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_TRANSPARENCY"))
})
## END setEdgeLabelOpacityDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeSourceArrowOpacityDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.values) {
        write(sprintf("WARNING: Method RCy3::setEdgeSourceArrowOpacityDirect() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END setEdgeSourceArrowOpacityDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowOpacityDirect', 'CytoscapeWindowClass', 
    function(obj, edge.names, new.values) {
        write(sprintf("WARNING: Method RCy3::setEdgeTargetArrowOpacityDirect() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END setEdgeTargetArrowOpacityDirect

#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeLabelPositionDirect', 'CytoscapeWindowClass',
#   function (obj, edge.names, new.value) {
#     id = as.character (obj@window.id)
#     for (edge.name in edge.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Position', as.character (new.value))
#     invisible (result)
#     })

# ------------------------------------------------------------------------------
setMethod('getNodeCount', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "nodes/count", sep="/")
        request.res <- GET(resource.uri)
        node.count <- unname(fromJSON(rawToChar(request.res$content)))
        
        return(as.integer(node.count))
})
## END getNodeCount

# ------------------------------------------------------------------------------
setMethod('getEdgeCount', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "edges/count", sep="/")
        request.res <- GET(resource.uri)
        edge.count <- unname(fromJSON(rawToChar(request.res$content)))
        
        return(as.integer(edge.count))
})
## END getEdgeCount

# ------------------------------------------------------------------------------
setMethod('getNodeAttribute', 'CytoscapeConnectionClass', 
    function(obj, node.name, attribute.name) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        node.SUID <- as.character(.nodeNameToNodeSUID(obj, node.name))
        
        if(length(node.SUID) < 1) {
            write(sprintf("WARNING in RCy3::getNodeAttribute():\n\t no node with name '%s' could be found >> function returns empty value", node.name), stderr())
            
            return("")
        } else {
            node.attribute.type <- getNodeAttributeType(obj, attribute.name)
            
            if(length(node.attribute.type) > 0) {
                resource.uri <- 
                    paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/rows", node.SUID, attribute.name, sep="/")
                request.res <- GET(url=resource.uri)
                
                node.attribute.value <- unname(rawToChar(request.res$content))
                
                switch(node.attribute.type, 
                    "Double"={
                        return(as.numeric(node.attribute.value))
                    },
                    "Long"=,
                    "Integer"={
                        return(as.integer(node.attribute.value))
                    },
                    "Boolean"={
                        return(as.logical(node.attribute.value))
                    },{
                        return(as.character(node.attribute.value))
                    }
                )
            }
            return("")
        }
})

# ------------------------------------------------------------------------------
setMethod('getNodeAttributeType', 'CytoscapeWindowClass', 
    function(obj, attribute.name) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        if(attribute.name %in% getNodeAttributeNames(obj)) {
            resource.uri <- 
                paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
            request.res <- GET(url=resource.uri)
            
            node.attributes.info <- fromJSON(rawToChar(request.res$content))
            return(node.attributes.info[[which(lapply(node.attributes.info, function(a) {a$name}) %in% attribute.name)]]$type)
        } else {
            write(sprintf("WARNING in RCy3::getNodeAttributeType():\n\t '%s' could not be recognized as a valid node attribute >> function returns empty value", attribute.name), stderr())
            
            return("")
        }
}) 
## END getNodeAttributeType

# ------------------------------------------------------------------------------
setMethod('getAllNodeAttributes', 'CytoscapeWindowClass', 
          function(obj, onlySelectedNodes = FALSE) {
              g = obj@graph
              attribute.names = names(nodeDataDefaults(g))
              nodes.of.interest = nodes(g)
              if(onlySelectedNodes) {
                  if(getSelectedNodeCount(obj) == 0) {
                      return(NA)
                  }
                  nodes.of.interest = getSelectedNodes(obj)
              }
              result = cbind (unlist (nodeData (g, nodes.of.interest, attr=attribute.names [1])))
              if (length (attribute.names) > 1) {
                  for (name in attribute.names [2:length (attribute.names)]) {
                      new.column = unlist (nodeData (g, nodes.of.interest, attr=name))
                      if (is.null (new.column)){
                          new.column = rep ('NULL', nrow (result))
                      }
                      result = cbind (result, new.column)
                      } # for name
                  } # if length > 1

              colnames (result) = attribute.names
              result = as.data.frame (result, stringsAsFactors=FALSE)
              
              for (name in attribute.names) {
                  attribute.class = attr (nodeDataDefaults (obj@graph, name), 'class')
                  if (attribute.class == 'FLOATING'){
                      result [, name] = as.numeric (result [, name])
                  }else if (attribute.class == 'STRING'){
                      result [, name] = as.character (result [, name])
                  }else if (attribute.class == 'INTEGER'){
                      result [, name] = as.integer (result [, name])
                  }
                  } # for name

              return (result)
})

# ------------------------------------------------------------------------------
setMethod('getEdgeAttribute', 'CytoscapeConnectionClass', 
    function(obj, edge.name, attribute.name) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        edge.SUID <- as.character(.edgeNameToEdgeSUID(obj, edge.name))
        
        if(length(edge.SUID) < 1) {
            write(sprintf("WARNING in RCy3::getEdgeAttribute():\n\t no edge with name '%s' could be found >> function returns empty value", edge.name), stderr())
            
            return("")
        } else {
            edge.attribute.type <- getEdgeAttributeType(obj, attribute.name)
            
            if(length(edge.attribute.type) > 0) {
                resource.uri <- 
                    paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/rows", edge.SUID, attribute.name, sep="/")
                request.res <- GET(url=resource.uri)
                
                edge.attribute.value <- unname(rawToChar(request.res$content))
                
                switch(edge.attribute.type, 
                    "Double"={
                        return(as.numeric(edge.attribute.value))
                    },
                    "Long"=,
                    "Integer"={
                        return(as.integer(edge.attribute.value))
                    },
                    "Boolean"={
                        return(as.logical(edge.attribute.value))
                    },{
                        return(as.character(edge.attribute.value))
                    }
                )
            }
            return("")
        }
})
## END getEdgeAttribute

# ------------------------------------------------------------------------------
setMethod('getEdgeAttributeType', 'CytoscapeWindowClass', 
    function(obj, attribute.name) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        if(attribute.name %in% getEdgeAttributeNames(obj)) {
            resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
            request.res <- GET(url=resource.uri)
            
            edge.attributes.info <- fromJSON(rawToChar(request.res$content))
            return(edge.attributes.info[[which(lapply(edge.attributes.info, function(a) {a$name}) %in% attribute.name)]]$type)
        } else {
            write(sprintf("WARNING in RCy3::getEdgeAttributeType():\n\t '%s' could not be recognized as a valid edge attribute >> function returns empty value", attribute.name), stderr())
            
            return("")
        }
})
## END getEdgeAttributeType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdgeAttributes', 'CytoscapeWindowClass',

  function (obj, onlySelectedEdges=FALSE) {

   g = obj@graph
   attribute.names = names (edgeDataDefaults (g))
   edges.of.interest = edgeNames (g)
   if (onlySelectedEdges) {
     if (getSelectedEdgeCount (obj) == 0){
       return (NA)
     }
     edges.of.interest = getSelectedEdges (obj)
     } # if onlySelectedEdges

   source.and.target.nodes = unlist (strsplit (edges.of.interest, '~'))
   node.count = length (source.and.target.nodes)
   source = source.and.target.nodes [seq (1, node.count, 2)]
   target = source.and.target.nodes [seq (2, node.count, 2)]

   #printf ('source nodes: %s', list.to.string (source))
   #printf ('target nodes: %s', list.to.string (target))
   #printf ('attribute names: %s', list.to.string (attribute.names))

   result = cbind (unlist (edgeData (g, source, target, attr=attribute.names [1])))
   result = cbind (result, source)
   result = cbind (result, target)

   if (length (attribute.names) > 1) {
     for (name in attribute.names [2:length (attribute.names)]) {
       new.column = unlist (edgeData (g, source, target, attr=name))
       result = cbind (result, new.column)
       } # for name
     } # if > 1
   
   column.names = c (attribute.names [1], 'source', 'target')
   if (length (attribute.names) > 1){
     column.names = c (column.names, attribute.names [2:length(attribute.names)])
   }

   colnames (result) = column.names
   result = as.data.frame (result, stringsAsFactors=FALSE)
   
      # we had a matrix of character strings, now a data.frame of character strings
      # use the embedded type information (created by initEdgeAttribute) to correct to the proper types
      # must be a more direct way to do this in the calls to cbind on a data.frame.
   
   for (name in attribute.names) {
     attribute.class = attr (edgeDataDefaults (obj@graph, name), 'class')
     if (attribute.class == 'FLOATING'){
       result [, name] = as.numeric (result [, name])
     } else if (attribute.class == 'STRING'){
       result [, name] = as.character (result [, name])
     } else if (attribute.class == 'INTEGER'){
       result [, name] = as.integer (result [, name])
     }
     } # for name
   
   return (result)
    })

# ------------------------------------------------------------------------------
setMethod('getNodeAttributeNames', 'CytoscapeConnectionClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        
        resource.uri <- 
            paste(obj@uri, pluginVersion(obj), "networks", net.SUID, "tables/defaultnode/columns", sep="/")
        # request result
        request.res <- GET(url=resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        request.res <- data.frame(t(sapply(request.res, c)))
        request.res <- unlist(request.res$name)
        # exclude some node attributes
        node.attribute.names <- request.res[! request.res %in% c("SUID", "shared name", "selected")]
        if (length(node.attribute.names) <=2 ){
            write(sprintf('Please ensure that you sent the R graph to Cytoscape before calling this function, e.g. using displayGraph. Otherwise names might not be displayed (correctly).'), stderr())
        }
        return (node.attribute.names)
})
## END getNodeAttributeNames

# ------------------------------------------------------------------------------
setMethod('getEdgeAttributeNames', 'CytoscapeConnectionClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        resource.uri <- 
            paste(obj@uri, pluginVersion(obj), "networks", net.SUID, "tables/defaultedge/columns", sep="/")
        # request result
        request.res <- GET(url=resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        request.res <- data.frame(t(sapply(request.res, c)))
        request.res <- unlist(request.res$name)
        # exclude some edge attributes
        edge.attribute.names <- request.res[! request.res %in% c("SUID", "shared name", "shared interaction", "selected")]
        return(edge.attribute.names)
})
## END getEdgeAttributeNames

# ------------------------------------------------------------------------------
# delete node attribute by deleting its column in the node table
setMethod('deleteNodeAttribute', 'CytoscapeConnectionClass', 
  function(obj, attribute.name) {
     if (attribute.name %in% getNodeAttributeNames(obj)){
        resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultnode/columns", as.character(attribute.name), sep="/")
        result <- DELETE(url= resource.uri)
        write(sprintf('Attribute "%s" has been deleted...', attribute.name), stderr())
        invisible(result)
     } else{
        msg = paste (attribute.name, 'does not exist and thus could not be deleted.')
        write (msg, stderr ())
     }
})

# ------------------------------------------------------------------------------
# delete edge attribute by deleting its column in the edge table
setMethod('deleteEdgeAttribute', 'CytoscapeConnectionClass', 
  function(obj, attribute.name) {
     if (attribute.name %in% getEdgeAttributeNames(obj)){
        resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultedge/columns", as.character(attribute.name), sep="/")
        request.res <- DELETE(url= resource.uri)
        write(sprintf('Attribute "%s" has been deleted...', attribute.name), stderr())
        invisible(request.res)
     } else{
        msg = paste (attribute.name, 'does not exist and thus could not be deleted.')
        write (msg, stderr ())
     }
})

# ------------------------------------------------------------------------------
setMethod('getAllNodes', 'CytoscapeWindowClass', 
    function(obj) {
        loc.obj <- obj      
        # CyREST version
        version = pluginVersion(loc.obj)
        # network suid
        net.SUID <- as.character(loc.obj@window.id)
        
        n.count <- getNodeCount(obj)
        
        if(n.count == 0) {
            return()
        }
        
        # get SUIDs of existing (in Cytoscape) nodes
        resource.uri <- paste(loc.obj@uri, version, "networks", net.SUID, "nodes", sep="/")
        # get the SUIDs of the nodes in the Cytoscape graph
        cy.nodes.SUIDs <- fromJSON(rawToChar(GET(resource.uri)$content))

        dict.nodes.SUIDs <- sapply(loc.obj@suid.name.dict, "[[", 2)
        
        # check that the nodes presented in Cytoscape & RCy3's session dictionary do match
        diff.nodes <- setdiff(cy.nodes.SUIDs, dict.nodes.SUIDs)
        
        # in case that differences exist, run synchronization b/n RCy3 and RCytoscape
        if(length(diff.nodes) > 0) {
            write(sprintf("WARNING in RCy3::getAllNodes():\n\t the following node(s) exist in Cytoscape, but don't exist in RCy3's session"), stderr())
            
            nodes.only.in.cytoscape <- c()
            for(i in 1:length(diff.nodes)) {
                resource.uri <- 
                    paste(loc.obj@uri, version, "networks", net.SUID, "nodes", as.character(diff.nodes[i]), sep="/")
                node.name <- fromJSON(rawToChar(GET(resource.uri)$content))$data$name 
                nodes.only.in.cytoscape <- c(nodes.only.in.cytoscape, node.name)
            #    [GIK, Jul 2015] synch to be implemented
            #    loc.obj@suid.name.dict[[length(loc.obj@suid.name.dict) + 1]] <- 
            #        list(name=node.name, SUID=diff.nodes[i])
            }
            print(nodes.only.in.cytoscape)
        }
        node.names <- .nodeSUIDToNodeName(obj, cy.nodes.SUIDs[order(cy.nodes.SUIDs)])
        
        eval.parent(substitute(obj <- loc.obj))
        
        return(node.names)
})
## END getAllNodes

# ------------------------------------------------------------------------------
setMethod('getAllEdges', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        count <- getEdgeCount(obj)
        if(count == 0) {
            return()
        }
        
        # get edge name column and return its values
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns/name", sep="/")
        request.res <- GET(url=resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        names <- request.res$values
        return(names)
})
## END getAllEdges

# ------------------------------------------------------------------------------
setMethod('clearSelection', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        # if any nodes are selected, unselect them
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns/selected?default=false", sep="/")
        request.res <- PUT(url=resource.uri, body=FALSE)
        
        # if any edges are selected, unselect them
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns/selected?default=false", sep="/")
        request.res <- PUT(url=resource.uri, body=FALSE)
        
        invisible(request.res)
}) 
## END clearSelection
   
# ------------------------------------------------------------------------------
setMethod('selectNodes', 'CytoscapeWindowClass', 
    function(obj, node.names, preserve.current.selection = TRUE) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        if(preserve.current.selection) {
            if(getSelectedNodeCount(obj) > 0) {
                node.names <- unique(c(getSelectedNodes(obj), node.names))
            }
        }
        
        if(!preserve.current.selection) {
            clearSelection(obj)
        }
        
        # check for unknown nodes
        unknown.nodes <- setdiff(node.names, getAllNodes(obj))
        
        if(length(unknown.nodes) > 0) {
            nodes.string <- paste(unknown.nodes, collapse=' ')
            
            write(sprintf("NOTICE in RCy3::selectNodes():\n\t nodes [%s] are not in the Cytoscape graph and will not be selected", nodes.string), stderr())
            
            node.names <- intersect(node.names, getAllNodes(obj))
        }
        
        if(length(node.names) == 0) {
            write(sprintf("NOTICE in RCy3::selectNodes():\t\n no nodes to select >> the function call has no effect"), stderr())
        }
        
        # update the 'selected' column
        node.SUIDs <- .nodeNameToNodeSUID(obj, node.names)
        SUID.value.pairs <- lapply(node.SUIDs, function(s) {list('SUID'=s, 'value'=TRUE)})
        SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns/selected", sep="/")
        request.res <- PUT(url=resource.uri, body=SUID.value.pairs.JSON, encode="json")
        
        invisible(request.res)
}) 
## END selectNodes

#' Select all nodes
#'
#' Selects all nodes in a Cytoscape Network 
#'
#' @param object Cytoscape network  
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
          'CytoscapeWindowClass', 
          function(obj) {
            
            resource.uri <- paste(obj@uri,
                                  pluginVersion(obj),
                                  "networks",
                                  obj@window.id,
                                  "nodes",
                                  sep = "/")
            
            request.res <- GET(resource.uri) # returns all of the node SUIDs
            all_node_SUIDs <- fromJSON(rawToChar(request.res$content))
            SUID.value.pairs <- lapply(all_node_SUIDs,
                                       function(s) {list('SUID' = s, 'value' = TRUE)})
            SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
            
            resource.uri <- paste(obj@uri,
                                  pluginVersion(obj),
                                  "networks",
                                  obj@window.id,
                                  "tables/defaultnode/columns/selected",
                                  sep = "/")
            request.res <- PUT(url = resource.uri,
                               body = SUID.value.pairs.JSON,
                               encode = "json")
            invisible(request.res)
          })


   
# ------------------------------------------------------------------------------
setMethod('getSelectedNodeCount', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
        request.res <- GET(url=resource.uri)
        
        num.selected.nodes <- length(fromJSON(rawToChar(request.res$content)))
        
        return(num.selected.nodes)
}) 
## END getSelectedNodeCount
   
# ------------------------------------------------------------------------------
setMethod('getSelectedNodes', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        if(getSelectedNodeCount(obj) == 0) {
            write (sprintf ('warning!  No nodes selected.'), stdout ())
            return(NA)
        } else {
            resource.uri <- paste(obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
            request.res <- GET(url=resource.uri)
            
            selected.node.SUIDs <- fromJSON(rawToChar(request.res$content))
            selected.node.names <- .nodeSUIDToNodeName(obj, selected.node.SUIDs)
            return(selected.node.names)
    }
}) 
## END getSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
       node.names <- getSelectedNodes(obj)
       setNodePropertyDirect(obj, node.names, 'false', "NODE_VISIBLE")
   }) # hideSelectedNodes
   
# ------------------------------------------------------------------------------
setMethod('hideNodes', 'CytoscapeWindowClass', 
    function(obj, node.names) {
        setNodePropertyDirect(obj, node.names, 'false', "NODE_VISIBLE")
}) 
## END hideNodes
   
# ------------------------------------------------------------------------------------------------------------------------
setMethod('unhideNodes', 'CytoscapeWindowClass', 
    function(obj, node.names) {
        setNodePropertyDirect(obj, node.names, 'true', "NODE_VISIBLE")
}) 
## END unhideNodes

# ------------------------------------------------------------------------------
# select all nodes that were not selected and deselect all nodes that were selected
setMethod('invertNodeSelection', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=false", sep="/")
        request.res <- GET(url=resource.uri)
        unselected.node.SUIDs <- fromJSON(rawToChar(request.res$content))
        
        # clear selection
        clearSelection(obj)
        
        selectNodes(obj, .nodeSUIDToNodeName(obj, unselected.node.SUIDs), FALSE)
}) 
## END invertNodeSelection
 
# ------------------------------------------------------------------------------
# [GIK - Jul, 2015] function might break if self-loops exist in the graph
setMethod('deleteSelectedNodes', 'CytoscapeWindowClass', 
    function(obj) {
        loc.obj <- obj
        
        net.SUID <- as.character(loc.obj@window.id)
        version <- pluginVersion(loc.obj)
        
        selected.node.names <- getSelectedNodes(loc.obj)
        selected.node.SUIDs <- .nodeNameToNodeSUID(loc.obj, selected.node.names)
        
        for(i in 1:length(selected.node.SUIDs)) {
            node.SUID <- selected.node.SUIDs[i]
            # (list of) edges that have this particular node as their source node
            source.bound.edges <- list()
            # (list of) edges that have this particular node as their target node
            target.bound.edges <- list()
            
            source.bound.edge.indices <- 
                which(sapply(loc.obj@edge.suid.name.dict, function(n) {n$source.node}) %in% node.SUID)
            
            if(length(source.bound.edge.indices) > 0) {
                # get edge SUIDs
                source.bound.edges <- 
                    sapply(loc.obj@edge.suid.name.dict[source.bound.edge.indices], function(e) { e$SUID })
                # delete all edges, whose source node is to-be deleted
                for(k in 1:length(source.bound.edges)) {
                    resource.uri <- paste(loc.obj@uri, version, "networks", net.SUID, "edges", as.character(source.bound.edges[k]), sep="/")
                    
                    request.res <- DELETE(url=resource.uri)
                    # [GIK] TO-DO: delete the edge row/entry from Cytoscape's Edge table
                }
                # also, delete those edges from the session dictionary
                loc.obj@edge.suid.name.dict[source.bound.edge.indices] <- NULL
            }
            
            target.bound.edge.indices <- 
                which(sapply(loc.obj@edge.suid.name.dict, function(n) {n$target.node}) %in% node.SUID)
            
            if(length(target.bound.edge.indices) > 0) {
                # get edge SUIDs
                target.bound.edges <- 
                    sapply(loc.obj@edge.suid.name.dict[target.bound.edge.indices], function(e) { e$SUID })
                # delete all edges, whose target node is to-be deleted
                for(k in 1:length(target.bound.edges)) {
                    resource.uri <- paste(loc.obj@uri, version, "networks", net.SUID, "edges", as.character(target.bound.edges[k]), sep="/")
                    
                    request.res <- DELETE(url=resource.uri)
                    # [GIK] TO-DO: delete the edge row/entry from Cytoscape's Edge table
                }
                # also, delete those edges from the session dictionary
                loc.obj@edge.suid.name.dict[target.bound.edge.indices] <- NULL
            }
            
            # delete the node from the Cytoscape network
            resource.uri <- 
                paste(loc.obj@uri, version, "networks", net.SUID, "nodes", as.character(node.SUID), sep="/")
            request.res <- DELETE(url=resource.uri)
        
            # [GIK] TO-DO: delete the node row/entry in the Cytoscape node table
        
            # delete the node from the session disctionary
            node.index <- 
                which(sapply(loc.obj@suid.name.dict, function(n) { n$SUID }) %in% node.SUID)
            loc.obj@suid.name.dict[node.index] <- NULL
        }
        eval.parent(substitute(obj <- loc.obj))
})
## END deleteSelectedNodes
   
# ------------------------------------------------------------------------------
setMethod('selectEdges', 'CytoscapeWindowClass', 
    function(obj, edge.names, preserve.current.selection=TRUE) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        # keep the currently selected edges
        if(preserve.current.selection) {
            edge.names <- unique(c(getSelectedEdges(obj), edge.names))
        }
        
        edge.SUIDs <- .edgeNameToEdgeSUID(obj, edge.names)
        SUID.value.pairs <- lapply(edge.SUIDs, function(s) {list('SUID'=s, 'value'=TRUE)})
        SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns/selected", sep="/")
        request.res <- PUT(url=resource.uri, body=SUID.value.pairs.JSON, encode="json")
        
        invisible(request.res)
}) 
## END selectEdges

#' Select all edges 
#'
#' Selects all edges in a Cytoscape Network 
#'
#' @param obj Cytoscape network  
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
          'CytoscapeWindowClass', 
          function(obj) {
            
            resource.uri <- paste(obj@uri,
                                  pluginVersion(obj),
                                  "networks",
                                  obj@window.id,
                                  "edges",
                                  sep = "/")
            
            request.res_edges <- GET(resource.uri) ## returns all of the edge suids
            all_edge_SUIDs <- fromJSON(rawToChar(request.res_edges$content))
            SUID.value.pairs <- lapply(all_edge_SUIDs,
                                       function(s) {list('SUID' = s, 'value' = TRUE)})
            SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
            
            resource.uri <- paste(obj@uri,
                                  pluginVersion(obj),
                                  "networks",
                                  obj@window.id,
                                  "tables/defaultedge/columns/selected",
                                  sep = "/")
            request.res <- PUT(url = resource.uri,
                               body = SUID.value.pairs.JSON,
                               encode = "json")
            invisible(request.res)
          })
 
# ------------------------------------------------------------------------------
setMethod('invertEdgeSelection', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "edges?column=selected&query=false", sep="/")
        request.res <- GET(url=resource.uri)
        unselected.edges.SUIDs <- fromJSON(rawToChar(request.res$content))
        # if any edges are selected, unselect them (nodes have clearSelection function) 
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns/selected?default=false", sep="/")
        request.res <- PUT(url=resource.uri, body=FALSE)
        
        to.be.selected.edges <- lapply(unselected.edges.SUIDs, function(s) {list('SUID'=s, 'value'=TRUE)})
        to.be.selected.edges.JSON <- toJSON(to.be.selected.edges)
        
        unselected.edges.names <- .edgeSUIDToEdgeName(obj, unselected.edges.SUIDs)
        selectEdges(obj, unselected.edges.names, FALSE)
}) 
## END invertEdgeSelection
 
# ------------------------------------------------------------------------------
setMethod('deleteSelectedEdges', 'CytoscapeWindowClass', 
    function(obj) {
        loc.obj <- obj
        
        net.SUID = as.character(loc.obj@window.id)
        version = pluginVersion(loc.obj)
        
        selected.edge.names = getSelectedEdges(loc.obj)
        selected.edge.SUIDs = .edgeNameToEdgeSUID(loc.obj, selected.edge.names)
        
        for(i in 1:length(selected.edge.SUIDs)) {
            edge.SUID = selected.edge.SUIDs[i]
            resource.uri = paste(loc.obj@uri, version, "networks", net.SUID, "edges", edge.SUID, sep="/")
            # delete edge from canvas / view
            request.res = DELETE(url=resource.uri)
            
            # delete edge from edge table : NOT possible in the API
            
            # delete edge record from the session dictionary
            loc.obj@edge.suid.name.dict[which(sapply(loc.obj@edge.suid.name.dict, function(e) { e$SUID }) %in% edge.SUID)] <- NULL
        }
        
        eval.parent(substitute(obj <- loc.obj))
}) 
## END deleteSelectedEdges
   
# ------------------------------------------------------------------------------
setMethod('getSelectedEdgeCount', 'CytoscapeWindowClass', 
    function(obj) {
        net.SUID <- as.character(obj@window.id)
        version <- pluginVersion(obj)
        
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
        request.res <- GET(url=resource.uri)
        
        num.selected.edges <- length(fromJSON(rawToChar(request.res$content)))
        return(num.selected.edges)
})
## END getSelectedEdgeCount
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdges', 'CytoscapeWindowClass',

    function (obj) {
        net.SUID = as.character(obj@window.id)
        version = pluginVersion(obj)
        if(getSelectedEdgeCount(obj) == 0) {
            return (NA)
        } else {
            resource.uri = paste(obj@uri, version, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
            request.res = GET(url=resource.uri)
            selected.edges.SUIDs = fromJSON(rawToChar(request.res$content))
            selected.edges = .edgeSUIDToEdgeName(obj, selected.edges.SUIDs)
            
            return(selected.edges)
        }
}) # getSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedEdges', 'CytoscapeWindowClass',

    function (obj) {
        edge.names <- getSelectedEdges(obj)
        setEdgePropertyDirect(obj, edge.names, 'false', "EDGE_VISIBLE")
     }) # hideSelectedEdges
   
# ------------------------------------------------------------------------------
setMethod('unhideAll', 'CytoscapeWindowClass', 
    function(obj) {
        node.names <- getAllNodes(obj)
        setNodePropertyDirect(obj, node.names, 'true', "NODE_VISIBLE")
        
        edge.names <- getAllEdges(obj)
        setEdgePropertyDirect(obj, edge.names, 'true', "EDGE_VISIBLE")
}) 
## END unhideAll
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getFirstNeighbors', 'CytoscapeWindowClass',

   function (obj, node.names, as.nested.list=FALSE) {
      if (length (node.names) == 0){
         return()
      }else{
         # map node names to node SUIDs
         dict.indices = which(sapply(obj@suid.name.dict, function(s) { s$name }) %in% node.names)
         node.SUIDs = sapply(obj@suid.name.dict[dict.indices], function(i) {i$SUID})
         
         # network ID and cyREST API version
         net.suid = as.character(obj@window.id)
         version = pluginVersion(obj)
         
         # get first neighbors
         # TODO at some later point it might be nice to return the first neighbors as nested lists
         neighbor.names <- c()
         
         for (node.SUID in node.SUIDs){
            # get first neighbors for each node
            resource.uri <- paste(obj@uri, version, "networks", net.suid, "nodes", as.character(node.SUID), "neighbors", sep="/")
            request.res <- GET(resource.uri)
            first.neighbors.SUIDs <- fromJSON(rawToChar(request.res$content))
            
            # map node SUIDs to node names
            dict.indices <- which(sapply(obj@suid.name.dict, function(s) { s$SUID }) %in% first.neighbors.SUIDs)
            if (as.nested.list){
                neighbor.names <- append(neighbor.names, list(c(neighbor.names, sapply(obj@suid.name.dict[dict.indices], function(i) {i$name}))))
            }else{
                neighbor.names <- c(neighbor.names, sapply(obj@suid.name.dict[dict.indices], function(i) {i$name}))
            }
         }
         return (neighbor.names)
      }
      })  # getFirstNeighbors

#------------------------------------------------------------------------------------------
setMethod ('selectFirstNeighborsOfSelectedNodes', 'CytoscapeWindowClass',

    function (obj) {
        if (getSelectedNodeCount (obj) > 0) {
            currently.selected = getSelectedNodes (obj)
            if (length (currently.selected) == 0){
                invisible ()
            }
            neighbors = getFirstNeighbors (obj, currently.selected)
            full.selection = unique (c (currently.selected, neighbors))
            selectNodes (obj, full.selection)
            invisible (full.selection)
        } # if any nodes are already selected
        else {
            write (sprintf ('warning!  No nodes selected.'), stdout ())
        }
     }) # selectFirstNeighborsOfSelectedNodes

# ------------------------------------------------------------------------------
setMethod('sfn', 'CytoscapeWindowClass', function (obj) {
  selectFirstNeighborsOfSelectedNodes (obj)
})

#------------------------------------------------------------------------------------------------------------------------
#' Select the edges connecting selected nodes in Cytoscape Network 
#'
#' Selects edges in a Cytoscape Network connecting the selected nodes 
#'
#' @param obj Cytoscape network 
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
#' @seealso \code{\link{createWindowFromSelection}}, \code{\link{selectEdgesConnectedBySelectedNodes}}, \code{\link{renameCytoscapeNetwork}}
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



noa.names = function(graph)
{
  return(names(nodeDataDefaults(graph)))
} # noa.names
#------------------------------------------------------------------------------------------------------------------------
eda.names = function(graph)
{
  return(names(edgeDataDefaults(graph)))
} # eda.names
#------------------------------------------------------------------------------------------------------------------------
noa = function(graph, node.attribute.name)
{
  if(!node.attribute.name %in% noa.names(graph))
    return(NA)
  return(unlist(nodeData(graph, attr=node.attribute.name)))
} # noa
#------------------------------------------------------------------------------------------------------------------------
# return the value of every edge in the graph for the specified attribute
eda = function(graph, edge.attribute.name)
{
  if(!edge.attribute.name %in% eda.names(graph))
    return (NA)
  return(unlist(edgeData(graph, attr=edge.attribute.name)))
} # eda

# ------------------------------------------------------------------------------
# use the expected 'edgeType' attribute to create cytoscape-style 'A (edgeType) B' 
# edge names from a graphNEL
# edgeNames (g) # "A~B" "B~C" "C~A"
# if there is no edge attribute named 'edgeType', then create edges(uninterestingly) named 'A (edge) B'
cy2.edge.names = function(graph, R.edge.names=NA)
{
   #printf('running new version of cy2.edge.names')
   if(length(edges(graph)) == 0) {
      return(NA)
   }
   
   edgeType.attribute.present = TRUE
   edge.type = 'unspecified'
   if('edgeType' %in% names(edgeDataDefaults(graph))) {
       # vector containing the 'edgeType'-attribute value for every edge
      edge.type = as.character(eda(graph, 'edgeType'))
   }
   
   tokens = strsplit(.rcyEdgeNames(graph), '~')
   a = sapply (tokens, function (tok) tok [1])
   b = sapply (tokens, function (tok) tok [2])
   edge.type = paste (' (', edge.type, ') ', sep='')
   edge.names = paste (a, edge.type, b, sep='')
   
   names (edge.names) = .rcyEdgeNames (graph)
   
   if (!(length (R.edge.names) == 1 && is.na (R.edge.names))) {  # we were given some subset of all edges to extract and get cy2 names for.  do that here
      new.edgeNames.tilde = gsub ('\\|', '~', R.edge.names)
      if (length (intersect (names (edge.names), new.edgeNames.tilde)) > 0){
         edge.names = edge.names [new.edgeNames.tilde]
      }
   }
   
   return (edge.names)

} # cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
getAdjacentEdgeNames = function (graph, node.names) 
{
    all.edge.names = cy2.edge.names (graph) 
    all.edge.names.cyStyle = as.character (all.edge.names) 
    indices.of.edges.with.nodes = c () 
    
    for (node in node.names) { 
        node.regex.nodeA = sprintf ('^%s ', node)
        node.regex.nodeB = sprintf (' %s$', node)
        indices.A = grep (node.regex.nodeA, all.edge.names.cyStyle) 
        indices.B = grep (node.regex.nodeB, all.edge.names.cyStyle) 
        indices.of.edges.with.nodes = c (indices.of.edges.with.nodes, indices.A, indices.B) 
    } # for node

    return (unique (as.character (all.edge.names) [indices.of.edges.with.nodes]))
    
} # getAdjacentEdgeNames
#------------------------------------------------------------------------------------------------------------------------
makeSimpleGraph = function ()
{
  g = new ('graphNEL', edgemode='directed')

  g = initNodeAttribute (g, 'type', 'char', 'undefined')
  g = initNodeAttribute (g, 'lfc', 'numeric', 1.0)
  g = initNodeAttribute (g, 'label', 'char', 'default node label')
  g = initNodeAttribute (g, 'count', 'integer', 0)

  g = initEdgeAttribute (g, 'edgeType', 'char', 'undefined')
  g = initEdgeAttribute (g, 'score', 'numeric', 0.0)
  g = initEdgeAttribute (g, 'misc',   'char', 'default misc')

  g = graph::addNode ('A', g)
  g = graph::addNode ('B', g)
  g = graph::addNode ('C', g)
  nodeData (g, 'A', 'type') = 'kinase'
  nodeData (g, 'B', 'type') = 'transcription factor'
  nodeData (g, 'C', 'type') = 'glycoprotein'

  nodeData (g, 'A', 'lfc') = -3.0
  nodeData (g, 'B', 'lfc') = 0.0
  nodeData (g, 'C', 'lfc') = 3.0

  nodeData (g, 'A', 'count') = 2
  nodeData (g, 'B', 'count') = 30
  nodeData (g, 'C', 'count') = 100

  nodeData (g, 'A', 'label') = 'Gene A'
  nodeData (g, 'B', 'label') = 'Gene B'
  nodeData (g, 'C', 'label') = 'Gene C'

  g = graph::addEdge ('A', 'B', g)
  g = graph::addEdge ('B', 'C', g)
  g = graph::addEdge ('C', 'A', g)

  edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
  edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'

  edgeData (g, 'A', 'B', 'score') =  35.0
  edgeData (g, 'B', 'C', 'score') =  -12

  return (g)

} # makeSimpleGraph
#------------------------------------------------------------------------------------------------------------------------
# create, display and render the 3-node, 3-edge simple graph
demoSimpleGraph = function ()
{
    window.title = 'demo.simpleGraph'
    cy = CytoscapeConnection ()
    if (window.title %in% as.character (getWindowList (cy)))
    deleteWindow (cy, window.title)
    
    g.simple = makeSimpleGraph ()
    cws = CytoscapeWindow (window.title, g.simple)
    
    displayGraph (cws)
    layoutNetwork (cws, 'grid')
    setNodeLabelRule (cws, 'label')
    
    node.attribute.values = c ("kinase",  "transcription factor")
    colors =                c ('#A0AA00', '#FF0000')
    setDefaultNodeBorderWidth (cws, 5)
    setNodeBorderColorRule (cws, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeRule (cws, 'count', count.control.points, sizes, mode='interpolate')
    setNodeColorRule (cws, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
    
    invisible (cws)

} # demoSimpleGraph

# ------------------------------------------------------------------------------
makeRandomGraph = function(node.count=12, seed=123)
{
  set.seed(seed); 
  #if(node.count > 26) node.count = 26
  node.names = as.character(1:node.count)
  g = randomGraph(node.names, M <- 1:2, p = 0.6)
  attr(edgeDataDefaults(g, attr="weight"), "class") = "DOUBLE"
  edgeDataDefaults(g, 'pmid') = '9988778899'
  attr(edgeDataDefaults(g, attr="pmid"), "class") = "STRING"
  return(g)
} # makeRandomGraph

#------------------------------------------------------------------------------------------------------------------------
# see Robert Flight's replacement below (pshannon, 20 jul 2012)
# the bioconductor graph class stores undirected graph edge attributes redundantly.  bioc's nishant says (email, 2 sep 2010):
#
# The people who started the graph package decided to return duplicate edge attributes / weights for the undirected
# case. ie if you have an edge a-b and the graph is undirected, methods such as edgeWeights, edgeData etc will end up
# returning duplicate values for the attribute for a-b and b-a.  That was a design decision taken by the creators of the
# package and I do not think it will be possible to change that now.  I guess the solution might be to create your own
# edgeWeights and edgeData methods in your package that retrieve only the non-duplicated attributes for the undirected
# case.
#
remove.redundancies.in.undirected.graph.old = function (gu)
{
  if (length (nodes (gu)) == 0)
    return (new ('graphNEL', edgemode='directed'))

  g = new ('graphNEL', edgemode='directed')

  if (length (edgeDataDefaults (gu)) > 0)
    edgeDataDefaults (g) = edgeDataDefaults (gu)

  if (length (nodeDataDefaults (gu)) > 0)
    nodeDataDefaults (g) = nodeDataDefaults (gu)

  g = addNode (nodes (gu), g)
  for (node in nodes (g)) {
    for (noa.name in noa.names (gu)) {
      nodeData (g, node, noa.name) = nodeData (gu, node, noa.name)
      } # for noa.name
    } # for node

  if (length (edges (gu)) == 0)
    return (g)

  edge.names = edgeNames (gu)
  edge.node.pairs = strsplit (edge.names, '\\~')
  eda.names = eda.names (gu)

  for (node.pair in edge.node.pairs) {
    source.node = node.pair [1]
    target.node = node.pair [2]
    #printf ('create edge from %s to %s', source.node, target.node)
    g = addEdge (source.node, target.node, g)
    for (eda.name in eda.names (gu)) {
      edgeData (g, source.node, target.node, eda.name) = edgeData (gu, source.node, target.node, eda.name)
      } # for eda.name
    } # for node.pair

  return (g)

} # remove.redundancies.in.undirected.graph.old
#------------------------------------------------------------------------------------------------------------------------
# Robert Flight offered this replacement, having encountered painfully slow execution with a 5k edge undirected graph
# this fast version, likes its slow predecessor, compensates for the (in my view) flawed implementation of undirected
# graphNELs by converting them to directed graphs.
# but because undirected graphs are logically sound, and representationally useful, this is only a temporary fix.
# a redesign of this aspect of the graphNEL class is needed.
#
# original comments:
# the bioconductor graph class stores undirected graph edge attributes redundantly.  bioc's nishant says (email, 2 sep 2010):
#
# The people who started the graph package decided to return duplicate edge attributes / weights for the undirected
# case. ie if you have an edge a-b and the graph is undirected, methods such as edgeWeights, edgeData etc will end up
# returning duplicate values for the attribute for a-b and b-a.  That was a design decision taken by the creators of the
# package and I do not think it will be possible to change that now.  I guess the solution might be to create your own
# edgeWeights and edgeData methods in your package that retrieve only the non-duplicated attributes for the undirected
# case.
#
remove.redundancies.in.undirected.graph = function(gu) 
{
  if (length(nodes(gu)) == 0) 
      return(new("graphNEL", edgemode = "directed"))

  g <- new("graphNEL", edgemode = "directed")

  if (length(edgeDataDefaults(gu)) > 0) 
      edgeDataDefaults(g) <- edgeDataDefaults(gu)

  if (length(nodeDataDefaults(gu)) > 0) 
      nodeDataDefaults(g) <- nodeDataDefaults(gu)

  g <- addNode(nodes(gu), g)

  allNodes <- nodes(gu)

  noa.name <- invisible(lapply(noa.names(gu), function(noa.name) {
      nodeData(g, allNodes, noa.name) <- nodeData(gu, allNodes, noa.name)
  }))

  if (length(edgeNames(gu)) == 0) 
      return(g)

  edge.names <- edgeNames(gu)
  edge.node.pairs <- strsplit(edge.names, "\\~")
  source.nodes <- sapply(edge.node.pairs, function(x) x[1])
  target.nodes <- sapply(edge.node.pairs, function(x) x[2])

  g = graph::addEdge(source.nodes, target.nodes, g)

  invisible(lapply(eda.names(gu), function(eda.name) {
      edgeData(g, source.nodes, target.nodes, eda.name) <- edgeData(gu, source.nodes, 
          target.nodes, eda.name)
  }))

  return(g)

}  # remove.redundancies.in.undirected.graph
#------------------------------------------------------------------------------------------------------------------------
initNodeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
  stopifnot (attribute.type %in% c ('char', 'integer', 'numeric'))
  if (attribute.type == 'char')
    attribute.type = 'STRING'
  else if (attribute.type == 'integer')
    attribute.type = 'INTEGER'
  else if (attribute.type == 'numeric')
    attribute.type = 'FLOATING'

  nodeDataDefaults (graph, attr=attribute.name) = default.value
  attr (nodeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type

  return (graph)

} # initNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
initEdgeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
  stopifnot (attribute.type %in% c ('char', 'integer', 'numeric'))
  if (attribute.type == 'char')
    attribute.type = 'STRING'
  else if (attribute.type == 'integer')
    attribute.type = 'INTEGER'
  else if (attribute.type == 'numeric')
    attribute.type = 'FLOATING'

  edgeDataDefaults (graph, attr=attribute.name) = default.value
  attr (edgeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type

  return (graph)

} # initEdgettribute

#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been updated with new ones from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new nodes.
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new nodes, use the standard method 'setNodeAttributesDirect' to send them to cytoscape
# 
.sendNodeAttributesForGraph = function(obj, other.graph, attribute.name, new.node.indices)
{
    caller.specified.attribute.class = attr(nodeDataDefaults(other.graph, attribute.name), 'class')
    if(is.null(caller.specified.attribute.class)) {
        msg1 = sprintf('Error! RCytoscape:::.sendNodeAttributesForGraph. You must initialize the "%s" node attribute.', attribute.name)
        msg2 = sprintf('        example: my.graph = initNodeAttribute(my.graph, attr="moleculeType", "char", "unspecified")')
        write(msg1, stderr())
        write(msg2, stderr())
        return(NA)
    }
    # only add attributes for new nodes, unique to the new graph 'other.graph'
    # new.node.names = setdiff(nodes(other.graph), nodes(obj@graph))
    new.node.names = nodes(other.graph)[new.node.indices]
    values = noa(other.graph, attribute.name)[new.node.names]
    invisible(setNodeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, new.node.names, values))
} # END .sendNodeAttributesForGraph 

#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been extended with all the new nodes and edges from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new edges
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new edges, use the standard method 'setEdgeAttributesDirect' to send them to cytoscape
# oddities: edge naming is a tricky business.  cytoscape lablels edges like this:
#    <sourceNode> (interactionType) <targetNode>
# RCy3 provide a utility function for retrieving them from an R graph object,   cy2.edge.names (g)
# which uses the edgeNames (g) method to get the R names
# edgeNames (g2)  # [1] "A~E" "A~B" "D~E"
# thus, R has a little inconsistency:  sometimes using the tilda, sometimes the vertical bar
#                 A~E                 A~B                 D~E 
#     "A (inferred) E" "A (unspecified) B"  "D (literature) E" 
# names (edgeData (g2, attr='edgeType'))
#    [1] "A|E" "A|B" "D|E"
# for historical reasons, and maybe laziness, these two conventions are supported here, at the cost of calling gsub on the edge
# names, so that A~E becomes A|E, setting the stage for calling 
#   values = eda (g, attribute.name) [new.edge.names.with.bar.delimitor]
# below, and thereby ensuring that only the attributes of new edges are sent to Cytoscape

.sendEdgeAttributesForGraph = function (obj, other.graph, attribute.name, new.edge.indices)
{
    caller.specified.attribute.class = attr(edgeDataDefaults(other.graph, attribute.name), 'class')
    
    if(is.null(caller.specified.attribute.class)) {
        msg1 = sprintf('Error!  RCytoscape:::.sendEdgeAttributesForGraph. You must initialize the "%s" edge attribute.', attribute.name)
        msg2 = sprintf('        example:  my.graph = initEdgeAttribute (my.graph, attr="edgeType", "char", "unspecified")')
        write(msg1, stderr())
        write(msg2, stderr())
        return(NA)
    }
    
    # send only attributes for edges which are unique to other.graph; 
    # we assume that any existing edges already have their attributes
    new.edge.names = unname(cy2.edge.names(other.graph)[new.edge.indices])
    
    if(length(new.edge.names) == 0) {
        return()
    }
    
    values = eda(other.graph, attribute.name)[new.edge.indices]
    invisible(setEdgeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, new.edge.names, values))
} # .sendEdgeAttributesForGraph 

# ------------------------------------------------------------------------------
setMethod('getVisualStyleNames', 'CytoscapeConnectionClass', 
  function(obj) {
    resource.uri = paste(obj@uri, pluginVersion(obj), "apply/styles", sep="/")
    request.res = GET(url=resource.uri)
    visual.style.names = unname(fromJSON(rawToChar(request.res$content)))
    return(visual.style.names)
})

# ------------------------------------------------------------------------------
setMethod('copyVisualStyle', 'CytoscapeConnectionClass', 
  function (obj, from.style, to.style) {
     current.names = getVisualStyleNames (obj)
     if (! from.style %in% current.names){
        stop (sprintf ('Cannot copy from a non-existent visual style (%s)', from.style))
     }
     # get the current style from Cytoscape
     resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", from.style, sep="/")
     from.style.JSON <- GET(url=resource.uri)
     from.style <- fromJSON(rawToChar(from.style.JSON$content))
     from.style[1] <- as.character(to.style)
     
     # and send it to Cytoscape as a new style with a new name
     to.style.JSON <- toJSON(from.style)
     resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", sep="/")
     request.res <- POST(url = resource.uri, body = to.style.JSON, encode = "json")
     invisible(request.res)
})

# ------------------------------------------------------------------------------
# apply visual style to network
setMethod('setVisualStyle', 'CytoscapeConnectionClass', 
  function(obj, new.style.name) {
    net.SUID = as.character(obj@window.id)
    current.names = getVisualStyleNames(obj)
    # inform user if they want to set style that does not exist 
    if(!new.style.name %in% current.names) { 
      stop(sprintf('Cannot call setVisualStyle on a non-existent visual style (%s)', new.style.name))
    }
    # change the current style to the new style
    resource.uri <- paste(obj@uri, pluginVersion(obj), "apply/styles", new.style.name, net.SUID, sep="/")
    req.res <- GET(url=resource.uri)
    write(sprintf('network visual style has been set to "%s"', new.style.name), stdout())
    invisible(req.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('lockNodeDimensions', 'CytoscapeConnectionClass',

    function (obj, new.state, visual.style.name='default') {
        # launch error if visual style name is missing
        if (! visual.style.name %in% getVisualStyleNames (obj)) {
            write (sprintf ('Error in RCy3::lockNodeDimensions. No visual style named "%s"', visual.style.name), stdout ())
            return ()
        }

        #lock node dimensions
        resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", as.character(visual.style.name), "dependencies", sep="/")
        style <- list(visualPropertyDependency="nodeSizeLocked", enabled = tolower(new.state))
        style.JSON <- toJSON(list(style))
        request.res <- PUT(url=resource.uri, body=style.JSON, encode="json")

        # inform the user if the request was a success or failure
        if (request.res$status == 204){
            if (new.state==TRUE){
                write (sprintf ('Locked node dimensions successfully even if the check box is not ticked.'), stdout ())
            }else{
                write (sprintf ('Unlocked node dimensions successfully even if the check box is not ticked.'), stdout ())
            }
        }else{
            write (sprintf ('Error in RCy3::lockNodeDimensions. Could not lock/unlocked node dimensions'), stderr ())
        }
        invisible(request.res)
        
     }) # lockNodeDimensions

# ------------------------------------------------------------------------------
setMethod('getDefaultBackgroundColor', 'CytoscapeConnectionClass', 
  function(obj, vizmap.style.name='default') {
    resource.uri = paste(obj@uri, pluginVersion(obj), "styles", as.character(vizmap.style.name), "defaults/NETWORK_BACKGROUND_PAINT", sep="/")
    request.res = GET(url=resource.uri)
    def.background.color = fromJSON(rawToChar(request.res$content))[[2]]
    return(def.background.color)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultBackgroundColor', 'CytoscapeConnectionClass', 
    function(obj, new.color, vizmap.style.name='default') {
        if (.isNotHexColor(new.color)){
            return()
        } 
        resource.uri = paste(obj@uri, pluginVersion(obj), "styles", as.character(vizmap.style.name), "defaults", sep="/")
        style = list(visualProperty = 'NETWORK_BACKGROUND_PAINT', value = new.color)
        style.JSON = toJSON(list(style))
        request.res = PUT(url=resource.uri, body=style.JSON, encode="json")
        invisible(request.res)
})

# ------------------------------------------------------------------------------
setMethod('getDefaultNodeSelectionColor', 'CytoscapeConnectionClass', 
  function(obj, vizmap.style.name='default') {
    return(getVisualProperty(obj, vizmap.style.name, 'NODE_SELECTED_PAINT'))
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSelectionColor', 'CytoscapeConnectionClass', 
    function(obj, new.color, vizmap.style.name='default') {
        if (.isNotHexColor(new.color)){
            return()
        } 
        style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
        setVisualProperty(obj, style, vizmap.style.name)
})
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
       return(getVisualProperty(obj, vizmap.style.name, 'NODE_PAINT'))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
       if (.isNotHexColor(new.color)){
           return()
       } 
       style = list(visualProperty = "NODE_PAINT", value = new.color) 
       setVisualProperty(obj, style, vizmap.style.name)
      })

# ------------------------------------------------------------------------------
setMethod('getDefaultEdgeSelectionColor', 'CytoscapeConnectionClass', 
  function(obj, vizmap.style.name='default') {
    return(getVisualProperty(obj, vizmap.style.name, 'EDGE_STROKE_SELECTED_PAINT'))
})

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeSelectionColor', 'CytoscapeConnectionClass', 
    function(obj, new.color, vizmap.style.name='default') {
        if (.isNotHexColor(new.color)){
            return()
        }
        style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
        setVisualProperty(obj, style, vizmap.style.name)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return(getVisualProperty(obj, vizmap.style.name, 'EDGE_STROKE_UNSELECTED_PAINT'))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      if (.isNotHexColor(new.color)){
          return()
      } 
      style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
      setVisualProperty(obj, style, vizmap.style.name)
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveImage', 'CytoscapeWindowClass',
           
           function (obj, file.name, image.type, h = 600) {
             image.type = tolower (image.type)
             stopifnot (image.type %in% c ('png', 'pdf', 'svg'))
             id = as.character (obj@window.id)
             
             if (!file.exists(file.name)){
               if(image.type=='png'){
                 
                 resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", id,
                                       paste0("views/first.", image.type, "?h=", h), sep="/")  
               } 
               else{
                 # get the view image from Cytoscape in PNG, PDF, or SVG format
                 resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", id,
                                       paste0("views/first.", image.type), sep="/")
               }
               request.res <- GET(resource.uri, write_disk(paste0(file.name,".", image.type), overwrite = TRUE))
               write (sprintf ('saving image to %s.%s', file.name, image.type), stderr ())
             }else{
               write (sprintf ('choose another filename. File exists: %s', file.name), stderr ())
             }
           }) # saveImage
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveNetwork', 'CytoscapeWindowClass',

   function (obj, file.name, format='cys') {
       if (!file.exists(file.name)){
           # TODO currently only saves as cys, enable to save also to other formats incl. glm
           resource.uri <- paste(obj@uri, pluginVersion(obj), "session", sep="/")
           request.res <- POST(url=resource.uri, body=NULL, write_disk(paste0(file.name, ".cys"), overwrite = TRUE))
           write (sprintf ('saving network to file %s.cys', file.name), stderr ())
           invisible(request.res)
       }
     })

#-----------------------------------------------------------------------------------------------------------------------
.classicGraphToNodePairTable = function (g)
{
  edges.g <- edges(g)
  edge.names = as.character(unlist(sapply(names(edges.g), function (a) {
         bs = edges.g[[a]]; 
         if (length (bs) > 0) paste (a, edges.g[[a]], sep='~') 
         })))
  # print(class(edge.names))
  # print(edge.names)
  pairs = strsplit (edge.names, '~')
  a = sapply (pairs, "[", 1)
  b = sapply (pairs, "[", 2)

  if ('edgeType' %in% eda.names (g)){
      edgeType = as.character (edgeData (g, from=a, to=b, attr='edgeType'))
  }else{
      edgeType = rep ('unspecified', length (a))
  }


  return (data.frame (source=a, target=b, edgeType=edgeType, stringsAsFactors=FALSE))

} # .classicGraphToNodePairTable
#------------------------------------------------------------------------------------------------------------------------
.multiGraphToNodePairTable = function (mg)
{
  edge.set.names = edgeSets (mg)

  template = list (source='', target='', edgeType='')
  tbl = data.frame (template, stringsAsFactors=FALSE)
  for (edge.set in edgeSets (mg)) {
    tilde.names = edgeNames (mg, edge.set)
    pairs = strsplit (tilde.names, '~')
    for (pair in pairs) {
      source.node = pair [1]
      target.node = pair [2]
      new.row = list (source=source.node, target=target.node, edgeType=edge.set)
      tbl = rbind (tbl, new.row)
      }
    } # for edge
  
  invisible (tbl [-1,])     

} # .multiGraphToNodePairTable

# ------------------------------------------------------------------------------
# the bioc graph 'edgeNames' function does not detect, distinguish or report 
# reciprocal edges 
# this is fixed here
.rcyEdgeNames = function(g) 
{
   nodes.list = edges(g)
   result = c()
   for(source.node in names(nodes.list)) {
      target.nodes = nodes.list[[source.node]]
    
   if(length(target.nodes) == 0) {
      next;
   }
   for(target.node in target.nodes) {
      tilde.edge.name = sprintf('%s~%s', source.node, target.node) 
      result = c(result, tilde.edge.name)
      } # for target.node
   } # for source.node
   
   return(result)
}

#------------------------------------------------------------------------------------------------------------------------
.getNovelEdges = function (g.old, g.new)
{
   if (length (edges (g.old)) == 0){
      gOld.edgeCount = 0
   } else {
      gOld.edgeCount = length (edgeNames (g.old))
   }

   if (length (edges (g.new)) == 0){
      gNew.edgeCount = 0
   } else{
      gNew.edgeCount = length (edgeNames (g.new))
   }
   
   if (gNew.edgeCount == 0){
      return (NA)
   }
   if (gOld.edgeCount == 0){
      return (cy2.edge.names (g.new))
   }

   old.edges = cy2.edge.names (g.old)
   new.edges = cy2.edge.names (g.new)
   novel.edges = setdiff (new.edges, old.edges)
   novel.edges.indices = match (novel.edges, as.character (new.edges))
   return (new.edges [novel.edges.indices])


} # .getNovelEdges

# ------------------------------------------------------------------------------
is.classic.graph = function(obj)
{
    obj.classes = is(obj)
    
    return ('graph' %in% obj.classes)
    
} # is.classic.graph

# ------------------------------------------------------------------------------
is.multiGraph = function(obj)
{
    obj.classes = is(obj)
    
    return('MultiGraph' %in% obj.classes)
    
} # is.multiGraph

# ------------------------------------------------------------------------------
# capitalizes the first letter of all words in a string
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    
    return(paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=""))
} ### END simpleCap

# ------------------------------------------------------------------------------
getVisualProperty <- function(obj, vizmap.style.name, property) {
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", as.character(vizmap.style.name), "defaults", property, sep="/")
    request.res <- GET(url=resource.uri)
    return(fromJSON(rawToChar(request.res$content))[[2]])
}

# ------------------------------------------------------------------------------
setVisualProperty <- function(obj, style.string, vizmap.style.name='default') {
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", as.character(vizmap.style.name), "defaults", sep="/")
    style.JSON <- toJSON(list(style.string))
    request.res <- PUT(url=resource.uri, body=style.JSON, encode="json")
    invisible(request.res)
}

.isNotHexColor <- function(color){
    if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
        write (sprintf ('Error. %s is not a valid hexadecimal color (has to begin with # and be 7 characters long).', color), stderr ())
        return(TRUE)
    }else{
        return(FALSE)
    }
}
# ------------------------------------------------------------------------------
# obtain every other value for vector : used to resolve CyREST bug with returning column values
obtainEveryOtherValue <- function(v) {
    return(v[c(TRUE, FALSE)])
}

# ------------------------------------------------------------------------------
setNodePropertyDirect <- function(obj, node.names, new.values, visual.property) {
    # get network ID and version
    net.SUID <- as.character(obj@window.id)
    version <- pluginVersion(obj)
    
    # cyREST allows for multiple views per network
    # get all views that associate with this network and select the first one
    net.views.SUIDs <- .getNetworkViews(obj)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    node.SUIDs <- .nodeNameToNodeSUID(obj, node.names)
    # 'node.names' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(node.names))
    }
    if (length(new.values) != length(node.names)) {
        write(sprintf("ERROR in setNodePropertyDirect():\n   the number of nodes [%d] and new values [%d] are not the same >> node(s) attribute couldn't be set", 
                      length(node.names), length(new.values)), stderr())
        return()
    } else if (length(node.names)==1) {
        # only one node
        resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "nodes", node.SUIDs, sep="/")
        node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=new.values)))
        request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
    } else {
        # multiple nodes
        for (i in seq(node.SUIDs)) {
            node.SUID <- as.character(node.SUIDs[i])
            current.value <- new.values[i]
            
            resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "nodes", node.SUID, sep="/")
            node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
            request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
        } # end for (node.SUID in node.SUIDs)
    }
    invisible(request.res)
}
## END setNodePropertyDirect

# ------------------------------------------------------------------------------
setEdgePropertyDirect <- function(obj, edge.names, new.values, visual.property) {
    # get network ID and version
    net.SUID <- as.character(obj@window.id)
    version <- pluginVersion(obj)
    
    # cyREST allows for multiple views per network
    # get all views that exist for this network and select the first one
    net.views.SUIDs <- .getNetworkViews(obj)
    view.SUID <- as.character(net.views.SUIDs[[1]])
    
    edge.SUIDs <- .edgeNameToEdgeSUID(obj, edge.names)
    
    # 'edge.names' and 'new.values' must have the same length
    if (length(new.values) == 1) {
        new.values <- rep(new.values, length(edge.names))
    }
    if (length(new.values) != length(edge.names)) {
        write(sprintf("ERROR in setEdgePropertyDirect():\n\t number of edge.names [%d] and new.values [%d] are not the same >> edge(s) attribute could not be set", 
                      length(edge.names), length(new.values)), stderr())
    } else {
        request.res <- c()
        for (i in seq(edge.SUIDs)) { 
            edge.SUID <- as.character(edge.SUIDs[i])
            current.value <- new.values[i]
            
            resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "edges", edge.SUID, sep="/")
            edge.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
            request.res <- PUT(url=resource.uri, body=edge.SUID.JSON, encode="json")
        }
        invisible(request.res)
    }
}
## END setEdgePropertyDirect

# ------------------------------------------------------------------------------
getEdgeNamesAndSUIDS <- function(obj){
    # map edge names to edge SUIDs
    resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultedge", sep="/")
    request.res <- GET(url=resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    # get the row information from the edge table
    row.lst <- request.res[[6]]
    suids <- sapply(row.lst, '[[', "SUID")
    names <- sapply(row.lst, '[[', "name")
    edge.dict <- as.data.frame(cbind(names, suids))
    return (edge.dict)
}

# ------------------------------------------------------------------------------
discreteMapping <- function(obj, attribute.name, control.points, colors, visual.property, columnType, style){
    mapped.content <- apply(cbind(control.points, colors), MARGIN=1,
                            FUN=function(s) {list(key=as.character(unname(s[[1]])), value=as.character(unname(s[[2]])))})
    
    discrete.mapping <- list(mappingType = "discrete", mappingColumn = attribute.name,
                             mappingColumnType = columnType, visualProperty=visual.property,
                             map = mapped.content)
    discrete.mapping.json <-toJSON(list(discrete.mapping))
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", style, "mappings", sep="/")
    request.res <- POST(url=resource.uri, body=discrete.mapping.json, encode="json")
    
    # inform the user if the request was a success or failure
    if (request.res$status == 201){
        write (sprintf ('Successfully set rule.'), stdout ())
    }else{
        write (sprintf ('Error. Could not set rule...'), stdout ())
    }
    
    invisible (request.res)
} # discreteMapping

# ------------------------------------------------------------------------------
continuousMapping <- function(obj, attribute.name, control.points, colors, visual.property, columnType, style){
    # continuous mapping
    mapped.content <- apply(cbind(control.points, colors[3:length(colors)-1]), MARGIN=1,
                            FUN=function(s) {list(value=as.character(unname(s[[1]])),
                                                  lesser=as.character(unname(s[[2]])),
                                                  equal=as.character(unname(s[[2]])),
                                                  greater=as.character(unname(s[[2]])))})
    
    # change the attributes values below the minimum and above the maximum
    mapped.content[[1]]$lesser <- colors[1]
    mapped.content[[length(mapped.content)]]$greater <- colors[length(colors)]
    
    continuous.mapping <- list(mappingType = "continuous", mappingColumn = attribute.name,
                               mappingColumnType = columnType, visualProperty=visual.property,
                               points = mapped.content)
    continuous.mapping.json <- toJSON(list(continuous.mapping))
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", style, "mappings", sep="/")
    request.res <- POST(url=resource.uri, body=continuous.mapping.json, encode="json")
    
    # inform the user if the request was a success or failure
    if (request.res$status == 201){
        write (sprintf ('Successfully set rule.'), stdout ())
    }else{
        write (sprintf ('Error. Could not set rule...'), stdout ())
    }
    invisible (request.res)
} # continuousMapping

# ------------------------------------------------------------------------------
findColumnType <- function(columnType){
    if (columnType=="double"){
        return("Double")
    } else if (columnType == "integer"){
        return("Integer")
    } else if (columnType == "logical"){
        return("Boolean")
    } else{
        return("String")
    }
} # findColumnType

# cyPlot
# New RCy3 function to read node and edge attributes according to class()
#
# Given a node attribute data frame (node.df) with the node names in column 1, 
# and an edge attribute data.frame (edge.df) with node names in the first two columns,
# cyPlot creates a graphNEL object with nodes, edges, and their attributes 
# that can be loaded into Cytoscape with CytoscapeWindow. 
#
#  Author: Mark Grimes
#	[cyPlot.5 in MGRCyFunctions.R]
#########################################################################################
#
cyPlot <- function (node.df, edge.df) {
  edge.nodes <- unique(c(as.character(edge.df[,1]),
                         as.character(edge.df[,2])))		
  mydata <- new("graphNEL",
                edgemode = 'directed',
                nodes = unique(c(as.character(node.df[, 1]),
                                 edge.nodes)))
  #	Set up and load all the node attributes
  # read class and convert factor to character as required
  node.df[,1] <- as.character(node.df[,1])
  edge.df[,1:2] <- sapply(edge.df[,1:2],
                          as.character)
  node.class <- sapply (node.df,
                        class)
  if (any(grep("factor", node.class))) {
    node.df[, grep("factor", node.class)] <- sapply(node.df[, grep("factor", node.class)],
                                                    as.character) }
  
  if (any(grep("integer", node.class))) {
    node.df[, grep("integer", node.class)] <- sapply(node.df[, grep("integer", node.class)],
                                                     as.numeric) }
  
  node.class <- sapply(node.df,
                       class)
  edge.class <- sapply(edge.df,
                       class)
  if (any(grep("factor", edge.class))) {
    edge.df[, grep("factor", edge.class)] <- sapply(edge.df[, grep("factor", edge.class)],
                                                    as.character) }
  edge.class <- sapply(edge.df,
                       class)
  
  # Nodes and attributes
  if (length(grep("character", node.class)) > 1) {
    for (i in 2:length(grep("character", node.class))) {
      mydata <- initNodeAttribute(graph = mydata,
                                  attribute.name = names(node.class[grep("character", node.class)])[i],
                                  attribute.type = 'char',
                                  default.value = 'undefined') 
      nodeData(mydata, n = as.character(node.df[, 1]), attr = names(node.class[grep("character", node.class)])[i]) <- as.character(node.df[,grep("character", node.class)[i]])		}
  }
  
  if (length(grep("numeric", node.class))){ 
    for (i in 1:length(grep("numeric", node.class))) {	
      mydata <- initNodeAttribute(graph = mydata,
                                  attribute.name = names(node.class[grep("numeric", node.class)])[i],
                                  attribute.type = 'numeric',
                                  default.value = 0.0) 
      nodeData(mydata, n = as.character(node.df[, 1]), attr = names(node.class[grep("numeric", node.class)])[i]) <- as.numeric(node.df[,grep("numeric", node.class)[i]])	}	
  }
  
  # Edges and attributes
  mydata = addEdge(as.vector(edge.df[,1],
                             mode = "character"),
                   as.vector(edge.df[,2],
                             mode = "character"),
                   mydata)
  
  if (length(grep("character", edge.class)) > 2){
    for (i in 3:length(grep("character", edge.class))) {
      mydata <- initEdgeAttribute(graph = mydata,
                                  attribute.name = names(edge.df[,grep("character", edge.class)])[i],
                                  attribute.type = 'char',
                                  default.value = 'undefined')
      edgeData(mydata, as.vector(edge.df[,1], mode = "character"), as.vector(edge.df[,2], mode = "character"), attr = names(edge.df[,grep("character", edge.class)])[i]) <- as.character(edge.df[,grep("character", edge.class)[i]])		}
  }
  if (any(grep("numeric", edge.class))){
    for (i in 1:length(grep("numeric", edge.class))) {	
      mydata <- initEdgeAttribute(mydata,
                                  attribute.name = names(edge.class[grep("numeric", edge.class)])[i],
                                  attribute.type = "numeric",
                                  default.value = 0)
      edgeData(mydata, as.vector(edge.df[,1], mode = "character"), as.vector(edge.df[,2], mode = "character"), attr = names(edge.class[grep("numeric", edge.class)])[i]) <- as.numeric(edge.df[,grep("numeric", edge.class)[i]])	}	
  }
  return(mydata)
}
# END cyPlot
