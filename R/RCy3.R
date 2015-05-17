# move these to the DESCRIPTION file
library(httr)
library(graph)
library(methods)
library(RJSONIO)
library(RCurl)
# ------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
# ------------------------------------------------------------------------------
setClass("CytoscapeConnectionClass", 
	representation = representation (uri="character"), 
	prototype = prototype (uri="http://localhost:1234/v1")
)

# ------------------------------------------------------------------------------
setClass("CytoscapeWindowClass", 
	representation = representation(
		title="character", 
		window.id='character', 
		graph="graphBase", 
		collectTimings="logical",
		suid.name.dict="list", 
    view.id='numeric'), 
	contains = 'CytoscapeConnectionClass', 
	prototype = prototype(title="R graph", 
		graph=new("graphNEL", edgemode='directed'), 
		uri="http://localhost:1234/v1", 
		collectTimings=FALSE, 
		suid.name.dict=list())
)
# ------------------------------------------------------------------------------
setGeneric ('ping', 
  signature='obj', function(obj) standardGeneric('ping'))
setGeneric ('pluginVersion', 
  signature='obj', function(obj) standardGeneric('pluginVersion'))
setGeneric ('msg', 
  signature='obj', function(obj, string) standardGeneric('msg'))
setGeneric ('clearMsg', 
  signature='obj', function(obj) standardGeneric('clearMsg'))
setGeneric ('createWindow', 
  signature='obj', function(obj) standardGeneric('createWindow'))
setGeneric ('createWindowFromSelection', 
  signature='obj', function(obj, new.windowTitle, return.graph) standardGeneric ('createWindowFromSelection'))
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

setGeneric ('getDefaultBackgroundColor',  signature='obj', 
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

setGeneric ('saveImage',                  signature='obj', function (obj, file.name, image.type, scale=1.0) standardGeneric ('saveImage'))
setGeneric ('saveNetwork',                signature='obj', function (obj, file.name, format='gml') standardGeneric ('saveNetwork'))

setGeneric ('setDefaultNodeShape',        signature='obj', function (obj, new.shape, vizmap.style.name='default') standardGeneric ('setDefaultNodeShape'))
setGeneric ('setDefaultNodeSize',         signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultNodeSize'))
setGeneric ('setDefaultNodeColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeColor'))
setGeneric ('setDefaultNodeBorderColor',  signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeBorderColor'))
setGeneric ('setDefaultNodeBorderWidth',  signature='obj', function (obj, new.width, vizmap.style.name='default') standardGeneric ('setDefaultNodeBorderWidth'))
setGeneric ('setDefaultNodeFontSize',     signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultNodeFontSize'))
setGeneric ('setDefaultNodeLabelColor',   signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeLabelColor'))

setGeneric ('setDefaultEdgeLineWidth',    signature='obj', function (obj, new.width, vizmap.style.name='default') standardGeneric ('setDefaultEdgeLineWidth'))
setGeneric ('setDefaultEdgeColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeColor'))
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
    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') standardGeneric ('setNodeShapeRule'))
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
setGeneric ('setNodeImageDirect',         signature='obj', function (obj, node.names, image.urls) standardGeneric ('setNodeImageDirect'))
setGeneric ('setNodeColorDirect',         signature='obj', function (obj, node.names, new.colors) standardGeneric ('setNodeColorDirect'))
setGeneric ('setNodeBorderWidthDirect',   signature='obj', function (obj, node.names, new.sizes) standardGeneric ('setNodeBorderWidthDirect'))
setGeneric ('setNodeBorderColorDirect',   signature='obj', function (obj, node.names, new.colors) standardGeneric ('setNodeBorderColorDirect'))

setGeneric ('setNodeOpacityDirect',       signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeOpacityDirect'))
setGeneric ('setNodeFillOpacityDirect',   signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeFillOpacityDirect'))
setGeneric ('setNodeLabelOpacityDirect',  signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeLabelOpacityDirect'))
setGeneric ('setNodeBorderOpacityDirect', signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeBorderOpacityDirect'))
setGeneric ('setEdgeOpacityDirect',         signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeOpacityDirect'))

#setGeneric ('setNodeOpacitiesDirect',       signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeOpacitiesDirect'))
#setGeneric ('setNodeFillOpacitiesDirect',   signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeFillOpacitiesDirect'))
#setGeneric ('setNodeLabelOpacitiesDirect',  signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeLabelOpacitiesDirect'))
#setGeneric ('setNodeBorderOpacitiesDirect', signature='obj', function (obj, node.names, new.values) standardGeneric ('setNodeBorderOpacitiesDirect'))
#setGeneric ('setEdgeOpacitiesDirect',       signature='obj', function (obj, edge.names, new.values) standardGeneric ('setEdgeOpacitiesDirect'))

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
setGeneric ('setEdgeLabelWidthDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeLabelWidthDirect'))


setGeneric ('setEdgeLineStyleRule',     signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') standardGeneric ('setEdgeLineStyleRule'))

setGeneric ('setEdgeLineWidthRule', signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.widths, default.width='1') standardGeneric ('setEdgeLineWidthRule'))

setGeneric ('setEdgeTargetArrowRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') standardGeneric ('setEdgeTargetArrowRule'))
setGeneric ('setEdgeSourceArrowRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') standardGeneric ('setEdgeSourceArrowRule'))

setGeneric ('setEdgeTargetArrowColorRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') standardGeneric ('setEdgeTargetArrowColorRule'))
setGeneric ('setEdgeSourceArrowColorRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') standardGeneric ('setEdgeSourceArrowColorRule'))

setGeneric ('setEdgeColorRule',         signature='obj',
    function (obj, edge.attribute.name, control.points, colors, mode, default.color='#FFFFFF') standardGeneric ('setEdgeColorRule'))

setGeneric ('setEdgeOpacityRule',          signature='obj', 
    function (obj, edge.attribute.name, control.points, opacities, mode) standardGeneric ('setEdgeOpacityRule'))


setGeneric ('getNodeCount',             signature='obj', function (obj) standardGeneric ('getNodeCount'))
setGeneric ('getEdgeCount',             signature='obj', function (obj) standardGeneric ('getEdgeCount'))
setGeneric ('getNodeAttribute',         signature='obj', function (obj, node.name, attribute.name) standardGeneric ('getNodeAttribute'))
setGeneric ('getAllNodeAttributes',     signature='obj', function (obj, onlySelectedNodes=FALSE) standardGeneric ('getAllNodeAttributes'))
setGeneric ('getEdgeAttribute',         signature='obj', function (obj, edge.name, attribute.name) standardGeneric ('getEdgeAttribute'))
setGeneric ('getAllEdgeAttributes',     signature='obj', function (obj, onlySelectedEdges=FALSE) standardGeneric ('getAllEdgeAttributes'))
setGeneric ('getNodeAttributeNames',    signature='obj', function (obj) standardGeneric ('getNodeAttributeNames'))
setGeneric ('getEdgeAttributeNames',    signature='obj', function (obj) standardGeneric ('getEdgeAttributeNames'))
setGeneric ('deleteNodeAttribute',      signature='obj', function (obj, attribute.name) standardGeneric ('deleteNodeAttribute'))
setGeneric ('deleteEdgeAttribute',      signature='obj', function (obj, attribute.name) standardGeneric ('deleteEdgeAttribute'))
setGeneric ('getAllNodes',              signature='obj', function (obj) standardGeneric ('getAllNodes'))
setGeneric ('getAllEdges',              signature='obj', function (obj) standardGeneric ('getAllEdges'))
setGeneric ('selectNodes',              signature='obj', function (obj, node.names, preserve.current.selection=TRUE) standardGeneric ('selectNodes'))
setGeneric ('getSelectedNodes',         signature='obj', function (obj) standardGeneric ('getSelectedNodes'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedNodeCount',     signature='obj', function (obj) standardGeneric ('getSelectedNodeCount'))
setGeneric ('hideNodes',                signature='obj', function (obj, node.names) standardGeneric ('hideNodes'))
# setGeneric ('unhideNodes',              signature='obj', function (obj, node.names) standardGeneric ('unhideNodes'))
setGeneric ('hideSelectedNodes',        signature='obj', function (obj) standardGeneric ('hideSelectedNodes'))
setGeneric ('invertNodeSelection',      signature='obj', function (obj) standardGeneric ('invertNodeSelection'))
setGeneric ('deleteSelectedNodes',      signature='obj', function (obj) standardGeneric ('deleteSelectedNodes'))

setGeneric ('selectEdges',              signature='obj', function (obj, edge.names, preserve.current.selection=TRUE) standardGeneric ('selectEdges'))
setGeneric ('invertEdgeSelection',      signature='obj', function (obj) standardGeneric ('invertEdgeSelection'))
setGeneric ('deleteSelectedEdges',      signature='obj', function (obj) standardGeneric ('deleteSelectedEdges'))

setGeneric ('getSelectedEdges',         signature='obj', function (obj) standardGeneric ('getSelectedEdges'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedEdgeCount',     signature='obj', function (obj) standardGeneric ('getSelectedEdgeCount'))
setGeneric ('hideSelectedEdges',        signature='obj', function (obj) standardGeneric ('hideSelectedEdges'))

setGeneric ('unhideAll',                signature='obj', function (obj) standardGeneric ('unhideAll'))

setGeneric ('getFirstNeighbors',        signature='obj', function (obj, node.names) standardGeneric ('getFirstNeighbors'))
setGeneric ('selectFirstNeighborsOfSelectedNodes',
                                        signature='obj', function (obj) standardGeneric ('selectFirstNeighborsOfSelectedNodes'))
setGeneric ('sfn',                      signature='obj', function (obj) standardGeneric ('sfn'))
#-----------------------------------------------------------
# methods related to transmitting data from Cytoscape to R
#-----------------------------------------------------------
setGeneric ('getWindowID',                   signature='obj', function (obj, window.title) standardGeneric ('getWindowID'))
setGeneric ('haveNodeAttribute',             signature='obj', function (obj, node.names, attribute.name) standardGeneric ('haveNodeAttribute'))
setGeneric ('haveEdgeAttribute',             signature='obj', function (obj, edge.names, attribute.name) standardGeneric ('haveEdgeAttribute'))
setGeneric ('copyNodeAttributesFromCyGraph', signature='obj', function (obj, window.id, existing.graph) standardGeneric ('copyNodeAttributesFromCyGraph'))
setGeneric ('copyEdgeAttributesFromCyGraph', signature='obj', function (obj, window.id, existing.graph) standardGeneric ('copyEdgeAttributesFromCyGraph'))
setGeneric ('getGraphFromCyWindow',          signature='obj', function (obj, window.title) standardGeneric ('getGraphFromCyWindow'))

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
  signature='obj', function(obj, other.graph) standardGeneric ('.addNodes'))
setGeneric ('.addEdges', 
  signature='obj', function(obj, other.graph) standardGeneric ('.addEdges'))

# ------------------------------------------------------------------------------
setValidity("CytoscapeWindowClass", function(object) {
  if(length(object@title) != 1) 
    "'title' is not a single string" 
  else if(!nzchar(object@title)) 
    "'title' is an empty string" 
  validObject(object@graph)
})

# ------------------------------------------------------------------------------
CytoscapeConnection = function(host='localhost', port=1234) {
  uri = sprintf('http://%s:%s', host, port)
  cc = new('CytoscapeConnectionClass', uri = uri)
  if (!url.exists(uri)){
      write(sprintf('Connection failed. Please ensure that you have Cytoscape open and CyREST installed.'), stderr())
      return()
  }
  return(cc)
}

# ------------------------------------------------------------------------------
# the 'new CytoscapeWindow' class constructor, defined as a simple function
CytoscapeWindow = function(title, graph=new('graphNEL', edgemode='directed'), host='localhost', 
                           port=1234, create.window=TRUE, overwriteWindow=FALSE, collectTimings=FALSE) 
{
  uri = sprintf('http://%s:%s', host, port)
  
  cy.tmp = CytoscapeConnection(host, port)
  check.cytoscape.plugin.version(cy.tmp)
  
  if(overwriteWindow) {
    if(title %in% as.character(getWindowList(cy.tmp))) {
      deleteWindow(cy.tmp, title)
    }
  }
  
  if(!is.na(getWindowID(cy.tmp, title))) {
    write(sprintf('There is already a window in Cytoscape named "%s".', title), stderr())
    write(sprintf('Please use a unique name, or set "overwriteWindow=TRUE".'), stderr())
    stop()
  }
  
  # add a label to each node if not already present. default label is the node name, the node ID
  if(is.classic.graph(graph)) 
    if(edgemode(graph) == 'undirected') {
      graph = remove.redundancies.in.undirected.graph(graph)
    }
  # are all node attributes properly initialized?
  node.attributes = noa.names(graph)
  if(length(node.attributes) > 0) {
    check.list = list()
    for(node.attribute in node.attributes) {
      check.list[[node.attribute]] = properlyInitializedNodeAttribute(graph, node.attribute)
    }
    uninitialized.attributes = which(check.list == FALSE)
    if(length(uninitialized.attributes) > 0) {
      write(sprintf("%d uninitialized node attribute/s", length(uninitialized.attributes)), stderr())
      return()
    }
  } # if node.attributes

  # are all edge attributes properly initialized?
  edge.attributes = eda.names(graph)
  if(length(edge.attributes) > 0) {
    check.list = list()
    for(edge.attribute in edge.attributes) {
      check.list[[edge.attribute]] = properlyInitializedEdgeAttribute(graph, edge.attribute)
    }
    uninitialized.attributes = which(check.list == FALSE)
    if(length(uninitialized.attributes) > 0) {
      write(sprintf("%d uninitialized edge attribute/s", length(uninitialized.attributes)), stderr())
      return()
    }
  } # if edge.attributes
  
  if(!'label' %in% noa.names(graph)) {
    write('nodes have no label attribute -- adding default labels', stderr())
    graph = initNodeAttribute(graph, 'label', 'char', 'noLabel')
    if(length(nodes(graph) > 0)) {
      nodeData(graph, nodes(graph), 'label') = nodes(graph) # nodes(graph) returns strings
    }
  }
  
  cw = new('CytoscapeWindowClass', title=title, graph=graph, uri=uri, 
           collectTimings=collectTimings, suid.name.dict = list())
  
  if(create.window) {
    cw@window.id = createWindow(cw)
  }
  cw@collectTimings = collectTimings
  
  # let user know that a new window was created
  write(sprintf('New window named "%s" was created in Cytoscape.', title), stderr())
  
  return (cw)

} # CytsoscapeWindow

#------------------------------------------------------------------------------------------------------------------------
# the 'existing window' class constructor, defined as a simple function, with no formal link to the class
existing.CytoscapeWindow = function (title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE)
{
   #The constructor for the CytoscapeWindowClass, used when Cytoscape already contains and displays a network.
    uri <- sprintf('http://%s:%s', host, port)
    # create this (inexpensively) just to gain access to the window list
    cy.tmp <- CytoscapeConnection(host, port)
    check.cytoscape.plugin.version(cy.tmp)
    
    existing.window.id <- getWindowID(cy.tmp, title)
    
    # alert the user if the desired window does not exist
    if(is.na(existing.window.id)) {
        write(sprintf('There is no window in Cytoscape named "%s". Please choose from the following titles:.', title), stderr())
        write(as.character(getWindowList(cy.tmp)), stderr())
        return(NA)
    }
    
    # get graph from Cytoscape
    cw <- new('CytoscapeWindowClass', title=title, window.id=existing.window.id, uri=uri)
    if(copy.graph.from.cytoscape.to.R) {
        g.cy <- getGraphFromCyWindow(cw, title)
        cw <- setGraph(cw, g.cy)
    }
    return(cw)
} # existing.CytsoscapeWindow

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
    write(sprintf('This version of the RCytoscape package requires CyREST plugin version %s or greater.', expected.version), 
          stderr ())
    write(sprintf('However, you are using version %s. You must upgrade.', plugin.version), stderr ())
    write('Please visit the plugins page at http://www.cytoscape.org.', stderr ())
    write(' ', stderr())
    stop('Wrong CyREST version.')
  }
} # check.cytoscape.plugin.version

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
  })
#------------------------------------------------------------------------------------------------------------------------
setMethod('pluginVersion', 'CytoscapeConnectionClass', 
  function(obj) {
    res <- GET(obj@uri)
    # get vector with available plugin versions
    available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
    
    api.version <- character(0)
    
    # loop through the vector and check which is the correct plugin version
    for(i in 1:length(available.api.versions)) {
      api.str <- paste(obj@uri, available.api.versions[i], sep="/")
      
      server.status.res <- GET(api.str)
      
      if(server.status.res$status_code == 200) {
        api.version <- fromJSON(rawToChar(server.status.res$content))$apiVersion
      }
    }
    # api.version will always be the highest / latest version
    return(api.version)
  })

# ------------------------------------------------------------------------------
setMethod('msg', 'CytoscapeConnectionClass', function (obj, string) { 
  ### invisible (xml.rpc (obj@uri, 'Cytoscape.setStatusBarMessage', string))
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('clearMsg', 'CytoscapeConnectionClass', 
  function (obj) { 
#    invisible (xml.rpc (obj@uri, 'Cytoscape.clearStatusBarMessage'))
    })

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
#    if (getSelectedNodeCount (obj) == 0) {
#      write (noquote ('RCytoscape::createWindowFromSelection error:  no nodes are selected'), stderr ())
#      return (NA)
#      }
#    if (new.windowTitle %in% as.character (getWindowList (obj))) {
#      msg = sprintf ('RCytoscape::createWindowFromSelection error:  window "%s" already exists', new.windowTitle)
#      write (noquote (msg), stderr ())
#      return (NA)
#      }
#      
#    window.id = xml.rpc (obj@uri, 'Cytoscape.createNetworkFromSelection', obj@window.id, new.windowTitle)
#    return (existing.CytoscapeWindow (new.windowTitle, copy.graph.from.cytoscape.to.R = return.graph))
    }) # createWindowFromSelection

# ------------------------------------------------------------------------------
setMethod('getWindowCount', 'CytoscapeConnectionClass',
  function(obj) {
    resource.uri <- paste(obj@uri, pluginVersion(obj), "networks/count", sep="/")
    res <- GET(resource.uri)
    num.cytoscape.windows <- fromJSON(rawToChar(res$content))
    return(as.integer(num.cytoscape.windows))
})

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
    
    for(net.SUID in cy.networks.SUIDs)  {
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
    
    for(net.SUID in cy.networks.SUIDs)  {
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
setMethod ('deleteAllWindows',  'CytoscapeConnectionClass',
           # deletes all networks and associated windows in Cytoscape
           function (obj) {
              resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", sep="/")
              request.res <- DELETE(resource.uri)
              invisible(request.res)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'CytoscapeConnectionClass',

  function (obj) {
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
setMethod ('getLineStyles', 'CytoscapeConnectionClass',

  function (obj) {
      resource.uri <- paste(obj@uri, pluginVersion(obj), "styles/visualproperties/EDGE_LINE_TYPE/values", sep="/")
      request.res <- GET(resource.uri)
      request.res <- fromJSON(rawToChar(request.res$content))
      return(request.res$values)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getArrowShapes', 'CytoscapeConnectionClass',

   function (obj) {
       resource.uri <- paste(obj@uri, pluginVersion(obj), "styles/visualproperties/EDGE_TARGET_ARROW_SHAPE/values", sep="/")
       #Comment TanjaM: EDGE_SOURCE_ARROW_SHAPE rather than TARGET returns the same results as of April 2015
       request.res <- GET(resource.uri)
       request.res <- fromJSON(rawToChar(request.res$content))
       return(request.res$values)
     })

# ------------------------------------------------------------------------------
setMethod('getLayoutNames', 'CytoscapeConnectionClass', 
  function(obj) {
    request.uri = paste(obj@uri, pluginVersion(obj), "apply/layouts", sep="/")
    request.res = GET(url=request.uri)
    available.layouts = unname(fromJSON(rawToChar(request.res$content)))
    
    return(available.layouts)
}) # getLayoutNames

#------------------------------------------------------------------------------------------------------------------------
setMethod('getLayoutNameMapping', 'CytoscapeConnectionClass', 
   function(obj) {
#   return (xml.rpc (obj@uri, 'Cytoscape.getLayoutNamesMapping'))
     }) # getLayoutNameMapping

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyNames', 'CytoscapeConnectionClass', 

   function (obj, layout.name) {
#     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutProperties', layout.name))
     }) # getLayoutProperties

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyType', 'CytoscapeConnectionClass', 

   function (obj, layout.name, property.name) {
#     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutPropertyType', layout.name, property.name))
     }) # getLayoutPropertyType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyValue', 'CytoscapeConnectionClass', 

   function (obj, layout.name, property.name) {
#     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutPropertyValue', layout.name, property.name))
     }) # getLayoutPropertyValue

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setLayoutProperties', 'CytoscapeConnectionClass', 

   function (obj, layout.name, properties.list) {
#     all.possible.props = getLayoutPropertyNames (obj, layout.name)   # will throw error if there are no modifiable properties
#     for (prop in names (properties.list)) {
#       if (!prop %in% all.possible.props)
#         write (sprintf ('%s is not a property in layout %s', prop, layout.name), stderr ())
#       else {
#         new.value = properties.list [[prop]]
#         result = xml.rpc (obj@uri, 'Cytoscape.setLayoutPropertyValue', layout.name, prop, as.character (new.value))
#         } # else
#       } # for prop
     }) # setLayoutProperties

# ------------------------------------------------------------------------------
setMethod ('setGraph', 'CytoscapeWindowClass', function(obj, graph) {
  if(edgemode(graph) == 'undirected') 
    graph = remove.redundancies.in.undirected.graph (graph)
  
  obj@graph = graph
  
  return (obj)
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

  function (obj, node.names, attribute.name) {
#    indices.of.nodes.with.attribute.value = which (xml.rpc (obj@uri, 'Cytoscape.nodesHaveAttribute', attribute.name, node.names))
#    if (length (indices.of.nodes.with.attribute.value) > 0)
#      return (node.names [indices.of.nodes.with.attribute.value])
#    else 
#      return (character(0))
    })

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveEdgeAttribute', 'CytoscapeConnectionClass',

  function (obj, edge.names, attribute.name) {
#    if (length (edge.names) == 1)
#      edge.names = rep (edge.names, 2)
#    indices.of.edges.with.attribute.value = which (xml.rpc (obj@uri, 'Cytoscape.edgesHaveAttribute', attribute.name, edge.names))
#    if (length (indices.of.edges.with.attribute.value) > 0)
#      return (unique (edge.names [indices.of.edges.with.attribute.value]))
#    else 
#      return (character(0))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyNodeAttributesFromCyGraph', 'CytoscapeConnectionClass',

  function (obj, window.id, existing.graph) {
#    node.attribute.names = getNodeAttributeNames (obj)
#    #node.attribute.names = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
#    for (attribute.name in node.attribute.names) {
#      known.node.names = xml.rpc (obj@uri, "Cytoscape.getNodes", window.id)
#      nodes.with.attribute = haveNodeAttribute (obj, known.node.names, attribute.name)
#      if (length (nodes.with.attribute) > 0) {
#        attribute.type = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeType', attribute.name, .convert=T)
#        write (sprintf ('retrieving %s "%s" attribute for %d nodes', attribute.type, attribute.name, length (nodes.with.attribute)), stderr ())
#        if (attribute.type == 'INTEGER') {
#          attribute.type = 'integer'
#          default.value = 0
#          }
#        else if (attribute.type == 'STRING') {
#          attribute.type = 'char'
#          default.value = 'unassigned'
#          }
#        else if (attribute.type == 'FLOATING') {
#          attribute.type = 'numeric'
#          default.value = as.numeric (0.0)
#          }
#        else {
#          write (sprintf ('RCytoscape::copyNodeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr ())
#          next ()
#          } 
#        existing.graph = initNodeAttribute (existing.graph, attribute.name, attribute.type, default.value)
#        if (length (nodes.with.attribute) == 0) next;
#        if (length (nodes.with.attribute) == 1)
#          attribute.values = xml.rpc (obj@uri, 'Cytoscape.getNodeAttribute', attribute.name, nodes.with.attribute)
#        else
#          attribute.values = xml.rpc (obj@uri, 'Cytoscape.getNodesAttributes', attribute.name, nodes.with.attribute)
#        nodeData (existing.graph, nodes.with.attribute, attribute.name) = attribute.values
#         } # if
#      } # for
#
#    return (existing.graph)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyEdgeAttributesFromCyGraph', 'CytoscapeConnectionClass',

  function (obj, window.id, existing.graph) {
#    edge.attribute.names = getEdgeAttributeNames (obj)
#    write (sprintf ('creating %d cytoscape-style edge names', length (edgeNames (existing.graph))), stderr ())
#    cy2.edgenames = as.character (cy2.edge.names (existing.graph))   # < 2 seconds for > 9000 edges
#  
#    for (attribute.name in edge.attribute.names) {
#      edges.with.attribute = haveEdgeAttribute (obj, cy2.edgenames, attribute.name)
#      if (length (edges.with.attribute) > 0) {
#         attribute.type = xml.rpc (obj@uri, 'Cytoscape.getEdgeAttributeType', attribute.name, .convert=T)
#         write (sprintf ('retrieving %s "%s" attribute for %d edges', attribute.type, attribute.name, length (edges.with.attribute)), stderr ())
#         if (attribute.type == 'INTEGER') {
#           attribute.type = 'integer'
#           default.value = 0
#           }
#         else if (attribute.type == 'STRING') {
#           attribute.type = 'char'
#           default.value = 'unassigned'
#           }
#         else if (attribute.type == 'FLOATING') {
#           attribute.type = 'numeric'
#           default.value = as.numeric (0.0)
#           }
#        else {
#          write (sprintf ('RCytoscape::copyEdgeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr ())
#          next ()
#          } 
#         existing.graph = initEdgeAttribute (existing.graph, attribute.name, attribute.type, default.value)
#         if (length (edges.with.attribute) == 1)
#           eda.value = unique (xml.rpc (obj@uri, 'Cytoscape.getEdgesAttributes', attribute.name, rep (edges.with.attribute, 2)))
#         else
#           eda.value = xml.rpc (obj@uri, 'Cytoscape.getEdgesAttributes', attribute.name, edges.with.attribute)
#         regex = ' *[\\(|\\)] *'
#         edges.tokens = strsplit (edges.with.attribute, regex)
#         source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
#         target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
#         edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
#         edgeData (existing.graph, source.nodes, target.nodes, attribute.name) = eda.value
#         } # if
#      } # for
#  
#     return (existing.graph)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getGraphFromCyWindow', 'CytoscapeConnectionClass',

  function (obj,  window.title) {
     # check if the network is non-empty and get its id
     window.id = getWindowID (obj, window.title)
     stopifnot (!is.na (window.id))
     node.count = getNodeCount(obj)
     if (node.count == 0){
        return (new ('graphNEL', edgemode='directed'))
     }
     
     # get the entire graph
     resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), sep="/")
     request.res <- GET(resource.uri)
     request.res <- fromJSON(rawToChar(request.res$content))
     
     # get nodes
     all.nodes <- request.res$elements$nodes
     all.nodes <- sapply(all.nodes, '[[', 1)
     #print(all.nodes)
     
     # get node names and add them to the R graph
     all.node.names <- unlist(all.nodes["name",])
     write (sprintf ('received %d nodes from %s', length (all.node.names), window.title), stderr ())
     g = new ("graphNEL", edgemode='directed')
     write (sprintf ('adding %d nodes to local graph', length (all.node.names)), stderr ())
     for(node in all.node.names){
        g = graph::addNode (node, g)
     }
     
     # get node attributes and add them to the R graph
     node.attribute.names = getNodeAttributeNames (obj)
     g = initNodeAttribute (g, 'edgeType', 'char', 'assoc')
     
     #for each attribute:
     #g = initNodeAttribute (g, 'type', 'char', 'undefined')
     #for each value
     #nodeData (g, 'C', 'label') = 'Gene C'
     
     #g = initEdgeAttribute (g, 'edgeType', 'char', 'assoc')

     # get edges
     edge.count = getEdgeCount(obj)
     
     if (edge.count > 0) {
#      regex = ' *[\\(|\\)] *'
#      all.edge.names = xml.rpc (obj@uri, "Cytoscape.getEdges", window.id)
#      write (sprintf ('received %d edges from %s', length (all.edge.names), window.title), stderr ())
#      edges.tokens = strsplit (all.edge.names, regex)
#      source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
#      target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
#      edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
#      write (sprintf ('adding %d edges to local graph', length (edges.tokens)), stderr ())
#      g = addEdge (source.nodes, target.nodes, g)
#      edgeData (g, source.nodes, target.nodes, 'edgeType') = edge.types
#      g = copyNodeAttributesFromCyGraph (obj, window.id, g)
#      g = copyEdgeAttributesFromCyGraph (obj, window.id, g)
      } # if edgeCount > 0
  
    return (g)
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
    
    write('ending sendNodes', stderr())
    # needed for 'pass-by-reference' R functionality 
    eval.parent(substitute(obj <- loc.obj))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addNodes', signature (obj='CytoscapeWindowClass'),

  function (obj, other.graph) {
#     if (length (nodes (other.graph)) == 0) {
#       write ('CytoscapeWindow.sendNodes, no nodes in other.graph.  returning', stderr ())
#       return ()
#       }
#     new.nodes = setdiff (nodes (other.graph), nodes (obj@graph))
#     invisible (xml.rpc (obj@uri, 'Cytoscape.createNodes', as.character (obj@window.id), new.nodes))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addEdges', signature (obj='CytoscapeWindowClass'),

  function (obj, other.graph) {
#    if (length (edgeNames (other.graph)) == 0) {
#       write ('CytoscapeWindow::.addEdges, no edges in graph.  returning', stderr ())
#       return ()
#       }
#                 
#       # extract and compare edge names
#    new.edgeNames = setdiff (edgeNames (other.graph), edgeNames (obj@graph))
#    #printf ('---- new.edgeNames %d', length (new.edgeNames))
#    #print (new.edgeNames)
#    new.edgeNames.withBar = gsub ('~','|', new.edgeNames)
#
#    tokens = strsplit (new.edgeNames, '~')
#    #tokens = strsplit (new.edgeNames, '~')
#    #tokens = strsplit (edgeNames (other.graph), '~')
#    a = sapply (tokens, function (tok) tok [1])
#    b = sapply (tokens, function (tok) tok [2])
#    edge.type = as.character (eda (other.graph, 'edgeType') [new.edgeNames.withBar])
#    #printf ('edge.type:    ')
#    #print (edge.type)
#
#    if (length (edge.type) == 1 && is.na (edge.type))
#      edge.type = rep ('unspecified', length (tokens))
#    directed = rep (TRUE, length (tokens))
#    forgive.if.node.is.missing = TRUE
#
#     # deferring this effiency (sending only new edges) for now.
#     # if ('edgeType' %in% eda.names (obj@graph) && 'edgeType' %in% eda.names (other.graph)) {
#     #   existing.edge.signatures = sort (paste (names (edgeNames (obj@graph)), as.character (eda (obj@graph, 'edgeType'))))
#     #   new.edge.signatures      = sort (paste (names (edgeNames (other.graph)), as.character (eda (other.graph, 'edgeType'))))
#     #   new.edges = 
#     #   } # if
#
#    #write ('---- about to xml.rpc call Cytoscape.createEdges', stderr ())
#    #write (a, stderr ())
#    #write (b, stderr ())
#    #write (edge.type, stderr ())
#    xml.rpc (obj@uri, 'Cytoscape.createEdges', as.character (obj@window.id), a, b, edge.type, directed, forgive.if.node.is.missing, .convert=F)
    }) # .addEdges


# ------------------------------------------------------------------------------
setMethod('addCyNode', 'CytoscapeWindowClass', function(obj, nodeName) { 
  loc.obj <- obj
  
  if(nodeName %in% getAllNodes(loc.obj)) {
    write(sprintf('RCy3::addCyNode, %s node already present in Cytoscape graph', nodeName), stderr())
    
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
  # TO DO: add the node to the cw@graph object
  # cw@graph <- addNode(nodeName, cw@graph)
  
  eval.parent(substitute(obj <- loc.obj))
})

# ------------------------------------------------------------------------------
setMethod('addCyEdge', 'CytoscapeWindowClass', 
  function (obj, sourceNode, targetNode, edgeType, directed) {
    loc.obj <- obj
    
    good.args = TRUE
    # check that there is only one source and only one target nodes
    if(length(sourceNode) > 1 || length(targetNode) > 1) {
      good.args = FALSE
      write(sprintf('RCytoscape::addEdge can have only one source and one target nodes'), stderr())
    }
    
    if(!sourceNode %in% getAllNodes(loc.obj)) {
      good.args = FALSE
      write(sprintf('RCytoscape::addEdge, %s node not in Cytoscape graph', sourceNode), stderr())
    }
    if(!targetNode %in% getAllNodes(loc.obj)) {
      good.args = FALSE
      write(sprintf('RCytoscape::addEdge, %s node not in Cytoscape graph', targetNode), stderr())
    }
    if(!good.args) {
      return(NA)
    }
    
    net.suid <- as.character(loc.obj@window.id)
    
    resource.uri <- paste(loc.obj@uri, pluginVersion(loc.obj), "networks", net.suid, "edges", sep="/")
    
    node.names.vec <- sapply(loc.obj@suid.name.dict, "[[", 1)
    edge.data <- list(source = loc.obj@suid.name.dict[[which(node.names.vec %in% sourceNode)]]$SUID, 
                      target = loc.obj@suid.name.dict[[which(node.names.vec %in% targetNode)]]$SUID, 
                      directed = directed, interaction = edgeType)
    
    edge.data.JSON <- toJSON(list(edge.data))
    
    new.cyedge.res <- POST(url=resource.uri, body=edge.data.JSON, encode='json')
})

#------------------------------------------------------------------------------------------------------------------------
# this method adds a new graph to an existing graph.  first the new nodes, then the new edges, then node attributes, then edge
# attributes
setMethod ('addGraphToGraph', 'CytoscapeWindowClass',

  function (obj, other.graph) {
#    .addNodes (obj, other.graph)  
#    .addEdges (obj, other.graph)
#  
#    node.attribute.names = noa.names (other.graph)
#    for (attribute.name in node.attribute.names) {
#      printf ('sending noa %s', attribute.name)
#      .sendNodeAttributesForGraph (obj, other.graph, attr=attribute.name)
#      }
#  
#    node.attribute.names = noa.names (other.graph)
#    for (attribute.name in node.attribute.names) {
#      printf ('sending noa %s', attribute.name)
#      .sendNodeAttributesForGraph (obj, other.graph, attr=attribute.name)
#      }
#  
#    edge.attribute.names = eda.names (other.graph)
#    for (attribute.name in edge.attribute.names) {
#      printf ('sending eda %s', attribute.name)
#      .sendEdgeAttributesForGraph (obj, other.graph, attr=attribute.name)
#      }
    }) # addGraphToGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod('sendEdges', 'CytoscapeWindowClass',
  function(obj) {
      net.SUID = as.character(obj@window.id)
      # check that there exist graph edges
      if(length(edgeNames(obj@graph)) == 0) {
          write('CyR3::sendEdges, no edges in graph. returning', stderr())
          return()
      }
      
      write(sprintf('transforming (%d) graph edges to nodePairTable', length(edgeNames(obj@graph))), stderr())
      
      if(obj@collectTimings) {
          start.time = Sys.time()
      }
      if(is.classic.graph(obj@graph)) {
          tbl.edges = .classicGraphToNodePairTable(obj@graph)
      }
      else if(is.multiGraph(obj@graph)) {
          tbl.edges = .multiGraphToNodePairTable(obj@graph)
      }
      if(obj@collectTimings) {
          write(sprintf(' *** create node pair table: %f secs', difftime(Sys.time(), start.time, units='secs')), stderr())
      }
      
      write(sprintf('sending %d edges', nrow(tbl.edges)), stderr())
      
      # source nodes vector
      source.nodes = tbl.edges$source
      # target nodes vector
      target.nodes = tbl.edges$target
      # edge types vector
      edge.type = tbl.edges$edgeType
      directed = rep(TRUE, length(source.nodes))
      
      # below setting could be set in RCytoscape, but does not exist in RCy3
      # forgive.if.node.is.missing = TRUE
      
      suid.name.dict.df = 
          data.frame(matrix(unlist(obj@suid.name.dict), nrow=length(obj@suid.name.dict), byrow=T), stringsAsFactors=FALSE)
      colnames(suid.name.dict.df) <- c("name", "SUID")
      
      source.node.SUIDs = 
          unname(sapply(source.nodes, function(n) {subset(suid.name.dict.df, name==n)$SUID}))
      
      target.node.SUIDs = 
          unname(sapply(target.nodes, function(n) {subset(suid.name.dict.df, name==n)$SUID}))
      
      edge.records = 
          apply(cbind(source.node.SUIDs, target.node.SUIDs, directed, edge.type), MARGIN=1, 
                FUN=function(r) {list(source=unname(r[[1]]), target=unname(r[[2]]), directed=unname(r[[3]]), interaction=unname(r[[4]]))})
      edge.records.JSON = toJSON(edge.records)
      
      resource.uri = paste(obj@uri, pluginVersion(obj), "networks", net.SUID, "edges", sep="/")
      request.res = POST(url=resource.uri, body=edge.records.JSON, encode="json")
      invisible(request.res)
  }) # sendEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod('layoutNetwork', 'CytoscapeWindowClass',
  function(obj, layout.name = 'grid') {
    if(!layout.name %in% getLayoutNames(obj)) {
      write(sprintf("layout.name '%s' is not recognized; call getLayoutNames(<CytoscapeWindow>) to see those which are supported", layout.name), stderr())
      return()
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
     load (filename)
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
     if (length (unknown.nodes) > 0) {
        node.names = intersect (node.names, nodes (obj@graph))
        write (sprintf ("Error!  unknown nodes in RCytoscape::setNodePosition"), stderr ())
        for (i in 1:length (unknown.nodes)){
           write (sprintf ("     %s", unknown.nodes [i]), stderr ())
        } # end for
        return ()
      } # end if 
     
     indices <- match(recognized.nodes, node.names)
     node.names <- recognized.nodes
     
     x.coords <- x.coords[indices]
     y.coords <- y.coords[indices]
     count = length (node.names)
     #stopifnot (length (x.coords) == count)
     #stopifnot (length (y.coords) == count)

     if (count == 0){
        return ()
     }
     
     # TODO we might want to check if the coordinates are valid numbers
     
     # set x position
     res <- setNodePropertyDirect(obj, node.names, x.coords, "NODE_X_LOCATION")
     
     # set y position
     res <- setNodePropertyDirect(obj, node.names, y.coords, "NODE_Y_LOCATION")

     invisible(res)
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

#------------------------------------------------------------------------------------------------------------------------
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
setMethod('setNodeAttributes', 'CytoscapeWindowClass', function(obj, attribute.name) { 
  if(length(nodes(obj@graph)) == 0) {
    return()
  }
  suid.name.pairs = obj@suid.name.dict
  values = noa(obj@graph, attribute.name)
  caller.specified.attribute.class = attr(nodeDataDefaults(obj@graph, attribute.name), 'class')
  invisible(setNodeAttributesDirect(obj, attribute.name, caller.specified.attribute.class, suid.name.pairs, values))
}) ### END setNodeAttributes

# ------------------------------------------------------------------------------
setMethod('setNodeAttributesDirect', 'CytoscapeWindowClass', 
  function(obj, attribute.name, attribute.type, node.names, values) {
    if(length(node.names) == 0) {
      return()
    }
    # assert that each node has matching value for this attribute
    if(length(node.names) != length(values)) {
      write(sprintf('RCy3::setNodeAttributeDirect ERROR.'), stderr())
      write(sprintf('attribute name %s, node.names %d, values %d', attribute.name, length(node.names), length(values)), stderr())
      return()
    }
    #TODO check if nodes were already sent. If not, stop (TanjaM April 2015)
    
    caller.specified.attribute.class = simpleCap(tolower(attribute.type))
    # if the attribute's 'class'-parameter is null or empty, make it 'String'
    if(is.null(caller.specified.attribute.class) || length(caller.specified.attribute.class) == 0) {
      caller.specified.attribute.class = 'String'
    }
    # CyREST does not support 'Floating' data type, so have to use 'Double' 
    if(caller.specified.attribute.class == 'Floating') {
      caller.specified.attribute.class = 'Double'
    }
    
    res = ''
    plug.ver <- pluginVersion(obj)
    net.suid <- as.character(obj@window.id)
    # check that the attribute does not already exist in the Cytoscape network
    existing.attribute.names <- getNodeAttributeNames(obj)
    
    if(!attribute.name %in% existing.attribute.names) {
      # creates new 'Node Table' attribute (column) in Cytoscape
      # TO DO: [GIK] maybe put in seperate function
      new.cynode.tbl.col <- list(name=attribute.name, type=caller.specified.attribute.class)
      new.cynode.tbl.col.JSON <- toJSON(new.cynode.tbl.col)
      resource.uri <- paste(obj@uri, plug.ver, "networks", net.suid, "tables/defaultnode/columns", sep="/")
      new.cynode.tbl.col.res <- POST(url=resource.uri, body=new.cynode.tbl.col.JSON, encode="json")
      # END TO DO
    } 
    
    # TO DO: [GIK] maybe put in seperate function
    resource.uri <- paste(obj@uri, plug.ver, "networks", net.suid, "tables/defaultnode/columns", attribute.name, sep="/")
    
    name = sapply(obj@suid.name.dict, function(x) x$name)
    SUID = sapply(obj@suid.name.dict, function(x) x$SUID)
    df <- data.frame(name, SUID)
    
    df$value <- values
    pair <- apply(df[,c('SUID', 'value')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
    pair.JSON <- toJSON(pair)
    # print(pair.JSON)
    res <- PUT(url=resource.uri, body=pair.JSON, encode="json")
    # END TO DO
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeAttributes', 'CytoscapeWindowClass',

   function (obj, attribute.name) {
      if (length (edgeNames (obj@graph)) == 0){
         return ()
      }
      
      # get edge names and types
      caller.specified.attribute.class = attr (edgeDataDefaults (obj@graph, attribute.name), 'class')
      
      edge.names = as.character (cy2.edge.names (obj@graph))
      edge.names.tilde = names (cy2.edge.names (obj@graph))
      edge.names.with.bars = gsub ('~', '|', edge.names.tilde)
      values = unname(eda (obj@graph, attribute.name) [edge.names.with.bars])
      result = setEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, edge.names, values)
      invisible (result)
     }) # setEdgeAttributes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, edge.names, values) {
      write (sprintf ('entering setEdgeAttributesDirect, %s, with %d names and %d values',
                      attribute.name, length (edge.names), length (values)), stderr ())
      
      # return if no edge attributes
      if (length (edge.names) == 0){
         return ()
      }
      
      #write (sprintf ('edge.names: %s', unlist (edge.names)), stderr ())
      #write (sprintf ('    values: %s', unlist (values)), stderr ())
      
      caller.specified.attribute.class = tolower (attribute.type)
      
      if (is.null (caller.specified.attribute.class) || length (caller.specified.attribute.class) == 0){
         # NULL, or non-null but empty
         caller.specified.attribute.class = 'string'
      }
      
      result = ''
      # alert user if number of edges is not the same as number of edges with associated values
      if (length (edge.names) != length (values)) {
         write (sprintf ('RCytoscape::setEdgeAttributesDirect ERROR....'), stderr ())
         write (sprintf ('attribute name %s, edge.names %d, values %d', attribute.name, length (edge.names), length (values)), stderr ())
         return ();
      }

      # create table column for the attribute based on type
      if (caller.specified.attribute.class %in% c ('float', 'floating', 'numeric', 'double')) {
         attributes.to.send <- list(name=attribute.name, type="Double")
      } else if (caller.specified.attribute.class %in% c ('integer', 'int')) {
         attributes.to.send <- list(name=attribute.name, type="Integer")
      } else if (caller.specified.attribute.class %in% c ('string', 'char', 'character', 'STRING')) {
         attributes.to.send <- list(name=attribute.name, type="String")
      } else if (caller.specified.attribute.class %in% c ('logical', 'bool', 'boolean')) {
         attributes.to.send <- list(name=attribute.name, type="Boolean")
      } else{
         attributes.to.send <- list(name=toString(attribute.name), type="String")
      }

      # map edge names to edge SUIDs
      resource.uri = paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultedge", sep="/")
      request.res = GET(url=resource.uri)
      request.res = fromJSON(rawToChar(request.res$content))

      # get the row information from the edge table
      row.lst <- request.res[[6]]
      suids <- sapply(row.lst, '[[', "SUID")
      names <- sapply(row.lst, '[[', "name")
      edge.dict <- as.data.frame(cbind(names, suids))
      
      # send edge attributes as column names
      resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultedge/columns", sep="/")
      edge.attributes.JSON <- toJSON(list(attributes.to.send))
      request.res <- POST(url=resource.uri, body=edge.attributes.JSON, encode="json")
      
      # populate columns with attribute values
      resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultedge/columns", as.character(attribute.name), sep="/")
      df <- cbind(suids, values)
      attribute.values.to.send <- apply(df, 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
      attribute.values.to.send.JSON <- toJSON(attribute.values.to.send)
      
      request.res <- PUT(url=resource.uri, body=attribute.values.to.send.JSON, encode="json")

      invisible (request.res)
     }) # setEdgeAttributesDirect

# ------------------------------------------------------------------------------
setMethod('displayGraph', 'CytoscapeWindowClass', function(obj) {
  # R does not pass values by-reference
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
  if(loc.obj@collectTimings) {
    method.start.time = Sys.time()
    # start time (for sending nodes to Cytoscape) 
    stepwise.start.time = Sys.time()
  }
    
  write(sprintf('estimated displayGraph time: %8.1f seconds', estimated.time), stderr())
  write(sprintf('adding %d nodes...', length(nodes(obj@graph))), stderr())
  
  sendNodes(loc.obj)
    
  if(obj@collectTimings) {
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
}) # END displayGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod('predictTimeToDisplayGraph', 'CytoscapeWindowClass',
  function (obj) {
     g = obj@graph
     node.count = length(nodes(g))
     edge.count = length(edgeNames(g))
     noa.count = length(noa.names(g)) * node.count
     eda.count = length(eda.names(g)) * edge.count
     prediction = (0.002 * node.count) + (0.010 * edge.count) + (0.001 * noa.count) + (0.001 * eda.count)
     return (prediction)
  })
#------------------------------------------------------------------------------------------------------------------------
setMethod('redraw', 'CytoscapeWindowClass', 
  function(obj) {
    id = as.character(obj@window.id)
    
    api.str = paste(obj@uri, pluginVersion(obj), "apply/styles", "default", id, sep = "/")
    invisible(api.str)
    #res <- GET(api.str)
    #res.msg <- fromJSON(rawToChar(res$content))
    #invisible(res.msg)
  }) # redraw

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setWindowSize', 'CytoscapeWindowClass',

   function (obj, width, height) {
#     id = as.character (obj@window.id)
#     invisible (xml.rpc (obj@uri, 'Cytoscape.resizeNetworkView', id, as.integer (width), as.integer (height)))
     }) # redraw

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipInitialDelay', 'CytoscapeConnectionClass',

   function (obj, msecs) {
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipInitialDelay', as.integer (msecs)))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipDismissDelay', 'CytoscapeConnectionClass',

   function (obj, msecs) {
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipDismissDelay', as.integer (msecs)))
     })

# ------------------------------------------------------------------------------
setMethod('raiseWindow', 'CytoscapeConnectionClass', 
  function(obj, window.title = NA) {
    if(is.na(window.title)) {
      if(class(obj) == 'CytoscapeWindowClass') {
        window.id = obj@window.id
      } else {
        write(sprintf('error in RCy3::raiseWindow(), no window title provided'), stderr())
        return()
      }
    } # no window title
    # if window title was provided
    if(!is.na(window.title)) {
      window.id = getWindowID(obj, window.title)
      
      if(is.na(window.id)) {
        write(sprintf('error in RCy3::raiseWindow(), unrecognized window title: %s', window.title), stderr ())
        return()
      }
      # TO DO: call to raise the view
      
    } # window title was provided
}) # raiseWindow

#------------------------------------------------------------------------------------------------------------------------
setMethod ('showGraphicsDetails', 'CytoscapeConnectionClass',

  function (obj, new.value) {
#    invisible (xml.rpc (obj@uri, 'Cytoscape.setShowGraphicsDetails', new.value))
#    if (class (obj) == 'CytoscapeWindowClass')
#      redraw (obj)
    })

# ------------------------------------------------------------------------------
# display the graph using all of the available window space (the Cytoscape drawing canvas)
setMethod('fitContent', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    resource.uri = paste(obj@uri, pluginVersion(obj), "apply/fit", net.SUID, sep="/")
    request.res = GET(url=resource.uri)
    invisible(request.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('fitSelectedContent', 'CytoscapeWindowClass',

   function (obj) {
#     invisible (xml.rpc (obj@uri, 'Cytoscape.fitSelectedContent', obj@window.id))
     })

# ------------------------------------------------------------------------------
setMethod('getCenter', 'CytoscapeWindowClass', 
  function(obj) {
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
      write(sprintf("RCy3::getCenter() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
    }
    # get the X-coordinate
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "network/NETWORK_CENTER_X_LOCATION", sep="/")
    request.res <- GET(resource.uri)
    x.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
    # get the Y-coordinate
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "network/NETWORK_CENTER_Y_LOCATION", sep="/")
    request.res <- GET(resource.uri)
    y.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
    return(list(x = x.coordinate, y = y.coordinate))
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setCenter', 'CytoscapeWindowClass',
    # This method can be used to pan and scroll the Cytoscape canvas, which is adjusted (moved) so that the specified
    # x and y coordinates are at the center of the visible window.
   function (obj, x, y) {
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
           write(sprintf("RCy3::setCenter() - %d views found... setting coordinates of the first one", length(net.views.SUIDs)), stderr())
       }
       
       # set the X-coordinate
       resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "network/NETWORK_CENTER_X_LOCATION", sep="/")
       new.x.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_X_LOCATION", value=x)))
       request.res <- PUT(resource.uri, body=new.x.coordinate.JSON, encode="json")
       #x.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
       # set the Y-coordinate
       resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "network/NETWORK_CENTER_Y_LOCATION", sep="/")
       new.y.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_Y_LOCATION", value=y)))
       request.res <- PUT(resource.uri, body=new.y.coordinate.JSON, encode="json")
       invisible(request.res)
       #y.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]       
     })

# ------------------------------------------------------------------------------
setMethod('getZoom', 'CytoscapeWindowClass', 
  function(obj) {
    net.suid = as.character(obj@window.id)
    # cyREST API version
    version = pluginVersion(obj) 
    # get the views for the given network model
    # TO DO: put the SUIDs in vector in obj
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", sep="/") 
    request.res <- GET(resource.uri) 
    net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
    
    view.SUID <- as.character(net.views.SUIDs[[1]])
    # if multiple views are found, inform the user about it
    if(length(net.views.SUIDs) > 1) {
      write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
    }
    
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "network/NETWORK_SCALE_FACTOR", sep="/")
    request.res <- GET(resource.uri)
    zoom.level <- fromJSON(rawToChar(request.res$content))$value[[1]]
    
    return(zoom.level)
})

# ------------------------------------------------------------------------------
setMethod('setZoom', 'CytoscapeWindowClass', 
  function(obj, new.level) {
    net.suid = as.character(obj@window.id)
    version = pluginVersion(obj)
    
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", sep="/") 
    request.res <- GET(resource.uri) 
    net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
    
    view.SUID <- as.character(net.views.SUIDs[[1]])
    # if multiple views are found, inform the user about it
    if(length(net.views.SUIDs) > 1) {
      write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
    }
    
    view.zoom.value <- list(visualProperty = 'NETWORK_SCALE_FACTOR', value = new.level)
    
    view.zoom.value.JSON <- toJSON(list(view.zoom.value))
    
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "views", view.SUID, "network", sep="/")
    request.res <- PUT(url=resource.uri, body=view.zoom.value.JSON, encode="json")
    
    invisible(request.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getViewCoordinates', 'CytoscapeWindowClass',

   function (obj) {
#     tmp = xml.rpc (obj@uri, 'Cytoscape.getViewCoordinates', obj@window.id)
#     return (list (top.x=tmp[1], top.y=tmp[2], bottom.x=tmp[3], bottom.y=tmp[4]))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hidePanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
#     invisible (xml.rpc (obj@uri, 'Cytoscape.hidePanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideAllPanels', 'CytoscapeConnectionClass',

  function (obj) {
#    invisible (sapply (tolower (LETTERS), function (letter) hidePanel (obj, letter)))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('dockPanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
#     invisible (xml.rpc (obj@uri, 'Cytoscape.dockPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('floatPanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
#     invisible (xml.rpc (obj@uri, 'Cytoscape.floatPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeTooltipRule', 'CytoscapeWindowClass',

      # todo:  prevent the obligatory redraw
      # Comment TanjaM: the comment above was there already

      function (obj, node.attribute.name) {
          id = as.character (obj@window.id)
          viz.style.name = 'default'
          if (!node.attribute.name %in% noa.names (obj@graph)) {
              write (sprintf ('warning!  setNodeTooltipRule passed non-existent node attribute: %s', node.attribute.name), stderr ())
              return ()
          }
          attribute.values = as.character (noa (obj@graph, node.attribute.name))
          
          # set default tooltip
          default.tooltip <- list(visualProperty = "NODE_TOOLTIP", value = "")
          setVisualProperty(obj, default.tooltip, viz.style.name)
          
          # define the column type
          columnType <- findColumnType(typeof(attribute.values[1]))
          
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
                   write ("Error! RCytoscape:setNodeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
                   return ()
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
                       write ("RCytoscape::setNodeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
                } #
                good.args = length (control.points) == (length (colors) - 2)
                if (!good.args) {
                    write (sprintf ('cp: %d', length (control.points)), stderr ())
                    write (sprintf ('co: %d', length (colors)), stderr ())
                    write ("Error! RCytoscape:setNodeColorRule, interpolate mode.", stderr ())
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
                       write ("Error! RCytoscape:setNodeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                       return ()
                }
                   
                discreteMapping(obj, node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=vizmap.style.name)    
              
                } # else: !interpolate, aka lookup
     }) # setNodeColorRule


discreteMapping <- function(obj, attribute.name, control.points, colors, visual.property, columnType, style){
    mapped.content <- apply(cbind(control.points, colors), MARGIN=1,
                            FUN=function(s) {list(key=as.character(unname(s[[1]])), value=as.character(unname(s[[2]])))})
    
    discrete.mapping <- list(mappingType = "discrete", mappingColumn = attribute.name,
                             mappingColumnType = columnType, visualProperty=visual.property,
                             map = mapped.content)
    discrete.mapping.json <-toJSON(list(discrete.mapping))
    #print(discrete.mapping.json)
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", style, "mappings", sep="/")
    request.res <- POST(url=resource.uri, body=discrete.mapping.json, encode="json")
    #print(request.res)
    # inform the user if the request was a success of failure
    if (request.res$status == 201){
        return(TRUE)
    }else{
        write (sprintf ('Error. Could not set rule...'), stderr ())
    }

    invisible (request.res)
} # END discreteMapping

continuousMapping <- function(obj, node.attribute.name, control.points, colors, visual.property, columnType, style){
    # continuous mapping
    mapped.content <- apply(cbind(control.points, colors[3:length(colors)-1]), MARGIN=1,
                            FUN=function(s) {list(value=as.character(unname(s[[1]])),
                                                  lesser=as.character(unname(s[[2]])),
                                                  equal=as.character(unname(s[[2]])),
                                                  greater=as.character(unname(s[[2]])))})
    
    # change the attributes values below the minimum and above the maximum
    mapped.content[[1]]$lesser <- colors[1]
    mapped.content[[length(mapped.content)]]$greater <- colors[length(colors)]
    
    continuous.mapping <- list(mappingType = "continuous", mappingColumn = node.attribute.name,
                               mappingColumnType = columnType, visualProperty=visual.property,
                               points = mapped.content)
    continuous.mapping.json <- toJSON(list(continuous.mapping))
    
    resource.uri <- paste(obj@uri, pluginVersion(obj), "styles", style, "mappings", sep="/")
    request.res <- POST(url=resource.uri, body=continuous.mapping.json, encode="json")
    
    invisible (request.res)
} # END continuousMapping

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

#------------------------------------------------------------------------------------------------------------------------
# Cytoscape distinguishes between Node Opacity, Node Border Opacity, and Node Label Opacity.  we call this 'aspect' here.

setMethod ('setNodeOpacityRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, opacities, mode, aspect='all') {

#     if (!mode %in% c ('interpolate', 'lookup')) {
#       write ("Error! RCytoscape:setNodeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
#       return ()
#       }
#
#     aspect.all = length (grep ('all', aspect))  > 0
#     aspect.fill = length (grep ('fill', aspect)) > 0
#     aspect.border = length (grep ('border', aspect)) > 0
#     aspect.label = length (grep ('label', aspect)) > 0
#
#     if (aspect.all) {
#       aspect.fill = TRUE
#       aspect.border = TRUE
#       aspect.label = TRUE
#       }
#
#     if (aspect.fill == FALSE && aspect.border == FALSE && aspect.label == FALSE) {
#       specific.options = 'fill, border, label'
#       msg.1 = "Error! RCytoscape:setNodeOpacityRule.  apect must be 'all' (the default) "
#       msg.2 = sprintf ("or some combination, in any order, of %s", specific.options)
#       write (msg.1, stderr ())
#       write (msg.2, stderr ())
#       return ()
#       }
#
#     if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
#       if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
#         opacities = c (opacities [1], opacities, opacities [length (opacities)])
#         #write ("RCytoscape::setNodeOpacityRule, no 'below' or 'above' opacities specified.  Inferred from supplied opacities.", stderr ());
#         } # 
#
#       good.args = length (control.points) == (length (opacities) - 2)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (opacities)), stderr ())
#         write ("Error! RCytoscape:setNodeOpacityRule, interpolate mode.", stderr ())
#         write ("Expecting 1 opacity for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
#         return ()
#         }
#       
#       if (aspect.fill)
#         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Opacity', control.points, opacities, FALSE)
#       if (aspect.border) 
#         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Border Opacity', control.points, 
#                           opacities, FALSE)
#       if (aspect.label) 
#         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Label Opacity', control.points, 
#                           opacities, FALSE)
#       invisible (result)
#       } # if mode==interpolate
#
#     else { # mode==lookup, use a discrete rule, with no interpolation
#       good.args = length (control.points) == length (opacities)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (opacities)), stderr ())
#         write ("Error! RCytoscape:setNodeOpacityRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
#         return ()
#         }
#
#       default.style = 'default'
#       default.opacity = 255;
#       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
#         control.points = rep (control.points, 2)
#         opacities = rep (opacities, 2)
#         } 
#
#       if (aspect.fill)
#         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
#                           node.attribute.name, 'Node Opacity', as.character (default.opacity), control.points, as.character (opacities))
#       if (aspect.border) 
#         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
#                           node.attribute.name, 'Node Border Opacity', as.character (default.opacity), control.points, as.character (opacities))
#       if (aspect.label) 
#         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
#                           node.attribute.name, 'Node Label Opacity', as.character (default.opacity), control.points, as.character (opacities))
#       invisible (result)
#       } # else: !interpolate
     }) # setNodeOpacityRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorRule', 'CytoscapeWindowClass',

    function (obj, node.attribute.name, control.points, colors, mode, default.color='#000000') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCytoscape:setNodeBorderColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
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
                write ("Error! RCytoscape:setNodeBorderColorRule, interpolate mode.", stderr ())
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
                write ("Error! RCytoscape:setNodeBorderColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
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
       columnType <- findColumnType(typeof(line.widths[1]))

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
      if (new.shape %in% getNodeShapes(obj)){
          style = list(visualProperty = "NODE_SHAPE", value = new.shape)
          setVisualProperty(obj, style, vizmap.style.name)
      }else{
          write (sprintf ('%s is not a valid shape. Get valid shapes using getNodeShapes', new.shape), stderr ())
      }
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSize', 'CytoscapeConnectionClass', 
  function(obj, new.size, vizmap.style.name='default') {
    style = list(visualProperty = "NODE_SIZE", value = new.size)
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeColor', 'CytoscapeConnectionClass', 
  function(obj, new.color, vizmap.style.name='default') {
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setVisualProperty(obj, style, vizmap.style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeBorderColor', 'CytoscapeConnectionClass', 
  function(obj, new.color, vizmap.style.name='default') {
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
     # TODO Comment Tanja: maybe change to EDGE_UNSELECTED_PAINT
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
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

    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') {
        id = as.character (obj@window.id)
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'

        if (!node.attribute.name %in% noa.names (obj@graph)) {
            write (sprintf ('warning!  setNodeShapeRule passed non-existent node attribute: %s', node.attribute.name), stderr ())
            return ()
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

#   function (obj, node.attribute.name, attribute.values, node.sizes) {
#     id = as.character (obj@window.id)
#        # take the first and last node size, prepend and append them respectively, so that there are 2 more
#        # visual attribute values (in this case, node size in pixels) than there are node data attribute values
#     adjusted.node.sizes = c (node.sizes [1], node.sizes, c (node.sizes [length (node.sizes)]))
#     result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Size', 
#                       attribute.values, adjusted.node.sizes, FALSE)
#     return (result)
#     }) # setNodeSizeRule

   function (obj, node.attribute.name, control.points, node.sizes, mode, default.size=40) {

#     if (!mode %in% c ('interpolate', 'lookup')) {
#       write ("Error! RCytoscape:setNodeSizeRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
#       return ()
#       }
#
#     setDefaultNodeSize (obj, default.size)
#
#     if (mode=='interpolate') {  # need a 'below' size and an 'above' size.  so there should be two more colors than control.points 
#       if (length (control.points) == length (node.sizes)) { # caller did not supply 'below' and 'above' values; manufacture them
#         node.sizes = c (node.sizes [1], node.sizes, node.sizes [length (node.sizes)])
#         write ("RCytoscape::setNodeSizeRule, no 'below' or 'above' sizes specified.  Inferred from node.sizes.", stderr ())
#         } # 
#
#       good.args = length (control.points) == (length (node.sizes) - 2)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (node.sizes)), stderr ())
#         write ("Error! RCytoscape:setNodeSizeRule, interpolate mode.", stderr ())
#         write ("Expecting 1 node.size for each control.point, one for 'above' size, one for 'below' size.", stderr ())
#         return ()
#         }
#       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Size', control.points, node.sizes, FALSE)
#       invisible (result)
#       } # if mode==interpolate
#
#     else { # use a discrete rule, with no interpolation
#       good.args = length (control.points) == length (node.sizes)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (node.sizes)), stderr ())
#         write ("Error! RCytoscape:setNodeSizeRule.  Expecting exactly as many node.sizes as control.points in lookup mode.", stderr ())
#         return ()
#         }
#
#       default.style = 'default'
#       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
#         control.points = rep (control.points, 2)
#         node.sizes = rep (node.sizes, 2)
#         } 
#
#       result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
#                         node.attribute.name, 'Node Size', as.character (default.size), 
#                         as.character (control.points), as.character (node.sizes))
#       invisible (result)
#       } # else: !interpolate
     }) # setNodeSizeRule


#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeColorRule', 'CytoscapeWindowClass',
#
# function (obj, attribute.name, attribute.values, colors, default.color='#000000') {
#     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
#       attribute.values = rep (attribute.values, 2)
#       colors = rep (colors, 2)
#       }
#     setDefaultEdgeColor (obj, default.color)
#     id = as.character (obj@window.id)
#     default.color = '#000000'
#     result = xml.rpc (obj@uri, "Cytoscape.setEdgeColorRule", id, attribute.name, default.color, attribute.values, colors, .convert=TRUE)
#     invisible (result)
#     }) # setEdgeColorRule
#
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, control.points, colors, mode, default.color='#FFFFFF') {

#     if (!mode %in% c ('interpolate', 'lookup')) {
#       write ("Error! RCytoscape:setEdgeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
#       return ()
#       }
#
#     setDefaultEdgeColor (obj, default.color)
#     if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
#       if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
#         colors = c (colors [1], colors, colors [length (colors)])
#         #write ("RCytoscape::setEdgeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
#         } # 
#
#       good.args = length (control.points) == (length (colors) - 2)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (colors)), stderr ())
#         write ("Error! RCytoscape:setEdgeColorRule, interpolate mode.", stderr ())
#         write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
#         return ()
#         }
#       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousEdgeVisualStyle', edge.attribute.name, 'Edge Color', control.points, colors, FALSE)
#       invisible (result)
#       } # if mode==interpolate
#
#     else { # use a discrete rule, with no interpolation
#       good.args = length (control.points) == length (colors)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (colors)), stderr ())
#         write ("Error! RCytoscape:setEdgeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
#         return ()
#         }
#
#       default.style = 'default'
#       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
#         control.points = rep (control.points, 2)
#         colors = rep (colors, 2)
#         } 
#       result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
#                         edge.attribute.name, 'Edge Color', default.color, control.points, colors)
#       invisible (result)
#       } # else: !interpolate
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeOpacityRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, control.points, opacities, mode) {

#     if (!mode %in% c ('interpolate', 'lookup')) {
#       write ("Error! RCytoscape:setEdgeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
#       return ()
#       }
# 
#     aspects = c ('Edge Opacity', 'Edge Target Arrow Opacity', 'Edge Source Arrow Opacity')
#
#     if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
#       if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
#         opacities = c (opacities [1], opacities, opacities [length (opacities)])
#         } # 
#
#       good.args = length (control.points) == (length (opacities) - 2)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (opacities)), stderr ())
#         write ("Error! RCytoscape:setEdgeOpacityRule, interpolate mode.", stderr ())
#         write ("Expecting 1 opacity value for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
#         return ()
#         }
#       for (aspect in aspects)
#         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousEdgeVisualStyle', edge.attribute.name, aspect, control.points, opacities, FALSE)
#       invisible (result)
#       } # if mode==interpolate
#
#     else { # use a discrete rule, with no interpolation
#       good.args = length (control.points) == length (opacities)
#       if (!good.args) {
#         write (sprintf ('cp: %d', length (control.points)), stderr ())
#         write (sprintf ('co: %d', length (opacities)), stderr ())
#         write ("Error! RCytoscape:setEdgeColorRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
#         return ()
#         }
#
#       default.style = 'default'
#       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
#         control.points = rep (control.points, 2)
#         opacities = rep (opacities, 2)
#         } 
#       opacities = as.character (opacities)
#       for (aspect in aspects) {
#         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, edge.attribute.name, aspect, '255', control.points, opacities)
#         }
#
#       invisible (result)
#       } # else: !interpolate
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
            write (sprintf ('warning!  setEdgeLineWidthRule passed non-existent node attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # set default
        default.width.list <- list(visualProperty = "EDGE_WIDTH", value = default.width)
        setVisualProperty(obj, default.width.list, vizmap.style.name)
        
        # define the column type
        # TODO there seems to be an error in Cytoscape. It requires a String rather than a Double.
        # if we don't say columnType = String, an error occurs or the user has to input the numbers as strings
        #columnType <- findColumnType(typeof(line.widths[1]))
        columnType <- "String"
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, line.widths,
                         visual.property="EDGE_WIDTH", columnType=columnType, style=vizmap.style.name)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'CytoscapeWindowClass', 

    function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
        id = as.character (obj@window.id)
        #TODO the style should be passed as a parameter
        vizmap.style.name = 'default'
        
        if (!edge.attribute.name %in% eda.names (obj@graph)) {
            write (sprintf ('warning!  setEdgeTargetArrowRule passed non-existent node attribute: %s', edge.attribute.name), stderr ())
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

    function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
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

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
#     id = as.character (obj@window.id)
#     style.name = 'default'
#
#     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
#       attribute.values = rep (attribute.values, 2)
#       colors = rep (colors, 2)
#       } 
#
#     result = xml.rpc (obj@uri, "Cytoscape.createDiscreteMapper", style.name, edge.attribute.name,
#                      'Edge Target Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
#     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
#     id = as.character (obj@window.id)
#     style.name = 'default'
#
#     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
#       attribute.values = rep (attribute.values, 2)
#       colors = rep (colors, 2)
#       } 
#
#     result = xml.rpc (obj@uri, "Cytoscape.createDiscreteMapper", style.name, edge.attribute.name,
#                      'Edge Source Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
#     invisible (result)
     }) # setEdgeSourceArrowColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.colors) {
      for (current.color in new.colors){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setNodeColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
         }
      }
      # set the node color direct
      return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_FILL_COLOR"))
     })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeSizeDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.sizes) {
      for (current.size in new.sizes){
         # ensure the sizes are numbers
         if (!is.double(current.size)) {
            write (sprintf ('illegal size string "%s" in RCytoscape::setNodeSizeDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      # set the node properties direct
      res <- setNodePropertyDirect(obj, node.names, new.sizes, "NODE_WIDTH")
      res <- setNodePropertyDirect(obj, node.names, new.sizes, "NODE_HEIGHT")
      invisible(res)
     })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeWidthDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.widths) {
      for (current.width in new.widths){
         # ensure the sizes are numbers
         if (!is.double(current.width)) {
            write (sprintf ('illegal size string "%s" in RCytoscape::setNodeWidthDirect. It needs to be a number.', current.width), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.widths, "NODE_WIDTH"))
     })

#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeHeightDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.heights) {
      for (current.height in new.heights){
         # ensure the sizes are numbers
         if (!is.double(current.height)) {
            write (sprintf ('illegal size string "%s" in RCytoscape::setNodeHeightDirect. It needs to be a number.', current.height), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.heights, "NODE_HEIGHT"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.labels) {
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.labels, "NODE_LABEL"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeFontSizeDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.sizes) {
      for (current.size in new.sizes){
         # ensure the sizes are numbers
         if (!is.double(current.size)) {
            write (sprintf ('illegal size string "%s" in RCytoscape::setNodeFontSizeDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.sizes, "NODE_LABEL_FONT_SIZE"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.colors) {
      for (current.color in new.colors){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setNodeLabelColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.colors, "NODE_LABEL_COLOR"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.shapes) {
       if (new.shapes %in% getNodeShapes(obj)){
           # set the node property direct
           return(setNodePropertyDirect(obj, node.names, new.shapes, "NODE_SHAPE"))
       }else{
           write (sprintf ('%s is not a valid shape. Get valid shapes using getNodeShapes', new.shapes), stderr ())
       }
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeImageDirect', 'CytoscapeWindowClass',

    function (obj, node.names, image.urls) {
        return(setNodePropertyDirect(obj, node.names, paste0("org.cytoscape.ding.customgraphics.bitmap.URLImageCustomGraphics,14,bundle:", image.urls, ",bitmap image"), "NODE_CUSTOMGRAPHICS_1"))

#     if (length (image.urls) == 1)
#       image.urls = rep (image.urls, length (node.names))
#
#     if (length (node.names) != length (image.urls)) {
#       msg = sprintf ('error in RCytoscape::setNodeImageDirect.  image.urls count (%d) is neither 1 nor same as node.names count (%d)',
#                      length (image.urls), length (node.names))
#       write (msg, stderr ())
#       return ()
#       }
#    
#     id = as.character (obj@window.id)
#     for (i in 1:length (node.names)) {
#       setNodeShapeDirect (obj, node.names [i], 'rect')
#       setNodeLabelDirect (obj, node.names [i], '')
#       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names [i], 'Node Custom Graphics 1', image.urls [i])
#       }
#     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.sizes) {
      for (current.size in new.sizes){
         # ensure the sizes are numbers
         if (!is.double(current.size)) {
            write (sprintf ('illegal size string "%s" in RCytoscape::setNodeBorderWidthDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setNodePropertyDirect(obj, node.names, new.sizes, "NODE_BORDER_WIDTH"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.colors) {
      for (current.color in new.colors){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setNodeBorderColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
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
            write (sprintf ('illegal opacity string "%s" in RCytoscape::setNodeOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      res <- setNodePropertyDirect(obj, node.names, new.values, "NODE_TRANSPARENCY")
      res <- setNodePropertyDirect(obj, node.names, new.values, "NODE_BORDER_TRANSPARENCY")
      res <- setNodePropertyDirect(obj, node.names, new.values, "NODE_LABEL_TRANSPARENCY")
      invisible(res)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeFillOpacityDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.values) {
      for (current.value in new.values){
         # ensure the opacity value is between 0 and 255
         if (!is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('illegal opacity string "%s" in RCytoscape::setNodeFillOpacityDirect. It needs to be a double and between 0 and 255.', current.value), stderr ())
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
            write (sprintf ('illegal opacity string "%s" in RCytoscape::setNodeBorderOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
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
            write (sprintf ('illegal opacity string "%s" in RCytoscape::setNodeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
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
            write (sprintf ('illegal opacity string "%s" in RCytoscape::setEdgeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      # set the edge property direct
      #     property.names = c ('Edge Opacity',  'Edge Source Arrow Opacity', 'Edge Target Arrow Opacity')
      res <- setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_LABEL_TRANSPARENCY")
      res <- setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TRANSPARENCY")
      invisible(res)
     })

#------------------------------------------------------------------------------------------------------------------------
set.node.or.edge.properties = function (host.uri, property.name, names, values)
{
   # TODO this function can be deleted as we set node and edge properties via
   # setEdgePropertyDirect() and setNodePropertyDirect()
   
  if (length (names) == 1) {
#    name = names [1]
#    value = values [1]
#    if (length (grep ('Node', property.name) > 0))
#      target.function = "Cytoscape.setNodeProperty"
#    else {
#      target.function = "Cytoscape.setEdgeProperty"
#      name = as.character (name)   # either a nop, or a kindness to callers who pass in the full output of cy2.edge.names, a list
#      }
#    result = xml.rpc (host.uri, target.function, name, property.name , as.character (value))
#    invisible (result)
#    } # 1 name only
#  else {
#    if (length (values) == 1)  # apply this one value to all nodes or edges
#      values = rep (values, length (names))
#    properties = rep (property.name, length (names))
#    #print ('--- set.node.or.edge..properties')
#    #print (names)
#    #print (properties)
#    #print (as.character (values))
#    if (length (grep ('Node', property.name) > 0))
#      target.function = "Cytoscape.setNodeProperties"
#    else {
#      target.function = "Cytoscape.setEdgeProperties"
#      names = as.character (names)   # either a nop, or a kindness to callers who pass in the full output of cy2.edge.names, a list
#      }
#    result = xml.rpc (host.uri, target.function, names, properties, as.character (values))
#    #print ('--- back from xml.rpc')
#    invisible (result)
    } # else: 

} # set.node.or.edge.properties
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      for (current.color in new.value){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setEdgeColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
         }
      }
      # set the edge color direct
      # TODO maybe this should be EDGE_PAINT
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_STROKE_UNSELECTED_PAINT"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      # set the edge color direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeFontFaceDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_FACE"))

     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeFontSizeDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      for (current.size in new.value){
         # ensure the sizes are numbers
         if (!is.double(current.size)) {
            write (sprintf ('illegal font string "%s" in RCytoscape::setEdgeFontSizeDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_SIZE"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelColorDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      for (current.color in new.value){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setEdgeLabelColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
         }
      }
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_COLOR"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.values) {
      if (length (edge.names) != length (new.values)) {
         msg = sprintf ('error in RCytoscape::setEdgeTooltipDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                        length (new.values), length (edge.names))
         write (msg, stderr ())
         return ()
      }
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TOOLTIP"))

     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineWidthDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      for (current.size in new.value){
         # ensure the sizes are numbers
         if (!is.double(current.size)) {
            write (sprintf ('illegal size string "%s" in RCytoscape::setEdgeLineWidthDirect. It needs to be a number.', current.size), stderr ())
            return ()
         }
      }
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_WIDTH"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.values) {
      # TODO this if statement should be implemented for all node/edge direct functions
      if (length (edge.names) != length (new.values)) {
         msg = sprintf ('error in RCytoscape::setEdgeLineStyleDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                        length (new.values), length (edge.names))
         write (msg, stderr ())
         return ()
      }
      if (new.values %in% getLineStyles(obj)){
          # set the edge property direct
          return(setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_LINE_TYPE"))
      }else{
          write (sprintf ('%s is not a valid line style. Get valid types using getLineStyles', new.values), stderr ())
          return ()
      }
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowShapeDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
      if (length (edge.names) != length (new.values)) {
         msg = sprintf ('error in RCytoscape::setEdgeSourceArrowShapeDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                        length (new.values), length (edge.names))
         write (msg, stderr ())
         return ()
      }
      if (new.values %in% getArrowShapes(obj)){
          # set the edge property direct
          return(setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_SOURCE_ARROW_SHAPE"))
      }else{
          write (sprintf ('%s is not a valid shape. Get valid shapes using getArrowShapes', new.values), stderr ())
          return ()
      }
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowShapeDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
      if (length (edge.names) != length (new.values)) {
         msg = sprintf ('error in RCytoscape::setEdgeTargetArrowShapeDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                        length (new.values), length (edge.names))
         write (msg, stderr ())
         return ()
      }
      if (new.values %in% getArrowShapes(obj)){
          # set the edge property direct
          return(setEdgePropertyDirect(obj, edge.names, new.values, "EDGE_TARGET_ARROW_SHAPE"))
      }else{
          write (sprintf ('%s is not a valid shape. Get valid shapes using getArrowShapes', new.values), stderr ())
          return ()
      }
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.colors) {
      if (length (edge.names) != length (new.colors)) {
         msg = sprintf ('error in RCytoscape::setEdgeSourceArrowColorDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                        length (new.colors), length (edge.names))
         write (msg, stderr ())
         return ()
      }
      
      for (current.color in new.colors){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setEdgeSourceArrowColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
         }
      }
      
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.colors, "EDGE_SOURCE_ARROW_UNSELECTED_PAINT"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.colors) {
      if (length (edge.names) != length (new.colors)) {
         msg = sprintf ('error in RCytoscape::setEdgeTargetArrowColorDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                        length (new.colors), length (edge.names))
         write (msg, stderr ())
         return ()
      }
      
      for (current.color in new.colors){
         # ensure the color is formated in correct hexadecimal style
         if (substring(current.color, 1, 1) != "#" || nchar(current.color) != 7) {
            write (sprintf ('illegal color string "%s" in RCytoscape::setEdgeTargetArrowColorDirect. It needs to be in hexadecimal.', current.color), stderr ())
            return ()
         }
      }
      
      # set the edge property direct
      return(setEdgePropertyDirect(obj, edge.names, new.colors, "EDGE_TARGET_ARROW_UNSELECTED_PAINT"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelOpacityDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
      for (current.value in new.value){
         # ensure the opacity value is a double and between 0 and 255
         if (! is.double(current.value) || current.value < 0  || current.value > 255) {
            write (sprintf ('illegal opacity string "%s" in RCytoscape::setEdgeLabelOpacityDirect. It needs to be between 0 and 255.', current.value), stderr ())
            return ()
         }
      }
      # set the node property direct
      return(setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_TRANSPARENCY"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowOpacityDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.values) {

#     id = as.character (obj@window.id)
#
#     if (length (new.values) == 1)
#       new.values = rep (new.values, length (edge.names))
#     
#     if (length (edge.names) != length (new.values)) {
#       msg = sprintf ('error in RCytoscape::setEdgeSourceArrowOpacityirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
#                      length (new.values), length (edge.names))
#       write (msg, stderr ())
#       return ()
#       }
#
#     for (i in 1:length (edge.names))
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Source Arrow Opacity', as.character (new.values [i]))
#
#     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowOpacityDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {

#     id = as.character (obj@window.id)
#
#     if (length (new.values) == 1)
#       new.values = rep (new.values, length (edge.names))
#     
#     if (length (edge.names) != length (new.values)) {
#       msg = sprintf ('error in RCytoscape::setEdgeTargetArrowOpacityirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
#                      length (new.values), length (edge.names))
#       write (msg, stderr ())
#       return ()
#       }
#
#     for (i in 1:length (edge.names))
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Target Arrow Opacity', as.character (new.values [i]))
#
#     invisible (result)
     })


#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeLabelPositionDirect', 'CytoscapeWindowClass',
#   function (obj, edge.names, new.value) {
#     id = as.character (obj@window.id)
#     for (edge.name in edge.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Position', as.character (new.value))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelWidthDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {

#     id = as.character (obj@window.id)
#     for (edge.name in edge.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Width', as.character (new.value))
#     invisible (result)
     })
# ------------------------------------------------------------------------------
setMethod('getNodeCount', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    
    resource.uri = paste(obj@uri, version, "networks", net.SUID, "nodes/count", sep="/")
    request.res = GET(resource.uri)
    node.count = unname(fromJSON(rawToChar(request.res$content)))
    return(node.count)
})

# ------------------------------------------------------------------------------
setMethod('getEdgeCount', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    
    resource.uri = paste(obj@uri, version, "networks", net.SUID, "edges/count", sep="/")
    # request result
    request.res = GET(resource.uri)
    edge.count <- unname(fromJSON(rawToChar(request.res$content)))
    return(edge.count)
})

# ------------------------------------------------------------------------------
setMethod('getNodeAttribute', 'CytoscapeConnectionClass', 
  function (obj, node.name, attribute.name) {
     # map node names to node SUIDs
     dict.indices = which(sapply(obj@suid.name.dict, function(s) { s$name }) %in% node.name)
     node.SUID = sapply(obj@suid.name.dict[dict.indices], function(i) {i$SUID})
     
     # network ID and cyREST API version
     net.suid = as.character(obj@window.id)
     version = pluginVersion(obj)
     
     # get the node attribute in the nodes table
     resource.uri <- paste(obj@uri, version, "networks", net.suid, "tables/defaultnode/rows", as.character(node.SUID), attribute.name, sep="/")
     request.res <- GET(resource.uri)
     
     node.attribute.value <- unname(rawToChar(request.res$content))

     return(node.attribute.value)
})

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

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeAttribute', 'CytoscapeConnectionClass',

   function (obj, edge.name, attribute.name) {
       # get edge.name - edge.SUID table
       edge.name.suid.tbl <- getEdgeNamesAndSUIDS(obj)
       # TODO check if edge exists (TanjaM April 2015)
       edge.SUID <- edge.name.suid.tbl$suids[which(edge.name.suid.tbl$names == edge.name)]
       
       # network ID and cyREST API version
       net.suid = as.character(obj@window.id)
       version = pluginVersion(obj)
       
       # get the node attribute in the nodes table
       resource.uri <- paste(obj@uri, version, "networks", net.suid, "tables/defaultedge/rows", as.character(edge.SUID), attribute.name, sep="/")
       request.res <- GET(resource.uri)
       
       edge.attribute.value <- unname(rawToChar(request.res$content))
       
       return(edge.attribute.value)
     })

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
    net.SUID = as.character(obj@window.id)
    resource.uri = paste(obj@uri, pluginVersion(obj), "networks", net.SUID, "tables/defaultnode/columns", sep="/")
    # request result
    request.res = GET(url = resource.uri)
    request.res = fromJSON(rawToChar(request.res$content))
    request.res = data.frame(t(sapply(request.res, c)))
    request.res = unlist(request.res$name)
    # exclude the 'default' nodes
    node.attributes = request.res[! request.res %in% c("SUID", "shared name", "name", "selected")]
    invisible(request.res)
    return(node.attributes)
})

# ------------------------------------------------------------------------------
setMethod('getEdgeAttributeNames', 'CytoscapeConnectionClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    resource.uri = paste(obj@uri, pluginVersion(obj), "networks", net.SUID, "tables/defaultedge/columns", sep="/")
    # request result
    request.res = GET(url=resource.uri)
    request.res = fromJSON(rawToChar(request.res$content))
    request.res = data.frame(t(sapply(request.res, c)))
    request.res = unlist(request.res$name)
    edge.attributes = request.res[! request.res %in% c("SUID", "shared name", "name", "selected")]
    invisible(request.res)
    return(edge.attributes)
})

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
setMethod('getAllNodes', 'CytoscapeWindowClass', function(obj) {
  loc.obj <- obj
  
  # CyREST version
  version = pluginVersion(loc.obj)
  # network suid
  net.suid = as.character(loc.obj@window.id)
  resource.uri <- paste(loc.obj@uri, version, "networks", net.suid, "nodes/count", sep="/")
  
  count <- rawToChar(GET(resource.uri)$content)
  if(count == 0) {
    return()
  }
  # get SUIDs of existing (in Cytoscape) nodes
  resource.uri <- paste(loc.obj@uri, version, "networks", net.suid, "nodes", sep="/")
  # get the SUIDs of the nodes in the Cytoscape graph
  cy.nodes.SUIDs <- fromJSON(rawToChar(GET(resource.uri)$content))
  # translate the SUIDs to node names
  dict.suids.vec <- sapply(loc.obj@suid.name.dict, "[[", 2)
  # if there is difference b/n the nodes in obj@graph and Cytoscape
  diff.nodes <- setdiff(cy.nodes.SUIDs, dict.suids.vec)
  
  if(length(diff.nodes) > 0) {
    for(i in 1:length(diff.nodes)) {
      res.uri <- paste(loc.obj@uri, version, "networks", net.suid, "nodes", as.character(diff.nodes[i]), sep="/")
      
      node.name <- fromJSON(rawToChar(GET(res.uri)$content))$data$name
      
      loc.obj@suid.name.dict[[length(loc.obj@suid.name.dict) + 1]] <- list(name=node.name, SUID=diff.nodes[i])
    }
    ### TO DO: add new node to graph
  }
  indices <- which(dict.suids.vec %in% cy.nodes.SUIDs)
  
  node.names <- sapply(loc.obj@suid.name.dict, function(x) x[[1]])
  
  eval.parent(substitute(obj <- loc.obj))
  
  return(node.names)
}) # getAllNodes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdges', 'CytoscapeWindowClass',

   function (obj) {
       id = as.character (obj@window.id)
       count = getEdgeCount(obj)
       if (count == 0){
           return ()
       }
       # get edge name column and return its values
       resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", as.character(obj@window.id), "tables/defaultedge/columns/name", sep="/")
       request.res <- GET(url=resource.uri)
       request.res <- fromJSON(rawToChar(request.res$content))
       names <- request.res$values
       return(names)
     }) # getAllEdges

# ------------------------------------------------------------------------------
setMethod('clearSelection', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    # if any nodes are selected, unselect them
    resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns/selected?default=false", sep="/")
    request.res <- PUT(url=resource.uri, body=FALSE)
    
    redraw(obj)
    invisible(request.res)
}) # clearSelection
   
# ------------------------------------------------------------------------------
setMethod('selectNodes', 'CytoscapeWindowClass', 
  function(obj, node.names, preserve.current.selection = TRUE) {
    net.suid = as.character(obj@window.id)
    
    if(preserve.current.selection) {
      if(getSelectedNodeCount(obj) > 0) {
        node.names = unique(c(getSelectedNodes(), node.names))
      }
    }
    
    if(!preserve.current.selection) {
      clearSelection(obj)
    }
    # check for unknown nodes
    unknown.nodes = setdiff(node.names, nodes(obj@graph))
    if(length(unknown.nodes) > 0) {
      node.string = paste(unknown.nodes, collapse='')
      msg = paste('RCy3::selectNodes asked to select nodes not in graph: ', node.string)
      write(msg, stderr())
      node.names = intersect(node.names, nodes(obj@graph))
    }
    
    if(length(node.names) == 0) {
      invisible('no nodes to select')
    }
    
    version <- pluginVersion(obj)
    # update the 'selected' column
    resource.uri <- paste(obj@uri, version, "networks", net.suid, "tables/defaultnode/columns/selected", sep="/")
    
    node.suid.indices <- which(sapply(obj@suid.name.dict, function(d) d$name) %in% node.names)
    # set the RESTful request body(content)
    select.nodes.req <- lapply(obj@suid.name.dict[node.suid.indices], function(i) {list(SUID=i$SUID, value='true')})
    select.nodes.req.JSON <- toJSON(select.nodes.req)
    
    select.nodes.req.res <- PUT(url=resource.uri, body=select.nodes.req.JSON, encode="json")
    
    redraw(obj)
    invisible(select.nodes.req.res)
}) # selectNodes
   
# ------------------------------------------------------------------------------
setMethod('getSelectedNodeCount', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    resource.uri = paste(obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
    request.res = GET(url=resource.uri)
    
    num.selected.nodes = length(fromJSON(rawToChar(request.res$content)))
    return(num.selected.nodes)
}) # getSelectedNodeCount
   
# ------------------------------------------------------------------------------
setMethod('getSelectedNodes', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    
    if(getSelectedNodeCount(obj) == 0) {
      return(NA)
    } else {
      resource.uri = paste(obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
      request.res = GET(url=resource.uri)
      
      selected.nodes.SUIDs = fromJSON(rawToChar(request.res$content))
      selected.nodes.names = c()
      
      # uses the nodes dictionary; (not decided yet if the dictionary will be used)
      dict.indices = which(sapply(obj@suid.name.dict, function(s) { s$SUID }) %in% selected.nodes.SUIDs)
      selected.nodes.names = sapply(obj@suid.name.dict[dict.indices], function(i) {i$name})
      return(selected.nodes.names)
    }
}) # getSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
#     id = as.character (obj@window.id)
#     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedNodes', id, .convert=TRUE))
     }) # hideSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideNodes', 'CytoscapeWindowClass',

   function (obj, node.names) {
#     id = as.character (obj@window.id)
#     for (node in node.names)
#       invisible (xml.rpc (obj@uri, 'Cytoscape.hideNode', id, node, .convert=TRUE))
     }) # hideNodes
   
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('unhideNodes', 'CytoscapeWindowClass',
#
#   function (obj, node.names) {
#     id = as.character (obj@window.id)
#     for (node in node.names)
#       invisible (xml.rpc (obj@uri, 'Cytoscape.unhideNode', id, node, .convert=TRUE))
#     }) # unhideNodes
#   

# ------------------------------------------------------------------------------
# select all nodes that were not selected and deselect all nodes that were selected
setMethod('invertNodeSelection', 'CytoscapeWindowClass', function(obj) {
  net.SUID = as.character(obj@window.id)
  version = pluginVersion(obj)
  
  resource.uri = paste(obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=false", sep="/")
  request.res = GET(url=resource.uri)
  unselected.nodes.SUIDs = fromJSON(rawToChar(request.res$content))
  # clear the selection
  clearSelection(obj)
  
  to.be.selected.nodes = lapply(unselected.nodes.SUIDs, function(s) {list('SUID' = s, 'value' = TRUE)})
  to.be.selected.nodes.JSON = toJSON(to.be.selected.nodes)
  
  resource.uri = paste(obj@uri, version, "networks", net.SUID, "tables/defaultnode/columns/selected", sep="/")
  request.res = PUT(url=resource.uri, body=to.be.selected.nodes.JSON, encode="json")
  invisible(request.res)
  
  # another better option: call selectNodes() 
  # selectNodes(obj, c('A', 'C'), FALSE)
}) # invertNodeSelection
 
# ------------------------------------------------------------------------------
setMethod('deleteSelectedNodes', 'CytoscapeWindowClass', 
  function(obj) {
    loc.obj <- obj
    
    net.SUID = as.character(loc.obj@window.id)
    version = pluginVersion(loc.obj)
    
    resource.uri = paste(loc.obj@uri, version, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
    request.res = GET(url=resource.uri)
    selected.node.SUIDs = unname(fromJSON(rawToChar(request.res$content)))
    
    print(selected.node.SUIDs)
}) # deleteSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('selectEdges', 'CytoscapeWindowClass',

   function (obj, edge.names, preserve.current.selection=TRUE) {
#     id = as.character (obj@window.id)
#     if (preserve.current.selection)
#       edge.names = unique (c (getSelectedEdges (obj), edge.names))
#     if (length (edge.names) == 1)
#       result = xml.rpc (obj@uri, 'Cytoscape.selectEdge', id, edge.names, .convert=TRUE)
#     else
#       result = xml.rpc (obj@uri, 'Cytoscape.selectEdges', id, edge.names, .convert=TRUE)
#     redraw (obj)
#     invisible (result)
     }) # selectEdges
 
# ------------------------------------------------------------------------------
setMethod ('invertEdgeSelection', 'CytoscapeWindowClass', 
  function (obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    
    resource.uri = paste(obj@uri, version, "networks", net.SUID, "edges?column=selected&query=false", sep="/")
    request.res = GET(url=resource.uri)
    unselected.edges.SUIDs = fromJSON(rawToChar(request.res$content))
    # if any edges are selected, unselect them (nodes have clearSelection function) 
    resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns/selected?default=false", sep="/")
    request.res <- PUT(url=resource.uri, body=FALSE)
    
    to.be.selected.edges = lapply(unselected.edges.SUIDs, function(s) {list('SUID' = s, 'value' = TRUE)})
    to.be.selected.edges.JSON = toJSON(to.be.selected.edges)
    
    # better option: use selectEdges() function
    resource.uri = paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge/columns/selected", sep="/")
    request.res = PUT(url=resource.uri, body=to.be.selected.edges.JSON, encode="json")
    invisible(request.res)
}) # invertEdgeSelection
 
# ------------------------------------------------------------------------------
setMethod('deleteSelectedEdges', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    delete.from.root.graph.also = TRUE
    
}) # deleteSelectedEdges
   
# ------------------------------------------------------------------------------
setMethod('getSelectedEdgeCount', 'CytoscapeWindowClass', 
  function(obj) {
    net.SUID = as.character(obj@window.id)
    version = pluginVersion(obj)
    resource.uri = paste(obj@uri, version, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
    request.res = GET(url=resource.uri)
    
    num.selected.edges = length(fromJSON(rawToChar(request.res$content)))
    return(num.selected.edges)
}) # getSelectedEdgeCount
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
#     id = as.character (obj@window.id)
#     if (getSelectedEdgeCount (obj) == 0)
#       return (NA)
#     else {
#       result = xml.rpc (obj@uri, 'Cytoscape.getSelectedEdges', id, .convert=TRUE)
#       return (result)
#       }
}) # getSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
#     id = as.character (obj@window.id)
#     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedEdges', id, .convert=TRUE))
     }) # hideSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('unhideAll', 'CytoscapeWindowClass',

   function (obj) {
#     id = as.character (obj@window.id)
#     result = xml.rpc (obj@uri, 'Cytoscape.unhideAll', id, .convert=TRUE)
#     redraw (obj)
#     invisible (result)
     }) # unhideAll
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getFirstNeighbors', 'CytoscapeWindowClass',

   function (obj, node.names) {
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
            neighbor.names <- c(neighbor.names, sapply(obj@suid.name.dict[dict.indices], function(i) {i$name}))
         }
         return (neighbor.names)
      }
      })  # getFirstNeighbors

#------------------------------------------------------------------------------------------
setMethod ('selectFirstNeighborsOfSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
#     if (getSelectedNodeCount (obj) > 0) {
#       currently.selected = getSelectedNodes (obj)
#       if (length (currently.selected) == 0)
#         invisible ()
#       neighbors = getFirstNeighbors (obj, currently.selected)
#       full.selection = unique (c (currently.selected, neighbors))
#       selectNodes (obj, full.selection)
#       invisible (full.selection)
#       } # if any nodes are already selected
     }) # selectFirstNeighborsOfSelectedNodes

# ------------------------------------------------------------------------------
setMethod('sfn', 'CytoscapeWindowClass', function (obj) {
  selectFirstNeighborsOfSelectedNodes (obj)
})

#------------------------------------------------------------------------------------------------------------------------
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
#  all.edge.names = cy2.edge.names (graph) 
#  all.edge.names.cyStyle = as.character (all.edge.names) 
#  indices.of.edges.with.nodes = c () 
#
#  for (node in node.names) { 
#    node.regex.nodeA = sprintf ('^%s ', node)
#    node.regex.nodeB = sprintf (' %s$', node)
#    indices.A = grep (node.regex.nodeA, all.edge.names.cyStyle) 
#    indices.B = grep (node.regex.nodeB, all.edge.names.cyStyle) 
#    indices.of.edges.with.nodes = c (indices.of.edges.with.nodes, indices.A, indices.B) 
#    } # for node
#
#  return (unique (as.character (all.edge.names) [indices.of.edges.with.nodes]))

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
    
    g.simple = RCytoscape::makeSimpleGraph ()
    cws = CytoscapeWindow (window.title, g.simple)
    
    displayGraph (cws)
    layoutNetwork (cws, 'default')
    setNodeLabelRule (cws, 'label')
    
    node.attribute.values = c ("kinase",  "transcription factor")
    colors =                c ('#A0AA00', '#FF0000')
    setDefaultNodeBorderWidth (cws, 5)
    setNodeBorderColorRule (cws, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeRule (cws, 'count', count.control.points, sizes, mode='interpolate')
    setNodeColorRule (cws, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
    
    redraw (cws)
    
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
      edgeData(g, source.nodes, target.nodes, eda.name) <<- edgeData(gu, source.nodes, 
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
.cleanup = function (libpath)
{
   cw.closer = CytoscapeWindow ('closer', create.window=FALSE)
   deleteAllWindows (cw.closer)
  
} # .onUnload
#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been updated with new ones from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new nodes.
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new nodes, use the standard method 'setNodeAttributesDirect' to send them to cytoscape
# 
.sendNodeAttributesForGraph = function (obj, other.graph, attribute.name)
{
#  caller.specified.attribute.class = attr (nodeDataDefaults (other.graph, attribute.name), 'class')
#
#  if (is.null (caller.specified.attribute.class)) {
#    msg1 = sprintf ('Error!  RCytoscape:::.sendNodeAttributesForGraph. You must initialize the "%s" node attribute.', attribute.name)
#    msg2 = sprintf ('        example:  my.graph = initNodeAttribute (my.graph, attr="moleculeType", "char", "unspecified")')
#    write (msg1, stderr ())
#    write (msg2, stderr ())
#    return (NA)
#    }
#
#     # only add attributes for new nodes, unique to the new graph 'other.graph'
#   new.node.names = setdiff (nodes (other.graph), nodes (obj@graph))
#   values = noa (other.graph, attribute.name) [new.node.names]
#   invisible (setNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, new.node.names, values))

} # .sendNodeAttributesForGraph 
#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been extended with all the new nodes and edges from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new edges
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new edges, use the standard method 'setEdgeAttributesDirect' to send them to cytoscape
# oddities: edge naming is a tricky business.  cytoscape lablels edges like this:
#    <sourceNode> (interactionType) <targetNode>
# RCytoscape provide a utility function for retrieving them from an R graph object,   cy2.edge.names (g)
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

.sendEdgeAttributesForGraph = function (obj, other.graph, attribute.name)
{
#  caller.specified.attribute.class = attr (edgeDataDefaults (other.graph, attribute.name), 'class')
#
#  if (is.null (caller.specified.attribute.class)) {
#    msg1 = sprintf ('Error!  RCytoscape:::.sendEdgeAttributesForGraph. You must initialize the "%s" edge attribute.', attribute.name)
#    msg2 = sprintf ('        example:  my.graph = initEdgeAttribute (my.graph, attr="edgeType", "char", "unspecified")')
#    write (msg1, stderr ())
#    write (msg2, stderr ())
#    return (NA)
#    }
#
#     # send only attributes for edges which are unique to other.graph; we assume that any existing edges already have their attributes
#
#   printf ('other.graph: %d', length (edgeNames (other.graph)))
#   printf ('obj@graph: %d', length (edgeNames (obj@graph)))
#
#   novel.edges = .getNovelEdges (obj@graph, other.graph)
#
#   if (length (novel.edges) == 0)
#     return ()
#
#   new.edge.names.compact = names (novel.edges)
#   new.edge.names.cy2.style = as.character (novel.edges)
#
#   #print ('--- new.edge.names.compact')
#   #print (new.edge.names.compact)
#
#   #print ('--- new.edge.names.cy2.style')
#   #print (new.edge.names.cy2.style)
#
#   #new.edge.names.cy2.style = setdiff (as.character (cy2.edge.names (other.graph)), as.character (cy2.edge.names (obj@graph)))
#
#   new.edge.names.with.bar.delimitor = gsub ('~', '|', new.edge.names.compact)
#   values = eda (other.graph, attribute.name) [new.edge.names.with.bar.delimitor]
#   #write (sprintf ('sending edge attributes direct for attr %s', attribute.name), stderr ())
#   #write (new.edge.names.compact, stderr ())
#   #write (new.edge.names.with.bar.delimitor, stderr ())
#   #write ('---- new.edge.names.cy2.style', stderr ())
#   #write (new.edge.names.cy2.style, stderr ())
#   #write (values, stderr ())
#
#   invisible (setEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, new.edge.names.cy2.style, values))

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
    write(sprintf('network visual style has been set to (%s)', new.style.name), stderr())
    invisible(req.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('lockNodeDimensions', 'CytoscapeConnectionClass',

   function (obj, new.state, visual.style.name='default') {
#     if (! visual.style.name %in% getVisualStyleNames (obj)) {
#       write (sprintf ('Error in RCytoscape::lockNodeDimensions.  No visual style named "%s"', visual.style.name), stderr ())
#       return ()
#       }
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setNodeSizeLocked', visual.style.name, new.state))
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

   function (obj, new.color,vizmap.style.name='default') {
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
    style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
    setVisualProperty(obj, style, vizmap.style.name)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return(getVisualProperty(obj, vizmap.style.name, 'EDGE_PAINT'))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      style = list(visualProperty = "EDGE_PAINT", value = new.color) 
      setVisualProperty(obj, style, vizmap.style.name)
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveImage', 'CytoscapeWindowClass',

   function (obj, file.name, image.type, scale=1.0) {
      image.type = tolower (image.type)
      stopifnot (image.type %in% c ('png', 'pdf', 'svg'))
      id = as.character (obj@window.id)
      
      if (!file.exists(file.name)){
          # TODO Comment TanjaM - scaling not possible with the new version -- remove?
          
          # get the view image from Cytoscape in PNG
          if (image.type == 'png'){
             resource.uri <- paste(obj@uri, pluginVersion(obj), "networks", id,"views/first.png", sep="/")
          } else{
              write (sprintf ('Only png is currently supported.'), stderr ())
          }
          request.res <- GET(resource.uri, write_disk(paste0(file.name,".png")))
          
          write (sprintf ('saving image to %s.png', file.name), stderr ())
          
          # TODO add the other file types once CyREST allows for them, using else if statements
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
           request.res <- POST(url=resource.uri, body=NULL, write_disk(paste0(file.name, ".cys")))
           write (sprintf ('saving network to file %s.cys', file.name), stderr ())
           invisible(request.res)
       }
     })

#------------------------------------------------------------------------------------------------------------------------
hexColorToInt = function (hex.string)
{
   if (substr (hex.string, 1, 1) == '#') {
      base.index = 2
      if (nchar (hex.string) != 7)
         return (NA)
   }
   else {
      base.index = 1
      if (nchar (hex.string) != 6)
         return (NA)
   }
   
   red =   strtoi (substr (hex.string, base.index, base.index+1),   base=16)
   green = strtoi (substr (hex.string, base.index+2, base.index+3), base=16)
   blue =  strtoi (substr (hex.string, base.index+4, base.index+5), base=16)
   
   return (list (red=red, green=green, blue=blue))
   
} # hexColorToInt
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

  if ('edgeType' %in% eda.names (g))
    edgeType = as.character (edgeData (g, from=a, to=b, attr='edgeType'))
  else
    edgeType = rep ('unspecified', length (a))

  return (data.frame (source=a, target=b, edgeType=edgeType, stringsAsFactors=FALSE))

} # .classicGraphToNodePairTable
#------------------------------------------------------------------------------------------------------------------------
.multiGraphToNodePairTable = function (mg)
{
  edge.set.names = edgeSets (mg)

  template = list (source='', target='', edgeType='')
  tbl = data.frame (template, stringsAsFactors=F)
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

#-------------------------------------------------------------------------------
predictedDisplayGraphTime = function (graph) {

}

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
  redraw(obj)
}

# ------------------------------------------------------------------------------
# obtain every other value for vector : used to resolve CyREST bug with returning column values
obtainEveryOtherValue <- function(v) {
  return(v[c(TRUE, FALSE)])
}
# ------------------------------------------------------------------------------
setNodePropertyDirect <- function(obj, node.names, new.values, visual.property){
   # get network ID and version
   net.SUID <- as.character(obj@window.id)
   version <- pluginVersion(obj)
   
   # get the views for the given network model
   resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", sep="/")
   request.res <- GET(resource.uri)
   net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
   view.SUID <- as.character(net.views.SUIDs[[1]])
   
   for (pos in seq(node.names)){
      node.name <- node.names[pos]
      current.value <- new.values[pos]
      # map node name to node SUID
      dict.indices <- which(sapply(obj@suid.name.dict, function(s) { s$name }) %in% node.name)
      node.SUID <- sapply(obj@suid.name.dict[dict.indices], function(i) {i$SUID})
      
      # request 
      resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "nodes", as.character(node.SUID), sep="/")
      node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
      # request result
      request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
      print(request.res)
   } # end for (node.name in node.names)
   
   invisible(request.res)
}
# ------------------------------------------------------------------------------
setEdgePropertyDirect <- function(obj, edge.names, new.values, visual.property){
   # get network ID and version
   net.SUID <- as.character(obj@window.id)
   version <- pluginVersion(obj)
   
   # get the views for the given network model
   resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", sep="/")
   request.res <- GET(resource.uri)
   net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
   view.SUID <- as.character(net.views.SUIDs[[1]])
   
   # map edge names to edge SUIDs
   resource.uri <- paste(obj@uri, version, "networks", net.SUID, "tables/defaultedge", sep="/")
   request.res <- GET(url=resource.uri)
   request.res <- fromJSON(rawToChar(request.res$content))
   # get the row information from the edge table
   row.lst <- request.res[[6]]
   suids <- sapply(row.lst, '[[', "SUID")
   names <- sapply(row.lst, '[[', "name")
   edge.dict <- as.data.frame(cbind(names, suids))
   
   for (pos in seq(edge.names)){
      edge.name <- edge.names[pos]
      current.value <- new.values[pos]
      edge.SUID <- edge.dict$suids[which(names==edge.name)]
      
      # request 
      resource.uri <- paste(obj@uri, version, "networks", net.SUID, "views", view.SUID, "edges", as.character(edge.SUID), sep="/")
      node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
      
      # request result
      request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
   } # end for (edge.name in edge.names)
   
   invisible(request.res)
}

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