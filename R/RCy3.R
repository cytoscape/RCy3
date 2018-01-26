
#-----------------------------------------------------------
# methods taking an optional CytoscapeConnectionClass
#-----------------------------------------------------------
setGeneric ('ping', 	 	             function (obj) standardGeneric('ping'))
setGeneric ('apiVersion', 	             signature='obj', function (obj=CytoscapeConnection()) standardGeneric('apiVersion'))
setGeneric ('getNetworkCount',	         signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getNetworkCount'))
setGeneric ('getNetworkList',            signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getNetworkList'))
setGeneric ('deleteAllNetworks',	     signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('deleteAllNetworks'))
setGeneric ('getArrowShapes', 	         signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getArrowShapes'))
setGeneric ('getLayoutNames', 	         signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getLayoutNames'))
setGeneric ('getLayoutNameMapping',	     signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getLayoutNameMapping'))
setGeneric ('getLayoutPropertyNames',    signature='obj', function (obj=CytoscapeConnection(), layout.name) standardGeneric ('getLayoutPropertyNames'))
setGeneric ('getLayoutPropertyType',     signature='obj', function (obj=CytoscapeConnection(), layout.name, property.name) standardGeneric ('getLayoutPropertyType'))
setGeneric ('getLayoutPropertyValue',    signature='obj', function (obj=CytoscapeConnection(), layout.name, property.name) standardGeneric ('getLayoutPropertyValue'))
setGeneric ('setLayoutProperties',       signature='obj', function (obj=CytoscapeConnection(), layout.name, properties.list) standardGeneric ('setLayoutProperties'))
setGeneric ('getLineStyles',             signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getLineStyles'))
setGeneric ('getNodeShapes',             signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getNodeShapes'))
setGeneric ('hidePanel',				 signature='obj', function (obj=CytoscapeConnection(), panel.name) standardGeneric ('hidePanel'))
setGeneric ('hideAllPanels',			 signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('hideAllPanels'))
setGeneric ('dockPanel',				 signature='obj', function (obj=CytoscapeConnection(), panel.name) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',				 signature='obj', function (obj=CytoscapeConnection(), panel.name) standardGeneric ('floatPanel'))
setGeneric ('setTooltipInitialDelay',	 signature='obj', function (obj=CytoscapeConnection(), msecs) standardGeneric ('setTooltipInitialDelay'))
setGeneric ('setTooltipDismissDelay',	 signature='obj', function (obj=CytoscapeConnection(), msecs) standardGeneric ('setTooltipDismissDelay'))
setGeneric ('getDirectlyModifiableVisualProperties',    
                                         signature='obj', function(obj=CytoscapeConnection(), style.name="default") standardGeneric ('getDirectlyModifiableVisualProperties'))

#-----------------------------------------------------------
# methods taking an optional CytoscapeWindowClass; otherwise
# operating on current network in Cytoscape
#-----------------------------------------------------------
setGeneric ('createNetworkFromSelection',signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.title, return.graph=FALSE, exclude.edges=FALSE) standardGeneric ('createNetworkFromSelection'))
setGeneric ('cloneNetwork',              signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.title, return.graph = FALSE) standardGeneric('cloneNetwork'))
setGeneric ('deleteNetwork',  	         signature='obj', function (obj=CytoscapeWindowFromNetwork(), title=NA) standardGeneric ('deleteNetwork'))
setGeneric ('renameNetwork',             signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.title, return.graph = FALSE) standardGeneric('renameNetwork'))
setGeneric ('getAttributeClassNames', 	 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getAttributeClassNames'))
setGeneric ('addCyNode',                 signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.name) standardGeneric ('addCyNode'))
setGeneric ('addCyEdge',	             signature='obj', function (obj=CytoscapeWindowFromNetwork(), source.node.name, target.node.name, edgeType, directed) standardGeneric ('addCyEdge'))
setGeneric ('layoutNetwork',             signature='obj', function (obj=CytoscapeWindowFromNetwork(), layout.name='grid') standardGeneric ('layoutNetwork'))
setGeneric ('saveLayout',           	 signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename, timestamp.in.filename=FALSE) standardGeneric ('saveLayout'))
setGeneric ('restoreLayout',        	 signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename) standardGeneric ('restoreLayout'))
setGeneric ('setNodePosition',           signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, x.coords, y.coords) standardGeneric ('setNodePosition'))
setGeneric ('getNodePosition',			 signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('getNodePosition'))
setGeneric ('getNodeSize',				 signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('getNodeSize'))
setGeneric ('redraw',					 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('redraw'))
setGeneric ('raiseWindow',				 signature='obj', function (obj=CytoscapeWindowFromNetwork(), window.title=NA) standardGeneric ('raiseWindow'))
setGeneric ('setWindowSize',			 signature='obj', function (obj=CytoscapeWindowFromNetwork(), width, height) standardGeneric ('setWindowSize'))
setGeneric ('showGraphicsDetails',		 signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.value) standardGeneric ('showGraphicsDetails'))
setGeneric ('fitContent',				 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('fitContent'))
setGeneric ('fitSelectedContent',		 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('fitSelectedContent'))
setGeneric ('getCenter',				 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getCenter'))
setGeneric ('setCenter',				 signature='obj', function (obj=CytoscapeWindowFromNetwork(), x, y) standardGeneric ('setCenter'))
setGeneric ('getZoom',					 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getZoom'))
setGeneric ('setZoom',					 signature='obj', function (obj=CytoscapeWindowFromNetwork(), new.level) standardGeneric ('setZoom'))
setGeneric ('getViewCoordinates',		 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getViewCoordinates'))
setGeneric ('saveImage',                 signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename, image.type, h=600) standardGeneric ('saveImage'))
setGeneric ('saveNetwork',               signature='obj', function (obj=CytoscapeWindowFromNetwork(), filename, type='cys') standardGeneric ('saveNetwork'))
setGeneric ('getNodeCount',              signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getNodeCount'))
setGeneric ('getEdgeCount',              signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getEdgeCount'))
setGeneric ('getNodeAttributeType',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('getNodeAttributeType'))
setGeneric ('getEdgeAttributeType',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('getEdgeAttributeType'))
setGeneric ('getNodeAttributeNames',     signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getNodeAttributeNames'))
setGeneric ('getEdgeAttributeNames',     signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getEdgeAttributeNames'))
setGeneric ('deleteNodeAttribute',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('deleteNodeAttribute'))
setGeneric ('deleteEdgeAttribute',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name) standardGeneric ('deleteEdgeAttribute'))
setGeneric ('getAllNodeAttributes',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), onlySelectedNodes=FALSE) standardGeneric ('getAllNodeAttributes'))
setGeneric ('getAllEdgeAttributes',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), onlySelectedEdges=FALSE) standardGeneric ('getAllEdgeAttributes'))
setGeneric ('getNodeAttribute',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.name, attribute.name) standardGeneric ('getNodeAttribute'))
setGeneric ('getEdgeAttribute',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.name, attribute.name) standardGeneric ('getEdgeAttribute'))
setGeneric ('setNodeAttributesDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name, attribute.type, node.names, values) standardGeneric ('setNodeAttributesDirect'))
setGeneric ('setEdgeAttributesDirect', 	 signature='obj', function (obj=CytoscapeWindowFromNetwork(), attribute.name, attribute.type, edge.names, values) standardGeneric ('setEdgeAttributesDirect'))
setGeneric ('getAllNodes',               signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getAllNodes'))
setGeneric ('getAllEdges',               signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getAllEdges'))
setGeneric ('selectNodes',               signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, by.col='name', preserve.current.selection=TRUE) standardGeneric ('selectNodes'))
setGeneric ('selectAllNodes',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric('selectAllNodes'))
setGeneric ('getSelectedNodes',          signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedNodes'))
setGeneric ('clearSelection',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('clearSelection'))
setGeneric ('getSelectedNodeCount',      signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedNodeCount'))
setGeneric ('hideNodes',                 signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('hideNodes'))
setGeneric ('unhideNodes',               signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('unhideNodes'))
setGeneric ('hideSelectedNodes',         signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('hideSelectedNodes'))
setGeneric ('invertNodeSelection',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('invertNodeSelection'))
setGeneric ('deleteSelectedNodes',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('deleteSelectedNodes'))
setGeneric ('selectEdges',               signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, by.col='name', preserve.current.selection=TRUE) standardGeneric ('selectEdges'))
setGeneric ('selectAllEdges',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric('selectAllEdges'))
setGeneric ('invertEdgeSelection',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('invertEdgeSelection'))
setGeneric ('deleteSelectedEdges',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('deleteSelectedEdges'))
setGeneric ('getSelectedEdges',          signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedEdges'))
setGeneric ('clearSelection',            signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('clearSelection'))
setGeneric ('getSelectedEdgeCount',      signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('getSelectedEdgeCount'))
setGeneric ('hideSelectedEdges',         signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('hideSelectedEdges'))
setGeneric ('unhideAll',                 signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('unhideAll'))
setGeneric ('getFirstNeighbors',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, as.nested.list=FALSE) standardGeneric ('getFirstNeighbors'))
setGeneric ('selectEdgesConnectedBySelectedNodes', 
                                         signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('selectEdgesConnectedBySelectedNodes'))

#-----------------------------------------------------------
# methods related to visual styles
#-----------------------------------------------------------
setGeneric ('setVisualStyle',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), style.name) standardGeneric ('setVisualStyle'))
setGeneric ('getVisualStyleNames',    signature='obj', function (obj=CytoscapeConnection()) standardGeneric ('getVisualStyleNames'))
setGeneric ('copyVisualStyle',        signature='obj', function (obj=CytoscapeConnection(), from.style, to.style) standardGeneric ('copyVisualStyle'))
setGeneric ('lockNodeDimensions',     signature='obj', function (obj=CytoscapeConnection(), new.state, style.name='default') standardGeneric ('lockNodeDimensions'))

#--- defaults ----------------------------------------------
setGeneric ('getDefaultBackgroundColor',	        signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultBackgroundColor'))
setGeneric ('setDefaultBackgroundColor',            signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultBackgroundColor'))
setGeneric ('getDefaultNodeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultNodeSelectionColor'))
setGeneric ('setDefaultNodeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeSelectionColor'))
setGeneric ('getDefaultNodeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultNodeReverseSelectionColor'))
setGeneric ('setDefaultNodeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeReverseSelectionColor'))
setGeneric ('getDefaultEdgeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultEdgeSelectionColor'))
setGeneric ('setDefaultEdgeSelectionColor',         signature='obj', function (obj=CytoscapeConnection(), new.color,  style.name='default') standardGeneric ('setDefaultEdgeSelectionColor'))
setGeneric ('getDefaultEdgeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), style.name='default') standardGeneric ('getDefaultEdgeReverseSelectionColor'))
setGeneric ('setDefaultEdgeReverseSelectionColor',  signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeReverseSelectionColor'))

setGeneric ('setDefaultNodeShape',        signature='obj', function (obj=CytoscapeConnection(), new.shape, style.name='default') standardGeneric ('setDefaultNodeShape'))
setGeneric ('setDefaultNodeSize',         signature='obj', function (obj=CytoscapeConnection(), new.size, style.name='default') standardGeneric ('setDefaultNodeSize'))
setGeneric ('setDefaultNodeColor',        signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeColor'))
setGeneric ('setDefaultNodeBorderColor',  signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeBorderColor'))
setGeneric ('setDefaultNodeBorderWidth',  signature='obj', function (obj=CytoscapeConnection(), new.width, style.name='default') standardGeneric ('setDefaultNodeBorderWidth'))
setGeneric ('setDefaultNodeFontSize',     signature='obj', function (obj=CytoscapeConnection(), new.size, style.name='default') standardGeneric ('setDefaultNodeFontSize'))
setGeneric ('setDefaultNodeLabelColor',   signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultNodeLabelColor'))

setGeneric ('setDefaultEdgeLineWidth',        signature='obj', function (obj=CytoscapeConnection(), new.width, style.name='default') standardGeneric ('setDefaultEdgeLineWidth'))
setGeneric ('setDefaultEdgeLineStyle',        signature='obj', function (obj=CytoscapeConnection(), new.line.style, style.name='default') standardGeneric ('setDefaultEdgeLineStyle'))
setGeneric ('setDefaultEdgeColor',            signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeColor'))
setGeneric ('setDefaultEdgeSourceArrowColor', signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeSourceArrowColor'))
setGeneric ('setDefaultEdgeTargetArrowColor', signature='obj', function (obj=CytoscapeConnection(), new.color, style.name='default') standardGeneric ('setDefaultEdgeTargetArrowColor'))
setGeneric ('setDefaultEdgeFontSize',         signature='obj', function (obj=CytoscapeConnection(), new.size, style.name='default') standardGeneric ('setDefaultEdgeFontSize'))

#--- mappings --------------------------------------------
setGeneric ('setNodeLabelRule',         signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, style.name = 'default') standardGeneric ('setNodeLabelRule'))
setGeneric ('setEdgeLabelRule',         signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, style.name = 'default') standardGeneric ('setEdgeLabelRule'))
setGeneric ('setNodeColorRule',         signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, control.points, colors, mode, default.color='#FFFFFF', style.name = 'default') standardGeneric ('setNodeColorRule'))
setGeneric ('setNodeBorderColorRule',   signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, control.points, colors, mode, default.color='#000000', style.name = 'default') standardGeneric ('setNodeBorderColorRule'))
setGeneric ('setNodeBorderWidthRule',   signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, attribute.values, line.widths, default.width=1, style.name = 'default') standardGeneric ('setNodeBorderWidthRule'))
setGeneric ('setNodeShapeRule',         signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, attribute.values, node.shapes, default.shape='ELLIPSE', style.name = 'default') standardGeneric ('setNodeShapeRule'))
setGeneric ('setNodeSizeRule',          signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, control.points, node.sizes, mode, default.size=40, style.name = 'default') standardGeneric ('setNodeSizeRule'))
setGeneric ('setNodeOpacityRule',       signature='obj', function (obj=CytoscapeConnection(), node.attribute.name, control.points, opacities, mode, aspect='all', style.name = 'default') standardGeneric ('setNodeOpacityRule'))
setGeneric ('setNodeTooltipRule',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.attribute.name, style.name = 'default') standardGeneric ('setNodeTooltipRule'))

setGeneric ('setEdgeLineStyleRule',        signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, attribute.values, line.styles, default.style='SOLID', style.name = 'default') standardGeneric ('setEdgeLineStyleRule'))
setGeneric ('setEdgeLineWidthRule',        signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, attribute.values, line.widths, mode="interpolate", default.width='1', style.name = 'default') standardGeneric ('setEdgeLineWidthRule'))
setGeneric ('setEdgeTargetArrowRule',      signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default') standardGeneric ('setEdgeTargetArrowRule'))
setGeneric ('setEdgeSourceArrowRule',      signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default') standardGeneric ('setEdgeSourceArrowRule'))
setGeneric ('setEdgeTargetArrowColorRule', signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000', style.name = 'default') standardGeneric ('setEdgeTargetArrowColorRule'))
setGeneric ('setEdgeSourceArrowColorRule', signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000', style.name = 'default') standardGeneric ('setEdgeSourceArrowColorRule'))
setGeneric ('setEdgeColorRule',            signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, control.points, colors, mode="interpolate", default.color='#FFFFFF', style.name = 'default') standardGeneric ('setEdgeColorRule'))
setGeneric ('setEdgeOpacityRule',          signature='obj', function (obj=CytoscapeConnection(), edge.attribute.name, control.points, opacities, mode, style.name = 'default') standardGeneric ('setEdgeOpacityRule'))
setGeneric ('setEdgeTooltipRule',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.attribute.name, style.name = 'default') standardGeneric ('setEdgeTooltipRule'))

#--- bypasses ---------------------------------------------
setGeneric ('setNodeSizeDirect',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.sizes) standardGeneric ('setNodeSizeDirect'))
setGeneric ('setNodeLabelDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.labels) standardGeneric ('setNodeLabelDirect'))
setGeneric ('setNodeFontSizeDirect',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.sizes) standardGeneric ('setNodeFontSizeDirect'))
setGeneric ('setNodeLabelColorDirect',    signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.colors) standardGeneric ('setNodeLabelColorDirect'))
setGeneric ('setNodeWidthDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.widths) standardGeneric ('setNodeWidthDirect'))
setGeneric ('setNodeHeightDirect',        signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.heights) standardGeneric ('setNodeHeightDirect'))
setGeneric ('setNodeShapeDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.shapes) standardGeneric ('setNodeShapeDirect'))
setGeneric ('setNodeImageDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, image.positions) standardGeneric ('setNodeImageDirect'))
setGeneric ('setNodeColorDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.colors) standardGeneric ('setNodeColorDirect'))
setGeneric ('setNodeBorderWidthDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.sizes) standardGeneric ('setNodeBorderWidthDirect'))
setGeneric ('setNodeBorderColorDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.colors) standardGeneric ('setNodeBorderColorDirect'))
setGeneric ('setNodeOpacityDirect',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeOpacityDirect'))
setGeneric ('setNodeFillOpacityDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeFillOpacityDirect'))
setGeneric ('setNodeLabelOpacityDirect',  signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeLabelOpacityDirect'))
setGeneric ('setNodeBorderOpacityDirect', signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names, new.values) standardGeneric ('setNodeBorderOpacityDirect'))
setGeneric ('setEdgeOpacityDirect',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeOpacityDirect'))

setGeneric ('setEdgeColorDirect',              signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeColorDirect'))
setGeneric ('setEdgeLabelDirect',              signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelDirect'))
setGeneric ('setEdgeFontFaceDirect',           signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeFontFaceDirect'))
setGeneric ('setEdgeFontSizeDirect',           signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeFontSizeDirect'))
setGeneric ('setEdgeLabelColorDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelColorDirect'))
setGeneric ('setEdgeTooltipDirect',            signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeTooltipDirect'))
setGeneric ('setEdgeLineWidthDirect',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLineWidthDirect'))
setGeneric ('setEdgeLineStyleDirect',          signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeLineStyleDirect'))
setGeneric ('setEdgeSourceArrowShapeDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeSourceArrowShapeDirect'))
setGeneric ('setEdgeTargetArrowShapeDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeTargetArrowShapeDirect'))
setGeneric ('setEdgeSourceArrowColorDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.colors) standardGeneric ('setEdgeSourceArrowColorDirect'))
setGeneric ('setEdgeTargetArrowColorDirect',   signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.colors) standardGeneric ('setEdgeTargetArrowColorDirect'))
setGeneric ('setEdgeLabelOpacityDirect',       signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelOpacityDirect'))
setGeneric ('setEdgeSourceArrowOpacityDirect', signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeSourceArrowOpacityDirect'))
setGeneric ('setEdgeTargetArrowOpacityDirect', signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.values) standardGeneric ('setEdgeTargetArrowOpacityDirect'))
#setGeneric ('setEdgeLabelPositionDirect',      signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelPositionDirect'))
#setGeneric ('setEdgeLabelWidthDirect',         signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names, new.value) standardGeneric ('setEdgeLabelWidthDirect'))


#-----------------------------------------------------------
# methods related to transmitting data from Cytoscape to R
#-----------------------------------------------------------
setGeneric ('getNetworkSuid',                signature='obj', function (obj=CytoscapeConnection(), title=NA) standardGeneric ('getNetworkSuid'))
setGeneric ('haveNodeAttribute',             signature='obj', function (obj=CytoscapeConnection(), node.names, attribute.name) standardGeneric ('haveNodeAttribute'))
setGeneric ('haveEdgeAttribute',             signature='obj', function (obj=CytoscapeConnection(), edge.names, attribute.name) standardGeneric ('haveEdgeAttribute'))
setGeneric ('copyNodeAttributesFromCyGraph', signature='obj', function (obj=CytoscapeConnection(), suid, existing.graph) standardGeneric ('copyNodeAttributesFromCyGraph'))
setGeneric ('copyEdgeAttributesFromCyGraph', signature='obj', function (obj=CytoscapeConnection(), suid, existing.graph) standardGeneric ('copyEdgeAttributesFromCyGraph'))
setGeneric ('getGraphFromNetwork',           function (obj, title=NA) standardGeneric ('getGraphFromNetwork'))
setGeneric ('connectToNewestCyWindow',       signature='obj', function (obj=CytoscapeConnection(), copyToR = FALSE) standardGeneric('connectToNewestCyWindow'))

#-----------------------------------------------------------
# methods related to transmitting data from obj@graph to 
# Cytoscape thus requiring a full CytoscapeWindowClass obj
#-----------------------------------------------------------
setGeneric ('sendNetworkFromGraph',               function (obj) standardGeneric('sendNetworkFromGraph'))
setGeneric ('displayGraph',               signature='obj', function (obj) standardGeneric ('displayGraph'))
setGeneric ('predictTimeToDisplayGraph',  signature='obj', function (obj) standardGeneric ('predictTimeToDisplayGraph'))
setGeneric ('addGraphToGraph',            signature='obj', function (obj, other.graph) standardGeneric ('addGraphToGraph'))
setGeneric ('setGraph', 	              signature='obj', function (obj, graph) standardGeneric ('setGraph'))
setGeneric ('getGraph', 	              signature='obj', function (obj) standardGeneric ('getGraph'))
setGeneric ('sendNodeAttributesFromGraph',signature='obj', function (obj, attribute.name) standardGeneric ('sendNodeAttributesFromGraph'))
setGeneric ('sendEdgeAttributesFromGraph',signature='obj', function (obj, attribute.name) standardGeneric ('sendEdgeAttributesFromGraph'))
setGeneric ('sendNodesFromGraph',	      signature='obj', function (obj) standardGeneric ('sendNodesFromGraph'))
setGeneric ('sendEdgesFromGraph',	      signature='obj', function (obj) standardGeneric ('sendEdgesFromGraph'))
setGeneric ('sendEdgesFromGraph',	      signature='obj', function (obj) standardGeneric ('sendEdgesFromGraph'))

#-----------------------------------------------------------
# private methods, for internal use only; dependent on 
# a full CytoscapeWindowClass obj
#-----------------------------------------------------------
setGeneric ('.addNodes',              signature='obj', function (obj, other.graph) standardGeneric ('.addNodes'))
setGeneric ('.addEdges',              signature='obj', function (obj, other.graph) standardGeneric ('.addEdges'))
setGeneric ('.getNetworkViews',       signature='obj', function (obj=CytoscapeWindowFromNetwork()) standardGeneric ('.getNetworkViews'))
setGeneric ('.nodeNameToNodeSUID',    signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.names) standardGeneric ('.nodeNameToNodeSUID'))
setGeneric ('.edgeNameToEdgeSUID',    signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.names) standardGeneric ('.edgeNameToEdgeSUID'))
setGeneric ('.nodeSUIDToNodeName',    signature='obj', function (obj=CytoscapeWindowFromNetwork(), node.suids) standardGeneric ('.nodeSUIDToNodeName'))
setGeneric ('.edgeSUIDToEdgeName',    signature='obj', function (obj=CytoscapeWindowFromNetwork(), edge.suids) standardGeneric ('.edgeSUIDToEdgeName'))
setGeneric ('cyPlot',                                  function (node.df, edge.df) standardGeneric('cyPlot'))

# ------------------------------------------------------------------------------
CytoscapeWindowFromNetwork = 
    function(obj, title=NA, return.graph=FALSE) 
        {

        # establish a connection to Cytoscape
        cy.conn <- new ('CytoscapeConnectionClass',uri=obj@uri, api=obj@api)
        if (is.null(cy.conn)) {
            write(sprintf("ERROR in CytoscapeWindowFromNetwork():\n\t Cytoscape connection could not be established >> NULL returned"), stderr())
            return()
        }
        
        # if title=NA, will return current network in Cytoscape
		existing.suid = as.character(getNetworkSuid(cy.conn,title))
		
		# inform user if the window does not exist
        if (is.na(existing.suid)) {
            write(sprintf("ERROR in RCy3::CytoscapeWindowFromNetwork():\n\t no network named '%s' exists in Cytoscape >> choose from the following titles: ", title), stderr())
			write(as.character(getNetworkList(cy.conn)), stderr())
            return(NA)
        }
		
		# if title=NA, fill in value from current network retrieved above
		if(is.na(title)){
		    title = getNetworkName(existing.suid)
		    #write(sprintf("Connecting to current network named '%s'", title), stderr())
		}
		
		# create minimal Cytoscape Window
        cy.window <- new('CytoscapeWindowClass', title=title, suid=existing.suid, uri=obj@uri, api=obj@api)

        # optionally, get graph from Cytoscape
        if (return.graph) {
            # copy over graph
            g.cy <- getGraphFromNetwork(cy.window, title)
            cy.window <- setGraph(cy.window, g.cy)

            # copy over obj@node.suid.name.dict
            resource.uri <- paste(cy.window@uri, apiVersion(cy.window), "networks", as.character(cy.window@suid), sep="/")
            request.res <- GET(url=resource.uri)
            request.res <- fromJSON(rawToChar(request.res$content))

            if (length(request.res$elements$nodes) != 0){
                cy.window@node.suid.name.dict = lapply(request.res$elements$nodes, function(n) { 
                    list(name=n$data$name, SUID=n$data$SUID) })
            }
            if (length(request.res$elements$edges) != 0){
                cy.window@edge.node.suid.name.dict = lapply(request.res$elements$edges, function(e) {
                    list(name=e$data$name, SUID=e$data$SUID) })
            }
        }
        return (cy.window)
} # END CytoscapeWindowFromNetwork

# ------------------------------------------------------------------------------
check.api.version = function(cyCon=CytoscapeConnection()) 
{
	api.version.string = apiVersion(cyCon)
	string.tmp1 = strsplit(api.version.string, ' ')[[1]][1]
	string.tmp2 = gsub('[a-z]', '', string.tmp1)
	string.tmp3 = gsub('[A-Z]', '', string.tmp2)
	api.version = as.numeric(string.tmp3)
	
	# SET MINIMUM REQUIRED API VERSION FOR RCY3 HERE
	expected.version = 1
	
	if(api.version < expected.version) { 
		write(' ', stderr())
		write(sprintf('This version of the RCy3 package requires CyREST version %s or greater.', expected.version), 
					stderr ())
		write(sprintf('However, you are using version %s. You must upgrade.', api.version), stderr ())
		write('Please visit the app page at http://apps.cytoscape.org/apps/cyrest.', stderr ())
		write(' ', stderr())
		stop('Wrong CyREST version.')
	}
} # END check.api.version

# ------------------------------------------------------------------------------
getServerStatus = function(uri,api) { 
    request.uri = paste(uri, api, sep="/")
    request.res = GET(url=request.uri)
    return(request.res)
} 

#------------------------------------------------------------------------------------------------------------------------
#' Ping Cytoscape
#' 
#' @description Test the connection to Cytoscape via CyREST 
#' @param obj a \code{CytoscapeConnectionClass} object (optional)
#' @return "It works!"
#' @author Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples \donttest{
#' ping()
#' 
#' cy <- CytoscapeConnection ()
#' ping (cy)
#' }
#' @export

#' @rdname ping
setMethod('ping','missing', 
function() {
    ping(CytoscapeConnection());
});

#' @rdname ping
setMethod('ping', 'CytoscapeConnectionClass',
	function(obj) {
		conn.str <- paste(obj@uri, obj@api, sep="/")
		res <- GET(conn.str)
		apiVersion <- fromJSON(rawToChar(res$content))$apiVersion
		
		if(length(apiVersion) > 0) {
			return("You are connected to Cytoscape!")
		} else {
			write(sprintf('CyREST connection problem. RCy3 can not continue!'), stderr())
			stop()
		}
	}) # END ping

#------------------------------------------------------------------------------------------------------------------------
setMethod('apiVersion', 'OptionalCyObjClass', 
	function(obj) {
		res <- GET(obj@uri)
		# get vector with available plugin versions
		available.api.versions <- fromJSON(rawToChar(res$content))$availableApiVersion
		
		api.version <- character(0)
		
		# loop through the vector and check which is the correct plugin version
		for(i in 1:length(available.api.versions)) {
		    server.status = getServerStatus(obj@uri, available.api.versions[i])
		    
		    if(server.status$status_code == 200) {
		        api.version = fromJSON(rawToChar(server.status$content))$apiVersion
		    }
		}
		# current api.version will be the highest/latest version
		return(api.version)
	}) # END apiVersion

# ------------------------------------------------------------------------------
# Send a window from a CWnetwork from a graph (obj@graph)
setMethod('sendNetworkFromGraph', 'CytoscapeWindowClass', 
	function(obj) {
	    g = obj@graph
		g@graphData$name <- obj@title
		graph.attributes <- g@graphData
		graph.elements = list(nodes = list(), edges = list())
		
		cygraph <- toJSON(list(data = graph.attributes, elements = graph.elements))
		resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
		request.res <- POST(url = resource.uri, body = cygraph, encode = "json")
		suid <- unname(fromJSON(rawToChar(request.res$content)))
		
		return(as.character(suid))
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('createNetworkFromSelection', 'OptionalCyWinClass',
    function (obj, new.title, return.graph=FALSE, exclude.edges=FALSE) {
        if (getSelectedNodeCount (obj) == 0) {
            write (noquote ('RCy3::createNetworkFromSelection error:  no nodes are selected'), stderr ())
            return (NA)
        }

        if (new.title %in% as.character (getNetworkList (obj))) {
            msg <- sprintf ('RCy3::createNetworkFromSelection error:  window "%s" already exists', new.title)
            write (noquote (msg), stderr ())
            return (NA)
        }
        
        if(exclude.edges){
            exclude.edges = "true"
        } else {
            exclude.edges = "false"
        }
        
        json_sub=NULL
        json_sub$source=obj@title
        json_sub$nodeList="selected"
        json_sub$excludeEdges=exclude.edges
        
        subnetwork.arg = NULL
        if(!missing(new.title)){
            json_sub$networkName=new.title
        }
        
        sub <- toJSON(as.list(json_sub))
        url<- sprintf("%s/%s/commands/network/create", obj@uri,obj@api,sep="")
        response <- POST(url=url,body=sub, encode="json",content_type_json())
        subnetwork.suid=unname(fromJSON(rawToChar(response$content)))[[1]][[1]]
        cat(sprintf("Subnetwork SUID is : %i \n", subnetwork.suid))
        sub.cw<-CytoscapeWindowFromNetwork(obj,new.title,return.graph=return.graph)
        return(sub.cw)
}) # createNetworkFromSelection

#' Copy a Cytoscape Network 
#'
#' Makes a copy of a Cytoscape Network with all of its edges and nodes 
#'
#' @param obj Cytoscape network 
#' @param new.title New name for the copy
#' @param return.graph Logical whether to copy the graph to a new object in R 
#' 
#' @return Connection to new copy of network. 
#'
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' copy_of_your_net <- cloneNetwork(cw, "new_copy")
#' }
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{createNetworkFromSelection}}, \code{\link{CytoscapeWindowFromNetwork}}, \code{\link{renameNetwork}}
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('cloneNetwork',
          'OptionalCyWinClass', 
          function(obj,
                   new.title,
                   return.graph = FALSE) {
            if (obj@title == new.title){
              print("Copy not made. The titles of the original window and its copy are the same. Please pick a new name for the copy.")
              stderr()
            }
            else{
              selectAllNodes(obj)
              selectAllEdges(obj)
              request.uri <- paste(obj@uri,
                                   obj@api,
                                   "networks",
                                   obj@suid,
                                   sep = "/")
              
              request.res <- POST(url = request.uri,
                                  query = list(title = new.title))
              
              invisible(request.res)
              
              if (return.graph){
                connect_window <- CytoscapeWindowFromNetwork(obj,new.title,
                                                           return.graph = TRUE)
                print(paste("Cytoscape window",
                            obj@title,
                            "successfully copied to",
                            connect_window@title,
                            "and the graph was copied to R."))
              } 
              else {
                connect_window <- CytoscapeWindowFromNetwork(obj,new.title,
                                                           return.graph = FALSE) 
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
#' @param new.title New name for the copy
#' @param return.graph Logical whether to copy the graph to a new object in R 
#' 
#' @return Connection to the renamed network. 
#'
#' @author Julia Gustavsen, \email{j.gustavsen@@gmail.com}
#' @seealso \code{\link{createNetworkFromSelection}}, \code{\link{CytoscapeWindowFromNetwork}}, \code{\link{cloneNetwork}}
#'
#' @examples \dontrun{
#' cw <- CytoscapeWindow('new.demo', new('graphNEL'))
#' renamed_net <- renameNetwork(cw, "renamed_network")
#' }
#' 
#' @concept RCy3
#' @export
#' 
#' @importFrom methods setGeneric
setMethod('renameNetwork',
          'OptionalCyWinClass', 
          function(obj,
                   new.title,
                   return.graph = FALSE) {
              key_value_pairs <- c()
              current <- list( id = paste(obj@suid),name = new.title)
              key_value_pairs <- c(key_value_pairs,list(current))
              selection <- list( key = "SUID", dataKey="id", data = key_value_pairs)
              selection <- toJSON(selection)
              update.name.url <- paste(obj@uri,obj@api,"networks",obj@suid,"tables/defaultnetwork",sep="/")
              
              invisible(PUT(url=update.name.url,
                            body=selection, encode="json"))    
   
              # if object was provided, then update its title as well
              if(!missing(obj)){
                 loc.obj <- obj
                 loc.obj@title<-new.title
                 eval.parent(substitute(obj <- loc.obj))
              }
})

# ------------------------------------------------------------------------------
setMethod('getNetworkCount', 'OptionalCyObjClass',
	function(obj) {
		resource.uri <- paste(obj@uri, obj@api, "networks/count", sep="/")
		res <- GET(url=resource.uri)
		num.cytoscape.windows <- unname(fromJSON(rawToChar(res$content)))
		return(as.integer(num.cytoscape.windows))
}) # END getNetworkCount

# ------------------------------------------------------------------------------
setMethod('getNetworkSuid', 'OptionalCyObjClass', 
	function(obj, title=NA) {
	    
	    if(is.na(title)){
	        if(class(obj) == 'CytoscapeWindowClass'){
	            title = obj@title
	        } else { # a CyConn was provided, but no title, so just get current network 
	           cmd<-paste0('network get attribute network=current namespace="default" columnList="SUID"')
        	    res <- commandRun(cmd,obj)
        	    network.suid <- gsub("\\{SUID:|\\}","",res)
         	    return(network.suid)
	        }
	    }
	    
		# get all window suids and associates names
		resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
		request.res <- GET(resource.uri)
		# SUIDs list of the existing Cytoscape networks	
		cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
		# names list of the existing Cytoscape networks
		cy.networks.names = c()
		
		for(net.SUID in cy.networks.SUIDs)	{
			 res.uri <- paste(obj@uri, obj@api, "networks", as.character(net.SUID), sep="/")
			 result <- GET(res.uri)
			 net.name <- fromJSON(rawToChar(result$content))$data$name
			 cy.networks.names <- c(cy.networks.names, net.name)
		}

		if(!title %in% as.character(cy.networks.names)) {
			write(sprintf("Cytoscape window named '%s' does not exist yet", title), stderr())
			return (NA)
		} # if unrecognized title
		
		window.entry = which(as.character(cy.networks.names) == title)
		suid = as.character(cy.networks.SUIDs[window.entry])
		
		return(suid)
})

# ------------------------------------------------------------------------------
setMethod('getNetworkList', 'OptionalCyObjClass', 
	function(obj) {
		if(getNetworkCount(obj) == 0) {
			return(c())
		}
		resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
		request.res <- GET(resource.uri)
		# SUIDs list of the existing Cytoscape networks	
		cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
		# names list of the existing Cytoscape networks
		cy.networks.names = c()
		
		for(net.SUID in cy.networks.SUIDs)	{
			res.uri <- paste(obj@uri, obj@api, "networks", as.character(net.SUID), sep="/")
			result <- GET(res.uri)
			net.name <- fromJSON(rawToChar(result$content))$data$name
			cy.networks.names <- c(cy.networks.names, net.name)
		}
		
		return(cy.networks.names)
})

# ------------------------------------------------------------------------------
setMethod('deleteNetwork', 'OptionalCyObjClass',
	function (obj, title=NA) {
		if(!is.na(title))
			suid = getNetworkSuid(obj, title)
		else if(class(obj) == 'CytoscapeWindowClass')
			suid = as.character(obj@suid)
		else { # a CyConn was provided, but no title, so just get current network 
		    suid = getNetworkSuid(obj)
		}
		resource.uri = paste(obj@uri, obj@api, "networks", suid, sep="/")
		request.res = DELETE(url=resource.uri)
		invisible(request.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteAllNetworks',	'OptionalCyObjClass', function (obj) {
    # deletes all networks and associated windows in Cytoscape
    resource.uri <- paste(obj@uri, obj@api, "networks", sep="/")
    request.res <- DELETE(resource.uri)
    invisible(request.res)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'OptionalCyObjClass', function (obj) {
    resource.uri <- paste(obj@uri, obj@api, "styles/visualproperties/NODE_SHAPE/values", sep="/")
    request.res <- GET(resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    return(request.res$values)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDirectlyModifiableVisualProperties', 'OptionalCyObjClass',
    function (obj, style.name="default") {
        resource.uri = paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", sep="/")
        request.res = GET(url=resource.uri)
        visual.properties <- unname(fromJSON(rawToChar(request.res$content))[[1]])
        visual.properties <- sapply(visual.properties, '[[', 1)
        return(visual.properties)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAttributeClassNames', 'OptionalCyObjClass',
# retrieve the names of the recognized and supported names for the class of any node or edge attribute.
	function (obj) {
		 return (c ('floating|numeric|double', 'integer|int', 'string|char|character'))
		 })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLineStyles', 'OptionalCyObjClass', function (obj) {
    resource.uri <- paste(obj@uri, obj@api, "styles/visualproperties/EDGE_LINE_TYPE/values", sep="/")
	request.res <- GET(resource.uri)
    request.res <- fromJSON(rawToChar(request.res$content))
    return(request.res$values)
    })

# ------------------------------------------------------------------------------
setMethod('getArrowShapes', 'OptionalCyObjClass', 
    function(obj) {
        
        
        resource.uri <- paste(obj@uri, obj@api, "styles/visualproperties/EDGE_TARGET_ARROW_SHAPE/values", sep="/")
        # TanjaM: EDGE_SOURCE_ARROW_SHAPE rather than TARGET returns the same results as of April 2015
        request.res <- GET(resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        return(request.res$values)
})
## END getArrowShapes

# ------------------------------------------------------------------------------
setMethod('getLayoutNames', 'OptionalCyObjClass', 
	function(obj) {
        request.uri <- paste(obj@uri, obj@api, "apply/layouts", sep="/")
        request.res <- GET(url=request.uri)
        
        available.layouts <- unname(fromJSON(rawToChar(request.res$content)))
        return(available.layouts)
}) 
## END getLayoutNames

# ------------------------------------------------------------------------------
setMethod('getLayoutNameMapping', 'OptionalCyObjClass', 
    function(obj) {
        layout.names <- getLayoutNames(obj)
        layout.full.names <- c()
        
        # get the English/full name of a layout
        for (layout.name in layout.names){
            request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), sep="/")
            request.res <- GET(url=request.uri)
            
            layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
            layout.full.names <- c(layout.full.names, layout.property.names[[4]])
        }
        names(layout.names) <- layout.full.names
        
        return(layout.names)
})
## END getLayoutNameMapping

# ------------------------------------------------------------------------------
setMethod('getLayoutPropertyNames', 'OptionalCyObjClass', 
    function(obj, layout.name) {
        request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
        request.res <- GET(url=request.uri)
        
        layout.property.names <- unname(fromJSON(rawToChar(request.res$content)))
        return(sapply(layout.property.names, '[[', 1))
})
## END getLayoutPropertyNames

# ------------------------------------------------------------------------------
setMethod('getLayoutPropertyType', 'OptionalCyObjClass', 
    function(obj, layout.name, property.name) {
        request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
        request.res <- GET(url=request.uri)
        
        layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
        layout.property.names <- sapply(layout.property.list, '[[', 1)
        position <- layout.property.names == property.name
        return(sapply(layout.property.list, '[[', 3)[position])
}) 
## END getLayoutPropertyType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyValue', 'OptionalCyObjClass', 

   function (obj, layout.name, property.name) {
       request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
       request.res <- GET(url=request.uri)
       
       layout.property.list <- unname(fromJSON(rawToChar(request.res$content)))
       layout.property.names <- sapply(layout.property.list, '[[', 1)
       position <- layout.property.names == property.name
       return(sapply(layout.property.list, '[[', 4)[position])
     }) # getLayoutPropertyValue

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setLayoutProperties', 'OptionalCyObjClass', 

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
                
                request.uri <- paste(obj@uri, obj@api, "apply/layouts", as.character(layout.name), "parameters/", sep="/")
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
setMethod ('setGraph', 'CytoscapeWindowClass', 
           function(obj, graph) {
    # copy the graph over
    loc.obj <- obj
    if (edgemode(graph) == 'undirected'){
        graph = remove.redundancies.in.undirected.graph (graph) #AP: not sure this is needed anymore...
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

setMethod ('haveNodeAttribute', 'OptionalCyObjClass',
    function(obj, node.names, attribute.name) {
    
        net.SUID = as.character(obj@suid)
        # check the attribute exists
        if (attribute.name %in% getNodeAttributeNames(obj)) {
        # get the node SUIDs
            node.SUIDs = .nodeNameToNodeSUID(obj, node.names)
            nodes.that.have.attribute = c()
            
            for (i in 1:length(node.SUIDs)) {
                resource.uri = paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/rows", as.character(node.SUIDs[i]), attribute.name, sep="/")
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

setMethod ('haveEdgeAttribute', 'OptionalCyObjClass',

    function (obj, edge.names, attribute.name) {
        net.SUID = as.character(obj@suid)
        
        if(attribute.name %in% getEdgeAttributeNames(obj)) {
            edge.SUIDs = .edgeNameToEdgeSUID(obj, edge.names)
            edges.that.have.attribute = c()
            
            for(i in 1:length(edge.SUIDs)) {
                resource.uri = paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/rows", as.character(edge.SUIDs[i]), attribute.name, sep="/")
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
setMethod ('copyNodeAttributesFromCyGraph', 'OptionalCyObjClass',

    function (obj, suid, existing.graph) {
        
        node.attribute.names = getNodeAttributeNames(obj)
        
        for(attribute.name in node.attribute.names) {
            known.node.names = sapply(obj@node.suid.name.dict, function(n) { n$name })
            # nodes that store values for this attribute (meaning the value is not empty)
            nodes.with.attribute = haveNodeAttribute(obj, known.node.names, attribute.name)
            if(length(nodes.with.attribute) > 0) {
                attribute.type = getNodeAttributeType(obj, attribute.name)
                write(sprintf("\t retrieving attribute '%s' values for %d nodes", attribute.name, length(nodes.with.attribute)), stderr())
                # write(sprintf("\t retrieving %s '%s' attribute for %d nodes", attribute.type, attribute.name, length(nodes.with.attribute)), stderr())
                if(attribute.type == 'Integer' || attribute.type == 'Long') {
                    attribute.type = 'integer'
                    default.value = as.integer(0)
                } else if(attribute.type == 'String') {
                    attribute.type = 'char'
                    default.value = as.character('unassigned')
                } else if(attribute.type == 'Double' || attribute.type == 'Float') {
                    attribute.type = 'numeric'
                    default.value = as.numeric(0.0)
                } else if(attribute.type == 'Boolean') {
                    attribute.type = 'boolean'
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
setMethod ('copyEdgeAttributesFromCyGraph', 'OptionalCyObjClass',

    function (obj, suid, existing.graph) {
        edge.attribute.names = getEdgeAttributeNames(obj)
        
        cy2.edgenames = as.character(cy2.edge.names(existing.graph)) # < 2 seconds for > 9000 edges
        
        for(attribute.name in edge.attribute.names) {
            edges.with.attribute = haveEdgeAttribute(obj, cy2.edgenames, attribute.name)
            
            if(length(edges.with.attribute) > 0) {
                attribute.type = getEdgeAttributeType(obj, attribute.name) 
                
                write(sprintf("\t retrieving attribute '%s' values for %d edges", attribute.name, length(edges.with.attribute)), stderr())
                if(attribute.type == 'Integer' || attribute.type == 'Long') {
                    attribute.type = 'integer'
                    default.value = as.integer(0)
                } else if(attribute.type == 'String') {
                    attribute.type = 'char'
                    default.value = as.character('unassigned')
                } else if(attribute.type == 'Double' || attribute.type == 'Float') {
                    attribute.type = 'numeric'
                    default.value = as.numeric(0.0)
                } else if(attribute.type == 'Boolean') {
                    attribute.type = 'boolean'
                    default.value = as.logical(FALSE)
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
#' getGraphFromNetwork
#' 
#' @description Returns the Cytoscape network as a Bioconductor graph.
#' @return A Bioconductor graph object.
#' @author Tanja Muetze, Georgi Kolishovski, Paul Shannon
#' @examples 
#' \dontrun{
#' g.net <- getGraphFromNetwork() #current network
#' 
#' g.net1 <- getGraphFromNetwork('network1')
#' 
#' cc <- CytoscapeConnection()
#' g.net2 <- getGraphFromNetwork(cc, 'network2')
#' 
#' cw <- CytoscapeWindow('network3', graph=makeSimpleGraph())
#' displayGraph(cw)
#' layoutNetwork(cw)
#' g.net3 <- getGraphFromNetwork(cw)
#'
#' }
#' @export

#' @rdname getGraphFromNetwork
setMethod ('getGraphFromNetwork', 'missing',
           function (title = NA) {
               if (is.na(title))
                   title=getNetworkName() #current network
               cc<-CytoscapeConnection() #default connection
               obj = CytoscapeWindowFromNetwork(cc,title=title)
               getGraphFromNetwork(obj)
           });
#' @rdname getGraphFromNetwork
setMethod ('getGraphFromNetwork', 'CytoscapeConnectionClass',
        function (obj,title= NA) {
            if (is.na(title))
                title=getNetworkName() #current network
            loc.obj = CytoscapeWindowFromNetwork(obj,title=title)
            getGraphFromNetwork(loc.obj)
           });
#' @rdname getGraphFromNetwork
setMethod ('getGraphFromNetwork', 'CytoscapeWindowClass',
    function (obj) {
        loc.obj = obj
        # network id 
        net.SUID = as.character(loc.obj@suid)
        title = as.character(loc.obj@title)
        
        if (!is.na(net.SUID)) {
            # get the graph from Cytoscape
            resource.uri = paste(loc.obj@uri, obj@api, "networks", net.SUID, sep="/")
            request.res = GET(url=resource.uri)
            request.res = fromJSON(rawToChar(request.res$content))
            
            g = new("graphNEL", edgemode='directed') # create graph object
            
            # GET GRAPH NODES
            g.nodes = request.res$elements$nodes
            # if there are no nodes in the graph received from Cytoscape, return an empty 'graphNEL' object
            if(length(g.nodes) == 0) {
                write(sprintf("NOTICE in RCy3::getGraphFromNetwork():\n\t returning an empty 'graphNEL'"), stderr())
                return(g)
            }
            
            # else get the node names and add them to the R graph
            loc.obj@node.suid.name.dict = lapply(g.nodes, function(n) { 
            list(name=n$data$name, SUID=n$data$SUID) })
            g.node.names = sapply(loc.obj@node.suid.name.dict, function(n) { n$name })
            write(sprintf("\t received %d NODES from '%s'", length(g.nodes), title), stderr())
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
                write(sprintf("\n\t received %d EDGES from '%s'", length(g.edges), title), stderr())
                
                loc.obj@edge.node.suid.name.dict = lapply(g.edges, function(e) {
                    list(name=e$data$name, SUID=e$data$SUID) })
                g.edge.names = sapply(loc.obj@edge.node.suid.name.dict, function(e) { e$name })
                edges.tokens = strsplit(g.edge.names, regex)
                source.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[1]))
                target.nodes = unlist(lapply(edges.tokens, function(tokens) tokens[3]))
                edge.types = unlist(lapply(edges.tokens, function(tokens) tokens[2]))
                write(sprintf('\t - adding %d edges to the returned graph\n', length(edges.tokens)), stderr())
               
                tryCatch({
                    g = addEdge(source.nodes, target.nodes, g)
                    edgeData(g, source.nodes, target.nodes, 'edgeType') = edge.types
                    
                    # GET EDGE ATTRIBUTES (if any)
                    g = copyEdgeAttributesFromCyGraph(loc.obj, suid, g)
                },
                error = function(cond){
                    write(sprintf("ERROR in RCy3::getGraphFromNetwork(): '%s'", cond), stderr())
                    return(NA)
                })
                

            }
          
        } else {
            write(sprintf("ERROR in RCy3::getGraphFromNetwork():\n\t there is no graph with name '%s' in Cytoscape", title), stderr())
            return(NA)
        }
        
        return(g)
  });
## END getGraphFromNetwork

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
          'OptionalCyObjClass',
          function(obj,
                                    copyToR = FALSE) {
  resource.uri <- paste(obj@uri,
                        obj@api,
                        "networks",
                        sep = "/")
  request.res <- GET(resource.uri)
  # SUIDs list of the existing Cytoscape networks
  cy.networks.SUIDs <- fromJSON(rawToChar(request.res$content))
  # most recently made enrichment map will have the highest SUID
  cy.networks.SUIDs.last <- max(cy.networks.SUIDs)
  
  res.uri.last <- paste(obj@uri,
                        obj@api,
                        "networks",
                        as.character(cy.networks.SUIDs.last),
                        sep = "/")
  result <- GET(res.uri.last)
  net.name <- fromJSON(rawToChar(result$content))$data$name
  
  ## to get edges request.res$elements$edges
  newest_CyWindow <- CytoscapeWindowFromNetwork(obj,net.name,
                                              return.graph = copyToR) 
  return(newest_CyWindow)
})


# ------------------------------------------------------------------------------
setMethod('sendNodesFromGraph', 'CytoscapeWindowClass', function(obj) {
    loc.obj <- obj
    # returns the nodes currently stored in the graph object
    graph.network.nodes = nodes(loc.obj@graph)
    # returns the nodes currently displayed in Cytoscape
    current.cytoscape.nodes = sapply(loc.obj@node.suid.name.dict, function(n) n$name)
    
    node.node.suid.name.dict <- (0)
    
    diff.nodes = setdiff(graph.network.nodes, current.cytoscape.nodes)
    # if new nodes need to be added
    if(length(diff.nodes) > 0) {
        net.SUID = as.character(loc.obj@suid)
        
        resource.uri = paste(loc.obj@uri, obj@api, "networks", net.SUID, "nodes", sep="/")
        diff.nodes.JSON = toJSON(diff.nodes)
        
        write(sprintf('sending %d node(s)', length(diff.nodes)), stderr())
        
        request.res = POST(url=resource.uri, body=diff.nodes.JSON, encode="json")
        new.node.SUIDs = unname(fromJSON(rawToChar(request.res$content)))
        
        for(i in 1:length(new.node.SUIDs)) {
            loc.obj@node.suid.name.dict[[length(loc.obj@node.suid.name.dict)+1]] = new.node.SUIDs[[i]]
        }
    } else {
        write('CytoscapeWindow.sendNodesFromGraph(), no new nodes to send, returning', stderr())
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
    }) # END .addNodes
#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addEdges', signature (obj='CytoscapeWindowClass'),

    function (obj, other.graph) {
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
    }) # END .addEdges

# ------------------------------------------------------------------------------
# helper function: returns the SUIDs of all views belonging to specific network
setMethod('.getNetworkViews', 'OptionalCyObjClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", sep="/")
        request.res <- GET(url=resource.uri)
        network.view.SUIDs <- unname(fromJSON(rawToChar(request.res$content)))
        return(network.view.SUIDs)
}) 
## END .getNetworkViews

# ------------------------------------------------------------------------------
setMethod('.nodeNameToNodeSUID', 'OptionalCyWinClass', 
    function(obj, node.names) {
        
        if(length(obj@node.suid.name.dict)==0){  # i.e., no obj dictionary
                dict <- getTableColumns('node',c('SUID','name'),obj=obj)
                node.SUIDs <- dict[which(dict$name  %in% node.names),'SUID']
                return(node.SUIDs)
        }
        
        # initial source used 'which', but it did not return SUIDs in the input names order  
        # dict.indices = which(node.names %in% sapply(obj@node.suid.name.dict, function(n) { n$name}))
        # 'match' achieves this desired behavior
        dict.node.names <- sapply(obj@node.suid.name.dict, function(n) {n$name})
        dict.indices <- match(node.names, dict.node.names)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector 
        node.SUIDs <- sapply(obj@node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(i) {i$SUID})
        return(node.SUIDs)
}) 
## END .nodeNamesToNodeSUID

# ------------------------------------------------------------------------------
setMethod('.nodeSUIDToNodeName', 'OptionalCyWinClass', 
    function(obj, node.suids) {
        
        if(length(obj@node.suid.name.dict)==0){  # i.e., no obj dictionary
            dict <- getTableColumns('node',c('SUID','name'),obj=obj)
            node.names <- dict[which(dict$SUID  %in% node.suids),'name']
            return(node.names)
        }
        
        dict.node.SUIDs <- sapply(obj@node.suid.name.dict, function(s) {s$SUID})
        dict.indices <- match(node.suids, dict.node.SUIDs)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
        node.names <- sapply(obj@node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(n) {n$name})
        return(node.names)
}) 
## END .nodeSUIDToNodeName

# ------------------------------------------------------------------------------
setMethod('.edgeNameToEdgeSUID', 'OptionalCyWinClass', 
    function(obj, edge.names) {
        
        if(length(obj@edge.node.suid.name.dict)==0){  # i.e., no obj dictionary
            dict <- getTableColumns('edge',c('SUID','name'),obj=obj)
            edge.SUIDs <- dict[which(dict$name  %in% edge.names),'SUID']
            return(edge.SUIDs)
        }
        
        dict.edge.names <- sapply(obj@edge.node.suid.name.dict, function(e) {e$name})
        dict.indices <- match(edge.names, dict.edge.names)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
        edge.SUIDs <- sapply(obj@edge.node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(i){ i$SUID })
        return(edge.SUIDs)
}) 
## END .edgeNamesToEdgeSUID

# ------------------------------------------------------------------------------
setMethod('.edgeSUIDToEdgeName', 'OptionalCyWinClass', 
    function(obj, edge.suids) {
        
        if(length(obj@edge.node.suid.name.dict)==0){  # i.e., no obj dictionary
            dict <- getTableColumns('edge',c('SUID','name'),obj=obj)
            edge.names <- dict[which(dict$SUID  %in% edge.suids),'name']
            return(edge.names)
        }
        
        dict.edge.SUIDs = sapply(obj@edge.node.suid.name.dict, function(s) {s$SUID})
        dict.indices = match(edge.suids, dict.edge.SUIDs)
        
        # [dict.indices[!is.na(dict.indices)]] is used to clean any 'NAs' from the vector
        edge.names = sapply(obj@edge.node.suid.name.dict[dict.indices[!is.na(dict.indices)]], function(e) {e$name})
        return(edge.names)
}) 
## END .edgeSUIDToEdgeName

# ------------------------------------------------------------------------------
setMethod('addCyNode', 'OptionalCyWinClass', function(obj,node.name) {

    if(node.name %in% getAllNodes(obj)) {
        write(sprintf('RCy3::addCyNode, node "%s" already present in Cytoscape graph', node.name), stderr())
        return()
    }
    
    # get the network suid
    net.suid <- as.character(obj@suid)
    resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "nodes", sep="/")
    nodename.json = toJSON(c(node.name))
    
    # add the node to the Cytoscape graph
    new.cynode.res <- POST(url=resource.uri, body=nodename.json, encode="json")
    new.cynode.suid.name <- unname(fromJSON(rawToChar(new.cynode.res$content)))
    
    
    # if obj was provided, then modify
    if(!missing(obj)){
        loc.obj <- obj
        # add the node to the R graph object
        loc.obj@graph <- addNode(node.name, loc.obj@graph)
        # add the new node to the cw@node.suid.name.dict
        loc.obj@node.suid.name.dict[[length(loc.obj@node.suid.name.dict)+1]] <- 
            list(name=new.cynode.suid.name[[1]]$name, SUID=new.cynode.suid.name[[1]]$SUID)
        eval.parent(substitute(obj <- loc.obj))
    }
}) # addCyNode

# ------------------------------------------------------------------------------
setMethod('addCyEdge', 'OptionalCyWinClass', 
  function (obj, source.node.name, target.node.name, edgeType, directed) {
    
    good.args = TRUE
    # confirm that the user has provided exactly one source and one target nodes
    if(length(source.node.name) > 1 || length(target.node.name) > 1) {
      good.args = FALSE
      write(sprintf('RCy3::addEdge can have only one source and one target nodes'), stderr())
    }
    
    if(!source.node.name %in% getAllNodes(obj)) {
      good.args = FALSE
      write(sprintf('RCy3::addEdge. Error: source node %s does not exist in the Cytoscape graph. Edge cannot be created.', source.node.name), stderr())
    }
    if(!target.node.name %in% getAllNodes(obj)) {
      good.args = FALSE
      write(sprintf('RCy3::addEdge. Error: source node %s does not exist in the Cytoscape graph. Edge cannot be created.', target.node.name), stderr())
    }
    if(!good.args) {
      return()
    }
    
    net.suid <- as.character(obj@suid)
    resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "edges", sep="/")
    
    node.names.vec <- sapply(obj@node.suid.name.dict, "[[", 1)
    edge.data <- list(source = obj@node.suid.name.dict[[which(node.names.vec %in% source.node.name)]]$SUID, 
                      target = obj@node.suid.name.dict[[which(node.names.vec %in% target.node.name)]]$SUID, 
                      directed = directed, interaction = edgeType)
    
    edge.data.JSON <- toJSON(list(edge.data))
    
    new.cyedge.res <- POST(url=resource.uri, body=edge.data.JSON, encode='json')
    invisible(new.cyedge.res)
    
    # if obj was provided, then modify
    if(!missing(obj)){
        loc.obj <- obj
        # add the edge to the R graph object
        loc.obj@graph <- addEdge(source.node.name, target.node.name, loc.obj@graph)
        eval.parent(substitute(obj <- loc.obj))
    }
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
setMethod('sendEdgesFromGraph', 'CytoscapeWindowClass',
  function(obj) {
      loc.obj <- obj
      net.SUID = as.character(loc.obj@suid)
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
      # get the list of currently existing edges (from dict)
      existing.edge.names = 
          sapply(loc.obj@edge.node.suid.name.dict, function(n) {return(n$name)})
      
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
              loc.obj@edge.node.suid.name.dict[[length(loc.obj@edge.node.suid.name.dict)+1]] = 
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
setMethod('layoutNetwork', 'OptionalCyWinClass', 
    function(obj, layout.name = 'grid') {
        if(!layout.name %in% getLayoutNames(obj)) {
            write(sprintf("layout.name '%s' is not recognized; call getLayoutNames(<CytoscapeWindow>) to see those which are supported", layout.name), stderr())
    }
    id = as.character(obj@suid)
    
    api.str <- paste(obj@uri, obj@api, "apply/layouts", layout.name, id, sep = "/")
    
    res <- GET(api.str)
    invisible(res)
  }) # layoutNetwork

#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveLayout', 'OptionalCyWinClass',

  function (obj, filename, timestamp.in.filename=FALSE) {

     custom.layout <- RCy3::getNodePosition (obj,  getAllNodes (obj))
     if (timestamp.in.filename) {
        dateString <- format (Sys.time (), "%a.%b.%d.%Y-%H.%M.%S")
        stem <- strsplit (filename, '\\.RData')[[1]]
        filename <- sprintf ('%s.%s.RData', stem, dateString)
        write (sprintf ('saving layout to %s\n', filename), stderr ())
     }
     save (custom.layout, file=filename)
    }) # save.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('restoreLayout', 'OptionalCyWinClass',

  function (obj, filename) {
     custom.layout <- local({x=load(filename); get(x)})
     node.names <- names (custom.layout)
     node.names.filtered <- intersect (node.names, getAllNodes (obj))
     x <- as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$x)))
     y <- as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$y)))
     setNodePosition (obj, node.names.filtered, x, y)
    }) # restoreLayout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodePosition', 'OptionalCyWinClass',

    function (obj, node.names, x.coords, y.coords) {
        unknown.nodes <- setdiff (node.names, getAllNodes (obj))
        recognized.nodes <- intersect(node.names, getAllNodes(obj))
        
        # ensure that nodes were provided
        if (length (node.names) == 0){
            return ()
        }

        # throw error if nodes in node.names don't exist in the network
        if (length (unknown.nodes) > 0) {
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
setMethod ('getNodePosition', 'OptionalCyWinClass',
  function (obj, node.names) {
      net.suid = as.character(obj@suid)

      # get the views for the given network model
      resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "views", sep="/")
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
          query.node = .nodeNameToNodeSUID(obj,node.name)
          
          # get node x coordinate
          resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "views", view.SUID, "nodes", as.character(query.node) ,"NODE_X_LOCATION", sep="/")
          request.res <- GET(resource.uri)
          node.x.position <- fromJSON(rawToChar(request.res$content))
          node.x.position <- node.x.position[[2]]
          # node.x.position <- commandRun(paste0('node get properties network="',obj@title,'" nodeList="',node.name,'" propertyList="X LOCATION"'))
          # node.x.position <- gsub("}","",node.x.position)
          # node.x.position <- unlist(strsplit(node.x.position,":"))[4]
          
          # get node y coordinate
          resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "views", view.SUID, "nodes", as.character(query.node) ,"NODE_Y_LOCATION", sep="/")
          request.res <- GET(resource.uri)
          node.y.position <- fromJSON(rawToChar(request.res$content))
          node.y.position <- node.y.position[[2]]
          # node.y.position <- commandRun(paste0('node get properties network="',obj@title,'" nodeList="',node.name,'" propertyList="Y LOCATION"'))
          # node.y.position <- gsub("}","",node.y.position)
          # node.y.position <- unlist(strsplit(node.y.position,":"))[4]
          
          # add x and y coordinates to the coordinates list
          coordinates.list[[node.name]] <- list(x= as.numeric(node.x.position), y=as.numeric(node.y.position))
      }
      return(coordinates.list)
    }) # getNodePosition

# ------------------------------------------------------------------------------
setMethod ('getNodeSize', 'OptionalCyWinClass',

  function (obj, node.names) {
     # get network ID 
     net.SUID = as.character(obj@suid)
     
     # get the views for the given network model
     resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", sep="/")
     request.res <- GET(resource.uri)
     net.views.SUIDs <- fromJSON(rawToChar(request.res$content))
     view.SUID <- as.character(net.views.SUIDs[[1]])
     
     widths <- c()
     heights <- c()
     
     node.SUIDs <- .nodeNameToNodeSUID(obj,node.names)
     
     for (node.SUID in node.SUIDs){
        # request 
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "nodes", as.character(node.SUID), sep="/")
    
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

# ------------------------------------------------------------------------------
# Sets node attributes from graph (obj@graph)
setMethod('sendNodeAttributesFromGraph', 'CytoscapeWindowClass', 
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
setMethod('setNodeAttributesDirect', 'OptionalCyWinClass', 
    function(obj, attribute.name, attribute.type, node.names, values) {
        net.SUID = as.character(obj@suid)
        
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
                paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
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
                    paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns", attribute.name, sep="/")
                request.res = PUT(url=resource.uri, body=node.SUID.value.pairs.JSON, encode="json")
                invisible(request.res)
            }
        }
})
## END setNodeAttributesDirect

# ------------------------------------------------------------------------------
setMethod('sendEdgeAttributesFromGraph', 'CytoscapeWindowClass', 
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
setMethod('setEdgeAttributesDirect', 'OptionalCyWinClass', 
    function(obj, attribute.name, attribute.type, edge.names, values) {
        net.SUID = as.character(obj@suid)
        
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
                        paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
                    request.res = POST(url=resource.uri, body=tbl.col.JSON, encode="json")
                }
                
                edge.SUIDs = .edgeNameToEdgeSUID(obj, edge.names)
                edge.name.suid.value.df = data.frame(edge.names, edge.SUIDs, values)
                
                edge.SUID.value.pairs = 
                    apply(edge.name.suid.value.df[,c('edge.SUIDs','values')], 1, function(x) {list(SUID=unname(x[1]), value=unname(x[2]))})
                edge.SUID.value.pairs.JSON = toJSON(edge.SUID.value.pairs)
                resource.uri = 
                    paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", attribute.name, sep="/")
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
    
    sendNodesFromGraph(loc.obj)
    
    if(loc.obj@collectTimings) {
        current.step.exec.time = difftime(Sys.time(), stepwise.start.time, units='secs')
        write(sprintf(' *** sendNodes: %f secs', current.step.exec.time, stderr()))
        # start time (for sending node attributes to Cytoscape)
        stepwise.start.time = Sys.time()
    }
    
    # sends edges to Cytoscape
    write (sprintf ('adding %d edges...', length (edgeNames (loc.obj@graph))), stderr ())
    sendEdgesFromGraph (loc.obj)
    
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
setMethod('redraw', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        resource.uri <- paste(obj@uri, obj@api, "apply/styles", "default", net.SUID, sep = "/")
        request.res <- GET(url=resource.uri)
        invisible(request.res)
}) 
## END redraw

# ------------------------------------------------------------------------------
setMethod('setWindowSize', 'OptionalCyWinClass', 
    function(obj, width, height) {
        write(sprintf("WARNING: Method RCy3::setWindowSize() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END setWindowSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipInitialDelay', 'OptionalCyObjClass',

   function (obj, msecs) {
       write(sprintf("WARNING: Method RCy3::setTooltipInitialDelay() is not implemented in RCy3!"), stderr())
       return(FALSE)
       
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipInitialDelay', as.integer (msecs)))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipDismissDelay', 'OptionalCyObjClass',

   function (obj, msecs) {
       write(sprintf("WARNING: Method RCy3::setTooltipDismissDelay() is not implemented in RCy3!"), stderr())
       return(FALSE)
       
#     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipDismissDelay', as.integer (msecs)))
     })

# ------------------------------------------------------------------------------
setMethod('raiseWindow', 'OptionalCyObjClass', 
    function(obj, window.title = NA) {
        write(sprintf("WARNING: Method RCy3::raiseWindow() is not implemented in RCy3!"), stderr())
        return(FALSE)
        
#         if (is.na(window.title)) {
#             if(class(obj) == 'CytoscapeWindowClass') {
#                 suid = obj@suid
#             } else {
#                 write(sprintf('error in RCy3::raiseWindow(), no window title provided'), stderr())
#                 return()
#             }
#         } # no window title
#         
#         # if window title was provided
#         if(!is.na(window.title)) {
#             suid = getNetworkSuid(obj, window.title)
#             
#             if(is.na(suid)) {
#                 write(sprintf('error in RCy3::raiseWindow(), unrecognized window title: %s', window.title), stderr ())
#                 return()
#             }
#             # TO DO: call to raise the view
#         } # window title was provided
    }) # raiseWindow

#------------------------------------------------------------------------------------------------------------------------
setMethod ('showGraphicsDetails', 'OptionalCyObjClass',

    function (obj, new.value) {
        resource.uri <- paste(obj@uri, obj@api, "ui/lod/", sep="/")
        request.res <- PUT(resource.uri)
        invisible (request.res)
        if (class (obj) == 'CytoscapeWindowClass'){
            redraw (obj)
        }
        write(sprintf('RCy3::showGraphicsDetails(), Switching between show and hide full graphics details.'), stdout())
        
    })

# ------------------------------------------------------------------------------
# display the graph using all of the available window space (the Cytoscape drawing canvas)
setMethod('fitContent', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        resource.uri <- paste(obj@uri, obj@api, "apply/fit", net.SUID, sep="/")
        request.res <- GET(url=resource.uri)
        invisible(request.res)
})
## END fitContent

# ------------------------------------------------------------------------------
setMethod('fitSelectedContent', 'OptionalCyWinClass', 
    function(obj) {
        write(sprintf("WARNING: Method RCy3::fitSelectedContent() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END fitSelectedContent

# ------------------------------------------------------------------------------
setMethod('getCenter', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        # get all Cytoscape views belonging to that network
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::getCenter() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
        }
        # get the X-coordinate
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_X_LOCATION", sep="/")
        request.res <- GET(resource.uri)
        x.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
        # get the Y-coordinate
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network/NETWORK_CENTER_Y_LOCATION", sep="/")
        request.res <- GET(resource.uri)
        y.coordinate <- fromJSON(rawToChar(request.res$content))$value[[1]]
        
        return(list(x = x.coordinate, y = y.coordinate))
})
## END getCenter

# ------------------------------------------------------------------------------
# this method could be used to pan and scroll the Cytoscape canvas, which is adjusted(moved) 
# so that the specified x and y coordinates are at the center of the visible window.
setMethod('setCenter', 'OptionalCyWinClass', 
    function(obj, x, y) {
        net.SUID <- as.character(obj@suid)
        
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::setCenter() - %d views found... setting coordinates of the first one", length(net.views.SUIDs)), stderr())
        }
        
        # set the X-coordinate
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network", sep="/")
        new.x.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_X_LOCATION", value=x)))
        request.res <- PUT(resource.uri, body=new.x.coordinate.JSON, encode="json")
        # set the Y-coordinate
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network", sep="/")
        new.y.coordinate.JSON <- toJSON(list(list(visualProperty="NETWORK_CENTER_Y_LOCATION", value=y)))
        request.res <- PUT(resource.uri, body=new.y.coordinate.JSON, encode="json")
        invisible(request.res)
})
## END setCenter

# ------------------------------------------------------------------------------
setMethod('getZoom', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        # get the existing views for the given network model
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first one", length(net.views.SUIDs)), stderr())
        }
        
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network/NETWORK_SCALE_FACTOR", sep="/")
        request.res <- GET(resource.uri)
        zoom.level <- fromJSON(rawToChar(request.res$content))$value[[1]]
        
        return(zoom.level)
})
## END getZoom

# ------------------------------------------------------------------------------
setMethod('setZoom', 'OptionalCyWinClass', 
    function(obj, new.level) {
        net.SUID <- as.character(obj@suid)
        
        net.views.SUIDs <- .getNetworkViews(obj)
        view.SUID <- as.character(net.views.SUIDs[[1]])
        
        # if multiple views are found, inform the user about it
        if(length(net.views.SUIDs) > 1) {
            write(sprintf("RCy3::getZoom() - %d views found... returning coordinates of the first view", length(net.views.SUIDs)), stderr())
        }
        
        view.zoom.value <- list(visualProperty='NETWORK_SCALE_FACTOR', value=new.level)
        view.zoom.value.JSON <- toJSON(list(view.zoom.value))
        
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "network", sep="/")
        request.res <- PUT(url=resource.uri, body=view.zoom.value.JSON, encode="json")
        
        invisible(request.res)
})
## END setZoom

# ------------------------------------------------------------------------------
setMethod('getViewCoordinates', 'OptionalCyWinClass', 
    function(obj) {
        write(sprintf("WARNING: Method RCy3::getViewCoordinates() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END getViewCoordinates

# ------------------------------------------------------------------------------
setMethod('hidePanel', 'OptionalCyObjClass', 
    function(obj, panel.name) {
        
        
        if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
            panel.name <- 'SOUTH'
        }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
            panel.name <- 'WEST'
        }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
            write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
            return(NA)
        }
        
        panel.name.state = list(name=panel.name, state='HIDE')
        
        resource.uri <- paste(obj@uri, obj@api, "ui/panels", sep="/")
        request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
        
        invisible(request.res)
})
## END hidePanel

# ------------------------------------------------------------------------------
setMethod('hideAllPanels', 'OptionalCyObjClass', 
    function(obj) {
        hidePanel(obj, "SOUTH")
        hidePanel(obj, "EAST")
        hidePanel(obj, "WEST")
        hidePanel(obj, "SOUTH_WEST")
})
## END hideAllPanels

# ------------------------------------------------------------------------------
setMethod('dockPanel', 'OptionalCyObjClass', 
    function(obj, panel.name) {
        

        if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
            panel.name <- 'SOUTH'
        }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
            panel.name <- 'WEST'
        }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
            write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
            return(NA)
        }
        
        panel.name.state = list(name=panel.name, state='DOCK')
        
        resource.uri <- paste(obj@uri, obj@api, "ui/panels", sep="/")
        request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
        
        invisible(request.res)
})
## END dockPanel

# ------------------------------------------------------------------------------
setMethod('floatPanel', 'OptionalCyObjClass', 
    function(obj, panel.name) {
        
        
        if (tolower(panel.name) %in% c('data panel', 'd', 'data', 'da')){
            panel.name <- 'SOUTH'
        }else if (tolower(panel.name) %in% c('control panel', 'control', 'c', 'co')){
            panel.name <- 'WEST'
        }else if (!(panel.name %in% c('WEST', 'EAST', 'SOUTH', 'SOUTH_WEST'))){
            write (sprintf ('ERROR! Define a valid panel name.'), stderr ())
            return(NA)
        }
        
        panel.name.state = list(name=panel.name, state='FLOAT')
        
        resource.uri <- paste(obj@uri, obj@api, "ui/panels", sep="/")
        request.res <- PUT(url=resource.uri, body=toJSON(list(panel.name.state)), encoding="json")
        
        invisible(request.res)
})
## END floatPanel

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeTooltipRule', 'OptionalCyWinClass',

      function (obj, node.attribute.name, style.name = 'default') {
          id <- as.character (obj@suid)
          
          if (!node.attribute.name %in% getNodeAttributeNames(obj)) {
              write (sprintf ('Warning! RCy3::setNodeTooltipRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
              return ()
          }
          
          if(length(nodes(obj@graph))==0){ #i.e., empty obj@graph 
              attribute.values <- getTableColumns('node',node.attribute.name)[,node.attribute.name]
          } else {
             attribute.values = noa (obj@graph, node.attribute.name)
          }
          
          # set default tooltip
          default.tooltip <- list(visualProperty = "NODE_TOOLTIP", value = "")
          setVisualProperty(obj, default.tooltip, style.name)
          
          # define the column type
          sample.node.attribute <- getNodeAttribute (obj, getAllNodes(obj)[1], node.attribute.name)
          columnType <- findColumnType(typeof(sample.node.attribute))

          # discrete mapping
          discreteMapping(obj, node.attribute.name, attribute.values, attribute.values,
                          visual.property="NODE_TOOLTIP", columnType=columnType, style=style.name)
          
    })  # END setNodeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipRule', 'OptionalCyWinClass',

    function (obj, edge.attribute.name, style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!edge.attribute.name %in% getEdgeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setEdgeTooltipRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        if(length(edgeL(obj@graph))==0){ #i.e., empty obj@graph 
            attribute.values <- getTableColumns('edge',edge.attribute.name)[,edge.attribute.name]
        } else {
            attribute.values = noa (obj@graph, edge.attribute.name)
        }
      
        # set default tooltip
        default.tooltip <- list(visualProperty = "EDGE_TOOLTIP", value = "")
        setVisualProperty(obj, default.tooltip, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(attribute.values[1]))
        
        # discrete mapping
        discreteMapping(obj, edge.attribute.name, attribute.values, attribute.values,
                        visual.property="EDGE_TOOLTIP", columnType=columnType, style=style.name)

    })  # setEdgeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelRule', 'OptionalCyWinClass',
    function (obj, node.attribute.name, style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!node.attribute.name %in% getNodeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setNodeLabelRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
            return ()
        }
        
        if(length(nodes(obj@graph))==0){ #i.e., empty obj@graph 
            attribute.values <- getTableColumns('node',node.attribute.name)[,node.attribute.name]
        } else {
            attribute.values = noa (obj@graph, node.attribute.name)
        }
        
        # set default label
        default.label <- list(visualProperty = "NODE_LABEL", value = "")
        setVisualProperty(obj, default.label, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(attribute.values[1]))
        
        # discrete mapping
        discreteMapping(obj, node.attribute.name, attribute.values, attribute.values,
                        visual.property="NODE_LABEL", columnType=columnType, style=style.name)
    })  # setNodeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelRule', 'OptionalCyWinClass',

    function (obj, edge.attribute.name, style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!edge.attribute.name %in% getEdgeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setEdgeLabelRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        if(length(edgeL(obj@graph))==0){ #i.e., empty obj@graph 
            attribute.values <- getTableColumns('edge',edge.attribute.name)[,edge.attribute.name]
        } else {
            attribute.values = noa (obj@graph, edge.attribute.name)
        }
        
        # set default label
        default.label <- list(visualProperty = "EDGE_LABEL", value = "")
        setVisualProperty(obj, default.label, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(attribute.values[1]))
        
        # discrete mapping
        discreteMapping(obj, edge.attribute.name, attribute.values, attribute.values,
                        visual.property="EDGE_LABEL", columnType=columnType, style=style.name)
    })  # setEdgeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorRule', 'OptionalCyWinClass',
           
           function (obj, node.attribute.name, control.points, colors, mode, default.color='#FFFFFF', style.name = 'default') {
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

               # set default
               setDefaultNodeColor (obj, default.color, style.name)
               
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
                
                continuousMapping (obj, node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=style.name)
                 
                } # if mode==interpolate
                else { # use a discrete rule, with no interpolation, mode==lookup
                   good.args = length (control.points) == length (colors)
                   if (!good.args) {
                       write (sprintf ('control points: %d', length (control.points)), stderr ())
                       write (sprintf ('colors: %d', length (colors)), stderr ())
                       write ("Error! RCy3:setNodeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                       return ()
                }
                   
                discreteMapping(obj, node.attribute.name, control.points, colors, visual.property="NODE_FILL_COLOR", columnType=columnType, style=style.name)    
              
                } # else: !interpolate, aka lookup
     }) # setNodeColorRule

#------------------------------------------------------------------------------------------------------------------------
# Cytoscape distinguishes between Node Opacity, Node Border Opacity, and Node Label Opacity.  we call this 'aspect' here.

setMethod ('setNodeOpacityRule', 'OptionalCyWinClass',

    function (obj, node.attribute.name, control.points, opacities, mode, aspect='all', style.name = 'default') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setNodeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }

        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))
        
        # set default # Comment TanjaM: Current version does not set default
        #setDefaultNodeOpacity (obj, default.opacity, style.name)
        
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
                                   columnType=columnType, style=style.name)
            }
            if (aspect.border){
                continuousMapping (obj, node.attribute.name, control.points, opacities,
                                   visual.property="NODE_BORDER_TRANSPARENCY",
                                   columnType=columnType, style=style.name)
            }
            if (aspect.label){
                continuousMapping (obj, node.attribute.name, control.points, opacities,
                                   visual.property="NODE_LABEL_TRANSPARENCY",
                                   columnType=columnType, style=style.name)
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
                                columnType=columnType, style=style.name)
            }
            
            if (aspect.border){
                discreteMapping(obj, node.attribute.name, control.points, opacities,
                                visual.property="NODE_BORDER_TRANSPARENCY",
                                columnType=columnType, style=style.name)
            }
            
            if (aspect.label){
                discreteMapping(obj, node.attribute.name, control.points, opacities,
                                visual.property="NODE_LABEL_TRANSPARENCY",
                                columnType=columnType, style=style.name)
            }
        } # else: !interpolate
     }) # setNodeOpacityRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorRule', 'OptionalCyWinClass',

    function (obj, node.attribute.name, control.points, colors, mode, default.color='#000000', style.name = 'default') {
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
        
        # set default
        setDefaultNodeBorderColor (obj, default.color, style.name)
        
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
            continuousMapping (obj, node.attribute.name, control.points, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=style.name)

        } # if mode==interpolate
        else { # use a discrete rule, with no interpolation
            good.args = length (control.points) == length (colors)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (colors)), stderr ())
                write ("Error! RCy3:setNodeBorderColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
                return ()
            }
            discreteMapping(obj, node.attribute.name, control.points, colors, visual.property="NODE_BORDER_PAINT", columnType=columnType, style=style.name)
        } # else: !interpolate
     }) # setNodeBorderColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthRule', 'OptionalCyWinClass',

   function (obj, node.attribute.name, attribute.values, line.widths, default.width=1, style.name = 'default') {
       id = as.character (obj@suid)
       
       #TODO we should add interpolate as mode in the function
       mode = "lookup"
       
       if (!node.attribute.name %in% getNodeAttributeNames(obj)) {
           write (sprintf ('Warning! RCy3::setNodeBorderWidthRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
           return ()
       }
       
       # set default
       setDefaultNodeBorderWidth(obj, default.width, style.name)
       
       # define the column type
       columnType <- "String" #findColumnType(typeof(line.widths[1]))
       # discrete mapping
       if (mode=="lookup"){
           discreteMapping (obj, node.attribute.name, attribute.values, line.widths,
                       visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=style.name)
       } else{
           # continuous mapping
           # TODO need to check here if 2 more values were passed in for width
           continuousMapping (obj, node.attribute.name, attribute.values, line.widths, visual.property="NODE_BORDER_WIDTH", columnType=columnType, style=style.name)
       }
     })

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeShape', 'OptionalCyObjClass', 
  function(obj, new.shape, style.name='default') {
      new.shape <- toupper(new.shape)
      if (new.shape %in% getNodeShapes(obj)){
          style = list(visualProperty = "NODE_SHAPE", value = new.shape)
          setVisualProperty(obj, style, style.name)
      }else{
          write (sprintf ('%s is not a valid shape. Use getNodeShapes() to find valid values.', new.shape), stderr ())
      }
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSize', 'OptionalCyObjClass', 
    function(obj, new.size, style.name='default') {
        # lock node dimensions
        lockNodeDimensions (obj, TRUE)
        
        style <- list(visualProperty = "NODE_SIZE", value = new.size)
        setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeColor', 'OptionalCyObjClass', 
  function(obj, new.color, style.name='default') {
    if (.isNotHexColor(new.color)){
        return()
    }
    style = list(visualProperty = "NODE_FILL_COLOR", value = new.color)
    setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeBorderColor', 'OptionalCyObjClass', 
  function(obj, new.color, style.name='default') {
    if (.isNotHexColor(new.color)){
      return()
    }
    style = list(visualProperty = "NODE_BORDER_PAINT", value = new.color)
    setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderWidth', 'OptionalCyObjClass', 
  function(obj, new.width, style.name='default') {
    style = list(visualProperty = "NODE_BORDER_WIDTH", value = new.width) 
    setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeFontSize', 'OptionalCyObjClass', 
  function(obj, new.size, style.name='default') {
    style = list(visualProperty = "NODE_LABEL_FONT_SIZE", value = new.size)
    setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeLabelColor', 'OptionalCyObjClass', 
  function(obj, new.color, style.name='default') {
    if (.isNotHexColor(new.color)){
      return()
    }      
    style = list(visualProperty = "NODE_LABEL_COLOR", value = new.color)
    setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeLineWidth', 'OptionalCyObjClass', 
  function(obj, new.width, style.name='default') {
    style = list(visualProperty = "EDGE_WIDTH", value = new.width) 
    setVisualProperty(obj, style, style.name)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeLineStyle', 'OptionalCyObjClass', 
          function(obj, new.line.style, style.name='default') {
              style = list(visualProperty = "EDGE_LINE_TYPE", value = new.line.style) 
              setVisualProperty(obj, style, style.name)
          })

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeColor', 'OptionalCyObjClass', 
  function(obj, new.color, style.name='default') {
    if (.isNotHexColor(new.color)){
      return()
    }
     # TODO Comment Tanja: maybe change to EDGE_UNSELECTED_PAINT
    style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
    setVisualProperty(obj, style, style.name)
})

setMethod('setDefaultEdgeSourceArrowColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_SOURCE_ARROW_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })

setMethod('setDefaultEdgeTargetArrowColor', 'OptionalCyObjClass', 
          function(obj, new.color, style.name='default') {
              if (.isNotHexColor(new.color)){
                  return()
              }
              style = list(visualProperty = "EDGE_TARGET_ARROW_UNSELECTED_PAINT", value = new.color) 
              setVisualProperty(obj, style, style.name)
          })
# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeFontSize', 'OptionalCyObjClass', 
  function(obj, new.size, style.name='default') {
    style = list(visualProperty = "EDGE_LABEL_FONT_SIZE", value = new.size)
    setVisualProperty(obj, style, style.name)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeRule', 'OptionalCyWinClass',

    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ELLIPSE', style.name = 'default') {
        id = as.character (obj@suid)

        if (!node.attribute.name %in% getNodeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setNodeShapeRule: passed non-existent node attribute: %s', node.attribute.name), stderr ())
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
        setDefaultNodeShape (obj, default.shape, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(node.shapes[1]))
        
        # discrete mapping
        discreteMapping (obj, node.attribute.name, attribute.values, node.shapes,
                             visual.property="NODE_SHAPE", columnType=columnType, style=style.name)
     }) # setNodeShapeRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeSizeRule', 'OptionalCyWinClass',

    function (obj, node.attribute.name, control.points, node.sizes, mode, default.size=40, style.name = 'default') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setNodeSizeRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        # define the column type
        columnType <- findColumnType(typeof(control.points[1]))

        # lock node dimensions
        lockNodeDimensions (obj, TRUE)
        
        # set default
        setDefaultNodeSize (obj, default.size, style.name)
        
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
                               columnType=columnType, style=style.name)
            
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
                            columnType=columnType, style=style.name)
        } # else: !interpolate, aka lookup
        
    }) # setNodeSizeRule
#
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorRule', 'OptionalCyWinClass',

    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#FFFFFF', style.name = 'default') {
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
        
        #set default
        setDefaultEdgeColor (obj, default.color, style.name)
        
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
                           columnType=columnType, style=style.name)
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
                        columnType=columnType, style=style.name)
        } # else: !interpolate, aka lookup
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeOpacityRule', 'OptionalCyWinClass',

    function (obj, edge.attribute.name, control.points, opacities, mode, style.name = 'default') {
        if (!mode %in% c ('interpolate', 'lookup')) {
            write ("Error! RCy3:setEdgeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
            return ()
        }
        
        # set default
        setDefaultEdgeOpacity (obj, default.opacity, style.name)
        
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
                                   columnType=columnType, style=style.name)
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
                            columnType=columnType, style=style.name)
        } # else: !interpolate
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleRule', 'OptionalCyWinClass',

    function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID', style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!edge.attribute.name %in% getEdgeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setEdgeLineStyleRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
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
        setVisualProperty(obj, default.style.list, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(line.styles[1]))
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, line.styles,
                         visual.property="EDGE_LINE_TYPE", columnType=columnType, style=style.name)
     }) # setEdgeLineStyleRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineWidthRule', 'OptionalCyWinClass',
    function (obj, edge.attribute.name, attribute.values, line.widths, mode="interpolate", default.width=1, style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!edge.attribute.name %in% getEdgeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setEdgeLineWidthRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # unconventional arg.name
        control.points = attribute.values
        
        # set default
        default.width.list <- list(visualProperty = "EDGE_WIDTH", value = default.width)
        setVisualProperty(obj, default.width.list, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(line.widths[1]))
        #columnType <- 'String'
        
        if (mode=='interpolate') {  # need a 'below' width and an 'above' width  so there should be two more width than control.points
            if (length (control.points) == length (line.widths)) { # caller did not supply 'below' and 'above' values; manufacture them
                line.widths = c (line.widths [1], line.widths, line.widths [length (line.widths)])
                write ("RCy3::setEdgeLineWidthRule, no 'below' or 'above' widths specified.  Inferred from supplied widths.", stderr ());
            } 
            good.args = length (control.points) == (length (line.widths) - 2)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (line.widths)), stderr ())
                write ("Error! RCy3:setEdgeLineWidthRule, interpolate mode.", stderr ())
                write ("Expecting 1 widths for each control.point, one for 'above' widths, one for 'below' widths", stderr ())
                return ()
            }
            
            continuousMapping (obj, edge.attribute.name, control.points, line.widths,
                               visual.property="EDGE_WIDTH",
                               columnType=columnType, style=style.name)
        } 
        else { # use a discrete rule, with no interpolation, mode==lookup
            good.args = length (control.points) == length (line.widths)
            if (!good.args) {
                write (sprintf ('cp: %d', length (control.points)), stderr ())
                write (sprintf ('co: %d', length (line.widths)), stderr ())
                write ("Error! RCy3:setEdgeLineWidthRule.  Expecting exactly as many widths as control.points in lookup mode.", stderr ())
                return ()
            }
            
            discreteMapping (obj, edge.attribute.name, control.points, line.widths,
                             visual.property="EDGE_WIDTH", columnType=columnType, style=style.name)

        } 
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'OptionalCyWinClass', 

    function (obj, edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!edge.attribute.name %in% getEdgeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setEdgeTargetArrowRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # set default
        default.style.list <- list(visualProperty = "EDGE_TARGET_ARROW_SHAPE", value = default)
        setVisualProperty(obj, default.style.list, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(arrows[1]))
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, arrows,
                         visual.property="EDGE_TARGET_ARROW_SHAPE", columnType=columnType, style=style.name)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowRule', 'OptionalCyWinClass', 

    function (obj, edge.attribute.name, attribute.values, arrows, default='ARROW', style.name = 'default') {
        id = as.character (obj@suid)
        
        if (!edge.attribute.name %in% getEdgeAttributeNames(obj)) {
            write (sprintf ('Warning! RCy3::setEdgeSourceArrowRule: passed non-existent edge attribute: %s', edge.attribute.name), stderr ())
            return ()
        }
        
        # set default
        default.style.list <- list(visualProperty = "EDGE_SOURCE_ARROW_SHAPE", value = default)
        setVisualProperty(obj, default.style.list, style.name)
        
        # define the column type
        columnType <- findColumnType(typeof(arrows[1]))
        
        # discrete mapping
        discreteMapping (obj, edge.attribute.name, attribute.values, arrows,
                         visual.property="EDGE_SOURCE_ARROW_SHAPE", columnType=columnType, style=style.name)
    }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorRule', 'OptionalCyWinClass', 

    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000', style.name = 'default') {
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
        
        #set default
        setDefaultEdgeTargetArrowColor (obj, default.color, style.name)
        
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
                              columnType=columnType, style=style.name)
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
                            columnType=columnType, style=style.name)
        } # else: !interpolate, aka lookup
    }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorRule', 'OptionalCyWinClass', 

    function (obj, edge.attribute.name, control.points, colors, mode="interpolate", default.color='#000000', style.name = 'default') {

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
        
        #set default
        setDefaultEdgeSourceArrowColor (obj, default.color, style.name)
        
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
                               columnType=columnType, style=style.name)
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
                            columnType=columnType, style=style.name)
        } # else: !interpolate, aka lookup
        
     }) # setEdgeSourceArrowColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeSizeDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeWidthDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeHeightDirect', 'OptionalCyWinClass',
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
setMethod('setNodeLabelDirect', 'OptionalCyWinClass', 
    function(obj, node.names, new.labels) {
        setNodePropertyDirect(obj, node.names, new.labels, "NODE_LABEL")
})
## END setNodeLabelDirect

# ------------------------------------------------------------------------------
setMethod('setNodeFontSizeDirect', 'OptionalCyWinClass', 
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
setMethod ('setNodeLabelColorDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeShapeDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeImageDirect', 'OptionalCyWinClass',

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
setMethod ('setNodeBorderWidthDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeBorderColorDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeOpacityDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeFillOpacityDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeBorderOpacityDirect', 'OptionalCyWinClass',
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
setMethod ('setNodeLabelOpacityDirect', 'OptionalCyWinClass',

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
setMethod ('setEdgeOpacityDirect', 'OptionalCyWinClass',

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
setMethod('setEdgeColorDirect', 'OptionalCyWinClass',
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
setMethod('setEdgeLabelDirect', 'OptionalCyWinClass', 
    function(obj, edge.names, new.value) {
        setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL")
})
## END setEdgeLabelDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeFontFaceDirect', 'OptionalCyWinClass', 
    function(obj, edge.names, new.value) {
        setEdgePropertyDirect(obj, edge.names, new.value, "EDGE_LABEL_FONT_FACE")
})
## END setEdgeFontFaceDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeFontSizeDirect', 'OptionalCyWinClass', 
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
setMethod ('setEdgeLabelColorDirect', 'OptionalCyWinClass',
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
setMethod ('setEdgeTooltipDirect', 'OptionalCyWinClass',
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
setMethod('setEdgeLineWidthDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeLineStyleDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeSourceArrowShapeDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeTargetArrowShapeDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeSourceArrowColorDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeTargetArrowColorDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeLabelOpacityDirect', 'OptionalCyWinClass', 
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
setMethod('setEdgeSourceArrowOpacityDirect', 'OptionalCyWinClass', 
    function(obj, edge.names, new.values) {
        write(sprintf("WARNING: Method RCy3::setEdgeSourceArrowOpacityDirect() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END setEdgeSourceArrowOpacityDirect

# ------------------------------------------------------------------------------
setMethod('setEdgeTargetArrowOpacityDirect', 'OptionalCyWinClass', 
    function(obj, edge.names, new.values) {
        write(sprintf("WARNING: Method RCy3::setEdgeTargetArrowOpacityDirect() is not implemented in RCy3!"), stderr())
        
        return(FALSE)
})
## END setEdgeTargetArrowOpacityDirect

#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeLabelPositionDirect', 'OptionalCyWinClass',
#   function (obj, edge.names, new.value) {
#     id = as.character (obj@suid)
#     for (edge.name in edge.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Position', as.character (new.value))
#     invisible (result)
#     })

# ------------------------------------------------------------------------------
setMethod('getNodeCount', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes/count", sep="/")
        request.res <- GET(resource.uri)
        node.count <- unname(fromJSON(rawToChar(request.res$content)))
        
        return(as.integer(node.count))
})
## END getNodeCount

# ------------------------------------------------------------------------------
setMethod('getEdgeCount', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "edges/count", sep="/")
        request.res <- GET(resource.uri)
        edge.count <- unname(fromJSON(rawToChar(request.res$content)))
        
        return(as.integer(edge.count))
})
## END getEdgeCount

# ------------------------------------------------------------------------------
setMethod('getNodeAttribute', 'OptionalCyWinClass', 
    function(obj, node.name, attribute.name) {
        net.SUID <- as.character(obj@suid)
        
        
        node.SUID <- as.character(.nodeNameToNodeSUID(obj, node.name))
        
        if(length(node.SUID) < 1) {
            write(sprintf("WARNING in RCy3::getNodeAttribute():\n\t no node with name '%s' could be found >> function returns empty value", node.name), stderr())
            
            return("")
        } else {
            node.attribute.type <- getNodeAttributeType(obj, attribute.name)
            
            if(length(node.attribute.type) > 0) {
                res=commandRun(paste0('node get attribute nodeList=',node.name,' columnList=',attribute.name))
                res <- gsub("}","",res)
                node.attribute.value <- unlist(strsplit(res,":"))[4]
                # resource.uri <- 
                #     paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/rows", node.SUID, attribute.name, sep="/")
                # request.res <- GET(url=resource.uri)
                # node.attribute.value <- unname(rawToChar(request.res$content))
                
                switch(node.attribute.type, 
                    "Double"={
                        if(is.na(node.attribute.value))
                            node.attribute.value = 0.0
                        return(as.numeric(node.attribute.value))
                    },
                    "Long"=,
                    "Integer"={
                        if(is.na(node.attribute.value))
                            node.attribute.value = 0
                        return(as.integer(node.attribute.value))
                    },
                    "Boolean"={
                        if(is.na(node.attribute.value))
                            node.attribute.value = FALSE
                        return(as.logical(node.attribute.value))
                    },{
                        if(is.na(node.attribute.value))
                            node.attribute.value = ''
                        return(as.character(node.attribute.value))
                    }
                )
            }
            return("")
        }
})

# ------------------------------------------------------------------------------
setMethod('getNodeAttributeType', 'OptionalCyWinClass', 
    function(obj, attribute.name) {
        net.SUID <- as.character(obj@suid)
        
        
        if(attribute.name %in% getNodeAttributeNames(obj)) {
            resource.uri <- 
                paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
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
setMethod('getAllNodeAttributes', 'OptionalCyWinClass', 
          function(obj, onlySelectedNodes = FALSE) {
              
              if(length(nodes(obj@graph))==0){ #i.e., empty obj@graph 
                getTableColumns('node',obj=obj)
              }
              
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
setMethod('getEdgeAttribute', 'OptionalCyWinClass', 
    function(obj, edge.name, attribute.name) {
        net.SUID <- as.character(obj@suid)
        
        
        edge.SUID <- as.character(.edgeNameToEdgeSUID(obj, edge.name))
        
        if(length(edge.SUID) < 1) {
            write(sprintf("WARNING in RCy3::getEdgeAttribute():\n\t no edge with name '%s' could be found >> function returns empty value", edge.name), stderr())
            
            return("")
        } else {
            edge.attribute.type <- getEdgeAttributeType(obj, attribute.name)
            
            if(length(edge.attribute.type) > 0) {
                resource.uri <- 
                    paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/rows", edge.SUID, attribute.name, sep="/")
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
setMethod('getEdgeAttributeType', 'OptionalCyWinClass', 
    function(obj, attribute.name) {
        net.SUID <- as.character(obj@suid)
        
        
        if(attribute.name %in% getEdgeAttributeNames(obj)) {
            resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
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
setMethod ('getAllEdgeAttributes', 'OptionalCyWinClass',
  function (obj, onlySelectedEdges=FALSE) {
      
      if(length(edgeL(obj@graph))==0){ #i.e., empty obj@graph 
        getTableColumns('edge',obj=obj)
      }

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
setMethod('getNodeAttributeNames', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns", sep="/")
        # request result
        request.res <- GET(url=resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        request.res <- data.frame(t(sapply(request.res, base::c)))
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
setMethod('getEdgeAttributeNames', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        resource.uri <- 
            paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns", sep="/")
        # request result
        request.res <- GET(url=resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        request.res <- data.frame(t(sapply(request.res, base::c)))
        request.res <- unlist(request.res$name)
        # exclude some edge attributes
        edge.attribute.names <- request.res[! request.res %in% c("SUID", "shared name", "shared interaction", "selected")]
        return(edge.attribute.names)
})
## END getEdgeAttributeNames

# ------------------------------------------------------------------------------
# delete node attribute by deleting its column in the node table
setMethod('deleteNodeAttribute', 'OptionalCyObjClass', 
  function(obj, attribute.name) {
     if (attribute.name %in% getNodeAttributeNames(obj)){
        resource.uri <- paste(obj@uri, obj@api, "networks", as.character(obj@suid), "tables/defaultnode/columns", as.character(attribute.name), sep="/")
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
setMethod('deleteEdgeAttribute', 'OptionalCyObjClass', 
  function(obj, attribute.name) {
     if (attribute.name %in% getEdgeAttributeNames(obj)){
        resource.uri <- paste(obj@uri, obj@api, "networks", as.character(obj@suid), "tables/defaultedge/columns", as.character(attribute.name), sep="/")
        request.res <- DELETE(url= resource.uri)
        write(sprintf('Attribute "%s" has been deleted...', attribute.name), stderr())
        invisible(request.res)
     } else{
        msg = paste (attribute.name, 'does not exist and thus could not be deleted.')
        write (msg, stderr ())
     }
})

# ------------------------------------------------------------------------------
setMethod('getAllNodes', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        n.count <- getNodeCount(obj)
        
        if(n.count == 0) {
            return()
        }
        
        # get SUIDs of existing (in Cytoscape) nodes
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes", sep="/")
        # get the SUIDs of the nodes in the Cytoscape graph
        cy.nodes.SUIDs <- fromJSON(rawToChar(GET(resource.uri)$content))

        dict.nodes.SUIDs <- sapply(obj@node.suid.name.dict, "[[", 2)
        
        # check that the nodes presented in Cytoscape & RCy3's session dictionary do match
        diff.nodes <- setdiff(cy.nodes.SUIDs, dict.nodes.SUIDs)
        
        # in case that differences exist, run synchronization b/n RCy3 and RCytoscape
        if(length(diff.nodes) > 0) {
            write(sprintf("WARNING in RCy3::getAllNodes():\n\t the following node(s) exist in Cytoscape, but don't exist in RCy3's session"), stderr())
            
            nodes.only.in.cytoscape <- c()
            for(i in 1:length(diff.nodes)) {
                resource.uri <- 
                    paste(obj@uri, obj@api, "networks", net.SUID, "nodes", as.character(diff.nodes[i]), sep="/")
                node.name <- fromJSON(rawToChar(GET(resource.uri)$content))$data$name 
                nodes.only.in.cytoscape <- c(nodes.only.in.cytoscape, node.name)
            #    [GIK, Jul 2015] synch to be implemented
            #    obj@node.suid.name.dict[[length(obj@node.suid.name.dict) + 1]] <- 
            #        list(name=node.name, SUID=diff.nodes[i])
            }
            print(nodes.only.in.cytoscape)
        }
        node.names <- .nodeSUIDToNodeName(obj, cy.nodes.SUIDs[order(cy.nodes.SUIDs)])

        return(node.names)
})
## END getAllNodes

# ------------------------------------------------------------------------------
setMethod('getAllEdges', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        
        count <- getEdgeCount(obj)
        if(count == 0) {
            return()
        }
        
        # get edge name column and return its values
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns/name", sep="/")
        request.res <- GET(url=resource.uri)
        request.res <- fromJSON(rawToChar(request.res$content))
        names <- request.res$values
        return(names)
})
## END getAllEdges

# ------------------------------------------------------------------------------
setMethod('clearSelection', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        
        # if any nodes are selected, unselect them
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultnode/columns/selected?default=false", sep="/")
        request.res <- PUT(url=resource.uri, body=FALSE)
        
        # if any edges are selected, unselect them
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "tables/defaultedge/columns/selected?default=false", sep="/")
        request.res <- PUT(url=resource.uri, body=FALSE)
        
        invisible(request.res)
}) 
## END clearSelection
   
# ------------------------------------------------------------------------------
setMethod('selectNodes', 'OptionalCyWinClass', 
    function(obj, node.names, by.col='name', preserve.current.selection = TRUE) {
        base.url=paste(obj@uri,obj@api,sep = "/")
        network=obj@title

        if (!preserve.current.selection )
            clearSelection(obj)

        node.list.str = NULL
        for (n in node.names){
            if(is.null(node.list.str))
                node.list.str = paste(by.col,n,sep=":")
            else
                node.list.str = paste(node.list.str,paste(by.col,n,sep=":"),sep=",")
        }
        
        json_sel<-list(
            network=network,
            nodeList=node.list.str
        )
        sel <- toJSON(json_sel)
        url<- sprintf("%s/commands/network/select", base.url,sep="")
        response <- POST(url=url,body=sel, encode="json",content_type_json())
        selectedNodes=unname(fromJSON(rawToChar(response$content)))[[1]]
        if(length(selectedNodes)==0)
            selectedNodes = c()
        return(selectedNodes)
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
          'OptionalCyWinClass', 
          function(obj) {
            
            resource.uri <- paste(obj@uri,
                                  obj@api,
                                  "networks",
                                  obj@suid,
                                  "nodes",
                                  sep = "/")
            
            request.res <- GET(resource.uri) # returns all of the node SUIDs
            all_node_SUIDs <- fromJSON(rawToChar(request.res$content))
            SUID.value.pairs <- lapply(all_node_SUIDs,
                                       function(s) {list('SUID' = s, 'value' = TRUE)})
            SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
            
            resource.uri <- paste(obj@uri,
                                  obj@api,
                                  "networks",
                                  obj@suid,
                                  "tables/defaultnode/columns/selected",
                                  sep = "/")
            request.res <- PUT(url = resource.uri,
                               body = SUID.value.pairs.JSON,
                               encode = "json")
            invisible(request.res)
          })


   
# ------------------------------------------------------------------------------
setMethod('getSelectedNodeCount', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
        request.res <- GET(url=resource.uri)
        
        num.selected.nodes <- length(fromJSON(rawToChar(request.res$content)))
        
        return(num.selected.nodes)
}) 
## END getSelectedNodeCount
   
# ------------------------------------------------------------------------------
setMethod('getSelectedNodes', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        
        if(getSelectedNodeCount(obj) == 0) {
            write (sprintf ('warning!  No nodes selected.'), stdout ())
            return(NA)
        } else {
            resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "nodes?column=selected&query=true", sep="/")
            request.res <- GET(url=resource.uri)
            
            selected.node.SUIDs <- fromJSON(rawToChar(request.res$content))
            selected.node.names <- .nodeSUIDToNodeName(obj, selected.node.SUIDs)
            return(selected.node.names)
    }
}) 
## END getSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedNodes', 'OptionalCyWinClass',

   function (obj) {
       node.names <- getSelectedNodes(obj)
       setNodePropertyDirect(obj, node.names, 'false', "NODE_VISIBLE")
   }) # hideSelectedNodes
   
# ------------------------------------------------------------------------------
setMethod('hideNodes', 'OptionalCyWinClass', 
    function(obj, node.names) {
        setNodePropertyDirect(obj, node.names, 'false', "NODE_VISIBLE")
}) 
## END hideNodes
   
# ------------------------------------------------------------------------------------------------------------------------
setMethod('unhideNodes', 'OptionalCyWinClass', 
    function(obj, node.names) {
        setNodePropertyDirect(obj, node.names, 'true', "NODE_VISIBLE")
}) 
## END unhideNodes

# ------------------------------------------------------------------------------
# select all nodes that were not selected and deselect all nodes that were selected
setMethod('invertNodeSelection', 'OptionalCyWinClass', 
    function(obj) {
        commandRun(paste0('network select invert=nodes network=',obj@title), obj)
}) 
## END invertNodeSelection
 
# ------------------------------------------------------------------------------
# [GIK - Jul, 2015] function might break if self-loops exist in the graph
setMethod('deleteSelectedNodes', 'OptionalCyWinClass', 
    function(obj) {
        
        if(missing(obj))
            commandRun(paste0('network delete nodeList=selected network=',obj@title), obj)
        
        loc.obj <- obj
        
        net.SUID <- as.character(loc.obj@suid)
        
        selected.node.names <- getSelectedNodes(loc.obj)
        selected.node.SUIDs <- .nodeNameToNodeSUID(loc.obj, selected.node.names)
        
        for(i in 1:length(selected.node.SUIDs)) {
            node.SUID <- selected.node.SUIDs[i]
            # (list of) edges that have this particular node as their source node
            source.bound.edges <- list()
            # (list of) edges that have this particular node as their target node
            target.bound.edges <- list()
            
            source.bound.edge.indices <- 
                which(sapply(loc.obj@edge.node.suid.name.dict, function(n) {n$source.node}) %in% node.SUID)
            
            if(length(source.bound.edge.indices) > 0) {
                # get edge SUIDs
                source.bound.edges <- 
                    sapply(loc.obj@edge.node.suid.name.dict[source.bound.edge.indices], function(e) { e$SUID })
                # delete all edges, whose source node is to-be deleted
                for(k in 1:length(source.bound.edges)) {
                    resource.uri <- paste(loc.obj@uri, obj@api, "networks", net.SUID, "edges", as.character(source.bound.edges[k]), sep="/")
                    
                    request.res <- DELETE(url=resource.uri)
                    # [GIK] TO-DO: delete the edge row/entry from Cytoscape's Edge table
                }
                # also, delete those edges from the session dictionary
                loc.obj@edge.node.suid.name.dict[source.bound.edge.indices] <- NULL
            }
            
            target.bound.edge.indices <- 
                which(sapply(loc.obj@edge.node.suid.name.dict, function(n) {n$target.node}) %in% node.SUID)
            
            if(length(target.bound.edge.indices) > 0) {
                # get edge SUIDs
                target.bound.edges <- 
                    sapply(loc.obj@edge.node.suid.name.dict[target.bound.edge.indices], function(e) { e$SUID })
                # delete all edges, whose target node is to-be deleted
                for(k in 1:length(target.bound.edges)) {
                    resource.uri <- paste(loc.obj@uri, obj@api, "networks", net.SUID, "edges", as.character(target.bound.edges[k]), sep="/")
                    
                    request.res <- DELETE(url=resource.uri)
                    # [GIK] TO-DO: delete the edge row/entry from Cytoscape's Edge table
                }
                # also, delete those edges from the session dictionary
                loc.obj@edge.node.suid.name.dict[target.bound.edge.indices] <- NULL
            }
            
            # delete the node from the Cytoscape network
            resource.uri <- 
                paste(loc.obj@uri, obj@api, "networks", net.SUID, "nodes", as.character(node.SUID), sep="/")
            request.res <- DELETE(url=resource.uri)
        
            # [GIK] TO-DO: delete the node row/entry in the Cytoscape node table
        
            # delete the node from the session disctionary
            node.index <- 
                which(sapply(loc.obj@node.suid.name.dict, function(n) { n$SUID }) %in% node.SUID)
            loc.obj@node.suid.name.dict[node.index] <- NULL
        }
        eval.parent(substitute(obj <- loc.obj))
})
## END deleteSelectedNodes
   
# ------------------------------------------------------------------------------
setMethod('selectEdges', 'OptionalCyWinClass', 
    function(obj, edge.names, by.col, preserve.current.selection=TRUE) {
        base.url=paste(obj@uri,obj@api,sep = "/")
        network=obj@title
        
        if (!preserve.current.selection )
            clearSelection(network=network,base.url=base.url)
        
        edge.list.str = NULL
        for (n in edge.names){
            if(is.null(edge.list.str))
                edge.list.str = paste(by.col,n,sep=":")
            else
                edge.list.str = paste(edge.list.str,paste(by.col,n,sep=":"),sep=",")
        }
        
        json_sel<-list(
            network=network,
            edgeList=edge.list.str
        )
        sel <- toJSON(json_sel)
        url<- sprintf("%s/commands/network/select", base.url,sep="")
        response <- POST(url=url,body=sel, encode="json",content_type_json())
        selectedEdges=unname(fromJSON(rawToChar(response$content)))[[1]]
        if(length(selectedEdges)==0)
            selectedEdges = c()
        return(selectedEdges)
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
          'OptionalCyWinClass', 
          function(obj) {
            
            resource.uri <- paste(obj@uri,
                                  obj@api,
                                  "networks",
                                  obj@suid,
                                  "edges",
                                  sep = "/")
            
            request.res_edges <- GET(resource.uri) ## returns all of the edge suids
            all_edge_SUIDs <- fromJSON(rawToChar(request.res_edges$content))
            SUID.value.pairs <- lapply(all_edge_SUIDs,
                                       function(s) {list('SUID' = s, 'value' = TRUE)})
            SUID.value.pairs.JSON <- toJSON(SUID.value.pairs)
            
            resource.uri <- paste(obj@uri,
                                  obj@api,
                                  "networks",
                                  obj@suid,
                                  "tables/defaultedge/columns/selected",
                                  sep = "/")
            request.res <- PUT(url = resource.uri,
                               body = SUID.value.pairs.JSON,
                               encode = "json")
            invisible(request.res)
          })
 
# ------------------------------------------------------------------------------
setMethod('invertEdgeSelection', 'OptionalCyWinClass', 
    function(obj) {
        commandRun(paste0('network select invert=edges network=',obj@title), obj)
}) 
## END invertEdgeSelection
 
# ------------------------------------------------------------------------------
setMethod('deleteSelectedEdges', 'OptionalCyWinClass', 
    function(obj) {
        
        if(missing(obj))
            commandRun(paste0('network delete edgeList=selected network=',obj@title), obj)
        
        loc.obj <- obj
        
        net.SUID = as.character(loc.obj@suid)
        
        selected.edge.names = getSelectedEdges(loc.obj)
        selected.edge.SUIDs = .edgeNameToEdgeSUID(loc.obj, selected.edge.names)
        
        for(i in 1:length(selected.edge.SUIDs)) {
            edge.SUID = selected.edge.SUIDs[i]
            resource.uri = paste(loc.obj@uri, obj@api, "networks", net.SUID, "edges", edge.SUID, sep="/")
            # delete edge from canvas / view
            request.res = DELETE(url=resource.uri)
            
            # delete edge from edge table : NOT possible in the API
            
            # delete edge record from the session dictionary
            loc.obj@edge.node.suid.name.dict[which(sapply(loc.obj@edge.node.suid.name.dict, function(e) { e$SUID }) %in% edge.SUID)] <- NULL
        }
        
        eval.parent(substitute(obj <- loc.obj))
}) 
## END deleteSelectedEdges
   
# ------------------------------------------------------------------------------
setMethod('getSelectedEdgeCount', 'OptionalCyWinClass', 
    function(obj) {
        net.SUID <- as.character(obj@suid)
        
        
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
        request.res <- GET(url=resource.uri)
        
        num.selected.edges <- length(fromJSON(rawToChar(request.res$content)))
        return(num.selected.edges)
})
## END getSelectedEdgeCount
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdges', 'OptionalCyWinClass',

    function (obj) {
        net.SUID = as.character(obj@suid)
        if(getSelectedEdgeCount(obj) == 0) {
            return (NA)
        } else {
            resource.uri = paste(obj@uri, obj@api, "networks", net.SUID, "edges?column=selected&query=true", sep="/")
            request.res = GET(url=resource.uri)
            selected.edges.SUIDs = fromJSON(rawToChar(request.res$content))
            selected.edges = .edgeSUIDToEdgeName(obj, selected.edges.SUIDs)
            
            return(selected.edges)
        }
}) # getSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedEdges', 'OptionalCyWinClass',

    function (obj) {
        edge.names <- getSelectedEdges(obj)
        setEdgePropertyDirect(obj, edge.names, 'false', "EDGE_VISIBLE")
     }) # hideSelectedEdges
   
# ------------------------------------------------------------------------------
setMethod('unhideAll', 'OptionalCyWinClass', 
    function(obj) {
        node.names <- getAllNodes(obj)
        setNodePropertyDirect(obj, node.names, 'true', "NODE_VISIBLE")
        
        edge.names <- getAllEdges(obj)
        setEdgePropertyDirect(obj, edge.names, 'true', "EDGE_VISIBLE")
}) 
## END unhideAll
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getFirstNeighbors', 'OptionalCyWinClass',

   function (obj, node.names, as.nested.list=FALSE) {
      if (length (node.names) == 0){
         return()
      }else{
         # map node names to node SUIDs
         dict.indices = which(sapply(obj@node.suid.name.dict, function(s) { s$name }) %in% node.names)
         node.SUIDs = sapply(obj@node.suid.name.dict[dict.indices], function(i) {i$SUID})
         
         # network ID 
         net.suid = as.character(obj@suid)
         
         # get first neighbors
         # TODO at some later point it might be nice to return the first neighbors as nested lists
         neighbor.names <- c()
         
         for (node.SUID in node.SUIDs){
            # get first neighbors for each node
            resource.uri <- paste(obj@uri, obj@api, "networks", net.suid, "nodes", as.character(node.SUID), "neighbors", sep="/")
            request.res <- GET(resource.uri)
            first.neighbors.SUIDs <- fromJSON(rawToChar(request.res$content))
            
            # map node SUIDs to node names
            dict.indices <- which(sapply(obj@node.suid.name.dict, function(s) { s$SUID }) %in% first.neighbors.SUIDs)
            if (as.nested.list){
                neighbor.names <- append(neighbor.names, list(c(neighbor.names, sapply(obj@node.suid.name.dict[dict.indices], function(i) {i$name}))))
            }else{
                neighbor.names <- c(neighbor.names, sapply(obj@node.suid.name.dict[dict.indices], function(i) {i$name}))
            }
         }
         return (neighbor.names)
      }
      })  # getFirstNeighbors

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
#' @seealso \code{\link{createNetworkFromSelection}}, \code{\link{selectEdgesConnectedBySelectedNodes}}, \code{\link{renameNetwork}}
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
    if (window.title %in% as.character (getNetworkList (cy)))
    deleteNetwork (cy, window.title)
    
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
  stopifnot (attribute.type %in% c ('char', 'integer', 'numeric', 'boolean'))
  if (attribute.type == 'char')
    attribute.type = 'STRING'
  else if (attribute.type == 'integer')
    attribute.type = 'INTEGER'
  else if (attribute.type == 'numeric')
    attribute.type = 'FLOATING'
  else if (attribute.type == 'boolean')
      attribute.type = 'BOOLEAN'

  nodeDataDefaults (graph, attr=attribute.name) = default.value
  attr (nodeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type

  return (graph)

} # initNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
initEdgeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
    stopifnot (attribute.type %in% c ('char', 'integer', 'numeric', 'boolean'))
    if (attribute.type == 'char')
        attribute.type = 'STRING'
    else if (attribute.type == 'integer')
        attribute.type = 'INTEGER'
    else if (attribute.type == 'numeric')
        attribute.type = 'FLOATING'
    else if (attribute.type == 'boolean')
        attribute.type = 'BOOLEAN'

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
#    <sourceNode> (interactionType) <target.node.name>
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
setMethod('getVisualStyleNames', 'OptionalCyObjClass', 
  function(obj) {
    resource.uri = paste(obj@uri, obj@api, "apply/styles", sep="/")
    request.res = GET(url=resource.uri)
    style.names = unname(fromJSON(rawToChar(request.res$content)))
    return(style.names)
})

# ------------------------------------------------------------------------------
setMethod('copyVisualStyle', 'OptionalCyObjClass', 
  function (obj, from.style, to.style) {
     current.names = getVisualStyleNames (obj)
     if (! from.style %in% current.names){
        stop (sprintf ('Cannot copy from a non-existent visual style (%s)', from.style))
     }
     # get the current style from Cytoscape
     resource.uri <- paste(obj@uri, obj@api, "styles", from.style, sep="/")
     from.style.JSON <- GET(url=URLencode(resource.uri))
     from.style <- fromJSON(rawToChar(from.style.JSON$content))
     from.style[1] <- as.character(to.style)
     
     # and send it to Cytoscape as a new style with a new name
     to.style.JSON <- toJSON(from.style)
     resource.uri <- paste(obj@uri, obj@api, "styles", sep="/")
     request.res <- POST(url = resource.uri, body = to.style.JSON, encode = "json")
     invisible(request.res)
})

# ------------------------------------------------------------------------------
# apply visual style to network
setMethod('setVisualStyle', 'OptionalCyWinClass', 
  function(obj, style.name) {
      
    net.SUID = as.character(obj@suid)
    current.names = getVisualStyleNames(obj)
    # inform user if they want to set style that does not exist 
    if(!style.name %in% current.names) { 
      stop(sprintf('Cannot call setVisualStyle on a non-existent visual style (%s)', style.name))
    }
    # apply style
    resource.uri <- paste(obj@uri, obj@api, "apply/styles", style.name, net.SUID, sep="/")
    req.res <- GET(url=URLencode(resource.uri))
    write(sprintf('network visual style has been set to "%s"', style.name), stdout())
    invisible(req.res)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('lockNodeDimensions', 'OptionalCyObjClass',

    function (obj, new.state, style.name='default') {
        # launch error if visual style name is missing
        if (! style.name %in% getVisualStyleNames (obj)) {
            write (sprintf ('Error in RCy3::lockNodeDimensions. No visual style named "%s"', style.name), stdout ())
            return ()
        }

        #lock node dimensions
        resource.uri <- paste(obj@uri, obj@api, "styles", as.character(style.name), "dependencies", sep="/")
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
setMethod('getDefaultBackgroundColor', 'OptionalCyObjClass', 
  function(obj, style.name='default') {
    resource.uri = paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults/NETWORK_BACKGROUND_PAINT", sep="/")
    request.res = GET(url=resource.uri)
    def.background.color = fromJSON(rawToChar(request.res$content))[[2]]
    return(def.background.color)
})

# ------------------------------------------------------------------------------
setMethod('setDefaultBackgroundColor', 'OptionalCyObjClass', 
    function(obj, new.color, style.name='default') {
        if (.isNotHexColor(new.color)){
            return()
        } 
        resource.uri = paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", sep="/")
        style = list(visualProperty = 'NETWORK_BACKGROUND_PAINT', value = new.color)
        style.JSON = toJSON(list(style))
        request.res = PUT(url=resource.uri, body=style.JSON, encode="json")
        invisible(request.res)
})

# ------------------------------------------------------------------------------
setMethod('getDefaultNodeSelectionColor', 'OptionalCyObjClass', 
  function(obj, style.name='default') {
    return(getVisualProperty(obj, style.name, 'NODE_SELECTED_PAINT'))
})

# ------------------------------------------------------------------------------
setMethod('setDefaultNodeSelectionColor', 'OptionalCyObjClass', 
    function(obj, new.color, style.name='default') {
        if (.isNotHexColor(new.color)){
            return()
        } 
        style = list(visualProperty = "NODE_SELECTED_PAINT", value = new.color) 
        setVisualProperty(obj, style, style.name)
})
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeReverseSelectionColor',  'OptionalCyObjClass',

   function (obj, style.name='default') {
       return(getVisualProperty(obj, style.name, 'NODE_PAINT'))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeReverseSelectionColor',  'OptionalCyObjClass',

   function (obj, new.color, style.name='default') {
       if (.isNotHexColor(new.color)){
           return()
       } 
       style = list(visualProperty = "NODE_PAINT", value = new.color) 
       setVisualProperty(obj, style, style.name)
      })

# ------------------------------------------------------------------------------
setMethod('getDefaultEdgeSelectionColor', 'OptionalCyObjClass', 
  function(obj, style.name='default') {
    return(getVisualProperty(obj, style.name, 'EDGE_STROKE_SELECTED_PAINT'))
})

# ------------------------------------------------------------------------------
setMethod('setDefaultEdgeSelectionColor', 'OptionalCyObjClass', 
    function(obj, new.color, style.name='default') {
        if (.isNotHexColor(new.color)){
            return()
        }
        style = list(visualProperty = "EDGE_STROKE_SELECTED_PAINT", value = new.color) 
        setVisualProperty(obj, style, style.name)
})

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeReverseSelectionColor',  'OptionalCyObjClass',

   function (obj, style.name='default') {
      return(getVisualProperty(obj, style.name, 'EDGE_STROKE_UNSELECTED_PAINT'))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeReverseSelectionColor',  'OptionalCyObjClass',

   function (obj, new.color, style.name='default') {
      if (.isNotHexColor(new.color)){
          return()
      } 
      style = list(visualProperty = "EDGE_STROKE_UNSELECTED_PAINT", value = new.color) 
      setVisualProperty(obj, style, style.name)
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveImage', 'OptionalCyWinClass',
           
           function (obj, filename, image.type, h = 600) {
             image.type = tolower (image.type)
             stopifnot (image.type %in% c ('png', 'pdf', 'svg'))
             id = as.character (obj@suid)
             
             if (!file.exists(filename)){
               if(image.type=='png'){
                 
                 resource.uri <- paste(obj@uri, obj@api, "networks", id,
                                       paste0("views/first.", image.type, "?h=", h), sep="/")  
               } 
               else{
                 # get the view image from Cytoscape in PNG, PDF, or SVG format
                 resource.uri <- paste(obj@uri, obj@api, "networks", id,
                                       paste0("views/first.", image.type), sep="/")
               }
               request.res <- GET(resource.uri, write_disk(paste0(filename,".", image.type), overwrite = TRUE))
               write (sprintf ('saving image to %s.%s', filename, image.type), stderr ())
             }else{
               write (sprintf ('choose another filename. File exists: %s', filename), stderr ())
             }
           }) # saveImage
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveNetwork', 'OptionalCyWinClass',

   function (obj, filename, type='cys') {
       if (!file.exists(filename)){
           type=toupper(type)
           if(type=='CYS'){ # save entire session
               saveSession(filename = filename, obj = obj)
           }
           else { #e.g., CX, CYJS, GraphML, NNF, SIF, XGMML (case sensitive)
               if(type=="GRAPHML") #fix case for exceptions
                   type = 'GraphML'
               commandRun(paste0('network export options=',type,' OutputFile="',filename,'"'),obj)
           }
       }else{
           write (sprintf ('choose another filename. File exists: %s', filename), stderr ())
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
getVisualProperty <- function(obj, style.name, property) {
    resource.uri <- paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", property, sep="/")
    request.res <- GET(url=resource.uri)
    return(fromJSON(rawToChar(request.res$content))[[2]])
}

# ------------------------------------------------------------------------------
setVisualProperty <- function(obj, style.string, style.name='default') {
    resource.uri <- paste(obj@uri, obj@api, "styles", as.character(style.name), "defaults", sep="/")
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
setNodePropertyDirect <- function(obj=CytoscapeWindowFromNetwork(), node.names, new.values, visual.property) {
    # get network ID 
    net.SUID <- as.character(obj@suid)
    
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
        resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "nodes", node.SUIDs, sep="/")
        node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=new.values)))
        request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
    } else {
        # multiple nodes
        for (i in seq(node.SUIDs)) {
            node.SUID <- as.character(node.SUIDs[i])
            current.value <- new.values[i]
            
            resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "nodes", node.SUID, sep="/")
            node.SUID.JSON <- toJSON(list(list(visualProperty=visual.property, value=current.value)))
            request.res <- PUT(resource.uri, body=node.SUID.JSON, encode="json")
        } # end for (node.SUID in node.SUIDs)
    }
    invisible(request.res)
}
## END setNodePropertyDirect

# ------------------------------------------------------------------------------
setEdgePropertyDirect <- function(obj=CytoscapeWindowFromNetwork(), edge.names, new.values, visual.property) {
    # get network ID 
    net.SUID <- as.character(obj@suid)
    
    
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
            
            resource.uri <- paste(obj@uri, obj@api, "networks", net.SUID, "views", view.SUID, "edges", edge.SUID, sep="/")
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
    resource.uri <- paste(obj@uri, obj@api, "networks", as.character(obj@suid), "tables/defaultedge", sep="/")
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
    resource.uri <- paste(obj@uri, obj@api, "styles", style, "mappings", sep="/")
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
    resource.uri <- paste(obj@uri, obj@api, "styles", style, "mappings", sep="/")
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

#########################
# ported from r2cytoscape 
#########################
# ------------------------------------------------------------------------------
#' Check the version of Cytoscape
#'
#' @param base.url cyrest base url for communicating with cytoscape
#' @return Cytoscape version
#' @export
#' @import httr
#' @import RJSONIO

checkCytoscapeVersion<-function(obj=CytoscapeConnection()){
    checkversion.url = paste(obj@uri, obj@api, "version", sep="/")
    res = GET(checkversion.url)
    cytoscape.version = fromJSON(rawToChar(res$content))[2][[1]]
    return(cytoscape.version)
}

# ------------------------------------------------------------------------------
#' Command Help
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function returns a list of available commands or args.
#' @details Works with or without 'help' command prefix. Note that if you ask about a command that doesn't
#' have any arguments, this function will run the command!
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return List of available commands or args
#' @export
#' @examples
#' \donttest{
#' commandHelp()
#' commandHelp('node')
#' commandHelp('node get attribute')
#' }
#' @import XML
#' @import httr
#' @importFrom utils head
#' @importFrom utils tail

commandHelp<-function(cmd.string='help', obj=CytoscapeConnection()){
    s=sub('help *','',cmd.string)
    cmds = GET(command2query(s,obj))
    cmds.html = htmlParse(rawToChar(cmds$content), asText=TRUE)
    cmds.elem = xpathSApply(cmds.html, "//p", xmlValue)
    cmds.list = cmds.elem
    if (length(cmds.elem)==1){
        cmds.list = unlist(strsplit(cmds.elem[1],"\n\\s*"))
    }
    print(head(cmds.list,1))
    tail(cmds.list,-1)
}

# ------------------------------------------------------------------------------
#' Command Run
#'
#' @description Using the same syntax as Cytoscape's Command Line Dialog,
#' this function converts a command string into a CyREST URL, executes a GET
#' request, and parses the HTML result content into an R list object.
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return List object
#' @export
#' @examples
#' \donttest{
#' commandRun('layout get preferred')
#' }
#' @import XML
#' @import httr

commandRun<-function(cmd.string, obj=CytoscapeConnection()){
    
    ##TODO use POST or leave alone for "GET friendly" queries, i.e., guaranteed to be short urls?
    res = GET(command2query(cmd.string,obj))
    res.html = htmlParse(rawToChar(res$content), asText=TRUE)
    res.elem = xpathSApply(res.html, "//p", xmlValue)
    if(startsWith(res.elem[1],"[")){
        res.elem[1] = gsub("\\[|\\]|\"","",res.elem[1])
        res.elem2 = unlist(strsplit(res.elem[1],"\n"))[1]
        res.list = unlist(strsplit(res.elem2,","))
    }else {
        res.list = unlist(strsplit(res.elem[1],"\n\\s*"))
        res.list = res.list[!(res.list=="Finished")]
    }
    res.list
}

# ------------------------------------------------------------------------------
#' Command string to CyREST query URL
#'
#' @description Converts a command string to a CyREST query url.
#' @param cmd.string (char) command
#' @param base.url cyrest base url for communicating with cytoscape
#' @return cyrest url
#' @export
#' @examples
#' \donttest{
#' command2query('layout get preferred')
#' }
#' @importFrom utils URLencode

command2query<-function(cmd.string, obj=CytoscapeConnection()){
    base.url = paste(obj@uri,obj@api,sep="/")
    cmd.string = sub(" ([[:alnum:]]*=)","XXXXXX\\1",cmd.string)
    cmdargs = unlist(strsplit(cmd.string,"XXXXXX"))
    cmd = cmdargs[1]
    if(is.na(cmd)){cmd=""}
    q.cmd = URLencode(paste(base.url, "commands", sub(" ","/",cmd), sep="/"))
    args = cmdargs[2]
    if (is.na(args)){
        q.cmd
    }else{
        args = gsub("\"","",args)
        p = "[[:alnum:]]+="
        m = gregexpr(p,args)
        args1 = unlist(regmatches(args,m))
        args1 = gsub('=','',args1)
        #args1 = unlist(str_extract_all(args,"[[:alnum:]]+(?==)")) # requires stringr lib
        args2 = unlist(strsplit(args," *[[:alnum:]]+="))
        args2 = args2[-1]
        q.args = paste(args1[1],URLencode(args2[1]),sep="=")
        
        for (i in seq(args1)[-1]){
            arg = paste(args1[i],URLencode(args2[i]),sep="=")
            q.args = paste(q.args,arg,sep="&")
        }
        paste(q.cmd,q.args,sep="?")
    }
}

# ------------------------------------------------------------------------------
#' Create an igraph network from a Cytoscape network
#'
#' @description Takes a Cytoscape network and generates data frames for vertices and edges to
#' send to the graph_from_data_frame function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Nodes and edges from the Cytoscape network will be translated into vertices and edges
#' in igraph. Associated table columns will also be passed to igraph as vertiex and edge attributes.
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return (igraph) an igraph network
#' @export
#' @import igraph
#' @examples
#' \donttest{
#' createNetworkFromIgraph(g)
#' }
#' @seealso createNetworkFromDataFrames, createIgraphFromNetwork

createIgraphFromNetwork <- function(title=NA, obj=CytoscapeWindowFromNetwork(), ...){
    
    if(!is.na(title))
        obj<-CytoscapeWindowFromNetwork(title)
    else if(class(obj) == 'CytoscapeConnectionClass'){
        obj<-CytoscapeWindowFromNetwork()
    }
    network = obj@suid
    
    #get dataframes
    cyedges <- getTableColumns('edge',obj=obj)
    cynodes <- getTableColumns('node',obj=obj)
    
    #check for source and target columns
    if(!"source" %in% colnames(cyedges)||(!"target" %in% colnames(cyedges))){
        st=data.frame(do.call('rbind',strsplit(cyedges$name,"\ \\(.*\\)\ ")))
        colnames(st) <- c("source","target")
        cyedges <- cbind(st,cyedges)
    }
    
    #setup columns for igraph construction
    colnames(cyedges)[colnames(cyedges)=="source"]<-"from"
    colnames(cyedges)[colnames(cyedges)=="target"]<-"to"
    cyedges2=cbind(cyedges[c("from","to")], cyedges[ ,!(names(cyedges) %in% c("from","to"))])
    cynodes2=cbind(cynodes["name"], cynodes[ ,!(names(cynodes)=="name")])
    
    #ship
    graph_from_data_frame(cyedges2, directed=TRUE, vertices=cynodes2)
}

# ------------------------------------------------------------------------------
#' Create a Cytoscape network from an igraph network
#'
#' @description Takes an igraph network and generates data frames for nodes and edges to
#' send to the createNetwork function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Vertices and edges from the igraph network will be translated into nodes and edges
#' in Cytoscape. Associated attributes will also be passed to Cytoscape as node and edge table columns.
#' @param igraph (igraph) igraph network object
#' @param new.title (char) network name
#' @param collection.title (char) network collection name
#' @param base.url cyrest base url for communicating with cytoscape
#' @param ... params for nodeSet2JSON() and edgeSet2JSON(); see createNetwork
#' @return (int) network SUID
#' @export
#' @import igraph
#' @examples
#' \donttest{
#' createNetworkFromIgraph(g)
#' }
#' @seealso createNetworkFromDataFrames, createIgraphFromNetwork

createNetworkFromIgraph <- function(igraph, new.title="MyNetwork",
                                    collection.title="myNetworkCollection",return.graph=FALSE, obj=CytoscapeConnection(),...) {
    
    #extract dataframes
    igedges = as_data_frame(igraph, what="edges")
    ignodes = as_data_frame(igraph, what="vertices")
    
    #setup columns for Cytoscape import
    ignodes$id <- row.names(ignodes)
    colnames(igedges)[colnames(igedges)=="from"]<-"source"
    colnames(igedges)[colnames(igedges)=="to"]<-"target"
    
    #ship
    createNetworkFromDataFrames(ignodes,igedges,new.title,collection.title,return.graph,obj)
}

# ------------------------------------------------------------------------------
#' Create a network from data frames
#'
#' @description Takes data frames for nodes and edges, as well as naming parameters to
#' generate the JSON data format required by the "networks" POST operation via CyREST.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details NODES should contain a column named: id. This name can be overridden by
#' the arg: node.id.list. Additional columns are loaded as node attributes.
#' EDGES should contain columns named: source, target and interaction. These names can be overridden by
#' args: source.id.list, target.id.list, interaction.type.list. Additional columns
#' are loaded as edge attributes. The 'interaction' list can contain a single
#' value to apply to all rows; and if excluded altogether, the interaction type
#' wiil be set to "interacts with". NOTE: attribute values of types (num) and (int) will be imported
#' as (Double); (chr) as (String); and (logical) as (Boolean).
#' @param nodes (data.frame) see details and examples below; default NULL to derive nodes from edge sources and targets
#' @param edges (data.frame) see details and examples below; default NULL for disconnected set of nodes
#' @param new.title (char) network name
#' @param collection.title (char) network collection name
#' @param base.url cyrest base url for communicating with cytoscape
#' @param ... params for nodeSet2JSON() and edgeSet2JSON()
#' @return (int) network SUID
#' @export
#' @import RJSONIO
#' @seealso createSubnetwork
#' @examples
#' \donttest{
#' nodes <- data.frame(id=c("node 0","node 1","node 2","node 3"),
#'            group=c("A","A","B","B"), # optional
#'            stringsAsFactors=FALSE)
#' edges <- data.frame(source=c("node 0","node 0","node 0","node 2"),
#'            target=c("node 1","node 2","node 3","node 3"),
#'            interaction=c("inhibits","interacts","activates","interacts"),  # optional
#'            weight=c(5,3,5,9), # optional
#'            stringsAsFactors=FALSE)
#'
#' createNetworkFromDataFrames(nodes,edges)
#' }

createNetworkFromDataFrames <- function(nodes=NULL,edges=NULL,new.title="MyNetwork",
                          collection.title="MyNetworkCollection",return.graph=FALSE, obj=CytoscapeConnection(),...) {
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    #defining variable names to be used globally later on (to avoid devtools::check() NOTES)
    RCy3.CreateNetworkFromDataFrames.temp.global.counter <- NULL
    RCy3.CreateNetworkFromDataFrames.temp.global.size <- NULL
    RCy3.CreateNetworkFromDataFrames.temp.global.json_set <- NULL
    
    if (is.null(nodes)) {
        if (!is.null(edges)) {
            nodes = data.frame(id=c(edges$source,edges$target),stringsAsFactors = FALSE)
        }else
            return("Create Network Failed: Must provide either nodes or edges")
    }
    
    json_nodes <- nodeSet2JSON(nodes,...)
    # cleanup global environment variables (which can be quite large)
    remove(RCy3.CreateNetworkFromDataFrames.temp.global.counter, envir = globalenv())
    remove(RCy3.CreateNetworkFromDataFrames.temp.global.size, envir = globalenv())
    remove(RCy3.CreateNetworkFromDataFrames.temp.global.json_set, envir = globalenv())
    
    json_edges<-c()
    
    if(!is.null(edges)){
        json_edges <- edgeSet2JSON(edges,...)
        # cleanup global environment variables (which can be quite large)
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.counter, envir = globalenv())
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.size, envir = globalenv())
        remove(RCy3.CreateNetworkFromDataFrames.temp.global.json_set, envir = globalenv())
    } else {
        json_edges <- "[]" #fake empty array
    }
    
    json_network <- list(
        data<-list(name=new.title),
        elements<-c(nodes=list(json_nodes),edges=list(json_edges))
    )
    
    network <- toJSON(json_network)
    
    #swap any spaces in names
    new.title <- gsub(" ","%20",new.title)
    collection.title <- gsub(" ","%20",collection.title)
    
    url<- sprintf("%s/networks?title=%s&collection=%s",
                  base.url,new.title,collection.title,sep="")
    
    response <- POST(url=url,body=network, encode="json",content_type_json())
    
    network.suid <- unname(fromJSON(rawToChar(response$content)))
    if(is.numeric(network.suid))
        cat(sprintf("Network SUID is : %i \n", network.suid))
    else
        return(response)
    
    cat("Applying default style\n")
    commandRun('vizmap apply styles="default"',obj)
    
    cat(sprintf("Applying %s layout\n", invisible(commandRun('layout get preferred network="current"'))))
    commandRun('layout apply preferred networkSelected="current',obj)
    
    net.title = getNetworkName(network.suid)
    net.cw<-CytoscapeWindowFromNetwork(net.title,return.graph=return.graph)
    return(net.cw)
}

# Convert edges to JSON format needed for CyRest network creation
#
# @param edge_set (data.frame) Rows contain pairwise interactions.
# @param source.id.list (char) override default list name for source node ids
# @param target.id.list (char) override default list name for target node ids
# @param interaction.type.list (char) override default list name for interaction types
#
edgeSet2JSON <- function(edge_set, source.id.list = 'source',
                         target.id.list = 'target', interaction.type.list='interaction',...){
    
    #using global environment variables for performance
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter<-0
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size<-1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set<-c()
    #json_edges <- c()
    
    if(!(interaction.type.list %in% names(edge_set)))
        edge_set[,interaction.type.list] = rep('interacts with')
    
    computed_name <- paste(edge_set[,source.id.list], paste('(',edge_set[,interaction.type.list],')',sep=''),
                           edge_set[,target.id.list],sep=" ")
    
    for(i in 1:dim(edge_set)[1]){
        rest <- list()
        rest[["name"]] = computed_name[i]
        for(j in 1:dim(edge_set)[2]){
            rest[[colnames(edge_set)[j]]] = edge_set[i,j]
        }
        current_edge = list("data"=rest)
        #json_edges[[i]] <- current_edge
        FastAppendListGlobal(current_edge)
    }
    return(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[1:.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter])
}

# Creates a table of nodes to CyREST JSON
#
# @param node.set (data.frame) each row is a node and columns contain node attributes
# @param node.id.list (char) override default list name for node ids
# Adapted from Ruth Isserlin's CellCellINteractions_utility_functions.R
nodeSet2JSON <- function(node.set, node.id.list='id',...){
    
    #using global environment variables for performance
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter<-0
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size<-1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set<-c()
    #json_nodes <- c()
    
    for(i in 1:dim(node.set)[1]){
        rest <- list()
        for(j in 1:dim(node.set)[2]){
            rest[[colnames(node.set)[j]]] = node.set[i,j]
        }
        current_node = list("data"=rest)
        #json_nodes[[i]] <- current_node
        FastAppendListGlobal(current_node)
    }
    return(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[1:.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter])
}

# FastAppendListGlobal
# Appends lists at high performance using global variables explictly
#  Note: relies on managing gloval environment variables: initializing and removing
#  https://stackoverflow.com/questions/17046336/here-we-go-again-append-an-element-to-a-list-in-r
#
FastAppendListGlobal <- function(item)
{
    if( .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter == .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size )
        length(.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set) <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.size * 2
    
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter <- .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter + 1
    .GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.json_set[[.GlobalEnv$RCy3.CreateNetworkFromDataFrames.temp.global.counter]] <- item
}

# ------------------------------------------------------------------------------
#' @title Create subnetwork from existing network
#'
#' @description Copies a subset of nodes and edges into a newly created subnetwork.
#' @details If you specify both nodes and edges, the resulting subset will be the union of those sets.
#' Typical usage only requires specifying either nodes or edges. Note that selected nodes will bring
#' along their connecting edges by default (see exclude.edges arg) and selected edges will always
#' bring along their source and target nodes.
#' @param nodes list of node names or keyword: selected, unselected or all
#' @param nodes.by.col name of node table column corresponding to provided nodes list; default is 'name'
#' @param edges list of edge names or keyword: selected, unselected or all
#' @param edges.by.col name of edge table column corresponding to provided edges list; default is 'name'
#' @param exclude.edges (boolean) whether to exclude connecting edges; default is FALSE
#' @param new.title name of new subnetwork to be created;
#' default is to add a numbered suffix to source network name
#' @param network name or suid of the source network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return SUID of new subnetwork
#' @export
#' @examples
#' \donttest{
#' createSubnetwork("selected")
#' createSubnetwork("selected",new.title="mySubnetwork")
#' createSubnetwork(c("node 1","node 2","node 3"))
#' createSubnetwork(c("AKT1","TP53","PIK3CA"),"display name")
#' createSubnetwork(edges="all") #subnetwork of all connected nodes
#' }
#' @seealso createNetworkFromDataFrames

createSubnetwork <- function(nodes,nodes.by.col='name',edges,edges.by.col='name',
                             exclude.edges='F',new.title, return.graph=FALSE, obj=CytoscapeWindowFromNetwork()){
    base.url=paste(obj@uri,obj@api,sep = "/")
    network=obj@title
    
    if(exclude.edges){
        exclude.edges = "true"
    } else {
        exclude.edges = "false"
    }
    
    json_sub=NULL
    json_sub$source=network
    json_sub$excludeEdges=exclude.edges
    
    node.str = NULL
    if(missing(nodes)){
        json_sub$nodeList="selected" #need something here for edge selections to work
    } else {
        if(!nodes[1] %in% c('all','selected','unselected')){
            for (n in nodes){
                if(is.null(node.str))
                    node.str = paste(nodes.by.col,n,sep=":")
                else
                    node.str = paste(node.str,paste(nodes.by.col,n,sep=":"),sep=",")
            }
        } else {
            node.str = nodes
        }
        json_sub$nodeList=node.str
    }
    
    edge.str = NULL
    if(!missing(edges)){
        if(!edges[1] %in% c('all','selected','unselected')){
            for (e in edges){
                if(is.null(edge.str))
                    edge.str = paste(edges.by.col,e,sep=":")
                else
                    edge.str = paste(edge.str,paste(edges.by.col,e,sep=":"),sep=",")
            }
        } else {
            edge.str = edges
        }
        json_sub$edgeList=edge.str
    }
    
    subnetwork.arg = NULL
    if(!missing(new.title)){
        json_sub$networkName=new.title
    }
    
    sub <- toJSON(as.list(json_sub))
    url<- sprintf("%s/commands/network/create", base.url,sep="")
    response <- POST(url=url,body=sub, encode="json",content_type_json())
    subnetwork.suid=unname(fromJSON(rawToChar(response$content)))[[1]][[1]]
    
    sub.title = getNetworkName(subnetwork.suid)
    sub.cw<-CytoscapeWindowFromNetwork(sub.title,return.graph=return.graph)
    return(sub.cw)
}

# ------------------------------------------------------------------------------
#' Create a visual style from components
#'
#' @description Creates a style from defaults and predefined mappings.
#' @details Requires attribute mappings to be previously created, see mapVisualProperty.
#' @param style.name (char) name for style
#' @param defaults (list) key-value pairs for default mappings.
#' @param mappings (list) visual property mappings, see mapVisualProperty
#' @param base.url cyrest base url for communicating with cytoscape
#' @return None
#' @export
#' @import RJSONIO
#' @import httr
#' @examples
#' \donttest{
#' #first there has to be a network to apply style to
#' example(createNetworkFromDataFrames)
#'
#' #then prepare style variables
#' style.name = "myStyle"
#' defaults <- list(NODE_SHAPE="diamond",
#'                  NODE_SIZE=30,
#'                  EDGE_TRANSPARENCY=120,
#'                  NODE_LABEL_POSITION="W,E,c,0.00,0.00")
#' nodeLabels <- mapVisualProperty('node label','id','p')
#' nodeFills <- mapVisualProperty('node fill color','group','d',c("A","B"), c("#FF9900","#66AAAA"))
#' arrowShapes <- mapVisualProperty('Edge Target Arrow Shape','interaction','d',
#'                                  c("activates","inhibits","interacts"),c("Arrow","T","None"))
#' edgeWidth <- mapVisualProperty('edge width','weight','p')
#'
#' #and then create the style
#' createVisualStyle(style.name, defaults, list(nodeLabels,nodeFills,arrowShapes,edgeWidth))
#'
#' #finsh by applying the style
#' example(applyStyle)
#' }
#' @seealso applyStyle, mapVisualProperty

createVisualStyle <- function(style.name, defaults, mappings, obj=CytoscapeConnection()) {
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    if(missing(mappings))
        mappings <- list()
    
    styleDef <- list()
    if(!missing(defaults)){
        for (i in 1:length(defaults)) {
            styleDef[[i]] <- list(visualProperty=names(defaults)[i], value=defaults[[i]])
        }
    }
    style <- list(title=style.name, defaults=styleDef,mappings=mappings)
    jsonStyle <- toJSON(style)
    style.url <- paste(base.url,'styles', sep = '/')
    invisible(POST(url=style.url,body=jsonStyle, encode="json"))
}

# ------------------------------------------------------------------------------
#' Get the name of a network
#'
#' @param network.suid SUID of the network; default is current network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return network viewid
#' @export
#' @examples
#' \donttest{
#' getNetworkName()
#' getNetworkName(1111)
#' }

getNetworkName <- function(network.suid=NA, obj=CytoscapeConnection()){
    base.url = paste(obj@uri,obj@api,sep="/")
    if(is.na(network.suid))
        network.suid = getNetworkSuid(obj)
    
    url <- paste0(base.url,"/networks.names?column=suid&query=",network.suid)
    response <- GET(url=url)
    network.name <- unname(fromJSON(rawToChar(response$content)))
    network.name <- network.name[[1]]$name
    return(network.name)
}

# ------------------------------------------------------------------------------
#' Get table column values
#'
#' @details The 'name' column is always retrieved along with specified columns. The 'name' values are used
#' as row names in the returned data frame. Note that this function fails on columns with missing values.
#' @param table name of table, e.g., node, edge, network
#' @param columns names of columns to retrieve values from as list object or comma-separated list; default is all columns
#' @param namespace namespace of table; default is "default"
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return data frame of column values
#' @export
#' @import httr
#' @import RJSONIO
#' @importFrom utils URLencode
#' @examples
#' \donttest{
#' example(createNetworkFromDataFrames)
#'
#' getTableColumns('node','group')
#' }

getTableColumns<-function(table,columns=NULL,namespace='default',obj=CytoscapeWindowFromNetwork()){
    
    base.url = paste(obj@uri,obj@api,sep="/")
    
    #all columns
    if(is.null(columns))
        columns = listTableColumns(table, namespace, obj=obj)
    
    #handle comma separated lists and list objects
    col.list = columns
    if(length(col.list)==1)
        col.list = unlist(strsplit(columns, ","))
    
    #get name column first
    tbl = paste0(namespace,table)
    cmd = paste(base.url,'networks',obj@suid,'tables',tbl,'columns','name',sep = '/')
    res = GET(URLencode(cmd))
    res.html = htmlParse(rawToChar(res$content), asText=TRUE)
    res.elem = xpathSApply(res.html, "//p", xmlValue)
    names <- fromJSON(res.elem[1])
    df = data.frame(row.names=names$values)
    
    #retrieve column names
    table.col.list = listTableColumns(table,namespace,obj)
    
    # then append other requested columns
    for (col in col.list){
        
        #check for column names
        if(!col %in% table.col.list){
            cat(sprintf("Error: Column %s not found in %s table \n",col,table))
            next()
        }
        
        cmd = paste(base.url,'networks',obj@suid,'tables',tbl,'columns',col,sep = '/')
        res = GET(URLencode(cmd))
        res.html = htmlParse(rawToChar(res$content), asText=TRUE)
        res.elem = xpathSApply(res.html, "//p", xmlValue)
        col.vals <- fromJSON(res.elem[1])
        #convert NULL to NA, then unlist
        cvv <- unlist(lapply(col.vals$values, function(x) ifelse(is.null(x),NA,x)))
        if(length(names$values)==length(cvv)){
            for(i in 1:length(names$values)){
                df[i,col] <- cvv[i]
            }
        } else {
            print("Warning: Requested column has missing values. Returning single column without row.names...")
            df2 = data.frame(col=unlist(col.vals$values))
            return(df2)
        }
    }
    
    return(df)
}

# ------------------------------------------------------------------------------
#' List names of all columns in a table
#'
#' @param table name of table, e.g., node, edge, network; default is "node"
#' @param namespace namespace of table, e.g., default
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return list of column names
#' @export
#' @import httr
#' @import RJSONIO
#' @examples
#' \donttest{
#' listTableColumns()
#' listTableColumns('edge')
#' listTableColumns('network')
#' }

listTableColumns<-function(table='node',namespace='default',obj=CytoscapeWindowFromNetwork()){
    cmd = paste(table,' list attributes network="',obj@title,'" namespace="',namespace,sep='')
    return(commandRun(cmd,obj))
}

# ------------------------------------------------------------------------------
#' Loads data into Cytoscape tables keyed by row
#'
#' @description This function loads data into Cytoscape node/edge/network tables provided a
#' common key, e.g., name. Data.frame column names will be used to set Cytoscape table column
#' names.
#' @details Numeric (or integer) values will be stored as Doubles in Cytoscape tables.
#' Character or mixed values will be stored as Strings. Logical values will be stored as Boolean.
#' Existing columns with the same names will be overwritten.
#' @param data (data.frame) each row is a node and columns contain node attributes
#' @param data.key.column (char) name of data.frame column to use as key; default is "row.names"
#' @param table (char) name of Cytoscape table to load data into, e.g., node, edge or network; default is "node"
#' @param table.key.column (char) name of Cytoscape table column to use as key; default is "name"
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export

loadTableData<-function(data, data.key.column='row.names', table='node', table.key.column='name',
                        obj=CytoscapeWindowFromNetwork()){
    
    base.url = paste(obj@uri,obj@api,sep="/")
    
    table.key.column.values =  getTableColumns(table=table,columns = table.key.column, obj=obj)
    
    ##ERROR if table.key.column.values is empty
    if(ncol(table.key.column.values)==0)
        return("Failed to load data: Please check table.key.column")
    
    if(data.key.column=='row.names')  # if key in row.names...
        data$row.names<-row.names(data)  # then copy to new "row.names" column :)
    
    ##ERROR if data.key.column not in data
    if (!data.key.column %in% colnames(data))
        return("Failed to load data: Please check data.key.column")
    
    filter = data[,data.key.column] %in% table.key.column.values[,1]
    
    ##ERROR if filter is empty
    if(!TRUE %in% filter)
        return("Failed to load data: Provided key columns do not contain any matches")
    
    data.subset = data[filter,]
    
    #remove troublesome factors
    if(class(data.subset[,data.key.column])=="factor")
        data.subset[,data.key.column] = levels(droplevels(data.subset[,data.key.column]))
    
    data.list <- c()
    for(i in 1:dim(data.subset)[1]){  #each rows
        rest <- list()
        for(j in 1:dim(data.subset)[2]){  #each column
            rest[[colnames(data.subset)[j]]] = data.subset[i,j]
        }
        data.list[[i]] <- rest
    }
    
    table = paste0("default",table) #add prefix
    
    tojson.list <- list(key=table.key.column,dataKey=data.key.column,data=data.list)
    jsonData <- toJSON(tojson.list)
    data.url <- paste(base.url,'networks',obj@suid,"tables",table, sep = '/')
    
    PUT(url=data.url,body=jsonData, encode="json")
    return(sprintf("Success: Data loaded in %s table", table))
}

# ------------------------------------------------------------------------------
#' Creates a mapping between an attribute and a visual property
#'
#' @description Generates the appropriate data structure for the "mapping" parameter
#' in setStyleMappings and createStyle.
#' @details The paired list of values must be of the same length or mapping will fail.
#' Mapping will also fail if the data type of table.column.values does not match that of
#' the existing table.column. Note that all imported numeric data are stored as Doubles in
#' Cytosacpe tables; and character or mixed data are stored as Strings.
#' @param visual.prop (char) name of visual property to map
#' @param table.column (char) name of table column to map
#' @param mapping.type (char) continuous, discrete or passthrough (c,d,p)
#' @param table.column.values (list) list of values paired with visual.prop.values; skip for passthrough mapping
#' @param visual.prop.values (list) list of values paired with table.column.values; skip for passthrough mapping
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return (obj) ready to convert into JSON by style mapping operations
#' @export
#' @seealso setStyleMappings createStyle
#' @examples
#' \donttest{
#' mapVisualProperty('node fill color','score','c',c(-4.0,0.0,9.0),c('#99CCFF','#FFFFFF','#FF7777'))
#' mapVisualProperty('node shape','type','d',c('protein','metabolite'),c('ellipse','rectangle'))
#' mapVisualProperty('node label','alias','p')
#' }
#' @section List of visual properties:
#' \tabular{lll}{
#' Node Border Line Type \tab Edge Bend \tab Network Background Paint \cr
#' Node Border Paint \tab Edge Curved \tab Network Center X Location \cr
#' Node Border Transparency \tab Edge Label \tab Network Center Y Location \cr
#' Node Border Width \tab Edge Label Color \tab Network Center Z Location \cr
#' Node CustomGraphics 1-9 \tab Edge Label Font Face \tab Network Depth \cr
#' Node CustomGraphics Position 1-9 \tab Edge Label Font Size \tab Network Edge Selection \cr
#' Node CustomGraphics Size 1-9 \tab Edge Label Transparency \tab Network Height \cr
#' Node CustomPaint 1-9 \tab Edge Label Width \tab Network Node Selection \cr
#' Node Depth \tab Edge Line Type \tab Network Scale Factor \cr
#' Node Fill Color \tab Edge Paint \tab Network Size \cr
#' Node Height \tab Edge Selected \tab Network Title \cr
#' Node Label \tab Edge Selected Paint \tab Network Width \cr
#' Node Label Color \tab Edge Source Arrow Selected Paint \tab  \cr
#' Node Label Font Face \tab Edge Source Arrow Shape \tab  \cr
#' Node Label Font Size \tab Edge Source Arrow Size \tab  \cr
#' Node Label Position \tab Edge Source Arrow Unselected Paint \tab  \cr
#' Node Label Transparency \tab Edge Stroke Selected Paint \tab  \cr
#' Node Label Width \tab Edge Stroke Unselected Paint \tab  \cr
#' Node Network Image Visible \tab Edge Target Arrow Selected Paint \tab  \cr
#' Node Paint \tab Edge Target Arrow Shape \tab  \cr
#' Node Selected \tab Edge Target Arrow Size \tab  \cr
#' Node Selected Paint \tab Edge Target Arrow Unselected Paint \tab  \cr
#' Node Shape \tab Edge Tooltip \tab  \cr
#' Node Size \tab Edge Transparency \tab  \cr
#' Node Tooltip \tab Edge Unselected Paint \tab  \cr
#' Node Transparency \tab Edge Visible \tab  \cr
#' Node Visible \tab Edge Visual Property \tab  \cr
#' Node Width \tab Edge Width \tab  \cr
#' Node X Location \tab  \tab  \cr
#' Node Y Location \tab  \tab  \cr
#' Node Z Location \tab  \tab  \cr
#' }

mapVisualProperty <- function(visual.prop, table.column, mapping.type, table.column.values,
                              visual.prop.values, obj=CytoscapeWindowFromNetwork()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    #process mapping type
    mapping.type.name = switch(mapping.type, 'c'='continuous','d'='discrete','p'='passthrough',mapping.type)
    
    #processs visual property, including common alternatives for vp names :)
    visual.prop.name = toupper(gsub("\\s+","_",visual.prop))
    visual.prop.name = switch(visual.prop.name,
                              'EDGE_COLOR'='EDGE_STROKE_UNSELECTED_PAINT',
                              'EDGE_THICKNESS'='EDGE_WIDTH',
                              'NODE_BORDER_COLOR'='NODE_BORDER_PAINT',
                              visual.prop.name)
    
    #check mapping column and get type
    tp = tolower(strsplit(visual.prop.name,"_")[[1]][1])
    table = paste0('default',tp)
    t.url = paste(base.url,'networks',obj@suid,'tables',table,'columns',sep='/')
    res <- GET(url=t.url)
    t.res <- unname(fromJSON(rawToChar(res$content)))
    table.column.type = NULL
    for(i in 1:length(t.res)){
        if(t.res[[i]]$name==table.column){
            table.column.type = t.res[[i]]$type
            break
        }
    }
    if(is.null(table.column.type))
        print(paste0('Error: Could not find ',table.column,' column in ',table,' table of network: ',obj@title,'.'))
    
    #construct visual property map
    visual.prop.map <- list(
        mappingType=mapping.type.name,
        mappingColumn=table.column,
        mappingColumnType=table.column.type,
        visualProperty=visual.prop.name
    )
    
    if(mapping.type.name=='discrete'){
        map <- list()
        for (i in 1:length(table.column.values)) {
            map[[i]] <- list(key=table.column.values[i], value=visual.prop.values[i])
        }
        visual.prop.map$map=map
    }else if(mapping.type.name=='continuous'){
        points <- list()
        for (i in 1:length(table.column.values)) {
            points[[i]] <- list(value=table.column.values[i],
                                lesser=visual.prop.values[i],
                                equal=visual.prop.values[i],
                                greater=visual.prop.values[i])
        }
        visual.prop.map$points=points
    }
    
    return(visual.prop.map)
}

# ------------------------------------------------------------------------------
#' Open CySwagger docs in browser
#'
#' @description Opens swagger docs in default browser for a live
#' instance of CyREST or CyREST-supported operations.
#' @param domain (char) documentation domain or scope
#' @param base.url cyrest base url for communicating with cytoscape
#' @return Web page
#' @export
#' @examples
#' \donttest{
#' openCySwagger()
#' openCySwagger('commands')
#' }
#' @importFrom utils browseURL

openCySwagger<-function(domain='cyrest', obj=CytoscapeConnection()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    if(domain=='cyrest'){
        domain = ''
    }else{
        domain = paste('/',domain,sep='')
    }
    browseURL(paste(base.url,'/swaggerUI/swagger-ui/index.html?url=',base.url,domain,'/swagger.json#/',sep=""))
}

# ------------------------------------------------------------------------------
#' Saves the current Cytoscape session
#'
#' @details Saves session as a CYS file.
#' @param filename (char) name of the session file to save
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @examples
#' \donttest{
#' saveSession('myFirstSession')
#' }

saveSession<-function(filename,obj=CytoscapeConnection()){
    commandRun(paste0('session save as file="',filename,'"'),obj)
}

# ------------------------------------------------------------------------------
#' Saves the current visual style as a data file
#'
#' @param filename (char) name of the style file to save
#' @param type (char) type of data file to export, e.g., XML, JSON (case sensitive)
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @examples
#' \donttest{
#' saveVisualStyle('myStyle','JSON')
#' }

saveVisualStyle<-function(filename,type,obj=CytoscapeConnection()){
    commandRun(paste0('vizmap export options=',type,' OutputFile="',filename,'"'),obj)
}

# ------------------------------------------------------------------------------
#' @title Select first neighbor nodes
#' @description Select nodes directly connected to currently selected nodes. Can
#' specify connection directionality using the direction param.
#' @param direction direction of connections to neighbors to follow, e.g., incoming, outgoing, undirected, or any (default)
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return list of names of selected nodes, including original selection
#' @export
#' @import RJSONIO
#' @import httr
#' @examples
#' \donttest{
#' selectFirstNeighbors()
#' selectFirstNeighors('outgoing')
#' selectFirstNeighors('incoming')
#' }

selectFirstNeighbors <- function(direction='any', obj=CytoscapeWindowFromNetwork()){
    network = obj@title
    cmd<-paste0('network select firstNeighbors="',direction,'" network="',network,'"')
    res <- commandRun(cmd,obj)
    return(res[-1])
}

# ------------------------------------------------------------------------------
#' @title Set current network
#'
#' @description Selects the given network as "current"
#' @param network name or suid of the network that you want set as current
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @examples
#' \donttest{
#' setCurrentNetwork('MyNetwork')
#' }

setCurrentNetwork <- function(obj=CytoscapeConnection(),title=NA){

    if(!is.na(title))
        network = title
    else if(class(obj) == 'CytoscapeWindowClass')
        network = obj@title
    else { # a CyConn was provided, but no title, so there is nothing to do 
        write("Neither a network title nor a Cytoscape Window was provided. No action performed.")
        stop()
    }

    cmd<-paste0('network set current network="',network,'"')
    res <- commandRun(cmd,obj)
    return(res)
}

# ------------------------------------------------------------------------------
#' Updates the default values of visual properties in a style
#'
#' @description Updates visual property defaults, overriding any prior settings. See mapVisualProperty for
#' the list of visual properties.
#' @param style.name (char) name for style
#' @param defaults (list) a list of visual property default settings, see mapVisualProperty
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @examples
#' \donttest{
#' updateStyleDefaults('myStyle',list('node fill color'='#0000FF','node size'=50))
#' }
#' @export
#' @seealso mapVisualProperty

updateStyleDefaults <- function(style.name,defaults,obj=CytoscapeConnection()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    def.list <- list()
    for (i in 1:length(defaults)) {
        visual.prop.name <- names(defaults)[i]
        visual.prop.name = toupper(gsub("\\s+","_",visual.prop.name))
        visual.prop.name = switch(visual.prop.name,
                                  'EDGE_COLOR'='EDGE_STROKE_UNSELECTED_PAINT',
                                  'EDGE_THICKNESS'='EDGE_WIDTH',
                                  'NODE_BORDER_COLOR'='NODE_BORDER_PAINT',
                                  visual.prop.name)
        def.list[[i]] <- list(visualProperty=visual.prop.name,
                              value=defaults[[i]])
    }
    
    style.url <- URLencode(paste(base.url,'styles', style.name,'defaults', sep = '/'))
    map.body <- toJSON(def.list)
    invisible(PUT(url=style.url,body=map.body, encode="json"))
    
}

# ------------------------------------------------------------------------------
#' Updates the values of dependencies in a style
#'
#' @description Updates style dependencies, overriding any prior settings.
#' @param style.name (char) name for style
#' @param dependencies (list) a list of style dependencies, see list below. Note:
#' each dependency is set by a boolean, TRUE or FALSE (T or F)
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @examples
#' \donttest{
#' updateStyleDependencies("myStyle",list(nodeSizeLocked=TRUE))
#' }
#' @export
#' @section List of Dependencies:
#' arrowColorMatchesEdge
#' nodeCustomGraphicsSizeSync
#' nodeSizeLocked

updateStyleDependencies <- function(style.name,dependencies,obj=CytoscapeConnection()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    dep.list <- list()
    for (i in 1:length(dependencies)) {
        dep.list[[i]] <- list(visualPropertyDependency=names(dependencies)[i],
                              enabled=dependencies[[i]])
    }
    
    style.url <- URLencode(paste(base.url,'styles', style.name,'dependencies', sep = '/'))
    map.body <- toJSON(dep.list)
    
    cat("PUT-ing style\n")
    invisible(PUT(url=style.url,body=map.body, encode="json"))
    invisible(commandRun(paste('vizmap apply styles',style.name,sep='=')))
}

# ------------------------------------------------------------------------------
#' Updates a visual property mapping in a style
#'
#' @description Updates the visual property mapping, overriding any prior mapping. Creates a
#' visual property mapping if it doesn't already exist in the style.
#' @details Requires visual property mappings to be previously created, see mapVisualProperty.
#' @param style.name (char) name for style
#' @param mapping a single visual property mapping, see mapVisualProperty
#' @param base.url cyrest base url for communicating with cytoscape
#' @return server response
#' @export
#' @seealso mapVisualProperty
#' @examples
#' \donttest{
#' updateStyleMapping('myStyle',mapVisualProperty('node label','name','p'))
#' }
#' @import RJSONIO
#' @import httr

updateStyleMapping <- function(style.name, mapping, obj=CytoscapeConnection()){
    
    base.url=paste(obj@uri,obj@api,sep = "/")
    
    visual.prop.name = mapping$visualProperty
    
    # check if vp exists already
    exists = FALSE
    check.url <- URLencode(paste(base.url,'styles', style.name,'mappings',sep = '/'))
    res <- GET(check.url)
    res.elem <- fromJSON(rawToChar(res$content))
    for(re in res.elem){
        if(class(re)=='list')
            if(!is.null(re$visualProperty))
                if(re$visualProperty==visual.prop.name)
                    exists = TRUE
    }
    
    if(exists==TRUE){     #if yes...
        style.url <- URLencode(paste(base.url,'styles', style.name,'mappings',visual.prop.name, sep = '/'))
        map.body <- toJSON(list(mapping))
        invisible(PUT(url=style.url,body=map.body, encode="json"))
        invisible(PUT(url=style.url,body=map.body, encode="json"))    # have to run it twice!? omg...
    }
    else {    # if no...
        style.url <- check.url
        map.body <- toJSON(list(mapping))
        invisible(POST(url=style.url,body=map.body, encode="json"))
    }
}

######################
# DEPRECATED
######################
copyCytoscapeNetwork<-function(obj,new.title,return.graph = FALSE) {
    .Deprecated("cloneNetwork")
    cloneNetwork(obj=obj,new.title=new.title,return.graph=return.graph)
}
createWindow<-function(obj) {
    .Deprecated("sendNetworkFromGraph")
    sendNetworkFromGraph(obj=obj)
}
createWindowFromSelection<-function(obj,new.windowTitle,return.graph){
    .Deprecated("createNetworkFromSelection(obj, new.title, return.graph, exclude.edges=FALSE)")
    createNetworkFromSelection(obj=obj, new.title=new.windowTitle, return.graph=return.graph, exclude.edges=FALSE)
}
deleteAllWindows<-function(obj){
    .Deprecated("deleteAllNetworks(obj)")
    deleteAllNetworks(obj=obj)
}
deleteWindow<-function(obj,window.title){
    .Deprecated("deleteNetwork(obj,title)")
    deleteNetwork(obj=obj,title=window.title)
}
existing.CytoscapeWindow<-function(title, host='localhost', port=1234, copy.graph.from.cytoscape.to.R=FALSE){
    .Deprecated("CytoscapeWindowFromNetwork(title,host,port,return.graph")
    cc<-CytoscapeConnection(host=host,port=port)
    CytoscapeWindowFromNetwork(cc,title = title,return.graph = copy.graph.from.cytoscape.to.R)
}
getGraphFromCyWindow<-function(obj,window.title){
    .Deprecated("getGraphFromNetwork(obj,title)")
    getGraphFromNetwork(obj=obj,title=window.title)
}
getWindowCount<-function(obj){
    .Deprecated("getNetworkCount")
    getNetworkCount(obj=obj)
}
getWindowID<-function(obj,window.title){
    .Deprecated("getNetworkSuid(obj,title")
    getNetworkSuid(obj=obj,title=window.title)
}
getWindowList<-function(obj){
    .Deprecated("getNetworkList")
    getNetworkList(obj=obj)
}
pluginVersion<-function(obj){
    .Deprecated("apiVersion(obj)")
    apiVersion(obj=obj)
}
renameCytoscapeNetwork<-function(obj, new.title, return.graph = FALSE) {
    .Deprecated("renameNetwork")
    renameNetwork(obj=obj,new.title=new.title,return.graph=return.graph)
}
selectFirstNeighborsOfSelectedNodes<-function (obj) {
    selectFirstNeighbors(obj=obj)
}
sfn<-function (obj) {
    selectFirstNeighbors(obj=obj)
}
sendNodes<-function(obj){
    .Deprecated("sendNodesFromGraph(obj)")
    sendNodesFromGraph(obj=obj)
}
sendEdges<-function(obj){
    .Deprecated("sendEdgesFromGraph(obj)")
    sendEdgesFromGraph(obj=obj)
}
setNodeAttributes<-function(obj, attribute.name){
    .Deprecated("sendNodeAttributesFromGraph(obj)")
    sendNodeAttributesFromGraph(obj=obj, attribute.name=attribute.name)
}
setEdgeAttributes<-function(obj, attribute.name){
    .Deprecated("sendEdgeAttributesFromGraph(obj)")
    sendEdgeAttributesFromGraph(obj=obj, attribute.name=attribute.name)
}

