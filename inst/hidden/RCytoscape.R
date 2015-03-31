library (graph)
library (XMLRPC)
library (methods)


# this code is for the Bioconductor build system. You should never need to set or
# read these environment variables in ordinary use.
.BBSOverride <- function(host, rpcPort) {
    ret <- list()
    if ((Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE") != "")) {
      host = Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE")
      rpcPort = as(Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE"),"integer")
      }
    if (.Platform$r_arch == "x64") {
        if ((Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE_64") != "") &&  (Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE_64") != "")) {
          host = Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE_64")
          rpcPort = as(Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE_64"),"integer")
          }
    }
    #cat(paste("Using host", host, "and port", rpcPort, "."))
    
    ret["host"] <- host
    ret["rpcPort"] <- rpcPort
    ret
}

#------------------------------------------------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
#------------------------------------------------------------------------------------------------------------------------
#setClassUnion ("biocGraph", c ("graph", "MultiGraph"))
#setClassUnion ("biocGraph", c ("graph", "graphNEL", "graphAM", "distGraph", "clusterGraph", "graphBAM", "MultiGraph"))


setClass ("CytoscapeConnectionClass", 
          representation = representation (uri="character"),
          prototype = prototype (uri="http://localhost:9000")
          )

#------------------------------------------------------------------------------------------------------------------------
setClass ("CytoscapeWindowClass", 
          representation = representation (title="character",
                                           window.id='character',
                                           graph="graphBase"),
          contains='CytoscapeConnectionClass',
          prototype = prototype (title="R graph", 
                                 graph=new ("graphNEL", edgemode='directed'), 
                                 uri="http://localhost:9000")
          )


#------------------------------------------------------------------------------------------------------------------------
setGeneric ('ping',                     signature='obj', function (obj) standardGeneric ('ping'))
setGeneric ('pluginVersion',            signature='obj', function (obj) standardGeneric ('pluginVersion'))
setGeneric ('msg',                      signature='obj', function (obj, string) standardGeneric ('msg'))
setGeneric ('clearMsg',                 signature='obj', function (obj) standardGeneric ('clearMsg'))
setGeneric ('createWindow',             signature='obj', function (obj) standardGeneric ('createWindow'))
setGeneric ('createWindowFromSelection', signature='obj', function (obj, new.windowTitle, return.graph) standardGeneric ('createWindowFromSelection'))
setGeneric ('getWindowCount',           signature='obj', function (obj) standardGeneric ('getWindowCount'))
setGeneric ('getWindowList',            signature='obj', function (obj) standardGeneric ('getWindowList'))
setGeneric ('deleteWindow',             signature='obj', function (obj, window.title=NA) standardGeneric ('deleteWindow'))
setGeneric ('deleteAllWindows',         signature='obj', function (obj) standardGeneric ('deleteAllWindows'))
setGeneric ('getArrowShapes',           signature='obj', function (obj) standardGeneric ('getArrowShapes'))
setGeneric ('getLayoutNames',           signature='obj', function (obj) standardGeneric ('getLayoutNames'))
setGeneric ('getLayoutNameMapping',     signature='obj', function (obj) standardGeneric ('getLayoutNameMapping'))
setGeneric ('getLayoutPropertyNames',   signature='obj', function (obj, layout.name) standardGeneric ('getLayoutPropertyNames'))
setGeneric ('getLayoutPropertyType',    signature='obj', function (obj, layout.name, property.name) standardGeneric ('getLayoutPropertyType'))
setGeneric ('getLayoutPropertyValue',   signature='obj', function (obj, layout.name, property.name) standardGeneric ('getLayoutPropertyValue'))
setGeneric ('setLayoutProperties',      signature='obj', function (obj, layout.name, properties.list) standardGeneric ('setLayoutProperties'))
setGeneric ('getLineStyles',            signature='obj', function (obj) standardGeneric ('getLineStyles'))
setGeneric ('getNodeShapes',            signature='obj', function (obj) standardGeneric ('getNodeShapes'))
setGeneric ('getDirectlyModifiableVisualProperties', 
                                        signature='obj', function (obj) standardGeneric ('getDirectlyModifiableVisualProperties'))
setGeneric ('getAttributeClassNames',   signature='obj', function (obj) standardGeneric ('getAttributeClassNames'))
setGeneric ('setGraph',                 signature='obj', function (obj, graph) standardGeneric ('setGraph'))
setGeneric ('getGraph',                 signature='obj', function (obj) standardGeneric ('getGraph'))
setGeneric ('sendNodes',                signature='obj', function (obj) standardGeneric ('sendNodes'))
setGeneric ('sendEdges',                signature='obj', function (obj) standardGeneric ('sendEdges'))

setGeneric ('addCyNode',                signature='obj', function (obj, nodeName) standardGeneric ('addCyNode'))
setGeneric ('addCyEdge',                signature='obj', function (obj, sourceNode, targetNode, edgeType, directed) standardGeneric ('addCyEdge'))
setGeneric ('addGraphToGraph',          signature='obj', function (obj, other.graph) standardGeneric ('addGraphToGraph'))

setGeneric ('setNodeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('setNodeAttributes'))
setGeneric ('setNodeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, node.names, values) standardGeneric ('setNodeAttributesDirect'))

setGeneric ('setEdgeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('setEdgeAttributes'))
setGeneric ('setEdgeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, edge.names, values) standardGeneric ('setEdgeAttributesDirect'))

setGeneric ('displayGraph',             signature='obj', function (obj) standardGeneric ('displayGraph'))
setGeneric ('layoutNetwork',            signature='obj', function (obj, layout.name='jgraph-spring') standardGeneric ('layoutNetwork'))
setGeneric ('saveLayout',               signature='obj', function (obj, filename, timestamp.in.filename=FALSE) standardGeneric ('saveLayout'))
setGeneric ('restoreLayout',            signature='obj', function (obj, filename) standardGeneric ('restoreLayout'))
setGeneric ('setNodePosition',          signature='obj', function (obj, node.names, x.coords, y.coords) standardGeneric ('setNodePosition'))
setGeneric ('getNodePosition',          signature='obj', function (obj, node.names) standardGeneric ('getNodePosition'))
setGeneric ('getNodeSize',              signature='obj', function (obj, node.names) standardGeneric ('getNodeSize'))
setGeneric ('redraw',                   signature='obj', function (obj) standardGeneric ('redraw'))
setGeneric ('hidePanel',                signature='obj', function (obj, panelName) standardGeneric ('hidePanel'))
setGeneric ('hideAllPanels',            signature='obj', function (obj) standardGeneric ('hideAllPanels'))
setGeneric ('dockPanel',                signature='obj', function (obj, panelName) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',               signature='obj', function (obj, panelName) standardGeneric ('floatPanel'))

setGeneric ('setTooltipInitialDelay',   signature='obj', function (obj, msecs) standardGeneric ('setTooltipInitialDelay'))
setGeneric ('setTooltipDismissDelay',   signature='obj', function (obj, msecs) standardGeneric ('setTooltipDismissDelay'))

setGeneric ('raiseWindow',              signature='obj', function (obj, window.title=NA) standardGeneric ('raiseWindow'))
setGeneric ('setWindowSize',            signature='obj', function (obj, width, height) standardGeneric ('setWindowSize'))
setGeneric ('showGraphicsDetails',      signature='obj', function (obj, new.value) standardGeneric ('showGraphicsDetails'))
setGeneric ('fitContent',               signature='obj', function (obj) standardGeneric ('fitContent'))
setGeneric ('fitSelectedContent',       signature='obj', function (obj) standardGeneric ('fitSelectedContent'))
setGeneric ('getCenter',                signature='obj', function (obj) standardGeneric ('getCenter'))
setGeneric ('setCenter',                signature='obj', function (obj, x, y) standardGeneric ('setCenter'))
setGeneric ('getZoom',                  signature='obj', function (obj) standardGeneric ('getZoom'))
setGeneric ('setZoom',                  signature='obj', function (obj, new.level) standardGeneric ('setZoom'))
setGeneric ('getViewCoordinates',       signature='obj', function (obj) standardGeneric ('getViewCoordinates'))

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
setGeneric ('setDefaultEdgeFontSize',     signature='obj', function (obj, new.size) standardGeneric ('setDefaultEdgeFontSize'))

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
setGeneric ('setNodeColorDirect',         signature='obj', function (obj, node.names, new.color) standardGeneric ('setNodeColorDirect'))
setGeneric ('setNodeBorderWidthDirect',   signature='obj', function (obj, node.names, new.sizes) standardGeneric ('setNodeBorderWidthDirect'))
setGeneric ('setNodeBorderColorDirect',   signature='obj', function (obj, node.names, new.color) standardGeneric ('setNodeBorderColorDirect'))

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
#setGeneric ('setEdgeFontFaceDirect', signature='obj', function (obj, edge.names, new.value) standardGeneric ('setEdgeFontFaceDirect'))
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
setGeneric ('.addNodes',                 signature='obj', function (obj, other.graph) standardGeneric ('.addNodes'))
setGeneric ('.addEdges',                 signature='obj', function (obj, other.graph) standardGeneric ('.addEdges'))

#------------------------------------------------------------------------------------------------------------------------
setValidity ("CytoscapeWindowClass",

  function (object) {
    if (length (object@title) != 1) 
      "'title' is not a single string" 
    else if (!nzchar (object@title))
      "'title' is an empty string" 
    validObject (object@graph)
    })

#------------------------------------------------------------------------------------------------------------------------
CytoscapeConnection = function (host='localhost', rpcPort=9000)
{
  
  res <- .BBSOverride(host, rpcPort)
  host = res$host
  rpcPort = res$rpcPort
  
  uri = sprintf ('http://%s:%s', host, rpcPort)
  cc = new ('CytoscapeConnectionClass', uri=uri)
  return (cc)

} # CytoscapeConnection
#------------------------------------------------------------------------------------------------------------------------
# the 'new window' class constructor, defined as a simple function, with no formal link to the class
new.CytoscapeWindow = function (title, graph=new('graphNEL', edgemode='directed'), host='localhost', rpcPort=9000, create.window=TRUE)
{
  
  res <- .BBSOverride(host, rpcPort)
  host = res$host
  rpcPort = res$rpcPort
  
  uri = sprintf ('http://%s:%s', host, rpcPort)

  cy.tmp = CytoscapeConnection (host, rpcPort)
  check.cytoscape.plugin.version (cy.tmp)

  if (!is.na (getWindowID (cy.tmp, title))) {
    write (sprintf ('There is already a window in Cytoscape named "%s".  Please use a unique name.', title), stderr ())
    stop ()
    }

    # add a label to each node if not already present.  default label is the node name, the node ID
  if (is.classic.graph (graph)) 
    if (edgemode (graph) == 'undirected'){
      graph = remove.redundancies.in.undirected.graph (graph)
      }

  if (! 'label' %in% noa.names (graph)) {
    write ('nodes have no label attribute -- adding default labels', stderr ())
    graph = initNodeAttribute (graph, 'label', 'char', 'noLabel')
    if (length (nodes (graph) > 0))
      nodeData (graph, nodes (graph), 'label') = nodes (graph)  # nodes (graph) returns strings
    } # if no label node attribute

  cw = new ('CytoscapeWindowClass', title=title, graph=graph, uri=uri)

  if (create.window)
    cw@window.id = createWindow (cw)

  return (cw)

} # new.CytsoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
CytoscapeWindow = new.CytoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
# the 'existing window' class constructor, defined as a simple function, with no formal link to the class
existing.CytoscapeWindow = function (title, host='localhost', rpcPort=9000, copy.graph.from.cytoscape.to.R=FALSE)
{
  
  res <- .BBSOverride(host, rpcPort)
  host = res$host
  rpcPort = res$rpcPort
  
  uri = sprintf ('http://%s:%s', host, rpcPort)

  cy.tmp = CytoscapeConnection (host, rpcPort)     # create this (inexpensively) just to gain access tothe window list
  check.cytoscape.plugin.version (cy.tmp)

  existing.window.id = getWindowID (cy.tmp, title)

  if (is.na (existing.window.id)) {
    write (sprintf ('There is no window in Cytoscape named "%s".  Please choose from the following titles:.', title), stderr ())
    write (as.character (getWindowList (cy.tmp)), stderr ())
    return (NA)
    }

  cw = new ('CytoscapeWindowClass', title=title, window.id=existing.window.id, uri=uri)

  if (copy.graph.from.cytoscape.to.R) {
    g.cy = getGraphFromCyWindow (cw, title)
    cw = setGraph (cw, g.cy)
    }

  return (cw)

} # existing.CytsoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
check.cytoscape.plugin.version = function (cyCon)
{
  plugin.version.string = pluginVersion (cyCon)
  string.tmp1 = strsplit (plugin.version.string,' ')[[1]][1]
  string.tmp2 = gsub ('[a-z]', '', string.tmp1)
  string.tmp3 = gsub ('[A-Z]', '', string.tmp2)
  plugin.version = as.numeric (string.tmp3)
  # plugin.version = as.numeric ((strsplit (plugin.version.string,' ')[[1]][1]))
  
  expected.version = 1.7   # 1.8 not yet ready at the Cytoscape plugin page
  if (plugin.version < expected.version) { 
    write (' ', stderr ())
    write (sprintf ('This version of the RCytoscape package requires CytoscapeRPC plugin version %s or greater.', expected.version), stderr ())
    write (sprintf ('However, you are using version %s.   You must upgrade.', plugin.version), stderr ())
    write ('Please visit the plugins page at http://www.cytoscape.org.', stderr ())
    write (' ', stderr ())
    stop ('Wrong CytoscapeRPC version.')
    }

} # check.cytoscape.plugin.version
#------------------------------------------------------------------------------------------------------------------------
setMethod ('ping', signature = 'CytoscapeConnectionClass',
  function (obj) { 
    return (xml.rpc (obj@uri, 'Cytoscape.test'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('pluginVersion', 'CytoscapeConnectionClass', 
  function (obj) { 
    return (xml.rpc (obj@uri, 'Cytoscape.version'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('msg', 'CytoscapeConnectionClass', 
  function (obj, string) { 
    invisible (xml.rpc (obj@uri, 'Cytoscape.setStatusBarMessage', string))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('clearMsg', 'CytoscapeConnectionClass', 
  function (obj) { 
    invisible (xml.rpc (obj@uri, 'Cytoscape.clearStatusBarMessage'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('createWindow', 'CytoscapeWindowClass',
  function (obj) {
       # window.ids are often character versions of integers.  but they can be character titles, as when an SBML
       # file is imported from disk.
    window.id = xml.rpc (obj@uri, 'Cytoscape.createNetwork', obj@title, .convert=TRUE)
    #write (sprintf ('createWindow, id = %d', window.id), stderr ()) 
    return (window.id)
  })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('createWindowFromSelection', 'CytoscapeWindowClass',

  function (obj, new.windowTitle, return.graph=FALSE) {
    if (getSelectedNodeCount (obj) == 0) {
      write (noquote ('RCytoscape::createWindowFromSelection error:  no nodes are selected'), stderr ())
      return (NA)
      }
    if (new.windowTitle %in% as.character (getWindowList (obj))) {
      msg = sprintf ('RCytoscape::createWindowFromSelection error:  window "%s" already exists', new.windowTitle)
      write (noquote (msg), stderr ())
      return (NA)
      }
      
    window.id = xml.rpc (obj@uri, 'Cytoscape.createNetworkFromSelection', obj@window.id, new.windowTitle)
    return (existing.CytoscapeWindow (new.windowTitle, copy.graph.from.cytoscape.to.R = return.graph))
    }) # createWindowFromSelection

#------------------------------------------------------------------------------------------------------------------------

setMethod ('getWindowCount', 'CytoscapeConnectionClass',
  function (obj) {
    return (as.integer (xml.rpc (obj@uri, 'Cytoscape.getNetworkCount')))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowID', 'CytoscapeConnectionClass',
  function (obj, window.title) {
    current.window.list = getWindowList (obj)
     if (!window.title %in% as.character (current.window.list)) {
       #write (sprintf ("No existing Cytoscape window named '%s'", window.title), stderr ())
       return (NA)
       } # if unrecognized window.title

    window.entry = which (as.character (current.window.list) == window.title)
    window.id =  as.character (names (current.window.list) [window.entry])
    return (window.id)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowCount', 'CytoscapeConnectionClass',
  function (obj) {
    return (as.integer (xml.rpc (obj@uri, 'Cytoscape.getNetworkCount')))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowList', 'CytoscapeConnectionClass',

  function (obj) {
    if (getWindowCount (obj) == 0)
      return (c ())

    result.raw = xml.rpc (obj@uri, 'Cytoscape.getNetworkList')
    result = c ()

    for (i in 1:length (result.raw)) {
      id = result.raw [[i]]$networkID
      title = result.raw [[i]]$networktitle
      result [[id]] = title
      } # for i

  return (result)

  }) # getWindowList
#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteWindow',  'CytoscapeConnectionClass',

  function (obj, window.title=NA) {
    if (!is.na (window.title))
      window.id = getWindowID (obj, window.title)
    else if (class (obj) == 'CytoscapeWindowClass')
      window.id = obj@window.id
    else {
      write (sprintf ('RCy::deleteWindow error.  You must provide a valid CytoscapeWindow object, or a CytoscapeConnection object and a window title'), stderr ())
      return ()
      }
    xml.rpc (obj@uri, 'Cytoscape.destroyNetwork', window.id)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteAllWindows',  'CytoscapeConnectionClass',

  function (obj) {
    ids = names (getWindowList (obj))
    invisible (sapply (ids, function (id)  xml.rpc (obj@uri, 'Cytoscape.destroyNetwork', id)))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'CytoscapeConnectionClass',

  function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getNodeShapeNames'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDirectlyModifiableVisualProperties', 'CytoscapeConnectionClass',

  function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getVisualStyleModifiables'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAttributeClassNames', 'CytoscapeConnectionClass',

  function (obj) {
     return (c ('floating|numeric|double', 'integer|int', 'string|char|character'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLineStyles', 'CytoscapeConnectionClass',

  function (obj) {
    return (xml.rpc (obj@uri, 'Cytoscape.getLineStyleNames'))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getArrowShapes', 'CytoscapeConnectionClass',

   function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getArrowShapeNames'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutNames', 'CytoscapeConnectionClass', 

   function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutNames'))
     }) # getLayoutNames

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutNameMapping', 'CytoscapeConnectionClass', 

   function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutNamesMapping'))
     }) # getLayoutNameMapping

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyNames', 'CytoscapeConnectionClass', 

   function (obj, layout.name) {
     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutProperties', layout.name))
     }) # getLayoutProperties

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyType', 'CytoscapeConnectionClass', 

   function (obj, layout.name, property.name) {
     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutPropertyType', layout.name, property.name))
     }) # getLayoutPropertyType

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutPropertyValue', 'CytoscapeConnectionClass', 

   function (obj, layout.name, property.name) {
     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutPropertyValue', layout.name, property.name))
     }) # getLayoutPropertyValue

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setLayoutProperties', 'CytoscapeConnectionClass', 

   function (obj, layout.name, properties.list) {
     all.possible.props = getLayoutPropertyNames (obj, layout.name)   # will throw error if there are no modifiable properties
     for (prop in names (properties.list)) {
       if (!prop %in% all.possible.props)
         write (sprintf ('%s is not a property in layout %s', prop, layout.name), stderr ())
       else {
         new.value = properties.list [[prop]]
         result = xml.rpc (obj@uri, 'Cytoscape.setLayoutPropertyValue', layout.name, prop, as.character (new.value))
         } # else
       } # for prop
     }) # setLayoutProperties

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setGraph', 'CytoscapeWindowClass',

  function (obj, graph) {
    if (edgemode (graph) == 'undirected') 
      graph = remove.redundancies.in.undirected.graph (graph)
    obj@graph = graph
    return (obj)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getGraph', 'CytoscapeWindowClass',

  function (obj) {
    return (obj@graph)
    })

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveNodeAttribute', 'CytoscapeConnectionClass',

  function (obj, node.names, attribute.name) {
    indices.of.nodes.with.attribute.value = which (xml.rpc (obj@uri, 'Cytoscape.nodesHaveAttribute', attribute.name, node.names))
    if (length (indices.of.nodes.with.attribute.value) > 0)
      return (node.names [indices.of.nodes.with.attribute.value])
    else 
      return (character(0))
    })

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveEdgeAttribute', 'CytoscapeConnectionClass',

  function (obj, edge.names, attribute.name) {
    if (length (edge.names) == 1)
      edge.names = rep (edge.names, 2)
    indices.of.edges.with.attribute.value = which (xml.rpc (obj@uri, 'Cytoscape.edgesHaveAttribute', attribute.name, edge.names))
    if (length (indices.of.edges.with.attribute.value) > 0)
      return (unique (edge.names [indices.of.edges.with.attribute.value]))
    else 
      return (character(0))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyNodeAttributesFromCyGraph', 'CytoscapeConnectionClass',

  function (obj, window.id, existing.graph) {
    node.attribute.names = getNodeAttributeNames (obj)
    #node.attribute.names = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
    for (attribute.name in node.attribute.names) {
      known.node.names = xml.rpc (obj@uri, "Cytoscape.getNodes", window.id)
      nodes.with.attribute = haveNodeAttribute (obj, known.node.names, attribute.name)
      if (length (nodes.with.attribute) > 0) {
        attribute.type = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeType', attribute.name, .convert=T)
        write (sprintf ('retrieving %s "%s" attribute for %d nodes', attribute.type, attribute.name, length (nodes.with.attribute)), stderr ())
        if (attribute.type == 'INTEGER') {
          attribute.type = 'integer'
          default.value = 0
          }
        else if (attribute.type == 'STRING') {
          attribute.type = 'char'
          default.value = 'unassigned'
          }
        else if (attribute.type == 'FLOATING') {
          attribute.type = 'numeric'
          default.value = as.numeric (0.0)
          }
        else {
          write (sprintf ('RCytoscape::copyNodeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr ())
          next ()
          } 
        existing.graph = initNodeAttribute (existing.graph, attribute.name, attribute.type, default.value)
        if (length (nodes.with.attribute) == 0) next;
        if (length (nodes.with.attribute) == 1)
          attribute.values = xml.rpc (obj@uri, 'Cytoscape.getNodeAttribute', attribute.name, nodes.with.attribute)
        else
          attribute.values = xml.rpc (obj@uri, 'Cytoscape.getNodesAttributes', attribute.name, nodes.with.attribute)
        nodeData (existing.graph, nodes.with.attribute, attribute.name) = attribute.values
         } # if
      } # for

    return (existing.graph)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyEdgeAttributesFromCyGraph', 'CytoscapeConnectionClass',

  function (obj, window.id, existing.graph) {
    edge.attribute.names = getEdgeAttributeNames (obj)
    write (sprintf ('creating %d cytoscape-style edge names', length (edgeNames (existing.graph))), stderr ())
    cy2.edgenames = as.character (cy2.edge.names (existing.graph))   # < 2 seconds for > 9000 edges
  
    for (attribute.name in edge.attribute.names) {
      edges.with.attribute = haveEdgeAttribute (obj, cy2.edgenames, attribute.name)
      if (length (edges.with.attribute) > 0) {
         attribute.type = xml.rpc (obj@uri, 'Cytoscape.getEdgeAttributeType', attribute.name, .convert=T)
         write (sprintf ('retrieving %s "%s" attribute for %d edges', attribute.type, attribute.name, length (edges.with.attribute)), stderr ())
         if (attribute.type == 'INTEGER') {
           attribute.type = 'integer'
           default.value = 0
           }
         else if (attribute.type == 'STRING') {
           attribute.type = 'char'
           default.value = 'unassigned'
           }
         else if (attribute.type == 'FLOATING') {
           attribute.type = 'numeric'
           default.value = as.numeric (0.0)
           }
        else {
          write (sprintf ('RCytoscape::copyEdgeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr ())
          next ()
          } 
         existing.graph = initEdgeAttribute (existing.graph, attribute.name, attribute.type, default.value)
         if (length (edges.with.attribute) == 1)
           eda.value = unique (xml.rpc (obj@uri, 'Cytoscape.getEdgesAttributes', attribute.name, rep (edges.with.attribute, 2)))
         else
           eda.value = xml.rpc (obj@uri, 'Cytoscape.getEdgesAttributes', attribute.name, edges.with.attribute)
         regex = ' *[\\(|\\)] *'
         edges.tokens = strsplit (edges.with.attribute, regex)
         source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
         target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
         edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
         edgeData (existing.graph, source.nodes, target.nodes, attribute.name) = eda.value
         } # if
      } # for
  
     return (existing.graph)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getGraphFromCyWindow', 'CytoscapeConnectionClass',

  function (obj,  window.title) {
    window.id = getWindowID (obj, window.title)
    stopifnot (!is.na (window.id))
  
    node.count = xml.rpc (obj@uri, "Cytoscape.countNodes", window.id)
    if (node.count == 0)
      return (new ('graphNEL', edgemode='directed'))
      
    all.node.names = xml.rpc (obj@uri, "Cytoscape.getNodes", window.id)
    write (sprintf ('received %d nodes from %s', length (all.node.names), window.title), stderr ())
    g = new ("graphNEL", edgemode='directed')
    write (sprintf ('adding %d nodes to local graph', length (all.node.names)), stderr ())
    g = graph::addNode (all.node.names, g)
    
    node.attribute.names = getNodeAttributeNames (obj)
    #node.attribute.names = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
    g = initEdgeAttribute (g, 'edgeType', 'char', 'assoc')

    edge.count = xml.rpc (obj@uri, "Cytoscape.countEdges", window.id)
  
    if (edge.count > 0) {
      regex = ' *[\\(|\\)] *'
      all.edge.names = xml.rpc (obj@uri, "Cytoscape.getEdges", window.id)
      write (sprintf ('received %d edges from %s', length (all.edge.names), window.title), stderr ())
      edges.tokens = strsplit (all.edge.names, regex)
      source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
      target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
      edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
      write (sprintf ('adding %d edges to local graph', length (edges.tokens)), stderr ())
      g = addEdge (source.nodes, target.nodes, g)
      edgeData (g, source.nodes, target.nodes, 'edgeType') = edge.types
      g = copyNodeAttributesFromCyGraph (obj, window.id, g)
      g = copyEdgeAttributesFromCyGraph (obj, window.id, g)
      } # if edgeCount > 0
  
    return (g)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendNodes', 'CytoscapeWindowClass',

  function (obj) {
     printf ('sendNodes %d', 0)
     if (length (nodes (obj@graph)) == 0) {
       write ('CytoscapeWindow.sendNodes, no nodes in graph.  returning', stderr ())
       return ()
       }
     printf ('sendNodes %d', 1)
     graph.nodes = nodes (obj@graph)
     printf ('sendNodes %d', 1)
     write (sprintf ('sending %d nodes', length (graph.nodes)), stderr ())
     printf ('sendNodes %d', 2)
     invisible (xml.rpc (obj@uri, 'Cytoscape.createNodes', as.character (obj@window.id), graph.nodes))
     printf ('sendNodes %d', 3)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addNodes', signature (obj='CytoscapeWindowClass'),

  function (obj, other.graph) {
     if (length (nodes (other.graph)) == 0) {
       write ('CytoscapeWindow.sendNodes, no nodes in other.graph.  returning', stderr ())
       return ()
       }
     new.nodes = setdiff (nodes (other.graph), nodes (obj@graph))
     invisible (xml.rpc (obj@uri, 'Cytoscape.createNodes', as.character (obj@window.id), new.nodes))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('.addEdges', signature (obj='CytoscapeWindowClass'),

  function (obj, other.graph) {
    if (length (edgeNames (other.graph)) == 0) {
       write ('CytoscapeWindow::.addEdges, no edges in graph.  returning', stderr ())
       return ()
       }
                 
       # extract and compare edge names
    new.edgeNames = setdiff (edgeNames (other.graph), edgeNames (obj@graph))
    #printf ('---- new.edgeNames %d', length (new.edgeNames))
    #print (new.edgeNames)
    new.edgeNames.withBar = gsub ('~','|', new.edgeNames)

    tokens = strsplit (new.edgeNames, '~')
    #tokens = strsplit (new.edgeNames, '~')
    #tokens = strsplit (edgeNames (other.graph), '~')
    a = sapply (tokens, function (tok) tok [1])
    b = sapply (tokens, function (tok) tok [2])
    edge.type = as.character (eda (other.graph, 'edgeType') [new.edgeNames.withBar])
    #printf ('edge.type:    ')
    #print (edge.type)

    if (length (edge.type) == 1 && is.na (edge.type))
      edge.type = rep ('unspecified', length (tokens))
    directed = rep (TRUE, length (tokens))
    forgive.if.node.is.missing = TRUE

     # deferring this effiency (sending only new edges) for now.
     # if ('edgeType' %in% eda.names (obj@graph) && 'edgeType' %in% eda.names (other.graph)) {
     #   existing.edge.signatures = sort (paste (names (edgeNames (obj@graph)), as.character (eda (obj@graph, 'edgeType'))))
     #   new.edge.signatures      = sort (paste (names (edgeNames (other.graph)), as.character (eda (other.graph, 'edgeType'))))
     #   new.edges = 
     #   } # if

    #write ('---- about to xml.rpc call Cytoscape.createEdges', stderr ())
    #write (a, stderr ())
    #write (b, stderr ())
    #write (edge.type, stderr ())
    xml.rpc (obj@uri, 'Cytoscape.createEdges', as.character (obj@window.id), a, b, edge.type, directed, forgive.if.node.is.missing, .convert=F)
    }) # .addEdges


#------------------------------------------------------------------------------------------------------------------------
setMethod ('addCyNode', 'CytoscapeWindowClass',

  function (obj, nodeName) {
    if (nodeName %in% getAllNodes (obj))
      write (sprintf ('RCytoscape::addNode, %s node already present in Cytoscape graph', nodeName), stderr ())
    invisible (xml.rpc (obj@uri, 'Cytoscape.createNode', obj@window.id, nodeName))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('addCyEdge', 'CytoscapeWindowClass',

  function (obj, sourceNode, targetNode, edgeType, directed) {
    good.args = TRUE
    if (!sourceNode %in% getAllNodes (obj)) {
      good.args = FALSE
      write (sprintf ('RCytoscape::addEdge, %s node not in Cytoscape graph', sourceNode), stderr ())
      }
    if (!targetNode %in% getAllNodes (obj)) {
      good.args = FALSE
      write (sprintf ('RCytoscape::addEdge, %s node not in Cytoscape graph', targetNode), stderr ())
      }
    if (!good.args)
      return (NA)
    invisible (xml.rpc (obj@uri, 'Cytoscape.createEdge', obj@window.id, sourceNode, targetNode, edgeType, directed))
    })

#------------------------------------------------------------------------------------------------------------------------
# this method adds a new graph to an existing graph.  first the new nodes, then the new edges, then node attributes, then edge
# attributes
setMethod ('addGraphToGraph', 'CytoscapeWindowClass',

  function (obj, other.graph) {
    .addNodes (obj, other.graph)  
    .addEdges (obj, other.graph)
  
    node.attribute.names = noa.names (other.graph)
    for (attribute.name in node.attribute.names) {
      printf ('sending noa %s', attribute.name)
      .sendNodeAttributesForGraph (obj, other.graph, attr=attribute.name)
      }
  
    node.attribute.names = noa.names (other.graph)
    for (attribute.name in node.attribute.names) {
      printf ('sending noa %s', attribute.name)
      .sendNodeAttributesForGraph (obj, other.graph, attr=attribute.name)
      }
  
    edge.attribute.names = eda.names (other.graph)
    for (attribute.name in edge.attribute.names) {
      printf ('sending eda %s', attribute.name)
      .sendEdgeAttributesForGraph (obj, other.graph, attr=attribute.name)
      }
    }) # addGraphToGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdges', 'CytoscapeWindowClass',

  function (obj) {
    printf ('sendEdges %d', 0)
    if (length (edgeNames (obj@graph)) == 0) {
       write ('CytoscapeWindow.sendEdges, no edges in graph.  returning', stderr ())
       return ()
       }
                 
    printf ('sendEdges %d', 1)
    if (is.classic.graph (obj@graph))
      tbl.edges = .classicGraphToNodePairTable (obj@graph)
    else if (is.multiGraph (obj@graph))
      tbl.edges = .multiGraphToNodePairTable (obj@graph)
    printf ('sendEdges %d', 2)

       # todo:  if there is only one edge, Cytoscape.createEdges does not resolve, since arrays are expected, and 1-element arrays
       # todo:  are treated as scalars in the trip from R to the java virutal machine.  (pshannon, 5 apr 2011)
    printf ('sendEdges %d', 3)
    write (sprintf ('sending %d edges', nrow (tbl.edges)), stderr ())
    printf ('sendEdges %d', 4)
    a = tbl.edges$source
    printf ('sendEdges %d', 5)
    b = tbl.edges$target
    printf ('sendEdges %d', 6)
    edge.type = tbl.edges$edgeType
    printf ('sendEdges %d', 7)
    directed = rep (TRUE, length (a))
    printf ('sendEdges %d', 8)
    forgive.if.node.is.missing = TRUE
    printf ('sendEdges %d', 9)
    if (length (a) == 1)
      invisible (xml.rpc (obj@uri, 'Cytoscape.createEdge', obj@window.id, a, b, edge.type, directed))
    else
      invisible (xml.rpc (obj@uri, 'Cytoscape.createEdges', obj@window.id, a, b, edge.type, directed, forgive.if.node.is.missing, .convert=T))
    }) # sendEdges


#------------------------------------------------------------------------------------------------------------------------
#setMethod ('sendEdges', 'CytoscapeWindowClass',
#
#  function (obj) {
#    if (length (edgeNames (obj@graph)) == 0) {
#       write ('CytoscapeWindow.sendEdges, no edges in graph.  returning', stderr ())
#       return ()
#       }
#                 
#    tokens = strsplit (edgeNames (obj@graph), '~')
#    a = sapply (tokens, function (tok) tok [1])
#    b = sapply (tokens, function (tok) tok [2])
#    edge.type = as.character (eda (obj@graph, 'edgeType'))
#    if (length (edge.type) == 1 && is.na (edge.type))
#      edge.type = rep ('unspecified', length (tokens))
#    directed = rep (TRUE, length (tokens))
#    forgive.if.node.is.missing = TRUE
#  
#    if (length (edge.type) > length (a)) { # sign of pathological graph, probably has edges going both ways between two nodes
#      write (sprintf ('RCytoscape::sendEdges error, probably a pathological graph, with edges going both ways between pair or pairs of nodes.'), stderr ())
#      write (sprintf ('length of a: %d   length of b: %d   length of edge.type: %d', length (a), length (b), length (edge.type)), stderr ())
#      #stop ()
#      } # pathological graph
#    
#    xml.rpc (obj@uri, 'Cytoscape.createEdges', as.character (obj@window.id), a, b, edge.type, directed, forgive.if.node.is.missing, .convert=F)
#    }) # sendEdges
#
#
   # for (source.node in names (edges (obj@graph))) {
   #   for (target.node in edges (obj@graph)[[source.node]]) {
   #     interaction = 'unknown'
   #     if ('edgeType' %in% names (edgeDataDefaults (obj@graph)))
   #       interaction = as.character (edgeData (obj@graph, source.node, target.node, 'edgeType'))
   #     else if ('type' %in% names (edgeDataDefaults (obj@graph)))
   #       interaction = as.character (edgeData (obj@graph, source.node, target.node, 'type'))
   #     #printf ('creating edge  %s (%s) %s', source.node, interaction, target.node)     
   #     xml.rpc (obj@uri, 'Cytoscape.createEdge', source.node, target.node, interaction, TRUE)
   #     } # for target.node
   #   } # for source.node
   # }) # sendEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod ('layoutNetwork', 'CytoscapeWindowClass',

  function (obj, layout.name='jgraph-spring') {

    if (!layout.name %in% getLayoutNames (obj)) {
      write (sprintf ("layout.name '%s' is not recognized; call getLayoutNames (<CytoscapeWindow>) to see those which are supported", layout.name), stderr ())
      return ()
      }

    id = as.character (obj@window.id)
    invisible (xml.rpc (obj@uri, 'Cytoscape.performLayout', id, layout.name))
    }) # cy.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveLayout', 'CytoscapeWindowClass',

  function (obj, filename, timestamp.in.filename=FALSE) {
    custom.layout = getNodePosition (obj,  getAllNodes (obj))
    if (timestamp.in.filename) {
      dateString = format (Sys.time (), "%a.%b.%d.%Y-%H.%M.%S")
      stem = strsplit (filename, '\\.RData')[[1]]
      filename = sprintf ('%s.%s.RData', stem, dateString)
      write (sprintf ('saving layout to %s\n', filename), stderr ())
      }
    save (custom.layout, file=filename)
    }) # save.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('restoreLayout', 'CytoscapeWindowClass',

  function (obj, filename) {
    load (filename)
    node.names = names (custom.layout)
    node.names.filtered = intersect (node.names, getAllNodes (obj))
    x = as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$x)))
    y = as.integer (sapply (node.names.filtered, function (node.name) return (custom.layout [[node.name]]$y)))
    setNodePosition (obj, node.names.filtered, x, y)
    }) # restoreLayout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodePosition', 'CytoscapeWindowClass',

  function (obj, node.names, x.coords, y.coords) {

    unknown.nodes = setdiff (node.names, nodes (obj@graph))
    if (length (unknown.nodes) > 0) {
      node.names = intersect (node.names, nodes (obj@graph))
      write (sprintf ("Error!  unknown nodes in RCytoscape::setNodePosition"), stderr ())
      for (i in 1:length (unknown.nodes))
        write (sprintf ("     %s", unknown.nodes [i]), stderr ())
      return ()
      } # if 

    count = length (node.names)
    stopifnot (length (x.coords) == count)
    stopifnot (length (y.coords) == count)

    if (count == 0)
      return ()

    id = as.character (obj@window.id)

    if (count == 1)
      invisible (xml.rpc (obj@uri, 'Cytoscape.setNodePosition', id, node.names, as.numeric (x.coords), as.numeric (y.coords)))
    else 
      invisible (xml.rpc (obj@uri, 'Cytoscape.setNodesPositions', id, node.names, as.numeric (x.coords), as.numeric (y.coords)))

    }) # cy.setNodePosition

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodePosition', 'CytoscapeWindowClass',

  function (obj, node.names) {

    count = length (node.names)
    if (count == 1)
      node.names = rep (node.names, 2)   # work around R's distinction between scalar and list of strings
    node.name.delimiter = ':-:'
    xy.delimiter = ';;'
    raw.result = xml.rpc (obj@uri, 'Cytoscape._rGetNodesPositions', obj@window.id, node.names, node.name.delimiter, xy.delimiter)
    #raw.result = xml.rpc (obj@uri, 'Cytoscape._rGetNodesPositions', obj@window.id, node.names)
     # sample raw result (16 dec 2010): "2022:417.0,122.0" "659:156.0,0.0"   
     # now parse this list of strings into directly usable values, a named list (using node ID's) with x,y pair values

    #printf ('raw.result: %s', raw.result)
    tokens = strsplit (raw.result, node.name.delimiter)
    result = list ()
    for (token in tokens) {
      #printf ('token: %s', list.to.string (token))
      name = token [1]
      #printf ('name: %s', name)
      xy.tokens = strsplit (token [2], xy.delimiter)
      #printf ('xy.tokens: %s', list.to.string (xy.tokens))
      x = as.integer (xy.tokens[[1]][1])
      y = as.integer (xy.tokens[[1]][2])
      #printf ('x: %d', x)
      #printf ('y: %d', y)
      result [[name]] = list (x=x, y=y)
      } # for token

    return (result)
    }) # cy.getNodePosition

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeSize', 'CytoscapeWindowClass',

  function (obj, node.names) {

    count = length (node.names)
    if (count == 1)
      node.names = rep (node.names, 2)   # work around R's distinction between scalar and list of strings

    widths  = as.integer (round (xml.rpc (obj@uri, 'Cytoscape.getNodesWidth',  node.names)))
    heights = as.integer (round (xml.rpc (obj@uri, 'Cytoscape.getNodesHeight', node.names)))

    if (count == 1) {
      widths = widths [1]
      heights = heights [1]
      }

    return (list (width=widths, height=heights))
    }) # cy.getNodeSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeAttributes', 'CytoscapeWindowClass',

   function (obj, attribute.name) {

     if (length (nodes (obj@graph)) == 0)
       return ()

     caller.specified.attribute.class = attr (nodeDataDefaults (obj@graph, attribute.name), 'class')

     if (is.null (caller.specified.attribute.class)) {
       msg1 = sprintf ('Error!  RCytoscape::setNodeAttributes, attributes not initialized. You must call')
       msg2 = sprintf ('        initNodeAttribute (graph, attribute.name, attribute.type, default.value)')
       msg3 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
       msg4 = sprintf ('        example:  g <- initNodeAttribute (g, "nodeType", "char", "molecule")')
       msg5 = sprintf ('             or:  g <- initNodeAttribute (g, "pValue", "numeric", 1.0)')
       write (msg1, stderr ())
       write (msg2, stderr ())
       write (msg3, stderr ())
       write (msg4, stderr ())
       invisible (NA)
       }

     node.names = nodes (obj@graph)
     values = noa (obj@graph, attribute.name)
     invisible (setNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, node.names, values))
     }) # setNodeAttributes

#------------------------------------------------------------------------------------------------------------------------
# with this version, unlike setNodeAttributes, the attributes need not be already stored in the graph
setMethod ('setNodeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, node.names, values) {

     if (length (node.names) == 0)
       return ()

     #chad.debug (obj, "in RCytoscape::setNodeAttributesDirect")

     if (length (node.names) != length (values)) {
       write (sprintf ('RCytoscape::setNodeAttributesDirect ERROR.'), stderr ())
       write (sprintf ('attribute name %s, node.names %d, values %d', attribute.name, length (node.names), length (values)), stderr ())
       return ();
       }

        # in sending arguments to CytoscapeRPC, lists of length one become scalars, and so fail to match
        # java methods that expect lists.  to sidestep that problem, duplicate node.name and value, 
        # creating silly but effective lists of length 2

     if (length (node.names) == 1) {
       node.names = rep (node.names, 2)
       values = rep (values, 2)
       }


     caller.specified.attribute.class = tolower (attribute.type)
     if (is.null (caller.specified.attribute.class) || length (caller.specified.attribute.class) == 0)   # NULL, or non-null but empty
       caller.specified.attribute.class = 'string'

     result = ''

     if (caller.specified.attribute.class %in% c ('float', 'floating', 'numeric', 'double')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addDoubleNodeAttributes', attribute.name, node.names, as.numeric (values), .convert=TRUE)
       #write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('integer', 'int')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addIntegerNodeAttributes', attribute.name, node.names, as.integer (values), .convert=TRUE)
       #write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('string', 'char', 'character')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addStringNodeAttributes', attribute.name, node.names, as.character (values), .convert=TRUE)
       #write (result, stderr ())
       }

     invisible (result)
     }) # setNodeAttributesDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeAttributes', 'CytoscapeWindowClass',

   function (obj, attribute.name) {

     printf ('--- setEdgeAttributes')
     if (length (edgeNames (obj@graph)) == 0)
       return ()
     
     printf ('sEA %d', 0)
     caller.specified.attribute.class = attr (edgeDataDefaults (obj@graph, attribute.name), 'class')
     printf ('sEA %d', 1)
     #write (sprintf ('RCy::setEdgeAttributes, eda name = %s', attribute.name), stderr ())

     if (is.null (caller.specified.attribute.class)) {
       msg1 = sprintf ('Error!  RCytoscape::setEdgeAttributes, attributes not initialized. You must call')
       msg2 = sprintf ('        initEdgeAttribute (graph, attribute.name, attribute.type, default.value)')
       msg3 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
       msg4 = sprintf ('        example:  g <- initEdgeAttribute (g, "edgeType", "char", "protein-protein interaction")')
       msg5 = sprintf ('             or:  g <- initEdgeAttribute (g, "confidence", "numeric", 0.0)')
       write (msg1, stderr ())
       write (msg2, stderr ())
       write (msg3, stderr ())
       write (msg4, stderr ())
       return (NA)
       }
     printf ('sEA %d', 2)
     cy2Names = cy2.edge.names (obj@graph)
     edge.names = as.character (cy2Names)
     printf ('sEA %d', 3)
     edge.names.tilde = names (cy2Names)
     printf ('sEA %d', 4)
     edge.names.with.bars = gsub ('~', '|', edge.names.tilde)
     printf ('sEA %d', 5)
     values = eda (obj@graph, attribute.name) [edge.names.with.bars]
     #print (values)

     #write (edge.names, stderr ())
     #write (values, stderr ())

     #write (sprintf ('about to call setEdgeAttributesDirect %s, %d edge.names, %d values', attribute.name, length (edge.names), length (values)), stderr())
     printf ('sEA %d', 6)
     invisible (setEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, edge.names, values))
     printf ('sEA %d', 7)
     }) # setEdgeAttributes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, edge.names, values) {

     printf ('sEAD %d', 0)
     write (sprintf ('entering setEdgeAttributesDirect, %s, with %d names and %d values', attribute.name, length (edge.names), length (values)), stderr ())

     printf ('sEAD %d', 1)
     if (length (edge.names) == 0)
       return ()

     printf ('sEAD %d', 2)
     if (length (values) == 2 * length (edge.names))
       values = values [1:length (edge.names)]

        # in sending arguments to CytoscapeRPC, lists of length one become scalars, and so fail to match
        # java methods that expect lists.  to sidestep that problem, duplicate edge.name and value, 
        # creating silly but effective lists of length 2

     printf ('sEAD %d', 3)
     if (length (edge.names) == 1) {
       edge.names = rep (edge.names, 2)
       values = rep (values, 2)
       }

     #write (sprintf ('edge.names: %s', list.to.string (edge.names)), stderr ())
     #write (sprintf ('    values: %s', list.to.string (values)), stderr ())

     printf ('sEAD %d', 4)
     caller.specified.attribute.class = tolower (attribute.type)

     printf ('sEAD %d', 5)
     if (is.null (caller.specified.attribute.class) || length (caller.specified.attribute.class) == 0)   # NULL, or non-null but empty
       caller.specified.attribute.class = 'string'

     result = ''

     printf ('sEAD %d', 6)
     if (length (edge.names) != length (values)) {
       write (sprintf ('RCytoscape::setEdgeAttributesDirect ERROR....'), stderr ())
       write (sprintf ('attribute name %s, edge.names %d, values %d', attribute.name, length (edge.names), length (values)), stderr ())
       return ();
       }

     printf ('sEAD %d', 7)
     if (caller.specified.attribute.class %in% c ('floating', 'numeric', 'double')) {
     printf ('sEAD %d', 8)
       result = xml.rpc (obj@uri, 'Cytoscape.addDoubleEdgeAttributes', attribute.name, edge.names, as.numeric (values), .convert=TRUE)
       #write (sprintf ('result of addDoubleEdgeAttributes: %s', result), stderr ())
       #write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('integer', 'int')) {
     printf ('sEAD %d', 9)
       result = xml.rpc (obj@uri, 'Cytoscape.addIntegerEdgeAttributes', attribute.name, edge.names, as.integer (values), .convert=TRUE)
       #write (sprintf ('result of addIntegerEdgeAttributes: %s', result), stderr ())
       #write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('string', 'char', 'character')) {
     printf ('sEAD %d', 10)
       if (length (edge.names) == 1)
         result = xml.rpc (obj@uri, 'Cytoscape.addStringEdgeAttribute', attribute.name, edge.names, as.character (values), .convert=TRUE)
       else
         result = xml.rpc (obj@uri, 'Cytoscape.addStringEdgeAttributes', attribute.name, edge.names, as.character (values), .convert=TRUE)
       #write (sprintf ('result of addStringEdgeAttribute/s: %s', result), stderr ())
       #write (result, stderr ())
       }
     printf ('sEAD %d', 11)
     invisible (result)
     }) # setEdgeAttributesDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('displayGraph', 'CytoscapeWindowClass',

   function (obj) {
     write ('entering RCytoscape::displayGraph', stderr ())
     if (length (nodes (obj@graph)) == 0) {
       write ('RCytoscape::displayGraph, empty graph, returning', stderr ())
       return ()
       }

     #write (sprintf ('adding %d nodes...', length (nodes (obj@graph))),  stderr ())
     sendNodes (obj)
     #write (sprintf ('adding %d edges...', length (edgeNames (obj@graph))), stderr ())
     sendEdges (obj)
     write ('adding node attributes...', stderr ())
     sapply (noa.names (obj@graph), function (name) {print (name); setNodeAttributes (obj, name)})
     write ('adding edge attributes...', stderr ())
     sapply (eda.names (obj@graph), function (name) {print (name);  setEdgeAttributes (obj, name)})
     }) # displayGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod ('redraw', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.redraw', id))
     }) # redraw

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setWindowSize', 'CytoscapeWindowClass',

   function (obj, width, height) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.resizeNetworkView', id, as.integer (width), as.integer (height)))
     }) # redraw

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipInitialDelay', 'CytoscapeConnectionClass',

   function (obj, msecs) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipInitialDelay', as.integer (msecs)))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setTooltipDismissDelay', 'CytoscapeConnectionClass',

   function (obj, msecs) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.setToolTipDismissDelay', as.integer (msecs)))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('raiseWindow', 'CytoscapeConnectionClass',

   function (obj, window.title=NA) {
     if (is.na (window.title)) {
       if (class (obj) == 'CytoscapeWindowClass')
         window.id = obj@window.id
       else {  # must be a CytoscapeConnectionClass object, but no title was provide.  error.
         write (sprintf ('error in raiseWindow, no window title provided'), stderr ())
         return ()
         }
       } # no title
     if (!is.na (window.title))  {  # a title was provided
       window.id = getWindowID (obj, window.title)
       if (is.na (window.id)) {
         write (sprintf ('error in raiseWindow, unrecognized window title: %s', window.title), stderr ())
         return ()
         }
        } # title was provide
     invisible (xml.rpc (obj@uri, 'Cytoscape.raiseNetworkView', window.id))
     }) # raiseWindow

#------------------------------------------------------------------------------------------------------------------------
setMethod ('showGraphicsDetails', 'CytoscapeConnectionClass',

  function (obj, new.value) {
    invisible (xml.rpc (obj@uri, 'Cytoscape.setShowGraphicsDetails', new.value))
    if (class (obj) == 'CytoscapeWindowClass')
      redraw (obj)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('fitContent', 'CytoscapeWindowClass',

   function (obj) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.fitContent', obj@window.id))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('fitSelectedContent', 'CytoscapeWindowClass',

   function (obj) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.fitSelectedContent', obj@window.id))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getCenter', 'CytoscapeWindowClass',

   function (obj) {
     tmp = xml.rpc (obj@uri, 'Cytoscape.getCenter', obj@window.id)
     return (list (x=tmp [1], y=tmp[2]))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setCenter', 'CytoscapeWindowClass',

   function (obj, x, y) {
     tmp = xml.rpc (obj@uri, 'Cytoscape.setCenter', obj@window.id, as.numeric (x), as.numeric (y))
     invisible (tmp)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getZoom', 'CytoscapeWindowClass',

   function (obj) {
     tmp = xml.rpc (obj@uri, 'Cytoscape.getZoom', obj@window.id)
     return (tmp)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setZoom', 'CytoscapeWindowClass',

   function (obj, new.level) {
     tmp = xml.rpc (obj@uri, 'Cytoscape.setZoom', obj@window.id, new.level)
     invisible (tmp)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getViewCoordinates', 'CytoscapeWindowClass',

   function (obj) {
     tmp = xml.rpc (obj@uri, 'Cytoscape.getViewCoordinates', obj@window.id)
     return (list (top.x=tmp[1], top.y=tmp[2], bottom.x=tmp[3], bottom.y=tmp[4]))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hidePanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.hidePanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideAllPanels', 'CytoscapeConnectionClass',

  function (obj) {
    invisible (sapply (tolower (LETTERS), function (letter) hidePanel (obj, letter)))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('dockPanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.dockPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('floatPanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.floatPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeTooltipRule', 'CytoscapeWindowClass',

      # todo:  prevent the obligatory redraw
           
  function (obj, node.attribute.name) {
    id = as.character (obj@window.id)
    viz.style.name = 'default'
    if (!node.attribute.name %in% noa.names (obj@graph)) {
      write (sprintf ('warning!  setNodeTooltipRule passed non-existent node attribute: %s', node.attribute.name), stderr ())
      return ()
      }
    attribute.values = as.character (noa (obj@graph, node.attribute.name))
    tooltips = attribute.values   # an identity mapping: if you see node attribute x, then display x.  odd but true.
    default.tooltip = ''
    xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', viz.style.name, node.attribute.name, 'Node Tooltip', default.tooltip,
             attribute.values, tooltips)
    })  # setNodeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipRule', 'CytoscapeWindowClass',

  function (obj, edge.attribute.name) {
    id = as.character (obj@window.id)
    viz.style.name = 'default'
    attribute.values = as.character (eda (obj@graph, edge.attribute.name))
    tooltips = attribute.values  # identity mapping: when eda has value x, tooltip is x.  odd but true.
    default.tooltip = ''
    xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', viz.style.name, edge.attribute.name, 'Edge Tooltip', default.tooltip,
             attribute.values, tooltips)
    })  # setEdgeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelRule', 'CytoscapeWindowClass',

  function (obj, node.attribute.name) {
    id = as.character (obj@window.id)
    xml.rpc (obj@uri, 'Cytoscape.setNodeLabel', id, node.attribute.name, 'label', 'default'); 
    })  # setNodeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelRule', 'CytoscapeWindowClass',

  function (obj, edge.attribute.name) {
    id = as.character (obj@window.id)
    default.value = ''
    result = xml.rpc (obj@uri, 'Cytoscape.edgePassthroughMapper', edge.attribute.name, 'Edge Label', default.value)
    invisible (result)
    })  # setEdgeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, colors, mode, default.color='#FFFFFF') {

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultNodeColor (obj, default.color)
     if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
       if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
         colors = c (colors [1], colors, colors [length (colors)])
         #write ("RCytoscape::setNodeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
         } # 

       good.args = length (control.points) == (length (colors) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeColorRule, interpolate mode.", stderr ())
         write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Color', control.points, colors, FALSE)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (colors)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         colors = rep (colors, 2)
         } 
       result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
                         node.attribute.name, 'Node Color', default.color, control.points, colors)
       invisible (result)
       } # else: !interpolate
     }) # setNodeColorRule


#------------------------------------------------------------------------------------------------------------------------
# Cytoscape distinguishes between Node Opacity, Node Border Opacity, and Node Label Opacity.  we call this 'aspect' here.

setMethod ('setNodeOpacityRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, opacities, mode, aspect='all') {

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeOpacityRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

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
       msg.1 = "Error! RCytoscape:setNodeOpacityRule.  apect must be 'all' (the default) "
       msg.2 = sprintf ("or some combination, in any order, of %s", specific.options)
       write (msg.1, stderr ())
       write (msg.2, stderr ())
       return ()
       }

     if (mode=='interpolate') {  # need a 'below' opacity and an 'above' opacity.  so there should be two more opacities than control.points 
       if (length (control.points) == length (opacities)) { # caller did not supply 'below' and 'above' values; manufacture them
         opacities = c (opacities [1], opacities, opacities [length (opacities)])
         #write ("RCytoscape::setNodeOpacityRule, no 'below' or 'above' opacities specified.  Inferred from supplied opacities.", stderr ());
         } # 

       good.args = length (control.points) == (length (opacities) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (opacities)), stderr ())
         write ("Error! RCytoscape:setNodeOpacityRule, interpolate mode.", stderr ())
         write ("Expecting 1 opacity for each control.point, one for 'above' opacity, one for 'below' opacity.", stderr ())
         return ()
         }
       
       if (aspect.fill)
         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Opacity', control.points, opacities, FALSE)
       if (aspect.border) 
         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Border Opacity', control.points, 
                           opacities, FALSE)
       if (aspect.label) 
         result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Label Opacity', control.points, 
                           opacities, FALSE)
       invisible (result)
       } # if mode==interpolate

     else { # mode==lookup, use a discrete rule, with no interpolation
       good.args = length (control.points) == length (opacities)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (opacities)), stderr ())
         write ("Error! RCytoscape:setNodeOpacityRule.  Expecting exactly as many opacities as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       default.opacity = 255;
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         opacities = rep (opacities, 2)
         } 

       if (aspect.fill)
         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
                           node.attribute.name, 'Node Opacity', as.character (default.opacity), control.points, as.character (opacities))
       if (aspect.border) 
         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
                           node.attribute.name, 'Node Border Opacity', as.character (default.opacity), control.points, as.character (opacities))
       if (aspect.label) 
         result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
                           node.attribute.name, 'Node Label Opacity', as.character (default.opacity), control.points, as.character (opacities))
       invisible (result)
       } # else: !interpolate
     }) # setNodeOpacityRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, colors, mode, default.color='#000000') {

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeBorderColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultNodeBorderColor (obj, default.color)
     
     if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
       if (length (control.points) == length (colors))  # caller did not supply 'below' and 'above' values; manufacture them
         colors = c (default.color, colors, default.color)

       good.args = length (control.points) == (length (colors) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeBorderColorRule, interpolate mode.", stderr ())
         write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Border Color', control.points, colors, FALSE)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (colors)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeBorderColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         colors = rep (colors, 2)
         } 
       result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, node.attribute.name,
                          'Node Border Color', default.color, control.points, colors)
       invisible (result)
       } # else: !interpolate
     }) # setNodeBorderColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, line.widths, default.width=1) {

     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       }
     id = as.character (obj@window.id)
     visual.property.type.name = 'Node Line Width'  # see class cytoscape.visual.VisualPropertyType

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       } 
     result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', 'default', node.attribute.name, 
                       'Node Line Width', as.character (default.width), attribute.values, as.character (line.widths))
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeShape', 'CytoscapeConnectionClass', 

   function (obj, new.shape, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Shape', new.shape); 
     #redraw (obj)
     }) # setDefaultNodeShape

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeSize', 'CytoscapeConnectionClass', 

   function (obj, new.size, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Size', as.character (new.size)); 
     #redraw (obj)
     }) # setDefaultNodeSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeColor', 'CytoscapeConnectionClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Color', as.character (new.color)); 
     }) # setDefaultNodeColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderColor', 'CytoscapeConnectionClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Border Color', as.character (new.color)); 
     }) # setDefaultNodeBorderColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderWidth', 'CytoscapeConnectionClass', 

   function (obj, new.width, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Line Width', as.character (new.width)); 
     }) # setDefaultNodeBorderWidth

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeFontSize', 'CytoscapeConnectionClass', 

   function (obj, new.size, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Font Size', as.character (new.size));
     }) # setDefaultNodeFontSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeLabelColor', 'CytoscapeConnectionClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Label Color', as.character (new.color)); 
     }) # setDefaultNodeLabelColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeLineWidth', 'CytoscapeConnectionClass', 

   function (obj, new.width, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Edge Line Width', as.character (new.width)); 
     }) # setDefaultEdgeLineWidth

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeColor', 'CytoscapeConnectionClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Edge Color', as.character (new.color)); 
     }) # setDefaultEdgeColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeFontSize', 'CytoscapeConnectionClass', 

   function (obj, new.size) {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Edge Font Size', as.character (new.size))
     }) # setDefaultEdgeColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       node.shapes = rep (node.shapes, 2)
       }
     setDefaultNodeShape (obj, default.shape)
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setNodeShapeRule", id, node.attribute.name, default.shape, 
                      attribute.values, node.shapes, .convert=TRUE)
     invisible (result)
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

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeSizeRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultNodeSize (obj, default.size)

     if (mode=='interpolate') {  # need a 'below' size and an 'above' size.  so there should be two more colors than control.points 
       if (length (control.points) == length (node.sizes)) { # caller did not supply 'below' and 'above' values; manufacture them
         node.sizes = c (node.sizes [1], node.sizes, node.sizes [length (node.sizes)])
         write ("RCytoscape::setNodeSizeRule, no 'below' or 'above' sizes specified.  Inferred from node.sizes.", stderr ())
         } # 

       good.args = length (control.points) == (length (node.sizes) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (node.sizes)), stderr ())
         write ("Error! RCytoscape:setNodeSizeRule, interpolate mode.", stderr ())
         write ("Expecting 1 node.size for each control.point, one for 'above' size, one for 'below' size.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Size', control.points, node.sizes, FALSE)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (node.sizes)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (node.sizes)), stderr ())
         write ("Error! RCytoscape:setNodeSizeRule.  Expecting exactly as many node.sizes as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         node.sizes = rep (node.sizes, 2)
         } 

       result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
                         node.attribute.name, 'Node Size', as.character (default.size), 
                         as.character (control.points), as.character (node.sizes))
       invisible (result)
       } # else: !interpolate
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

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setEdgeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultEdgeColor (obj, default.color)
     if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
       if (length (control.points) == length (colors)) { # caller did not supply 'below' and 'above' values; manufacture them
         colors = c (colors [1], colors, colors [length (colors)])
         #write ("RCytoscape::setEdgeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
         } # 

       good.args = length (control.points) == (length (colors) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setEdgeColorRule, interpolate mode.", stderr ())
         write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousEdgeVisualStyle', edge.attribute.name, 'Edge Color', control.points, colors, FALSE)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (colors)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setEdgeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         colors = rep (colors, 2)
         } 
       result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', default.style, 
                         edge.attribute.name, 'Edge Color', default.color, control.points, colors)
       invisible (result)
       } # else: !interpolate
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       line.styles = rep (line.styles, 2)
       }
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeLineStyleRule", id, edge.attribute.name, default.style, 
                       attribute.values, line.styles, .convert=TRUE)
     invisible (result)
     }) # set.edge.line.style.rule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineWidthRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, attribute.values, line.widths, default.width=1) {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       }
     id = as.character (obj@window.id)
     visual.property.type.name = 'Edge Line Width'  # see class cytoscape.visual.VisualPropertyType

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       } 
     result = xml.rpc (obj@uri, 'Cytoscape.createDiscreteMapper', 'default', edge.attribute.name, 
                       'Edge Line Width', as.character (default.width), attribute.values, as.character (line.widths))
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
     # write (sprintf ('before -- attribute.values: %d   arrows: %d', length (attribute.values), length (arrows)), stderr ())
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       arrows = rep (arrows, 2)
       }
     #write (sprintf ('after -- attribute.values: %d   arrows: %d', length (attribute.values), length (arrows)), stderr ())
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeTargetArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       arrows = rep (arrows, 2)
       }
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeSourceArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
     id = as.character (obj@window.id)
     style.name = 'default'

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       colors = rep (colors, 2)
       } 

     result = xml.rpc (obj@uri, "Cytoscape.createDiscreteMapper", style.name, edge.attribute.name,
                      'Edge Target Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
     id = as.character (obj@window.id)
     style.name = 'default'

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       colors = rep (colors, 2)
       } 

     result = xml.rpc (obj@uri, "Cytoscape.createDiscreteMapper", style.name, edge.attribute.name,
                      'Edge Source Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.color) {
     id = as.character (obj@window.id)
     if (length (node.names) == 1)
       node.names = rep (node.names, 2)
     converted.color = hexColorToInt (new.color)
     if (length (converted.color) == 1 && is.na (converted.color)) {
       write (sprintf ('illegal color string "%s" in RCytoscape::setNodeColorDirect'), stderr ())
       return ()
       }
     result = xml.rpc (obj@uri, "Cytoscape.setNodeFillColor", id, node.names,
                       converted.color$red, converted.color$green, converted.color$blue, FALSE)
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeSizeDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.sizes) {
     id = as.character (obj@window.id)
     property.name = 'Node Size'
     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.sizes [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.sizes) == 1)
         new.sizes = rep (new.sizes, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.sizes))
       } # else: multiple nodes
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeWidthDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.widths) {

     id = as.character (obj@window.id)

     property.name = 'Node Width'
     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.widths [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.widths) == 1)
         new.widths = rep (new.widths, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.widths))
       } # else: multiple nodes
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
# only works if node dimensions are not locked (that is, tied together).  see lockNodeDimensions (T/F)
setMethod ('setNodeHeightDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.heights) {

     id = as.character (obj@window.id)

     property.name = 'Node Height'
     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.heights [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.heights) == 1)
         new.heights = rep (new.heights, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.heights))
       } # else: multiple nodes
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.labels) {

     id = as.character (obj@window.id)

     property.name = 'Node Label'

     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.labels [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.labels) == 1)
         new.labels = rep (new.labels, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.labels))
       } # else: multiple nodes
     invisible (result)
     })


#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeFontSizeDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.sizes) {
     id = as.character (obj@window.id)

     property.name = 'Node Font Size'

     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.sizes [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.sizes) == 1)
         new.sizes = rep (new.sizes, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.sizes))
       } # else: multiple nodes
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelColorDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.colors) {

     id = as.character (obj@window.id)
     property.name = 'Node Label Color'

     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.colors [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.colors) == 1)
         new.colors = rep (new.colors, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.colors))
       } # else: multiple nodes
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.shapes) {
     id = as.character (obj@window.id)

     property.name = 'Node Shape'

     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, property.name, as.character (new.shapes [1]))
     else {
       properties = rep (property.name, length (node.names))
       if (length (new.shapes) == 1)
         new.shapes = rep (new.shapes, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.shapes))
       } # else: multiple nodes
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeImageDirect', 'CytoscapeWindowClass',

  function (obj, node.names, image.urls) {

     if (length (image.urls) == 1)
       image.urls = rep (image.urls, length (node.names))

     if (length (node.names) != length (image.urls)) {
       msg = sprintf ('error in RCytoscape::setNodeImageDirect.  image.urls count (%d) is neither 1 nor same as node.names count (%d)',
                      length (image.urls), length (node.names))
       write (msg, stderr ())
       return ()
       }
    
     id = as.character (obj@window.id)
     for (i in 1:length (node.names)) {
       setNodeShapeDirect (obj, node.names [i], 'rect')
       setNodeLabelDirect (obj, node.names [i], '')
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names [i], 'Node Custom Graphics 1', image.urls [i])
       }
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.sizes) {

     id = as.character (obj@window.id)
     if (length (node.names) == 1)
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.names, 'Node Line Width', as.character (new.sizes [1]))
     else {
       properties = rep ('Node Line Width', length (node.names))
       if (length (new.sizes) == 1)
         new.sizes = rep (new.sizes, length (node.names))
       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.sizes))
       } # else: multiple nodes
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.color) {
     id = as.character (obj@window.id)
     if (length (node.names) == 1)
       node.names = rep (node.names, 2)
     converted.color = hexColorToInt (new.color)
     if (length (converted.color) == 1 && is.na (converted.color)) {
       write (sprintf ('illegal color string "%s" in RCytoscape::setNodeBorderColorDirect'), stderr ())
       return ()
       }
     result = xml.rpc (obj@uri, "Cytoscape.setNodeBorderColor", id, node.names,
                       converted.color$red, converted.color$green, converted.color$blue, FALSE)
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeOpacityDirect', 'CytoscapeWindowClass',
   function (obj, node.names, new.values) {
     
  if (length (node.names) == 1) {
    names = rep (node.names [1], 3)
    values = rep (new.values [1], 3)
    properties = c ('Node Opacity', 'Node Border Opacity', 'Node Label Opacity') 
    target.function = "Cytoscape.setNodeProperties"
    result = xml.rpc (obj@uri, target.function, names, properties, as.character (values))
    invisible (result)
    } # 1 name only
  else {
    count = length (node.names)
    if (length (new.values) == 1)  # apply this one value to all nodes or edges
      values = rep (new.values, 3 * count)
    else 
      values = c (rep (new.values, 3))
    properties = c (rep ('Node Opacity', count), rep ('Node Border Opacity', count), rep ('Node Label Opacity', count))
    names = rep (node.names, 3)
    #print (names)
    #print (properties)
    #print (as.character (values))
    target.function = "Cytoscape.setNodeProperties"
    result = xml.rpc (obj@uri, target.function, names, properties, as.character (values))
    #print ('--- back from xml.rpc')
    invisible (result)
    } # else: 
    })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setNodeOpacitiesDirect', 'CytoscapeWindowClass',
#   function (obj, node.names, new.values) {
#     id = as.character (obj@window.id)
#     result = setNodeFillOpacitiesDirect (obj, node.names, new.values)
#     result = setNodeLabelOpacitiesDirect (obj, node.names, new.values)
#     result = setNodeBorderOpacitiesDirect (obj, node.names, new.values)
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeFillOpacityDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.values) {
     property.name = 'Node Opacity'
     host.uri = obj@uri
     set.node.or.edge.properties (host.uri, property.name, node.names, new.values)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderOpacityDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.values) {
     property.name = 'Node Border Opacity'
     host.uri = obj@uri
     set.node.or.edge.properties (host.uri, property.name, node.names, new.values)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelOpacityDirect', 'CytoscapeWindowClass',

   function (obj, node.names, new.values) {
     property.name = 'Node Label Opacity'
     host.uri = obj@uri
     set.node.or.edge.properties (host.uri, property.name, node.names, new.values)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeOpacityDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
     property.names = c ('Edge Opacity',  'Edge Source Arrow Opacity', 'Edge Target Arrow Opacity')
     host.uri = obj@uri
     for (property.name in property.names)
       set.node.or.edge.properties (host.uri, property.name, edge.names, new.values)
     })

#------------------------------------------------------------------------------------------------------------------------
set.node.or.edge.properties = function (host.uri, property.name, names, values)
{
  if (length (names) == 1) {
    name = names [1]
    value = values [1]
    if (length (grep ('Node', property.name) > 0))
      target.function = "Cytoscape.setNodeProperty"
    else {
      target.function = "Cytoscape.setEdgeProperty"
      name = as.character (name)   # either a nop, or a kindness to callers who pass in the full output of cy2.edge.names, a list
      }
    result = xml.rpc (host.uri, target.function, name, property.name , as.character (value))
    invisible (result)
    } # 1 name only
  else {
    if (length (values) == 1)  # apply this one value to all nodes or edges
      values = rep (values, length (names))
    properties = rep (property.name, length (names))
    #print ('--- set.node.or.edge..properties')
    #print (names)
    #print (properties)
    #print (as.character (values))
    if (length (grep ('Node', property.name) > 0))
      target.function = "Cytoscape.setNodeProperties"
    else {
      target.function = "Cytoscape.setEdgeProperties"
      names = as.character (names)   # either a nop, or a kindness to callers who pass in the full output of cy2.edge.names, a list
      }
    result = xml.rpc (host.uri, target.function, names, properties, as.character (values))
    #print ('--- back from xml.rpc')
    invisible (result)
    } # else: 

} # set.node.or.edge.properties
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setNodeFillOpacitiesDirect', 'CytoscapeWindowClass',
#   function (obj, node.names, new.values) {
#     id = as.character (obj@window.id)
#     if (length (new.values) == 1)
#       new.values = rep (new.values, length (node.names))
#     properties = rep ('Node Opacity', length (node.names))
#     result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.values))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setNodeBorderOpacityDirect', 'CytoscapeWindowClass',
#   function (obj, node.names, new.value) {
#     id = as.character (obj@window.id)
#     for (node.name in node.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.name, 'Node Border Opacity', as.character (new.value))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setNodeBorderOpacitiesDirect', 'CytoscapeWindowClass',
#   function (obj, node.names, new.values) {
#     id = as.character (obj@window.id)
#     if (length (new.values) == 1)
#       new.values = rep (new.values, length (node.names))
#     properties = rep ('Node Border Opacity', length (node.names))
#     result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.values))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setNodeLabelOpacityDirect', 'CytoscapeWindowClass',
#   function (obj, node.names, new.value) {
#     id = as.character (obj@window.id)
#     for (node.name in node.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setNodeProperty", node.name, 'Node Label Opacity', as.character (new.value))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setNodeLabelOpacitiesDirect', 'CytoscapeWindowClass',
#   function (obj, node.names, new.values) {
#     id = as.character (obj@window.id)
#     if (length (new.values) == 1)
#       new.values = rep (new.values, length (node.names))
#     properties = rep ('Node Label Opacity', length (node.names))
#     result = xml.rpc (obj@uri, "Cytoscape.setNodeProperties", node.names, properties, as.character (new.values))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeOpacityDirect', 'CytoscapeWindowClass',
#
#   function (obj, edge.names, new.values) {
#
#     id = as.character (obj@window.id)
#     if (length (edge.names) == 1) {
#       edge.name = edge.names [1]
#       new.value = new.values [1]
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Opacity', as.character (new.value))
#       invisible (result)
#       } # 1 edge.name only
#     else {
#       if (length (new.values) == 1)
#         new.values = rep (new.values, length (edge.names))
#       properties = rep ('Edge Opacity', length (edge.names))
#       print ('--- setEdgeOpacitiesDirect')
#       print (edge.names)
#       print (properties)
#       print (as.character (new.values))
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperties", edge.names, properties, as.character (new.values))
#       print ('--- back from xml.rpc')
#       invisible (result)
#       } # multiple edges
#     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeOpacitiesDirect', 'CytoscapeWindowClass',
#   function (obj, edge.names, new.values) {
#     id = as.character (obj@window.id)
#     if (length (new.values) == 1)
#       new.values = rep (new.values, length (edge.names))
#     properties = rep ('Edge Opacity', length (edge.names))
#     print ('--- setEdgeOpacitiesDirect')
#     print (edge.names)
#     print (properties)
#     print (as.character (new.values))
#     result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperties", edge.names, properties, as.character (new.values))
#     print ('--- back from xml.rpc')
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Color', as.character (new.value))
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label', as.character (new.value))
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
#setMethod ('setEdgeFontFaceDirect', 'CytoscapeWindowClass',
#   function (obj, edge.names, new.value) {
#     id = as.character (obj@window.id)
#     for (edge.name in edge.names)
#       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Font Face', as.character (new.value))
#     invisible (result)
#     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeFontSizeDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Font Size', as.character (new.value))
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelColorDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Color', as.character (new.value))
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.values) {
     id = as.character (obj@window.id)

     if (length (new.values) == 1)
       new.values = rep (new.values, length (edge.names))
     
     if (length (edge.names) != length (new.values)) {
       msg = sprintf ('error in RCytoscape::setEdgeTooltipDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.values), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names))
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Tooltip', as.character (new.values [i]))
 
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineWidthDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Line Width', as.character (new.value))
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
     id = as.character (obj@window.id)

     if (length (new.values) == 1)
       new.values = rep (new.values, length (edge.names))
     
     if (length (edge.names) != length (new.values)) {
       msg = sprintf ('error in RCytoscape::setEdgeLineStyleDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.values), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names))
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Line Style', as.character (new.values [i]))
 
    invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowShapeDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
     id = as.character (obj@window.id)

     if (length (new.values) == 1)
       new.values = rep (new.values, length (edge.names))
     
     if (length (edge.names) != length (new.values)) {
       msg = sprintf ('error in RCytoscape::setEdgeSourceArrowShapeDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.values), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names)) 
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Source Arrow Shape', as.character (new.values [i]))

     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowShapeDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {
     id = as.character (obj@window.id)

     if (length (new.values) == 1)
       new.values = rep (new.values, length (edge.names))
     
     if (length (edge.names) != length (new.values)) {
       msg = sprintf ('error in RCytoscape::setEdgeTargetArrowShapeDirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.values), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names)) 
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Target Arrow Shape', as.character (new.values [i]))

     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.colors) {

     id = as.character (obj@window.id)

     if (length (new.colors) == 1)
       new.colors = rep (new.colors, length (edge.names))

     if (length (edge.names) != length (new.colors)) {
       msg = sprintf ('error in RCytoscape::setEdgeSourceArrowColorDirect.  new.colors count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.colors), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names))
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Source Arrow Color', as.character (new.colors [i]))

     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.colors) {

     id = as.character (obj@window.id)

     if (length (new.colors) == 1)
       new.colors = rep (new.colors, length (edge.names))

     if (length (edge.names) != length (new.colors)) {
       msg = sprintf ('error in RCytoscape::setEdgeTargetArrowColorDirect.  new.colors count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.colors), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names))
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Target Arrow Color', as.character (new.colors [i]))

     invisible (result)
     })


#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLabelOpacityDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.value) {
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Opacity', as.character (new.value))
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowOpacityDirect', 'CytoscapeWindowClass',
   function (obj, edge.names, new.values) {

     id = as.character (obj@window.id)

     if (length (new.values) == 1)
       new.values = rep (new.values, length (edge.names))
     
     if (length (edge.names) != length (new.values)) {
       msg = sprintf ('error in RCytoscape::setEdgeSourceArrowOpacityirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.values), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names))
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Source Arrow Opacity', as.character (new.values [i]))

     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowOpacityDirect', 'CytoscapeWindowClass',

   function (obj, edge.names, new.values) {

     id = as.character (obj@window.id)

     if (length (new.values) == 1)
       new.values = rep (new.values, length (edge.names))
     
     if (length (edge.names) != length (new.values)) {
       msg = sprintf ('error in RCytoscape::setEdgeTargetArrowOpacityirect.  new.values count (%d) is neither 1 nor same as edge.names count (%d)',
                      length (new.values), length (edge.names))
       write (msg, stderr ())
       return ()
       }

     for (i in 1:length (edge.names))
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.names [i], 'Edge Target Arrow Opacity', as.character (new.values [i]))

     invisible (result)
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
     id = as.character (obj@window.id)
     for (edge.name in edge.names)
       result = xml.rpc (obj@uri, "Cytoscape.setEdgeProperty", edge.name, 'Edge Label Width', as.character (new.value))
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeCount', 'CytoscapeWindowClass',
   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countNodes", id)
     return (count)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeCount', 'CytoscapeWindowClass',
   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countEdges", id)
     return (count)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeAttribute', 'CytoscapeConnectionClass',

   function (obj, node.name, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.getNodeAttribute", node.name, attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllNodeAttributes', 'CytoscapeWindowClass',

  function (obj, onlySelectedNodes=FALSE) {
    g = obj@graph
    attribute.names = names (nodeDataDefaults (g))
    nodes.of.interest = nodes (g)
    if (onlySelectedNodes) {
      if (getSelectedNodeCount (obj) == 0)
        return (NA)
      nodes.of.interest = getSelectedNodes (obj)
      }
  
    result = cbind (unlist (nodeData (g, nodes.of.interest, attr=attribute.names [1])))
    
    if (length (attribute.names) > 1) {
      for (name in attribute.names [2:length (attribute.names)]) {
        new.column = unlist (nodeData (g, nodes.of.interest, attr=name))
        if (is.null (new.column))
          new.column = rep ('NULL', nrow (result))
        result = cbind (result, new.column)
        } # for name
      } # if length > 1
  
    colnames (result) = attribute.names
    result = as.data.frame (result, stringsAsFactors=FALSE)
  
    for (name in attribute.names) {
      attribute.class = attr (nodeDataDefaults (obj@graph, name), 'class')
      if (attribute.class == 'FLOATING')
        result [, name] = as.numeric (result [, name])
      else if (attribute.class == 'STRING')
        result [, name] = as.character (result [, name])
      else if (attribute.class == 'INTEGER')
        result [, name] = as.integer (result [, name])
      } # for name
  
    return (result)

    }) # getAllNodeAttributes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeAttribute', 'CytoscapeConnectionClass',

   function (obj, edge.name, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.getEdgeAttribute", edge.name, attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdgeAttributes', 'CytoscapeWindowClass',

  function (obj, onlySelectedEdges=FALSE) {

    g = obj@graph
    attribute.names = names (edgeDataDefaults (g))
    edges.of.interest = edgeNames (g)
    if (onlySelectedEdges) {
      if (getSelectedEdgeCount (obj) == 0)
        return (NA)
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
    if (length (attribute.names) > 1)
      column.names = c (column.names, attribute.names [2:length(attribute.names)])

    colnames (result) = column.names
    result = as.data.frame (result, stringsAsFactors=FALSE)
    
       # we had a matrix of character strings, now a data.frame of character strings
       # use the embedded type information (created by initEdgeAttribute) to correct to the proper types
       # must be a more direct way to do this in the calls to cbind on a data.frame.
    
    for (name in attribute.names) {
      attribute.class = attr (edgeDataDefaults (obj@graph, name), 'class')
      if (attribute.class == 'FLOATING')
        result [, name] = as.numeric (result [, name])
      else if (attribute.class == 'STRING')
        result [, name] = as.character (result [, name])
      else if (attribute.class == 'INTEGER')
        result [, name] = as.integer (result [, name])
      } # for name
    
    return (result)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeAttributeNames', 'CytoscapeConnectionClass',

   function (obj) {
     return (xml.rpc (obj@uri, "Cytoscape.getNodeAttributeNames"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeAttributeNames', 'CytoscapeConnectionClass',

   function (obj) {
     return (xml.rpc (obj@uri, "Cytoscape.getEdgeAttributeNames"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteNodeAttribute', 'CytoscapeConnectionClass',

   function (obj, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.deleteNodeAttribute", attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteEdgeAttribute', 'CytoscapeConnectionClass',

   function (obj, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.deleteEdgeAttribute", attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countNodes", id)
     if (count == 0)
       return ()
     result = xml.rpc (obj@uri, "Cytoscape.getNodes", id, .convert=TRUE)
     return (result)
     }) # getAllNodes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countEdges", id)
     if (count == 0)
       return ()
     result = xml.rpc (obj@uri, "Cytoscape.getEdges", id, .convert=TRUE)
     #result = xml.rpc (obj@uri, "Cytoscape.getAllEdges", .convert=TRUE)
     return (result)
     }) # getAllEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod ('clearSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.clearSelection', id, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # clearSelection
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('selectNodes', 'CytoscapeWindowClass',

   function (obj, node.names, preserve.current.selection=TRUE) {
     id = as.character (obj@window.id)
     if (preserve.current.selection)
       if (getSelectedNodeCount (obj) > 0)
         node.names = unique (c (getSelectedNodes (obj), node.names))
     if (!preserve.current.selection)
        clearSelection (obj)

     #write (sprintf ('selectNodes, node.names (%d): %s', length (node.names), list.to.string (node.names)), stderr ())

     result = xml.rpc (obj@uri, 'Cytoscape.selectNodes',id, node.names, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # selectNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedNodeCount', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.countSelectedNodes', id, .convert=TRUE))
     }) # countSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     if (getSelectedNodeCount (obj) == 0)
       return (NA)
     else {
       result = xml.rpc (obj@uri, 'Cytoscape.getSelectedNodes', id, .convert=TRUE)
       return (result)
       }
     }) # getSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedNodes', id, .convert=TRUE))
     }) # hideSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideNodes', 'CytoscapeWindowClass',

   function (obj, node.names) {
     id = as.character (obj@window.id)
     for (node in node.names)
       invisible (xml.rpc (obj@uri, 'Cytoscape.hideNode', id, node, .convert=TRUE))
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
#------------------------------------------------------------------------------------------------------------------------
setMethod ('invertNodeSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.invertNodeSelection', id, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # invertNodeSelection
 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     delete.from.root.graph.also=TRUE
     invisible (xml.rpc (obj@uri, 'Cytoscape.removeSelectedNodes', id, delete.from.root.graph.also, .convert=TRUE))
     }) # deleteSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('selectEdges', 'CytoscapeWindowClass',

   function (obj, edge.names, preserve.current.selection=TRUE) {
     id = as.character (obj@window.id)
     if (preserve.current.selection)
       edge.names = unique (c (getSelectedEdges (obj), edge.names))
     if (length (edge.names) == 1)
       result = xml.rpc (obj@uri, 'Cytoscape.selectEdge', id, edge.names, .convert=TRUE)
     else
       result = xml.rpc (obj@uri, 'Cytoscape.selectEdges', id, edge.names, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # selectEdges
 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('invertEdgeSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.invertEdgeSelection', id, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # invertEdgeSelection
 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     delete.from.root.graph.also = TRUE
     invisible (xml.rpc (obj@uri, 'Cytoscape.removeSelectedEdges', id, delete.from.root.graph.also, .convert=TRUE))
     }) # deleteSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdgeCount', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.countSelectedEdges', id, .convert=TRUE))
     }) # countSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     if (getSelectedEdgeCount (obj) == 0)
       return (NA)
     else {
       result = xml.rpc (obj@uri, 'Cytoscape.getSelectedEdges', id, .convert=TRUE)
       return (result)
       }
     }) # getSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedEdges', id, .convert=TRUE))
     }) # hideSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------
setMethod ('unhideAll', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.unhideAll', id, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # unhideAll
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getFirstNeighbors', 'CytoscapeWindowClass',

   function (obj, node.names) {
       if (length (node.names) == 0)
         invisible ()
       if (length (node.names) == 1)
         neighbors = xml.rpc (obj@uri, 'Cytoscape.getNodeNeighbors', node.names)
       else
         neighbors = xml.rpc (obj@uri, 'Cytoscape.getNodesNeighbors', node.names)
      return (neighbors)
      })  # getFirstNeighbors

#------------------------------------------------------------------------------------------
setMethod ('selectFirstNeighborsOfSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     if (getSelectedNodeCount (obj) > 0) {
       currently.selected = getSelectedNodes (obj)
       if (length (currently.selected) == 0)
         invisible ()
       neighbors = getFirstNeighbors (obj, currently.selected)
       full.selection = unique (c (currently.selected, neighbors))
       selectNodes (obj, full.selection)
       invisible (full.selection)
       } # if any nodes are already selected
     }) # selectFirstNeighborsOfSelectedNodes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sfn', 'CytoscapeWindowClass',

   function (obj) {
     selectFirstNeighborsOfSelectedNodes (obj)
     })

#------------------------------------------------------------------------------------------------------------------------
noa.names = function (graph)
{
  return (names (nodeDataDefaults (graph)))
}
#------------------------------------------------------------------------------------------------------------------------
eda.names = function (graph)
{
  return (names (edgeDataDefaults (graph)))
}
#------------------------------------------------------------------------------------------------------------------------
noa = function (graph, node.attribute.name)
{
  if (!node.attribute.name %in% noa.names (graph))
    return (NA)

  return (unlist (nodeData (graph, attr=node.attribute.name)))

} # noa
#------------------------------------------------------------------------------------------------------------------------
eda = function (graph, edge.attribute.name)
{
  if (!edge.attribute.name %in% eda.names (graph))
    return (NA)

  return (unlist (edgeData (graph, attr=edge.attribute.name)))

} # eda
#------------------------------------------------------------------------------------------------------------------------
#  use the expected 'edgeType' attribute to create cytoscape-style 'A (edgeType) B' edge names from a graphNEL
#  edgeNames (g) # "A~B" "B~C" "C~A"
#  if there is no edge attribute named 'edgeType', then create edges (uninterestingly) named 'A (edge) B'
cy2.edge.names = function (graph, R.edge.names=NA)
{
  printf ('c.e.n: %d', 0)
  if (length (edges (graph)) == 0)
    return (NA)

  printf ('c.e.n: %d', 1)
  edgeType.attribute.present = TRUE
  printf ('c.e.n: %d', 2)
  edge.type = 'unspecified'
  printf ('c.e.n: %d', 3)
  if ('edgeType' %in% names (edgeDataDefaults (graph))) {
    edge.type = as.character (eda (graph, 'edgeType'))
    }

  printf ('c.e.n: %d', 4)
  rcy.edgeNames = .rcyEdgeNames (graph)
  tokens = strsplit (rcy.edgeNames, '~')
  printf ('c.e.n: %d', 5)
  a = sapply (tokens, function (tok) tok [1])
  printf ('c.e.n: %d', 6)
  b = sapply (tokens, function (tok) tok [2])
  printf ('c.e.n: %d', 7)
  edge.type = paste (' (', edge.type, ') ', sep='')
  printf ('c.e.n: %d', 8)
  edge.names = paste (a, edge.type, b, sep='')
  printf ('c.e.n: %d', 9)

  names (edge.names) = rcy.edgeNames
  printf ('c.e.n: %d', 10)

  if (!(length (R.edge.names) == 1 && is.na (R.edge.names))) {  # we were given some subset of all edges to extract and get cy2 names for.  do that here
  printf ('c.e.n: %d', 11)
    new.edgeNames.tilde = gsub ('\\|', '~', R.edge.names)
    if (length (intersect (names (edge.names), new.edgeNames.tilde)) > 0)
      edge.names = edge.names [new.edgeNames.tilde]
    }
  printf ('c.e.n: %d', 12)

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

  g.simple = RCytoscape::makeSimpleGraph ()
  cws = new.CytoscapeWindow (window.title, g.simple)

  displayGraph (cws)
  layoutNetwork (cws, 'jgraph-spring')
  setNodeLabelRule (cws, 'label')

  node.attribute.values = c ("kinase",  "transcription factor")
  colors =                c ('#A0AA00', '#FF0000')
  setDefaultNodeBorderWidth (cws, 5)
  setNodeBorderColorRule (cws, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cws, 'count', count.control.points, sizes, mode='interpolate')
  setNodeColorRule (cws, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')

  #redraw (cws)

  invisible (cws)

} # demoSimpleGraph
#------------------------------------------------------------------------------------------------------------------------
makeRandomGraph = function (node.count=12, seed = 123)
{
  set.seed (seed); 
  #if (node.count > 26) node.count = 26
  node.names = as.character (1:node.count)
  g = randomGraph (node.names, M <- 1:2, p = 0.6)
  attr (edgeDataDefaults (g, attr="weight"), "class") = "DOUBLE"
  edgeDataDefaults (g, 'pmid') = '9988778899'
  attr (edgeDataDefaults (g, attr="pmid"), "class") = "STRING"
  return (g)

} # makeRandomGraph
#------------------------------------------------------------------------------------------------------------------------
# the bioconductor graph class stores undirected graph edge attributes redundantly.  bioc's nishant says (email, 2 sep 2010):
#
# The people who started the graph package decided to return duplicate edge attributes / weights for the undirected
# case. ie if you have an edge a-b and the graph is undirected, methods such as edgeWeights, edgeData etc will end up
# returning duplicate values for the attribute for a-b and b-a.  That was a design decision taken by the creators of the
# package and I do not think it will be possible to change that now.  I guess the solution might be to create your own
# edgeWeights and edgeData methods in your package that retrieve only the non-duplicated attributes for the undirected
# case.
#
remove.redundancies.in.undirected.graph = function (gu)
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

} # remove.redundancies.in.undirected.graph 
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
  caller.specified.attribute.class = attr (nodeDataDefaults (other.graph, attribute.name), 'class')

  if (is.null (caller.specified.attribute.class)) {
    msg1 = sprintf ('Error!  RCytoscape:::.sendNodeAttributesForGraph. You must initialize the "%s" node attribute.', attribute.name)
    msg2 = sprintf ('        example:  my.graph = initNodeAttribute (my.graph, attr="moleculeType", "char", "unspecified")')
    write (msg1, stderr ())
    write (msg2, stderr ())
    return (NA)
    }

     # only add attributes for new nodes, unique to the new graph 'other.graph'
   new.node.names = setdiff (nodes (other.graph), nodes (obj@graph))
   values = noa (other.graph, attribute.name) [new.node.names]
   invisible (setNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, new.node.names, values))

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
  printf ('--- .sendEdgeAttributesForGraph')
  caller.specified.attribute.class = attr (edgeDataDefaults (other.graph, attribute.name), 'class')

  if (is.null (caller.specified.attribute.class)) {
    msg1 = sprintf ('Error!  RCytoscape:::.sendEdgeAttributesForGraph. You must initialize the "%s" edge attribute.', attribute.name)
    msg2 = sprintf ('        example:  my.graph = initEdgeAttribute (my.graph, attr="edgeType", "char", "unspecified")')
    write (msg1, stderr ())
    write (msg2, stderr ())
    return (NA)
    }

     # send only attributes for edges which are unique to other.graph; we assume that any existing edges already have their attributes

   printf ('other.graph: %d', length (edgeNames (other.graph)))
   printf ('obj@graph: %d', length (edgeNames (obj@graph)))

   novel.edges = .getNovelEdges (obj@graph, other.graph)

   if (length (novel.edges) == 0)
     return ()

   new.edge.names.compact = names (novel.edges)
   new.edge.names.cy2.style = as.character (novel.edges)

   #print ('--- new.edge.names.compact')
   #print (new.edge.names.compact)

   #print ('--- new.edge.names.cy2.style')
   #print (new.edge.names.cy2.style)

   #new.edge.names.cy2.style = setdiff (as.character (cy2.edge.names (other.graph)), as.character (cy2.edge.names (obj@graph)))

   new.edge.names.with.bar.delimitor = gsub ('~', '|', new.edge.names.compact)
   values = eda (other.graph, attribute.name) [new.edge.names.with.bar.delimitor]
   #write (sprintf ('sending edge attributes direct for attr %s', attribute.name), stderr ())
   #write (new.edge.names.compact, stderr ())
   #write (new.edge.names.with.bar.delimitor, stderr ())
   #write ('---- new.edge.names.cy2.style', stderr ())
   #write (new.edge.names.cy2.style, stderr ())
   #write (values, stderr ())

   invisible (setEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, new.edge.names.cy2.style, values))

} # .sendEdgeAttributesForGraph 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getVisualStyleNames', 'CytoscapeConnectionClass',

  function (obj) {
    result = xml.rpc (obj@uri, 'Cytoscape.getVisualStyleNames')
    return (result)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyVisualStyle', 'CytoscapeConnectionClass',

  function (obj, from.style, to.style) {
    current.names = getVisualStyleNames (obj)
    if (! from.style %in% current.names)
      stop (sprintf ('Cannot copy from a non-existent visual style (%s)', from.style))
    xml.rpc (obj@uri, 'Cytoscape.copyVisualStyle', from.style, to.style)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setVisualStyle', 'CytoscapeConnectionClass',

  function (obj, new.style.name) {
    current.names = getVisualStyleNames (obj)
    if (! new.style.name %in% current.names)
      stop (sprintf ('Cannot call setVisualStyle on a non-existent visual style (%s)', new.style.name))
    xml.rpc (obj@uri, 'Cytoscape.setVisualStyle', new.style.name)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('lockNodeDimensions', 'CytoscapeConnectionClass',

   function (obj, new.state, visual.style.name='default') {
     if (! visual.style.name %in% getVisualStyleNames (obj)) {
       write (sprintf ('Error in RCytoscape::lockNodeDimensions.  No visual style named "%s"', visual.style.name), stderr ())
       return ()
       }
     invisible (xml.rpc (obj@uri, 'Cytoscape.setNodeSizeLocked', visual.style.name, new.state))
     }) # lockNodeDimensions

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultBackgroundColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultBackgroundColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultBackgroundColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultBackgroundColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultNodeSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultNodeSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultNodeReverseSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color,vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultNodeReverseSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultEdgeSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultEdgeSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultEdgeReverseSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultEdgeReverseSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveImage', 'CytoscapeWindowClass',

   function (obj, file.name, image.type, scale=1.0) {
     image.type = tolower (image.type)
     stopifnot (image.type %in% c ('png', 'pdf', 'svg'))
     id = as.character (obj@window.id)
     result = NA
     if (image.type == 'png')
        result = xml.rpc (obj@uri, 'Cytoscape.exportView', id, file.name, image.type, scale)
     else if (image.type == 'pdf') {
       if (length (grep ('1.8', pluginVersion (obj))) == 1)
         result = xml.rpc (obj@uri, 'Cytoscape.exportViewToPDF', id, file.name)
       else
         write ('saveImage to format pdf requires CytoscapeRPC.jar version 1.8, which is still being tested', stderr ())
       }
     else if (image.type == 'svg') {
       if (length (grep ('1.8', pluginVersion (obj))) == 1)
         result = xml.rpc (obj@uri, 'Cytoscape.exportViewToSVG', id, file.name)
       else
         write ('saveImage to format svg requires CytoscapeRPC.jar version 1.8, which is still being tested', stderr ())
       }
     invisible (result)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveNetwork', 'CytoscapeWindowClass',

   function (obj, file.name, format='gml') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.saveNetwork', id, file.name)
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
chad.debug = function (obj, msg)
{
  write ('special debug version for chad burrus', stderr ())
  write (msg, stderr ())
  write ('--- node attribute names', stderr ())

  node.attribute.names = noa.names (obj@graph)
  write (node.attribute.names, stderr ())

  for (noa.name in node.attribute.names) {
     if (noa.name == 'label') next
     write (sprintf (' ** %s', noa.name), stderr ())
     write (unlist (nodeData (obj@graph, attr=noa.name)), stderr ())
     }

} # chad.debug
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
# edges are stored variously and confusingly in graphNELs, maybe in other graph classes also
# this function takes a native bioc graph object, and returns a data frame with 3 columns:  A, B, and edgeType
.classicGraphToNodePairTable = function (g)
{
  nodes.list = edges (g)   # one named entry per node, each containing 0 or more partner (target) nodes

  edge.attribute.names = names (edgeDataDefaults (g))

  edgeType.supplied = TRUE

  if (!'edgeType' %in% edge.attribute.names) {
    edgeType.supplied = FALSE
    edgeType.default.value = 'unspecified'
    }
  else {
    edgeType.supplied = TRUE
    edgeTypes = edgeData (g, attr='edgeType')
    }

  template = list (source='', target='', edgeType='')
  tbl = data.frame (template, stringsAsFactors=F)
  for (source.node in names (nodes.list)) {
    target.nodes = nodes.list [[source.node]]
    if (length (target.nodes) == 0)
      next;
    for (target.node in target.nodes) {
      barred.edge.name = sprintf ('%s|%s', source.node, target.node)      
      if (edgeType.supplied)
        e.type = edgeTypes [[barred.edge.name]]
      else
        e.type = edgeType.default.value
      new.row = list (source=source.node, target=target.node, edgeType=e.type)
      tbl = rbind (tbl, new.row)
      } # for target.node
    } # for source.node

  return (tbl [-1,])

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
#------------------------------------------------------------------------------------------------------------------------
# the bioc graph 'edgeNames' function does not detect, distinguish or report reciprocal edges.
# this is fixed here.
.rcyEdgeNames = function (g)
{
  printf ('.rEN: %d', 1)
  nodes.list = edges (g)
  printf ('.rEN: %d', 2)
  result = c ()
  #printf ('.rEN: %d', 3)
  for (source.node in names (nodes.list)) {
    #printf ('.rEN: %d', 4)
    target.nodes = nodes.list [[source.node]]
    if (length (target.nodes) == 0)
      next;
    for (target.node in target.nodes) {
      #printf ('.rEN: %d', 5)
      tilde.edge.name = sprintf ('%s~%s', source.node, target.node)      
      result = c (result, tilde.edge.name)
      } # for target.node
    } # for source.node

  #printf ('.rEN: %d', 6)
  return (result)

} # .rcyEdgeNames
#------------------------------------------------------------------------------------------------------------------------
.getNovelEdges = function (g.old, g.new)
{
  if (length (edges (g.old)) == 0)
    gOld.edgeCount = 0
  else
    gOld.edgeCount = length (edgeNames (g.old))

  if (length (edges (g.new)) == 0)
    gNew.edgeCount = 0
  else
    gNew.edgeCount = length (edgeNames (g.new))

  #printf ('g.old: %d edges', gOld.edgeCount)
  #printf ('g.new: %d edges', gNew.edgeCount)

  if (gNew.edgeCount == 0)
    return (NA)

  if (gOld.edgeCount == 0)
    return (cy2.edge.names (g.new))

  old.edges = cy2.edge.names (g.old)
  new.edges = cy2.edge.names (g.new)
  novel.edges = setdiff (new.edges, old.edges)
  novel.edges.indices = match (novel.edges, as.character (new.edges))
  return (new.edges [novel.edges.indices])
  

} # .getNovelEdges
#------------------------------------------------------------------------------------------------------------------------
is.classic.graph = function (obj)
{
  obj.classes = is (obj)

  return ('graph' %in% obj.classes)

} # is.classic.graph
#------------------------------------------------------------------------------------------------------------------------
is.multiGraph = function (obj)
{
  obj.classes = is (obj)

  return ('MultiGraph' %in% obj.classes)

} # is.multiGraph
#------------------------------------------------------------------------------------------------------------------------
