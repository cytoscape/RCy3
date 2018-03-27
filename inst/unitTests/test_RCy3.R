# RCytoscape/inst/test_cytoscape.R
#-------------------------------------------------------------------------------
library (RCy3)
library (RUnit)

#-------------------------------------------------------------------------------
run.tests = function()
{
    options('warn'=0) # deprecation warnings (and others) are stored and reported
    
    # before doing anything else, make sure that the Cytoscape plugin version is one we can respond to
    test.app.version()
    
    # start with a clean slate, and no windows
    deleteAllNetworks()
    
    test.deleteNetwork ()
    test.deleteAllNetworks ()
    test.getNetworkSuid ()
    test.getNetworkName ()
    test.getNetworkList ()
    deleteAllNetworks ()

    test.getNodeShapes ()
    test.getArrowShapes ()
    test.getLineStyles ()
    test.getLayoutNames ()
    test.getLayoutNameMapping ()
    test.getLayoutPropertyNames ()
    test.getLayoutPropertyType ()
    test.getLayoutPropertyValue ()
    test.setLayoutProperties ()
    test.panelOperations ()

    test.showGraphicsDetails ()
    test.setNodeShapeDefault ()
    test.setNodeColorDefault ()
    test.setNodeSizeDefault ()
    test.setNodeBorderColorDefault ()
    test.setNodeBorderWidthDefault ()
    test.setNodeFontSizeDefault ()
    test.setNodeLabelColorDefault ()

    test.setEdgeLineWidthDefault ()
    test.setEdgeColorDefault ()
    test.setEdgeFontSizeDefault ()
    
    # test.setNodeLabelMapping ()
    # 
    # deleteAllNetworks ()
    # 
    # test.setEdgeLabelMapping ()
    # test.setNodeTooltipMapping ()
    # test.setEdgeTooltipMapping ()
    # test.setNodeColorMapping ()
    # 
    # deleteAllNetworks ()
    # 
    # test.setNodeBorderColorMapping ()
    # test.setNodeBorderWidthMapping ()
    # test.setNodeSizeMapping ()
    # test.setNodeShapeMapping ()
    # 
    # deleteAllNetworks ()
    # 
    # test.setNodeOpacityMapping ()
    # test.setNodeColorDirect ()
    # test.setNodeBorderColorDirect ()
    # test.setNodeLabelDirect () #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # test.setNodeLabelPropertiesDirect ()  #--> too slow
    # test.setNodeOpacityDirect ()  #--> too slow
    # #test.setEdgeOpacityDirect ()  #--> too slow
    # #test.setEdgeColorDirect ()  #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # test.setEdgeSourceArrowShapeDirect ()
    # test.setEdgeLabelDirect ()
    # #test.setEdgeFontSizeDirect ()  #--> too slow
    # #test.setEdgeLabelColorDirect ()  #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # test.setEdgeTooltipDirect ()
    # test.setEdgeLineWidthDirect ()
    # test.setEdgeLineStyleDirect ()
    # test.setEdgeSourceArrowShapeDirect ()
    # 
    # deleteAllNetworks ()
    # 
    # test.setEdgeTargetArrowShapeDirect ()
    # test.setEdgeSourceArrowColorDirect ()
    # test.setEdgeTargetArrowColorDirect ()
    # #test.setEdgeLabelOpacityDirect ()  #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # #test.setEdgeSourceArrowOpacityDirect ()
    # #test.setEdgeTargetArrowOpacityDirect ()
    # #test.setEdgeLabelPositionDirect ()  #--> too slow
    # #test.setEdgeLabelWidthDirect ()
    # test.countNodes ()
    # test.countEdges ()
    # 
    # deleteAllNetworks ()
    # 
    # test.countNodesAndEdgesInEmptyGraph ()
    # test.getAllNodes ()
    # test.getAllEdges ()
    # test.selectNodes ()
    # test.nodeNeighborReportingAndSelection ()
    # 
    # deleteAllNetworks ()
    # 
    # test.invertSelection ()
    # test.deleteSelectedNodes ()
    # test.hideNodes ()
    # test.selectEdges ()
    # test.setEdgeLineStyleMapping ()
    # 
    # deleteAllNetworks ()
    # 
    # test.setEdgeLineWidthMapping ()
    # test.setEdgeColorMapping ()
    # test.setEdgeTargetArrowMapping ()
    # test.setEdgeArrowColorMappings ()
    # test.setEdgeSourceArrowMapping ()
    # 
    # deleteAllNetworks ()
    # 
    # test.movie ()
    # #test.unmatchedAttributesError ()
    # #test.remove.redundancies.in.undirected.graph ()
    # test.randomUndirectedGraph ()
    # 
    # deleteAllNetworks ()
    # 
    # test.simpleGraph ()
    # test.simpleGraphWithReciprocalEdge ()
    # test.setGraph ()
    # test.setNodePosition ()  #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # test.getNodePosition ()
    # test.getNodePosition.colonInNodeName ()  #--> too slow
    # test.getNodeSize ()
    # test.haveNodeAttribute ()
    # 
    # deleteAllNetworks ()
    # 
    # test.haveEdgeAttribute ()
    # test.copyNodeAttributesFromCyGraph ()
    # test.copyEdgeAttributesFromCyGraph ()
    # test.getGraphFromCyNetwork ()
    # 
    # deleteAllNetworks ()
    # 
    # test.sendDegenerateGraphs ()
    # test.createNetworkFromSelection ()
    # test.addGraphToGraph ()
    # 
    # deleteAllNetworks ()
    # 
    # test.addGraphToGraph.degenerateFirstGraph ()
    # test.existing.CytoscapeNetwork ()
    # test.existing.CytoscapeNetwork.noEdges ()
    # test.existing.CytoscapeNetwork.emptyGraph ()
    # 
    # deleteAllNetworks ()
    # 
    # #test.getAttributeNames ()
    # test.addGetAndDeleteEdgeAttributes ()
    # test.addGetAndDeleteNodeAttributes ()
    # test.getAllNodeAttributes ()
    # 
    # deleteAllNetworks ()
    # 
    # test.getAllEdgeAttributes ()
    # test.getVisualStyleNames ()
    # #test.copyVisualStyle () # test passes alone but not during run.tests()??
    # #test.setVisualStyle () # to be commented in again
    # 
    # deleteAllNetworks ()
    # 
    # test.defaultColors ()
    # test.fitContent ()
    # #test.windowCoordinates ()
    # 
    # deleteAllNetworks ()
    # 
    # #test.zoom () # timeout
    # test.center ()
    # #test.setNodeSizeDirect ()  #--> too slow
    # #test.setNodeWidthAndHeightDirect ()  #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # test.setNodeFontSizeDirect ()  #--> too slow
    # test.setNodeShapeDirect ()  #--> too slow
    # #test.setEdgeVizPropertiesDirect ()  #--> too slow
    # #test.graphBAM ()
    # 
    # deleteAllNetworks ()
    # 
    # test.addCyNode ()
    # test.addCyEdge ()
    # test.twoGraphsDoubleEdges ()
    # test..classicGraphToNodePairTable ()
    # test.rcy.edgeNames ()
    # deleteAllNetworks ()
    # 
    # test..getNovelEdges ()
    # #test.setNodeImageDirect ()
    # #test.validity ()
    # #test.tooltip.delays ()
    
    #deleteAllNetworks()
    
    options('warn'=0)
    
} # run.tests
#-------------------------------------------------------------------------------
# almost every test needs to
#
#   1) announce it's name to stdout
#   2) delete any previous network with the same title, should any exist
#
# these services are provided here
#
test.prep = function (title, make.net=TRUE)
{
    write (noquote (sprintf ('------- %s', title)), stderr ())
    
    if(!make.net)
        return()
    
    if (title %in% as.character(getNetworkList())){
        deleteNetwork(title)
    }
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    return(unname(net.suid))
} 

#-------------------------------------------------------------------------------
test.app.version = function ()
{
    title = 'test.app.version'
    test.prep(title,F)
    app.version.string = cytoscapeVersionInfo()
    app.version.string = unname(app.version.string['apiVersion'])
    string.tmp = gsub ('[a-z]', '', app.version.string)
    major.minor.version = as.numeric (string.tmp)
    checkTrue (major.minor.version >= 1)
    
} 


#-------------------------------------------------------------------------------
test.deleteNetwork = function ()
{
    title = 'test.deleteNetwork'
    net.suid = test.prep(title)
    original.window.count = getNetworkCount ()
    deleteNetwork (net.suid)
    new.window.count = getNetworkCount ()
    checkTrue (new.window.count == original.window.count - 1)
    
    # now delete a network by name
    net.suid =test.prep (title)
    original.window.count = getNetworkCount ()
    deleteNetwork (getNetworkName(net.suid))
    new.window.count = getNetworkCount ()
    checkTrue (new.window.count == original.window.count - 1)
} 
#-------------------------------------------------------------------------------
test.deleteAllNetworks = function ()
{
    title = 'test.deleteAllNetworks'
    test.prep(title)
    createNetworkFromIgraph(makeSimpleIgraph(), title="another network")
    deleteAllNetworks ()
    new.window.count = getNetworkCount ()
    checkEquals (new.window.count, 0)
} 
#-------------------------------------------------------------------------------
test.getNetworkSuid = function ()
{
    title = 'test.getNetworkSuid'
    net.suid = test.prep (title)
    id = getNetworkSuid (title=title)
    checkEquals (id, net.suid)
} 
#-------------------------------------------------------------------------------
test.getNetworkName = function ()
{
    title = 'test.getNetworkName'
    net.suid = test.prep (title)
    name = getNetworkName (suid=unname(net.suid))
    checkEquals (name, title)
} 
#-------------------------------------------------------------------------------
test.getNetworkList = function ()
{
    title = 'test.getNetworkList'
    test.prep (title)
    net.list = getNetworkList ()
    checkTrue (title %in% as.character (net.list))
} 
#-------------------------------------------------------------------------------
test.getNodeShapes = function ()
{
    title = 'test.getNodeShapes'
    test.prep (title,F)
    shapes = getNodeShapes ()
    checkTrue (length (shapes) > 8)
    # pick a few specific shapes to test
    checkTrue (all (sapply (c ('HEXAGON', 'ELLIPSE', 'TRIANGLE'), function (s) s %in% shapes)))
    
} 
#-------------------------------------------------------------------------------
test.getTableColumnTypes = function ()
{
    title = 'test.getTableColumnTypes'
    test.prep(title,F)
    possible.values = getTableColumnTypes ()
    checkTrue (grep ('Integer', possible.values) > 0)
    checkTrue (grep ('String', possible.values) > 0)
    checkTrue (grep ('Boolean', possible.values) > 0)
} 
#-------------------------------------------------------------------------------
test.getArrowShapes = function ()
{
    title = 'test.getArrowShapes'
    test.prep(title,F)
    shapes = getArrowShapes ()
    checkTrue (length (shapes) >= 8)
    # pick a few specific shapes to test
    checkTrue (all (sapply (c ('DIAMOND', 'T', 'CIRCLE'), function (s) s %in% shapes)))
    
} 
#-------------------------------------------------------------------------------
test.getLineStyles = function ()
{
    title = 'test.getLineStyles'
    test.prep(title,F)
    styles = getLineStyles ()
    checkTrue (length (styles) > 10)
    # pick a few specific styles to test
    checkTrue (all (sapply (c ('SOLID', 'DOT', 'EQUAL_DASH'), function (s) s %in% styles)))
    
} 
#-------------------------------------------------------------------------------
test.getLayoutNames = function ()
{
    title = 'test.getLayoutNames'
    test.prep(title,F)
    names = getLayoutNames ()
    checkTrue (length (names) > 10)
    # pick a few specific styles to test
    checkTrue (all (sapply (c ('grid', 'isom', 'circular'), function (s) s %in% names)))
    
} 
#-------------------------------------------------------------------------------
test.getLayoutNameMapping = function ()
{
    title = 'test.getLayoutNameMapping'
    test.prep(title,F)
    name.map = getLayoutNameMapping ()
    checkTrue (length (name.map) >= 8)  
    checkEquals (name.map [['Attribute Circle Layout']], "attribute-circle")
    checkEquals (name.map [['Edge-weighted Spring Embedded Layout']], "kamada-kawai")
    checkEquals (name.map [['Grid Layout']], "grid")
} 
#-------------------------------------------------------------------------------
test.getLayoutPropertyNames = function ()
{
    title = 'test.getLayoutNameMapping'
    test.prep(title,F)
    props = getLayoutPropertyNames ('force-directed')
    expected = c ("numIterations", "defaultSpringCoefficient", "defaultSpringLength",
                  "defaultNodeMass", "isDeterministic", "singlePartition")
    checkTrue (length (intersect (props, expected)) > (length (props) - 2))  # some variation across Cytoscape versions
    
    props = getLayoutPropertyNames ('isom')
    expected = c ("coolingFactor", "initialAdaptation", "maxEpoch", "minAdaptation",
                  "minRadius", "radius", "radiusConstantTime", "singlePartition", "sizeFactor")
    checkEquals (sort (props), expected)
    
    
} 
#-------------------------------------------------------------------------------
test.getLayoutPropertyType = function ()
{
    title = 'test.getLayoutPropertyType'
    test.prep(title,F)
    
    checkEquals (getLayoutPropertyType ('isom', 'coolingFactor'), 'double')
    
    props = getLayoutPropertyNames ('force-directed')
    # now get all the property types for the force-directed layout
    propTypes.all = sapply (sort (props), function (prop) getLayoutPropertyType ('force-directed', prop))
    # check them all
    checkEquals (propTypes.all [["defaultNodeMass"]], "double")
    checkEquals (propTypes.all [["defaultSpringCoefficient"]], "double")
    checkEquals (propTypes.all [["defaultSpringLength"]], "double")
    checkEquals (propTypes.all [["numIterations"]], "int")
    checkEquals (propTypes.all [["singlePartition"]], "boolean")
    checkEquals (propTypes.all [["isDeterministic"]], "boolean")
    
} 
#-------------------------------------------------------------------------------
test.getLayoutPropertyValue = function ()
{
    title = 'test.getLayoutPropertyValue'
    test.prep(title,F)
    
    layout.name = 'force-directed'
    props = getLayoutPropertyNames (layout.name)
    
    for (prop in props) {
        value = getLayoutPropertyValue (layout.name, prop)
        prop.value.output <- sprintf ('force-directed layout %s: %s', prop, value)
        print (prop.value.output)
    } 
} 
#-------------------------------------------------------------------------------
test.setLayoutProperties = function ()
{
    title = 'test.setLayoutProperties'
    test.prep(title,F)
    layout.name = 'force-directed'
    prop = 'numIterations'
    setLayoutProperties (layout.name, list (numIterations=200))
    checkEquals (getLayoutPropertyValue (layout.name, prop), 200)
    
    # return to the defaults
    setLayoutProperties (layout.name, list (numIterations=100))
    checkEquals (getLayoutPropertyValue (layout.name, prop), 100)
} 

#-------------------------------------------------------------------------------
test.panelOperations = function ()
{
    title = 'test.panelOperations'
    test.prep (title,F)
    
    hidePanel ('control panel')
    hidePanel ('SOUTH')
    floatPanel ('c')
    floatPanel ('table')
    dockPanel ('control')
    dockPanel ('table panel')
    
    dockPanel ('results')
    dockPanel ('tool panel')
    floatPanel ('r')
    floatPanel ('tool')
    hidePanel ('results panel')
    hidePanel ('SOUTH_WEST')
    
} 

#-------------------------------------------------------------------------------
test.showGraphicsDetails = function ()
{
    title = 'test.showGraphicsDetails'
    net.suid = test.prep (title)

    toggleGraphicsDetails()
    toggleGraphicsDetails()
} 

#-------------------------------------------------------------------------------
test.setNodeShapeDefault = function ()
{
    title = 'test.setNodeShapeDefault'
    test.prep (title,F)
    
    shapes = getNodeShapes()
    
    for (shape in shapes) {
        setNodeShapeDefault(shape)
        Sys.sleep (0.3)
    } 
} 
#-------------------------------------------------------------------------------
test.setNodeColorDefault = function ()
{
    title = 'test.setNodeColorDefault'
    test.prep (title,F)

    setNodeColorDefault ('#AA00AA')
} 
#-------------------------------------------------------------------------------
test.setNodeSizeDefault = function ()
{
    title = 'test.setNodeSizeDefault'
    test.prep (title,F)
    
    setNodeSizeDefault (200)   
    setNodeSizeDefault (20)
} 
#-------------------------------------------------------------------------------
test.setNodeBorderColorDefault = function ()
{
    title = 'test.setNodeBorderColorDefault'
    test.prep (title,F)
    
    setNodeBorderColorDefault ('#FFFFFF')
    setNodeBorderColorDefault ('#FF0000')
}
#-------------------------------------------------------------------------------
test.setNodeBorderWidthDefault = function ()
{
    title = 'test.setNodeBorderWidthDefault'
    test.prep (title,F)
    
    setNodeBorderWidthDefault (5)
    setNodeBorderWidthDefault (1)
}
#-------------------------------------------------------------------------------
test.setNodeFontSizeDefault = function ()
{
    title = 'test.setNodeFontSizeDefault'
    test.prep (title,F)
    
    setNodeFontSizeDefault (12)
}
#-------------------------------------------------------------------------------
test.setNodeLabelColorDefault = function ()
{
    title = 'test.setNodeLabelColorDefault'
    test.prep (title,F)
    
    setNodeLabelColorDefault ('#FFAAAA')
}
#-------------------------------------------------------------------------------
test.setEdgeLineWidthDefault = function ()
{
    title = 'test.setEdgeLineWidthDefault'
    test.prep (title,F)
    
    setEdgeLineWidthDefault (10)
}
#-------------------------------------------------------------------------------
test.setEdgeColorDefault = function ()
{
    title = 'test.setEdgeColorDefault'
    test.prep (title,F)
    
    setEdgeColorDefault ('#FF0000')
}
#-------------------------------------------------------------------------------
test.setEdgeFontSizeDefault = function ()
{
    title = 'test.setEdgeFontSizeDefault'
    test.prep (title,F)
    
    setEdgeFontSizeDefault (12);
}
#-------------------------------------------------------------------------------
test.setNodeLabelMapping = function ()
{
    title = 'test.setNodeLabelMapping'
    test.prep (title,F)
    
    setNodeLabelMapping ('label')
    setNodeLabelMapping ('type')
    setNodeLabelMapping ('lfc')
    setNodeLabelMapping ('count')
}
#-------------------------------------------------------------------------------
test.setEdgeLabelMapping = function ()
{
    title = 'test.setEdgeLabelMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    setEdgeLabelMapping ('edgeType')
    setEdgeLabelMapping ('score')
    

    
}  # test.setEdgeLabelMapping
#-------------------------------------------------------------------------------
test.setNodeTooltipMapping = function ()
{
    title = 'test.setNodeTooltipMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    
    setNodeTooltipMapping ('type')
    

    
}  # test.setNodeTooltipMapping
#-------------------------------------------------------------------------------
test.setEdgeTooltipMapping = function ()
{
    title = 'test.setEdgeTooltipMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    setEdgeTooltipMapping ('edgeType')
    

    
}  # test.setEdgeTooltipMapping
#-------------------------------------------------------------------------------
test.setNodeColorMapping = function ()
{
    title = 'test.setNodeColorMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    
    # first, specify a mode='interpolate' Mapping -- the default
    node.attribute.values = c (-3.0, 0.0, 3.0)
    node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
    setNodeColorMapping ('lfc', node.attribute.values, node.colors, mode='interpolate')
    
    # now, a lookup Mapping
    node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
    node.colors =           c ('#8888FF', '#00F088',              "#00CCCC")
    setNodeColorMapping ('type', node.attribute.values, node.colors, mode='lookup')
    
    # now, a lookup Mapping with an incomplete lookup table:  does the default.color argument work?  cy2.7 bug -- not yet.
    # instead, the node is painted the cytoscape default color, pale red
    node.attribute.values = c ("kinase",  "transcription factor")
    node.colors = c ('#8888FF', '#00F088')
    setNodeColorMapping ('type', node.attribute.values, node.colors, mode='lookup', default.color='#AA33AA')
    
    # now, use 1 element lists.
    node.attribute.values = c ("kinase")
    node.colors = c ('#00AA88')
    setNodeColorMapping ('type', node.attribute.values, node.colors, mode='lookup', default.color='#AA33AA')
    

    
} # test.setNodeColorMapping
#-------------------------------------------------------------------------------
test.setNodeBorderColorMapping = function ()
{
    title = 'test.setNodeBorderColorMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    # set the stage by making all the nodes white, to provide better contrast for the node border colors
    node.attribute.values = c (-3.0, 0.0, 3.0)
    colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF')
    setNodeColorMapping ('lfc', node.attribute.values, colors, mode='interpolate')
    
    # first, specify a mode='interpolate' Mapping -- the default
    node.attribute.values = c (-3.0, 0.0, 3.0)
    colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
    setNodeBorderColorMapping ('lfc', node.attribute.values, colors, mode='interpolate')
    Sys.sleep (0.3)
    
    # now, a lookup Mapping.  bright red, green and blue borders
    node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
    colors =                c ('#FF0000', '#00FF00',              "#0000FF")
    setNodeBorderColorMapping ('type', node.attribute.values, colors, mode='lookup')
    Sys.sleep (0.3)
    
    # now, a lookup Mapping with an incomplete lookup table:  does the default.color argument work?  cy2.7 bug -- not yet.
    #  the glycoprotein node, 'Gene C', should have a white border around white fill
    node.attribute.values = c ("kinase",  "transcription factor")
    colors =                c ('#0000FF', '#FF0000')
    setNodeBorderColorMapping ('type', node.attribute.values, colors, mode='lookup', default.color='#FFFFFF')
    
    # now, one element lists
    node.attribute.values = c ("transcription factor")
    colors =                c ('#FF00FF')
    setNodeBorderColorMapping ('type', node.attribute.values, colors, mode='lookup', default.color='#FFFFFF')
    
    

    
} # test.setNodeBorderColorMapping
#-------------------------------------------------------------------------------
test.setNodeBorderWidthMapping = function ()
{
    title  = 'test.setNodeBorderWidthMapping'
    test.prep (title)
    cy = CytoscapeConnection ()
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    
    # set the stage by making all the nodes white, to provide better contrast for the node border colors
    node.attribute.values = c (-3.0, 0.0, 3.0)
    colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF')
    setNodeColorMapping ('lfc', node.attribute.values, colors, mode='interpolate')
    setNodeBorderColorDefault ('#FF0000')
    
    for (i in 1:3) {
        # 3 different node border sizes
        node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
        border.widths =         c (0, 10, 20)
        setNodeBorderWidthMapping ('type', node.attribute.values, border.widths)
        # swap them around different node border sizes
        node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
        border.widths =         c (20, 0, 10);
        setNodeBorderWidthMapping ('type', node.attribute.values, border.widths)
    } # for i
    

    
} # test.setNodeBorderWidthMapping
#-------------------------------------------------------------------------------
test.setNodeSizeMapping = function ()
{
    title = 'test.setNodeSizeMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    
    # first, create a simple 2-point Mapping, with 'below' and 'above' values strong enough to see that they are working
    # recall that makeSimpleGraph creates count attributes like this:
    # noa (getGraph (net.suide), 'count')     #   A.A   B.B   C.C
    #                                       "2"  "30" "100"
    
    count.control.points = c (20,  40)
    node.sizes           = c (1, 80,  120, 300)
    setNodeSizeMapping ('count', count.control.points, node.sizes, mode='interpolate')
    system ('sleep 0.3')
    
    # now chop off the below & above values.  A should grow to 80, almost as big as B, and C should shrink to 120, larger that B
    
    count.control.points = c (20,  40)
    node.sizes           = c (80,  120)
    setNodeSizeMapping ('count', count.control.points, node.sizes, mode='interpolate')
    system ('sleep 0.3')
    
    # now use a mode='lookup' Mapping.  specify two sizes, look to see that the third type, glycoprotein, gets the tiny small size
    molecule.types = c ('kinase', 'transcription factor')
    node.sizes     = c (60, 80)
    setNodeSizeMapping ('type', molecule.types,  node.sizes, default.size= 5, mode='lookup')
    

    
} # test.setNodeSizeMapping
#-------------------------------------------------------------------------------
test.setNodeShapeMapping = function ()
{
    title = 'test.setNodeShapeMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    
    # specify shapes for only two of the three nodes and node types.  make sure that the third node gets
    # the default shape
    
    # make Mapping for 2 of 3 node types, leaving the third as the default
    node.shapes = c ('diamond', 'triangle')
    attribute.values = c ('kinase', 'glycoprotein')
    setNodeShapeMapping (node.attribute.name='type', attribute.values, node.shapes, default.shape='ellipse')
    
    # test one-element lists
    node.shapes = c ('diamond')
    attribute.values = c ('glycoprotein')
    setNodeShapeMapping (node.attribute.name='type', attribute.values, node.shapes, default.shape='ellipse')
    

    
} # test.setNodeShapeMapping
#-------------------------------------------------------------------------------
test.setNodeOpacityMapping = function ()
{
    title = 'test.setNodeOpacityMapping'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    # make the node borders prominent
    setNodeBorderColorDefault (net.suid, '#FFFF00')
    setNodeBorderWidthDefault (net.suid, 10)
    
    lfc.values = c (-3.0, 0, 3.0)
    
    # make the nodes big, give them strong colors
    setNodeSizeDirect (net.suid, nodes (net.suid@graph), 100)
    setNodeColorMapping (net.suid, 'lfc', lfc.values, c ('#FF0000', '#00FF00', '#0000FF'), mode='interpolate'); redraw (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    # first, the continuous 'interpolate' case, in which opacity is a function of lfc
    opacities = c (10, 128, 255)
    setNodeOpacityMapping (net.suid, node.attribute.name='lfc', lfc.values, opacities, mode='interpolate')
    redraw (net.suid)
    
    # reset
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate', aspect='all');
    redraw (net.suid)
    
    # now try a few of the aspect-specific Mappings, still in interpolate mode
    # border:
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate', aspect='border');
    redraw (net.suid)
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (40,128, 0), mode='interpolate', aspect='border');
    redraw (net.suid)
    
    # reset
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate');   redraw (net.suid)
    
    # label
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (40,128, 0), mode='interpolate', aspect='border');
    redraw (net.suid)
    
    # reset
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate');   redraw (net.suid)
    
    # border
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (40,128, 0), mode='interpolate', aspect='border');
    redraw (net.suid)
    
    # a mix...
    setNodeOpacityMapping (net.suid, 'lfc', lfc.values, c (128, 128, 128), mode='interpolate', aspect='border, label, fill');
    redraw (net.suid)
    
    
    scalar.values = as.character (noa (net.suid@graph, 'type'))
    # reset
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (255, 255, 255), mode='lookup');   redraw (net.suid)
    
    # label
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (40,128, 0), mode='lookup', aspect='border');
    redraw (net.suid)
    
    # reset
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (255, 255, 255), mode='lookup');   redraw (net.suid)
    
    # border
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (40,128, 0), mode='lookup', aspect='border');
    redraw (net.suid)
    
    # a mix...
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (128, 128, 128), mode='lookup', aspect='border, label, fill');
    redraw (net.suid)
    
    # make everything except labels transparent
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (0, 0, 0), mode='lookup', aspect='border, fill');
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='label')
    redraw (net.suid)
    
    # make everything except borders transparent
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (0, 0, 0), mode='lookup', aspect='label, fill');
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='border')
    redraw (net.suid)
    
    # make everything except fill transparent
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (0, 0, 0), mode='lookup', aspect='label, border');
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='fill')
    redraw (net.suid)
    
    # now restore everything
    setNodeOpacityMapping (net.suid, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='all')
    redraw (net.suid)
    
    invisible (net.suid)
    
} # test.setNodeOpacityMapping
#-------------------------------------------------------------------------------
test.setNodeColorDirect = function ()
{
    title = 'test.setNodeColorDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    setNodeColorDirect (net.suid, 'A', '#AA0088')
    setNodeColorDirect (net.suid, c ('C', 'B'), '#448844')
    
    invisible (net.suid)
    
} # test.setNodeColorirect
#-------------------------------------------------------------------------------
test.setNodeBorderColorDirect = function ()
{
    title = 'test.setNodeBorderColorDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    setNodeBorderColorDirect (net.suid, 'A', '#AA4488')
    setNodeBorderColorDirect (net.suid, c ('C', 'B'), '#AA8888')
    
    invisible (net.suid)
    
} # test.setNodeBorderColorDirect
#-------------------------------------------------------------------------------
test.setNodeLabelDirect = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeLabelDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    setNodeLabelDirect (net.suid, 'A', 'new A label')
    # try multiple nodes, one label, which RCy will replicate into the right number
    setNodeLabelDirect (net.suid, nodes (net.suid@graph), '')
    setNodeLabelDirect (net.suid, c ('A', 'C'), c ('AzA', 'ByB'))
    
    invisible (net.suid)
    
} # test.setNodeLabelDirect
#-------------------------------------------------------------------------------
test.setNodeLabelPropertiesDirect = function ()
{
    #DEACTIVATED("too slow")
    print ('--- test.setNodeLabelsPropertiesDirect')
    title = 'test.setNodeLabelPropertiesDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    sizes = c (10, 50, 80)
    colors = c ('#0000FF', '#00FF00', '#FF0000')
    for (i in 1:length (sizes)) {
        setNodeFontSizeDirect (net.suid, 'A', sizes [i])
        setNodeLabelColorDirect (net.suid, 'A', colors [i])
    } # for i
    
    invisible (net.suid)
    
} # test.setNodeLabelsPropertiesDirect
#-------------------------------------------------------------------------------
test.setNodeOpacityDirect = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeOpacityDirect'
    test.prep (title)
    
    g = RCy3::makeSimpleGraph ()
    g = graph::addNode ('D', g)
    nodeData (g, 'D', 'label') = 'blink'
    net.suid = createNetworkFromGraph(g, title=title)
    displayGraph (net.suid)
    setNodeSizeDirect (net.suid, 'D', 120)
    layoutNetwork (net.suid, 'grid')
    fitContent (net.suid)
    setZoom (net.suid, 0.8 * getZoom (net.suid))
    redraw (net.suid)
    
    setNodeFillOpacityDirect (net.suid, 'A', 0)
    setNodeLabelOpacityDirect (net.suid, 'B', 0)
    setNodeBorderOpacityDirect (net.suid, 'C', 0)
    for (i in 1:3) {
        setNodeOpacityDirect (net.suid, 'D', 0)
        setNodeOpacityDirect (net.suid, 'D', 255)
    } # for i
    
    setNodeOpacityDirect (net.suid, c ('A', 'C'), 255)
    setNodeOpacityDirect (net.suid, c ('B', 'D'), 50)
    setNodeOpacityDirect (net.suid, c ('A', 'B', 'C', 'D'), c (10, 50, 100, 200))
    setNodeOpacityDirect (net.suid, c ('A', 'B', 'C', 'D'), c (200, 100, 50, 10))
    Sys.sleep (0.3)
    
    setNodeOpacityDirect (net.suid, c ('A', 'B', 'C', 'D'), 255); redraw (net.suid)
    
    invisible (net.suid)
    
} # test.setNodeOpacityDirect
#-------------------------------------------------------------------------------
# test.setEdgeOpacityDirect = function ()
# {
#     #DEACTIVATED("too slow for some reason")
#     title = 'test.setEdgeOpacityDirect'
#     test.prep (title)
#
#     g = RCy3::makeSimpleGraph ()
#     net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#     setEdgeLineWidthDefault (net.suid, 10)
#     displayGraph (net.suid)
#     layoutNetwork (net.suid, 'grid')
#
#     named.edge.names = cy2.edge.names (g)
#     edge.names <- unname(named.edge.names)
#
#     setEdgeOpacityDirect (net.suid, edge.names [1], 80)
#     setEdgeOpacityDirect (net.suid, edge.names [2], 0)
#     setEdgeOpacityDirect (net.suid, edge.names [3], 255)
#
#     setEdgeOpacityDirect (net.suid, edge.names [2], 80)
#     setEdgeOpacityDirect (net.suid, edge.names [3], 0)
#     setEdgeOpacityDirect (net.suid, edge.names [1], 255)
#
#     setEdgeOpacityDirect (net.suid, edge.names [1], 80)
#     setEdgeOpacityDirect (net.suid, edge.names [3], 40)
#     setEdgeOpacityDirect (net.suid, edge.names [2], 255)
#
#     setEdgeOpacityDirect (net.suid, edge.names [1], 0)
#     setEdgeOpacityDirect (net.suid, edge.names [3], 0)
#     setEdgeOpacityDirect (net.suid, edge.names [2], 0)
#
#     setEdgeOpacityDirect (net.suid, edge.names [1], 255)
#     setEdgeOpacityDirect (net.suid, edge.names [3], 255)
#     setEdgeOpacityDirect (net.suid, edge.names [2], 255)
#
#     setEdgeOpacityDirect (net.suid, edge.names, 0)
#     setEdgeOpacityDirect (net.suid, edge.names, 255)
#
#     setEdgeOpacityDirect (net.suid, edge.names, c (0, 128, 255))
#     setEdgeOpacityDirect (net.suid, edge.names, c (255, 0, 128))
#
#     setEdgeOpacityDirect (net.suid, edge.names, 255)
#
#     invisible (net.suid)
#
# } # test.setEdgeOpacityDirect
#-------------------------------------------------------------------------------
test.setEdgeColorDirect = function ()
{
    #DEACTIVATED("very slow for some reason")
    title = 'test.setEdgeColorDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    edge.of.interest = as.character (cy2.edge.names (g) [1])
    setEdgeColorDirect (net.suid, edge.of.interest, '#FF0000')
    setEdgeColorDirect (net.suid, edge.of.interest, '#00FF00')
    setEdgeColorDirect (net.suid, edge.of.interest, '#0000FF')
    
    invisible (net.suid)
    
} # test.setEdgeColorDirect
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowShapeDirect = function ()
{
    title = 'test.setEdgeSourceArrowShapeDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    fitContent (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (g))
    supported.arrow.shapes = getArrowShapes (net.suid)
    
    # first try passing three edges and three arrow shapes
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, supported.arrow.shapes [2:4])
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one arrow.shapes
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, supported.arrow.shapes [6])
    
    # restore the default
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, 'NONE')
    invisible (net.suid)
    
} # test.setEdgeSourceArrowShapeDirect
#-------------------------------------------------------------------------------
test.setEdgeLabelDirect = function ()
{
    title = 'test.setEdgeLabelDirect '
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    edge.names = cy2.edge.names (net.suid@graph)[1:2]
    setEdgeLabelDirect (net.suid, edge.names, 'some name')
    
    invisible (net.suid)
    
} # test.setEdgeLabelDirect
#-------------------------------------------------------------------------------
#test.setEdgeFontFaceDirect = function ()
#{
#  title = 'test.setEdgeFontFaceDirect'
#  test.prep (title)
#
#  g = RCy3::makeSimpleGraph ()
#  net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#  displayGraph (net.suid)
#  layoutNetwork (net.suid, 'grid')
#  redraw (net.suid)
#
#  edge.of.interest = cy2.edge.names (g) [1]
#  fonts = c ('courier', 'arial')
#  for (font in fonts) {
#    setEdgeFontFaceDirect (net.suid, edge.of.interest, font); redraw (net.suid);
#    Sys.sleep (0.3)
#    } # for i
#
#} # test.
#-------------------------------------------------------------------------------
test.setEdgeFontSizeDirect = function ()
{
    #DEACTIVATED("too slow for some reason.")
    title = 'test.setEdgeFontSizeDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    edge.of.interest = cy2.edge.names (g) [1]
    setEdgeFontSizeDirect (net.suid, edge.of.interest, 12)
    
} # test.setEdgeFontSizeDirect
#-------------------------------------------------------------------------------
# test.setEdgeLabelColorDirect = function () # WORKS BUT IS A BIT SLOW
# {
#     #DEACTIVATED("too slow for some reason")
#     title = 'test.setEdgeLabelColorDirect'
#     test.prep (title)
#
#     g = RCy3::makeSimpleGraph ()
#     net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#     displayGraph (net.suid)
#     layoutNetwork (net.suid, 'grid')
#     redraw (net.suid)
#
#     edge.names = cy2.edge.names (g)
#     setEdgeLabelDirect (net.suid, edge.names, 'some label')
#     setEdgeLabelColorDirect (net.suid, edge.names [1:2], '#FF0000')
#     setEdgeLabelColorDirect (net.suid, edge.names, '#00FF00')
#     setEdgeLabelColorDirect (net.suid, edge.names [3], '#000000')
#
# } # test.setEdgeLabelColorDirect
#-------------------------------------------------------------------------------
test.setEdgeTooltipDirect = function ()
{
    title = 'test.setEdgeTooltipDirect'
    test.prep (title)
    
    net.suid <- createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (net.suid@graph))
    
    # first try passing three edges and three tooltips
    setEdgeTooltipDirect (net.suid, edges.of.interest, c ('tooltip #1', 'tooltip #2', 'tooltip #3'))
    redraw (net.suid)
    
    # now try passing three edges and one tooltip
    setEdgeTooltipDirect (net.suid, edges.of.interest [1:2], 'a general purpose tooltip')
    redraw (net.suid)
    
    invisible (net.suid)
    
} # test.setEdgeTooltipDirect
#-------------------------------------------------------------------------------
test.setEdgeLineWidthDirect = function ()
{
    title = 'test.setEdgeLineWidthDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    edges.of.interest = cy2.edge.names (g) [1:2]
    
    for (i in 1:10) {
        setEdgeLineWidthDirect (net.suid, edges.of.interest, i)
    }
    
    setEdgeLineWidthDirect (net.suid, edges.of.interest, 1)
    redraw (net.suid)
    
} # test.setEdgeLineWidthDirect
#-------------------------------------------------------------------------------
test.setEdgeLineStyleDirect = function ()
{
    title = 'test.setEdgeLineStyleDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (g))
    
    supported.styles = getLineStyles (net.suid)
    
    # first try passing three edges and three styles
    setEdgeLineStyleDirect (net.suid, edges.of.interest, supported.styles [5:7])
    redraw (net.suid)
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one styles
    setEdgeLineStyleDirect (net.suid, edges.of.interest, supported.styles [8])
    redraw (net.suid)
    
    # now loop through all of the styles
    
    for (style in supported.styles) {
        setEdgeLineStyleDirect (net.suid, edges.of.interest, style)
    }
    
    setEdgeLineStyleDirect (net.suid, edges.of.interest, 'SOLID')
    redraw (net.suid)
    
    invisible (net.suid)
    
} # test.setEdgeLineStyleDirect
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowShapeDirect = function ()
{
    title = 'test.setEdgeSourceArrowShapeDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    fitContent (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (g))
    supported.arrow.shapes = getArrowShapes (net.suid)
    
    # first try passing three edges and three arrow.shapes
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, supported.arrow.shapes [5:7])
    redraw (net.suid)
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one arrow shape
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, supported.arrow.shapes [8])
    redraw (net.suid)
    
    # now loop through all of the arrow.shapes
    
    for (shape in supported.arrow.shapes) {
        setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, shape)
    }
    
    # restore the default
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, 'NONE')
    redraw (net.suid)
    
    invisible (net.suid)
    
} # test.setEdgeSourceArrowShapeDirect
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowShapeDirect = function ()
{
    title = 'test.setEdgeTargetArrowShapeDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (g))
    supported.arrow.shapes = getArrowShapes (net.suid)
    
    # first try passing three edges and three arrow.shapes
    setEdgeTargetArrowShapeDirect (net.suid, edges.of.interest, supported.arrow.shapes [5:7])
    redraw (net.suid)
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one arrow shape
    setEdgeTargetArrowShapeDirect (net.suid, edges.of.interest, supported.arrow.shapes [8])
    redraw (net.suid)
    
    # now loop through all of the arrow.shapes
    for (shape in supported.arrow.shapes) {
        setEdgeTargetArrowShapeDirect (net.suid, edges.of.interest, shape)
    }
    
    # restore the default
    setEdgeTargetArrowShapeDirect (net.suid, edges.of.interest, 'NONE')
    
    invisible (net.suid)
    
} # test.setTargetArrowShapeDirect
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowColorDirect = function ()
{
    title = 'test.setEdgeSourceArrowColorDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    arrows = c ('Arrow', 'Diamond', 'Circle')
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    setEdgeSourceArrowMapping (net.suid, 'edgeType', edgeType.values, arrows)
    setEdgeTargetArrowMapping (net.suid, 'edgeType', edgeType.values, arrows)
    
    colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
    colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")
    
    edge.names = as.character (cy2.edge.names (g) [1:3])
    
    setEdgeSourceArrowColorDirect (net.suid, edge.names, colors.1)
    setEdgeSourceArrowColorDirect (net.suid, edge.names, colors.2)
    
    invisible (net.suid)
    
} # test.setEdgeSourceArrowColorDirect
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowColorDirect = function ()
{
    title = 'test.setEdgeTargetArrowColorDirect'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    fitContent (net.suid)
    
    arrows = c ('Arrow', 'Diamond', 'Circle')
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    setEdgeSourceArrowMapping (net.suid, 'edgeType', edgeType.values, arrows)
    setEdgeTargetArrowMapping (net.suid, 'edgeType', edgeType.values, arrows)
    
    colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
    colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")
    
    edge.names = as.character (cy2.edge.names (g) [1:3])
    
    setEdgeTargetArrowColorDirect (net.suid, edge.names, colors.1)
    setEdgeTargetArrowColorDirect (net.suid, edge.names, colors.2)
    
    invisible (net.suid)
    
} # test.setEdgeTargetArrowColorDirect
#-------------------------------------------------------------------------------
# test.setEdgeLabelOpacityDirect = function () # WORKS PERFECTLY BUT TOO SLOW
# {
#     #DEACTIVATED("too slow for some reason")
#     title = 'test.setEdgeLabelOpacityDirect'
#     test.prep (title)
#
#     g = RCy3::makeSimpleGraph ()
#     net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#     displayGraph (net.suid)
#     layoutNetwork (net.suid, 'grid')
#     redraw (net.suid)
#
#     edge.of.interest = cy2.edge.names (g) [1]
#     for (i in 1:5) {
#         setEdgeOpacityDirect (net.suid, edge.of.interest, i * 30); redraw (net.suid);
#     } # for i
#
# } # test.setEdgeLabelOpacityDirect
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowOpacityDirect = function ()
{
    title = 'test.setEdgeSourceArrowOpacityDirect'
    test.prep (title)
    
    g = RCy3::makeSimpleGraph ()
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (g))
    
    # make sure the source arrows are visible
    setEdgeSourceArrowShapeDirect (net.suid, edges.of.interest, 'Circle')
    
    # first try passing three edges and three arrow opacity values
    setEdgeSourceArrowOpacityDirect (net.suid, edges.of.interest, c (64, 128, 255))
    redraw (net.suid)
    
    Sys.sleep (0.3)
    
    # now try passing three edges and just one opacity value; it will be applied to all arrows
    setEdgeSourceArrowOpacityDirect (net.suid, edges.of.interest, 32)
    redraw (net.suid)
    
    # now loop through all of the arrow.opacitys
    for (opacity in seq (0, 255, by=45)) {
        setEdgeSourceArrowOpacityDirect (net.suid, edges.of.interest, opacity)
    }
    
    # restore the default
    setEdgeSourceArrowOpacityDirect (net.suid, edges.of.interest, 255)
    
} # test.setEdgeSourceArrowOpacityDirect
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowOpacityDirect = function ()
{
    title = 'test.setEdgeTargetArrowOpacityDirect'
    test.prep (title)
    
    g = RCy3::makeSimpleGraph ()
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    edges.of.interest = as.character (cy2.edge.names (g))
    
    # make sure the target arrows are visible
    setEdgeTargetArrowShapeDirect (net.suid, edges.of.interest, 'Circle')
    
    # first try passing three edges and three arrow opacity values
    setEdgeTargetArrowOpacityDirect (net.suid, edges.of.interest, c (64, 128, 255))
    redraw (net.suid)
    
    Sys.sleep (0.3)
    
    # now try passing three edges and just one opacity value; it will be applied to all arrows
    setEdgeTargetArrowOpacityDirect (net.suid, edges.of.interest, 32)
    redraw (net.suid)
    
    # now loop through all of the arrow.opacitys
    for (opacity in seq (0, 255, by=45)) {
        setEdgeTargetArrowOpacityDirect (net.suid, edges.of.interest, opacity)
    }
    
    # restore the default
    setEdgeTargetArrowOpacityDirect (net.suid, edges.of.interest, 255)
    redraw (net.suid)
    
} # test.setEdgeTargetArrowOpacityDirect
#-------------------------------------------------------------------------------
# test.setEdgeLabelPositionDirect = function () # WORKS PERFECTLY FINE BUT SLOW
# {
#     #DEACTIVATED("too slow for some reason")
#     title = 'test.setEdgeLabelPositionDirect'
#     test.prep (title)
#
#     g = RCy3::makeSimpleGraph ()
#     net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#     displayGraph (net.suid)
#     layoutNetwork (net.suid, 'grid')
#     redraw (net.suid)
#
#     edge.of.interest = cy2.edge.names (g) [1]
#     for (i in 1:5) {
#         setEdgeOpacityDirect (net.suid, edge.of.interest, i * 30)
#         Sys.sleep (0.3)
#     } # for i
#
# } # test.setEdgeLabelPositionDirect
#-------------------------------------------------------------------------------
#test.setEdgeLabelWidthDirect = function ()
#{
#     title = 'test.setEdgeLabelWidthDirect',
#     test.prep (title),
#   ,
#     g = RCy3::makeSimpleGraph (),
#  net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#  displayGraph (net.suid)
#  layoutNetwork (net.suid, 'grid')
#  redraw (net.suid)
#
#  edge.of.interest = cy2.edge.names (g) [1]
#  for (i in 1:5) {
#    setEdgeOpacityDirect (net.suid, edge.of.interest, i * 30); redraw (net.suid);
#    Sys.sleep (0.3)
#    } # for i
#
#} # test.
#-------------------------------------------------------------------------------
test.countNodes = function ()
{
    title = 'test.countNodes'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    checkEquals (getNodeCount (net.suide), length (nodes (getGraph (net.suide))))
    

    
} # test.countNodes
#-------------------------------------------------------------------------------
test.countEdges = function ()
{
    title = 'test.countEdges'
    test.prep (title)
    
    cy = CytoscapeConnection ()
    if (title %in% as.character (getNetworkList ())){
        deleteNetwork (title)
    }
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    checkEquals (getEdgeCount (net.suide), length (edgeNames (getGraph (net.suide))))
    

    
} # test.countNodes
#-------------------------------------------------------------------------------
test.countNodesAndEdgesInEmptyGraph = function ()
{
    title = 'test.countNodesAndEdgesInEmptyGraph'
    test.prep (title)
    
    g.empty = new ("graphNEL", edgemode = "directed")
    checkEquals (length(g.empty@nodes), 0)
    checkEquals (length(g.empty@edgeL), 0)
    
    net.suide = createNetworkFromGraph (g.empty,title)  # default behavior, but let's make it explicit
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    checkEquals (getNodeCount (net.suide), 0)
    checkEquals (getEdgeCount (net.suide), 0)
    

    
} # test.countNodesAndEdgesInEmptyGraph
#-------------------------------------------------------------------------------
test.getAllNodes = function ()
{
    title = 'test.getAllNodes'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    net.suide.nodes = getAllNodes (net.suide)
    checkEquals (length (intersect (net.suide.nodes, nodes (net.suide@graph))), 3)
    

    
} # test.getAllNodes
#-------------------------------------------------------------------------------
test.getAllEdges = function ()
{
    title = 'test.getAllEdges'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    net.suide.edges = getAllEdges(net.suide)
    checkTrue ("C (undefined) A" %in% net.suide.edges)
    checkTrue ("B (synthetic lethal) C" %in% net.suide.edges)
    checkTrue ("A (phosphorylates) B" %in% net.suide.edges)
    
    #msg ('test.getAllEdges')
    

    
} # test.getAllEdges
#-------------------------------------------------------------------------------
test.selectNodes = function ()
{
    title = 'test.selectNodes'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    clearSelection (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 0)
    net.suide.nodes = selectNodes (c ('A', 'B'), preserve=T)
    checkEquals (getSelectedNodeCount (net.suide), 2)
    
    net.suide.nodes = selectNodes ('C', preserve=T)
    checkEquals (getSelectedNodeCount (net.suide), 3)
    
    clearSelection (net.suide)
    net.suide.nodes = selectNodes (c ('A', 'B'), preserve=TRUE)
    checkEquals (getSelectedNodeCount (net.suide), 2)
    net.suide.nodes = selectNodes ('C', preserve=FALSE)
    checkEquals (getSelectedNodeCount (net.suide), 1)
    checkEquals (getSelectedNodes (net.suide), 'C')
    
    clearSelection (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 0)
    nodes.to.select = c ('bogus', 'missing')
    selectNodes (nodes.to.select)
    checkEquals (getSelectedNodeCount (net.suide), 0)
    nodes.to.select = c (nodes.to.select, nodes (net.suide@graph))
    selectNodes (nodes.to.select)
    checkEquals (getSelectedNodeCount (net.suide), 3)
    

    
} # test.selectNodes
#-------------------------------------------------------------------------------
test.nodeNeighborReportingAndSelection = function ()
{
    title = 'test.nodeNeighborReportingAndSelection'
    test.prep (title)
    
    # create a circular graph
    LETTERS = toupper (letters)
    source.nodes  <- LETTERS [1:26]
    target.nodes  <- c (LETTERS [2:26], LETTERS [1])
    weights <- runif (length (letters))
    df <- data.frame (from=source.nodes, to=target.nodes, weight=weights)
    g.bam <- graphBAM (df, edgemode='directed')
    g.bam <- initEdgeAttribute (g.bam, 'weight', 'numeric', 0.0)
    
    net.suid = createNetworkFromGraph (g.bam,title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    # paint the edges shades of green as function of weight
    setEdgeLineWidthDefault (net.suid, 5)
    setEdgeColorMapping (net.suid, 'weight',  c (0, 1), c ('#FFFFFF', '#00FF00'),  mode='interpolate')
    
    # select M, then its immediate neighbors
    checkEquals (getSelectedNodeCount (net.suid), 0)
    checkEquals (sort (getFirstNeighbors (net.suid, 'M')), c ('L', 'N'))
    selectNodes (net.suid, 'M')
    checkEquals (getSelectedNodeCount (net.suid), 1)
    selectFirstNeighborsOfSelectedNodes (net.suid)
    checkEquals (getSelectedNodeCount (net.suid), 3)
    checkEquals (sort (getSelectedNodes (net.suid)), c ('L', 'M', 'N'))
    sfn (net.suid)
    checkEquals (getSelectedNodeCount (net.suid), 5)
    nodes = sort (getSelectedNodes (net.suid))
    checkEquals (nodes, c ("K", "L", "M", "N", "O"))
    invisible (net.suid)
    
} # test.nodeNeighborReportingAndSelection
#-------------------------------------------------------------------------------
test.invertSelection = function ()
{
    title = 'test.invertSelection'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    clearSelection (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 0)
    net.suide.nodes = selectNodes (c ('A', 'B'))
    checkEquals (getSelectedNodeCount (net.suide), 2)
    
    invertNodeSelection (net.suide)
    redraw (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 1)
    invertNodeSelection (net.suide)
    redraw (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 2)
    
    clearSelection (net.suide)
    

    
} # test.invertSelection
#-------------------------------------------------------------------------------
test.deleteSelectedNodes = function ()
{
    title = 'test.deleteSelectedNodes'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    clearSelection (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 0)
    net.suide.nodes = selectNodes (c ('A', 'B'))
    checkEquals (getSelectedNodeCount (net.suide), 2)
    
    deleteSelectedNodes(net.suide)
    checkEquals(getNodeCount(net.suide), 1)

    
} # test.invertNodeSelection
#-------------------------------------------------------------------------------
test.hideNodes = function ()
{
    title = 'test.hideNodes'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    clearSelection (net.suide)
    checkEquals (getSelectedNodeCount (net.suide), 0)
    net.suide.nodes = selectNodes (c ('A', 'B'))
    checkEquals (getSelectedNodeCount (net.suide), 2)
    checkEquals (getNodeCount (net.suide), 3)
    hideSelectedNodes (net.suide)
    checkEquals (getNodeCount (net.suide), 3)
    unhideAll (net.suide)
    layoutNetwork (net.suide)
    redraw (net.suide)
    checkEquals (getNodeCount (net.suide), 3)
    

    
} # test.hideNodes
#-------------------------------------------------------------------------------
test.selectEdges = function ()
{
    title = 'test.selectEdges'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    clearSelection (net.suid)
    checkEquals (getSelectedEdgeCount (net.suid), 0)
    selectEdges (net.suid, "A (phosphorylates) B")
    checkEquals (getSelectedEdgeCount (net.suid), 1)
    Sys.sleep (0.3)
    clearSelection (net.suid)
    checkEquals (getSelectedEdgeCount (net.suid), 0)
    
    invisible (net.suid)
    
} # test.selectEdges
#-------------------------------------------------------------------------------
test.getAdjacentEdgeNames = function ()
{
    title = 'test.getAdjacentEdgeNames'
    g = RCy3::makeSimpleGraph ()
    expected.names = c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (undefined) A")
    checkEquals (sort (as.character (cy2.edge.names (g))), expected.names)
    
    checkEquals (sort (getAdjacentEdgeNames (g, 'A')), expected.names [c (1,3)])
    checkEquals (sort (getAdjacentEdgeNames (g, 'B')), expected.names [c (1,2)])
    checkEquals (sort (getAdjacentEdgeNames (g, 'C')), expected.names [c (2,3)])
    
    checkEquals (sort (getAdjacentEdgeNames (g, c ('A', 'B'))), expected.names [1:3])
    checkEquals (sort (getAdjacentEdgeNames (g, c ('B', 'C'))), expected.names [1:3])
    checkEquals (sort (getAdjacentEdgeNames (g, c ('A', 'C'))), expected.names [1:3])
    invisible (g)
    
} # test.getAdjacentEdgeNames
#-------------------------------------------------------------------------------
test.setEdgeLineStyleMapping = function ()
{
    title = 'test.setEdgeLineStyleMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    
    line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    checkEquals (length (intersect (line.styles, getLineStyles (net.suide))), 3)
    
    setEdgeLineStyleMapping ('edgeType', edgeType.values, line.styles)
    
    # test one-element lists
    line.styles = c ('DOT')
    edgeType.values = c ('synthetic lethal')
    checkEquals (length (intersect (line.styles, getLineStyles (net.suide))), 1)
    setEdgeLineStyleMapping ('edgeType', edgeType.values, line.styles)
    
    #msg ('test.setEdgeLineStyleMapping')
    

    
} # test.setEdgeLineStyleMapping
#-------------------------------------------------------------------------------
test.setEdgeLineWidthMapping = function ()
{
    title = 'test.setEdgeLineWidthMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    checkEquals (length (intersect (line.styles, getLineStyles (net.suide))), 3)
    
    setEdgeLineStyleMapping ('edgeType', edgeType.values, line.styles)
    setEdgeLineWidthMapping ('edgeType', edgeType.values, c (0, 8, 16))
    
    # try one-element lists
    setEdgeLineWidthMapping ('edgeType', edgeType.values [1], 10)
    

    
} # test.setEdgeLineWidthMapping
#-------------------------------------------------------------------------------
test.setEdgeColorMapping = function ()
{
    title = 'test.setEdgeColorMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    colors = c ('#FF0000', '#FFFF00', '#00FF00')
    setEdgeColorMapping ('edgeType',  edgeType.values, colors, mode='lookup')
    Sys.sleep (0.3)
    
    all.white  = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
    setEdgeColorMapping ('edgeType',  edgeType.values [2], mode='lookup', '#000000')
    
    # now create a continuous ('interpolate') mode Mapping, using the score edge attribute
    score.values = c (-15, 0, 40);
    colors = c ('#00FF00', '#FFFFFF', '#FF0000')
    setEdgeColorMapping ('score',  score.values, colors, mode='interpolate')
    
    # now swap the colors
    colors = c ('#FF0000', '#000000', '#00FF00')
    setEdgeColorMapping ('score',  score.values, colors, mode='interpolate')
    

    
} # test.setEdgeColorMapping
#-------------------------------------------------------------------------------
test.setEdgeOpacityMapping = function ()
{
    title = 'test.setEdgeOpacityMapping'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    edgeType.values = c ("phosphorylates", "synthetic lethal", "undefined")
    
    # want to see edges and both arrows, to check success of opacity Mapping
    setEdgeTargetArrowMapping (net.suid, 'edgeType', edgeType.values, rep ('ARROW', 3))
    setEdgeSourceArrowMapping (net.suid, 'edgeType', edgeType.values, rep ('ARROW', 3))
    setEdgeLineWidthDefault (net.suid, 5)
    
    redraw (net.suid)
    
    # do the lookup Mapping
    opacities = c (25, 100, 255)
    setEdgeOpacityMapping (net.suid, 'edgeType',  edgeType.values, opacities, mode='lookup')
    redraw (net.suid)
    
    # now do the interpolated version
    opacities = c (10, 125, 255)
    control.points = c (-12, 0, 35)
    setEdgeOpacityMapping (net.suid, 'score',  control.points, opacities, mode='interpolate')
    redraw (net.suid)
    
    invisible (net.suid)
    
} # test.setEdgeOpacityMapping
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowMapping = function ()
{
    title = 'test.setEdgeTargetArrowMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    arrows = c ('DELTA', 'T', 'DIAMOND')
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    checkEquals (length (intersect (arrows, getArrowShapes (net.suide))), 3)
    
    setEdgeTargetArrowMapping ('edgeType', edgeType.values, arrows)
    
    # now test the list-of-length-one call.  the called method will double the list to get past the xmlrpc
    # treatment of lists of length one as scalars, and a failed signature match
    arrows = c ('CIRCLE')
    edgeType.values = c ('phosphorylates')
    checkEquals (length (intersect (arrows, getArrowShapes (net.suide))), 1)
    
    setEdgeTargetArrowMapping ('edgeType', edgeType.values, arrows)
    

    
} # test.setEdgeTargetArrowMapping
#-------------------------------------------------------------------------------
test.setEdgeArrowColorMappings = function ()
{
    title = 'test.setEdgeArrowColorMappings'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
    colors.2 = c ("#AA00AA", "#AAAA00", "#AA0000")
    
    setEdgeTargetArrowColorMapping ('edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.1)
    setEdgeSourceArrowColorMapping ('edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.1)
    system ('sleep 0.3')
    setEdgeTargetArrowColorMapping ('edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.2)
    setEdgeSourceArrowColorMapping ('edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.2)
    
    # test one-element list
    setEdgeSourceArrowColorMapping ('edgeType', "phosphorylates", '#000000')
    

    
} # test.setEdgetArrowColorMappings
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowMapping = function ()
{
    title = 'test.setEdgeSourceArrowMapping'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    arrows = c ('ARROW', 'DIAMOND', 'CIRCLE')
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
    checkEquals (length (intersect (arrows, getArrowShapes (net.suide))), 3)
    
    setEdgeSourceArrowMapping ('edgeType', edgeType.values, arrows)
    
    # test one-element Mapping
    setEdgeSourceArrowMapping ('edgeType', edgeType.values [2], arrows [2])
    

    
} # test.setEdgeSourceArrowMapping
#-------------------------------------------------------------------------------
test.movie = function ()
{
    title = 'test.movie'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    # establish the Mappings which apply during the full run of the movie
    # different node sizes and node colors are created, not by changing these Mappings, but
    # by changing node attribute values, for the integer attribute 'count' and the numeric attribute 'lfc'
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeMapping ('count', count.control.points, sizes, mode='interpolate')
    setNodeColorMapping ('lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
    
    count = 3
    
    # three renderings of the 3-node, 3-edge network are created in this loop, which runs 'count' times
    # the first two set new attributes on the R graph data structure, then ask RCy3 to send those values
    # to R from the graph
    # the third rendering bypasses storage of new attribute values on the R graph, sending them instead
    # directly to Cytoscape.  (hence 'setNodeAttributesDirect')
    
    for (i in 1:count) {
        nodeData (net.suide@graph, 'A', 'lfc') = -3.0
        nodeData (net.suide@graph, 'B', 'lfc') = -0.7
        nodeData (net.suide@graph, 'C', 'lfc') = -1.9
        nodeData (net.suide@graph, 'A', 'count') = 10
        nodeData (net.suide@graph, 'B', 'count') = 140
        nodeData (net.suide@graph, 'C', 'count') = 32
        result = setNodeAttributes ('lfc')
        result = setNodeAttributes ('count')
        redraw (net.suide)
        
        Sys.sleep (0.3)
        nodeData (net.suide@graph, 'A', 'lfc') = 3.0
        nodeData (net.suide@graph, 'B', 'lfc') = 0.7
        nodeData (net.suide@graph, 'C', 'lfc') = 1.9
        nodeData (net.suide@graph, 'A', 'count') = 50
        nodeData (net.suide@graph, 'B', 'count') = 22
        nodeData (net.suide@graph, 'C', 'count') = 180
        result = setNodeAttributes ('lfc')
        result = setNodeAttributes ('count')
        redraw (net.suide)
        Sys.sleep (0.3)
        
        count.A = round (runif (1, 1, 200))
        count.B = round (runif (1, 1, 200))
        count.C = round (runif (1, 1, 200))
        
        result = setNodeAttributesDirect ('count', 'int', c ('A', 'B', 'C'), c (count.A, count.B, count.C));
        result = setNodeAttributesDirect ('lfc', 'numeric', c ('A', 'B', 'C'), c (-1.0, 0.0, 1.0))
        redraw (net.suide)
    }
    

    
} # test.movie
#-------------------------------------------------------------------------------
# test.unmatchedAttributesError = function ()
# {
#     title = 'test.unmatchedAttributesError'
#     test.prep (title)
#
#     net.suide = CytoscapeNetwork (title, RCy3::makeSimpleGraph ())
#     displayGraph (net.suide)
#     layoutNetwork ('grid')
#
#     # this works
#     count.control.points = c (2, 30, 100)
#     sizes = c (20, 50, 100)
#     setNodeSizeMapping ('count', count.control.points, sizes, mode='interpolate')
#
# 
#
# } # test.unmatchedAttributesError
#-------------------------------------------------------------------------------
#RCy3:::makeRandomGraph ()
#-------------------------------------------------------------------------------
# this tests the otherwise invisible method in RCy3.R, called to compensate for the extra edges and edge attributes
# packed into an undirected graph
#-------------------------------------------------------------------------------
test.randomUndirectedGraph = function ()
{
    title = 'test.randomUndirectedGraph'
    test.prep (title)
    
    g.random = RCy3::makeRandomGraph ()
    edgeData (g.random, '1', '2', 'weight') = 0.55
    edgeData (g.random, '1', '2', 'pmid') = '12345678'
    
    net.suidr = createNetworkFromGraph (g.random,title)
    displayGraph (net.suidr)
    layoutNetwork (net.suidr, 'grid')
    redraw (net.suidr)
    
    invisible (net.suidr)
    
} # test.randomUndirectedGraph
#-------------------------------------------------------------------------------
test.simpleGraph = function (apply.viz.Mappings=TRUE, do.redraw=TRUE)
{
    title = 'test.simpleGraph'
    test.prep (title)
    
    net.suids = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    
    displayGraph (net.suids)
    layoutNetwork (net.suids, 'grid')
    
    if (apply.viz.Mappings) {
        setNodeLabelMapping (net.suids, 'label')
        setNodeBorderWidthDefault (net.suids, 5)
        node.attribute.values = c ("kinase",  "transcription factor")
        colors =                c ('#A0AA00', '#FF0000')
        setNodeBorderColorMapping (net.suids, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
        count.control.points = c (2, 30, 100)
        sizes                = c (20, 50, 100)
        setNodeSizeMapping (net.suids, 'count', count.control.points, sizes, mode='interpolate')
        setNodeColorMapping (net.suids, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
        redraw (net.suids)
    } # if apply.viz.Mappings
    
    
    invisible (net.suids)
    
} # test.simpleGraph
#-------------------------------------------------------------------------------
test.simpleGraphWithReciprocalEdge = function ()
{
    title = 'test.simpleGraphWithReciprocalEdge'
    test.prep (title)
    
    g.simple = RCy3::makeSimpleGraph ()
    g.simple = graph::addEdge ('C', 'B', g.simple)
    edgeData (g.simple, 'C', 'B', attr='edgeType') = 'synthetic rescue'
    edgeData (g.simple, 'C', 'B', attr='score') = 42
    edgeData (g.simple, 'C', 'B', attr='misc') = 'ellany'
    g <- g.simple
    
    net.suids = createNetworkFromGraph (g.simple,title)
    
    displayGraph (net.suids)
    layoutNetwork (net.suids, 'grid')
    setNodeLabelMapping (net.suids, 'label')
    node.attribute.values = c ("kinase",  "transcription factor")
    colors =                c ('#A0AA00', '#FF0000')
    setNodeBorderWidthDefault (net.suids, 5)
    setNodeBorderColorMapping (net.suids, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeMapping (net.suids, 'count', count.control.points, sizes, mode='interpolate')
    setNodeColorMapping (net.suids, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
    arrows = c ('Arrow', 'Arrow', 'Arrow', 'None')
    edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'synthetic rescue', 'undefined')
    setEdgeTargetArrowMapping (net.suids, 'edgeType', edgeType.values, arrows)
    
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'synthetic rescue', 'undefined')
    edgeColors = c ('#0000AA', '#000000', '#00AA00', '#FFFFFF')
    setEdgeColorMapping (net.suids, 'edgeType',  edgeType.values, edgeColors, mode='lookup')
    
    redraw (net.suids)
    
    invisible (net.suids)
    
} # test.simpleGraphWithReciprocalEdge

#-------------------------------------------------------------------------------
test.setNodePosition = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodePosition'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    layoutNetwork ('grid')   # get a reasonable starting layout, with the nodes well-separate
    
    center.x = 200
    center.y = 200
    radius = 200
    angles = seq (0, 360, 5)  # sweep through full revoltion, 5 degrees at a time
    # move just the A node, swinging it around the 'center' at 200, 200.
    # it would be nice not know more about the coordinate system than I now do, perhaps to
    # query current position on any node
    for (angle in angles) {
        angle.in.radians = angle * pi / 180
        x = center.x + (radius * cos (angle.in.radians))
        y = center.y + (radius * sin (angle.in.radians))
        setNodePosition ('A', x, y)
    }
    

    
} # test.setNodePosition
#-------------------------------------------------------------------------------
test.getNodePosition = function ()
{
    title = 'test.getNodePosition'
    test.prep (title)
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    layoutNetwork ('grid')   # get a reasonable starting layout, with the nodes well-separate
    
    # the scheme:  get current positions, find their mean, place all the nodes there,
    # get their new positions, check to see that they are the means just set.
    
    positions <- RCy3::getNodePosition (c ('A', 'B', 'C'))
    
    # place the nodes on top of each other, at the center of their 3-cornered original layout
    center.x = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$x)))))
    center.y = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$y)))))
    
    setNodePosition (c ('A', 'B', 'C'), rep (center.x, 3), rep (center.y, 3))
    current.x = RCy3::getNodePosition ('A')[[1]]$x
    current.y = RCy3::getNodePosition ('A')[[1]]$y
    #printf ('center:  %d  %d', center.x, center.y)
    #printf ('current: %d  %d', current.x, current.y)
    
    checkEqualsNumeric (current.x, center.x, tol=1)
    checkEqualsNumeric (current.y, center.y, tol=1)
    

    
} # test.getNodePosition
#-------------------------------------------------------------------------------
test.getNodePosition.colonInNodeName = function ()
{
    #DEACTIVATED("not fatally slow, but i am impatient. Reactivate later.")
    title = 'test.getNodePosition.colonInNodeName'
    test.prep (title)
    
    g = RCy3::makeSimpleGraph ()
    funky.node.name = 'ab::cdxyz::1234,funky!?' 
    g = graph::addNode (funky.node.name, g)
    nodeData (g, funky.node.name, 'label') = funky.node.name
    
    net.suide = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suide)
    layoutNetwork ('grid')
    redraw (net.suide)
    
    layoutNetwork ('grid')   # get a reasonable starting layout, with the nodes well-separate
    
    # the scheme:  get current positions, find their mean, place all the nodes there,
    # get their new positions, check to see that they are the means just set.
    
    positions <- RCy3::getNodePosition (c ('A', 'B', 'C'))
    
    # place the nodes on top of each other, at the center of their 3-cornered original layout
    
    center.x = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$x)))))
    center.y = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$y)))))
    
    # rearrange the positions
    layoutNetwork ('grid')
    
    # superimpose A,B, and C  in the center
    setNodePosition (c ('A', 'B', 'C'), rep (center.x, 3), rep (center.y, 3))
    x.funky = center.x + 50
    y.funky = center.y + 50
    # offset funky.node.name
    setNodePosition (funky.node.name, x.funky, y.funky)
    fitContent (net.suide)
    setZoom (0.75 * getZoom (net.suide))
    
    # now check that the nodes have been repositioned from grid to centered (A,B,C) and offset (funky.node.name)
    current.x = RCy3::getNodePosition ('A')[[1]]$x
    current.y = RCy3::getNodePosition ('A')[[1]]$y
    
    checkEqualsNumeric (current.x, center.x, tol=1)
    checkEqualsNumeric (current.y, center.y, tol=1)
    
    funky.pos.x = RCy3::getNodePosition (funky.node.name) [[1]]$x
    funky.pos.y = RCy3::getNodePosition (funky.node.name) [[1]]$y
    checkEqualsNumeric (funky.pos.x, x.funky, tol=1)
    checkEqualsNumeric (funky.pos.y, y.funky, tol=1)
    

    
} # test.getNodePosition.colonInNodeName
#-------------------------------------------------------------------------------
test.getNodeSize = function ()
{
    title = 'test.getNodeSize'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    
    # establish a good starting point
    setNodeSizeDirect (net.suid, nodes (net.suid@graph), rep (100, 3))
    redraw (net.suid)
    
    sizes =  getNodeSize (net.suid, nodes (net.suid@graph))
    # these next test pass fine in uncomplicated circumstances, but (apparently) fail due to
    # vizmap complexities when lots of windows are or have been open
    #checkEquals (sizes$width, c (100, 100, 100))
    #checkEquals (sizes$height, c (100, 100, 100))
    
    setNodeSizeDirect (net.suid, c ('A', 'B'), 150); redraw (net.suid)
    sizes =  getNodeSize (net.suid, nodes (net.suid@graph))
    
    # these next test pass fine in uncomplicated circumstances, but (apparently) fail due to
    # vizmap complexities when lots of windows are or have been open
    #checkEquals (sizes$width, c (150, 150, 100))
    #checkEquals (sizes$height, c (150, 150, 100))
    
    setNodeSizeDirect (net.suid, c ('A', 'B'), c (180, 32));   redraw (net.suid)
    
    sizes = getNodeSize (net.suid, nodes (net.suid@graph))
    #checkEquals (sizes$width, c (180, 32, 100))
    #checkEquals (sizes$height, c (180, 32, 100))
    
    # now allow for non-symmetric dimensions, in which width and height are set separately
    lockNodeDimensions (net.suid, FALSE)
    setNodeHeightDirect (net.suid, c ('A', 'B', 'C'), c (12, 22, 32))
    setNodeWidthDirect (net.suid, c ('A', 'B', 'C'), c (120, 122, 132))
    redraw (net.suid)
    
    sizes = getNodeSize (net.suid, 'B')
    #checkEquals (sizes$width, 122)
    #checkEquals (sizes$height, 22)
    
    # return to symmetric dimensions
    lockNodeDimensions (net.suid, TRUE)
    redraw (net.suid)
    
    # not sure how width and height are rectified.  it appears that the last-used width=height values are returned
    sizes = getNodeSize (net.suid, nodes (net.suid@graph))
    #checkEquals (sizes$width, sizes$height)
    
    invisible (net.suid)
    
} # test.getNodeSize
#-------------------------------------------------------------------------------
test.haveNodeAttribute = function ()
{
    title = 'test.haveNodeAttribute'
    test.prep (title)
    
    net.suid3 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid3)
    redraw (net.suid3)
    layoutNetwork (net.suid3)
    
    nodes.with.attribute = RCy3:::haveNodeAttribute (net.suid3, nodes (getGraph (net.suid3)), 'lfc')
    checkEquals (sort (nodes.with.attribute),  c ('A', 'B', 'C'))
    
    checkEquals (length (RCy3:::haveNodeAttribute (net.suid3, nodes (getGraph (net.suid3)), 'type')), 3)
    checkEquals (length (RCy3:::haveNodeAttribute (net.suid3, nodes (getGraph (net.suid3)), 'label')), 3)
    checkEquals (length (RCy3:::haveNodeAttribute (net.suid3, nodes (getGraph (net.suid3)), 'count')), 3)
    
    checkEquals (length (RCy3:::haveNodeAttribute (net.suid3, nodes (getGraph (net.suid3)), 'bogus')), 0)
    
    invisible (net.suid3)
    
} # test.haveNodeAttribute
#-------------------------------------------------------------------------------
test.haveEdgeAttribute = function ()
{
    title = 'test.haveEdgeAttribute'
    test.prep (title)
    
    net.suid3 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid3)
    redraw (net.suid3)
    layoutNetwork (net.suid3)
    
    cy2.edgenames = as.character (cy2.edge.names (getGraph (net.suid3)))
    edges.with.attribute = RCy3:::haveEdgeAttribute (net.suid3, cy2.edgenames, 'edgeType')
    
    checkEquals (length (edges.with.attribute), 3)
    checkTrue ("A (phosphorylates) B" %in% edges.with.attribute)
    checkTrue ("B (synthetic lethal) C" %in% edges.with.attribute)
    checkTrue ("C (undefined) A" %in% edges.with.attribute)
    
    checkTrue (length (RCy3:::haveEdgeAttribute (net.suid3, cy2.edgenames, 'score')) == 3)
    checkTrue (length (RCy3:::haveEdgeAttribute (net.suid3, cy2.edgenames, 'misc')) == 3)
    checkTrue (length (RCy3:::haveEdgeAttribute (net.suid3, cy2.edgenames, 'bogus')) == 0)
    
} # test.haveEdgeAttribute
#-------------------------------------------------------------------------------
hiddenTest.haveEdgeAttribute.oneEdgeOnly = function ()
{
    title = 'test.haveEdgeAttribute.oneEdgeOnly'
    test.prep (title)
    
    g = makeSimpleGraph ()
    g = removeNode ('A', g)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    cy = CytoscapeConnection ()
    
    cy2.edgenames = as.character (cy2.edge.names (getGraph (net.suid)))
    
    checkTrue (length (RCy3:::haveEdgeAttribute (net.suid, cy2.edgenames, 'score')) == 1)
    
} # hiddenTest.haveEdgeAttribute.oneEdgeOnly
#-------------------------------------------------------------------------------
test.copyNodeAttributesFromCyGraph = function ()
{
    title = 'test.copyNodeAttributesFromCyGraph'
    test.prep (title)
    
    net.suid3 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid3)
    redraw (net.suid3)
    layoutNetwork (net.suid3)
    
    # we can now depend upon Cytoscape holding its own version of net.suid3@graph
    # in expected use, we expect that 'getGraphFromNetwork' will be called, to get the nodes, edges, and both
    # node & edge attributes
    # but here, we only want to test the reliability of querying the Cytoscape version of the graph for all of its node
    # attributes. So we build a 3-node graph, *without* attributes, and pass that to copyNodeAttributesFromCyGraph,
    # which should copy those Cytoscape graph node attributes onto the graph we pass in.
    g = new ('graphNEL', edgemode='directed')
    g = graph::addNode (c ('A', 'B', 'C'), g)
    
    g2 = RCy3:::copyNodeAttributesFromCyGraph (net.suid3, getNetworkID (title), g)
    checkEquals (length (intersect (noa.names (g2), c ("name", "count", "label", "lfc", "type"))), 5)
    checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='name')), c ('A', 'B', 'C'))
    checkEquals (as.integer (nodeData (g2, c ('A', 'B', 'C'), attr='count')), c (2, 30, 100))
    checkEquals (as.numeric (nodeData (g2, c ('A', 'B', 'C'), attr='lfc')), c (-3,  0,  3))
    checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='type')), c ("kinase", "transcription factor", "glycoprotein"))
    
    invisible (net.suid3)
    
} # test.copyNodeAttributesFromCyGraph
#-------------------------------------------------------------------------------
test.copyEdgeAttributesFromCyGraph = function ()
{
    title = 'test.copyEdgeAttributesFromCyGraph'
    test.prep (title)
    
    net.suid3 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid3)
    redraw (net.suid3)
    layoutNetwork (net.suid3)
    
    g = new ('graphNEL', edgemode='directed')
    g = graph::addNode (c ('A', 'B', 'C'), g)
    g = graph::addEdge("A", "B", g)
    g = graph::addEdge("B", "C", g)
    g = graph::addEdge("C", "A", g)
    # "C (undefined) A" "B (synthetic lethal) C"   "A (phosphorylates) B"
    edgeDataDefaults (g, 'edgeType') = 'undefined'
    edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
    edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'
    edgeData (g, 'C', 'A', 'edgeType') = 'undefined'
    
    cy = CytoscapeConnection ()
    g2 = RCy3:::copyEdgeAttributesFromCyGraph (net.suid3, net.suid3, g)
    
    checkEquals (eda (g2, 'score') [['A|B']], 35)
    checkEquals (eda (g2, 'score') [['B|C']], -12)
    checkEquals (eda (g2, 'score') [['C|A']], 0)
    
    checkEquals (eda (g2, 'edgeType') [['A|B']], 'phosphorylates')
    checkEquals (eda (g2, 'edgeType') [['B|C']], 'synthetic lethal')
    checkEquals (eda (g2, 'edgeType') [['C|A']], 'undefined')
    
    checkEquals (eda (g2, 'interaction') [['A|B']], 'phosphorylates')
    checkEquals (eda (g2, 'interaction') [['B|C']], 'synthetic lethal')
    checkEquals (eda (g2, 'interaction') [['C|A']], 'undefined')
    
    checkEquals (eda (g2, 'misc') [['A|B']], 'default misc')
    checkEquals (eda (g2, 'misc') [['B|C']], 'default misc')
    checkEquals (eda (g2, 'misc') [['C|A']], 'default misc')
    
    checkEquals (eda (g2, 'name') [['A|B']],  "A (phosphorylates) B")
    checkEquals (eda (g2, 'name') [['B|C']],  "B (synthetic lethal) C")
    checkEquals (eda (g2, 'name') [['C|A']],  "C (undefined) A")
    
    invisible (g2)
    
} # test.copyEdgeAttributesFromCyGraph
#-------------------------------------------------------------------------------
test.getGraphFromCyNetwork = function ()
{
    cy = CytoscapeConnection ()
    
    title = 'test.getGraphFromCyNetwork'
    test.prep (title)
    
    net.suid3 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid3)
    redraw (net.suid3)
    layoutNetwork (net.suid3)
    
    g3 = getGraphFromCyNetwork ('test.getGraphFromCyNetwork')
    checkEquals (sort (nodes (g3)), c ('A', 'B', 'C'))
    checkEquals (length (intersect (noa.names (g3), c ("name", "count", "label", "lfc", "type"))), 5)
    checkEquals (as.character (sort (noa (g3, 'name'))), c ('A', 'B', 'C'))
    checkEquals (as.integer   (sort (noa (g3, 'count'))),         c (2, 30, 100))
    checkEquals (as.character (sort (noa (g3, 'label'))),         c ('Gene A', 'Gene B', 'Gene C'))
    checkEquals (as.numeric (sort (noa (g3, 'lfc'))),             c (-3,  0,  3))
    checkEquals (as.character (sort (noa (g3, 'type'))),          c ("glycoprotein", "kinase", "transcription factor"))
    
    checkEquals (length (intersect (eda.names (g3), c ("name", "edgeType", "interaction", "misc", "score"))), 5)
    
    checkEquals (sort (names (cy2.edge.names (g3))),        c ('A~B',                   'B~C',                    'C~A'))
    checkEquals (sort (as.character (cy2.edge.names (g3))), c ("A (phosphorylates) B",  "B (synthetic lethal) C", "C (undefined) A"))
    
    checkEquals (as.character (sort (eda (g3, 'edgeType'))), c ("phosphorylates", "synthetic lethal", "undefined"))
    checkEquals (as.character (sort (eda (g3, 'name'))), c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (undefined) A"))
    checkEquals (as.character (sort (eda (g3, 'interaction'))), c ("phosphorylates", "synthetic lethal", "undefined"))
    checkEquals (as.character (sort (eda (g3, 'misc'))), c ("default misc", "default misc", "default misc"))
    checkEquals (as.numeric (sort (eda (g3, 'score'))), c (-12,  0,  35))
    
    invisible (g3)
    
} # test.getGraphFromCyNetwork
#-------------------------------------------------------------------------------
# try graphs with no edges, then one with neither nodes nor edges
# todo:  try single node, and single edge graphs.
test.sendDegenerateGraphs = function ()
{
    title = 'test.sendDegenerateGraphs'
    test.prep (title)
    
    g.no.edges <- new ('graphNEL')
    g.no.edges <- graph::addNode (c ('A', 'B'), g.no.edges)
    net.suid.degen <- createNetworkFromGraph (g.no.edges,title)
    displayGraph (net.suid.degen)
    redraw (net.suid.degen)
    layoutNetwork (net.suid.degen, 'grid')
    
    title = 'test.sendEmptyGraph'
    test.prep (title)
    
    g.empty <- new ('graphNEL')
    net.suid.empty <- createNetworkFromGraph (g.empty,title)
    displayGraph (net.suid.empty)
    redraw (net.suid.empty)
    layoutNetwork (net.suid.empty, 'grid')
    
    invisible (net.suid.empty)
    
} # test.sendDegenerateGraphs
#-------------------------------------------------------------------------------
test.createNetworkFromSelection = function ()
{
    title = 'test.createNetworkFromSelection'
    test.prep (title)
    
    net.suid =  createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    selectNodes (net.suid, c ('A', 'C'))
    
    new.window.title = 'NEW'
    if (new.window.title %in% as.character (getNetworkList ())){
        deleteNetwork (new.window.title)
    }
    
    c2 = createNetworkFromSelection (net.suid, new.window.title, TRUE)
    redraw (c2)
    layoutNetwork (c2)
    
    clearSelection (c2)
    selectNodes (c2, 'C')
    #     checkEquals (getSelectedNodeCount (c2), 1)
    #
    #     new.window.title = 'NEW, just 1 node'
    #     if (new.window.title %in% as.character (getNetworkList ())){
    #         deleteNetwork (new.window.title)
    #     }
    #
    #     c3 = createNetworkFromSelection (c2, new.window.title, T)
    #     redraw (c3)
    #     layoutNetwork (c3)
    #
    #    invisible (list (net.suid=net.suid, c2=c2, c3=c3))
    
} # test.createNetworkFromSelection

#-------------------------------------------------------------------------------
test.addGetAndDeleteEdgeAttributes = function ()
{
    title = 'test.addGetAndDeleteEdgeAttributes'
    net.suid = test.prep (title)
    
    # in this test we add two new edge attributes, 'species' and 'ageInYears'
    # if they are already defined, from a previous run of this test, start by deleting them.
    
    novel.eda.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getEdgeAttributeNames(net.suid))
    for (eda.name in novel.eda.to.delete){
        deleteEdgeAttribute (eda.name)
    }
    
    # name and interaction are added by Cytoscape
    checkEquals (length (intersect (getEdgeAttributeNames (net.suid), c ("name", "edgeType", "interaction", "misc", "score"))), 5)
    
    # now add an attribute to two of the edges
    first.two.edges = as.character (cy2.edge.names (g)[1:2])
    values = c ('hemlock', 'yew')
    setEdgeAttributesDirect (net.suid, 'treeSpecies', 'char', first.two.edges, values)
    
    # now add an attribute to a single edge.  this exercises a different branch in RCytoscape:setEdgeAttributesDirect
    first.edge = as.character (cy2.edge.names (g)[1])
    value = 'one century'
    setEdgeAttributesDirect (net.suid, 'ageInYears', 'char', first.edge, value)
    checkTrue ('ageInYears' %in% getEdgeAttributeNames (net.suid))
    
    # get names from cy2.edge.names (net.suid@graph)
    checkEquals (getEdgeAttribute (net.suid, "B (synthetic lethal) C", 'treeSpecies'), "yew")
    checkEquals (getEdgeAttribute (net.suid, "B (synthetic lethal) C", 'score'), -12)
    
    deleteEdgeAttribute (net.suid, 'species')
    deleteEdgeAttribute (net.suid, 'ageInYears')
    
    invisible (net.suid)
    
} #  test.addGetAndDeleteEdgeAttributes
#-------------------------------------------------------------------------------
test.addGetAndDeleteNodeAttributes = function ()
{
    title = 'test.addGetAndDeleteNodeAttributes'
    test.prep (title)
    
    # in this test we add two new node attributes, 'species' and 'ageInYears'
    # if they are already defined, from a previous run of this test, start by deleting them.
    
    g  = makeSimpleGraph ()
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    redraw (net.suid)
    
    novel.noa.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getNodeAttributeNames(net.suid))
    for (noa.name in novel.noa.to.delete){
        deleteNodeAttribute (noa.name)
    }
    
    # name is added by Cytoscape
    checkEquals (length (intersect (getNodeAttributeNames (net.suid), c ("name", "count",  "label", "lfc", "type"))), 5)
    
    # now add an attribute to two of the nodes
    first.two.nodes = nodes (g) [1:2]
    values = c ('cedar', 'ash')
    setNodeAttributesDirect (net.suid, 'treeSpecies', 'char', first.two.nodes, values)
    
    # now add an attribute to a single node.  this exercises a different branch in RCytoscape:setNodeAttributesDirect
    first.node = nodes (g) [1]
    value = 'one millenium'
    setNodeAttributesDirect (net.suid, 'ageInYears', 'char', first.node, value)
    checkTrue ('ageInYears' %in% getNodeAttributeNames (net.suid))
    checkEquals (getNodeAttribute (net.suid, 'B', 'type'), 'transcription factor')
    checkEquals (getNodeAttribute (net.suid, 'A', 'ageInYears'), 'one millenium')
    checkEquals (getNodeAttribute (net.suid, 'B', 'ageInYears'), '')
    
    deleteNodeAttribute (net.suid, 'species')
    deleteNodeAttribute (net.suid, 'ageInYears')
    
    invisible (net.suid)
    
} #  test.addGetAndDeleteNodeAttributes
#-------------------------------------------------------------------------------
test.getAllNodeAttributes = function ()
{
    title = 'test.getAllNodeAttributes'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    net.suidc = existing.CytoscapeNetwork (title, copy=T)
    tbl.noa <- getAllNodeAttributes (net.suidc)
    checkEquals (nrow (tbl.noa), 3)
    checkTrue (ncol (tbl.noa) >= 5)
    expected.colnames =  c ("name", "count", "label", "lfc", "type")  # created here
    checkEquals (length (intersect (colnames (tbl.noa), expected.colnames)), 5)
    checkEquals (sort (rownames (tbl.noa)), c ("A", "B", "C"))
    
    # now try a graph with only one node attribute.  this case used to fail (pshannon, 16 feb 2011)
    
    g2 = new ('graphNEL', edgemode='directed')
    g2 = initNodeAttribute (g2, 'label', 'char', 'NA')
    g2 = graph::addNode ('A', g2)
    nodeData (g2, 'A', 'label') = 'a label for A'
    window.title = 'single node attribute test'
    if (window.title %in% as.character (getNetworkList (net.suid)))
        deleteNetwork (net.suid, window.title)
    net.suid2 = CytoscapeNetwork (window.title, graph=g2)
    tbl.noa2 = getAllNodeAttributes (net.suid2)
    checkEquals (ncol (tbl.noa2), 1)
    checkEquals (nrow (tbl.noa2), 1)
    checkEquals (colnames (tbl.noa2), 'label')
    checkEquals (rownames (tbl.noa2), 'A')
    
    invisible (list (a=tbl.noa, b=tbl.noa2))
    
} # test.getAllNodeAttributes
#-------------------------------------------------------------------------------
test.getAllEdgeAttributes = function ()
{
    title = 'test.getAllEdgeAttributes'
    test.prep (title)
    
    net.suid =  createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    tbl.eda = getAllEdgeAttributes (net.suid)
    checkEquals (class (tbl.eda), 'data.frame')
    checkEquals (dim (tbl.eda), c (3, 5))
    checkEquals (sort (rownames (tbl.eda)), c ("A|B", "B|C", "C|A"))
    checkEquals (sort (colnames (tbl.eda)), c ("edgeType", "misc", "score", "source", "target"))
    checkEquals (class (tbl.eda$score), 'numeric')
    
    # now try a graph with one edge, and just one edge attribute, to make sure that this edge case is handled properly
    g2 = new ('graphNEL', edgemode='directed')
    g2 = initEdgeAttribute (g2, 'edgeType', 'char', 'unspecified')
    g2 = graph::addNode ('A', g2)
    g2 = graph::addNode ('B', g2)
    g2 = addEdge ('A', 'B', g2)
    
    edgeData (g2, 'A', 'B', 'edgeType') = 'phosphorylates'
    
    cy = CytoscapeConnection ()
    
    window.title = 'edge attribute test, one attribute only'
    if (window.title %in% as.character (getNetworkList ())){
        deleteNetwork (window.title)
    }
    
    net.suid2 = CytoscapeNetwork (window.title, graph=g2, create.window=FALSE)
    tbl.eda2 = getAllEdgeAttributes (net.suid2)
    
    checkEquals (ncol (tbl.eda2), 3)
    checkEquals (nrow (tbl.eda2), 1)
    checkEquals (sort (colnames (tbl.eda2)), c ('edgeType', 'source', 'target'))
    
    invisible (tbl.eda2)
    
} # test.getAllEdgeAttributes
#-------------------------------------------------------------------------------
test.getVisualStyleNames = function ()
{
    title = 'test.getVisualStyleNames'
    test.prep (title)
    
    net.suid3 =  createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid3)
    redraw (net.suid3)
    layoutNetwork (net.suid3)
    current.names = getVisualStyleNames (net.suid3)
    checkTrue (length (intersect (current.names, c (title, 'default', 'Nested Network Style', 'Minimal', 'Sample1', 'Universe'))) >= 3)
    
    invisible (net.suid3)
    
} # test.getVisualStyleNames
#-------------------------------------------------------------------------------
test.copyVisualStyle = function ()
{
    title = 'test.copyVisualStyle'
    test.prep (title)
    
    net.suid4 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid4)
    redraw (net.suid4)
    layoutNetwork (net.suid4)
    
    current.names = getVisualStyleNames (net.suid4)
    
    unique.name = FALSE
    new.style.name = sprintf ("tmp.%s", runif (1, 1, 1000))
    copyVisualStyle (net.suid4, 'default', new.style.name)
    new.names = getVisualStyleNames (net.suid4)
    checkEquals (setdiff (new.names, current.names), new.style.name)
    
    invisible (net.suid4)
    
} # test.copyVisualStyle
#-------------------------------------------------------------------------------
test.setVisualStyle = function ()
{
    title = 'test.setVisualStyle'
    test.prep (title)
    cy = CytoscapeConnection ()
    
    net.suid5 = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid5)
    redraw (net.suid5)
    layoutNetwork (net.suid5)
    
    current.names = getVisualStyleNames (net.suid5)
    for (style.name in current.names[1:5]) {
        setVisualStyle (net.suid5, style.name)
        Sys.sleep (0.1)
    } # for style.name
    
    invisible (net.suid5)
    
} # test.setVisualStyle
#-------------------------------------------------------------------------------
# meager test only:  make sure all of these methods can be called
# todo:  call set, call get, check for color match
test.defaultColors = function ()
{
    title = 'test.defaultColors'
    test.prep (title)
    cy = CytoscapeConnection ()
    getDefaultBackgroundColor ()
    getDefaultEdgeReverseSelectionColor ()
    getDefaultEdgeSelectionColor ()
    getDefaultNodeReverseSelectionColor ()
    getDefaultNodeSelectionColor ()
    
    black = '#000000'
    red = '#FF0000'
    white = '#FFFFFF'
    green = '#00FF00'
    gray = '#888888'
    
    setDefaultBackgroundColor (white)
    setDefaultEdgeReverseSelectionColor (red)
    setDefaultEdgeSelectionColor (green)
    setDefaultNodeReverseSelectionColor (red)
    setDefaultNodeSelectionColor (green)
    
} # test.defaultColors
#-------------------------------------------------------------------------------
test.fitContent = function ()
{
    title = 'test.fitContent'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    clearSelection (net.suid)
    selectNodes (net.suid, 'A')
    checkEquals (getSelectedNodeCount (net.suid), 1)
    
    #fitSelectedContent (net.suid)
    fitContent (net.suid)
    
} # test.fitContent
#-------------------------------------------------------------------------------
# test.windowCoordinates = function ()
# {
#     title = 'test.windowCoordinates'
#     test.prep (title)
#     net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
#     displayGraph (net.suid)
#     redraw (net.suid)
#     layoutNetwork (net.suid)
#
#     center = getCenter (net.suid)
#     checkEquals (names (center), c ('x', 'y'))
#
#     corners = getViewCoordinates (net.suid)
#     checkEquals (names (corners), c ('top.x', 'top.y', 'bottom.x', 'bottom.y'))
#
#     invisible (net.suid)
#
#} # test.windowCoordinates
#-------------------------------------------------------------------------------
test.zoom = function ()
{
    title = 'test.zoom'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    fitContent (net.suid)
    
    smaller = 0.5
    larger = 2
    
    for (i in 1:10){
        setZoom (net.suid, smaller * getZoom (net.suid))
    }
    
    for (i in 1:10){
        setZoom (net.suid, larger * getZoom (net.suid))
    }
    
    invisible (net.suid)
    
} # test.zoom
#-------------------------------------------------------------------------------
test.center = function ()
{
    title = 'test.setCenter'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    fitContent (net.suid)
    setZoom (net.suid, 0.5 * getZoom (net.suid))
    
    center.orig = getCenter (net.suid)
    delta = 100
    x.left = center.orig$x - delta
    x.right = center.orig$x + delta
    y.up = center.orig$y - delta
    y.down = center.orig$y + delta
    
    for (i in 1:10) {
        setCenter (net.suid, x.left, y.up)
        setCenter (net.suid, as.integer (x.left), as.integer (y.up))   # make sure the called function casts this int back to numeric
        setCenter (net.suid, x.left, y.down)
        setCenter (net.suid, x.right, y.down)
        setCenter (net.suid, x.right, y.up)
    } # for i
    
    setCenter (net.suid, center.orig$x, center.orig$y)
    
    invisible (net.suid)
    
} # test.center
#-------------------------------------------------------------------------------
test.setNodeSizeDirect = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeSizeDirect'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    lockNodeDimensions (net.suid, TRUE)
    
    small = 30
    large = 300
    setNodeSizeDirect (net.suid, 'A', small);
    setNodeSizeDirect (net.suid, 'A', large);
    
    invisible (net.suid)
    
} # test.setNodeSizeDirect
#-------------------------------------------------------------------------------
test.setNodeWidthAndHeightDirect = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeWidthAndHeightDirect'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    lockNodeDimensions (net.suid, FALSE)
    
    small = 30
    large = 300
    
    setNodeWidthDirect (net.suid, 'A', small);
    setNodeHeightDirect (net.suid, 'A', large);
    setNodeWidthDirect (net.suid, 'A', large);
    setNodeHeightDirect (net.suid, 'A', small);
    
    invisible (net.suid)
    
} # test.setNodeWidthAndHeightDirect
#-------------------------------------------------------------------------------
test.setNodeFontSizeDirect = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeFontSizeDirect'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    starting.size = 4
    setNodeSizeDirect (net.suid, c ('A', 'B', 'C'), 50)
    setNodeFontSizeDirect (net.suid, c ('A', 'B'), 12)
    redraw (net.suid)
    
    for (i in 1:20) {
        setNodeFontSizeDirect (net.suid, 'A', starting.size + i)
        setNodeFontSizeDirect (net.suid, 'B', starting.size + (i*3))
    } # for i
    
    starting.size = 32
    for (i in 20:1) {
        setNodeFontSizeDirect (net.suid, 'A', starting.size - i)
        setNodeFontSizeDirect (net.suid, 'B', starting.size - (i*3))
    } # for i
    
    invisible (net.suid)
    
} # test.setNodeSizeDirect
#-------------------------------------------------------------------------------
test.setNodeShapeDirect = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeShapeDirect'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    lockNodeDimensions (net.suid, TRUE)
    setNodeSizeDirect (net.suid, 'A', 100)
    
    for (new.shape in getNodeShapes (net.suid)) {
        setNodeShapeDirect (net.suid, 'A', new.shape)
    } # for new.shape
    
    invisible (net.suid)
    
} # test.setNodeShapeDirect
#-------------------------------------------------------------------------------
# add a node to an existing graph.
# questions:
#  1) what edge attribute values are assigned to this new edge?
#  2) can we assign new values to those attributes?  use setEdgeAttributesDirect
test.addCyNode = function ()
{
    title = 'test.addCyNode'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid, 'grid')
    checkEquals (getNodeCount (net.suid), 3)
    addCyNode (net.suid, 'NEW')
    layoutNetwork (net.suid, 'grid')
    checkEquals (getNodeCount (net.suid), 4)
    invisible (net.suid)
    
} # test.addCyNode
#-------------------------------------------------------------------------------
# add an edge to an existing graph.
# questions:
#  1) what edge attribute values are assigned to this new edge?
#  2) can we assign new values to those attributes?  use setEdgeAttributesDirect
test.addCyEdge = function ()
{
    title = 'test.addCyEdge'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    checkEquals (getEdgeCount (net.suid), 3)
    
    addCyNode (net.suid, 'NEW')
    directed.edge = TRUE
    addCyEdge (net.suid, 'A', 'NEW', 'synthetic rescue', directed.edge)
    redraw (net.suid)
    layoutNetwork (net.suid)
    checkEquals (getEdgeCount (net.suid), 4)
    invisible (net.suid)
    
} # test.addCyEdge
#-------------------------------------------------------------------------------
test.twoGraphsDoubleEdges = function ()
{
    title = 'test.twoGraphsDoubleEdges'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
    g2 = new ('graphNEL', edgemode='directed')
    g2 = initEdgeAttribute (g2, 'edgeType', 'char', 'unspecified')
    
    g2 = graph::addNode ('A', g2)
    g2 = graph::addNode ('B', g2)
    g2 = addEdge ('A', 'B', g2)
    
    edgeData (g2, 'A', 'B', 'edgeType') = 'synthetic rescue'
    
    addGraphToGraph (net.suid, g2)
    redraw (net.suid)
    layoutNetwork (net.suid)
    
} # test.twoGraphsoubleEdges
#-------------------------------------------------------------------------------
test..classicGraphToNodePairTable = function ()
{
    print (noquote ('------- test..classicGraphToNodePairTable'))
    
    # first, our standard demo graph, directed, no reciprocal edges
    
    g = makeSimpleGraph ()
    tbl.g = RCy3:::.classicGraphToNodePairTable (g)
    checkEquals (dim (tbl.g), c (3, 3))
    checkEquals (colnames (tbl.g), c ("source", "target", "edgeType"))
    checkEquals (tbl.g$edgeType, c ("phosphorylates", "synthetic lethal", "undefined"))
    checkEquals (tbl.g$source, c ("A", "B", "C"))
    checkEquals (tbl.g$target, c ("B", "C", "A"))
    
    # now extend the standard demo graph by adding an edge between C and B, making B & C reciprocally related nodes
    
    gx = makeSimpleGraph ()
    gx = graph::addEdge ('C', 'B', gx)
    edgeData (gx, 'C', 'B', attr='edgeType') = 'synthetic rescue'
    tbl.egx = RCy3:::.classicGraphToNodePairTable (gx)
    checkEquals (dim (tbl.egx), c (4, 3))
    checkEquals (colnames (tbl.egx), c ("source", "target", "edgeType"))
    checkEquals (tbl.egx$edgeType, c ("phosphorylates", "synthetic lethal", "undefined", "synthetic rescue"))
    checkEquals (tbl.egx$source, c ("A", "B", "C", "C"))
    checkEquals (tbl.egx$target, c ("B", "C", "A", "B"))
    
} # test..classicGraphToNodePairTable
#-------------------------------------------------------------------------------
test.rcy.edgeNames = function ()
{
    print (noquote ('------- test.rcy.edgeNames'))
    g = makeSimpleGraph ()
    checkEquals (sort (RCy3:::.rcyEdgeNames (g)), c ("A~B", "B~C", "C~A"))
    
    # now extend the standard demo graph by adding an edge between C and B, making B & C reciprocally related nodes
    gx = makeSimpleGraph ()
    gx = graph::addEdge ('C', 'B', gx)
    edgeData (gx, 'C', 'B', attr='edgeType') = 'synthetic rescue'
    checkEquals (sort (RCy3:::.rcyEdgeNames (gx)), c ("A~B", "B~C", "C~A", "C~B"))
    
} # test.rcy.edgeNames
#-------------------------------------------------------------------------------
restore.defaults = function ()
{
    cy = CytoscapeConnection ()
    setDefaultBackgroundColor ('#CCCCFF')
    setDefaultNodeShape ('ellipse')
    lockNodeDimensions (TRUE)
    setDefaultNodeSelectionColor ('#FFFF00')
    setDefaultNodeReverseSelectionColor ('#00FF00')
    setDefaultEdgeSelectionColor ('#FF0000')
    setDefaultEdgeReverseSelectionColor ('#00FF00')
    setNodeSizeDefault (30)
    setNodeColorDefault ('#FF8888')  # a guess
    
    setNodeBorderColorDefault ('#000000')
    setNodeBorderWidthDefault (1)
    setNodeFontSizeDefault (12)
    setNodeLabelColorDefault ('#000000')
    setEdgeLineWidthDefault (1)
    setEdgeColorDefault ('#0000FF')
    
} # restore.defaults
#-------------------------------------------------------------------------------
test..getNovelEdges = function ()
{
    g.3e <- makeSimpleGraph ()
    g.0e <- new ("graphNEL", edgemode = "directed")
    g.0e = initEdgeAttribute (g.0e, 'edgeType', 'char', 'unspecified')
    
    # no novel edges if the 2nd arg has no edges
    checkTrue (is.na (RCy3:::.getNovelEdges (g.3e, g.0e)))
    
    # three novel edges if the 1st arg has zero edges, the second has 3
    novel.edges <- RCy3:::.getNovelEdges (g.0e, g.3e)
    checkEquals (length (novel.edges), 3)
    
    # add one edge to g.0e which is an exact duplicate of the first edge of g.3e
    
    g.1e = graph::addNode ('A', g.0e)
    g.1e = graph::addNode ('B', g.1e)
    g.1e = addEdge ('A', 'B', g.1e)
    edgeData (g.1e, 'A', 'B', attr='edgeType') = 'phosphorylates'
    
    g1 <- g.1e
    g3 <- g.3e
    
    novel.edges <- RCy3:::.getNovelEdges (g.3e, g.1e)
    checkEquals (length (novel.edges), 0)
    
    novel.edges <- RCy3:::.getNovelEdges (g.1e, g.3e)
    checkEquals (length (novel.edges), 2)
    
} # test..getNovelEdges
#-------------------------------------------------------------------------------
# apparently does not run reliably at bioc
hiddenTest.saveImage = function ()
{
    title = 'test.saveImage'
    test.prep (title)
    
    g.simple = RCy3::makeSimpleGraph ()
    net.suid = CytoscapeNetwork (title, g.simple)
    
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    setNodeLabelMapping (net.suid, 'label')
    redraw (net.suid)
    
    #--- png first
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest')
    printf ('saving image file to %s', filename)
    saveImage (net.suid, filename, 'png', 1.0)
    checkTrue (file.exists (paste0(filename, '.png')))
    
    #--- now pdf
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest')
    printf ('saving image file to %s', filename)
    saveImage (net.suid, filename, 'pdf')
    checkTrue (file.exists (paste0(filename, '.pdf')))
    
    #--- now svg
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest')
    printf ('saving image file to %s', filename)
    saveImage (net.suid, filename, 'svg')
    checkTrue (file.exists (paste0(filename, '.svg')))
    
    invisible (net.suid)
    
} # test.saveImage
#-------------------------------------------------------------------------------
# apparently does not run reliably at bioc
hiddenTest.saveNetwork = function ()
{
    title = 'test.saveNetwork'
    test.prep (title)
    
    g.simple = RCy3::makeSimpleGraph ()
    net.suid = CytoscapeNetwork (title, g.simple)
    
    displayGraph (net.suid)
    layoutNetwork (net.suid, 'grid')
    setNodeLabelMapping (net.suid, 'label')
    redraw (net.suid)
    
    filename = sprintf ('%s/%s', tempdir (), 'saveNetworkTest')
    printf ('saving cys file to %s', filename)
    saveNetwork (net.suid, filename)
    checkTrue (file.exists (paste0(filename, '.cys')))
    
    invisible (net.suid)
    
} # test.saveNetwork
# also not an automated test, though exception testing could accomplish that
test.detectUnitializedNodeAttributes = function ()
{
    # starting with the code in makeSampleGraph, change 3 node and 1 edge attribute to use the  standard (not RCy)
    # attribute initializations. this is an error with respect to RCy, which needs explicit typing of the attributes
    # see if they are caught
    
    g = new("graphNEL", edgemode = "directed")
    
    g = initNodeAttribute(g, "type", "char", "undefined")
    g = initNodeAttribute(g, "lfc", "numeric", 1)
    g = initNodeAttribute(g, "label", "char", "default node label")
    g = initNodeAttribute(g, "count", "integer", 0)
    
    nodeDataDefaults (g, attr='type') = ''
    nodeDataDefaults (g, attr='lfc') = 0.0
    nodeDataDefaults (g, attr='label') = ''
    
    g = initEdgeAttribute(g, "edgeType", "char", "undefined")
    g = initEdgeAttribute(g, "score", "numeric", 0)
    g = initEdgeAttribute(g, "misc", "char", "default misc")
    
    g = graph::addNode("A", g)
    g = graph::addNode("B", g)
    g = graph::addNode("C", g)
    nodeData(g, "A", "type") = "kinase"
    nodeData(g, "B", "type") = "transcription factor"
    nodeData(g, "C", "type") = "glycoprotein"
    nodeData(g, "A", "lfc") = -3
    nodeData(g, "B", "lfc") = 0
    nodeData(g, "C", "lfc") = 3
    nodeData(g, "A", "count") = 2
    nodeData(g, "B", "count") = 30
    nodeData(g, "C", "count") = 100
    nodeData(g, "A", "label") = "Gene A"
    nodeData(g, "B", "label") = "Gene B"
    nodeData(g, "C", "label") = "Gene C"
    g = graph::addEdge("A", "B", g)
    g = graph::addEdge("B", "C", g)
    g = graph::addEdge("C", "A", g)
    edgeData(g, "A", "B", "edgeType") = "phosphorylates"
    edgeData(g, "B", "C", "edgeType") = "synthetic lethal"
    edgeData(g, "A", "B", "score") = 35
    edgeData(g, "B", "C", "score") = -12
    
    net.suid = CytoscapeNetwork (title = 'detect unitialized node attributes 1', graph = g)
    
} # test.detectUnitializedNodeAttributes
#-------------------------------------------------------------------------------
# also not an automated test, though exception testing could accomplish that
test.detectUnitializedEdgeAttributes = function ()
{
    # starting with the code in makeSampleGraph, change 3 node and 1 edge attribute to use the standard (not RCy)
    # attribute initializations. This is an error with respect to RCy, which needs explicit typing of the attributes
    # see if they are caught
    
    g = new("graphNEL", edgemode = "directed")
    
    g = initNodeAttribute(g, "type", "char", "undefined")
    g = initNodeAttribute(g, "lfc", "numeric", 1)
    g = initNodeAttribute(g, "label", "char", "default node label")
    g = initNodeAttribute(g, "count", "integer", 0)
    
    g = initEdgeAttribute(g, "edgeType", "char", "undefined")
    g = initEdgeAttribute(g, "score", "numeric", 0)
    g = initEdgeAttribute(g, "misc", "char", "default misc")
    edgeDataDefaults (g, attr='misc') = ''
    
    g = graph::addNode("A", g)
    g = graph::addNode("B", g)
    g = graph::addNode("C", g)
    nodeData(g, "A", "type") = "kinase"
    nodeData(g, "B", "type") = "transcription factor"
    nodeData(g, "C", "type") = "glycoprotein"
    nodeData(g, "A", "lfc") = -3
    nodeData(g, "B", "lfc") = 0
    nodeData(g, "C", "lfc") = 3
    nodeData(g, "A", "count") = 2
    nodeData(g, "B", "count") = 30
    nodeData(g, "C", "count") = 100
    nodeData(g, "A", "label") = "Gene A"
    nodeData(g, "B", "label") = "Gene B"
    nodeData(g, "C", "label") = "Gene C"
    g = graph::addEdge("A", "B", g)
    g = graph::addEdge("B", "C", g)
    g = graph::addEdge("C", "A", g)
    edgeData(g, "A", "B", "edgeType") = "phosphorylates"
    edgeData(g, "B", "C", "edgeType") = "synthetic lethal"
    edgeData(g, "A", "B", "score") = 35
    edgeData(g, "B", "C", "score") = -12
    
    net.suid = CytoscapeNetwork (title = 'detect unitialized node attributes 2', graph = g)
    
} 