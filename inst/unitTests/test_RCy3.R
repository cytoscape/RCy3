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
    
    test.setNodeLabelMapping ()
    test.setEdgeLabelMapping ()
    test.setNodeTooltipMapping ()
    test.setEdgeTooltipMapping ()
    test.setNodeColorMapping ()
    test.setNodeBorderColorMapping ()
    test.setNodeBorderWidthMapping ()
    test.setNodeSizeMapping ()
    test.setNodeShapeMapping ()
    test.setNodeComboOpacityMapping ()
    test.setNodeColorBypass ()
    test.setNodeBorderColorBypass ()
    test.setNodeLabelBypass () 
    test.setNodeOpacityBypass ()  
    
    deleteAllNetworks ()
    
    test.setEdgeOpacityBypass ()  #--> too slow
    test.setEdgeColorBypass ()  #--> too slow
    test.setEdgeSourceArrowShapeBypass ()
    test.setEdgeTargetArrowShapeBypass ()
    test.setEdgeSourceArrowColorBypass ()
    test.setEdgeTargetArrowColorBypass ()
    test.setEdgeLabelBypass ()
    test.setEdgeFontSizeBypass ()  #--> too slow
    test.setEdgeLabelColorBypass ()  #--> too slow
    test.setEdgeTooltipBypass ()
    test.setEdgeLineWidthBypass ()
    test.setEdgeLineStyleBypass ()

    deleteAllNetworks ()
    
    test.countNodes ()
    test.countEdges ()
    test.getAllNodes ()
    test.getAllEdges ()
    test.selectNodes ()
    
    test.nodeNeighborReportingAndSelection ()
    test.invertSelection ()
    test.deleteSelectedNodes ()

    deleteAllNetworks ()
    
    test.hideNodes ()
    test.selectEdges ()
    test.setEdgeLineStyleMapping ()
    test.setEdgeLineWidthMapping ()
    test.setEdgeColorMapping ()
    # test.setEdgeOpacityMapping()
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
    # #test.setNodeSizeBypass ()  #--> too slow
    # #test.setNodeWidthAndHeightBypass ()  #--> too slow
    # 
    # deleteAllNetworks ()
    # 
    # test.setNodeFontSizeBypass ()  #--> too slow
    # test.setNodeShapeBypass ()  #--> too slow
    # #test.setEdgeVizPropertiesBypass ()  #--> too slow
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
    # #test.setNodeImageBypass ()
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
    
    setEdgeColorDefault ('#5588FF')
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
    
    setNodeLabelMapping ('id')
    setNodeLabelMapping ('score')
    setNodeLabelMapping ('group')
    setNodeLabelMapping ('name')
}
#-------------------------------------------------------------------------------
test.setEdgeLabelMapping = function ()
{
    title = 'test.setEdgeLabelMapping'
    test.prep (title, F)
    
    setEdgeLabelMapping ('weight')
    setEdgeLabelMapping ('name')
} 
#-------------------------------------------------------------------------------
test.setNodeTooltipMapping = function ()
{
    title = 'test.setNodeTooltipMapping'
    test.prep (title, F)
    
    setNodeTooltipMapping ('group')
} 
#-------------------------------------------------------------------------------
test.setEdgeTooltipMapping = function ()
{
    title = 'test.setEdgeTooltipMapping'
    test.prep (title, F)

    setEdgeTooltipMapping ('weight')
} 
#-------------------------------------------------------------------------------
test.setNodeColorMapping = function ()
{
    title = 'test.setNodeColorMapping'
    test.prep (title, F)
    
    # first, continuous
    node.attribute.values = c (0.0, 10.0, 20.0)
    node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
    setNodeColorMapping ('score', node.attribute.values, node.colors, 'c')
    
    # now, discrete
    node.attribute.values = c ("A",  "B")
    node.colors =           c ('#8888FF', "#00CCCC")
    setNodeColorMapping ('group', node.attribute.values, node.colors, 'd')
    
    # now, test default
    node.attribute.values = c ("A")
    node.colors =           c ('#8888FF')
    setNodeColorMapping ('group', node.attribute.values, node.colors, 'd', default.color = '#5588FF')
} 
#-------------------------------------------------------------------------------
test.setNodeBorderColorMapping = function ()
{
    title = 'test.setNodeColorMapping'
    test.prep (title, F)
    
    # first, continuous
    node.attribute.values = c (0.0, 10.0, 20.0)
    node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
    setNodeBorderColorMapping ('score', node.attribute.values, node.colors, 'c')
    
    # now, discrete
    node.attribute.values = c ("A",  "B")
    node.colors =           c ('#8888FF', "#00CCCC")
    setNodeBorderColorMapping ('group', node.attribute.values, node.colors, 'd')
    
    # now, test default
    node.attribute.values = c ("A")
    node.colors =           c ('#8888FF')
    setNodeBorderColorMapping ('group', node.attribute.values, node.colors, 'd', default.color = '#5588FF')
} 
#-------------------------------------------------------------------------------
test.setNodeBorderWidthMapping = function ()
{
    title  = 'test.setNodeBorderWidthMapping'
    test.prep (title,F)
    
    # set the stage by making all the nodes white, to provide better contrast for the node border colors
    node.attribute.values = c (0.0, 10.0, 20.0)
    node.colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
    setNodeColorMapping ('score', node.attribute.values, node.colors, 'c')
    setNodeBorderColorDefault ('#5588FF')
    
    # 2 different node border sizes
    node.attribute.values = c ("A",  "B")
    border.widths =         c (5, 20)
    setNodeBorderWidthMapping ('group', node.attribute.values, border.widths, 'd')
    # swap them around different node border sizes
    node.attribute.values = c ("A",  "B")
    border.widths =         c (20, 5);
    setNodeBorderWidthMapping ('group', node.attribute.values, border.widths, 'd')
}
#-------------------------------------------------------------------------------
test.setNodeSizeMapping = function ()
{
    title = 'test.setNodeSizeMapping'
    test.prep (title,F)

    count.control.points = c(5,  20)
    node.sizes           = c(1, 80,  120, 300)
    setNodeSizeMapping ('score', count.control.points, node.sizes, 'c')
    system ('sleep 0.3')
    
    # now chop off the below & above values.  A should grow to 80, almost as big as B, and C should shrink to 120, larger that B
    
    count.control.points = c(5,  20)
    node.sizes           = c(80,  120)
    setNodeSizeMapping ('score', count.control.points, node.sizes, 'c')
    system ('sleep 0.3')
    
    # now use a 'd' Mapping. 
    molecule.types = c('A')
    node.sizes     = c(60)
    setNodeSizeMapping ('group', molecule.types,  node.sizes, default.size= 5, 'd')
} 
#-------------------------------------------------------------------------------
test.setNodeShapeMapping = function ()
{
    title = 'test.setNodeShapeMapping'
    test.prep (title,F)

    # make Mapping for 2 of 3 node types, leaving the third as the default
    node.shapes = c ('diamond', 'triangle')
    attribute.values = c ('A', 'B')
    setNodeShapeMapping ('group', attribute.values, node.shapes, default.shape='ellipse')
    
    # test one-element lists
    node.shapes = c ('diamond')
    attribute.values = c ('B')
    setNodeShapeMapping ('group', attribute.values, node.shapes, default.shape='ellipse')
} 
#-------------------------------------------------------------------------------
test.setNodeComboOpacityMapping = function ()
{
    title = 'test.setNodeComboOpacityMapping'
    test.prep (title, F)
    
    # make the node borders prominent
    setNodeBorderColorDefault ('#55FF88')
    setNodeBorderWidthDefault (10)
    
    score.values = c (5, 10, 20)
    
    # make the nodes big, give them strong colors
    setNodeSizeBypass (getAllNodes(), 100)
    setNodeColorMapping ('score', score.values, c ('#FF0000', '#00FF00', '#0000FF'), 'c')
    layoutNetwork ('grid')
    
    # first, the continuous 'interpolate' case, in which opacity is a function of score
    opacities = c (10, 128, 255)
    setNodeComboOpacityMapping ('score', score.values, opacities, 'c')
    
    # reset
    setNodeComboOpacityMapping ('score', score.values, c (255, 255, 255), 'c');
    
    scalar.values = c("A","B")
    # lookup
    setNodeComboOpacityMapping ('group', scalar.values, c (40, 128), 'd');   
 
    # reset
    setNodeComboOpacityMapping ('group', scalar.values, c (255, 255), 'd');   
 
} 
#-------------------------------------------------------------------------------
test.setNodeColorBypass = function ()
{
    title = 'test.setNodeColorBypass'
    test.prep (title,F)
    
    setNodeColorBypass ('node 0', '#AA0088')
    setNodeColorBypass ( c ('node 1', 'node 2'), '#448844')
} 
#-------------------------------------------------------------------------------
test.setNodeBorderColorBypass = function ()
{
    title = 'test.setNodeBorderColorBypass'
    test.prep (title,F)
    
    setNodeBorderColorBypass ('node 0', '#AA4488')
    setNodeBorderColorBypass (c ('node 1', 'node 2'), '#AA8888')
} 
#-------------------------------------------------------------------------------
test.setNodeLabelBypass = function ()
{
    title = 'test.setNodeLabelBypass'
    test.prep (title,F)

    setNodeLabelBypass ('node 0', 'new A label')

    setNodeLabelBypass (getAllNodes(), '')
    setNodeLabelBypass (c ('node 0', 'node 2'), c ('AzA', 'ByB'))
} 
#-------------------------------------------------------------------------------
test.setNodeLabelPropertiesBypass = function ()
{
    print ('--- test.setNodeLabelsPropertiesBypass')
    title = 'test.setNodeLabelPropertiesBypass'
    test.prep (title,F)
    
    sizes = c (10, 50, 80)
    colors = c ('#0000FF', '#00FF00', '#FF0000')
    for (i in 1:length (sizes)) {
        setNodeFontSizeBypass ('node 0', sizes [i])
        setNodeLabelColorBypass ('node 0', colors [i])
    }
} 
#-------------------------------------------------------------------------------
test.setNodeOpacityBypass = function ()
{
    title = 'test.setNodeOpacityBypass'
    test.prep (title, F)

    setNodeSizeBypass ('node 3', 120)
    layoutNetwork ('grid')
    fitContent ()
    setNetworkZoomBypass (0.8 * getNetworkZoom())
    
    setNodeFillOpacityBypass ('node 0', 0)
    setNodeLabelOpacityBypass ('node 1', 0)
    setNodeBorderOpacityBypass ('node 2', 0)
    for (i in 1:3) {
        setNodeOpacityBypass ('node 3', 0)
        setNodeOpacityBypass ('node 3', 255)
    } 
    
    setNodeOpacityBypass (c ('node 0', 'node 2'), 255)
    setNodeOpacityBypass (c ('node 1', 'node 3'), 50)
    setNodeOpacityBypass (c ('node 0', 'node 1', 'node 2', 'node 3'), c (10, 50, 100, 200))
    setNodeOpacityBypass (c ('node 0', 'node 1', 'node 2', 'node 3'), c (200, 100, 50, 10))
    Sys.sleep (0.3)
    
    setNodeOpacityBypass (c ('node 0', 'node 1', 'node 2', 'node 3'), 255); 
}
#-------------------------------------------------------------------------------
test.setEdgeOpacityBypass = function ()
{
    title = 'test.setEdgeOpacityBypass'
    test.prep (title)

    edge.names <- getAllEdges()

    setEdgeOpacityBypass (edge.names [1], 80)
    setEdgeOpacityBypass (edge.names [2], 0)
    setEdgeOpacityBypass (edge.names [3], 255)

    setEdgeOpacityBypass (edge.names [2], 80)
    setEdgeOpacityBypass (edge.names [3], 0)
    setEdgeOpacityBypass (edge.names [1], 255)

    setEdgeOpacityBypass (edge.names [1], 80)
    setEdgeOpacityBypass (edge.names [3], 40)
    setEdgeOpacityBypass (edge.names [2], 255)

    setEdgeOpacityBypass (edge.names [1], 0)
    setEdgeOpacityBypass (edge.names [3], 0)
    setEdgeOpacityBypass (edge.names [2], 0)

    setEdgeOpacityBypass (edge.names [1], 255)
    setEdgeOpacityBypass (edge.names [3], 255)
    setEdgeOpacityBypass (edge.names [2], 255)
    
    setEdgeOpacityBypass (edge.names[1:3], c (0, 128, 255))
    setEdgeOpacityBypass (edge.names[1:3], c (255, 0, 128))

    setEdgeOpacityBypass (edge.names, 255)
}
#-------------------------------------------------------------------------------
test.setEdgeColorBypass = function ()
{
    title = 'test.setEdgeColorBypass'
    test.prep (title, F)
    
    edge.of.interest = getAllEdges()[1]
    setEdgeColorBypass (edge.of.interest, '#FF0000')
    setEdgeColorBypass (edge.of.interest, '#00FF00')
    setEdgeColorBypass (edge.of.interest, '#0000FF')
}
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowShapeBypass = function ()
{
    title = 'test.setEdgeSourceArrowShapeBypass'
    test.prep (title, F)

    edges.of.interest = getAllEdges()
    supported.arrow.shapes = getArrowShapes()
    
    # first try passing three edges and three arrow shapes
    setEdgeSourceArrowShapeBypass (edges.of.interest[1:3], supported.arrow.shapes [2:4])
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one arrow.shapes
    setEdgeSourceArrowShapeBypass (edges.of.interest[1:3], supported.arrow.shapes [6])
    
    # now loop through all of the arrow.shapes
    for (shape in supported.arrow.shapes) {
        setEdgeSourceArrowShapeBypass (edges.of.interest, shape)
    }
    # restore the default
    setEdgeSourceArrowShapeBypass (edges.of.interest, 'NONE')
}
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowShapeBypass = function ()
{
    title = 'test.setEdgeTargetArrowShapeBypass'
    test.prep (title, F)

    edges.of.interest = getAllEdges()
    supported.arrow.shapes = getArrowShapes ()
    
    # first try passing three edges and three arrow.shapes
    setEdgeTargetArrowShapeBypass (edges.of.interest[1:3], supported.arrow.shapes [5:7])
    
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one arrow shape
    setEdgeTargetArrowShapeBypass (edges.of.interest[1:3], supported.arrow.shapes [8])
    
    
    # now loop through all of the arrow.shapes
    for (shape in supported.arrow.shapes) {
        setEdgeTargetArrowShapeBypass (edges.of.interest, shape)
    }
    
    # restore the default
    setEdgeTargetArrowShapeBypass (edges.of.interest, 'NONE')
}
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowColorBypass = function ()
{
    title = 'test.setEdgeSourceArrowColorBypass'
    test.prep (title,F)
    
    arrows = c ('Arrow', 'Diamond', 'Circle')
    edgeType.values = c ('inhibits', 'activates', 'interaction')
    setEdgeSourceArrowMapping ('interaction', edgeType.values, arrows)
    setEdgeTargetArrowMapping ('interaction', edgeType.values, arrows)
    
    colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
    colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")
    
    edge.names = getAllEdges()
    
    setEdgeSourceArrowColorBypass (edge.names[1:3], colors.1)
    setEdgeSourceArrowColorBypass (edge.names[1:3], colors.2)
}
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowColorBypass = function ()
{
    title = 'test.setEdgeTargetArrowColorBypass'
    test.prep (title, F)
    
    arrows = c ('Arrow', 'Diamond', 'Circle')
    edgeType.values = c ('inhibits', 'activates', 'interaction')
    setEdgeSourceArrowMapping ('interaction', edgeType.values, arrows)
    setEdgeTargetArrowMapping ('interaction', edgeType.values, arrows)
    
    colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
    colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")
    
    edge.names = getAllEdges()
    
    setEdgeTargetArrowColorBypass (edge.names[1:3], colors.1)
    setEdgeTargetArrowColorBypass (edge.names[1:3], colors.2)
} 
#-------------------------------------------------------------------------------
test.setEdgeLabelBypass = function ()
{
    title = 'test.setEdgeLabelBypass '
    test.prep (title, F)
    
    edge.names = getAllEdges()[1:2]
    setEdgeLabelBypass (edge.names, 'some name')
} 
#-------------------------------------------------------------------------------
test.setEdgeFontFaceBypass = function ()
{
 title = 'test.setEdgeFontFaceBypass'
 test.prep (title, F)

 edge.of.interest = getAllEdges()[1]
 fonts = c ('courier', 'arial')
 for (font in fonts) {
   setEdgeFontFaceBypass (edge.of.interest, font)
   Sys.sleep (0.3)
   } 
} 
#-------------------------------------------------------------------------------
test.setEdgeFontSizeBypass = function ()
{
    title = 'test.setEdgeFontSizeBypass'
    test.prep (title, F)
    
    edge.of.interest = getAllEdges()[1]
    setEdgeFontSizeBypass (edge.of.interest, 12)
}
#-------------------------------------------------------------------------------
test.setEdgeLabelColorBypass = function ()
{
    title = 'test.setEdgeLabelColorBypass'
    test.prep (title,F)

    edge.names = getAllEdges()
    setEdgeLabelBypass (edge.names, 'some label')
    setEdgeLabelColorBypass (edge.names [1:2], '#FF0000')
    setEdgeLabelColorBypass (edge.names, '#00FF00')
    setEdgeLabelColorBypass (edge.names [3], '#000000')
}
#-------------------------------------------------------------------------------
test.setEdgeTooltipBypass = function ()
{
    title = 'test.setEdgeTooltipBypass'
    test.prep (title,F)
    
    edges.of.interest = getAllEdges()
    
    # first try passing three edges and three tooltips
    setEdgeTooltipBypass (edges.of.interest[1:3], c ('tooltip #1', 'tooltip #2', 'tooltip #3'))
    
    # now try passing three edges and one tooltip
    setEdgeTooltipBypass (edges.of.interest [1:3], 'a general purpose tooltip')
}
#-------------------------------------------------------------------------------
test.setEdgeLineWidthBypass = function ()
{
    title = 'test.setEdgeLineWidthBypass'
    test.prep (title,F)
    
    edges.of.interest = getAllEdges()[1:2]
    
    for (i in 1:10) {
        setEdgeLineWidthBypass (edges.of.interest, i)
    }
    #reset
    setEdgeLineWidthBypass (edges.of.interest, 1)
}
#-------------------------------------------------------------------------------
test.setEdgeLineStyleBypass = function ()
{
    title = 'test.setEdgeLineStyleBypass'
    test.prep (title,F)
    
    edges.of.interest = getAllEdges()
    
    supported.styles = getLineStyles()
    
    # first try passing three edges and three styles
    setEdgeLineStyleBypass (edges.of.interest[1:3], supported.styles [5:7])
    
    Sys.sleep (0.3)
    
    # now try passing three edges and one styles
    setEdgeLineStyleBypass (edges.of.interest, supported.styles [8])
    
    # now loop through all of the styles
    for (style in supported.styles) {
        setEdgeLineStyleBypass (edges.of.interest, style)
    }
    
    setEdgeLineStyleBypass (edges.of.interest, 'SOLID')
}
#-------------------------------------------------------------------------------
test.setEdgeLabelOpacityBypass = function ()
{
    title = 'test.setEdgeLabelOpacityBypass'
    test.prep (title, F)

    edge.of.interest = getAllEdges()[1]
    for (i in 1:5) {
        setEdgeOpacityBypass (edge.of.interest, i * 30)
    }
}
#-------------------------------------------------------------------------------
test.countNodes = function ()
{
    title = 'test.countNodes'
    test.prep (title)
    g <- RCy3::makeSimpleGraph()
    checkEquals (getNodeCount(), 4)
}
#-------------------------------------------------------------------------------
test.countEdges = function ()
{
    title = 'test.countEdges'
    test.prep (title, F)
    g <- RCy3::makeSimpleGraph()
    checkEquals (getEdgeCount(), 4)
}
#-------------------------------------------------------------------------------
test.getAllNodes = function ()
{
    title = 'test.getAllNodes'
    test.prep (title, F)
    
    nodes = getAllNodes()
    checkEquals (length (nodes), 4)
}
#-------------------------------------------------------------------------------
test.getAllEdges = function ()
{
    title = 'test.getAllEdges'
    test.prep (title, F)
    
    edges = getAllEdges()
    checkTrue ("node 0 (inhibits) node 1" %in% edges)
    checkTrue ("node 2 (interacts) node 3" %in% edges)
}
#-------------------------------------------------------------------------------
test.selectNodes = function ()
{
    title = 'test.selectNodes'
    test.prep (title, F)
    
    clearSelection ()
    checkEquals (getSelectedNodeCount(), 0)
    net.suide.nodes = selectNodes (c ('node 0', 'node 1'), 'name', preserve=T)
    checkEquals (getSelectedNodeCount(), 2)
    
    net.suide.nodes = selectNodes ('node 2', 'name', preserve=T)
    checkEquals (getSelectedNodeCount(), 3)
    
    clearSelection ()
    net.suide.nodes = selectNodes (c ('node 0', 'node 1'), 'name', preserve=TRUE)
    checkEquals (getSelectedNodeCount (), 2)
    net.suide.nodes = selectNodes ('node 2', 'name', preserve=FALSE)
    checkEquals (getSelectedNodeCount (), 1)
    checkEquals (getSelectedNodes (), 'node 2')
    
    clearSelection ()
    checkEquals (getSelectedNodeCount (), 0)
    nodes.to.select = c ('bogus', 'missing')
    selectNodes (nodes.to.select, 'name')
    checkEquals (getSelectedNodeCount (), 0)
    nodes.to.select = c (nodes.to.select, getAllNodes())
    selectNodes (nodes.to.select, 'name')
    checkEquals (getSelectedNodeCount (), 4)
}
#-------------------------------------------------------------------------------
test.nodeNeighborReportingAndSelection = function ()
{
    title = 'test.nodeNeighborReportingAndSelection'
    test.prep (title, F)
    
    # create a circular graph
    LETTERS = toupper (letters)
    source.nodes  <- LETTERS [1:26]
    target.nodes  <- c (LETTERS [2:26], LETTERS [1])
    weights <- runif (length (letters))
    dfn <- data.frame(id=source.nodes, stringsAsFactors = F)
    dfe <- data.frame (source=source.nodes, target=target.nodes, weight=weights, stringsAsFactors = F)
    createNetworkFromDataFrames(dfn, dfe)
    
    # paint the edges shades of green as function of weight
    setEdgeLineWidthDefault (5)
    setEdgeColorMapping ('weight',  c (0, 1), c ('#FFFFFF', '#00FF00'),  'c')
    
    # select M, then its immediate neighbors
    checkEquals (getSelectedNodeCount(), 0)
    checkEquals (sort (getFirstNeighbors ('M')), c ('L', 'N'))
    selectNodes ('M', 'name')
    checkEquals (getSelectedNodeCount (), 1)
    selectFirstNeighbors ()
    checkEquals (getSelectedNodeCount (), 3)
    checkEquals (sort (getSelectedNodes ()), c ('L', 'M', 'N'))
    selectFirstNeighbors ()
    checkEquals (getSelectedNodeCount (), 5)
    nodes = sort (getSelectedNodes ())
    checkEquals (nodes, c ("K", "L", "M", "N", "O"))
}
#-------------------------------------------------------------------------------
test.invertSelection = function ()
{
    title = 'test.invertSelection'
    test.prep (title)
    
    clearSelection ()
    checkEquals (getSelectedNodeCount (), 0)
    selectNodes ('node 0','name')
    checkEquals (getSelectedNodeCount (), 1)
    
    invertNodeSelection ()
    checkEquals (getSelectedNodeCount (), 3)
    invertNodeSelection ()
    checkEquals (getSelectedNodeCount (), 1)
    
    clearSelection ()
}
#-------------------------------------------------------------------------------
test.deleteSelectedNodes = function ()
{
    title = 'test.deleteSelectedNodes'
    test.prep (title,F)

    selectNodes (c ('node 0', 'node 1'), 'name')
    checkEquals (getSelectedNodeCount (), 2)
    
    deleteSelectedNodes()
    checkEquals(getNodeCount(), 2)
}
#-------------------------------------------------------------------------------
test.hideNodes = function ()
{
    title = 'test.hideNodes'
    test.prep (title)

    selectNodes (c ('node 0', 'node 1'), 'name')
    hideSelectedNodes ()
    checkEquals (getNodeCount (), 4)
    unhideAll()
}
#-------------------------------------------------------------------------------
test.selectEdges = function ()
{
    title = 'test.selectEdges'
    test.prep (title, F)

    selectEdges ("node 0 (inhibits) node 1", "name")
    checkEquals (getSelectedEdgeCount (), 1)
    Sys.sleep (0.3)
    clearSelection ()
    checkEquals (getSelectedEdgeCount (), 0)
}
#-------------------------------------------------------------------------------
test.setEdgeLineStyleMapping = function ()
{
    title = 'test.setEdgeLineStyleMapping'
    test.prep (title,F)

    line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
    edgeType.values = c ('inhibits', 'activates', 'interacts')
    checkEquals (length (intersect (line.styles, getLineStyles ())), 3)
    
    setEdgeLineStyleMapping ('interaction', edgeType.values, line.styles)
    
    # test one-element lists
    line.styles = c ('DOT')
    edgeType.values = c ('activates')
    checkEquals (length (intersect (line.styles, getLineStyles ())), 1)
    setEdgeLineStyleMapping ('interaction', edgeType.values, line.styles)
}
#-------------------------------------------------------------------------------
test.setEdgeLineWidthMapping = function ()
{
    title = 'test.setEdgeLineWidthMapping'
    test.prep (title, F)

    edgeType.values = c ('inhibits', 'activates', 'interacts')
    setEdgeLineWidthMapping ('interaction', edgeType.values, c (0, 8, 16), 'd')
    
    # try one-element lists
    setEdgeLineWidthMapping ('interaction', edgeType.values [1], 10, 'd')
}
#-------------------------------------------------------------------------------
test.setEdgeColorMapping = function ()
{
    title = 'test.setEdgeColorMapping'
    test.prep (title, F)

    edgeType.values = c ('inhibits', 'activates', 'interacts')
    colors = c ('#FF0000', '#FFFF00', '#00FF00')
    setEdgeColorMapping ('interaction',  edgeType.values, colors, 'd')
    Sys.sleep (0.3)
    
    all.white  = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
    setEdgeColorMapping ('interaction',  edgeType.values [2],'#000000', 'd')
    
    # now create a continuous ('interpolate') mode Mapping, using the score edge attribute
    score.values = c (3, 6.5, 10);
    colors = c ('#00FF00', '#FFFFFF', '#FF0000')
    setEdgeColorMapping ('weight',  score.values, colors, 'c')
    
    # now swap the colors
    colors = c ('#FF0000', '#000000', '#00FF00')
    setEdgeColorMapping ('weight',  score.values, colors, 'c')
}
#-------------------------------------------------------------------------------
test.setEdgeOpacityMapping = function ()
{
    title = 'test.setEdgeOpacityMapping'
    test.prep (title, F)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    layoutNetwork ('grid')
    
    edgeType.values = c ("phosphorylates", "synthetic lethal", "undefined")
    
    # want to see edges and both arrows, to check success of opacity Mapping
    setEdgeTargetArrowMapping ('edgeType', edgeType.values, rep ('ARROW', 3))
    setEdgeSourceArrowMapping ('edgeType', edgeType.values, rep ('ARROW', 3))
    setEdgeLineWidthDefault (5)
    
    
    
    # do the lookup Mapping
    opacities = c (25, 100, 255)
    setEdgeOpacityMapping ('edgeType',  edgeType.values, opacities, 'd')
    
    
    # now do the interpolated version
    opacities = c (10, 125, 255)
    control.points = c (-12, 0, 35)
    setEdgeOpacityMapping ('score',  control.points, opacities, 'c')
    
    
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
    # by changing node attribute values, for the integer attribute 'count' and the numeric attribute 'score'
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeMapping ('count', count.control.points, sizes, 'c')
    setNodeColorMapping ('score', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), 'c')
    
    count = 3
    
    # three renderings of the 3-node, 3-edge network are created in this loop, which runs 'count' times
    # the first two set new attributes on the R graph data structure, then ask RCy3 to send those values
    # to R from the graph
    # the third rendering bypasses storage of new attribute values on the R graph, sending them instead
    # directly to Cytoscape.  (hence 'setNodeAttributesBypass')
    
    for (i in 1:count) {
        nodeData (net.suide@graph, 'A', 'score') = -3.0
        nodeData (net.suide@graph, 'B', 'score') = -0.7
        nodeData (net.suide@graph, 'C', 'score') = -1.9
        nodeData (net.suide@graph, 'A', 'count') = 10
        nodeData (net.suide@graph, 'B', 'count') = 140
        nodeData (net.suide@graph, 'C', 'count') = 32
        result = setNodeAttributes ('score')
        result = setNodeAttributes ('count')
        redraw (net.suide)
        
        Sys.sleep (0.3)
        nodeData (net.suide@graph, 'A', 'score') = 3.0
        nodeData (net.suide@graph, 'B', 'score') = 0.7
        nodeData (net.suide@graph, 'C', 'score') = 1.9
        nodeData (net.suide@graph, 'A', 'count') = 50
        nodeData (net.suide@graph, 'B', 'count') = 22
        nodeData (net.suide@graph, 'C', 'count') = 180
        result = setNodeAttributes ('score')
        result = setNodeAttributes ('count')
        redraw (net.suide)
        Sys.sleep (0.3)
        
        count.A = round (runif (1, 1, 200))
        count.B = round (runif (1, 1, 200))
        count.C = round (runif (1, 1, 200))
        
        result = setNodeAttributesBypass ('count', 'int', c ('A', 'B', 'C'), c (count.A, count.B, count.C));
        result = setNodeAttributesBypass ('score', 'numeric', c ('A', 'B', 'C'), c (-1.0, 0.0, 1.0))
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
#     setNodeSizeMapping ('count', count.control.points, sizes, 'c')
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
        setNodeBorderColorMapping (net.suids, 'type', node.attribute.values, colors, 'd', default.color='#88FF22')
        count.control.points = c (2, 30, 100)
        sizes                = c (20, 50, 100)
        setNodeSizeMapping (net.suids, 'count', count.control.points, sizes, 'c')
        setNodeColorMapping (net.suids, 'score', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), 'c')
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
    setNodeBorderColorMapping (net.suids, 'type', node.attribute.values, colors, 'd', default.color='#88FF22')
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeMapping (net.suids, 'count', count.control.points, sizes, 'c')
    setNodeColorMapping (net.suids, 'score', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), 'c')
    arrows = c ('Arrow', 'Arrow', 'Arrow', 'None')
    edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'synthetic rescue', 'undefined')
    setEdgeTargetArrowMapping (net.suids, 'edgeType', edgeType.values, arrows)
    
    edgeType.values = c ('phosphorylates', 'synthetic lethal', 'synthetic rescue', 'undefined')
    edgeColors = c ('#0000AA', '#000000', '#00AA00', '#FFFFFF')
    setEdgeColorMapping (net.suids, 'edgeType',  edgeType.values, edgeColors, 'd')
    
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
    layoutNetwork ('grid')
    
    # establish a good starting point
    setNodeSizeBypass (getAllNodes(), rep (100, 3))
    
    
    sizes =  getNodeSize (getAllNodes())
    # these next test pass fine in uncomplicated circumstances, but (apparently) fail due to
    # vizmap complexities when lots of windows are or have been open
    #checkEquals (sizes$width, c (100, 100, 100))
    #checkEquals (sizes$height, c (100, 100, 100))
    
    setNodeSizeBypass (c ('A', 'B'), 150); 
    sizes =  getNodeSize (getAllNodes())
    
    # these next test pass fine in uncomplicated circumstances, but (apparently) fail due to
    # vizmap complexities when lots of windows are or have been open
    #checkEquals (sizes$width, c (150, 150, 100))
    #checkEquals (sizes$height, c (150, 150, 100))
    
    setNodeSizeBypass (c ('A', 'B'), c (180, 32));   
    
    sizes = getNodeSize (getAllNodes())
    #checkEquals (sizes$width, c (180, 32, 100))
    #checkEquals (sizes$height, c (180, 32, 100))
    
    # now allow for non-symmetric dimensions, in which width and height are set separately
    lockNodeDimensions (FALSE)
    setNodeHeightBypass (c ('A', 'B', 'C'), c (12, 22, 32))
    setNodeWidthBypass (c ('A', 'B', 'C'), c (120, 122, 132))
    
    
    sizes = getNodeSize ('B')
    #checkEquals (sizes$width, 122)
    #checkEquals (sizes$height, 22)
    
    # return to symmetric dimensions
    lockNodeDimensions (TRUE)
    
    
    # not sure how width and height are rectified.  it appears that the last-used width=height values are returned
    sizes = getNodeSize (getAllNodes())
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
    
    nodes.with.attribute = RCy3:::haveNodeAttribute (net.suid3, nodes (getGraph (net.suid3)), 'score')
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
    
    layoutNetwork (net.suid)
    
    cy = CytoscapeConnection ()
    
    cy2.edgenames = as.character (cy2.edge.names (getGraph (net.suid)))
    
    checkTrue (length (RCy3:::haveEdgeAttribute (cy2.edgenames, 'score')) == 1)
    
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
    checkEquals (length (intersect (noa.names (g2), c ("name", "count", "label", "score", "type"))), 5)
    checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='name')), c ('A', 'B', 'C'))
    checkEquals (as.integer (nodeData (g2, c ('A', 'B', 'C'), attr='count')), c (2, 30, 100))
    checkEquals (as.numeric (nodeData (g2, c ('A', 'B', 'C'), attr='score')), c (-3,  0,  3))
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
    checkEquals (length (intersect (noa.names (g3), c ("name", "count", "label", "score", "type"))), 5)
    checkEquals (as.character (sort (noa (g3, 'name'))), c ('A', 'B', 'C'))
    checkEquals (as.integer   (sort (noa (g3, 'count'))),         c (2, 30, 100))
    checkEquals (as.character (sort (noa (g3, 'label'))),         c ('Gene A', 'Gene B', 'Gene C'))
    checkEquals (as.numeric (sort (noa (g3, 'score'))),             c (-3,  0,  3))
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
    
    layoutNetwork (net.suid)
    selectNodes (c ('A', 'C'), 'name')
    
    new.window.title = 'NEW'
    if (new.window.title %in% as.character (getNetworkList ())){
        deleteNetwork (new.window.title)
    }
    
    c2 = createNetworkFromSelection (new.window.title, TRUE)
    redraw (c2)
    layoutNetwork (c2)
    
    clearSelection ()
    selectNodes ('C', 'name')
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
    setEdgeAttributesBypass ('treeSpecies', 'char', first.two.edges, values)
    
    # now add an attribute to a single edge.  this exercises a different branch in RCytoscape:setEdgeAttributesBypass
    first.edge = as.character (cy2.edge.names (g)[1])
    value = 'one century'
    setEdgeAttributesBypass ('ageInYears', 'char', first.edge, value)
    checkTrue ('ageInYears' %in% getEdgeAttributeNames (net.suid))
    
    # get names from cy2.edge.names (net.suid@graph)
    checkEquals (getEdgeAttribute ("B (synthetic lethal) C", 'treeSpecies'), "yew")
    checkEquals (getEdgeAttribute ("B (synthetic lethal) C", 'score'), -12)
    
    deleteEdgeAttribute ('species')
    deleteEdgeAttribute ('ageInYears')
    
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
    layoutNetwork ('grid')
    
    
    novel.noa.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getNodeAttributeNames(net.suid))
    for (noa.name in novel.noa.to.delete){
        deleteNodeAttribute (noa.name)
    }
    
    # name is added by Cytoscape
    checkEquals (length (intersect (getNodeAttributeNames (net.suid), c ("name", "count",  "label", "score", "type"))), 5)
    
    # now add an attribute to two of the nodes
    first.two.nodes = nodes (g) [1:2]
    values = c ('cedar', 'ash')
    setNodeAttributesBypass ('treeSpecies', 'char', first.two.nodes, values)
    
    # now add an attribute to a single node.  this exercises a different branch in RCytoscape:setNodeAttributesBypass
    first.node = nodes (g) [1]
    value = 'one millenium'
    setNodeAttributesBypass ('ageInYears', 'char', first.node, value)
    checkTrue ('ageInYears' %in% getNodeAttributeNames (net.suid))
    checkEquals (getNodeAttribute ('B', 'type'), 'transcription factor')
    checkEquals (getNodeAttribute ('A', 'ageInYears'), 'one millenium')
    checkEquals (getNodeAttribute ('B', 'ageInYears'), '')
    
    deleteNodeAttribute ('species')
    deleteNodeAttribute ('ageInYears')
    
    invisible (net.suid)
    
} #  test.addGetAndDeleteNodeAttributes
#-------------------------------------------------------------------------------
test.getAllNodeAttributes = function ()
{
    title = 'test.getAllNodeAttributes'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork (net.suid)
    
    net.suidc = existing.CytoscapeNetwork (title, copy=T)
    tbl.noa <- getAllNodeAttributes (net.suidc)
    checkEquals (nrow (tbl.noa), 3)
    checkTrue (ncol (tbl.noa) >= 5)
    expected.colnames =  c ("name", "count", "label", "score", "type")  # created here
    checkEquals (length (intersect (colnames (tbl.noa), expected.colnames)), 5)
    checkEquals (sort (rownames (tbl.noa)), c ("A", "B", "C"))
    
    # now try a graph with only one node attribute.  this case used to fail (pshannon, 16 feb 2011)
    
    g2 = new ('graphNEL', edgemode='directed')
    g2 = initNodeAttribute (g2, 'label', 'char', 'NA')
    g2 = graph::addNode ('A', g2)
    nodeData (g2, 'A', 'label') = 'a label for A'
    window.title = 'single node attribute test'
    if (window.title %in% as.character (getNetworkList (net.suid)))
        deleteNetwork (window.title)
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
    
    layoutNetwork (net.suid)
    
    clearSelection (net.suid)
    selectNodes ('A', 'name')
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
#     
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
    
    layoutNetwork (net.suid)
    
    fitContent (net.suid)
    
    smaller = 0.5
    larger = 2
    
    for (i in 1:10){
        setZoom (smaller * getZoom (net.suid))
    }
    
    for (i in 1:10){
        setZoom (larger * getZoom (net.suid))
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
    
    layoutNetwork (net.suid)
    
    fitContent (net.suid)
    setZoom (0.5 * getZoom (net.suid))
    
    center.orig = getCenter (net.suid)
    delta = 100
    x.left = center.orig$x - delta
    x.right = center.orig$x + delta
    y.up = center.orig$y - delta
    y.down = center.orig$y + delta
    
    for (i in 1:10) {
        setCenter (x.left, y.up)
        setCenter (as.integer (x.left), as.integer (y.up))   # make sure the called function casts this int back to numeric
        setCenter (x.left, y.down)
        setCenter (x.right, y.down)
        setCenter (x.right, y.up)
    } # for i
    
    setCenter (center.orig$x, center.orig$y)
    
    invisible (net.suid)
    
} # test.center
#-------------------------------------------------------------------------------
test.setNodeSizeBypass = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeSizeBypass'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork (net.suid)
    
    lockNodeDimensions (TRUE)
    
    small = 30
    large = 300
    setNodeSizeBypass ('A', small);
    setNodeSizeBypass ('A', large);
    
    invisible (net.suid)
    
} # test.setNodeSizeBypass
#-------------------------------------------------------------------------------
test.setNodeWidthAndHeightBypass = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeWidthAndHeightBypass'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork (net.suid)
    
    lockNodeDimensions (FALSE)
    
    small = 30
    large = 300
    
    setNodeWidthBypass ('A', small);
    setNodeHeightBypass ('A', large);
    setNodeWidthBypass ('A', large);
    setNodeHeightBypass ('A', small);
    
    invisible (net.suid)
    
} # test.setNodeWidthAndHeightBypass
#-------------------------------------------------------------------------------
test.setNodeFontSizeBypass = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeFontSizeBypass'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork (net.suid)
    
    starting.size = 4
    setNodeSizeBypass (c ('A', 'B', 'C'), 50)
    setNodeFontSizeBypass (c ('A', 'B'), 12)
    
    
    for (i in 1:20) {
        setNodeFontSizeBypass ('A', starting.size + i)
        setNodeFontSizeBypass ('B', starting.size + (i*3))
    } # for i
    
    starting.size = 32
    for (i in 20:1) {
        setNodeFontSizeBypass ('A', starting.size - i)
        setNodeFontSizeBypass ('B', starting.size - (i*3))
    } # for i
    
    invisible (net.suid)
    
} # test.setNodeSizeBypass
#-------------------------------------------------------------------------------
test.setNodeShapeBypass = function ()
{
    #DEACTIVATED("too slow")
    title = 'test.setNodeShapeBypass'
    test.prep (title)
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork (net.suid)
    
    lockNodeDimensions (TRUE)
    setNodeSizeBypass ('A', 100)
    
    for (new.shape in getNodeShapes (net.suid)) {
        setNodeShapeBypass ('A', new.shape)
    } # for new.shape
    
    invisible (net.suid)
    
} # test.setNodeShapeBypass
#-------------------------------------------------------------------------------
# add a node to an existing graph.
# questions:
#  1) what edge attribute values are assigned to this new edge?
#  2) can we assign new values to those attributes?  use setEdgeAttributesBypass
test.addCyNode = function ()
{
    title = 'test.addCyNode'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork ('grid')
    checkEquals (getNodeCount (net.suid), 3)
    addCyNode ('NEW')
    layoutNetwork ('grid')
    checkEquals (getNodeCount (net.suid), 4)
    invisible (net.suid)
    
} # test.addCyNode
#-------------------------------------------------------------------------------
# add an edge to an existing graph.
# questions:
#  1) what edge attribute values are assigned to this new edge?
#  2) can we assign new values to those attributes?  use setEdgeAttributesBypass
test.addCyEdge = function ()
{
    title = 'test.addCyEdge'
    test.prep (title)
    
    net.suid = createNetworkFromIgraph(makeSimpleIgraph(), title=title)
    displayGraph (net.suid)
    
    layoutNetwork (net.suid)
    checkEquals (getEdgeCount (net.suid), 3)
    
    addCyNode ('NEW')
    directed.edge = TRUE
    addCyEdge ('A', 'NEW', 'synthetic rescue', directed.edge)
    
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
    
    layoutNetwork (net.suid)
    
    g2 = new ('graphNEL', edgemode='directed')
    g2 = initEdgeAttribute (g2, 'edgeType', 'char', 'unspecified')
    
    g2 = graph::addNode ('A', g2)
    g2 = graph::addNode ('B', g2)
    g2 = addEdge ('A', 'B', g2)
    
    edgeData (g2, 'A', 'B', 'edgeType') = 'synthetic rescue'
    
    addGraphToGraph (g2)
    
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
    layoutNetwork ('grid')
    setNodeLabelMapping ('label')
    
    
    #--- png first
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest')
    printf ('saving image file to %s', filename)
    saveImage (filename, 'png', 1.0)
    checkTrue (file.exists (paste0(filename, '.png')))
    
    #--- now pdf
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest')
    printf ('saving image file to %s', filename)
    saveImage (filename, 'pdf')
    checkTrue (file.exists (paste0(filename, '.pdf')))
    
    #--- now svg
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest')
    printf ('saving image file to %s', filename)
    saveImage (filename, 'svg')
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
    layoutNetwork ('grid')
    setNodeLabelMapping ('label')
    
    
    filename = sprintf ('%s/%s', tempdir (), 'saveNetworkTest')
    printf ('saving cys file to %s', filename)
    saveNetwork (filename)
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
    g = initNodeAttribute(g, "score", "numeric", 1)
    g = initNodeAttribute(g, "label", "char", "default node label")
    g = initNodeAttribute(g, "count", "integer", 0)
    
    nodeDataDefaults (g, attr='type') = ''
    nodeDataDefaults (g, attr='score') = 0.0
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
    nodeData(g, "A", "score") = -3
    nodeData(g, "B", "score") = 0
    nodeData(g, "C", "score") = 3
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
    g = initNodeAttribute(g, "score", "numeric", 1)
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
    nodeData(g, "A", "score") = -3
    nodeData(g, "B", "score") = 0
    nodeData(g, "C", "score") = 3
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