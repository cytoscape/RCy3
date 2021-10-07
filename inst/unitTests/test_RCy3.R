# RCytoscape/inst/test_cytoscape.R
# Do not run this in cloud via jupyter-bridge. It will fail.
#-------------------------------------------------------------------------------
library (RCy3)
library (RUnit)
library (graph)
if(!"igraph" %in% installed.packages()){
    install.packages("igraph")
}
library (igraph)

#-------------------------------------------------------------------------------
run.tests = function()
{
    options('warn'=0) # deprecation warnings (and others) are stored and reported
    
    # before doing anything else, make sure that the Cytoscape plugin version is one we can respond to
    test.app.version()
    
    # minimize all delays
    setCatchupFilterSecs(0)
    setModelPropagationSecs(0)
    setCatchupNetworkSecs(0)
    
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
    test.setNodeBorderWidthDefault ()
    test.setNodeBorderColorDefault ()
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
    test.setNodeBorderWidthMapping ()
    test.setNodeBorderColorMapping ()
    test.setNodeSizeMapping ()
    test.setNodeShapeMapping ()
    test.setNodeComboOpacityMapping ()
    test.setNodeColorBypass ()
    test.setNodeBorderColorBypass ()
    test.setNodeLabelBypass () 
    test.setNodeOpacityBypass ()  
    
    deleteAllNetworks ()
    
    test.setEdgeOpacityBypass ()  
    test.setEdgeColorBypass () 
    test.setEdgeSourceArrowShapeBypass ()
    test.setEdgeTargetArrowShapeBypass ()
    test.setEdgeSourceArrowColorBypass ()
    test.setEdgeTargetArrowColorBypass ()
    test.setEdgeLabelBypass ()
    test.setEdgeFontSizeBypass ()  
    test.setEdgeLabelColorBypass ()  
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
    test.setEdgeOpacityMapping()
    test.setEdgeTargetArrowMapping ()
    test.setEdgeArrowColorMappings ()
    test.setEdgeSourceArrowMapping ()
     
    deleteAllNetworks ()
     
    test.movie ()
    test.unmatchedAttributesError ()
    test.simpleGraphWithReciprocalEdge ()
    test.setNodePosition () 
    test.getNodePosition ()
    test.getNodePosition.colonInNodeName ()
    test.visualStyle ()
    
    test.defaultColors ()
    test.fitContent ()
    test.zoom () 
    test.center ()
    test.setNodeSizeBypass ()  
    test.getNodeSize ()
    test.setNodeWidthAndHeightBypass () 
    test.setNodeFontSizeBypass ()  
    test.setNodeShapeBypass () 
    test.createIgraphFromNetwork ()
    test.createGraphFromNetwork ()
    test.createNetworkFromSelection ()
    test.createNetworkFromDataFrames ()
    test.multigraph ()
    test.createNetworkFromIgraph()
    test.createNetworkFromGraph()
    
    test.customGraphics()
    test.filters()
    
    test.annotations()
    
    closeSession(FALSE)
    options('warn'=0)
    
    # reset all delays
    setCatchupFilterSecs()
    setModelPropagationSecs()
    setCatchupNetworkSecs()

    
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
    test.prep(title,FALSE)
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
    test.prep (title,FALSE)
    shapes = getNodeShapes ()
    checkTrue (length (shapes) > 8)
    # pick a few specific shapes to test
    checkTrue (all (sapply (c ('HEXAGON', 'ELLIPSE', 'TRIANGLE'), function (s) s %in% shapes)))
    
} 
#-------------------------------------------------------------------------------
test.getTableColumnTypes = function ()
{
    title = 'test.getTableColumnTypes'
    test.prep(title,FALSE)
    possible.values = getTableColumnTypes ()
    checkTrue (grep ('Integer', possible.values) > 0)
    checkTrue (grep ('String', possible.values) > 0)
    checkTrue (grep ('Boolean', possible.values) > 0)
} 
#-------------------------------------------------------------------------------
test.getArrowShapes = function ()
{
    title = 'test.getArrowShapes'
    test.prep(title,FALSE)
    shapes = getArrowShapes ()
    checkTrue (length (shapes) >= 8)
    # pick a few specific shapes to test
    checkTrue (all (sapply (c ('DIAMOND', 'T', 'CIRCLE'), function (s) s %in% shapes)))
    
} 
#-------------------------------------------------------------------------------
test.getLineStyles = function ()
{
    title = 'test.getLineStyles'
    test.prep(title,FALSE)
    styles = getLineStyles ()
    checkTrue (length (styles) > 10)
    # pick a few specific styles to test
    checkTrue (all (sapply (c ('SOLID', 'DOT', 'EQUAL_DASH'), function (s) s %in% styles)))
    
} 
#-------------------------------------------------------------------------------
test.getLayoutNames = function ()
{
    title = 'test.getLayoutNames'
    test.prep(title,FALSE)
    names = getLayoutNames ()
    checkTrue (length (names) > 10)
    # pick a few specific styles to test
    checkTrue (all (sapply (c ('grid', 'isom', 'circular'), function (s) s %in% names)))
    
} 
#-------------------------------------------------------------------------------
test.getLayoutNameMapping = function ()
{
    title = 'test.getLayoutNameMapping'
    test.prep(title,FALSE)
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
    test.prep(title,FALSE)
    props = getLayoutPropertyNames ('force-directed')
    expected = c ("numIterations", "defaultSpringCoefficient", "defaultSpringLength",
                  "defaultNodeMass", "isDeterministic", "singlePartition")
    checkTrue (length (intersect (props, expected)) >= length (expected))  # some variation across Cytoscape versions
    
    props = getLayoutPropertyNames ('isom')
    expected = c ("coolingFactor", "initialAdaptation", "maxEpoch", "minAdaptation",
                  "minRadius", "radius", "radiusConstantTime", "singlePartition", "sizeFactor")
    checkTrue (length (intersect (props, expected)) >= length (expected))  # some variation across Cytoscape versions
    
    
    
} 
#-------------------------------------------------------------------------------
test.getLayoutPropertyType = function ()
{
    title = 'test.getLayoutPropertyType'
    test.prep(title,FALSE)
    
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
    test.prep(title,FALSE)
    
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
    test.prep(title,FALSE)
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
    test.prep (title,FALSE)
    
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
    test.prep (title,FALSE)
    
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
    test.prep (title,FALSE)

    setNodeColorDefault ('#AA00AA')
} 
#-------------------------------------------------------------------------------
test.setNodeSizeDefault = function ()
{
    title = 'test.setNodeSizeDefault'
    test.prep (title,FALSE)
    
    setNodeSizeDefault (200)   
    setNodeSizeDefault (20)
} 
#-------------------------------------------------------------------------------
test.setNodeBorderColorDefault = function ()
{
    title = 'test.setNodeBorderColorDefault'
    test.prep (title,FALSE)
    
    setNodeBorderColorDefault ('#FFFFFF')
    setNodeBorderColorDefault ('#FF0000')
}
#-------------------------------------------------------------------------------
test.setNodeBorderWidthDefault = function ()
{
    title = 'test.setNodeBorderWidthDefault'
    test.prep (title,FALSE)
    
    setNodeBorderWidthDefault (5)
    setNodeBorderWidthDefault (1)
}
#-------------------------------------------------------------------------------
test.setNodeFontSizeDefault = function ()
{
    title = 'test.setNodeFontSizeDefault'
    test.prep (title,FALSE)
    
    setNodeFontSizeDefault (12)
}
#-------------------------------------------------------------------------------
test.setNodeLabelColorDefault = function ()
{
    title = 'test.setNodeLabelColorDefault'
    test.prep (title,FALSE)
    
    setNodeLabelColorDefault ('#FFAAAA')
}
#-------------------------------------------------------------------------------
test.setEdgeLineWidthDefault = function ()
{
    title = 'test.setEdgeLineWidthDefault'
    test.prep (title,FALSE)
    
    setEdgeLineWidthDefault (10)
}
#-------------------------------------------------------------------------------
test.setEdgeColorDefault = function ()
{
    title = 'test.setEdgeColorDefault'
    test.prep (title,FALSE)
    
    setEdgeColorDefault ('#5588FF')
}
#-------------------------------------------------------------------------------
test.setEdgeFontSizeDefault = function ()
{
    title = 'test.setEdgeFontSizeDefault'
    test.prep (title,FALSE)
    
    setEdgeFontSizeDefault (12);
}
#-------------------------------------------------------------------------------
test.setNodeLabelMapping = function ()
{
    title = 'test.setNodeLabelMapping'
    test.prep (title,FALSE)
    
    setNodeLabelMapping ('id')
    setNodeLabelMapping ('score')
    setNodeLabelMapping ('group')
    setNodeLabelMapping ('name')
}
#-------------------------------------------------------------------------------
test.setEdgeLabelMapping = function ()
{
    title = 'test.setEdgeLabelMapping'
    test.prep (title,FALSE)
    
    setEdgeLabelMapping ('weight')
    setEdgeLabelMapping ('name')
} 
#-------------------------------------------------------------------------------
test.setNodeTooltipMapping = function ()
{
    title = 'test.setNodeTooltipMapping'
    test.prep (title,FALSE)
    
    setNodeTooltipDefault('unknown')
    setNodeTooltipMapping ('group')
    setNodeTooltipBypass(c('node 1','node 2'), c('bypass1','bypass2'))
    setNodeTooltipBypass(c('node 1','node 2'), 'bypass3')
    setNodeTooltipBypass('node 1', 'bypass4')
} 
#-------------------------------------------------------------------------------
test.setEdgeTooltipMapping = function ()
{
    title = 'test.setEdgeTooltipMapping'
    test.prep (title,FALSE)

    edges.of.interest = getAllEdges()
    
    setEdgeTooltipDefault('unknown')
    setEdgeTooltipMapping ('weight')
    # first try passing three edges and three tooltips
    setEdgeTooltipBypass (edges.of.interest[1:3], c ('tooltip #1', 'tooltip #2', 'tooltip #3'))
    # now try passing three edges and one tooltip
    setEdgeTooltipBypass (edges.of.interest [1:3], 'a general purpose tooltip')
} 
#-------------------------------------------------------------------------------
test.setNodeColorMapping = function ()
{
    title = 'test.setNodeColorMapping'
    test.prep (title,FALSE)
    
    # first, continuous
    node.attribute.values = c (0.0, 10.0, 20.0)
    node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
    setNodeColorMapping ('score', node.attribute.values, node.colors, 'c')
    setNodeColorMapping('score',colors = paletteColorBrewerBlues)
    setNodeColorMapping('score',colors = paletteColorBrewerBrBG)
    setNodeColorMapping('score',colors = paletteColorBrewerBuGn)
    setNodeColorMapping('score',colors = paletteColorBrewerBuPu)
    setNodeColorMapping('score',colors = paletteColorBrewerGnBu)
    setNodeColorMapping('score',colors = paletteColorBrewerGreens)
    setNodeColorMapping('score',colors = paletteColorBrewerGreys)
    setNodeColorMapping('score',colors = paletteColorBrewerOranges)
    setNodeColorMapping('score',colors = paletteColorBrewerOrRd)
    setNodeColorMapping('score',colors = paletteColorBrewerPiYG)
    setNodeColorMapping('score',colors = paletteColorBrewerPRGn)
    setNodeColorMapping('score',colors = paletteColorBrewerPuBu)
    setNodeColorMapping('score',colors = paletteColorBrewerPuBuGn)
    setNodeColorMapping('score',colors = paletteColorBrewerPuOr)
    setNodeColorMapping('score',colors = paletteColorBrewerPuRd)
    setNodeColorMapping('score',colors = paletteColorBrewerPurples)
    setNodeColorMapping('score',colors = paletteColorBrewerRdBu)
    setNodeColorMapping('score',colors = paletteColorBrewerRdPu)
    setNodeColorMapping('score',colors = paletteColorBrewerRdYlBu)
    setNodeColorMapping('score',colors = paletteColorBrewerReds)
    setNodeColorMapping('score',colors = paletteColorBrewerYlGn)
    setNodeColorMapping('score',colors = paletteColorBrewerYlGnBu)
    setNodeColorMapping('score',colors = paletteColorBrewerYlOrBr)
    setNodeColorMapping('score',colors = paletteColorBrewerYlOrRd)
    
    # now, discrete
    node.attribute.values = c ("A",  "B")
    node.colors =           c ('#8888FF', "#00CCCC")
    setNodeColorMapping ('group', node.attribute.values, node.colors, 'd')
    setNodeColorMapping('score',colors = paletteColorBrewerSet1)
    setNodeColorMapping('score',colors = paletteColorBrewerSet2)
    setNodeColorMapping('score',colors = paletteColorBrewerSet3)
    setNodeColorMapping('score',colors = paletteColorBrewerDark2)
    setNodeColorMapping('score',colors = paletteColorRandom)
    setNodeColorMapping('score',colors = paletteColorBrewerAccent)
    setNodeColorMapping('score',colors = paletteColorBrewerPaired)
    setNodeColorMapping('score',colors = paletteColorBrewerPastel1)
    setNodeColorMapping('score',colors = paletteColorBrewerPastel2)
    
    # now, test default
    node.attribute.values = c ("A")
    node.colors =           c ('#8888FF')
    setNodeColorMapping ('group', node.attribute.values, node.colors, 'd', default.color = '#5588FF')
} 
#-------------------------------------------------------------------------------
test.setNodeBorderWidthMapping = function ()
{
    title  = 'test.setNodeBorderWidthMapping'
    test.prep (title,FALSE)
    
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
test.setNodeBorderColorMapping = function ()
{
    title = 'test.setNodeBorderColorMapping'
    test.prep (title,FALSE)
    
    # first, continuous
    node.attribute.values = c (0.0, 10.0, 20.0)
    node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
    setNodeBorderColorMapping ('score', node.attribute.values, node.colors, 'c')
    setNodeBorderColorMapping('score', colors = paletteColorBrewerReds)
    
    # now, discrete
    node.attribute.values = c ("A",  "B")
    node.colors =           c ('#8888FF', "#00CCCC")
    setNodeBorderColorMapping ('group', node.attribute.values, node.colors, 'd')
    setNodeBorderColorMapping('score', colors = paletteColorBrewerSet3)
    
    # now, test default
    node.attribute.values = c ("A")
    node.colors =           c ('#8888FF')
    setNodeBorderColorMapping ('group', node.attribute.values, node.colors, 'd', default.color = '#5588FF')
} 
#-------------------------------------------------------------------------------
test.setNodeSizeMapping = function ()
{
    title = 'test.setNodeSizeMapping'
    test.prep (title,FALSE)

    count.control.points = c(5,  20)
    node.sizes           = c(1, 80,  120, 300)
    setNodeHeightMapping ('score', count.control.points, node.sizes, 'c')
    setNodeWidthMapping ('score', count.control.points, node.sizes, 'c')
    setNodeSizeMapping ('score', count.control.points, node.sizes, 'c')
    setNodeSizeMapping ('score')
    setNodeSizeMapping ('score', sizes = c(30,60))
    
    # now use a 'd' Mapping. 
    molecule.types = c('A')
    node.sizes     = c(60)
    setNodeSizeMapping ('group', molecule.types,  node.sizes, default.size= 5, 'd')
    setNodeSizeMapping ('score', mapping.type = 'd')
} 
#-------------------------------------------------------------------------------
test.setNodeShapeMapping = function ()
{
    title = 'test.setNodeShapeMapping'
    test.prep (title,FALSE)

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
    test.prep (title,FALSE)
    
    # make the node borders prominent
    setNodeBorderColorDefault ('#55FF88')
    setNodeBorderWidthDefault (10)
    
    score.values = c (5, 10, 20)
    
    # make the nodes big, give them strong colors
    setNodeSizeBypass (getAllNodes(), 100)
    setNodeColorMapping ('score', score.values, c ('#FF0000', '#00FF00', '#0000FF'), 'c')
    setNodeLabelColorMapping('score', c(0,5), c('#FFFFFF','#FF7755'))
    layoutNetwork ('grid')
    
    # first, the continuous 'interpolate' case, in which opacity is a function of score
    opacities = c (10, 128, 255)
    setNodeFillOpacityMapping ('score', score.values, opacities, 'c')
    setNodeLabelOpacityMapping ('score', score.values, opacities, 'c')
    setNodeBorderOpacityMapping ('score', score.values, opacities, 'c')
    
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
    test.prep (title,FALSE)
    
    setNodeColorBypass ('node 0', '#AA0088')
    setNodeColorBypass ( c ('node 1', 'node 2'), '#448844')
} 
#-------------------------------------------------------------------------------
test.setNodeBorderColorBypass = function ()
{
    title = 'test.setNodeBorderColorBypass'
    test.prep (title,FALSE)
    
    setNodeBorderColorBypass ('node 0', '#AA4488')
    setNodeBorderColorBypass (c ('node 1', 'node 2'), '#AA8888')
} 
#-------------------------------------------------------------------------------
test.setNodeLabelBypass = function ()
{
    title = 'test.setNodeLabelBypass'
    test.prep (title,FALSE)

    setNodeLabelBypass ('node 0', 'new A label')

    setNodeLabelBypass (getAllNodes(), '')
    setNodeLabelBypass (c ('node 0', 'node 2'), c ('AzA', 'ByB'))
} 
#-------------------------------------------------------------------------------
test.setNodeLabelPropertiesBypass = function ()
{
    print ('--- test.setNodeLabelsPropertiesBypass')
    title = 'test.setNodeLabelPropertiesBypass'
    test.prep (title,FALSE)
    
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
    test.prep (title,FALSE)

    setNodeSizeBypass ('node 3', 120)
    layoutNetwork ('grid')
    fitContent ()
    setNetworkZoomBypass (0.8 * getNetworkZoom())
    
    setNodeFillOpacityDefault(150)
    setNodeLabelOpacityDefault(150)
    setNodeBorderOpacityDefault(150)
    
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

    setEdgeOpacityDefault(150)
    setEdgeLabelOpacityDefault(150)
    
    setEdgeOpacityMapping ('weight', c(1,10),c(50,255))
    setEdgeLabelOpacityMapping ('weight', c(1,10),c(50,255))
    
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
    test.prep (title,FALSE)
    
    edge.of.interest = getAllEdges()[1]
    setEdgeColorBypass (edge.of.interest, '#FF0000')
    setEdgeColorBypass (edge.of.interest, '#00FF00')
    setEdgeColorBypass (edge.of.interest, '#0000FF')
}
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowShapeBypass = function ()
{
    title = 'test.setEdgeSourceArrowShapeBypass'
    test.prep (title,FALSE)

    edges.of.interest = getAllEdges()
    supported.arrow.shapes = getArrowShapes()
    
    setEdgeSourceArrowShapeDefault("ARROW")
    setEdgeSourceArrowShapeMapping('interaction',c('activates','inhibits'),
                                   c('ARROW_SHORT','T'))
    
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
    test.prep (title,FALSE)

    edges.of.interest = getAllEdges()
    supported.arrow.shapes = getArrowShapes ()
    
    setEdgeTargetArrowShapeDefault("ARROW")
    setEdgeTargetArrowShapeMapping('interaction',c('activates','inhibits'),
                                   c('ARROW_SHORT','T'))
    
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
    test.prep (title,FALSE)
    
    arrows = c ('Arrow', 'Diamond', 'Circle')
    interaction.values = c ('inhibits', 'activates', 'interaction')
    setEdgeSourceArrowMapping ('interaction', interaction.values, arrows)
    setEdgeTargetArrowMapping ('interaction', interaction.values, arrows)
    
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
    test.prep (title,FALSE)
    
    arrows = c ('Arrow', 'Diamond', 'Circle')
    interaction.values = c ('inhibits', 'activates', 'interaction')
    setEdgeSourceArrowMapping ('interaction', interaction.values, arrows)
    setEdgeTargetArrowMapping ('interaction', interaction.values, arrows)
    
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
    test.prep (title,FALSE)
    
    edge.names = getAllEdges()[1:2]
    setEdgeLabelBypass (edge.names, 'some name')
} 
#-------------------------------------------------------------------------------
test.setEdgeFontFaceBypass = function ()
{
 title = 'test.setEdgeFontFaceBypass'
 test.prep (title,FALSE)

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
    test.prep (title,FALSE)
    
    edge.of.interest = getAllEdges()[1]
    setEdgeFontSizeBypass (edge.of.interest, 12)
}
#-------------------------------------------------------------------------------
test.setEdgeLabelColorBypass = function ()
{
    title = 'test.setEdgeLabelColorBypass'
    test.prep (title,FALSE)

    edge.names = getAllEdges()
    setEdgeLabelBypass (edge.names, 'some label')
    setEdgeLabelColorDefault("#8333FD")
    setEdgeLabelColorBypass (edge.names [1:2], '#FF0000')
    setEdgeLabelColorBypass (edge.names, '#00FF00')
    setEdgeLabelColorBypass (edge.names [3], '#000000')
    setEdgeFontFaceDefault("SansSerif,plain,9")
    setEdgeFontFaceMapping("interaction", c("activates","inhibits"),
                           c("SansSerif,plain,12", "Dialog,plain,10"))
}

#-------------------------------------------------------------------------------
test.setEdgeLineWidthBypass = function ()
{
    title = 'test.setEdgeLineWidthBypass'
    test.prep (title,FALSE)
    
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
    test.prep (title,FALSE)
    
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
    test.prep (title,FALSE)

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
    test.prep (title,FALSE)
    g <- RCy3::makeSimpleGraph()
    checkEquals (getEdgeCount(), 4)
}
#-------------------------------------------------------------------------------
test.getAllNodes = function ()
{
    title = 'test.getAllNodes'
    test.prep (title,FALSE)
    
    nodes = getAllNodes()
    checkEquals (length (nodes), 4)
}
#-------------------------------------------------------------------------------
test.getAllEdges = function ()
{
    title = 'test.getAllEdges'
    test.prep (title,FALSE)
    
    edges = getAllEdges()
    checkTrue ("node 0 (inhibits) node 1" %in% edges)
    checkTrue ("node 2 (interacts) node 3" %in% edges)
}
#-------------------------------------------------------------------------------
test.selectNodes = function ()
{
    title = 'test.selectNodes'
    test.prep (title,FALSE)
    
    clearSelection ()
    checkEquals (getSelectedNodeCount(), 0)
    net.suide.nodes = selectNodes (c ('node 0', 'node 1'), 'name', preserve=TRUE)
    checkEquals (getSelectedNodeCount(), 2)
    
    net.suide.nodes = selectNodes ('node 2', 'name', preserve=TRUE)
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
    test.prep (title,FALSE)
    
    # create a circular graph
    LETTERS = toupper (letters)
    source.nodes  <- LETTERS [1:26]
    target.nodes  <- c (LETTERS [2:26], LETTERS [1])
    weights <- runif (length (letters))
    dfn <- data.frame(id=source.nodes, stringsAsFactors = FALSE)
    dfe <- data.frame (source=source.nodes, target=target.nodes, weight=weights, stringsAsFactors = FALSE)
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
    test.prep (title,FALSE)

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
    test.prep (title,FALSE)

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
    test.prep (title,FALSE)

    line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
    interaction.values = c ('inhibits', 'activates', 'interacts')
    checkEquals (length (intersect (line.styles, getLineStyles ())), 3)
    
    setEdgeLineStyleMapping ('interaction', interaction.values, line.styles)
    
    # test one-element lists
    line.styles = c ('DOT')
    interaction.values = c ('activates')
    checkEquals (length (intersect (line.styles, getLineStyles ())), 1)
    setEdgeLineStyleMapping ('interaction', interaction.values, line.styles)
}
#-------------------------------------------------------------------------------
test.setEdgeLineWidthMapping = function ()
{
    title = 'test.setEdgeLineWidthMapping'
    test.prep (title,FALSE)

    interaction.values = c ('inhibits', 'activates', 'interacts')
    setEdgeLineWidthMapping ('interaction', interaction.values, c (0, 8, 16), 'd')
    
    # try one-element lists
    setEdgeLineWidthMapping ('interaction', interaction.values [1], 10, 'd')
}
#-------------------------------------------------------------------------------
test.setEdgeColorMapping = function ()
{
    title = 'test.setEdgeColorMapping'
    test.prep (title,FALSE)

    interaction.values = c ('inhibits', 'activates', 'interacts')
    colors = c ('#FF0000', '#FFFF00', '#00FF00')
    setEdgeColorMapping ('interaction',  interaction.values, colors, 'd')
    Sys.sleep (0.3)
    
    all.white  = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
    setEdgeColorMapping ('interaction',  interaction.values [2],'#000000', 'd')
    
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
    test.prep (title,FALSE)
    
    interaction.values = c ('inhibits', 'activates', 'interacts')
    
    # want to see edges and both arrows, to check success of opacity Mapping
    setEdgeTargetArrowMapping ('interaction', interaction.values, rep ('ARROW', 3))
    setEdgeSourceArrowMapping ('interaction', interaction.values, rep ('ARROW', 3))
    setEdgeLineWidthDefault (5)
    
    # discrete mapping
    opacities = c (10.0, 125.0, 255.0)
    setEdgeOpacityMapping ('interaction',  interaction.values, opacities, 'd')
    
    
    # now continuous mappting
    opacities = c (10, 125, 255)
    control.points = c (3, 6.5, 10)
    setEdgeOpacityMapping ('weight',  control.points, opacities, 'c')
}
#-------------------------------------------------------------------------------
test.setEdgeTargetArrowMapping = function ()
{
    title = 'test.setEdgeTargetArrowMapping'
    test.prep (title,FALSE)
    
    arrows = c ('DELTA', 'T', 'DIAMOND')
    interaction.values = c ('inhibits', 'activates', 'interacts')
    checkEquals (length (intersect (arrows, getArrowShapes ())), 3)
    
    setEdgeTargetArrowMapping ('interaction', interaction.values, arrows)
    
    # now test the list-of-length-one call
    arrows = c ('CIRCLE')
    interaction.values = c ('inhibits')
    
    setEdgeTargetArrowMapping ('interaction', interaction.values, arrows)
}
#-------------------------------------------------------------------------------
test.setEdgeArrowColorMappings = function ()
{
    title = 'test.setEdgeArrowColorMappings'
    test.prep (title,FALSE)
    
    colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
    colors.2 = c ("#AA00AA", "#AAAA00", "#AA0000")
    
    setEdgeTargetArrowColorMapping ('interaction', c ('inhibits', 'activates', 'interacts'), colors.1, 'd')
    setEdgeSourceArrowColorMapping ('interaction', c ('inhibits', 'activates', 'interacts'), colors.1, 'd')
    system ('sleep 0.3')
    setEdgeTargetArrowColorMapping ('interaction', c ('inhibits', 'activates', 'interacts'), colors.2, 'd')
    setEdgeSourceArrowColorMapping ('interaction', c ('inhibits', 'activates', 'interacts'), colors.2, 'd')
    
    # test one-element list
    setEdgeSourceArrowColorMapping ('interaction', "inhibits", '#000000', 'd')
}
#-------------------------------------------------------------------------------
test.setEdgeSourceArrowMapping = function ()
{
    title = 'test.setEdgeSourceArrowMapping'
    test.prep (title,FALSE)
    
    arrows = c ('ARROW', 'DIAMOND', 'CIRCLE')
    interaction.values = c ('inhibits', 'activates', 'interacts')

    setEdgeSourceArrowMapping ('interaction', interaction.values, arrows)
    
    # test one-element Mapping
    setEdgeSourceArrowMapping ('interaction', interaction.values [2], arrows [2])
}
#-------------------------------------------------------------------------------
test.movie = function ()
{
    title = 'test.movie'
    test.prep (title)
    
    # establish the Mappings which apply during the full run of the movie
    # different node sizes and node colors are created, not by changing these Mappings, but
    # by changing node attribute values, for the integer attribute 'count' and the numeric attribute 'score'
    control.points = c (5, 10.5, 20)
    sizes                = c (20, 50, 100)
    setNodeSizeMapping ('score', control.points, sizes, 'c')
    setNodeColorMapping ('score', control.points, c ('#00FF00', '#FFFFFF', '#FF0000'), 'c')
    
    count = 3
    
    # three renderings of the 4-node, 4-edge network are created in this loop, which runs 'count' times
    # the first two set new attributes on the R graph data structure, then ask RCy3 to send those values
    # to R from the graph the third rendering bypasses storage of new attribute values on the R graph, 
    # sending them instead directly to Cytoscape.  (hence 'setNodeAttributesBypass')
    
    new.data <- data.frame(name=c("node 0","node 1","node 2","node 3"),
                        score=as.integer(c(20,10,15,5)), 
                        stringsAsFactors=FALSE)
    
    for (i in 1:count) {
        new.data[,'score']<-c(5,20,10,15)        
        loadTableData(new.data,'name')
        
        Sys.sleep (0.3)
        new.data[,'score']<-c(15,5,20,10)        
        loadTableData(new.data,'name')

        Sys.sleep (0.3)
        new.data[,'score']<-c(10,15,5,20)        
        loadTableData(new.data,'name')
    }
}
#-------------------------------------------------------------------------------
test.unmatchedAttributesError = function ()
{
    title = 'test.unmatchedAttributesError'
    test.prep (title,FALSE)

    # this works
    control.points = c (2, 30, 100)
    sizes = c (20, 50, 100)
    setNodeSizeMapping ('score', control.points, sizes, 'c')
}
#-------------------------------------------------------------------------------
test.simpleGraphWithReciprocalEdge = function ()
{
    title = 'test.simpleGraphWithReciprocalEdge'
    test.prep (title,FALSE)
    
    g.simple = RCy3::makeSimpleGraph ()
    g.simple = graph::addEdge ('node 2', 'node 1', g.simple)
    graph::edgeData (g.simple, 'node 2', 'node 1', attr='interaction') = 'synthetic rescue'
    graph::edgeData (g.simple, 'node 2', 'node 1', attr='weight') = 42
    
    net.suids = createNetworkFromGraph (g.simple,title)
    
    setNodeLabelMapping ('label')
    node.attribute.values = c ("A",  "B")
    colors =                c ('#A0AA00', '#FF0000')
    setNodeBorderWidthDefault (5)
    setNodeBorderColorMapping ('group', node.attribute.values, colors, 'd', default.color='#88FF22')
    control.points = c (5, 10, 20)
    sizes                = c (20, 50, 100)
    setNodeSizeMapping ('score', control.points, sizes, 'c')
    setNodeColorMapping ('score', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), 'c')
    arrows = c ('Arrow', 'Arrow', 'Arrow', 'None')
    interaction.values <- c ('inhibits', 'activates', 'synthetic rescue', 'interacts')
    setEdgeTargetArrowMapping ('interaction', interaction.values, arrows)
    
    edgeColors = c ('#0000AA', '#000000', '#00AA00', '#FFFFFF')
    setEdgeColorMapping ('interaction',  interaction.values, edgeColors, 'd')
}
#-------------------------------------------------------------------------------
test.setNodePosition = function ()
{
    title = 'test.setNodePosition'
    test.prep (title,FALSE)

    center.x = getNetworkCenter()$x
    center.y = getNetworkCenter()$y
    radius = 200
    angles = seq (0, 360, 5)  # sweep through full revoltion, 5 degrees at a time
    # move just node 0, swinging it around the network center.
    for (angle in angles) {
        angle.in.radians = angle * pi / 180
        x = center.x + (radius * cos (angle.in.radians))
        y = center.y + (radius * sin (angle.in.radians))
        setNodePropertyBypass('node 0', x, 'NODE_X_LOCATION', bypass=FALSE)
        setNodePropertyBypass('node 0', y, 'NODE_Y_LOCATION', bypass=FALSE)
        fitContent()
    }
}
#-------------------------------------------------------------------------------
test.getNodePosition = function ()
{
    title = 'test.getNodePosition'
    test.prep (title,FALSE)

    node.x <- 200
    node.y <- 210
    
    setNodePropertyBypass('node 2', node.x, 'NODE_X_LOCATION', bypass=FALSE)
    setNodePropertyBypass('node 2', node.y, 'NODE_Y_LOCATION', bypass=FALSE)
    
    get.node.x <- getNodeProperty('node 2', 'NODE_X_LOCATION')
    get.node.y <- getNodeProperty('node 2', 'NODE_Y_LOCATION')
    
    checkEqualsNumeric (get.node.x, node.x, tol=1)
    checkEqualsNumeric (get.node.y, node.y, tol=1)
}
#-------------------------------------------------------------------------------
test.getNodePosition.colonInNodeName = function ()
{
    title = 'test.getNodePosition.colonInNodeName'
    test.prep (title,FALSE)
    
    g = RCy3::makeSimpleGraph ()
    funky.node.name = 'ab::cdxyz::1234,funky!?' 
    g = graph::addNode (funky.node.name, g)
    net.suid = createNetworkFromGraph(g, title=title)
    
    node.x <- 220
    node.y <- 240
    
    setNodePropertyBypass(funky.node.name, node.x, 'NODE_X_LOCATION', bypass=FALSE)
    setNodePropertyBypass(funky.node.name, node.y, 'NODE_Y_LOCATION', bypass=FALSE)
    
    get.node.x <- getNodeProperty(funky.node.name, 'NODE_X_LOCATION')
    get.node.y <- getNodeProperty(funky.node.name, 'NODE_Y_LOCATION')
    
    checkEqualsNumeric (get.node.x, node.x, tol=1)
    checkEqualsNumeric (get.node.y, node.y, tol=1)
}

#-------------------------------------------------------------------------------
test.visualStyle = function ()
{
    title = 'test.visualStyle'
    test.prep (title,FALSE)
    
    copyVisualStyle('default','test.default')
    current.names = getVisualStyleNames ()
    checkTrue (length (intersect (current.names, c ('test.default', 'default','Marquee','Minimal', 'Sample1', 'Universe'))) >= 6)
    
    for (style.name in current.names){
        setVisualStyle(style.name)
    }
    
    setVisualStyle('test.default')
} 
#-------------------------------------------------------------------------------
test.defaultColors = function ()
{
    title = 'test.defaultColors'
    test.prep (title,FALSE)
    getBackgroundColorDefault ()
    getEdgeSelectionColorDefault ()
    getNodeSelectionColorDefault ()
    
    red = '#FF0000'
    white = '#FFFFFF'
    green = '#00FF00'
    grey = '#BBBBBB'

    setBackgroundColorDefault (grey)
    setEdgeSelectionColorDefault (green)
    setNodeSelectionColorDefault (green)
}
#-------------------------------------------------------------------------------
test.fitContent = function ()
{
    title = 'test.fitContent'
    test.prep (title,FALSE)
    
    clearSelection ()
    selectNodes ('node 0', 'name')
    
    fitContent (selected.only = TRUE)
    fitContent()
}
#-------------------------------------------------------------------------------
test.zoom = function ()
{
    title = 'test.zoom'
    test.prep (title,FALSE)
    
    smaller = 0.5
    larger = 2
    
    for (i in 1:10){
        setNetworkZoomBypass (smaller * getNetworkZoom (), bypass = FALSE)
    }
    
    for (i in 1:10){
        setNetworkZoomBypass (larger * getNetworkZoom (), bypass = FALSE)
    }
}
#-------------------------------------------------------------------------------
test.center = function ()
{
    title = 'test.setCenter'
    test.prep (title,FALSE)
    
    center.orig = getNetworkCenter ()
    delta = 100
    x.left = center.orig$x - delta
    x.right = center.orig$x + delta
    y.up = center.orig$y - delta
    y.down = center.orig$y + delta
    
    for (i in 1:10) {
        setNetworkCenterBypass (x.left, y.up, bypass = FALSE)
        setNetworkCenterBypass (as.integer (x.left), as.integer (y.up), bypass = FALSE)   # make sure the called function casts this int back to numeric
        setNetworkCenterBypass (x.left, y.down, bypass = FALSE)
        setNetworkCenterBypass (x.right, y.down, bypass = FALSE)
        setNetworkCenterBypass (x.right, y.up, bypass = FALSE)
    } 
    
    setNetworkCenterBypass (center.orig$x, center.orig$y, bypass = FALSE)
}
#-------------------------------------------------------------------------------
test.setNodeSizeBypass = function ()
{
    title = 'test.setNodeSizeBypass'
    test.prep (title,FALSE)
    
    lockNodeDimensions (TRUE)
    
    small = 30
    large = 300
    setNodeSizeBypass ('node 0', small);
    setNodeSizeBypass ('node 0', large);
}
#-------------------------------------------------------------------------------
test.getNodeSize = function ()
{
    title = 'test.getNodeSize'
    test.prep (title,FALSE)
    
    size =  getNodeSize ('node 0')
    checkEquals (unname(size), 300)

    setNodeSizeBypass ('node 0', 150); 
    size =  getNodeSize ('node 0')
    checkEquals (unname(size), 150)
    
    # now allow for non-symmetric dimensions, in which width and height are set separately
    lockNodeDimensions (FALSE)
    setNodeHeightBypass ('node 0', 80)
    setNodeWidthBypass ('node 0', 120)
    
    height = getNodeHeight('node 0') 
    width = getNodeWidth('node 0')
    checkEquals (unname(width), 120)
    checkEquals (unname(height), 80)
    
    # return to symmetric dimensions
    lockNodeDimensions (TRUE)
} 
#-------------------------------------------------------------------------------
test.setNodeWidthAndHeightBypass = function ()
{
    title = 'test.setNodeWidthAndHeightBypass'
    test.prep (title,FALSE)
    
    lockNodeDimensions (FALSE)
    
    small = 30
    large = 300
    
    setNodeWidthBypass ('node 0', small);
    setNodeHeightBypass ('node 0', large);
    setNodeWidthBypass ('node 0', large);
    setNodeHeightBypass ('node 0', small);
}
#-------------------------------------------------------------------------------
test.setNodeFontSizeBypass = function ()
{
    title = 'test.setNodeFontSizeBypass'
    test.prep (title,FALSE)

    setNodeFontFaceDefault("SansSerif,plain,12")
    setNodeFontFaceMapping('group',c("A","B"),c("SansSerif,plain,8", "Dialog,plain,8"))
    starting.size = 12
    for (i in 1:20) {
        setNodeFontSizeBypass ('node 0', starting.size + i)
        setNodeFontSizeBypass ('node 1', starting.size + (i*3))
    } 
    
    starting.size = 32
    for (i in 20:1) {
        setNodeFontSizeBypass ('node 0', starting.size - i)
        setNodeFontSizeBypass ('node 1', starting.size - (i*3))
    } 
}
#-------------------------------------------------------------------------------
test.setNodeShapeBypass = function ()
{
    title = 'test.setNodeShapeBypass'
    test.prep (title,FALSE) 
    
    lockNodeDimensions (TRUE)
    setNodeSizeBypass ('node 0', 100)
    
    for (new.shape in getNodeShapes ()) {
        setNodeShapeBypass ('node 0', new.shape)
    } 
}

#-------------------------------------------------------------------------------
test.createNetworkFromDataFrames = function ()
{
    title = 'test.createNetworkFromDataFrames'
    test.prep (title,FALSE)
    
    nodes <- data.frame(id=c("node 0","node 1","node 2","node 3"),
                        group=c("A","A","B","B"), # categorical strings
                        score=as.integer(c(20,10,15,5)), # integers
                        stringsAsFactors=FALSE)
    edges <- data.frame(source=c("node 0","node 0","node 0","node 2"),
                        target=c("node 1","node 2","node 3","node 3"),
                        interaction=c("inhibits","interacts","activates","interacts"),  # optional
                        weight=c(5.1,3.0,5.2,9.9), # numeric
                        stringsAsFactors=FALSE)
        createNetworkFromDataFrames(nodes, edges)
}

#-------------------------------------------------------------------------------
test.multigraph = function ()
{
    title = 'test.multigraph'
    test.prep (title,FALSE)
    
    nodes <- data.frame(id=c("node 0","node 1","node 2","node 3"),
                        group=c("A","A","B","B"), # categorical strings
                        score=as.integer(c(20,10,15,5)), # integers
                        stringsAsFactors=FALSE)
    edges <- data.frame(source=c("node 0","node 0","node 0","node 2","node 2"),
                        target=c("node 1","node 2","node 3","node 3","node 3"),
                        interaction=c("inhibits","interacts","activates","interacts","interacts"),  # optional
                        weight=c(5.1,3.0,5.2,9.9,8.8), # numeric
                        stringsAsFactors=FALSE)
    createNetworkFromDataFrames(nodes, edges, title = "multigraph", collection = "multigraph")
}

#-------------------------------------------------------------------------------
test.createNetworkFromIgraph = function ()
{
    title = 'test.createNetworkFromIgraph'
    test.prep (title,FALSE)
    
    ig = makeSimpleIgraph()
    createNetworkFromIgraph(ig)
}
#-------------------------------------------------------------------------------
test.createNetworkFromGraph = function ()
{
    title = 'test.createNetworkFromGraph'
    test.prep (title,FALSE)
    
    g = makeSimpleGraph()
    createNetworkFromGraph(g)
}

#-------------------------------------------------------------------------------
test.createIgraphFromNetwork = function ()
{
    title = 'test.createIgraphFromNetwork'
    test.prep (title,FALSE)
    
    ig = createIgraphFromNetwork()
    
    vatts = get.vertex.attribute(ig)
    checkEquals(sort(vatts$name), sort(c("node 1","node 3","node 2","node 0","ab::cdxyz::1234,funky!?" )) )
    checkEqualsNumeric(sort(vatts$score),sort(c(10,5, 15, 20,  0)))
    checkEquals(sort(vatts$group),sort(c("A","B","B","A","none")))
    
    eatts = get.edge.attribute(ig)
    checkEquals(sort(eatts$name), sort(c("node 2 (interacts) node 3", "node 0 (inhibits) node 1",  "node 0 (interacts) node 2", "node 0 (activates) node 3")))
    checkEqualsNumeric(sort(eatts$weight), sort(c(9.9, 5.1, 3.0, 5.2)))
    
}
#-------------------------------------------------------------------------------
test.createGraphFromNetwork = function ()
{
    
    title = 'test.createGraphFromNetwork'
    test.prep (title,FALSE)
    
    g3 = createGraphFromNetwork() 
    
    nlist = graph::nodes(g3)
    checkEquals(sort(nlist), sort(c("node 1","node 3","node 2","node 0","ab::cdxyz::1234,funky!?" )))
    
    elist = graph::edges(g3)
    checkEquals(sort(elist$`node 0`),sort(c("node 1", "node 3", "node 2")))
    
}
#-------------------------------------------------------------------------------
test.createNetworkFromSelection = function ()
{
    title = 'test.createNetworkFromSelection'
    test.prep (title,FALSE)
    
    selectNodes ('node 3', 'name', preserve.current.selection = FALSE)
    selectFirstNeighbors()
    
    createSubnetwork('selected', subnetwork.name = 'test.createNetworkFromSelection')
    
    checkEqualsNumeric(getNodeCount(),3)
}

#-------------------------------------------------------------------------------
test.customGraphics = function ()
{
    title = 'test.customGraphics'
    test.prep (title,FALSE)
    
    openSession()
    checkEqualsNumeric(getNodeCount(),330)
    
    # set canvas
    selectNodes("Gal2", by.col="COMMON")
    #selectFirstNeighbors()
    fitContent(TRUE)
    setNetworkZoomBypass(4)
    clearSelection()
    setNodeColorDefault("#FFFFFF")
    setNodeLabelMapping("COMMON")
    setNodeBorderWidthDefault(1)
    setVisualStyle('default')
    
    # paint!
    mycols<-c("gal1RGexp","gal4RGexp","gal80Rexp")
    setNodeCustomBarChart(mycols, slot=7)
    setNodeCustomBarChart(mycols, "STACKED", slot=7)
    setNodeCustomBarChart(mycols, "HEAT_STRIPS", slot=7)
    setNodeCustomBarChart(mycols, "UP_DOWN", slot=7)
        setNodeCustomPosition("NW","SE", slot=7)
    commandSleep(1)
    
    setNodeCustomBoxChart(mycols, slot=2)
    setNodeCustomBoxChart(mycols, slot=2, rangeAxis = TRUE,
                          zeroLine = TRUE,axisWidth = 1,
                          axisColor = "#888888", axisFontSize = 3)
    setNodeCustomPosition("N","S", slot=2)
    commandSleep(1)
    
    setNodeCustomHeatMapChart(mycols, slot=3)
    setNodeCustomHeatMapChart(mycols, slot=3, orientation = "VERTICAL")
    setNodeCustomHeatMapChart(mycols, slot=3, orientation = "HORIZONTAL")
    setNodeCustomPosition("NE","SW", slot=3)
    commandSleep(1)
    
    setNodeCustomLineChart(mycols, slot=4)
    setNodeCustomLineChart(mycols, slot=4, axisWidth = 1,
                           axisFontSize = 3, axisColor = "#888888",
                           rangeAxis = TRUE, zeroLine = TRUE)
    setNodeCustomPosition("SW","NE", slot=4)
    commandSleep(1)
    
    setNodeCustomPieChart(mycols, slot=5)
    setNodeCustomPieChart(mycols, slot=5, startAngle = 45)
    setNodeCustomPieChart(mycols, slot=5, startAngle = 90)
    setNodeCustomPieChart(mycols, slot=5, startAngle = 180)
    setNodeCustomPieChart(mycols, slot=5, startAngle = 360)
    setNodeCustomPosition("S","N", slot=5)
    commandSleep(1)
    
    setNodeCustomRingChart(mycols, slot=6)
    setNodeCustomRingChart(mycols, slot=6, holeSize = .1)
    setNodeCustomRingChart(mycols, slot=6, holeSize = .25)
    setNodeCustomRingChart(mycols, slot=6, holeSize = .5)
    setNodeCustomRingChart(mycols, slot=6, holeSize = .75)
    setNodeCustomPosition("SE","NW", slot=6)
    commandSleep(1)
    
    setNodeCustomLinearGradient() #NOTE: gradients only work in slot 1
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,0,1))
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.2,1))
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.4,1))
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.6,1))
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.8,1))
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,1,1))
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.6,1), angle = 0)
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.6,1), angle = 45)
    setNodeCustomLinearGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.6,1), angle = 90)
    commandSleep(1)

    setNodeCustomRadialGradient()    
    setNodeShapeDefault("ELLIPSE")
    lockNodeDimensions(TRUE)
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,0,1))
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.2,1))
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.4,1))
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.6,1))
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.8,1))
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.2,1), 
                                xCenter = .5, yCenter = .5)
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.2,1), 
                                xCenter = .7, yCenter = .3)
    setNodeCustomRadialGradient(c("#660088","#EE99FF","#AA00DD"), c(0,.2,1), 
                                xCenter = .9, yCenter = .1)
    
    setNetworkZoomBypass(2)
    commandSleep(1)
    setNetworkZoomBypass(1)
    commandSleep(1)
    setNetworkZoomBypass(.5)
    commandSleep(1)
    fitContent()
    commandSleep(1)
    
    removeNodeCustomGraphics(slot=2)
    removeNodeCustomGraphics(slot=3)
    removeNodeCustomGraphics(slot=4)
    removeNodeCustomGraphics(slot=5)
    removeNodeCustomGraphics(slot=6)
    removeNodeCustomGraphics(slot=7)
    removeNodeCustomGraphics()
    
}

#-------------------------------------------------------------------------------
test.filters = function ()
{
    
    title = 'test.filters'
    test.prep (title,FALSE)
    
    setNetworkZoomBypass(0.6)
    # COLUMN FILTERS ON NODES
    sel<-createColumnFilter('gal4RGexp-up', 'gal4RGexp', 2, "GREATER_THAN")
    checkTrue(is.na(sel$nodes[1]))
    sel<-createColumnFilter('gal4RGexp-dn', 'gal4RGexp', 0, "LESS_THAN")
    checkEqualsNumeric(length(sel$nodes),212)
    sel<-createColumnFilter('gal4RGexp-sig', 'gal4RGexp', c(-1,1), "IS_NOT_BETWEEN")
    checkEqualsNumeric(length(sel$nodes),14)
    sel<-createColumnFilter('gal1RGexp-sig', 'gal1RGexp', c(-1,1), "IS_NOT_BETWEEN")
    checkEqualsNumeric(length(sel$nodes),7)
    sel<-createColumnFilter('Y*****C', 'name', "^Y.*C$", "REGEX")
    checkEqualsNumeric(length(sel$nodes),148)
    
    # COMPOSITE OF NODE COLUMN FILTERS
    sel<-createCompositeFilter('comp1', c('gal4RGexp-sig','gal1RGexp-sig'))
    checkEqualsNumeric(length(sel$nodes),4)
    sel<-createCompositeFilter('comp2', c('gal4RGexp-sig','gal1RGexp-sig'), "ANY")
    checkEqualsNumeric(length(sel$nodes),17)
    
    # APPLY AND HIDE
    sel<-applyFilter('gal4RGexp-sig', hide=TRUE)
    checkEqualsNumeric(length(sel$nodes),14)
    
    
    # COLUMN FILTER ON EDGES
    sel<-createColumnFilter('edge1', 'EdgeBetweenness', 6, "IS", type="edges")
    checkEqualsNumeric(length(sel$edges),7)
    
    #COMPOSITE OF NODE AND EDGE, AND HIDE
    sel<-createCompositeFilter('comp3', c('edge1','gal4RGexp-dn'), 'ANY', hide=TRUE)
    checkEqualsNumeric(length(sel$nodes),212)
    checkEqualsNumeric(length(sel$edges),7)
    unhideAll()
    
    #DEGREE FILTER
    sel<-createDegreeFilter('deg1', c(5,10))
    checkEqualsNumeric(length(sel$nodes),20)
    
    #GET FILTERS
    flist<-getFilterList()
    checkEqualsNumeric(length(flist),11)
    
}

#-------------------------------------------------------------------------------
test.annotations = function ()
{
    
    title = 'test.annotations'
    test.prep (title,FALSE)
    
    addAnnotationText("test1")
    addAnnotationText("test2", 1000, 1000, name="T1")
    addAnnotationText("test!@#$%^3", 1000, 1000, 30, "Helvetica", "bolditalic", "#990000",680,name="T2", canvas="background",z=10)
    addAnnotationText("test\n2", 1200, 1000, 30, "Courier New", "bold", "#009900",0,name="T3", canvas="foreground",z=1)
    addAnnotationText("test\t1", 1400, 1000, 30, "Comic sans MS", "italic", "#000099",40,name="T4", canvas="foreground",z=0)
    
    checkEqualsNumeric(length(getAnnotationList()),5)
    ann.uuids <- sapply(getAnnotationList(), '[[', 'uuid')
    
    deleteAnnotation(ann.uuids[1])
    checkEqualsNumeric(length(getAnnotationList()),4)
    deleteAnnotation(ann.uuids)
    
    addAnnotationBoundedText("test1")
    addAnnotationBoundedText("test2", 1000, 1000, name="B2")
    addAnnotationBoundedText("test3", 1200, 1000, 30, "Helvetica", "bold", "#990000",40,name="B3", canvas="foreground",z=4)

    addAnnotationShape("rectangle")
    addAnnotationShape("rectangle", 1000, 1000, name="S2")
    addAnnotationShape("rectangle", 1200, 1000, 30, "#990000", 40,name="S3", canvas="background",z=4)

    ann.uuids <- sapply(getAnnotationList(), '[[', 'uuid')
    deleteAnnotation(ann.uuids)
    checkEqualsNumeric(length(getAnnotationList()),0)
    
    addAnnotationText("test1", name="test1")
    addAnnotationText("test2", name="test2")
    groupAnnotation(c("test1","test2"))
    checkEqualsNumeric(length(getAnnotationList()),3)
    ungroupAnnotation("Group 1")
    checkEqualsNumeric(length(getAnnotationList()),2)
    ann.uuids <- sapply(getAnnotationList(), '[[', 'uuid')
    deleteAnnotation(ann.uuids)
    checkEqualsNumeric(length(getAnnotationList()),0)
    
}
