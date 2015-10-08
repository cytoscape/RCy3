# RCytoscape/inst/test_cytoscape.R
#------------------------------------------------------------------------------------------------------------------------
library (RCy3)
library (RUnit)

#TODO uncomment test.getNodeShapes (), test.getArrowShapes, test.getLineStyles () etc once working
#------------------------------------------------------------------------------------------------------------------------
if (!exists ('cy'))
  cy = CytoscapeConnection ()
#------------------------------------------------------------------------------------------------------------------------
# the peculiar naming of this function ensures that it will be called first, before all other test.xxx methods.  i think!
test...aaaaFirstTestCalled = function ()
{
  print ('------- first test, deleting any pre-existing CytoscapeWindows')
  deleteAllWindows (CytoscapeConnection ());
}
#------------------------------------------------------------------------------------------------------------------------
run.tests = function ()
{
  options ('warn'=2)   # make sure that any R warnings are treated as fatal errors

    # before doing anything else, make sure that the Cytoscape plugin version is one we can respond to
  test.plugin.version ()
  
    # start with a clean slate, and no windows
  deleteAllWindows (cy)

    # many of the tests modify the default visual style, quite heedless of prior or successor tests.
    # if you wish to restore the initial default style, it is saved here, just once, upon entry to this method
    # 
  #save.default.vizmap ()

  test.create.class ()
  test.deleteWindow ()
  test.deleteAllWindows ()
  test.getWindowID ()
  test.getWindowList ()
  test.getNodeShapes ()
  test.getAttributeClassNames ()
  test.getArrowShapes ()
  test.getLineStyles ()
  test.getLayoutNames ()
  #test.getLayoutNameMapping ()
  #test.getLayoutPropertyNames ()
  #test.getLayoutPropertyType ()
  #test.getLayoutPropertyValue ()
  #test.setLayoutProperties ()
  test.sendNodes ()
  test.sendEdges ()
  test.setNodeAttributes ()
  test.setEdgeAttributes ()
  test.noa ()
  test.eda ()
  test.cy2.edge.names ()
  test.getAdjacentEdgeNames ()
  #test.panelOperations ()
  test.showGraphicsDetails ()
  test.setDefaultNodeShape ()
  test.setDefaultNodeColor ()
  test.setDefaultNodeSize ()
  test.setDefaultNodeBorderColor ()
  test.setDefaultNodeBorderWidth ()
  test.setDefaultNodeFontSize ()
  test.setDefaultNodeLabelColor ()
  test.setDefaultEdgeLineWidth ()
  test.setDefaultEdgeColor ()
  test.setDefaultEdgeFontSize ()
  test.setNodeLabelRule ()
  test.setEdgeLabelRule ()
  test.setNodeTooltipRule ()
  test.setEdgeTooltipRule ()
  test.setNodeColorRule ()
  test.setNodeBorderColorRule ()
  test.setNodeBorderWidthRule ()
  test.setNodeSizeRule ()
  test.setNodeShapeRule ()
  test.setNodeOpacityRule ()
  test.setNodeColorDirect ()
  test.setNodeBorderColorDirect ()
  test.setNodeLabelDirect ()
  test.setNodeLabelPropertiesDirect ()
  test.setNodeOpacityDirect ()
  test.setEdgeOpacityDirect ()
  test.setEdgeColorDirect ()
  test.setEdgeSourceArrowShapeDirect ()
  test.setEdgeLabelDirect ()
  test.setEdgeFontSizeDirect ()
  test.setEdgeLabelColorDirect ()
  test.setEdgeTooltipDirect ()
  test.setEdgeLineWidthDirect ()
  test.setEdgeLineStyleDirect ()
  test.setEdgeSourceArrowShapeDirect ()
  test.setEdgeTargetArrowShapeDirect ()
  test.setEdgeSourceArrowColorDirect ()
  test.setEdgeTargetArrowColorDirect ()
  test.setEdgeLabelOpacityDirect ()
  test.setEdgeSourceArrowOpacityDirect ()
  test.setEdgeTargetArrowOpacityDirect ()
  test.setEdgeLabelPositionDirect ()
  #test.setEdgeLabelWidthDirect ()
  test.countNodes ()
  test.countEdges ()
  test.countNodesAndEdgesInEmptyGraph ()
  test.getAllNodes ()
  test.getAllEdges ()
  test.selectNodes ()
  #test.nodeNeighborReportingAndSelection ()
  test.invertSelection ()
  test.deleteSelectedNodes ()
  #test.hideNodes ()
  #test.selectEdges ()
  test.setEdgeLineStyleRule ()
  test.setEdgeLineWidthRule ()
  test.setEdgeColorRule ()
  #test.setEdgeTargetArrowRule ()
  test.setEdgeArrowColorRules ()
  #test.setEdgeSourceArrowRule ()
  test.movie ()
  test.unmatchedAttributesError ()
  test.remove.redundancies.in.undirected.graph ()
  test.randomUndirectedGraph ()
  test.simpleGraph ()
  test.simpleGraphWithReciprocalEdge ()
  test.setGraph ()
  test.setNodePosition ()
  test.getNodePosition ()
  test.getNodePosition.colonInNodeName ()
  test.getNodeSize ()
  #test.haveNodeAttribute ()
  #test.haveEdgeAttribute ()
  #test.copyNodeAttributesFromCyGraph ()
  #test.copyEdgeAttributesFromCyGraph ()
  #test.getGraphFromCyWindow ()
  test.sendDegenerateGraphs ()
  test.sendBigGraph ()
  #test.createWindowFromSelection ()
  #test.addGraphToGraph ()
  test.addGraphToGraph.degenerateFirstGraph ()
  test.existing.CytoscapeWindow ()
  test.existing.CytoscapeWindow.noEdges ()
  test.existing.CytoscapeWindow.emptyGraph ()
  test.getAttributeNames ()
  #test.addGetAndDeleteEdgeAttributes ()
  #test.addGetAndDeleteNodeAttributes ()
  #test.getAllNodeAttributes ()
  test.getAllEdgeAttributes ()
  test.getVisualStyleNames ()
  test.copyVisualStyle ()
  #test.setVisualStyle ()
  #test.defaultColors ()
  test.setWindowSizeRaiseWindow ()
  test.fitContent ()
  #test.windowCoordinates ()
  test.zoom ()
  test.center ()
  test.setNodeSizeDirect ()
  test.setNodeWidthAndHeightDirect ()
  test.setNodeFontSizeDirect ()
  test.setNodeShapeDirect ()
  #test.setEdgeVizPropertiesDirect ()
  #test.graphBAM ()
  test.addCyNode ()
  test.addCyEdge ()
  test.twoGraphsDoubleEdges ()
  test..classicGraphToNodePairTable ()
  test.rcy.edgeNames ()
  test..getNovelEdges ()
  test.setNodeImageDirect ()
  #test.validity ()
  test.tooltip.delays ()

  options ('warn'=0)

} # run.tests
#------------------------------------------------------------------------------------------------------------------------
save.default.vizmap = function ()
{
  default.style.name <- 'original.default.style'

  if (!default.style.name %in% getVisualStyleNames (cy)) # it has not previously been stored
     copyVisualStyle (CytoscapeConnection (), 'default', 'orginal.default.style')


} # save.default.vizmap
#------------------------------------------------------------------------------------------------------------------------
# almost every test needs to
#
#   !) announce it's name to stdout
#   2) delete any previous window with the same title, should any exist
#
# these services are provided here
#
window.prep = function (title)
{
  write (noquote (sprintf ('------- %s', title)), stderr ())

  cy = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cy)))
     deleteWindow (cy, title)

} # window.prep


.setUp <- function() deleteAllWindows(CytoscapeConnection())
.tearDown <- function() deleteAllWindows(CytoscapeConnection())


#------------------------------------------------------------------------------------------------------------------------
test.plugin.version = function ()
{
  title = 'test.plugin.version'
  cy = CytoscapeConnection ()

  plugin.version.string = pluginVersion (cy)
  string.tmp1 = strsplit (plugin.version.string,' ')[[1]][1]
  string.tmp2 = gsub ('[a-z]', '', string.tmp1)
  string.tmp3 = gsub ('[A-Z]', '', string.tmp2)
  major.minor.version = as.numeric (string.tmp3)


  #tokens = strsplit (version.string, ' ')[[1]][1]
  #version.numbers = as.integer (strsplit (tokens, '\\.')[[1]])
  #major.minor.version = version.numbers [1] + (version.numbers [2]/10.0)
  msg (cy, paste ('CytoscapeRPC version', major.minor.version))
  checkTrue (major.minor.version >= 0.99)

} # test.plugin.version
#------------------------------------------------------------------------------------------------------------------------
test.create.class = function ()
{
  title = 'test.create.class'
  window.prep (title)

  g = new ('graphNEL')

  cw = CytoscapeWindow (title, g)
  checkTrue (validObject (cw))

} # test.create.class
#------------------------------------------------------------------------------------------------------------------------
test.deleteWindow = function ()
{
  title = 'test.deleteWindow'
  window.prep (title)

  cy = CytoscapeConnection ()
  cw = CytoscapeWindow (title, new ('graphNEL'))

  original.window.count = getWindowCount (cy)
  deleteWindow (cw)
  new.window.count = getWindowCount (cy)
  checkTrue (new.window.count == original.window.count - 1)

    # now delete a window by name
  window.prep (title)
  cw = CytoscapeWindow (title, new ('graphNEL'))
  original.window.count = getWindowCount (cy)
  deleteWindow (cy, title)
  new.window.count = getWindowCount (cy)
  checkTrue (new.window.count == original.window.count - 1)

} # test.deleteWindow
#------------------------------------------------------------------------------------------------------------------------
test.deleteAllWindows = function ()
{
  title = 'test.deleteAllWindows'
  cy = CytoscapeConnection ()
  deleteAllWindows (cy)
  new.window.count = getWindowCount (cy)
  checkEquals (new.window.count, 0)
  msg (cy, 'deleted all windows')

} # test.deleteAllWindows
#------------------------------------------------------------------------------------------------------------------------
test.getWindowID = function ()
{
  title = 'test.getWindowID'
  window.prep (title)

  cw3 =  CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

  cy = CytoscapeConnection ()

  id = getWindowID (cy, 'test.getWindowID')
  checkEquals (id, cw3@window.id)

  invisible (cw3)

} # test.getWindowID
#------------------------------------------------------------------------------------------------------------------------
test.getWindowList = function ()
{
  title = 'test.getWindowList'
  window.prep (title)

  cw2 = CytoscapeWindow (title, new ('graphNEL'))
  window.list = getWindowList (cw2)
  checkTrue (title %in% as.character (getWindowList (cw2)))

  invisible (cw2)

} # test.getWindowList
#------------------------------------------------------------------------------------------------------------------------
test.getNodeShapes = function ()
{
  title = 'test.getNodeShapes'
  window.prep (title)

  cy = CytoscapeConnection ()
  shapes = getNodeShapes (cy)
  checkTrue (length (shapes) > 8)
  msg (cy, title)

   # pick a few specific shapes to test
 checkTrue (all (sapply (c ('HEXAGON', 'ELLIPSE', 'TRIANGLE'), function (s) s %in% shapes)))

} # test.getNodeShapes
#------------------------------------------------------------------------------------------------------------------------
test.getAttributeClassNames = function ()
{
  title = 'test.getAttributeClassNames'

  cy = CytoscapeConnection ()
  possible.values = getAttributeClassNames (cy)
  checkTrue (grep ('numeric', possible.values) > 0)
  checkTrue (grep ('integer', possible.values) > 0)
  checkTrue (grep ('character', possible.values) > 0)


} # test.getAttributeClassNames
#------------------------------------------------------------------------------------------------------------------------
test.getArrowShapes = function ()
{
  title = 'test.getArrowShapes'

  cy = CytoscapeConnection ()
  shapes = getArrowShapes (cy)
  checkTrue (length (shapes) >= 8)

   # pick a few specific shapes to test
  msg (cy, 'getArrowShapes')
  checkTrue (all (sapply (c ('DIAMOND', 'T', 'CIRCLE'), function (s) s %in% shapes)))

} # test.getArrowShapes
#------------------------------------------------------------------------------------------------------------------------
test.getLineStyles = function ()
{
  title = 'test.getLineStyles'

  cy = CytoscapeConnection ()
  styles = getLineStyles (cy)
  checkTrue (length (styles) > 10)

   # pick a few specific styles to test
  msg (cy, 'getLineStyles')
  checkTrue (all (sapply (c ('SOLID', 'DOT', 'EQUAL_DASH'), function (s) s %in% styles)))

} # test.getLineStyles
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutNames = function ()
{
  title = 'test.getLayoutNames'

  cy = CytoscapeConnection ()
  names = getLayoutNames (cy)
  checkTrue (length (names) > 10)

   # pick a few specific styles to test
  msg (cy, 'getLayoutNames')
  checkTrue (all (sapply (c ('grid', 'isom', 'circular'), function (s) s %in% names)))

} # test.getLayoutNames
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutNameMapping = function ()
{
  cy = CytoscapeConnection ()
  name.map = getLayoutNameMapping (cy)
  checkTrue (length (name.map) >= 18)    # 20 on (4 mar 2011)

  checkEquals (name.map [['Sugiyama Layout']], "jgraph-sugiyama")
  checkEquals (name.map [['Edge-Weighted Spring Embedded']], "kamada-kawai")
  checkEquals (name.map [['Grid Layout']], "grid")

} # test.getLayoutNameMapping
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutPropertyNames = function ()
{
   print (noquote ('------- test.getLayoutPropertyNames'))
   cy = CytoscapeConnection ()
   props = getLayoutPropertyNames (cy, 'force-directed')
   expected = c ("defaultNodeMass", "defaultSpringCoefficient", "defaultSpringLength", 
                 "discrete", 
                 "edge_attribute", 
                 "edge_weight_group", "force_alg_settings", "max_weight", "min_weight", "numIterations", 
                 "partition", "selected_only", "standard", "weight_type")
   checkTrue (length (intersect (props, expected)) > (length (props) - 2))  # some variation across Cytoscape versions

   props = getLayoutPropertyNames (cy, 'isom')
   expected = c ("coolingFactor", "initialAdaptation", "maxEpoch", "minAdaptation", "minRadius", 
                 "radius", "radiusConstantTime", "sizeFactor")
   checkEquals (sort (props), expected)


} # test.getLayoutPropertyNames
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutPropertyType = function ()
{
   print (noquote ('------- test.getLayoutPropertyType'))
   cy = CytoscapeConnection ()

     # a couple of single call tests
   checkEquals (getLayoutPropertyType (cy, 'isom', 'coolingFactor'), 'DOUBLE')
   checkEquals (getLayoutPropertyType (cy, 'force-directed', 'edge_weight_group'), 'GROUP')

   props = getLayoutPropertyNames (cy, 'force-directed')
    # now get all the property types for the force-directed layout
  propTypes.all = sapply (sort (props), function (prop) getLayoutPropertyType (cy, 'force-directed', prop))

    # check them all
  checkEquals (propTypes.all [["defaultNodeMass"]], "DOUBLE")
  checkEquals (propTypes.all [["defaultSpringCoefficient"]], "DOUBLE")
  checkEquals (propTypes.all [["defaultSpringLength"]], "DOUBLE")
  #checkEquals (propTypes.all [["discrete"]], "BOOLEAN")
  checkEquals (propTypes.all [["edge_attribute"]], "EDGEATTRIBUTE")
  checkEquals (propTypes.all [["edge_weight_group"]], "GROUP")
  checkEquals (propTypes.all [["force_alg_settings"]], "GROUP")
  checkEquals (propTypes.all [["max_weight"]], "DOUBLE")
  checkEquals (propTypes.all [["min_weight"]], "DOUBLE")
  checkEquals (propTypes.all [["numIterations"]], "INTEGER")
  checkEquals (propTypes.all [["partition"]], "BOOLEAN")
  checkEquals (propTypes.all [["selected_only"]], "BOOLEAN")
  checkEquals (propTypes.all [["standard"]], "GROUP")
  checkEquals (propTypes.all [["weight_type"]], "LIST")

} # test.getLayoutPropertyType
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutPropertyValue = function ()
{
   print (noquote ('------- test.getLayoutPropertyValue'))
   cy = CytoscapeConnection ()

   layout.name = 'force-directed'
   props = getLayoutPropertyNames (cy, layout.name)

   prop = 'edge_attribute'
   checkTrue (prop %in% props)
   checkEquals (getLayoutPropertyValue (cy, layout.name, prop), 'weight')

   prop = 'numIterations'
   checkTrue (prop %in% props)
   checkEquals (getLayoutPropertyValue (cy, layout.name, prop), 100)

   prop = 'min_weight'
   checkTrue (prop %in% props)
   checkEquals (getLayoutPropertyValue (cy, layout.name, prop), 0.0)

   prop = 'max_weight'
   checkTrue (prop %in% props)
   checkTrue (getLayoutPropertyValue (cy, layout.name, prop) > 1e300)

   for (prop in props) {
     value = getLayoutPropertyValue (cy, layout.name, prop)
     #printf ('force-directed layout %s: %s', prop, value)
     } # for prop

} # test.getLayoutPropertyValue
#------------------------------------------------------------------------------------------------------------------------
test.setLayoutProperties = function ()
{
    # first, do some 'blind' (that is, window-less) tests
  cy = CytoscapeConnection ()
  layout.name = 'force-directed'
  prop = 'numIterations'
  setLayoutProperties (cy, layout.name, list (numIterations=200))
  checkEquals (getLayoutPropertyValue (cy, layout.name, prop), 200)

    # return to the defaults
  setLayoutProperties (cy, layout.name, list (numIterations=100))
  checkEquals (getLayoutPropertyValue (cy, layout.name, prop), 100)

    # now create a Cy window and manipulate the layout

  title = 'test.setLayoutProperties'
  window.prep (title)

  cw =  CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)

     # use a popular (and tunable) layout algorithm, show how selected properties can be modified, 
     # and demonstrate how they affect the layout.

  layout.name = getLayoutNameMapping (cy)[['Edge-Weighted Spring Embedded']]  

  checkTrue ('edge_attribute'    %in% getLayoutPropertyNames (cy, layout.name))
  checkTrue ('distance_strength' %in% getLayoutPropertyNames (cy, layout.name))

  setLayoutProperties (cy, layout.name, list (edge_attribute='score', distance_strength=100))
  layoutNetwork (cw, layout.name)

  setLayoutProperties (cy, layout.name, list (edge_attribute='score', distance_strength=-100))
  layoutNetwork (cw, layout.name)

  setLayoutProperties (cy, layout.name, list (edge_attribute='score', distance_strength=100))
  layoutNetwork (cw, layout.name)

  invisible (cw)

} # test.setLayoutProperties
#------------------------------------------------------------------------------------------------------------------------
test.collectTimings = function ()
{
  title = 'test.collectTimings'
  window.prep (title)

  cy = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cy)))
     deleteWindow (cy, title)

  cwe = CytoscapeWindow (title, graph=makeSimpleGraph (), collectTimings=TRUE)
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  invisible (cwe)  

} # test.collectTimings
#------------------------------------------------------------------------------------------------------------------------
test.sendNodes = function ()
{
  title = 'test.sendNodes'
  window.prep (title)
  g = RCy3::makeSimpleGraph ()

  cwa = CytoscapeWindow (title, graph=g)
  sendNodes (cwa)
  layoutNetwork (cwa, "grid")   # no edges, so other layouts will simply superimpose the nodes
  redraw (cwa)
  msg (cwa, 'sendNodes')

  invisible (cwa)

} # test.sendNodes
#------------------------------------------------------------------------------------------------------------------------
test.sendEdges = function ()
{
  title = 'test.sendEdges'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cwe = CytoscapeWindow (title, graph=g)
  sendNodes (cwe)
  sendEdges (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  msg (cwe, 'sendEdges')

  invisible (cwe)

} # test.sendEdges
#------------------------------------------------------------------------------------------------------------------------
test.setNodeAttributes = function ()
{
  title = 'test.setNodeAttributes'
  window.prep (title)

  cy = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cy)))
     deleteWindow (cy, title)

  g = RCy3::makeSimpleGraph ()
  cwb = CytoscapeWindow (title, graph=g)
  sendNodes (cwb)
  attribute.names = noa.names (g)

  for (attribute.name in attribute.names) {
    result = setNodeAttributes (cwb, attribute.name)
    }

  layoutNetwork (cwb, 'grid')
  redraw (cwb)
  msg (cwb, 'setNodeAttributes')


    # now call the direct method, which -- in contrast to the unmarked method (setNodeAttributes) -- does not
    # extract attributes from the graph; they are instead supplied separately, and thus are well suited to
    # successive updates, as in a movie

  result = setNodeAttributesDirect (cwb, 'count', 'int', c ('A', 'B', 'C'), c (38, 105, 0))

    # sending a single attribute to CytoscapeRPC runs into a problem:  xmlrpc maps these to scalars,
    # rather than as lists of length 1, and no CytoscapeRPC method is matched.
    # (10 dec 2010) RCy3::setNodeAttributesDirect solves this inelegantly, but duplicating
    # the node name and attribute values, making lists of length 2
    # 
  result = setNodeAttributesDirect (cwb, 'count', 'int', 'A', 432)

  invisible (cwb)


} # test.setNodeAttributes
#------------------------------------------------------------------------------------------------------------------------
# depends on prior creation of cwe by test.sendEdges
test.setEdgeAttributes = function ()
{
  title = 'test.setEdgeAttributes'
  window.prep (title)

  cy = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cy)))
     deleteWindow (cy, title)

  cwe = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  attribute.names = eda.names (cwe@graph)

  for (attribute.name in attribute.names) {
    result = setEdgeAttributes (cwe, attribute.name)
    } 

  edge.names = as.character (cy2.edge.names (cwe@graph))
  checkEquals (length (edge.names), 3)
  edge.values = c ('alligator', 'hedgehog', 'anteater')
  result = setEdgeAttributesDirect (cwe, 'misc', 'string', edge.names, edge.values)

    # sending a single attribute to CytoscapeRPC runs into a problem:  xmlrpc maps these to scalars,
    # rather than as lists of length 1, and no CytoscapeRPC method is matched.
    # (10 dec 2010) RCy3::setEdgeAttributesDirect solves this inelegantly, but duplicating
    # the edge name and attribute values, making lists of length 2

  result = setEdgeAttributesDirect (cwe, 'misc', 'string', edge.names [1], edge.values [1])

  msg (cwe, 'setEdgeAttributes')

  invisible (cwe)  

} # test.setEdgeAttributes
#------------------------------------------------------------------------------------------------------------------------
test.noa = function ()
{
  title = 'test.noa'
  window.prep (title)

  g.simple = makeSimpleGraph ()

  result = noa (g.simple, 'type')
  checkEquals (length (result), 3)
  checkEquals (sort (names (result)), c ('A', 'B', 'C'))
  checkEquals (result [['A']], 'kinase')
  checkEquals (result [['B']], 'transcription factor')
  checkEquals (result [['C']], 'glycoprotein')

  checkTrue (is.na (noa (g.simple, 'bogusAttributeName')))
  invisible (g.simple)

} # test.noa
#------------------------------------------------------------------------------------------------------------------------
test.eda = function ()
{
  title = 'test.eda'
  window.prep (title)

  g.simple = makeSimpleGraph ()

  result = eda (g.simple, 'edgeType')
  checkEquals (length (result), 3)
  checkEquals (sort (names (result)), c ("A|B", "B|C", "C|A"))
  checkEquals (result [['A|B']], 'phosphorylates')
  checkEquals (result [['B|C']], 'synthetic lethal')
  checkEquals (result [['C|A']], 'undefined')

  checkTrue (is.na (eda (g.simple, 'bogusAttributeName')))
  invisible (g.simple)

} # test.eda
#------------------------------------------------------------------------------------------------------------------------
test.cy2.edge.names = function ()
{
  title = 'test.cy2.edge.names'
  window.prep (title)
  g = RCy3::makeSimpleGraph ()

    # this graph has the expected 'edgeType' edge attribute, used to make a standard cytoscape edge name
  edge.names = cy2.edge.names (g)
  checkEquals (edge.names [['A~B']], "A (phosphorylates) B")
  checkEquals (edge.names [['B~C']], "B (synthetic lethal) C")
  checkEquals (edge.names [['C~A']], "C (undefined) A")

    # now create a tiny graph, two nodes, one edge, with NO edgeType attribute.  make sure it is converted properly
  g2 =  new ('graphNEL', edgemode='directed')

  g2 = graph::addNode ('A', g2)
  g2 = graph::addNode ('B', g2)
  g2 = graph::addEdge ('A', 'B', g2)

  edge.names.2 = cy2.edge.names (g2)  
  checkEquals (edge.names.2 [['A~B']], "A (unspecified) B")

  g3 = new ('graphNEL', edgemode='directed')
  edge.names.should.be.empty = cy2.edge.names (g3)
  checkTrue (is.na (edge.names.should.be.empty))


   #  now create a directed graphNEL with one reciprocal edge.  do we get the cy2 edge names properly?
  g.recip <- RCy3::makeSimpleGraph ()
  g.recip <- graph::addEdge ('C', 'B', g.recip)
  edgeData (g.recip, 'C', 'B', attr='edgeType') <- 'synthetic rescue'
  edgeData (g.recip, 'C', 'B', attr='score') <- 42
  edgeData (g.recip, 'C', 'B', attr='misc') <- 'ellany'
  
  g.recip.cy2.edge.names <- cy2.edge.names (g.recip)
  checkEquals (length (g.recip.cy2.edge.names), 4)
  checkEquals (sort (names (g.recip.cy2.edge.names)), c ("A~B", "B~C", "C~A", "C~B"))
  checkEquals (sort (as.character (g.recip.cy2.edge.names)), 
               c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (synthetic rescue) B", "C (undefined) A"))

  invisible (g3)

    # now try the subsetting version, where only the cy2 edge names of the specified edges -- a subset -- are returned
  g = makeSimpleGraph ()
  r.edge.names = sort (edgeNames (g)) 
  checkEquals (r.edge.names, c ("A~B", "B~C", "C~A"))
  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))),
                c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (undefined) A"))

  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))) [1], "A (phosphorylates) B")
  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))) [2], "B (synthetic lethal) C")
  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))) [3], "C (undefined) A")

  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))) [1:2], c ("A (phosphorylates) B", "B (synthetic lethal) C"))
  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))) [c(1,3)], c ("A (phosphorylates) B", "C (undefined) A"))
  checkEquals (sort (as.character (cy2.edge.names (g, r.edge.names))) [2:3], c ("B (synthetic lethal) C", "C (undefined) A"))

} # test.cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
# depends on prior creation of cw by test.createClass, providing a CytoscapeWindow object, with a 'uri' slot
test.panelOperations = function ()
{
  title = 'test.panelOperations'
  window.prep (title)

  cw = CytoscapeWindow (title)

  hidePanel (cw, 'Control Panel')
  hidePanel (cw, 'd')

  floatPanel (cw, 'Control Pa')
  floatPanel (cw, 'DATA')

  dockPanel (cw, 'control ')
  dockPanel (cw, 'data panel')
  msg (cw, 'test.panelOperations')

} # test.panelOperations
#------------------------------------------------------------------------------------------------------------------------
test.showGraphicsDetails = function ()
{
  title = 'test.showGraphicsDetails'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  showGraphicsDetails (cw, FALSE)
  showGraphicsDetails (cw, TRUE)

  cy = CytoscapeConnection ()
  
  showGraphicsDetails (cy, FALSE)   
  showGraphicsDetails (cy, TRUE)

  invisible (cw)

} # test.showGraphicsDetails
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeShape = function (direct=FALSE)
{
  title = 'test.setDefaultNodeShape'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

   hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
   shapes = getNodeShapes (cwe)

   if (direct) {  # debug
     for (shape in shapes) {
       setDefaultNodeShape (cwe, shape); redraw (cwe)
       redraw (cwe); 
       Sys.sleep (1)
       } # for shape
     } # direct

  setDefaultNodeShape (cwe, 'octagon'); redraw (cwe)
  msg (cwe, 'octagon')
  Sys.sleep (1)
  setDefaultNodeShape (cwe, 'ellipse');  redraw (cwe)
  msg (cwe,'ellipse')
  Sys.sleep (1)
  setDefaultNodeShape (cwe, 'triangle');  redraw (cwe)
  msg (cwe, 'triangle')

  invisible (cwe)

} # test.setDefaultNodeShape
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeColor = function (direct=FALSE)
{
  title = 'test.setDefaultNodeColor'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  if (direct) {  # useful for debuggin
    for (i in 1:3) {
      if (!exists ('xml.rpc')) library (XMLRPC)
      xml.rpc (cwe@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Color', '#AAAA00'); redraw (cwe)
      xml.rpc (cwe@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Color', '#00AAAA'); redraw (cwe)
      } # for i
    } # direct

  setDefaultNodeColor (cwe, '#AA00AA')
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeSize = function (direct=FALSE)
{
  title = 'test.setDefaultNodeSize'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  for (i in 1:3) {
    setDefaultNodeSize (cwe, 20)
    redraw (cwe)
    Sys.sleep (1)
    setDefaultNodeSize (cwe, 200)
    redraw (cwe)
    Sys.sleep (1)
    } # for i

  setDefaultNodeSize (cwe, 60)
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeSize
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeBorderColor = function (direct=FALSE)
{
  title = 'test.setDefaultNodeBorderColor'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  for (i in 1:3) {
    setDefaultNodeBorderColor (cwe, '#FFFFFF'); 
    redraw (cwe)
    Sys.sleep (1)
    setDefaultNodeBorderColor (cwe, '#FF0000'); 
    redraw (cwe)
    Sys.sleep (1)
    } # for i

  invisible (cwe)

} # test.setDefaultNodeBorderColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeBorderWidth = function (direct=FALSE)
{
  title = 'test.setDefaultNodeBorderWidth'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultNodeBorderWidth (cwe, 5)
    redraw (cwe)
    Sys.sleep (1)
    setDefaultNodeBorderWidth (cwe, 0)
    redraw (cwe)
    Sys.sleep (1)
    } # for i

  setDefaultNodeBorderWidth (cwe, 1)
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeBorderWidth
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeFontSize = function (direct=FALSE)
{
  title = 'test.setDefaultNodeFontSize'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  for (i in 1:3) {
    setDefaultNodeFontSize (cwe, 3); redraw (cwe)
    Sys.sleep (1)
    setDefaultNodeFontSize (cwe, 30); redraw (cwe)
    Sys.sleep (1)
    }
  
  setDefaultNodeFontSize (cwe, 12); redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeFontSize
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeLabelColor = function (direct=FALSE)
{
  title = 'test.setDefaultNodeLabelColor'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultNodeLabelColor (cwe, '#FFAAAA');redraw (cwe)
    Sys.sleep (1)
    setDefaultNodeLabelColor (cwe, '#000000');redraw (cwe)
    Sys.sleep (1)
    } # for i

  invisible (cwe)

} # test.setDefaultNodeLabelColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultEdgeLineWidth = function (direct=FALSE)
{
  title = 'test.setDefaultEdgeLineWidth'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultEdgeLineWidth (cwe, 5); redraw (cwe)
    Sys.sleep (1)
    setDefaultEdgeLineWidth (cwe, 0); redraw (cwe)
    Sys.sleep (1)
    }

  setDefaultEdgeLineWidth (cwe, 1); redraw (cwe)

  invisible (cwe)

} # test.setDefaultEdgeLineWidth
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultEdgeColor = function (direct=FALSE)
{
  title = 'test.setDefaultEdgeColor'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultEdgeColor (cwe, '#FFFFFF'); redraw (cwe)
    Sys.sleep (1)
    setDefaultEdgeColor (cwe, '#FF0000'); redraw (cwe)
    Sys.sleep (1)
    } # for i

  setDefaultEdgeColor (cwe, '#000000'); redraw (cwe)

  invisible (cwe)

} # test.setDefaultEdgeColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultEdgeFontSize = function ()
{
  title = 'test.setDefaultEdgeFontSize'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  setNodeSizeDirect (cwe, getAllNodes (cwe), 40)
  setEdgeLabelRule (cwe, 'edgeType')   # gives us some text we can inspect for changing font size (below)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hideAllPanels (cwe)

  for (i in 1:3) {
    setDefaultEdgeFontSize (cwe, i * 15);
    redraw (cwe)
    Sys.sleep (1)
    } # for i

  setDefaultEdgeFontSize (cwe, 12);
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultEdgeFontSize
#------------------------------------------------------------------------------------------------------------------------
test.setNodeLabelRule = function ()
{
  title = 'test.setNodeLabelRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setNodeLabelRule (cwe, 'label')
  Sys.sleep (1)
  setNodeLabelRule (cwe, 'type')
  Sys.sleep (1)
  setNodeLabelRule (cwe, 'lfc')
  Sys.sleep (1)
  setNodeLabelRule (cwe, 'count')
  Sys.sleep (1)
  setNodeLabelRule (cwe, 'label')
  msg (cwe, 'test.setNodeLabelRule')

  invisible (cwe)

}  # test.setNodeLabelRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLabelRule = function ()
{
  title = 'test.setEdgeLabelRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setEdgeLabelRule (cwe, 'edgeType')
  Sys.sleep (1)
  setEdgeLabelRule (cwe, 'score')
  Sys.sleep (1)
  setEdgeLabelRule (cwe, 'canonicalName')
  msg (cwe, 'test.setEdgeLabelRule')

  invisible (cwe)

}  # test.setEdgeLabelRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeTooltipRule = function ()
{
  title = 'test.setNodeTooltipRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  #setNodeLabelRule (cwe, 'label')
  setNodeTooltipRule (cwe, 'type')
  #setNodeLabelRule (cwe, 'lfc')
  #setNodeLabelRule (cwe, 'count')
  #setNodeLabelRule (cwe, 'label')
  msg (cwe, 'test.setNodeTooltipRule')

  invisible (cwe)

}  # test.setNodeTooltipRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTooltipRule = function ()
{
  title = 'test.setEdgeTooltipRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setEdgeTooltipRule (cwe, 'edgeType')
  msg (cwe, 'test.setEdgeTooltipRule')

  invisible (cwe)

}  # test.setEdgeTooltipRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeColorRule = function ()
{
  title = 'test.setNodeColorRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');

    # first, specify a mode='interpolate' rule -- the default
  node.attribute.values = c (-3.0, 0.0, 3.0)
  node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
  setNodeColorRule (cwe, 'lfc', node.attribute.values, node.colors, mode='interpolate')
  Sys.sleep (1)

    # now, a lookup rule
  node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
  node.colors =           c ('#8888FF', '#00F088',              "#00CCCC")
  setNodeColorRule (cwe, 'type', node.attribute.values, node.colors, mode='lookup')
  Sys.sleep (1)

    # now, a lookup rule with an incomplete lookup table:  does the default.color argument work?  cy2.7 bug -- not yet.
    # instead, the node is painted the cytoscape default color, pale red
  node.attribute.values = c ("kinase",  "transcription factor")
  node.colors =           c ('#8888FF', '#00F088')
  setNodeColorRule (cwe, 'type', node.attribute.values, node.colors, mode='lookup', default.color='#AA33AA')
  msg (cwe, 'test.setNodeColorRule')

    # now, use 1 element lists.
  node.attribute.values = c ("kinase")
  node.colors =           c ('#FFFFFF')
  setNodeColorRule (cwe, 'type', node.attribute.values, node.colors, mode='lookup', default.color='#AA33AA')
  msg (cwe, 'test.setNodeColorRule')
  redraw (cwe)

  invisible (cwe)

} # test.setNodeColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeBorderColorRule = function ()
{
  title = 'test.setNodeBorderColorRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');

    # set the stage by making all the nodes white, to provide better contrast for the node border colors
  node.attribute.values = c (-3.0, 0.0, 3.0)
  colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF')
  setNodeColorRule (cwe, 'lfc', node.attribute.values, colors, mode='interpolate')

    # first, specify a mode='interpolate' rule -- the default
  node.attribute.values = c (-3.0, 0.0, 3.0)
  colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
  setNodeBorderColorRule (cwe, 'lfc', node.attribute.values, colors, mode='interpolate')
  Sys.sleep (1)

    # now, a lookup rule.  bright red, green and blue borders
  node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
  colors =                c ('#FF0000', '#00FF00',              "#0000FF")
  setNodeBorderColorRule (cwe, 'type', node.attribute.values, colors, mode='lookup')
  Sys.sleep (1)

    # now, a lookup rule with an incomplete lookup table:  does the default.color argument work?  cy2.7 bug -- not yet.
    #  the glycoprotein node, 'Gene C', should have a white border around white fill
  node.attribute.values = c ("kinase",  "transcription factor")
  colors =                c ('#0000FF', '#FF0000')
  setNodeBorderColorRule (cwe, 'type', node.attribute.values, colors, mode='lookup', default.color='#FFFFFF')

    # now, one element lists
  node.attribute.values = c ("transcription factor")
  colors =                c ('#FF00FF')
  setNodeBorderColorRule (cwe, 'type', node.attribute.values, colors, mode='lookup', default.color='#FFFFFF')

  msg (cwe, 'test.setNodeBorderColorRule')
 
  invisible (cwe)

} # test.setNodeBorderColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeBorderWidthRule = function ()
{
  title  = 'test.setNodeBorderWidthRule'
  window.prep (title)
  cy = CytoscapeConnection ()
  
  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  #hideAllPanels (cy)

    # set the stage by making all the nodes white, to provide better contrast for the node border colors
  node.attribute.values = c (-3.0, 0.0, 3.0)
  colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF')
  setNodeColorRule (cwe, 'lfc', node.attribute.values, colors, mode='interpolate')
  setDefaultNodeBorderColor (cwe, '#FF0000')

  for (i in 1:3) {
       # 3 different node border sizes
     node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
     border.widths =         c (0, 10, 20)
     setNodeBorderWidthRule (cwe, 'type', node.attribute.values, border.widths)
       # swap them around different node border sizes
     node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
     border.widths =         c (20, 0, 10);
     setNodeBorderWidthRule (cwe, 'type', node.attribute.values, border.widths)
     } # for i   

  invisible (cwe)

} # test.setNodeBorderWidthRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeSizeRule = function ()
{
  title = 'test.setNodeSizeRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');

    # first, create a simple 2-point rule, with 'below' and 'above' values strong enough to see that they are working
    # recall that makeSimpleGraph creates count attributes like this:
    # noa (getGraph (cwe), 'count')     #   A.A   B.B   C.C 
    #                                       "2"  "30" "100" 

  count.control.points = c (20,  40)
  node.sizes           = c (1, 80,  120, 300)
  setNodeSizeRule (cwe, 'count', count.control.points, node.sizes, mode='interpolate')
  system ('sleep 2')

    # now chop off the below & above values.  A should grow to 80, almost as big as B, and C should shrink to 120, larger that B

  count.control.points = c (20,  40)
  node.sizes           = c (80,  120)
  setNodeSizeRule (cwe, 'count', count.control.points, node.sizes, mode='interpolate')
  system ('sleep 2')

    # now use a mode='lookup' rule.  specify two sizes, look to see that the third type, glycoprotein, gets the tiny small size
  molecule.types = c ('kinase', 'transcription factor')
  node.sizes     = c (60,        80)
  setNodeSizeRule (cwe, 'type', molecule.types,  node.sizes, default.size= 5, mode='lookup')
  redraw (cwe)

  invisible (cwe)

} # test.setNodeSizeRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeShapeRule = function ()
{
  title = 'test.setNodeShapeRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

     # specify shapes for only two of the three nodes and node types.  make sure that the third node gets
     # the default shape
  
     # make rule for 2 of 3 node types, leaving the third as the default
  node.shapes = c ('diamond', 'triangle')
  attribute.values = c ('kinase', 'glycoprotein')
  setNodeShapeRule (cwe, node.attribute.name='type', attribute.values, node.shapes, default.shape='ellipse')

     # test one-element lists
  node.shapes = c ('diamond')
  attribute.values = c ('glycoprotein')
  setNodeShapeRule (cwe, node.attribute.name='type', attribute.values, node.shapes, default.shape='ellipse')

  msg (cwe, 'test.setNodeShapeRule')

  invisible (cwe)

} # test.setNodeShapeRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeOpacityRule = function ()
{
  title = 'test.setNodeOpacityRule'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)

     # make the node borders prominent
  setDefaultNodeBorderColor (cw, '#FFFF00')
  setDefaultNodeBorderWidth (cw, 10)

  lfc.values = c (-3.0, 0, 3.0)

    # make the nodes big, give them strong colors
  setNodeSizeDirect (cw, nodes (cw@graph), 100)
  setNodeColorRule (cw, 'lfc', lfc.values, c ('#FF0000', '#00FF00', '#0000FF'), mode='interpolate'); redraw (cw)
  layoutNetwork (cw, 'grid')

    # first, the continuous 'interpolate' case, in which opacity is a function of lfc
  opacities = c (10, 128, 255)
  x <- cw
  setNodeOpacityRule (cw, node.attribute.name='lfc', lfc.values, opacities, mode='interpolate')
  redraw (cw)

     # reset
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate', aspect='all'); 
  redraw (cw)

    # now try a few of the aspect-specific rules, still in interpolate mode
    # border:
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate', aspect='border'); 
  redraw (cw)
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (40,128, 0), mode='interpolate', aspect='border'); 
  redraw (cw)

     # reset
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate');   redraw (cw)

    # label
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (40,128, 0), mode='interpolate', aspect='border'); 
  redraw (cw)

     # reset
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (255, 255, 255), mode='interpolate');   redraw (cw)

    # border
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (40,128, 0), mode='interpolate', aspect='border'); 
  redraw (cw)

    # a mix...
  setNodeOpacityRule (cw, 'lfc', lfc.values, c (128, 128, 128), mode='interpolate', aspect='border, label, fill'); 
  redraw (cw)


  scalar.values = as.character (noa (cw@graph, 'type'))
     # reset
  setNodeOpacityRule (cw, 'type', scalar.values, c (255, 255, 255), mode='lookup');   redraw (cw)

    # label
  setNodeOpacityRule (cw, 'type', scalar.values, c (40,128, 0), mode='lookup', aspect='border'); 
  redraw (cw)

     # reset
  setNodeOpacityRule (cw, 'type', scalar.values, c (255, 255, 255), mode='lookup');   redraw (cw)

    # border
  setNodeOpacityRule (cw, 'type', scalar.values, c (40,128, 0), mode='lookup', aspect='border'); 
  redraw (cw)

    # a mix...
  setNodeOpacityRule (cw, 'type', scalar.values, c (128, 128, 128), mode='lookup', aspect='border, label, fill'); 
  redraw (cw)

    # make everything except labels transparent
  setNodeOpacityRule (cw, 'type', scalar.values, c (0, 0, 0), mode='lookup', aspect='border, fill'); 
  setNodeOpacityRule (cw, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='label')
  redraw (cw)

    # make everything except borders transparent
  setNodeOpacityRule (cw, 'type', scalar.values, c (0, 0, 0), mode='lookup', aspect='label, fill'); 
  setNodeOpacityRule (cw, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='border')
  redraw (cw)

    # make everything except fill transparent
  setNodeOpacityRule (cw, 'type', scalar.values, c (0, 0, 0), mode='lookup', aspect='label, border'); 
  setNodeOpacityRule (cw, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='fill')
  redraw (cw)

    # now restore everything
  setNodeOpacityRule (cw, 'type', scalar.values, c (255, 255, 255), mode='lookup', aspect='all')
  redraw (cw)

  invisible (cw)

} # test.setNodeOpacityRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeColorDirect = function ()
{
  DEACTIVATED("too slow")
  title = 'test.setNodeColorDirect'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  setNodeColorDirect (cw, 'A', '#AA0088')
  Sys.sleep (1)
  setNodeColorDirect (cw, 'A', '#AA4488')
  Sys.sleep (1)
  setNodeColorDirect (cw, 'A', '#AA8888')
  Sys.sleep (1)

  setNodeColorDirect (cw, c ('A', 'B'), '#448844')

  invisible (cw)

} # test.setNodeColorirect
#------------------------------------------------------------------------------------------------------------------------
test.setNodeBorderColorDirect = function ()
{
  DEACTIVATED("too slow")
  title = 'test.setNodeBorderColorDirect'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  setNodeBorderColorDirect (cw, 'A', '#AA0088')
  Sys.sleep (1)
  setNodeBorderColorDirect (cw, 'A', '#AA4488')
  Sys.sleep (1)
  setNodeBorderColorDirect (cw, 'A', '#AA8888')
  Sys.sleep (1)

  setNodeBorderColorDirect (cw, c ('A', 'B'), '#448844')

  invisible (cw)

} # test.setNodeBorderColorDirect 
#------------------------------------------------------------------------------------------------------------------------
test.setNodeLabelDirect = function ()
{
  DEACTIVATED("too slow")
  title = 'test.setNodeLabelDirect'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  setNodeLabelDirect (cw, 'A', 'new A label')
  redraw (cw)
  Sys.sleep (1)
    # try multiple nodes, one label, which RCy will replicate into the right number
  setNodeLabelDirect (cw, nodes (cw@graph), '')
  redraw (cw)
  Sys.sleep (1)

  setNodeLabelDirect (cw, c ('A', 'C'), c ('AzA', 'ByB'))
  redraw (cw)

  invisible (cw)

} # test.setNodeLabelDirect
#------------------------------------------------------------------------------------------------------------------------
test.setNodeLabelPropertiesDirect = function ()
{  
  DEACTIVATED("too slow")
  print ('--- test.setNodeLabelsPropertiesDirect')
  title = 'test.setNodeLabelPropertiesDirect'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  sizes = c (10, 50, 80)
  colors = c ('#0000FF', '#00FF00', '#FF0000')
  for (i in 1:length (sizes)) { 
    setNodeFontSizeDirect (cw, 'A', sizes [i])
    setNodeLabelColorDirect (cw, 'A', colors [i])
    redraw (cw)
    Sys.sleep (1)
    } # for i

  invisible (cw)

} # test.setNodeLabelsPropertiesDirect 
#------------------------------------------------------------------------------------------------------------------------
test.setNodeOpacityDirect = function ()
{
  DEACTIVATED("too slow")
  title = 'test.setNodeOpacityDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  g = addNode ('D', g)
  nodeData (g, 'D', 'label') = 'blink'
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  setNodeSizeDirect (cw, 'D', 120)
  layoutNetwork (cw, 'grid')
  fitContent (cw)
  setZoom (cw, 0.8 * getZoom (cw))
  redraw (cw)

  setNodeFillOpacityDirect (cw, 'A', 0); redraw (cw);
  setNodeLabelOpacityDirect (cw, 'B', 0); redraw (cw);
  setNodeBorderOpacityDirect (cw, 'C', 0); redraw (cw);
  for (i in 1:3) {
    setNodeOpacityDirect (cw, 'D', 0); redraw (cw);
    #Sys.sleep (1)
    setNodeOpacityDirect (cw, 'D', 255); redraw (cw);
    #Sys.sleep (1)
    } # for i

  setNodeOpacityDirect (cw, c ('A', 'C'), 255); redraw (cw)
  setNodeOpacityDirect (cw, c ('B', 'D'), 50); redraw (cw)
  setNodeOpacityDirect (cw, c ('A', 'B', 'C', 'D'), c (10, 50, 100, 200)); redraw (cw)
  setNodeOpacityDirect (cw, c ('A', 'B', 'C', 'D'), c (200, 100, 50, 10)); redraw (cw)
  Sys.sleep (1)

  setNodeOpacityDirect (cw, c ('A', 'B', 'C', 'D'), 255); redraw (cw)

  invisible (cw)

} # test.setNodeOpacityDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeOpacityDirect = function ()
{
    DEACTIVATED("too slow for some reason")
    title = 'test.setEdgeOpacityDirect'
    window.prep (title)
    
    g = RCy3::makeSimpleGraph ()
    cw = CytoscapeWindow (title, graph=g)
    setDefaultEdgeLineWidth (cw, 10)
    displayGraph (cw)
    layoutNetwork (cw, 'grid')
    redraw (cw)
    
    edge.names = cy2.edge.names (g)
    
    setEdgeOpacityDirect (cw, edge.names [1],  80); 
    setEdgeOpacityDirect (cw, edge.names [2],  0); 
    setEdgeOpacityDirect (cw, edge.names [3],  255); 
    redraw (cw);
    
    setEdgeOpacityDirect (cw, edge.names [2],  80); 
    setEdgeOpacityDirect (cw, edge.names [3],  0); 
    setEdgeOpacityDirect (cw, edge.names [1],  255); 
    redraw (cw);
    
    setEdgeOpacityDirect (cw, edge.names [1],  80); 
    setEdgeOpacityDirect (cw, edge.names [3],  40); 
    setEdgeOpacityDirect (cw, edge.names [2],  255); 
    redraw (cw);
    
    setEdgeOpacityDirect (cw, edge.names [1],  0); 
    setEdgeOpacityDirect (cw, edge.names [3],  0); 
    setEdgeOpacityDirect (cw, edge.names [2],  0); 
    redraw (cw);
    
    setEdgeOpacityDirect (cw, edge.names [1],  255); 
    setEdgeOpacityDirect (cw, edge.names [3],  255); 
    setEdgeOpacityDirect (cw, edge.names [2],  255); 
    redraw (cw);
    
    
    setEdgeOpacityDirect (cw, edge.names, 0); redraw (cw)
    setEdgeOpacityDirect (cw, edge.names, 255); redraw (cw)
    
    
    setEdgeOpacityDirect (cw, edge.names, c (0, 128, 255)); redraw (cw)
    setEdgeOpacityDirect (cw, edge.names, c (255, 0, 128)); redraw (cw)

    
    setEdgeOpacityDirect (cw, edge.names, 255); redraw (cw)
    
    invisible (cw)

} # test.setEdgeOpacityDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeColorDirect = function ()
{
  DEACTIVATED("very slow for some reason")
  title = 'test.setEdgeColorDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edge.of.interest = as.character (cy2.edge.names (g) [1])
  for (i in 1:5) {
    setEdgeColorDirect (cw, edge.of.interest, '#FF0000'); redraw (cw);
    Sys.sleep (1)
    setEdgeColorDirect (cw, edge.of.interest, '#00FF00'); redraw (cw);
    Sys.sleep (1)
    setEdgeColorDirect (cw, edge.of.interest, '#0000FF'); redraw (cw);
    Sys.sleep (1)
    } # for i

  invisible (cw)

} # test.setEdgeColorDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowShapeDirect = function ()
{
  title = 'test.setEdgeSourceArrowShapeDirect'
  window.prep (title)

  cw = CytoscapeWindow ('setEdgeSourceArrowShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)
  setWindowSize (cw, 800, 800)
  fitContent (cw)

  edges.of.interest = as.character (cy2.edge.names (g))
  supported.arrow.shapes = getArrowShapes (cw)

    # first try passing three edges and three arrow shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [2:5])
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one arrow.shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [6])
  redraw (cw)

    # now loop through all of the arrow.shapes

  for (shape in supported.arrow.shapes) {
    setEdgeSourceArrowShapeDirect (cw, edges.of.interest, shape)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, 'No Arrow')
  redraw (cw)
  invisible (cw)

} # test.setEdgeSourceArrowShapeDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLabelDirect = function ()
{
  title = 'test.setEdgeLabelDirect '
  window.prep (title)

  cw = CytoscapeWindow ('setEdgeLabelDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw, 'grid')
  edge.names = cy2.edge.names (cw@graph)[1:2]
  for (i in 1:10) {
    setEdgeLabelDirect (cw, edge.names, 255 - (i * 25))
    redraw (cw)
    }
  for (i in 1:10) {
    setEdgeLabelDirect (cw, edge.names, i * 25)
    redraw (cw)
    }

  invisible (cw)

} # test.setEdgeLabelDirect
#------------------------------------------------------------------------------------------------------------------------
#test.setEdgeFontFaceDirect = function ()
#{
#  title = 'test.setEdgeFontFaceDirect'
#  window.prep (title)
#
#  g = RCy3::makeSimpleGraph ()
#  cw = CytoscapeWindow (title, graph=g)
#  displayGraph (cw)
#  layoutNetwork (cw, 'grid')
#  redraw (cw)
#
#  edge.of.interest = cy2.edge.names (g) [1]
#  fonts = c ('courier', 'arial')
#  for (font in fonts) {
#    setEdgeFontFaceDirect (cw, edge.of.interest, font); redraw (cw);
#    Sys.sleep (1)
#    } # for i
#
#} # test.
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeFontSizeDirect = function ()
{
  DEACTIVATED("too slow for some reason.")
  title = 'test.setEdgeFontSizeDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edge.of.interest = cy2.edge.names (g) [1]
  for (i in 1:5) {
    setEdgeOpacityDirect (cw, edge.of.interest, i * 30); redraw (cw);
    Sys.sleep (1)
    } # for i

} # test.
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLabelColorDirect = function ()
{
  DEACTIVATED("too slow for some reason")
  title = 'test.setEdgeLabelColorDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edge.of.interest = cy2.edge.names (g) [1]
  for (i in 1:5) {
    setEdgeOpacityDirect (cw, edge.of.interest, i * 30); redraw (cw);
    Sys.sleep (1)
    } # for i

} # test.
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTooltipDirect = function ()
{
  title = 'setEdgeTooltipDirect.test'
  window.prep (title)

  cw <- CytoscapeWindow (title, graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (cw@graph))

    # first try passing three edges and three tooltips
  setEdgeTooltipDirect (cw, edges.of.interest, c ('tooltip #1', 'tooltip #2', 'tooltip #3'))
  redraw (cw)

    # now try passing three edges and one tooltip
  setEdgeTooltipDirect (cw, edges.of.interest [1:2], 'a general purpose tooltip')
  redraw (cw)

  invisible (cw)

} # test.setEdgeTooltipDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineWidthDirect = function ()
{
  title = 'test.setEdgeLineWidthDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edges.of.interest = cy2.edge.names (g) [1:2]

  for (i in 1:10) {
    setEdgeLineWidthDirect (cw, edges.of.interest, i)
    redraw (cw)
    }

  setEdgeLineWidthDirect (cw, edges.of.interest, 1)
  redraw (cw)

} # test.setEdgeLineWidthDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineStyleDirect = function ()
{
  title = 'test.setEdgeLineStyleDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (g))

  supported.styles = getLineStyles (cw)

    # first try passing three edges and three styles
  setEdgeLineStyleDirect (cw, edges.of.interest, supported.styles [5:7])
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one styles
  setEdgeLineStyleDirect (cw, edges.of.interest, supported.styles [8])
  redraw (cw)

    # now loop through all of the styles

  for (style in supported.styles) {
    setEdgeLineStyleDirect (cw, edges.of.interest, style)
    Sys.sleep (1)
    redraw (cw)
    }

  setEdgeLineStyleDirect (cw, edges.of.interest, 'SOLID')
  redraw (cw)

  invisible (cw)

} # test.setEdgeLineStyleDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowShapeDirect = function ()
{
  title = 'test.setEdgeSourceArrowShapeDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)
  setWindowSize (cw, 800, 800)
  fitContent (cw)

  edges.of.interest = as.character (cy2.edge.names (g))
  supported.arrow.shapes = getArrowShapes (cw)

    # first try passing three edges and three arrow.shapes
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [5:7])
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one arrow shape
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [8])
  redraw (cw)

    # now loop through all of the arrow.shapes

  for (shape in supported.arrow.shapes) {
    setEdgeSourceArrowShapeDirect (cw, edges.of.interest, shape)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, 'No Arrow')
  redraw (cw)

  invisible (cw)

} # test.setEdgeSourceArrowShapeDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTargetArrowShapeDirect = function ()
{
  title = 'test.setEdgeTargetArrowShapeDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (g))
  supported.arrow.shapes = getArrowShapes (cw)

    # first try passing three edges and three arrow.shapes
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [5:7])
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and one arrow shape
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, supported.arrow.shapes [8])
  redraw (cw)

    # now loop through all of the arrow.shapes

  for (shape in supported.arrow.shapes) {
    setEdgeTargetArrowShapeDirect (cw, edges.of.interest, shape)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, 'No Arrow')
  redraw (cw)

  invisible (cw)

} # test.setTargetArrowShapeDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowColorDirect = function ()
{
  title = 'test.setEdgeSourceArrowColorDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  arrows = c ('Arrow', 'Diamond', 'Circle')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, arrows)
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)

  colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
  colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")

  edge.names = as.character (cy2.edge.names (g) [1:3])

  for (i in 1:2) {
    setEdgeSourceArrowColorDirect (cw, edge.names, colors.1)
    redraw (cw)
    Sys.sleep (1)
    setEdgeSourceArrowColorDirect (cw, edge.names, colors.2)
    redraw (cw)
    Sys.sleep (1)
    } # for i

  invisible (cw)

} # test.setEdgeSourceArrowColorDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTargetArrowColorDirect = function ()
{
  title = 'test.setEdgeTargetArrowColorDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)
  setWindowSize (cw, 800, 800)
  fitContent (cw)

  arrows = c ('Arrow', 'Diamond', 'Circle')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, arrows)
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, arrows)

  colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
  colors.2 = c ("#AA00AA", "#00AAAA", "#0000AA")

  edge.names = as.character (cy2.edge.names (g) [1:3])

  for (i in 1:2) {
    setEdgeTargetArrowColorDirect (cw, edge.names, colors.1)
    redraw (cw)
    Sys.sleep (1)
    setEdgeTargetArrowColorDirect (cw, edge.names, colors.2)
    redraw (cw)
    Sys.sleep (1)
    } # for i

  invisible (cw)

} # test.setEdgeTargetArrowColorDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLabelOpacityDirect = function ()
{
  DEACTIVATED("too slow for some reason")
  title = 'test.setEdgeLabelOpacityDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edge.of.interest = cy2.edge.names (g) [1]
  for (i in 1:5) {
    setEdgeOpacityDirect (cw, edge.of.interest, i * 30); redraw (cw);
    Sys.sleep (1)
    } # for i

} # test.
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowOpacityDirect = function ()
{
  title = 'test.setEdgeSourceArrowOpacityDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (g))

     # make sure the source arrows are visible
  setEdgeSourceArrowShapeDirect (cw, edges.of.interest, 'Circle')

    # first try passing three edges and three arrow opacity values
  setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, c (64, 128, 255))
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and just one opacity value; it will be applied to all arrows
  setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, 32)
  redraw (cw)

    # now loop through all of the arrow.opacitys

  for (opacity in seq (0, 255, by=45)) {
    setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, opacity)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeSourceArrowOpacityDirect (cw, edges.of.interest, 255)
  redraw (cw)

} # test.setEdgeSourceArrowOpacityDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTargetArrowOpacityDirect = function ()
{
  title = 'test.setEdgeTargetArrowOpacityDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edges.of.interest = as.character (cy2.edge.names (g))

     # make sure the target arrows are visible
  setEdgeTargetArrowShapeDirect (cw, edges.of.interest, 'Circle')

    # first try passing three edges and three arrow opacity values
  setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, c (64, 128, 255))
  redraw (cw)

  Sys.sleep (1)
  
    # now try passing three edges and just one opacity value; it will be applied to all arrows
  setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, 32)
  redraw (cw)

    # now loop through all of the arrow.opacitys

  for (opacity in seq (0, 255, by=45)) {
    setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, opacity)
    Sys.sleep (1)
    redraw (cw)
    }

    # restore the default
  setEdgeTargetArrowOpacityDirect (cw, edges.of.interest, 255)
  redraw (cw)

} # test.setEdgeTargetArrowOpacityDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLabelPositionDirect = function ()
{
  DEACTIVATED("too slow for some reason")
  title = 'test.setEdgeLabelPositionDirect'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  edge.of.interest = cy2.edge.names (g) [1]
  for (i in 1:5) {
    setEdgeOpacityDirect (cw, edge.of.interest, i * 30); redraw (cw);
    Sys.sleep (1)
    } # for i

} # test.
#------------------------------------------------------------------------------------------------------------------------
#test.setEdgeLabelWidthDirect = function ()
#{
#     title = 'test.setEdgeLabelWidthDirect',
#     window.prep (title),
#   ,
#     g = RCy3::makeSimpleGraph (),
#  cw = CytoscapeWindow (title, graph=g)
#  displayGraph (cw)
#  layoutNetwork (cw, 'grid')
#  redraw (cw)
#
#  edge.of.interest = cy2.edge.names (g) [1]
#  for (i in 1:5) {
#    setEdgeOpacityDirect (cw, edge.of.interest, i * 30); redraw (cw);
#    Sys.sleep (1)
#    } # for i
#
#} # test.
#------------------------------------------------------------------------------------------------------------------------
test.countNodes = function ()
{
  title = 'test.countNodes'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  checkEquals (getNodeCount (cwe), length (nodes (getGraph (cwe))))

  invisible (cwe)

} # test.countNodes
#------------------------------------------------------------------------------------------------------------------------
test.countEdges = function ()
{
  title = 'test.countEdges'
  window.prep (title)

  cy = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cy)))
     deleteWindow (cy, title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  checkEquals (getEdgeCount (cwe), length (edgeNames (getGraph (cwe))))

  invisible (cwe)

} # test.countNodes
#------------------------------------------------------------------------------------------------------------------------
test.countNodesAndEdgesInEmptyGraph = function ()
{
  title = 'test.countNodesAndEdgesInEmptyGraph'
  window.prep (title)

  g.empty = new ("graphNEL", edgemode = "directed")
  checkEquals (length (nodes (g.empty)), 0)
  checkEquals (length (edges (g.empty)), 0)

  cwe = CytoscapeWindow (title, graph=g.empty)  # default behavior, but let's make it explicit
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  checkEquals (getNodeCount (cwe), 0)
  checkEquals (getEdgeCount (cwe), 0)

  invisible (cwe)

} # test.countNodesAndEdgesInEmptyGraph 
#------------------------------------------------------------------------------------------------------------------------
test.getAllNodes = function ()
{
  title = 'test.getAllNodes'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  cwe.nodes = getAllNodes (cwe)
  checkEquals (length (intersect (cwe.nodes, nodes (cwe@graph))), 3)

  msg (cwe, 'test.getAllNodes')

  invisible (cwe)

} # test.getAllNodes
#------------------------------------------------------------------------------------------------------------------------
test.getAllEdges = function ()
{
  title = 'test.getAllEdges'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  cwe.edges = getAllEdges(cwe)
  checkTrue ("C (undefined) A" %in% cwe.edges)
  checkTrue ("B (synthetic lethal) C" %in% cwe.edges)
  checkTrue ("A (phosphorylates) B" %in% cwe.edges)

  msg (cwe, 'test.getAllEdges')

  invisible (cwe)

} # test.getAllEdges
#------------------------------------------------------------------------------------------------------------------------
test.selectNodes = function ()
{
  title = 'test.selectNodes'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'), preserve=T)
  checkEquals (getSelectedNodeCount (cwe), 2)

  cwe.nodes = selectNodes (cwe, 'C', preserve=T)
  checkEquals (getSelectedNodeCount (cwe), 3)

  clearSelection (cwe)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'), preserve=TRUE)
  checkEquals (getSelectedNodeCount (cwe), 2)
  cwe.nodes = selectNodes (cwe, 'C', preserve=FALSE)
  checkEquals (getSelectedNodeCount (cwe), 1)
  checkEquals (getSelectedNodes (cwe), 'C')

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  nodes.to.select = c ('bogus', 'missing')
  selectNodes (cwe, nodes.to.select)
  checkEquals (getSelectedNodeCount (cwe), 0)
  nodes.to.select = c (nodes.to.select, nodes (cwe@graph))
  selectNodes (cwe, nodes.to.select)
  checkEquals (getSelectedNodeCount (cwe), 3)
  msg (cwe, 'test.selectNodes')

  msg (cwe, 'test.selectNodes')

  invisible (cwe)

} # test.selectNodes
#------------------------------------------------------------------------------------------------------------------------
test.nodeNeighborReportingAndSelection = function ()
{
  title = 'test.nodeNeighborReportingAndSelection'
  window.prep (title)

   # create a circular graph
  LETTERS = toupper (letters)
  source.nodes  <- LETTERS [1:26]
  target.nodes  <- c (LETTERS [2:26], LETTERS [1])
  weights <- runif (length (letters))
  df <- data.frame (from=source.nodes, to=target.nodes, weight=weights)
  g.bam <- graphBAM (df, edgemode='directed')
  g.bam <- initEdgeAttribute (g.bam, 'weight', 'numeric', 0.0)

  cw = CytoscapeWindow (title, graph=g.bam)
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

    # paint the edges shades of green as function of weight
  setDefaultEdgeLineWidth (cw, 5)
  setEdgeColorRule (cw, 'weight',  c (0, 1), c ('#FFFFFF', '#00FF00'),  mode='interpolate')

    # select M, then its immediate neighbors
  checkEquals (getSelectedNodeCount (cw), 0)
  checkEquals (sort (getFirstNeighbors (cw, 'M')), c ('L', 'N'))
  selectNodes (cw, 'M')
  checkEquals (getSelectedNodeCount (cw), 1)
  selectFirstNeighborsOfSelectedNodes (cw)
  checkEquals (getSelectedNodeCount (cw), 3)
  checkEquals (sort (getSelectedNodes (cw)), c ('L', 'M', 'N'))
  sfn (cw)
  checkEquals (getSelectedNodeCount (cw), 5)
  nodes = sort (getSelectedNodes (cw))
  checkEquals (nodes, c ("K", "L", "M", "N", "O"))
  invisible (cw)

} # test.nodeNeighborReportingAndSelection
#------------------------------------------------------------------------------------------------------------------------
test.invertSelection = function ()
{
  title = 'test.invertSelection'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)

  for (i in 1:5) {
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 1)
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 2)
    } # for i

  
  #deleteSelectedNodes ()
  clearSelection (cwe)

  invisible (cwe)

} # test.invertSelection 
#------------------------------------------------------------------------------------------------------------------------
test.deleteSelectedNodes = function ()
{
  title = 'test.deleteSelectedNodes'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)

  for (i in 1:5) {
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 1)
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 2)
    } # for i

  invisible (cwe)

} # test.invertNodeSelection 
#------------------------------------------------------------------------------------------------------------------------
# reveals unexpected behavior of 'unhideAll':  nodes & edges from other unknown places are 'unhidden' as well (pshannon: 07 jan 2011)
test.hideNodes = function ()
{
  title = 'test.hideNodes'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)
  checkEquals (getNodeCount (cwe), 3)
  hideSelectedNodes (cwe)
  checkEquals (getNodeCount (cwe), 1)
  unhideAll (cwe)
  layoutNetwork (cwe)
  redraw (cwe)
  #checkEquals (getNodeCount (cwe), 3)
  msg (cwe, 'test.selectNodes')

  invisible (cwe)

} # test.hideNodes
#------------------------------------------------------------------------------------------------------------------------
test.selectEdges = function ()
{
  title = 'test.selectEdges'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  clearSelection (cw)
  checkEquals (getSelectedEdgeCount (cw), 0)
    # not yet possible to select edges through CytoscapeRPCCallHandler
  selectEdges (cw, "A (phosphorylates) B")
  checkEquals (getSelectedEdgeCount (cw), 1)
  Sys.sleep (1)
  clearSelection (cw)
  checkEquals (getSelectedEdgeCount (cw), 0)

  msg (cw, 'test.selectEdges')

  invisible (cw)

} # test.selectEdges
#------------------------------------------------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineStyleRule = function ()
{
  title = 'test.setEdgeLineStyleRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)


  line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 3)

  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)

    # test one-element lists
  line.styles = c ('DOT')
  edgeType.values = c ('synthetic lethal')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 1)
  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)

  msg (cwe, 'test.setEdgeLineStyleRule')

  invisible (cwe)

} # test.setEdgeLineStyleRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineWidthRule = function ()
{
  title = 'test.setEdgeLineWidthRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 3)

  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)
  setEdgeLineWidthRule (cwe, 'edgeType', edgeType.values, c (0, 8, 16))

    # try one-element lists
  setEdgeLineWidthRule (cwe, 'edgeType', edgeType.values [1], 10)
  
  msg (cwe, 'test.setEdgeLineStyleRule')

  invisible (cwe)

} # test.setEdgeLineWidthRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeColorRule = function ()
{
  title = 'test.setEdgeColorRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  colors = c ('#FF0000', '#FFFF00', '#00FF00')
  setEdgeColorRule (cwe, 'edgeType',  edgeType.values, colors, mode='lookup')
  Sys.sleep (1)

  all.white  = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
  setEdgeColorRule (cwe, 'edgeType',  edgeType.values [2], mode='lookup', '#000000')

    # now create a continuous ('interpolate') mode rule, using the score edge attribute
  score.values = c (-15, 0, 40);
  colors = c ('#00FF00', '#FFFFFF', '#FF0000')
  setEdgeColorRule (cwe, 'score',  score.values, colors, mode='interpolate')

    # now swap the colors
  colors = c ('#FF0000', '#000000', '#00FF00')
  setEdgeColorRule (cwe, 'score',  score.values, colors, mode='interpolate')

  msg (cwe, 'test.setEdgeColorRule')

  invisible (cwe)

} # test.setEdgeColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeOpacityRule = function ()
{
  title = 'test.setEdgeOpacityRule'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')

  edgeType.values = c ("phosphorylates", "synthetic lethal", "undefined")

     # want to see edges and both arrows, to check success of opacity rule
  setEdgeTargetArrowRule (cw, 'edgeType', edgeType.values, rep ('ARROW', 3))
  setEdgeSourceArrowRule (cw, 'edgeType', edgeType.values, rep ('ARROW', 3))
  setDefaultEdgeLineWidth (cw, 5)

  redraw (cw)

    # do the lookup rule
  opacities = c (25, 100, 255)
  setEdgeOpacityRule (cw, 'edgeType',  edgeType.values, opacities, mode='lookup')
  redraw (cw)

    # now do the interpolated version
  opacities = c (10, 125, 255)
  control.points = c (-12, 0, 35)
  setEdgeOpacityRule (cw, 'score',  control.points, opacities, mode='interpolate')  
  redraw (cw)

  invisible (cw)

} # test.setEdgeOpacityRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTargetArrowRule = function ()
{
  title = 'test.setEdgeTargetArrowRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  arrows = c ('Delta', 'T', 'Diamond')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 3)

  setEdgeTargetArrowRule (cwe, 'edgeType', edgeType.values, arrows)
  msg (cwe, 'test.setEdgeTargetArrowRule')

    # now test the list-of-length-one call.  the called method will double the list to get past the xmlrpc
    # treatment of lists of length one as scalars, and a failed signature match

  arrows = c ('Circle')
  edgeType.values = c ('phosphorylates')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 1)

  setEdgeTargetArrowRule (cwe, 'edgeType', edgeType.values, arrows)
  msg (cwe, 'test.setEdgeTargetArrowRule')

  invisible (cwe)

} # test.setEdgeTargetArrowRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeArrowColorRules = function ()
{
  title = 'test.setEdgeArrowColorRules'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  #xml.rpc (cwe@uri, 'Cytoscape.discreteMapper', as.character (cwe@window.id), 'default', 'edgeType', 'Edge Target Arrow Color',
  #                  '#FFFFFF', c ("phosphorylates", "synthetic lethal", "undefined"), 
  #                  #c ("#0000AA", "#00AA00", "#AA0000"))
  #                  c ("#AA00AA", "#AAAA00", "#AA0000"))

  #xml.rpc (cwe@uri, 'Cytoscape.createContinuousEdgeVisualStyle', 'edgeType', 'Edge Target Arrow Color',
  #         c (-40, 0, 40), c ('#00FF00', '#FFFFFF', '#FF0000'))

  colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
  colors.2 = c ("#AA00AA", "#AAAA00", "#AA0000")

  setEdgeTargetArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.1)
  setEdgeSourceArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.1)
  system ('sleep 2')
  setEdgeTargetArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.2)
  setEdgeSourceArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.2)

    # test one-element list
  setEdgeSourceArrowColorRule (cwe, 'edgeType', "phosphorylates", '#000000')

  msg (cwe, 'test.setEdgeArrowColorRules')

  invisible (cwe)

} # test.setEdgetArrowColorRules
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowRule = function ()
{
  title = 'test.setEdgeSourceArrowRule'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  arrows = c ('Arrow', 'Diamond', 'Circle')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 3)

  setEdgeSourceArrowRule (cwe, 'edgeType', edgeType.values, arrows)

   # test one-element rule
  setEdgeSourceArrowRule (cwe, 'edgeType', edgeType.values [2], arrows [2])

  msg (cwe, 'test.setEdgeSourceArrowRule')

  invisible (cwe)

} # test.setEdgeSourceArrowRule
#------------------------------------------------------------------------------------------------------------------------
test.movie = function ()
{
  title = 'test.movie'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

    # establish the rules which apply during the full run of the movie
    # different node sizes and node colors are created, not by changing these rules, but
    # by changing node attribute values, for the integer attribute 'count' and the numeric attribute 'lfc'
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cwe, 'count', count.control.points, sizes, mode='interpolate')
  setNodeColorRule (cwe, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')

  count = 3

     # three renderings of the 3-node, 3-edge network are created in this loop, which runs 'count' times
     # the first two set new attributes on the R graph data structure, then ask RCy to send those values
     # to R from the graph
     # the third rendering bypasses storage of new attribute values on the R graph, sending them instead 
     # directly to Cytoscape.  (hence 'setNodeAttributesDirect')
    
  for (i in 1:count) { 
    nodeData (cwe@graph, 'A', 'lfc') = -3.0
    nodeData (cwe@graph, 'B', 'lfc') = -0.7
    nodeData (cwe@graph, 'C', 'lfc') = -1.9
    nodeData (cwe@graph, 'A', 'count') = 10
    nodeData (cwe@graph, 'B', 'count') = 140
    nodeData (cwe@graph, 'C', 'count') = 32
    result = setNodeAttributes (cwe, 'lfc')
    result = setNodeAttributes (cwe, 'count')
    redraw (cwe)

    Sys.sleep (1)
    nodeData (cwe@graph, 'A', 'lfc') = 3.0
    nodeData (cwe@graph, 'B', 'lfc') = 0.7
    nodeData (cwe@graph, 'C', 'lfc') = 1.9
    nodeData (cwe@graph, 'A', 'count') = 50
    nodeData (cwe@graph, 'B', 'count') = 22
    nodeData (cwe@graph, 'C', 'count') = 180
    result = setNodeAttributes (cwe, 'lfc')
    result = setNodeAttributes (cwe, 'count')
    redraw (cwe)
    Sys.sleep (1)

    count.A = round (runif (1, 1, 200))
    count.B = round (runif (1, 1, 200))
    count.C = round (runif (1, 1, 200))

    result = setNodeAttributesDirect (cwe, 'count', 'int', c ('A', 'B', 'C'), c (count.A, count.B, count.C)); 
    result = setNodeAttributesDirect (cwe, 'lfc', 'numeric', c ('A', 'B', 'C'), c (-1.0, 0.0, 1.0))
    redraw (cwe)

    if (i < count) Sys.sleep (1)
    } # for i

  msg (cwe, 'test.movie')

  invisible (cwe)

} # test.movie
#------------------------------------------------------------------------------------------------------------------------
test.unmatchedAttributesError = function ()
{
  title = 'test.unmatchedAttributesError'
  window.prep (title)

  cwe = CytoscapeWindow (title, RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

    # this works
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cwe, 'count', count.control.points, sizes, mode='interpolate')
  redraw (cwe)

    # this should fail gracefully
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cwe, 'count', count.control.points, sizes, mode='interpolate')

  redraw (cwe)
  msg (cwe, 'test.unmatchedAttributesError')

  invisible (cwe)

} # test.unmatchedAttributesError
#------------------------------------------------------------------------------------------------------------------------
#run.tests ()
#------------------------------------------------------------------------------------------------------------------------
#RCy3:::makeRandomGraph ()
#------------------------------------------------------------------------------------------------------------------------
# this tests the otherwise invisible method in RCytocape.R, called to compensate for the extra edges and edge attributes
# packed into an undirected graph
test.remove.redundancies.in.undirected.graph = function ()
{

  title = 'test.remove.redundancies.in.undirected.graph'
  window.prep (title)

     # create a small random graph,
  set.seed (333)
  V = letters [1:4]
  gu = randomEGraph (V, 0.7)

    # add 2 node attributes
  nodeDataDefaults (gu, 'count') = 0
  nodeDataDefaults (gu, 'char') = 'X'


  counts = sample (1:100, length (nodes (gu)))
  chars = sample (letters, length (nodes (gu)))

  for (i in 1: length (nodes (gu))) {
    nodeData (gu, nodes (gu)[i], 'count') = counts [i]
    nodeData (gu, nodes (gu)[i], 'char')  = chars [i]
    } # for i

    # now add an edge attribute, in addition to the 'weight' attribute which randomEGraph supplies
  edgeDataDefaults (gu, 'pmid') = '9999999'
  edge.node.pairs = strsplit (edgeNames (gu), '\\~')
  for (node.pair in edge.node.pairs) {
    source.node = node.pair [1]
    target.node = node.pair [2]
    pmid.fake = 87654321 + sample (1:1000, 1)
    edgeData (gu, source.node, target.node, 'pmid') = as.character (pmid.fake)
    } # for node.pair
    
  gu.fixed = RCy3:::remove.redundancies.in.undirected.graph (gu)
  gu.copy = gu

    # do some basic checks:  nodes?  edges?  count of node & edge attributes?  edge attribute names?  node attribute names? 
  checkEquals (sort (nodes (gu)), sort (nodes (gu.fixed)))
  checkEquals (sort (edgeNames (gu)), sort (edgeNames (gu.fixed)))
  checkEquals (length (edgeDataDefaults (gu)), length (edgeDataDefaults (gu.fixed)))
  checkEquals (length (nodeDataDefaults (gu)), length (nodeDataDefaults (gu.fixed)))
  checkEquals (eda.names (gu), eda.names (gu.fixed))
  checkEquals (noa.names (gu), noa.names (gu.fixed))

    # now check that the default edge attribute values are all the same
  if (length (edgeDataDefaults (gu) > 0)) 
    checkTrue (all (sapply (names (edgeDataDefaults (gu)), function (eda.name) 
                checkEquals (edgeDataDefaults (gu, eda.name), edgeDataDefaults (gu.fixed, eda.name)))))

    # having checked the default eda's above, now check the specific assigned values
  edge.node.pairs = strsplit (edgeNames (gu.fixed), '\\~')
  eda.names = eda.names (gu.fixed)

  for (node.pair in edge.node.pairs) {
    source.node = node.pair [1]
    target.node = node.pair [2]
    for (edge.attribute in eda.names) {
      checkEquals (unlist (edgeData (gu,       source.node, target.node, edge.attribute), use.names=FALSE),
                   unlist (edgeData (gu.fixed, source.node, target.node, edge.attribute), use.names=FALSE))
      } # for each edge.attribute
    } # for edge

     # was all the node data transferred properly?

  if (length (nodeDataDefaults (gu)) > 0) {
    for (node in nodes (gu)) {
      for (node.attribute in noa.names (gu.fixed)) {
        checkEquals (unlist (nodeData (gu,       node, node.attribute), use.names=FALSE),
                     unlist (nodeData (gu.fixed, node, node.attribute), use.names=FALSE))
       } # for node.attribute
     } # for node
   } # if length

  invisible (gu.fixed)
  
} # test.remove.redundancies.in.undirected.graph 
#------------------------------------------------------------------------------------------------------------------------
test.randomUndirectedGraph = function ()
{
  title = 'test.randomUndirectedGraph'
  window.prep (title)

  g.random = RCy3::makeRandomGraph ()
  edgeData (g.random, '1', '2', 'weight') = 0.55
  edgeData (g.random, '1', '2', 'pmid') = '12345678' 

  cwr = CytoscapeWindow (title, g.random)
  displayGraph (cwr)
  layoutNetwork (cwr, 'grid')
  redraw (cwr)

  invisible (cwr)

} # test.randomUndirectedGraph 
#------------------------------------------------------------------------------------------------------------------------
test.simpleGraph = function (apply.viz.rules=TRUE, do.redraw=TRUE)
{
  title = 'test.simpleGraph'
  window.prep (title)

  g.simple = RCy3::makeSimpleGraph ()
  cws = CytoscapeWindow (title, g.simple)

  displayGraph (cws)
  layoutNetwork (cws, 'grid')

  if (apply.viz.rules) {
    setNodeLabelRule (cws, 'label')
    setDefaultNodeBorderWidth (cws, 5)
    node.attribute.values = c ("kinase",  "transcription factor")
    colors =                c ('#A0AA00', '#FF0000')
    setNodeBorderColorRule (cws, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
    count.control.points = c (2, 30, 100)
    sizes                = c (20, 50, 100)
    setNodeSizeRule (cws, 'count', count.control.points, sizes, mode='interpolate')
    setNodeColorRule (cws, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'), mode='interpolate')
    redraw (cws)
    } # if apply.viz.rules


  invisible (cws)

} # test.simpleGraph
#------------------------------------------------------------------------------------------------------------------------
test.simpleGraphWithReciprocalEdge = function ()
{
  title = 'test.simpleGraphWithReciprocalEdge'
  window.prep (title)

  g.simple = RCy3::makeSimpleGraph ()
  g.simple = graph::addEdge ('C', 'B', g.simple)
  edgeData (g.simple, 'C', 'B', attr='edgeType') = 'synthetic rescue'
  edgeData (g.simple, 'C', 'B', attr='score') = 42
  edgeData (g.simple, 'C', 'B', attr='misc') = 'ellany'
  g <- g.simple

  cws = CytoscapeWindow (title, g.simple)
  cws.x <- cws

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
  arrows = c ('Arrow', 'Arrow', 'Arrow', 'None')
  edgeType.values <- c ('phosphorylates', 'synthetic lethal', 'synthetic rescue', 'undefined')
  setEdgeTargetArrowRule (cws, 'edgeType', edgeType.values, arrows)

  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'synthetic rescue', 'undefined')
  edgeColors = c ('#0000AA', '#000000', '#00AA00', '#FFFFFF')
  setEdgeColorRule (cws, 'edgeType',  edgeType.values, edgeColors, mode='lookup')

  redraw (cws)

  msg (cws, title)

  invisible (cws)

} # test.simpleGraphWithReciprocalEdge
#------------------------------------------------------------------------------------------------------------------------
test.setGraph = function ()
{
  title = 'test.setGraph'
  window.prep (title)

  cw = CytoscapeWindow (title)
  checkEquals (length (nodes (getGraph (cw))), 0)
  new.graph = RCy3::makeSimpleGraph ()
  cw = setGraph (cw, new.graph)
  checkEquals (length (nodes (getGraph (cw))), 3)

  msg (cw, 'test.setGraph')

  invisible (cw)

} # test.setGraph 
#------------------------------------------------------------------------------------------------------------------------
test.setNodePosition = function ()
{
  DEACTIVATED("too slow")
  title = 'test.setNodePosition'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)

  layoutNetwork (cwe, 'grid')   # get a reasonable starting layout, with the nodes well-separate

  center.x = 200
  center.y = 200
  radius = 200
  angles = rep (seq (0, 360, 5), 3)  # sweep through full revoltion 3 times, 5 degrees at a time
    # move just the A node, swinging it around the 'center' at 200, 200.  
    # it would be nice not know more about the coordinate system than I now do, perhaps to
    # query current position on any node
  for (angle in angles) {
    angle.in.radians = angle * pi / 180
    x = center.x + (radius * cos (angle.in.radians))
    y = center.y + (radius * sin (angle.in.radians))
    setNodePosition (cwe, 'A', x, y)
    }

  invisible (cwe)

} # test.setNodePosition
#------------------------------------------------------------------------------------------------------------------------
test.getNodePosition = function ()
{
  title = 'test.getNodePosition'
  window.prep (title)

  cwe = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  xx <- cwe
  
  layoutNetwork (cwe, 'grid')   # get a reasonable starting layout, with the nodes well-separate

     # the scheme:  get current positions, find their mean, place all the nodes there,
     # get their new positions, check to see that they are the means just set.
  
  positions <- getNodePosition (cwe, c ('A', 'B', 'C'))

     # place the nodes on top of each other, at the center of their 3-cornered original layout

  center.x = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$x)))))
  center.y = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$y)))))

  setNodePosition (cwe, c ('A', 'B', 'C'), rep (center.x, 3), rep (center.y, 3))
  current.x = getNodePosition (cwe, 'A')[[1]]$x
  current.y = getNodePosition (cwe, 'A')[[1]]$y
  #printf ('center:  %d  %d', center.x, center.y)
  #printf ('current: %d  %d', current.x, current.y)
  
  checkEqualsNumeric (current.x, center.x, tol=1)
  checkEqualsNumeric (current.y, center.y, tol=1)

  invisible (cwe)

} # test.getNodePosition
#------------------------------------------------------------------------------------------------------------------------
# until now, the encoding trick for returning node positions from RCytoscape has been to separate node name from x,y by ':'
#   "2022:417.0,122.0" "659:156.0,0.0"
# 
test.getNodePosition.colonInNodeName = function ()
{
  DEACTIVATED("not fatally slow, but i am impatient. Reactivate later.")
  title = 'test.getNodePosition.colonInNodeName'
  window.prep (title)

  g = RCy3::makeSimpleGraph ()
  funky.node.name = 'abcd:xyz::1234,funky?!'
  g = graph::addNode (funky.node.name, g)
  nodeData (g, funky.node.name, 'label') = funky.node.name

  cwe = CytoscapeWindow (title, graph=g)
  displayGraph (cwe)
  layoutNetwork (cwe, 'grid')
  redraw (cwe)
  xx <- cwe
  
  layoutNetwork (cwe, 'grid')   # get a reasonable starting layout, with the nodes well-separate

     # the scheme:  get current positions, find their mean, place all the nodes there,
     # get their new positions, check to see that they are the means just set.
  
  positions <- getNodePosition (cwe, c ('A', 'B', 'C'))

     # place the nodes on top of each other, at the center of their 3-cornered original layout

  center.x = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$x)))))
  center.y = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$y)))))

     # rearrange the positions
  layoutNetwork (cwe, 'grid')

    # superimpose A,B, and C  in the center
  setNodePosition (cwe, c ('A', 'B', 'C'), rep (center.x, 3), rep (center.y, 3))
  x.funky = center.x + 50
  y.funky = center.y + 50
    # offset funky.node.name
  setNodePosition (cwe, funky.node.name, x.funky, y.funky)
  fitContent (cwe)
  setZoom (cwe, 0.75 * getZoom (cwe))
  
     # now check that the nodes have been repositioned from grid to centered (A,B,C) and offset (funky.node.name)
  current.x = getNodePosition (cwe, 'A')[[1]]$x
  current.y = getNodePosition (cwe, 'A')[[1]]$y
  
  checkEqualsNumeric (current.x, center.x, tol=1)
  checkEqualsNumeric (current.y, center.y, tol=1)

  funky.pos.x = getNodePosition (cwe, funky.node.name) [[1]]$x
  funky.pos.y = getNodePosition (cwe, funky.node.name) [[1]]$y
  checkEqualsNumeric (funky.pos.x, x.funky, tol=1)
  checkEqualsNumeric (funky.pos.y, y.funky, tol=1)

  invisible (cwe)

} # test.getNodePosition.colonInNodeName
#------------------------------------------------------------------------------------------------------------------------
test.getNodeSize = function ()
{
  title = 'test.getNodeSize'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=RCy3::makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')

     # establish a good starting point
  setNodeSizeDirect (cw, nodes (cw@graph), rep (100, 3))
  redraw (cw)

  sizes =  getNodeSize (cw, nodes (cw@graph))
    # these next test pass fine in uncomplicated circumstances, but (apparently) fail due to
    # vizmap complexities when lots of windows are or have been open
  #checkEquals (sizes$width, c (100, 100, 100))
  #checkEquals (sizes$height, c (100, 100, 100))

  setNodeSizeDirect (cw, c ('A', 'B'), 150); redraw (cw)
  sizes =  getNodeSize (cw, nodes (cw@graph))

    # these next test pass fine in uncomplicated circumstances, but (apparently) fail due to
    # vizmap complexities when lots of windows are or have been open
  #checkEquals (sizes$width, c (150, 150, 100))
  #checkEquals (sizes$height, c (150, 150, 100))

  setNodeSizeDirect (cw, c ('A', 'B'), c (180, 32));   redraw (cw)

  sizes = getNodeSize (cw, nodes (cw@graph))
  #checkEquals (sizes$width, c (180, 32, 100))
  #checkEquals (sizes$height, c (180, 32, 100))

     # now allow for non-symmetric dimensions, in which width and height are set separately
  lockNodeDimensions (cw, FALSE)
  setNodeHeightDirect (cw, c ('A', 'B', 'C'), c (12, 22, 32))
  setNodeWidthDirect (cw, c ('A', 'B', 'C'), c (120, 122, 132))
  redraw (cw)

  sizes = getNodeSize (cw, 'B')
  #checkEquals (sizes$width, 122)
  #checkEquals (sizes$height, 22)

       # return to symmetric dimensions
  lockNodeDimensions (cw, TRUE)
  redraw (cw)

      # not sure how width and height are rectified.  it appears that the last-used width=height values are returned
  sizes = getNodeSize (cw, nodes (cw@graph))
  #checkEquals (sizes$width, sizes$height)
         
  invisible (cw)

} # test.getNodeSize
#------------------------------------------------------------------------------------------------------------------------
test.haveNodeAttribute = function ()
{
  title = 'test.haveNodeAttribute'
  window.prep (title)

  cw3 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

  cy = CytoscapeConnection ()

  nodes.with.attribute = RCy3:::haveNodeAttribute (cy, nodes (getGraph (cw3)), 'lfc')
  checkEquals (sort (nodes.with.attribute),  c ('A', 'B', 'C'))

  checkEquals (length (RCy3:::haveNodeAttribute (cy, nodes (getGraph (cw3)), 'type')), 3)
  checkEquals (length (RCy3:::haveNodeAttribute (cy, nodes (getGraph (cw3)), 'label')), 3)
  checkEquals (length (RCy3:::haveNodeAttribute (cy, nodes (getGraph (cw3)), 'count')), 3)

  checkEquals (length (RCy3:::haveNodeAttribute (cy, nodes (getGraph (cw3)), 'bogus')), 0)

  invisible (cw3)

} # test.haveNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.haveEdgeAttribute = function ()
{
  title = 'test.haveEdgeAttribute'
  window.prep (title)

  cw3 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

  cy = CytoscapeConnection ()

  cy2.edgenames = as.character (cy2.edge.names (getGraph (cw3)))
  edges.with.attribute = RCy3:::haveEdgeAttribute (cy, cy2.edgenames, 'edgeType')

  checkEquals (length (edges.with.attribute), 3)
  checkTrue ("A (phosphorylates) B" %in% edges.with.attribute)
  checkTrue ("B (synthetic lethal) C" %in% edges.with.attribute)
  checkTrue ("C (undefined) A" %in% edges.with.attribute)

  checkTrue (length (RCy3:::haveEdgeAttribute (cy, cy2.edgenames, 'score')) == 3)
  checkTrue (length (RCy3:::haveEdgeAttribute (cy, cy2.edgenames, 'misc')) == 3)
  checkTrue (length (RCy3:::haveEdgeAttribute (cy, cy2.edgenames, 'bogus')) == 0)

} # test.haveEdgeAttribute
#------------------------------------------------------------------------------------------------------------------------
hiddenTest.haveEdgeAttribute.oneEdgeOnly = function ()
{
  title = 'test.haveEdgeAttribute.oneEdgeOnly'
  window.prep (title)

  g = makeSimpleGraph ()
  g = removeNode ('A', g)
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  cy = CytoscapeConnection ()

  cy2.edgenames = as.character (cy2.edge.names (getGraph (cw)))

  checkTrue (length (RCy3:::haveEdgeAttribute (cy, cy2.edgenames, 'score')) == 1)

} # hiddenTest.haveEdgeAttribute.oneEdgeOnly
#------------------------------------------------------------------------------------------------------------------------
test.copyNodeAttributesFromCyGraph = function ()
{
  title = 'test.copyNodeAttributesFromCyGraph'
  window.prep (title)

  cw3 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

    # we can now depend upon Cytoscape holding its own version of cw3@graph 
    # in expected use, we expect that 'getGraphFromWindow' will be called, to get the nodes, edges, and both
    # node & edge attributes
    # but here, we only want to test the reliabiilty of querying the Cytoscape version of the graph for all of its node
    # attributes.  so we build a 3-node graph, *without* attributes, and pass that to copyNodeAttributesFromCyGraph, 
    # which should copy those Cytoscape graph node attributes onto the graph we pass in.
  g = new ('graphNEL', edgemode='directed')
  g = graph::addNode (c ('A', 'B', 'C'), g)

  cy = CytoscapeConnection ()
  g2 = RCy3:::copyNodeAttributesFromCyGraph (cy, getWindowID (cy, title), g)
  checkEquals (length (intersect (noa.names (g2), c ("canonicalName", "count", "label", "lfc", "type"))), 5)
  checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='canonicalName')), c ('A', 'B', 'C'))
  checkEquals (as.integer (nodeData (g2, c ('A', 'B', 'C'), attr='count')), c (2, 30, 100))
  checkEquals (as.numeric (nodeData (g2, c ('A', 'B', 'C'), attr='lfc')), c (-3,  0,  3))
  checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='type')), c ("kinase", "transcription factor", "glycoprotein"))

  invisible (cw3)

} # test.copyNodeAttributesFromCyGraph
#------------------------------------------------------------------------------------------------------------------------
test.copyEdgeAttributesFromCyGraph = function ()
{
  title = 'test.copyEdgeAttributesFromCyGraph'
  window.prep (title)

  cw3 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

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
  g2 = RCy3:::copyEdgeAttributesFromCyGraph (cy, cw3, g)

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

  checkEquals (eda (g2, 'canonicalName') [['A|B']],  "A (phosphorylates) B")
  checkEquals (eda (g2, 'canonicalName') [['B|C']],  "B (synthetic lethal) C")
  checkEquals (eda (g2, 'canonicalName') [['C|A']],  "C (undefined) A")

  invisible (g2)

} # test.copyEdgeAttributesFromCyGraph
#------------------------------------------------------------------------------------------------------------------------
test.getGraphFromCyWindow = function ()
{
  cy = CytoscapeConnection ()

  title = 'test.getGraphFromCyWindow'
  window.prep (title)

  cw3 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

  g3 = getGraphFromCyWindow (cy, 'test.getGraphFromCyWindow')
  checkEquals (sort (nodes (g3)), c ('A', 'B', 'C'))
  checkEquals (length (intersect (noa.names (g3), c ("canonicalName", "count", "label", "lfc", "type"))), 5)
  checkEquals (as.character (sort (noa (g3, 'canonicalName'))), c ('A', 'B', 'C'))
  checkEquals (as.integer   (sort (noa (g3, 'count'))),         c (2, 30, 100))
  checkEquals (as.character (sort (noa (g3, 'label'))),         c ('Gene A', 'Gene B', 'Gene C'))
  checkEquals (as.numeric (sort (noa (g3, 'lfc'))),             c (-3,  0,  3))
  checkEquals (as.character (sort (noa (g3, 'type'))),          c ("glycoprotein", "kinase", "transcription factor"))

  checkEquals (length (intersect (eda.names (g3), c ("canonicalName", "edgeType", "interaction", "misc", "score"))), 5)

  checkEquals (sort (names (cy2.edge.names (g3))),        c ('A~B',                   'B~C',                    'C~A'))
  checkEquals (sort (as.character (cy2.edge.names (g3))), c ("A (phosphorylates) B",  "B (synthetic lethal) C", "C (undefined) A"))

  checkEquals (as.character (sort (eda (g3, 'edgeType'))), c ("phosphorylates", "synthetic lethal", "undefined"))
  checkEquals (as.character (sort (eda (g3, 'canonicalName'))), c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (undefined) A"))
  checkEquals (as.character (sort (eda (g3, 'interaction'))), c ("phosphorylates", "synthetic lethal", "undefined"))
  checkEquals (as.character (sort (eda (g3, 'misc'))), c ("default misc", "default misc", "default misc"))
  checkEquals (as.numeric (sort (eda (g3, 'score'))), c (-12,  0,  35))

  invisible (g3)

} # test.getGraphFromCyWindow
#------------------------------------------------------------------------------------------------------------------------
# try graphs with no edges, then one with neither nodes nor edges
# todo:  try single node, and single edge graphs.
test.sendDegenerateGraphs = function ()
{
  title = 'test.sendDegenerateGraphs'
  window.prep (title)

  g.no.edges <- new ('graphNEL')
  g.no.edges <- addNode (c ('A', 'B'), g.no.edges)
  cw.degen <- CytoscapeWindow (title, g.no.edges)
  displayGraph (cw.degen)
  redraw (cw.degen)
  layoutNetwork (cw.degen, 'grid')

  title = 'test.sendEmptyGraph'
  window.prep (title)

  g.empty <- new ('graphNEL')
  cw.empty <- CytoscapeWindow (title, g.empty)
  displayGraph (cw.empty)
  redraw (cw.empty)
  layoutNetwork (cw.empty, 'grid')

  invisible (cw.empty)

} # test.sendDegenerateGraphs
#------------------------------------------------------------------------------------------------------------------------
# sending single strings from R to java via xmlrpc, when lists are expected, changes the signature match.
# make sure we have a solution.
#test.sendGraphWithSingleEdge = function ()
#{
#  title = 'test.sendGraphWithSingleEdge'
#  window.prep (title)
#
#  g = makeSimpleGraph ()
#  g = removeNode ('B', g)
#  cw = CytoscapeWindow (title, g)
#  displayGraph (cw)
#  redraw (cw)
#  layoutNetwork (cw, 'grid')
#
#  checkEquals (getEdgeCount (cw), 1)
#
#  invisible (cw)
#
#} # test.sendGraphWithSingleEdge
#------------------------------------------------------------------------------------------------------------------------
test.sendBigGraph = function ()
{
  title = 'test.sendBigGraph'
  window.prep (title)

  probability.of.edge.being.selected = 0.05
  node.names = as.character (1:30)

  g.big <- randomEGraph (node.names, probability.of.edge.being.selected)
  g.big <- initEdgeAttribute (g.big, 'weight', 'numeric', 0.0)
  write (sprintf (title, length (nodes (g.big)), length (edgeNames (g.big))), stderr ())
  cbig <- CytoscapeWindow (title, g.big)
  stopifnot (class (cbig) == "CytoscapeWindowClass")
  displayGraph (cbig)
  redraw (cbig)
  layoutNetwork (cbig, 'grid')

  invisible (cbig)

} # test.sendBigGraph
#------------------------------------------------------------------------------------------------------------------------
test.createWindowFromSelection = function ()
{
  title = 'test.createWindowFromSelection'
  window.prep (title)
  cy = CytoscapeConnection ()

  cw =  CytoscapeWindow (title, makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)
  selectNodes (cw, c ('A', 'C'))
 
  new.window.title = 'NEW'
  if (new.window.title %in% as.character (getWindowList (cy)))
    deleteWindow (cy, new.window.title)

  c2 = createWindowFromSelection (cw, new.window.title, TRUE)
  redraw (c2)
  layoutNetwork (c2)

  clearSelection (c2)
  selectNodes (c2, 'C')
  checkEquals (getSelectedNodeCount (c2), 1)

  new.window.title = 'NEW, just 1 node'
  if (new.window.title %in% as.character (getWindowList (cy)))
    deleteWindow (cy, new.window.title)

  c3 = createWindowFromSelection (c2, new.window.title, T)
  redraw (c3)
  layoutNetwork (c3)

  invisible (list (cw=cw, c2=c2, c3=c3))

} # test.createWindowFromSelection
#------------------------------------------------------------------------------------------------------------------------
test.addGraphToGraph = function ()
{
  title = 'test.addGraphToGraph'
  window.prep (title)

  cw3 <- CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)

  g2 <- new("graphNEL", edgemode = "directed")
  g2 <- graph::addNode ('A', g2)
  g2 <- graph::addNode ('B', g2)
  g2 <- graph::addNode ('D', g2)
  g2 <- graph::addNode ('E', g2)

  g2 <- initNodeAttribute (g2, "label", "char", "default node label")
  g2 <- initNodeAttribute (g2, "type", "char", "unspecified type")

  g2 <- initNodeAttribute (g2, "SCORE", "numeric", 0.0)

  g2 <- initEdgeAttribute (g2, "edgeType", "char", "unspecified")
  g2 <- initEdgeAttribute (g2, "probability", "numeric", 0.0)

  nodeData (g2, 'D', 'label') <- 'Gene D'
  nodeData (g2, 'E', 'label') <- 'Gene E'
  nodeData (g2, 'D', 'type') <- 'new and novel'
  nodeData (g2, 'E', 'type') <- 'new and credible'

  nodeData (g2, 'D', 'SCORE') <- 1001.01
  nodeData (g2, 'E', 'SCORE') <- 99.09

  g2 <- graph::addEdge ('D', 'E', g2)
  g2 <- graph::addEdge ('A', 'E', g2)
  g2 <- graph::addEdge ('A', 'B', g2)

  edgeData (g2, 'D', 'E', 'probability') <- 0.95
  edgeData (g2, 'D', 'E', 'edgeType') <- 'literature'
  edgeData (g2, 'A', 'E', 'edgeType') <- 'inferred'

  addGraphToGraph (cw3, g2)
  redraw (cw3)
  layoutNetwork (cw3)

    # now copy the combined graph back to R, check it for consistency
  cw.copy <- existing.CytoscapeWindow ('test.addGraphToGraph', copy=T)

    # first, simple node and edge names
  checkEquals (sort (nodes (cw.copy@graph)), c ('A', 'B', 'C', 'D', 'E'))
  checkEquals (sort (edgeNames (cw.copy@graph)), c ("A~B", "A~E", "B~C", "C~A", "D~E"))

    # are all the expected node and edge attributes present?
  checkEquals (length (intersect (noa.names (cw.copy@graph), c ("canonicalName", "count", "label", "lfc", "SCORE", "type"))), 6)

    # edge attributes
  checkEquals (length (intersect (eda.names (cw.copy@graph), c ("canonicalName", "edgeType", "interaction", "misc", "probability", "score"))), 6)

    # check the node label attributes
  checkEquals (nodeData (cw.copy@graph, attr='label')$A, 'Gene A')
  checkEquals (nodeData (cw.copy@graph, attr='label')$B, 'Gene B')
  checkEquals (nodeData (cw.copy@graph, attr='label')$C, 'Gene C')
  checkEquals (nodeData (cw.copy@graph, attr='label')$D, 'Gene D')
  checkEquals (nodeData (cw.copy@graph, attr='label')$E, 'Gene E')
  
    # check the edgeType attributes
  checkEquals (edgeData (cw.copy@graph, 'A', 'B', attr='edgeType')[[1]], 'phosphorylates')
  checkEquals (edgeData (cw.copy@graph, 'A', 'E', attr='edgeType')[[1]], 'inferred')
  checkEquals (edgeData (cw.copy@graph, 'B', 'C', attr='edgeType')[[1]], 'synthetic lethal')
  checkEquals (edgeData (cw.copy@graph, 'C', 'A', attr='edgeType')[[1]], 'undefined')
  checkEquals (edgeData (cw.copy@graph, 'D', 'E', attr='edgeType')[[1]], 'literature')

    # check the edge probability attributes
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'A', 'B', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'A', 'E', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'B', 'C', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'C', 'A', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'D', 'E', attr='probability')[[1]]), 0.95)

  checkEquals (as.integer (edgeData (cw.copy@graph, 'A', 'B', attr='score')[[1]]), 35)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'A', 'E', attr='score')[[1]]), 0)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'B', 'C', attr='score')[[1]]), -12)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'C', 'A', attr='score')[[1]]), 0)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'D', 'E', attr='score')[[1]]), 0)

  invisible (cw.copy)

} # test.addGraphToGraph
#------------------------------------------------------------------------------------------------------------------------
test.addGraphToGraph.degenerateFirstGraph = function ()
{
  window.title = 'test.addGraphToGraph.degenerateFirstGraph'
  g = new ('graphNEL', edgemode='directed')
  g = addNode ('A', g)
  g = addNode ('E', g)
  g = addNode ('F', g)
  window.prep (window.title)
  cw <- CytoscapeWindow (window.title, graph=g)

  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw, 'grid')

  g2 <- makeSimpleGraph ()
  addGraphToGraph (cw, g2)
  

  invisible (cw)

} # test.addGraphToGraph.degenerateFirstGraph
#------------------------------------------------------------------------------------------------------------------------
test.existing.CytoscapeWindow = function ()
{
  title = 'test.existing.CytoscapeWindow'
  window.prep (title)

    #----------------------------------------------------------
    # first, try our standard 3-node, 3-edge testing graph
    #----------------------------------------------------------

  cw <- CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  cw2 <- existing.CytoscapeWindow (title, copy=TRUE)
  g2 <- cw2@graph
  checkEquals (sort (nodes (g2)), c ('A', 'B', 'C'))
  checkEquals (sort (edgeNames (g2)), c ("A~B", "B~C", "C~A"))

} # test.existingCytoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
test.existing.CytoscapeWindow.noEdges = function ()
{
  window.title = 'test.existing.CytoscapeWindow.noEdges'
  window.prep (window.title)
 
  g.edgeless = new ('graphNEL', edgemode='directed')
  g.edgeless = addNode ('X', g.edgeless)
  g.edgeless = addNode ('Y', g.edgeless)
  g.edgeless = addNode ('Z', g.edgeless)
  cw.edgeless = CytoscapeWindow (window.title, graph=g.edgeless)
  displayGraph (cw.edgeless)
  redraw (cw.edgeless)
  layoutNetwork (cw.edgeless)

  cw3 = existing.CytoscapeWindow (window.title, copy=TRUE)
  g3 = cw3@graph
  checkEquals (sort (nodes (g3)), c ('X', 'Y', 'Z'))
  checkEquals (length (edgeNames (g3)), 0)
  
  invisible (cw3)

} # test.existingCytoscapeWindow.noEdges 
#------------------------------------------------------------------------------------------------------------------------
test.existing.CytoscapeWindow.emptyGraph = function ()
{
  window.title = 'test.existing.CytoscapeWindow.emptyGraph'
  window.prep (window.title)
  cw.empty = CytoscapeWindow (window.title)
  checkEquals (length (nodes (cw.empty@graph)), 0)
  displayGraph (cw.empty)
  redraw (cw.empty)
  layoutNetwork (cw.empty)

  cw3 <- existing.CytoscapeWindow (window.title, copy=TRUE)
  g3 <- cw3@graph
  checkEquals (length (nodes (g3)), 0)
  checkEquals (length (edges(g3)), 0)

} # test.existingCytoscapeWindow.emptyGraph
#------------------------------------------------------------------------------------------------------------------------
# can we create an edge attribute de novo?
# can we set its value?  retrieve its value?
test.getAttributeNames = function ()
{
  
} # test.addEdgeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.addGetAndDeleteEdgeAttributes = function ()
{
  title = 'test.addGetAndDeleteEdgeAttributes'
  window.prep (title)

  g  = makeSimpleGraph ()
  cw = CytoscapeWindow ('test.addGetAndDeleteEdgeAttributes', graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)

  cy = CytoscapeConnection ()

     # in this test we add two new edge attributes, 'species' and 'ageInYears'
     # if they are already defined, from a previous run of this test, start by deleting them.

  novel.eda.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getEdgeAttributeNames(cy))
  for (eda.name in novel.eda.to.delete)
    deleteEdgeAttribute (cy,eda.name)
    
     # canonicalName and interaction are added by Cytoscape
  checkEquals (length (intersect (getEdgeAttributeNames (cy), c ("canonicalName", "edgeType", "interaction", "misc", "score"))), 5)

     # now add an attribute to two of the edges 
  first.two.edges = as.character (cy2.edge.names (g)[1:2])
  values = c ('hemlock', 'yew')
  setEdgeAttributesDirect (cw, 'treeSpecies', 'char', first.two.edges, values)

    # now add an attribute to a single edge.  this exercises a different branch in RCytoscape:setEdgeAttributesDirect
  first.edge = as.character (cy2.edge.names (g)[1])
  value = 'one century'
  setEdgeAttributesDirect (cw, 'ageInYears', 'char', first.edge, value)
  checkTrue ('ageInYears' %in% getEdgeAttributeNames (cw))

     # get names from cy2.edge.names (cw@graph)
  checkEquals (getEdgeAttribute (cw, "B (synthetic lethal) C", 'treeSpecies'), "yew")
  checkEquals (getEdgeAttribute (cw, "B (synthetic lethal) C", 'score'), -12)

  deleteEdgeAttribute (cy, 'species')
  deleteEdgeAttribute (cy, 'ageInYears')

  invisible (cw)

} #  test.addGetAndDeleteEdgeAttributes 
#------------------------------------------------------------------------------------------------------------------------
test.addGetAndDeleteNodeAttributes = function ()
{
  title = 'test.addGetAndDeleteNodeAttributes'
  window.prep (title)

  cy = CytoscapeConnection ()

     # in this test we add two new node attributes, 'species' and 'ageInYears'
     # if they are already defined, from a previous run of this test, start by deleting them.

  novel.noa.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getNodeAttributeNames(cy))
  for (noa.name in novel.noa.to.delete)
    deleteNodeAttribute (cy, noa.name)
    
  g  = makeSimpleGraph ()
  cw = CytoscapeWindow (title, graph=g)
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  redraw (cw)
  x <- cw
     # canonicalName is added by Cytoscape
  checkEquals (length (intersect (getNodeAttributeNames (cy), c ("canonicalName", "count",  "label", "lfc", "type"))), 5)

     # now add an attribute to two of the nodes 
  first.two.nodes = nodes (g) [1:2]
  values = c ('cedar', 'ash')
  setNodeAttributesDirect (cw, 'treeSpecies', 'char', first.two.nodes, values)

    # now add an attribute to a single node.  this exercises a different branch in RCytoscape:setNodeAttributesDirect
  first.node = nodes (g) [1]
  value = 'one millenium'
  setNodeAttributesDirect (cw, 'ageInYears', 'char', first.node, value)
  checkTrue ('ageInYears' %in% getNodeAttributeNames (cw))
  checkEquals (getNodeAttribute (cw, 'B', 'type'), 'transcription factor')
  checkEquals (getNodeAttribute (cw, 'A', 'ageInYears'), 'one millenium')
  checkEquals (getNodeAttribute (cw, 'B', 'ageInYears'), '')

  deleteNodeAttribute (cy, 'species')
  deleteNodeAttribute (cy, 'ageInYears')

  invisible (cw)

} #  test.addGetAndDeleteNodeAttributes 
#------------------------------------------------------------------------------------------------------------------------
test.getAllNodeAttributes = function ()
{
  title = 'test.getAllNodeAttributes'
  window.prep (title)
  
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  cwc = existing.CytoscapeWindow (title, copy=T)
  tbl.noa <- getAllNodeAttributes (cwc)
  checkEquals (nrow (tbl.noa), 3)
  checkTrue (ncol (tbl.noa) >= 5)
  expected.colnames =  c ("canonicalName", "count", "label", "lfc", "type")  # created here
  checkEquals (length (intersect (colnames (tbl.noa), expected.colnames)), 5)
  checkEquals (sort (rownames (tbl.noa)), c ("A", "B", "C"))

   # now try a graph with only one node attribute.  this case used to fail (pshannon, 16 feb 2011)

  g2 = new ('graphNEL', edgemode='directed')
  g2 = initNodeAttribute (g2, 'label', 'char', 'NA')
  g2 = addNode ('A', g2)
  nodeData (g2, 'A', 'label') = 'a label for A'
  window.title = 'single node attribute test'
  if (window.title %in% as.character (getWindowList (cw)))
     deleteWindow (cw, window.title)
  cw2 = CytoscapeWindow (window.title, graph=g2)
  tbl.noa2 = getAllNodeAttributes (cw2)
  checkEquals (ncol (tbl.noa2), 1)
  checkEquals (nrow (tbl.noa2), 1)
  checkEquals (colnames (tbl.noa2), 'label')
  checkEquals (rownames (tbl.noa2), 'A')
  
  invisible (list (a=tbl.noa, b=tbl.noa2))
  
} # test.getAllNodeAttributes
#------------------------------------------------------------------------------------------------------------------------
test.getAllEdgeAttributes = function ()
{
  title = 'test.getAllEdgeAttributes'
  window.prep (title)
  
  cw =  CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)
  tbl.eda = getAllEdgeAttributes (cw)
  checkEquals (class (tbl.eda), 'data.frame')
  checkEquals (dim (tbl.eda), c (3, 5))
  checkEquals (sort (rownames (tbl.eda)), c ("A|B", "B|C", "C|A"))
  checkEquals (sort (colnames (tbl.eda)), c ("edgeType", "misc", "score", "source", "target"))
  checkEquals (class (tbl.eda$score), 'numeric')

    # now try a graph with one edge, and just one edge attribute, to make sure that this edge case is handled properly

  g2 = new ('graphNEL', edgemode='directed')
  g2 = initEdgeAttribute (g2, 'edgeType', 'char', 'unspecified')
  g2 = addNode ('A', g2)
  g2 = addNode ('B', g2)
  g2 = addEdge ('A', 'B', g2)

  edgeData (g2, 'A', 'B', 'edgeType') = 'phosphorylates'

  cy = CytoscapeConnection ()

  window.title = 'edge attribute test, one attribute only'
  if (window.title %in% as.character (getWindowList (cy)))
    deleteWindow (cy, window.title)

  cw2 = CytoscapeWindow (window.title, graph=g2, create.window=FALSE)
  tbl.eda2 = getAllEdgeAttributes (cw2)

  checkEquals (ncol (tbl.eda2), 3)
  checkEquals (nrow (tbl.eda2), 1)
  checkEquals (sort (colnames (tbl.eda2)), c ('edgeType', 'source', 'target'))

  invisible (tbl.eda2)

} # test.getAllEdgeAttributes
#------------------------------------------------------------------------------------------------------------------------
test.getVisualStyleNames = function ()
{
  title = 'test.getVisualStyleNames'
  window.prep (title)

  cw3 =  CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layoutNetwork (cw3)
  current.names = getVisualStyleNames (cw3)
  checkTrue (length (intersect (current.names, c (title, 'default', 'Nested Network Style', 'Minimal', 'Sample1', 'Universe'))) >= 3)

  invisible (cw3)

} # test.getVisualStyleNames 
#------------------------------------------------------------------------------------------------------------------------
test.copyVisualStyle = function ()
{
  title = 'test.copyVisualStyle'
  window.prep (title)

  cw4 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw4)
  redraw (cw4)
  layoutNetwork (cw4)

  current.names = getVisualStyleNames (cw4)

    # code around a very weird bug, which I do not understand at all (pshannon, 26 dec 2010)
  unique.name = FALSE;
  new.style.name = sprintf ("tmp.%s", runif (1, 1, 1000))
  copyVisualStyle (cw4, 'default', new.style.name)
  new.names = getVisualStyleNames (cw4)
  checkEquals (setdiff (new.names, current.names), new.style.name)

  invisible (cw4)

} # test.copyVisualStyle
#------------------------------------------------------------------------------------------------------------------------
test.setVisualStyle = function ()
{
  title = 'test.setVisualStyle'
  window.prep (title)
  cy = CytoscapeConnection ()

  cw5 = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw5)
  redraw (cw5)
  layoutNetwork (cw5)
  
  current.names = getVisualStyleNames (cw5)
  for (style.name in current.names) {
    setVisualStyle (cy, style.name)
    Sys.sleep (1)
    } # for style.name

  invisible (cw5)

} # test.setVisualStyle
#------------------------------------------------------------------------------------------------------------------------
# meager test only:  make sure all of these methods can be called
# todo:  call set, call get, check for color match
test.defaultColors = function ()
{
  title = 'test.defaultColors'
  window.prep (title)
  cy = CytoscapeConnection ()
  getDefaultBackgroundColor (cy)
  getDefaultEdgeReverseSelectionColor (cy)
  getDefaultEdgeSelectionColor (cy)
  getDefaultNodeReverseSelectionColor (cy)
  getDefaultNodeSelectionColor (cy)

  black = '#000000'
  red = '#FF0000'
  white = '#FFFFFF'
  green = '#00FF00'
  gray = '#888888'

  setDefaultBackgroundColor (cy, white)
  setDefaultEdgeReverseSelectionColor (cy, red)
  setDefaultEdgeSelectionColor (cy, green)
  setDefaultNodeReverseSelectionColor (cy, red)
  setDefaultNodeSelectionColor (cy, green)

} # test.defaultColors
#------------------------------------------------------------------------------------------------------------------------
test.setWindowSizeRaiseWindow = function ()
{
  title = 'test.setWindowSizeRaiseWindow'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  raiseWindow (cw)

  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)
  for (i in 1:10) {
    setWindowSize (cw, 200, 200)
    setWindowSize (cw, 400, 400)
    } # for i

    # now raise a few random windows by name

  if (!exists ('cy'))
    cy = CytoscapeConnection ()

  window.names = as.character (getWindowList (cy))

  if (length (window.names) > 2) {  # only if there are multiple windows
    for (i in 1:10) {
      index = as.integer (runif (1, 1, length (window.names)))
      raiseWindow (cy, window.names [index])
      raiseWindow (cw)
      } # for i
    } # if 3 or more windows are open

  invisible (cw)
  
} # test.setWindowSizeRaiseWindow
#------------------------------------------------------------------------------------------------------------------------
test.fitContent = function ()
{
  title = 'test.fitContent'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  clearSelection (cw)
  selectNodes (cw, 'A')
  checkEquals (getSelectedNodeCount (cw), 1)

  for (i in 1:10) {
    fitSelectedContent (cw)
    fitContent (cw)
    } # for i

} # test.fitContent
#------------------------------------------------------------------------------------------------------------------------
test.windowCoordinates = function ()
{
  title = 'test.windowCoordinates'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  center = getCenter (cw)
  checkEquals (names (center), c ('x', 'y'))

  corners = getViewCoordinates (cw)
  checkEquals (names (corners), c ('top.x', 'top.y', 'bottom.x', 'bottom.y'))

  invisible (cw)

} # test.windowCoordinates
#------------------------------------------------------------------------------------------------------------------------
test.zoom = function ()
{
  title = 'test.zoom'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  setWindowSize (cw, 1200, 800)  
  fitContent (cw)

  smaller = 0.5
  larger = 2

  for (i in 1:10) 
    setZoom (cw, smaller * getZoom (cw))

  for (i in 1:10) 
    setZoom (cw, larger * getZoom (cw))

  invisible (cw)

} # test.zoom
#------------------------------------------------------------------------------------------------------------------------
test.center = function ()
{
  title = 'test.setCenter'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  setWindowSize (cw, 1200, 800)  
  fitContent (cw)
  setZoom (cw, 0.5 * getZoom (cw))

  center.orig = getCenter (cw)
  delta = 100
  x.left = center.orig$x - delta
  x.right = center.orig$x + delta
  y.up = center.orig$y - delta
  y.down = center.orig$y + delta

  for (i in 1:10) {
    setCenter (cw, x.left, y.up)
    setCenter (cw, as.integer (x.left), as.integer (y.up))   # make sure the called function casts this int back to numeric
    setCenter (cw, x.left, y.down)
    setCenter (cw, x.right, y.down)
    setCenter (cw, x.right, y.up)
    } # for i

  setCenter (cw, center.orig$x, center.orig$y)

  invisible (cw)

} # test.center
#------------------------------------------------------------------------------------------------------------------------
test.setNodeSizeDirect = function ()
{ 
  DEACTIVATED("too slow")
  title = 'test.setNodeSizeDirect'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)
  
  lockNodeDimensions (cw, TRUE)

  small = 30
  large = 300
  for (i in 1:10) {
    setNodeSizeDirect (cw, 'A', small); 
    redraw (cw)
    setNodeSizeDirect (cw, 'A', large); 
    redraw (cw)
    } # for i

  invisible (cw)

} # test.setNodeSizeDirect
#------------------------------------------------------------------------------------------------------------------------
test.setNodeWidthAndHeightDirect = function ()
{ 
  DEACTIVATED("too slow")
  title = 'test.setNodeWidthAndHeightDirect'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  lockNodeDimensions (cw, FALSE)
  
  small = 30
  large = 300

  for (i in 1:10) {
    setNodeWidthDirect (cw, 'A', small); 
    setNodeHeightDirect (cw, 'A', large); 
    redraw (cw)
    setNodeWidthDirect (cw, 'A', large); 
    setNodeHeightDirect (cw, 'A', small); 
    redraw (cw)
    } # for i

  invisible (cw)

} # test.setNodeWidthAndHeightDirect
#------------------------------------------------------------------------------------------------------------------------
test.setNodeFontSizeDirect = function ()
{ 
  DEACTIVATED("too slow")
  title = 'test.setNodeFontSizeDirect'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  starting.size = 4
  setNodeSizeDirect (cw, c ('A', 'B', 'C'), 50)
  setNodeFontSizeDirect (cw, c ('A', 'B'), 12)
  redraw (cw)
  
  for (i in 1:20) {
    setNodeFontSizeDirect (cw, 'A', starting.size + i)
    setNodeFontSizeDirect (cw, 'B', starting.size + (i*3))
    redraw (cw)
    } # for i

  starting.size = 32
  for (i in 20:1) {
    setNodeFontSizeDirect (cw, 'A', starting.size - i)
    setNodeFontSizeDirect (cw, 'B', starting.size - (i*3))
    redraw (cw)
    } # for i


  invisible (cw)

} # test.setNodeSizeDirect
#------------------------------------------------------------------------------------------------------------------------
test.setNodeShapeDirect = function ()
{ 
  DEACTIVATED("too slow")
  title = 'test.setNodeShapeDirect'
  window.prep (title)
  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  lockNodeDimensions (cw, TRUE)
  setNodeSizeDirect (cw, 'A', 100)

  for (new.shape in getNodeShapes (cw)) {
    setNodeShapeDirect (cw, 'A', new.shape)
    redraw (cw)
    } # for new.shape

  invisible (cw)

} # test.setNodeShapeDirect
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeVizPropertiesDirect = function (cw=NULL)
{
  DEACTIVATED("too slow!")
  title = 'test.setEdgeVizPropertiesDirect'

  if (is.null (cw)) {
    window.prep (title)
    cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
    displayGraph (cw)
    redraw (cw)
    layoutNetwork (cw)
    }

  cy2.edgeNames = sort (getAllEdges (cw))
  e1 = cy2.edgeNames [1]
  e2 = cy2.edgeNames [2]
  e3 = cy2.edgeNames [3]

  colors = c ('#440000', '#004400', '#000044', '#880000', '#008800', '#000088', '#FF0000', '#00FF00', '#0000FF', '#FFFFFF')
  line.styles = c ('SOLID', 'LONG_DASH', 'EQUAL_DASH', 'DASH_DOT', 'DOT', 'ZIGZAG', 'SINEWAVE', 'VERTICAL_SLASH', 
                   'FORWARD_SLASH', 'BACKWARD_SLASH', 'PARALLEL_LINES', 'CONTIGUOUS_ARROW', 'SEPARATE_ARROW')

  arrow.shapes = c ('No Arrow', 'Diamond', 'Delta', 'Arrow', 'T', 'Circle', 'Half Arrow Top', 'Half Arrow Bottom', 'Diamond', 'T')
  labels = paste ('label', seq (10,100,10), sep='-');  labels [10] = ''
  tooltips = paste ('tooltip', seq (10,100,10), sep='-');  tooltips [10] = ''
  widths = c (1:9, 1)

  for (i in 1:10) {
    setEdgeOpacityDirect (cw, c (e2, e3), 25 * i)
    setEdgeColorDirect (cw, e1, colors [i])
    setEdgeLineStyleDirect (cw, e2, line.styles [i])
    setEdgeSourceArrowShapeDirect (cw, e1, arrow.shapes [i])
    setEdgeTargetArrowShapeDirect (cw, e1, arrow.shapes [i])
    setEdgeLabelDirect (cw, e2, labels [i])
    setEdgeLabelColorDirect (cw, e2, colors [i])
    setEdgeTooltipDirect (cw, e2, tooltips [i])
    setEdgeLineWidthDirect (cw, e3, widths [i])
    setEdgeFontSizeDirect (cw, e2, widths [i] * 3)
    setEdgeSourceArrowColorDirect (cw, e1, colors [11-i])
    setEdgeTargetArrowColorDirect (cw, e1, colors [11-i])
    setEdgeLabelOpacityDirect (cw, e2, 25 * i)
    setEdgeSourceArrowOpacityDirect (cw, e1, 25 * i)
    setEdgeTargetArrowOpacityDirect (cw, e1, 255 - (25 * i))
    redraw (cw)
    Sys.sleep (1)
    }

  invisible (cw)

} # test.setEdgeVizPropertiesDirect
#------------------------------------------------------------------------------------------------------------------------
test.graphBAM = function ()
{ 
  title = 'test.graphBAM'
  window.prep (title)

    # example is taken from Nishant's man page
  source.nodes  <- c ("a", "a", "b", "c", "d")
  target.nodes  <- c ("b", "c", "c", "d", "a")
  weights <- c(2.3, 2.3, 4.3, 1.0, 3.0)
  df <- data.frame (from=source.nodes, to=target.nodes, weight=weights)
  g.bam <- graphBAM (df)
  g.bam <- initEdgeAttribute (g.bam, 'weight', 'numeric', 0.0)

  cw = CytoscapeWindow (title, graph=g.bam)
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

    # paint the edges shades of red as function of weight
  setDefaultEdgeLineWidth (cw, 5)
  setEdgeColorRule (cw, 'weight',  c (0, 5), c ('#FFFFFF', '#FF0000'),  mode='interpolate')

  invisible (cw)

} # test.graphBAM
#------------------------------------------------------------------------------------------------------------------------
# add a node to an existing graph.
# questions:  
#  1) what edge attribute values are assigned to this new edge?  
#  2) can we assign new values to those attributes?  use setEdgeAttributesDirect
test.addCyNode = function ()
{ 
  title = 'test.addCyNode'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw, 'grid')
  checkEquals (getNodeCount (cw), 3)
  addCyNode (cw, 'NEW')
  layoutNetwork (cw, 'grid')
  checkEquals (getNodeCount (cw), 4)
  invisible (cw)

} # test.addCyNode
#------------------------------------------------------------------------------------------------------------------------
# add an edge to an existing graph.
# questions:  
#  1) what edge attribute values are assigned to this new edge?  
#  2) can we assign new values to those attributes?  use setEdgeAttributesDirect
test.addCyEdge = function ()
{ 
  title = 'test.addCyEdge'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)
  checkEquals (getEdgeCount (cw), 3)

  directed.edge = TRUE
  addCyEdge (cw, 'A', 'B', 'synthetic rescue', directed.edge)
  redraw (cw)
  layoutNetwork (cw)
  checkEquals (getEdgeCount (cw), 4)
  invisible (cw)
    
} # test.addCyEdge
#------------------------------------------------------------------------------------------------------------------------
test.twoGraphsDoubleEdges = function ()
{ 
  title = 'test.twoGraphsDoubleEdges'
  window.prep (title)

  cw = CytoscapeWindow (title, graph=makeSimpleGraph ())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork (cw)

  g2 = new ('graphNEL', edgemode='directed')
  g2 = initEdgeAttribute (g2, 'edgeType', 'char', 'unspecified')

  g2 = addNode ('A', g2)
  g2 = addNode ('B', g2)
  g2 = addEdge ('A', 'B', g2)

  edgeData (g2, 'A', 'B', 'edgeType') = 'synthetic rescue'


  # fails:  addGraphToGraph (cw, g2)
  #xml.rpc (cw@uri, 'Cytoscape.createEdge', cw@window.id, 'A', 'B', 'synthetic rescue', T)
  redraw (cw)
  layoutNetwork (cw)
    
} # test.twoGraphsoubleEdges
#------------------------------------------------------------------------------------------------------------------------
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

  #gx = makeSimpleGraph ()
  #gx = graph::addEdge ('C', 'B', gx)
  #edgeData (gx, 'C', 'B', attr='edgeType') = 'synthetic rescue'
  #tbl.egx = RCy3:::.classicGraphToNodePairTable (gx)
  #checkEquals (dim (tbl.egx), c (4, 3))
  #checkEquals (colnames (tbl.egx), c ("source", "target", "edgeType"))
  #checkEquals (tbl.egx$edgeType, c ("phosphorylates", "synthetic lethal", "undefined", "synthetic rescue"))
  #checkEquals (tbl.egx$source, c ("A", "B", "C", "C"))
  #checkEquals (tbl.egx$target, c ("B", "C", "A", "B"))

} # test..classicGraphToNodePairTable 
#------------------------------------------------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------------------------------------------------
# a standard demo, up on the website.  source it, run it.  a graph is created, displayed, retrieved, checked for identity
hiddentest.graphAM.round.trip = function ()
{
  title = 'test.graphAM.round.trip'
  write (noquote (sprintf ('------- %s', title)), stderr ())
  source ('http://rcytoscape.systemsbiology.net/versions/current/cookbook/randomAM/randomAdjacencyMatrixGraphTest.R')
  run (0:7)

} # test.graphAM.round.trip
#------------------------------------------------------------------------------------------------------------------------
restore.defaults = function ()
{
  cy = CytoscapeConnection ()
  setDefaultBackgroundColor (cy, '#CCCCFF')
  setDefaultNodeShape (cy, 'ellipse')
  lockNodeDimensions (cy, TRUE)
  setDefaultNodeSelectionColor (cy, '#FFFF00')
  setDefaultNodeReverseSelectionColor (cy, '#00FF00')
  setDefaultEdgeSelectionColor (cy, '#FF0000')
  setDefaultEdgeReverseSelectionColor (cy, '#00FF00')
  setDefaultNodeSize (cy, 30)
  setDefaultNodeColor (cy, '#FF8888')  # a guess

  setDefaultNodeBorderColor (cy, '#000000')
  setDefaultNodeBorderWidth (cy, 1)
  setDefaultNodeFontSize (cy, 12)
  setDefaultNodeLabelColor (cy, '#000000')
  setDefaultEdgeLineWidth (cy, 1)
  setDefaultEdgeColor (cy, '#0000FF')

} # restore.defaults
#------------------------------------------------------------------------------------------------------------------------
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

  g.1e = addNode ('A', g.0e)
  g.1e = addNode ('B', g.1e)
  g.1e = addEdge ('A', 'B', g.1e)
  edgeData (g.1e, 'A', 'B', attr='edgeType') = 'phosphorylates'

  g1 <- g.1e
  g3 <- g.3e

  novel.edges <- RCy3:::.getNovelEdges (g.3e, g.1e)
  checkEquals (length (novel.edges), 0)

  novel.edges <- RCy3:::.getNovelEdges (g.1e, g.3e)
  checkEquals (length (novel.edges), 2)

} # test..getNovelEdges
#------------------------------------------------------------------------------------------------------------------------
# apparently does not run reliably at bioc
hiddenTest.saveImage = function ()
{
  title = 'test.saveImage'
  window.prep (title)

  g.simple = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, g.simple)

  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  setNodeLabelRule (cw, 'label')
  redraw (cw)

    #--- png first
  filename = sprintf ('%s/%s', tempdir (), 'saveImageTest.png')
  printf ('saving image file to %s', filename)
  saveImage (cw, filename, 'png', 1.0)
  checkTrue (file.exists (filename))

  if (pluginVersion (cy) == '1.8') {
      #--- now pdf
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest.pdf')
    printf ('saving image file to %s', filename)
    saveImage (cw, filename, 'pdf')
    checkTrue (file.exists (filename))

      #--- now svg
    filename = sprintf ('%s/%s', tempdir (), 'saveImageTest.svg')
    printf ('saving image file to %s', filename)
    saveImage (cw, filename, 'svg')
    checkTrue (file.exists (filename))
    } # if plugin version 1.8 is being used

  invisible (cw)

} # test.saveImage
#------------------------------------------------------------------------------------------------------------------------
# apparently does not run reliably at bioc
hiddenTest.saveNetwork = function ()
{
  title = 'test.saveNetwork'
  window.prep (title)

  g.simple = RCy3::makeSimpleGraph ()
  cw = CytoscapeWindow (title, g.simple)

  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  setNodeLabelRule (cw, 'label')
  redraw (cw)

  filename = sprintf ('%s/%s', tempdir (), 'saveNetworkTest.gml')
  printf ('saving gml file to %s', filename)
  saveNetwork (cw, filename)
  checkTrue (file.exists (filename))

  invisible (cw)

} # test.saveNetwork
#------------------------------------------------------------------------------------------------------------------------
test.setNodeImageDirect = function (apply.viz.rules=FALSE)
{
  DEACTIVATED("may be too slow?")
  title = 'test.imageUrl'
  window.prep (title)

  cw = CytoscapeWindow (title, makeSimpleGraph ())
  displayGraph (cw)
  layoutNetwork (cw, 'grid')
  setNodeLabelRule (cw, 'label')

  setNodeImageDirect (cw, 'A', 'http://rcytoscape.systemsbiology.net/versions/current/images/isb.png')
  setNodeImageDirect (cw, 'B', 'http://rcytoscape.systemsbiology.net/versions/current/images/tudelft.jpg')
  setNodeImageDirect (cw, 'C', 'http://rcytoscape.systemsbiology.net/versions/current/images/bioc.tiff')

  setNodeColorDirect (cw, 'A', '#0000FF')
  setNodeColorDirect (cw, 'B', '#FF00FF')
  setNodeColorDirect (cw, 'C', '#FF0000')

  redraw (cw)
  setWindowSize (cw, 800, 800)
  fitContent (cw)

  invisible (cw)

} # test.setNodeImageDirect
#------------------------------------------------------------------------------------------------------------------------
test.validity = function ()
{
  title = 'test.validity error #1'
  window.prep (title)

  g.undirected = new ('graphNEL', edgemode='undirected')
  g.undirected = graph::addNode ('A', g.undirected)
  g.undirected = graph::addNode ('B', g.undirected)
  g.undirected = graph::addEdge ('A', 'B', g.undirected)
  g.undirected = initEdgeAttribute (g.undirected, 'edgeType', 'char', 'unspecified')
  edgeData (g.undirected, 'A', 'B', 'edgeType') = 'reciprocal'


     # should not fail, but warning should be given
  cw = CytoscapeWindow (title, g.undirected)
  checkEquals (validCyWin (cw), TRUE)

     # fix the edgeType complaint

  window.prep (title)
  g = new ('graphNEL', edgemode='directed')

     # should fail with 'You must provide an 'edgeType' edge attribute, which will be mapped to Cytoscape's crucial ...
  cw = CytoscapeWindow (title, g)
  checkEquals (validCyWin (cw), FALSE)

     # fix the edgeType complaint
  title = 'test.validity fixed'
  g = initEdgeAttribute (g, 'edgeType', 'char', 'unspecified')    
  window.prep (title)
  cw = CytoscapeWindow (title, g)
  print (checkTrue (validCyWin (cw)))

    # add a new node attribute the old-fashioned R graph way, make sure the failure to properly initialize is caught
  nodeDataDefaults (g, attr='pval') = 1.0
  title = 'test.validity error #2'
  window.prep (title)
  cw = CytoscapeWindow (title, g)
  checkTrue (!validCyWin (cw))

    # add a new edge attribute the old-fashioned R graph way, make sure the failure to properly initialize is caught
  title = 'test.validity error #3'
  edgeDataDefaults (g, attr='score') = 0.0
  window.prep (title)
  cw = CytoscapeWindow (title, g)
  checkTrue (!validCyWin (cw))

    # now fix them both
  title = 'test.validity fix all'
  g = initNodeAttribute (g, 'pval', 'numeric', 1.0)
  g = initEdgeAttribute (g, 'score', 'numeric', 0)
  window.prep (title)
  cw = CytoscapeWindow (title, g)
  print (checkTrue (validCyWin (cw)))

  invisible (cw)

} # test.validity
#------------------------------------------------------------------------------------------------------------------------
# not really an automated test, except of syntax.  cut and paste these commands into your R session to see that
# they really perform as expected.

test.tooltip.delays = function ()
{
  cw = demoSimpleGraph ()

    # display immediately, stay up until mouse moves away
  setTooltipInitialDelay (cw, 0)
  setTooltipDismissDelay (cw, 0)

    # display after 1 second, for 1 second
  setTooltipInitialDelay (cw, 1000)
  setTooltipDismissDelay (cw, 1000)
  
} # test.tooltip.delays
#------------------------------------------------------------------------------------------------------------------------
# also not an automated test, though exception testing could accomplish that
test.detectUnitializedNodeAttributes = function ()
{
    # starting with the code in makeSampleGraph, change 3 node and 1 edge attribute to use the  standard (not RCy) 
    # attribute initializations. this is an error with respect to RCy, which needs explicit typing of the attributes
    # see if they are caught

  g = new("graphNEL", edgemode = "directed")

  #g = initNodeAttribute(g, "type", "char", "undefined")
  #g = initNodeAttribute(g, "lfc", "numeric", 1)
  #g = initNodeAttribute(g, "label", "char", "default node label")
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

  cw = CytoscapeWindow (title = 'detect unitialized node attributes', graph = g, deleteAnyWindowsOfSameTitle=TRUE)


} # test.detectUnitializedNodeAttributes 
#------------------------------------------------------------------------------------------------------------------------
# also not an automated test, though exception testing could accomplish that
test.detectUnitializedEdgeAttributes = function ()
{
    # starting with the code in makeSampleGraph, change 3 node and 1 edge attribute to use the  standard (not RCy) 
    # attribute initializations. this is an error with respect to RCy, which needs explicit typing of the attributes
    # see if they are caught

  g = new("graphNEL", edgemode = "directed")

  g = initNodeAttribute(g, "type", "char", "undefined")
  g = initNodeAttribute(g, "lfc", "numeric", 1)
  g = initNodeAttribute(g, "label", "char", "default node label")
  g = initNodeAttribute(g, "count", "integer", 0)

  g = initEdgeAttribute(g, "edgeType", "char", "undefined")
  g = initEdgeAttribute(g, "score", "numeric", 0)
  #g = initEdgeAttribute(g, "misc", "char", "default misc")
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

  cw = CytoscapeWindow (title = 'detect unitialized node attributes', graph = g, deleteAnyWindowsOfSameTitle=TRUE)


} # test.detectUnitializedNodeAttributes 
#------------------------------------------------------------------------------------------------------------------------
test.remove.redundancies.in.undirected.graph = function ()
{
  print ('----- test.remove.redundancies.in.undirected.graph')
  nNode <- 500
  nEdge <- 5000
  tmpNodes <- as.character(seq(1, nNode))
  allEdges <- expand.grid(tmpNodes, tmpNodes, stringsAsFactors = F)
  allEdges <- allEdges[(allEdges[, 1] != allEdges[, 2]), ]
  allEdges <- allEdges[(sample(nrow(allEdges), nEdge)), ]
  edgeWeight <- rnorm(nEdge)
  gu <- new("graphNEL", nodes = tmpNodes, edgemode = "undirected")

  gu = initNodeAttribute(gu, "type", "char", "undefined")
  gu = initNodeAttribute(gu, "lfc", "numeric", 1)
  gu = initNodeAttribute(gu, "label", "char", "default node label")

  gu = initEdgeAttribute(gu, "edgeType", "char", "undefined")
  gu = initEdgeAttribute(gu, "weight", "numeric", 0)

  gu = addEdge(allEdges[, 1], allEdges[, 2], gu, edgeWeight)
  nodeData (gu, nodes (gu), 'label') = nodes (gu)

  t0 = Sys.time ()
  g.fixed <- RCy3:::remove.redundancies.in.undirected.graph (gu)
  t1 = Sys.time ()
  elapsed.time = as.numeric (difftime (t1, t0, units='secs'))
  checkTrue (elapsed.time < 5)  # consistently about 0.5 seconds in interactive testing
  checkTrue (all (nodes (gu) == nodes (g.fixed)))
  checkTrue (all (edgeNames (gu) == edgeNames (g.fixed)))

    # the main test
  checkEquals (length (unlist (edges (gu))), 2 * length (unlist (edges (g.fixed))))

} # test.remove.redundancies.in.undirected.graph 
#------------------------------------------------------------------------------------------------------------------------
