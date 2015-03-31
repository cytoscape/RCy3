# a set of functions for assessing and reporting the validity of a CytoscapeWindow object
#------------------------------------------------------------------------------------------------------------------------
validCyWin = function (obj)
{
  if (! 'CytoscapeWindowClass' %in% is (obj))
    return (FALSE)

  uri.result = valid.uri (obj)
  title.result = valid.title (obj)
  window.id.result = valid.window.id (obj)
  graph.result = (valid.graph (obj))

  return (all (uri.result, title.result, window.id.result, graph.result))

} # validCyWin
#------------------------------------------------------------------------------------------------------------------------
valid.uri = function (obj)
{
  slotName = 'uri'

  if (!slotName %in% slotNames (obj)) {
    write (sprintf ("no %s slot in CytoscapeWindow object", slotName), stderr ())
    return (FALSE)
    } # if

  if (class (obj@uri) != 'character') {
    write (sprintf ("CytoscapeRPC slotName 'uri' is not of type character"), stderr ())
    return (FALSE)
    }

  if (length (obj@uri) != 1) {  
    write (sprintf ("CytoscapeRPC slotName 'uri' should have length 1, not %d", length (obj@uri)), stderr ())
    return (FALSE)
    }

  if (gregexpr ('http://', obj@uri) == -1) {
    write (sprintf ("uri slot has no http protocol string: %s", obj@uri), stderr ())
    valid = FALSE
    } # no 'http://' in uri

  return (TRUE)

} # valid.uri
#------------------------------------------------------------------------------------------------------------------------
valid.title = function (obj)
{
  slotName = 'title'

  if (!slotName %in% slotNames (obj)) {
    write (sprintf ("no %s slot in CytoscapeWindow object", slotName), stderr ())
    return (FALSE)
    } # if

  if (class (obj@title) != 'character') {
    write (sprintf ("window title is not of type character"), stderr ())
    return (FALSE)
    }

  if (length (obj@title) == 0) {
    write (sprintf ("missing window title"), stderr ())
    return (FALSE)
    }

  if (length (obj@title) > 1) {
     write (sprintf ("window title is a vector, should be a single string"), stderr ())
     return (FALSE)
     }

  if (!nzchar (obj@title)) {
     write (sprintf ("window title is an empty string"), stderr ())
     return (FALSE)
     }

  if (gregexpr ('http://', obj@uri) == -1) {
    write (sprintf ("uri slot has no http protocol string: %s", obj@uri), stderr ())
    return (FALSE)
    } # no 'http://' in uri

  return (TRUE)

} # valid.title
#------------------------------------------------------------------------------------------------------------------------
valid.window.id = function (obj)
{
  slotName = 'window.id'

  if (!slotName %in% slotNames (obj)) {
    write (sprintf ("no %s slot in CytoscapeWindow object", slotName), stderr ())
    return (FALSE)
    } # if

  if (length (obj@window.id) == 0) {
    write (sprintf ("missing window.id"), stderr ())
    return (FALSE)
    }

  if (length (obj@title) > 1) {
    write (sprintf ("window title is a vector, should be a single string"), stderr ())
    return (FALSE)
    }

  if (class (obj@window.id) != 'character') {
    write (sprintf ("window.id is not of type character"), stderr ())
    return (FALSE)
    }

  if (!nzchar (obj@title)) {
    write (sprintf ("window title is an empty string"), stderr ())
    return (FALSE)
    }

  return (TRUE)

} # valid.window.id
#------------------------------------------------------------------------------------------------------------------------
valid.graph = function (obj)
{
  valid = TRUE

  slotName = 'graph'

  if (!slotName %in% slotNames (obj)) {
    write (sprintf ("no %s slot in CytoscapeWindow object", slotName), stderr ())
    return (FALSE)
    } # if

  if (edgemode (obj@graph) == 'undirected') {
    msgs = c ("Implementation of undirected graphs in the R graph classes includes the redundant storage of edge attributes.",
              "RCytoscape removes this redundancy before sending the graph to Cytoscape, but this operation",
              "can take a LONG time.",
              "Consider using a directed graph in R until we come up with a better solution.")
    for (msg in msgs) 
      write (msg, stderr ())
    } # if

  if (length (obj@graph) == 0) {
    write (sprintf ("missing graph"), stderr ())
    return (FALSE)
    }

  if (!is (obj@graph, 'graph')) {
    write (sprintf ("graph slot is is not an instance of 'graph'"), stderr ())
    return (FALSE)
    }

  edge.attribute.names = eda.names (obj@graph)

  if (! 'edgeType' %in% edge.attribute.names) {
    msgs = c ("Error!  You must provide an 'edgeType' edge attribute for your graph, and initialize it",
              "        with a call to (for instance) ", 
              "",
              "            initEdgeAttribute (g, 'edgeType', 'char', 'some.plausible.default.value')", 
              "", 
              "        This edge attribute will be mapped to  Cytoscape's crucial & roughly equivalent 'interaction'", 
              "        edge attribute.  It gives edges their 'true' cy2 name, i.e.,", 
              "",
              "            'A (phosphorylates) B'.", 
              "", 
              "        where",
              "", 
              "          A: the source node", 
              "          B: the target node", 
              "          phosphorylates:  'edgeType' attribute in RCy; 'interaction' in Cytoscape", 
              "")
    for (msg in msgs) 
      write (msg, stderr ())
    valid = FALSE
    } 

  for (eda.name in edge.attribute.names) {
    caller.specified.attribute.class = attr (edgeDataDefaults (obj@graph, eda.name), 'class')
    if (is.null (caller.specified.attribute.class)) {
      valid = FALSE
      msg1 = sprintf ('Error!  graph edge attribute %s not initialized. You must call', eda.name)
      msg2 = sprintf ('        initEdgeAttribute (graph, "%s", some.attribute.type, some.default.value)', eda.name)
      msg3 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
      msg4 = sprintf ('        example:  g <- initEdgeAttribute (g, "edgeType", "char", "physical interaction")')
      msg5 = sprintf ('             or:  g <- initEdgeAttribute (g, "score", "numeric", 0.0)')
      write (msg1, stderr ())
      write (msg2, stderr ())
      write (msg3, stderr ())
      write (msg4, stderr ())
      } # if uninitialized edge attributes
    } # for eda.name

  node.attribute.names = noa.names (obj@graph)

  for (noa.name in node.attribute.names) {
    caller.specified.attribute.class = attr (nodeDataDefaults (obj@graph, noa.name), 'class')
    if (is.null (caller.specified.attribute.class)) {
      valid = FALSE
      msg1 = sprintf ('Error!  graph node attribute %s not initialized. You must call', noa.name)
      msg2 = sprintf ('        initNodeAttribute (graph, "%s", some.attribute.type, some.default.value)', noa.name)
      msg3 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
      msg4 = sprintf ('        example:  g <- initNodeAttribute (g, "nodeType", "char", "molecule")')
      msg5 = sprintf ('             or:  g <- initNodeAttribute (g, "pValue", "numeric", 1.0)')
      write (msg1, stderr ())
      write (msg2, stderr ())
      write (msg3, stderr ())
      write (msg4, stderr ())
      } # if uninitialized edge attributes
    } # for eda.name

   return (valid)

} # valid.window.id
#------------------------------------------------------------------------------------------------------------------------
